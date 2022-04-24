""" Run Farm management. """

from __future__ import annotations

import re
import logging
import time
import os
from datetime import timedelta
from fabric.api import run, env, prefix, put, cd, warn_only, local, settings, hide # type: ignore
from fabric.contrib.project import rsync_project # type: ignore
from os.path import join as pjoin

from awstools.awstools import instances_sorted_by_avail_ip, get_run_instances_by_tag_type, get_private_ips_for_instances, launch_run_instances, wait_on_instance_launches, terminate_instances, get_instance_ids_for_instances
from util.streamlogger import StreamLogger

from typing import Dict, Optional, List, Union, TYPE_CHECKING
if TYPE_CHECKING:
    from mypy_boto3_ec2.service_resource import Instance as EC2InstanceResource
    from runtools.firesim_topology_elements import FireSimSwitchNode, FireSimServerNode

rootLogger = logging.getLogger()

def remote_kmsg(message: str) -> None:
    """ This will let you write whatever is passed as message into the kernel
    log of the remote machine.  Useful for figuring what the manager is doing
    w.r.t output from kernel stuff on the remote node. """
    commd = """echo '{}' | sudo tee /dev/kmsg""".format(message)
    run(commd, shell=True)

class MockBoto3Instance:
    """ This is used for testing without actually launching instances. """

    # don't use 0 unless you want stuff copied to your own instance.
    base_ip: int = 1
    ip_addr_int: int
    private_ip_address: str

    def __init__(self) -> None:
        self.ip_addr_int = MockBoto3Instance.base_ip
        MockBoto3Instance.base_ip += 1
        self.private_ip_address = ".".join([str((self.ip_addr_int >> (8*x)) & 0xFF) for x in [3, 2, 1, 0]])


class NBDTracker:
    """ Track allocation of NBD devices on an instance. Used for mounting
    qcow2 images."""

    # max number of NBDs allowed by the nbd.ko kernel module
    NBDS_MAX: int = 128
    unallocd: List[str]
    allocated_dict: Dict[str, str]

    def __init__(self) -> None:
        self.unallocd = ["""/dev/nbd{}""".format(x) for x in range(self.NBDS_MAX)]

        # this is a mapping from .qcow2 image name to nbd device.
        self.allocated_dict = {}

    def get_nbd_for_imagename(self, imagename: str) -> str:
        """ Call this when you need to allocate an nbd for a particular image,
        or when you need to know what nbd device is for that image.

        This will allocate an nbd for an image if one does not already exist.

        THIS DOES NOT CALL qemu-nbd to actually connect the image to the device"""
        if imagename not in self.allocated_dict.keys():
            # otherwise, allocate it
            assert len(self.unallocd) >= 1, "No NBDs left to allocate on this instance."
            self.allocated_dict[imagename] = self.unallocd.pop(0)

        return self.allocated_dict[imagename]


class EC2Inst:
    # TODO: this is leftover from when we could only support switch slots.
    # This can be removed once self.switch_slots is dynamically allocated.
    # Just make it arbitrarily large for now.
    SWITCH_SLOTS: int = 100000
    boto3_instance_object: Optional[Union[EC2InstanceResource, MockBoto3Instance]]
    switch_slots: List[FireSimSwitchNode]
    instance_deploy_manager: InstanceDeployManager
    _next_port: int
    nbd_tracker: NBDTracker

    def __init__(self) -> None:
        self.boto3_instance_object = None
        self.switch_slots = []
        self.instance_deploy_manager = InstanceDeployManager(self)
        self._next_port = 10000 # track ports to allocate for server switch model ports
        self.nbd_tracker = NBDTracker()

    def assign_boto3_instance_object(self, boto3obj: Union[EC2InstanceResource, MockBoto3Instance]) -> None:
        self.boto3_instance_object = boto3obj

    def is_bound_to_real_instance(self) -> bool:
        return self.boto3_instance_object is not None

    def get_private_ip(self) -> str:
        assert self.boto3_instance_object is not None
        return self.boto3_instance_object.private_ip_address

    def add_switch(self, firesimswitchnode: FireSimSwitchNode) -> None:
        """ Add a switch to the next available switch slot. """
        assert len(self.switch_slots) < self.SWITCH_SLOTS
        self.switch_slots.append(firesimswitchnode)
        firesimswitchnode.assign_host_instance(self)

    def allocate_host_port(self) -> int:
        """ Allocate a port to use for something on the host. Successive calls
        will return a new port. """
        retport = self._next_port
        assert retport < 11000, "Exceeded number of ports used on host. You will need to modify your security groups to increase this value."
        self._next_port += 1
        return retport

class F1_Instance(EC2Inst):
    FPGA_SLOTS: int = 0
    fpga_slots: List[FireSimServerNode]

    def __init__(self) -> None:
        super().__init__()
        self.fpga_slots = []

    def get_num_fpga_slots_max(self) -> int:
        """ Get the number of fpga slots. """
        return self.FPGA_SLOTS

    def add_simulation(self, firesimservernode: FireSimServerNode) -> None:
        """ Add a simulation to the next available slot. """
        assert len(self.fpga_slots) < self.FPGA_SLOTS
        self.fpga_slots.append(firesimservernode)
        firesimservernode.assign_host_instance(self)

class F1_16(F1_Instance):
    instance_counter: int = 0
    FPGA_SLOTS: int = 8

    def __init__(self) -> None:
        super().__init__()
        self.instance_id = F1_16.instance_counter
        F1_16.instance_counter += 1

class F1_4(F1_Instance):
    instance_counter: int = 0
    FPGA_SLOTS: int = 2

    def __init__(self) -> None:
        super().__init__()
        self.instance_id = F1_4.instance_counter
        F1_4.instance_counter += 1

class F1_2(F1_Instance):
    instance_counter: int = 0
    FPGA_SLOTS: int = 1

    def __init__(self) -> None:
        super().__init__()
        self.instance_id = F1_2.instance_counter
        F1_2.instance_counter += 1

class M4_16(EC2Inst):
    instance_counter: int = 0

    def __init__(self) -> None:
        super().__init__()
        self.instance_id = M4_16.instance_counter
        M4_16.instance_counter += 1

class RunFarm:
    """ This manages the set of AWS resources requested for the run farm. It
    essentially decouples launching instances from assigning them to simulations.

    This way, you can assign "instances" to simulations first, and then assign
    the real instance ids to the instance objects managed here."""
    f1_16s: List[F1_16]
    f1_4s: List[F1_4]
    f1_2s: List[F1_2]
    m4_16s: List[M4_16]
    runfarmtag: str
    run_instance_market: str
    spot_interruption_behavior: str
    spot_max_price: str
    launch_timeout: timedelta
    always_expand: bool

    def __init__(self, num_f1_16: int, num_f1_4: int, num_f1_2: int, num_m4_16: int, runfarmtag: str,
            run_instance_market: str, spot_interruption_behavior: str,
            spot_max_price: str, launch_timeout: timedelta, always_expand: bool):
        self.f1_16s = [F1_16() for x in range(num_f1_16)]
        self.f1_4s = [F1_4() for x in range(num_f1_4)]
        self.f1_2s = [F1_2() for x in range(num_f1_2)]
        self.m4_16s = [M4_16() for x in range(num_m4_16)]

        self.runfarmtag = runfarmtag
        self.run_instance_market = run_instance_market
        self.spot_interruption_behavior = spot_interruption_behavior
        self.spot_max_price = spot_max_price

        self.launch_timeout = launch_timeout
        self.always_expand = always_expand

    def bind_mock_instances_to_objects(self) -> None:
        """ Only used for testing. Bind mock Boto3 instances to objects. """
        for index in range(len(self.f1_16s)):
            self.f1_16s[index].assign_boto3_instance_object(MockBoto3Instance())

        for index in range(len(self.f1_4s)):
            self.f1_4s[index].assign_boto3_instance_object(MockBoto3Instance())

        for index in range(len(self.f1_2s)):
            self.f1_2s[index].assign_boto3_instance_object(MockBoto3Instance())

        for index in range(len(self.m4_16s)):
            self.m4_16s[index].assign_boto3_instance_object(MockBoto3Instance())

    def bind_real_instances_to_objects(self) -> None:
        """ Attach running instances to the Run Farm. """
        # fetch instances based on tag,
        # populate IP addr list for use in the rest of our tasks.
        # we always sort by private IP when handling instances
        available_f1_16_instances = instances_sorted_by_avail_ip(get_run_instances_by_tag_type(
            self.runfarmtag, 'f1.16xlarge'))
        available_f1_4_instances = instances_sorted_by_avail_ip(get_run_instances_by_tag_type(
            self.runfarmtag, 'f1.4xlarge'))
        available_m4_16_instances = instances_sorted_by_avail_ip(get_run_instances_by_tag_type(
            self.runfarmtag, 'm4.16xlarge'))
        available_f1_2_instances = instances_sorted_by_avail_ip(get_run_instances_by_tag_type(
            self.runfarmtag, 'f1.2xlarge'))

        message = """Insufficient {}. Did you run `firesim launchrunfarm`?"""
        # confirm that we have the correct number of instances
        if not (len(available_f1_16_instances) >= len(self.f1_16s)):
            rootLogger.warning(message.format("f1.16xlarges"))
        if not (len(available_f1_4_instances) >= len(self.f1_4s)):
            rootLogger.warning(message.format("f1.4xlarges"))
        if not (len(available_f1_2_instances) >= len(self.f1_2s)):
            rootLogger.warning(message.format("f1.2xlarges"))
        if not (len(available_m4_16_instances) >= len(self.m4_16s)):
            rootLogger.warning(message.format("m4.16xlarges"))

        ipmessage = """Using {} instances with IPs:\n{}"""
        rootLogger.debug(ipmessage.format("f1.16xlarge", str(get_private_ips_for_instances(available_f1_16_instances))))
        rootLogger.debug(ipmessage.format("f1.4xlarge", str(get_private_ips_for_instances(available_f1_4_instances))))
        rootLogger.debug(ipmessage.format("f1.2xlarge", str(get_private_ips_for_instances(available_f1_2_instances))))
        rootLogger.debug(ipmessage.format("m4.16xlarge", str(get_private_ips_for_instances(available_m4_16_instances))))

        # assign boto3 instance objects to our instance objects
        for index, instance in enumerate(available_f1_16_instances):
            self.f1_16s[index].assign_boto3_instance_object(instance)

        for index, instance in enumerate(available_f1_4_instances):
            self.f1_4s[index].assign_boto3_instance_object(instance)

        for index, instance in enumerate(available_m4_16_instances):
            self.m4_16s[index].assign_boto3_instance_object(instance)

        for index, instance in enumerate(available_f1_2_instances):
            self.f1_2s[index].assign_boto3_instance_object(instance)


    def launch_run_farm(self) -> None:
        """ Launch the run farm. """
        runfarmtag = self.runfarmtag
        runinstancemarket = self.run_instance_market
        spotinterruptionbehavior = self.spot_interruption_behavior
        spotmaxprice = self.spot_max_price

        num_f1_16xlarges = len(self.f1_16s)
        num_f1_4xlarges = len(self.f1_4s)
        num_f1_2xlarges = len(self.f1_2s)
        num_m4_16xlarges = len(self.m4_16s)

        timeout = self.launch_timeout
        always_expand = self.always_expand

        # actually launch the instances
        f1_16s = launch_run_instances('f1.16xlarge', num_f1_16xlarges, runfarmtag,
                                      runinstancemarket, spotinterruptionbehavior,
                                      spotmaxprice, timeout, always_expand)
        f1_4s = launch_run_instances('f1.4xlarge', num_f1_4xlarges, runfarmtag,
                                     runinstancemarket, spotinterruptionbehavior,
                                     spotmaxprice, timeout, always_expand)
        m4_16s = launch_run_instances('m4.16xlarge', num_m4_16xlarges, runfarmtag,
                                      runinstancemarket, spotinterruptionbehavior,
                                      spotmaxprice, timeout, always_expand)
        f1_2s = launch_run_instances('f1.2xlarge', num_f1_2xlarges, runfarmtag,
                                     runinstancemarket, spotinterruptionbehavior,
                                     spotmaxprice, timeout, always_expand)

        # wait for instances to get to running state, so that they have been
        # assigned IP addresses
        wait_on_instance_launches(f1_16s, 'f1.16xlarges')
        wait_on_instance_launches(f1_4s, 'f1.4xlarges')
        wait_on_instance_launches(m4_16s, 'm4.16xlarges')
        wait_on_instance_launches(f1_2s, 'f1.2xlarges')


    def terminate_run_farm(self, terminatesomef1_16: int, terminatesomef1_4: int, terminatesomef1_2: int,
            terminatesomem4_16: int, forceterminate: bool):
        runfarmtag = self.runfarmtag

        # get instances that belong to the run farm. sort them in case we're only
        # terminating some, to try to get intra-availability-zone locality
        f1_16_instances = instances_sorted_by_avail_ip(
            get_run_instances_by_tag_type(runfarmtag, 'f1.16xlarge'))
        f1_4_instances = instances_sorted_by_avail_ip(
            get_run_instances_by_tag_type(runfarmtag, 'f1.4xlarge'))
        m4_16_instances = instances_sorted_by_avail_ip(
            get_run_instances_by_tag_type(runfarmtag, 'm4.16xlarge'))
        f1_2_instances = instances_sorted_by_avail_ip(
            get_run_instances_by_tag_type(runfarmtag, 'f1.2xlarge'))

        f1_16_instance_ids = get_instance_ids_for_instances(f1_16_instances)
        f1_4_instance_ids = get_instance_ids_for_instances(f1_4_instances)
        m4_16_instance_ids = get_instance_ids_for_instances(m4_16_instances)
        f1_2_instance_ids = get_instance_ids_for_instances(f1_2_instances)

        argsupplied_f116 = terminatesomef1_16 != -1
        argsupplied_f14 = terminatesomef1_4 != -1
        argsupplied_f12 = terminatesomef1_2 != -1
        argsupplied_m416 = terminatesomem4_16 != -1

        if argsupplied_f116 or argsupplied_f14 or argsupplied_f12 or argsupplied_m416:
            # In this mode, only terminate instances that are specifically supplied.
            if argsupplied_f116 and terminatesomef1_16 != 0:
                # grab the last N instances to terminate
                f1_16_instance_ids = f1_16_instance_ids[-terminatesomef1_16:]
            else:
                f1_16_instance_ids = []

            if argsupplied_f14 and terminatesomef1_4 != 0:
                # grab the last N instances to terminate
                f1_4_instance_ids = f1_4_instance_ids[-terminatesomef1_4:]
            else:
                f1_4_instance_ids = []

            if argsupplied_f12 and terminatesomef1_2 != 0:
                # grab the last N instances to terminate
                f1_2_instance_ids = f1_2_instance_ids[-terminatesomef1_2:]
            else:
                f1_2_instance_ids = []

            if argsupplied_m416 and terminatesomem4_16 != 0:
                # grab the last N instances to terminate
                m4_16_instance_ids = m4_16_instance_ids[-terminatesomem4_16:]
            else:
                m4_16_instance_ids = []

        rootLogger.critical("IMPORTANT!: This will terminate the following instances:")
        rootLogger.critical("f1.16xlarges")
        rootLogger.critical(f1_16_instance_ids)
        rootLogger.critical("f1.4xlarges")
        rootLogger.critical(f1_4_instance_ids)
        rootLogger.critical("m4.16xlarges")
        rootLogger.critical(m4_16_instance_ids)
        rootLogger.critical("f1.2xlarges")
        rootLogger.critical(f1_2_instance_ids)

        if not forceterminate:
            # --forceterminate was not supplied, so confirm with the user
            userconfirm = input("Type yes, then press enter, to continue. Otherwise, the operation will be cancelled.\n")
        else:
            userconfirm = "yes"

        if userconfirm == "yes":
            if len(f1_16_instance_ids) != 0:
                terminate_instances(f1_16_instance_ids, False)
            if len(f1_4_instance_ids) != 0:
                terminate_instances(f1_4_instance_ids, False)
            if len(m4_16_instance_ids) != 0:
                terminate_instances(m4_16_instance_ids, False)
            if len(f1_2_instance_ids) != 0:
                terminate_instances(f1_2_instance_ids, False)
            rootLogger.critical("Instances terminated. Please confirm in your AWS Management Console.")
        else:
            rootLogger.critical("Termination cancelled.")

    def get_all_host_nodes(self) -> List[EC2Inst]:
        """ Get objects for all host nodes in the run farm that are bound to
        a real instance. """
        allinsts: List[EC2Inst] = [*self.f1_16s, *self.f1_2s, *self.f1_4s, *self.m4_16s]
        return [inst for inst in allinsts if inst.is_bound_to_real_instance()]

    def lookup_by_ip_addr(self, ipaddr: str) -> EC2Inst:
        """ Get an instance object from its IP address. """
        for host_node in self.get_all_host_nodes():
            if host_node.get_private_ip() == ipaddr:
                return host_node
        assert False, f"Unable to find host node by {ipaddr} host name"

class InstanceDeployManager:
    """  This class manages actually deploying/running stuff based on the
    definition of an instance and the simulations/switches assigned to it.

    This is in charge of managing the locations of stuff on remote nodes.
    """
    parentnode: EC2Inst

    def __init__(self, parentnode: EC2Inst) -> None:
        self.parentnode = parentnode

    def instance_logger(self, logstr: str) -> None:
        rootLogger.info("""[{}] """.format(env.host_string) + logstr)

    def get_and_install_aws_fpga_sdk(self) -> None:
        """ Installs the aws-sdk. This gets us access to tools to flash the fpga. """

        with prefix('cd ../'), \
             StreamLogger('stdout'), \
             StreamLogger('stderr'):
            # use local version of aws_fpga on runfarm nodes
            aws_fpga_upstream_version = local('git -C platforms/f1/aws-fpga describe --tags --always --dirty', capture=True)
            if "-dirty" in aws_fpga_upstream_version:
                rootLogger.critical("Unable to use local changes to aws-fpga. Continuing without them.")
        self.instance_logger("""Installing AWS FPGA SDK on remote nodes. Upstream hash: {}""".format(aws_fpga_upstream_version))
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run('git clone https://github.com/aws/aws-fpga')
            run('cd aws-fpga && git checkout ' + aws_fpga_upstream_version)
        with cd('/home/centos/aws-fpga'), StreamLogger('stdout'), StreamLogger('stderr'):
            run('source sdk_setup.sh')

    def fpga_node_xdma(self) -> None:
        """ Copy XDMA infra to remote node. This assumes that the driver was
        already built and that a binary exists in the directory on this machine
        """
        self.instance_logger("""Copying AWS FPGA XDMA driver to remote node.""")
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run('mkdir -p /home/centos/xdma/')
            put('../platforms/f1/aws-fpga/sdk/linux_kernel_drivers',
                '/home/centos/xdma/', mirror_local_mode=True)
            with cd('/home/centos/xdma/linux_kernel_drivers/xdma/'), \
                 prefix("export PATH=/usr/bin:$PATH"):
		 # prefix only needed if conda env is earlier in PATH
		 # see build-setup-nolog.sh for explanation.
                run('make clean')
                run('make')

    def fpga_node_qcow(self) -> None:
        """ Install qemu-img management tools and copy NBD infra to remote
        node. This assumes that the kernel module was already built and exists
        in the directory on this machine.
        """
        self.instance_logger("""Setting up remote node for qcow2 disk images.""")
        with StreamLogger('stdout'), StreamLogger('stderr'):
            # get qemu-nbd
            ### XXX Centos Specific
            run('sudo yum -y install qemu-img')
            # copy over kernel module
            put('../build/nbd.ko', '/home/centos/nbd.ko', mirror_local_mode=True)

    def load_nbd_module(self) -> None:
        """ load the nbd module. always unload the module first to ensure it
        is in a clean state. """
        self.unload_nbd_module()
        # now load xdma
        self.instance_logger("Loading NBD Kernel Module.")
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""sudo insmod /home/centos/nbd.ko nbds_max={}""".format(self.parentnode.nbd_tracker.NBDS_MAX))

    def unload_nbd_module(self) -> None:
        """ unload the nbd module. """
        self.instance_logger("Unloading NBD Kernel Module.")

        # disconnect all /dev/nbdX devices before rmmod
        self.disconnect_all_nbds_instance()
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run('sudo rmmod nbd')

    def disconnect_all_nbds_instance(self) -> None:
        """ Disconnect all nbds on the instance. """
        self.instance_logger("Disconnecting all NBDs.")

        # warn_only, so we can call this even if there are no nbds
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            # build up one large command with all the disconnects
            fullcmd = []
            for nbd_index in range(self.parentnode.nbd_tracker.NBDS_MAX):
                fullcmd.append("""sudo qemu-nbd -d /dev/nbd{nbdno}""".format(nbdno=nbd_index))

            run("; ".join(fullcmd))

    def unload_xrt_and_xocl(self) -> None:
        self.instance_logger("Unloading XRT-related Kernel Modules.")

        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            # fpga mgmt tools seem to force load xocl after a flash now...
            # so we just remove everything for good measure:
            remote_kmsg("removing_xrt_start")
            run('sudo systemctl stop mpd')
            run('sudo yum remove -y xrt xrt-aws')
            remote_kmsg("removing_xrt_end")

    def unload_xdma(self) -> None:
        self.instance_logger("Unloading XDMA Driver Kernel Module.")

        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            # fpga mgmt tools seem to force load xocl after a flash now...
            # so we just remove everything for good measure:
            remote_kmsg("removing_xdma_start")
            run('sudo rmmod xdma')
            remote_kmsg("removing_xdma_end")

        #self.instance_logger("Waiting 10 seconds after removing kernel modules (esp. xocl).")
        #time.sleep(10)

    def clear_fpgas(self) -> None:
        # we always clear ALL fpga slots
        assert isinstance(self.parentnode, F1_Instance)

        for slotno in range(self.parentnode.get_num_fpga_slots_max()):
            self.instance_logger("""Clearing FPGA Slot {}.""".format(slotno))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                remote_kmsg("""about_to_clear_fpga{}""".format(slotno))
                run("""sudo fpga-clear-local-image -S {} -A""".format(slotno))
                remote_kmsg("""done_clearing_fpga{}""".format(slotno))

        for slotno in range(self.parentnode.get_num_fpga_slots_max()):
            self.instance_logger("""Checking for Cleared FPGA Slot {}.""".format(slotno))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                remote_kmsg("""about_to_check_clear_fpga{}""".format(slotno))
                run("""until sudo fpga-describe-local-image -S {} -R -H | grep -q "cleared"; do  sleep 1;  done""".format(slotno))
                remote_kmsg("""done_checking_clear_fpga{}""".format(slotno))


    def flash_fpgas(self) -> None:
        assert isinstance(self.parentnode, F1_Instance)

        dummyagfi = None

        for slotno, firesimservernode in enumerate(self.parentnode.fpga_slots):
            agfi = firesimservernode.get_agfi()
            dummyagfi = agfi
            self.instance_logger("""Flashing FPGA Slot: {} with agfi: {}.""".format(slotno, agfi))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("""sudo fpga-load-local-image -S {} -I {} -A""".format(
                    slotno, agfi))

        # We only do this because XDMA hangs if some of the FPGAs on the instance
        # are left in the cleared state. So, if you're only using some of the
        # FPGAs on an instance, we flash the rest with one of your images
        # anyway. Since the only interaction we have with an FPGA right now
        # is over PCIe where the software component is mastering, this can't
        # break anything.
        for slotno in range(len(self.parentnode.fpga_slots), self.parentnode.get_num_fpga_slots_max()):
            self.instance_logger("""Flashing FPGA Slot: {} with dummy agfi: {}.""".format(slotno, dummyagfi))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("""sudo fpga-load-local-image -S {} -I {} -A""".format(
                    slotno, dummyagfi))

        for slotno, firesimservernode in enumerate(self.parentnode.fpga_slots):
            self.instance_logger("""Checking for Flashed FPGA Slot: {} with agfi: {}.""".format(slotno, agfi))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("""until sudo fpga-describe-local-image -S {} -R -H | grep -q "loaded"; do  sleep 1;  done""".format(slotno))

        for slotno in range(len(self.parentnode.fpga_slots), self.parentnode.get_num_fpga_slots_max()):
            self.instance_logger("""Checking for Flashed FPGA Slot: {} with agfi: {}.""".format(slotno, dummyagfi))
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("""until sudo fpga-describe-local-image -S {} -R -H | grep -q "loaded"; do  sleep 1;  done""".format(slotno))


    def load_xdma(self) -> None:
        """ load the xdma kernel module. """
        # fpga mgmt tools seem to force load xocl after a flash now...
        # xocl conflicts with the xdma driver, which we actually want to use
        # so we just remove everything for good measure before loading xdma:
        self.unload_xdma()
        # now load xdma
        self.instance_logger("Loading XDMA Driver Kernel Module.")
        # TODO: can make these values automatically be chosen based on link lat
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("sudo insmod /home/centos/xdma/linux_kernel_drivers/xdma/xdma.ko poll_mode=1")

    def start_ila_server(self) -> None:
        """ start the vivado hw_server and virtual jtag on simulation instance.) """
        self.instance_logger("Starting Vivado hw_server.")
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""screen -S hw_server -d -m bash -c "script -f -c 'hw_server'"; sleep 1""")
        self.instance_logger("Starting Vivado virtual JTAG.")
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""screen -S virtual_jtag -d -m bash -c "script -f -c 'sudo fpga-start-virtual-jtag -P 10201 -S 0'"; sleep 1""")

    def kill_ila_server(self) -> None:
        """ Kill the vivado hw_server and virtual jtag """
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run("sudo pkill -SIGKILL hw_server")
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run("sudo pkill -SIGKILL fpga-local-cmd")

    def copy_sim_slot_infrastructure(self, slotno: int) -> None:
        """ copy all the simulation infrastructure to the remote node. """
        assert isinstance(self.parentnode, F1_Instance)
        assert slotno < len(self.parentnode.fpga_slots)

        serv = self.parentnode.fpga_slots[slotno]

        self.instance_logger("""Copying FPGA simulation infrastructure for slot: {}.""".format(slotno))

        remote_sim_dir = """/home/centos/sim_slot_{}/""".format(slotno)
        remote_sim_rsync_dir = remote_sim_dir + "rsyncdir/"
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""mkdir -p {}""".format(remote_sim_rsync_dir))

        files_to_copy = serv.get_required_files_local_paths()
        for local_path, remote_path in files_to_copy:
            with StreamLogger('stdout'), StreamLogger('stderr'):
                # -z --inplace
                rsync_cap = rsync_project(local_dir=local_path, remote_dir=pjoin(remote_sim_rsync_dir, remote_path),
                            ssh_opts="-o StrictHostKeyChecking=no", extra_opts="-L", capture=True)
                rootLogger.debug(rsync_cap)
                rootLogger.debug(rsync_cap.stderr)

        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""cp -r {}/* {}/""".format(remote_sim_rsync_dir, remote_sim_dir), shell=True)


    def copy_switch_slot_infrastructure(self, switchslot: int) -> None:
        assert isinstance(self.parentnode, M4_16)
        assert switchslot < len(self.parentnode.switch_slots)

        self.instance_logger("""Copying switch simulation infrastructure for switch slot: {}.""".format(switchslot))

        remote_switch_dir = """/home/centos/switch_slot_{}/""".format(switchslot)
        with StreamLogger('stdout'), StreamLogger('stderr'):
            run("""mkdir -p {}""".format(remote_switch_dir))

        switch = self.parentnode.switch_slots[switchslot]
        files_to_copy = switch.get_required_files_local_paths()
        for local_path, remote_path in files_to_copy:
            with StreamLogger('stdout'), StreamLogger('stderr'):
                put(local_path, pjoin(remote_switch_dir, remote_path), mirror_local_mode=True)

    def start_switch_slot(self, switchslot: int) -> None:
        assert isinstance(self.parentnode, M4_16)
        assert switchslot < len(self.parentnode.switch_slots)

        self.instance_logger("""Starting switch simulation for switch slot: {}.""".format(switchslot))
        remote_switch_dir = """/home/centos/switch_slot_{}/""".format(switchslot)
        switch = self.parentnode.switch_slots[switchslot]
        with cd(remote_switch_dir), StreamLogger('stdout'), StreamLogger('stderr'):
            run(switch.get_switch_start_command())

    def start_sim_slot(self, slotno: int) -> None:
        assert isinstance(self.parentnode, F1_Instance)
        assert slotno < len(self.parentnode.fpga_slots)

        self.instance_logger("""Starting FPGA simulation for slot: {}.""".format(slotno))
        remote_sim_dir = """/home/centos/sim_slot_{}/""".format(slotno)
        server = self.parentnode.fpga_slots[slotno]
        with cd(remote_sim_dir), StreamLogger('stdout'), StreamLogger('stderr'):
            server.run_sim_start_command(slotno)

    def kill_switch_slot(self, switchslot: int) -> None:
        """ kill the switch in slot switchslot. """
        assert isinstance(self.parentnode, M4_16)
        assert switchslot < len(self.parentnode.switch_slots)

        self.instance_logger("""Killing switch simulation for switchslot: {}.""".format(switchslot))
        switch = self.parentnode.switch_slots[switchslot]
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run(switch.get_switch_kill_command())

    def kill_sim_slot(self, slotno: int) -> None:
        assert isinstance(self.parentnode, F1_Instance)
        assert slotno < len(self.parentnode.fpga_slots)

        self.instance_logger("""Killing FPGA simulation for slot: {}.""".format(slotno))
        server = self.parentnode.fpga_slots[slotno]
        with warn_only(), StreamLogger('stdout'), StreamLogger('stderr'):
            run(server.get_sim_kill_command(slotno))

    def instance_assigned_simulations(self) -> bool:
        """ return true if this instance has any assigned fpga simulations. """
        if isinstance(self.parentnode, F1_Instance):
            if len(self.parentnode.fpga_slots) > 0:
                return True
        return False

    def instance_assigned_switches(self) -> bool:
        """ return true if this instance has any assigned switch simulations. """
        if len(self.parentnode.switch_slots) > 0:
            return True
        return False

    def infrasetup_instance(self) -> None:
        """ Handle infrastructure setup for this instance. """
        # check if fpga node
        if self.instance_assigned_simulations():
            # This is an FPGA-host node.
            assert isinstance(self.parentnode, F1_Instance)

            # copy fpga sim infrastructure
            for slotno in range(len(self.parentnode.fpga_slots)):
                self.copy_sim_slot_infrastructure(slotno)

            self.get_and_install_aws_fpga_sdk()
            # unload any existing edma/xdma/xocl
            self.unload_xrt_and_xocl()
            # copy xdma driver
            self.fpga_node_xdma()
            # load xdma
            self.load_xdma()

            # setup nbd/qcow infra
            self.fpga_node_qcow()
            # load nbd module
            self.load_nbd_module()

            # clear/flash fpgas
            self.clear_fpgas()
            self.flash_fpgas()

            # re-load XDMA
            self.load_xdma()

            #restart (or start form scratch) ila server
            self.kill_ila_server()
            self.start_ila_server()

        if self.instance_assigned_switches():
            # all nodes could have a switch
            for slotno in range(len(self.parentnode.switch_slots)):
                self.copy_switch_slot_infrastructure(slotno)


    def start_switches_instance(self) -> None:
        """ Boot up all the switches in a screen. """
        # remove shared mem pages used by switches
        if self.instance_assigned_switches():
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("sudo rm -rf /dev/shm/*")

            for slotno in range(len(self.parentnode.switch_slots)):
                self.start_switch_slot(slotno)

    def start_simulations_instance(self) -> None:
        """ Boot up all the sims in a screen. """
        if self.instance_assigned_simulations():
            assert isinstance(self.parentnode, F1_Instance)

            # only on sim nodes
            for slotno in range(len(self.parentnode.fpga_slots)):
                self.start_sim_slot(slotno)

    def kill_switches_instance(self) -> None:
        """ Kill all the switches on this instance. """
        if self.instance_assigned_switches():
            for slotno in range(len(self.parentnode.switch_slots)):
                self.kill_switch_slot(slotno)
            with StreamLogger('stdout'), StreamLogger('stderr'):
                run("sudo rm -rf /dev/shm/*")

    def kill_simulations_instance(self, disconnect_all_nbds: bool = True) -> None:
        """ Kill all simulations on this instance. """
        if self.instance_assigned_simulations():
            assert isinstance(self.parentnode, F1_Instance)
            # only on sim nodes
            for slotno in range(len(self.parentnode.fpga_slots)):
                self.kill_sim_slot(slotno)
        if disconnect_all_nbds:
            # disconnect all NBDs
            self.disconnect_all_nbds_instance()

    def running_simulations(self) -> Dict[str, List[str]]:
        """ collect screen results from node to see what's running on it. """
        simdrivers = []
        switches = []
        with settings(warn_only=True), hide('everything'):
            collect = run('screen -ls')
            for line in collect.splitlines():
                if "(Detached)" in line or "(Attached)" in line:
                    line_stripped = line.strip()
                    if "fsim" in line:
                        search = re.search('fsim([0-9][0-9]*)', line_stripped)
                        assert search is not None
                        line_stripped = search.group(0)
                        line_stripped = line_stripped.replace('fsim', '')
                        simdrivers.append(line_stripped)
                    elif "switch" in line:
                        search = re.search('switch([0-9][0-9]*)', line_stripped)
                        assert search is not None
                        line_stripped = search.group(0)
                        switches.append(line_stripped)
        return {'switches': switches, 'simdrivers': simdrivers}

    def monitor_jobs_instance(self, completed_jobs: List[str], teardown: bool, terminateoncompletion: bool,
            job_results_dir: str) -> Dict[str, Dict[str, bool]]:
        """ Job monitoring for this instance. """
        # make a local copy of completed_jobs, so that we can update it
        completed_jobs = list(completed_jobs)

        rootLogger.debug("completed jobs " + str(completed_jobs))

        if not self.instance_assigned_simulations() and self.instance_assigned_switches():
            # this node hosts ONLY switches and not fpga sims
            #
            # just confirm that our switches are still running
            # switches will never trigger shutdown in the cycle-accurate -
            # they should run forever until torn down
            if teardown:
                # handle the case where we're just tearing down nodes that have
                # ONLY switches
                for counter in range(len(self.parentnode.switch_slots)):
                    switchsim = self.parentnode.switch_slots[counter]
                    switchsim.copy_back_switchlog_from_run(job_results_dir, counter)

                if terminateoncompletion:
                    # terminate the instance since teardown is called and instance
                    # termination is enabled
                    assert isinstance(self.parentnode.boto3_instance_object, EC2InstanceResource)
                    instanceids = get_instance_ids_for_instances([self.parentnode.boto3_instance_object])
                    terminate_instances(instanceids, dryrun=False)

                # don't really care about the return val in the teardown case
                return {'switches': dict(), 'sims': dict()}

            # not teardown - just get the status of the switch sims
            switchescompleteddict = {k: False for k in self.running_simulations()['switches']}
            for switchsim in self.parentnode.switch_slots:
                swname = switchsim.switch_builder.switch_binary_name()
                if swname not in switchescompleteddict.keys():
                    switchescompleteddict[swname] = True
            return {'switches': switchescompleteddict, 'sims': dict()}

        if self.instance_assigned_simulations():
            # this node has fpga sims attached
            assert isinstance(self.parentnode, F1_Instance)

            # first, figure out which jobs belong to this instance.
            # if they are all completed already. RETURN, DON'T TRY TO DO ANYTHING
            # ON THE INSTANCE.
            parentslots = self.parentnode.fpga_slots
            rootLogger.debug("parentslots " + str(parentslots))
            jobnames = [slot.get_job_name() for slot in parentslots]
            rootLogger.debug("jobnames " + str(jobnames))
            already_done = all([job in completed_jobs for job in jobnames])
            rootLogger.debug("already done? " + str(already_done))
            if already_done:
                # in this case, all of the nodes jobs have already completed. do nothing.
                # this can never happen in the cycle-accurate case at a point where we care
                # about switch status, so don't bother to populate it
                jobnames_to_completed = {jname: True for jname in jobnames}
                return {'sims': jobnames_to_completed, 'switches': dict()}

            # at this point, all jobs are NOT completed. so, see how they're doing now:
            instance_screen_status = self.running_simulations()
            switchescompleteddict = {k: False for k in instance_screen_status['switches']}

            if self.instance_assigned_switches():
                # fill in whether switches have terminated for some reason
                for switchsim in self.parentnode.switch_slots:
                    swname = switchsim.switch_builder.switch_binary_name()
                    if swname not in switchescompleteddict.keys():
                        switchescompleteddict[swname] = True

            slotsrunning = [x for x in instance_screen_status['simdrivers']]

            rootLogger.debug("slots running")
            rootLogger.debug(slotsrunning)
            for slotno, jobname in enumerate(jobnames):
                if str(slotno) not in slotsrunning and jobname not in completed_jobs:
                    assert slotno < len(parentslots)
                    self.instance_logger("Slot " + str(slotno) + " completed! copying results.")
                    # NOW, we must copy off the results of this sim, since it just exited
                    parentslots[slotno].copy_back_job_results_from_run(slotno)
                    # add our job to our copy of completed_jobs, so that next,
                    # we can test again to see if this instance is "done" and
                    # can be terminated
                    completed_jobs.append(jobname)

            # determine if we're done now.
            jobs_done_q = {job: job in completed_jobs for job in jobnames}
            now_done = all(jobs_done_q.values())
            rootLogger.debug("now done: " + str(now_done))
            if now_done and self.instance_assigned_switches():
                # we're done AND we have switches running here, so kill them,
                # then copy off their logs. this handles the case where you
                # have a node with one simulation and some switches, to make
                # sure the switch logs are copied off.
                #
                # the other cases are when you have multiple sims and a cycle-acc network,
                # in which case the all() will never actually happen (unless someone builds
                # a workload where two sims exit at exactly the same time, which we should
                # advise users not to do)
                #
                # a last use case is when there's no network, in which case
                # instance_assigned_switches won't be true, so this won't be called

                self.kill_switches_instance()

                for counter, switchsim in enumerate(self.parentnode.switch_slots):
                    switchsim.copy_back_switchlog_from_run(job_results_dir, counter)

            if now_done and terminateoncompletion:
                # terminate the instance since everything is done and instance
                # termination is enabled
                assert isinstance(self.parentnode.boto3_instance_object, EC2InstanceResource)
                instanceids = get_instance_ids_for_instances([self.parentnode.boto3_instance_object])
                terminate_instances(instanceids, dryrun=False)

            return {'switches': switchescompleteddict, 'sims': jobs_done_q}

        assert False, "Instance must host switch slots and/or FPGA slots"
