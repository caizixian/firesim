# See LICENSE for license details.

# DOC include start: Bridge Build System Changes
##########################
# Driver Sources & Flags #
##########################

# dromajo modifications
DROMAJO_LIB_DIR ?= $(CONDA_PREFIX)/lib
DROMAJO_INCLUDE_DIR ?= $(CONDA_PREFIX)/include

DROMAJO_LIB_NAME = dromajo_cosim

DROMAJO_H = $(GENERATED_DIR)/dromajo_params.h
DROMAJO_LONG_H = $(GENERATED_DIR)/$(long_name).dromajo_params.h

TESTCHIPIP_CSRC_DIR = $(chipyard_dir)/generators/testchipip/src/main/resources/testchipip/csrc

CHIPYARD_ROM = $(chipyard_dir)/generators/testchipip/bootrom/bootrom.rv64.img
DROMAJO_ROM = $(GENERATED_DIR)/$(long_name).rom

DTS_FILE = $(GENERATED_DIR)/$(long_name).dts
DROMAJO_DTB = $(GENERATED_DIR)/$(long_name).dtb

$(DROMAJO_LONG_H) $(DTS_FILE): $(simulator_verilog)

$(DROMAJO_H): $(DROMAJO_LONG_H)
	rm -rf $(DROMAJO_H)
	ln -s $(DROMAJO_LONG_H) $(DROMAJO_H)

$(DROMAJO_DTB): $(DTS_FILE)
	dtc -I dts -O dtb -o $(DROMAJO_DTB) $(DTS_FILE)

$(DROMAJO_ROM): $(CHIPYARD_ROM)
	rm -rf $(DROMAJO_ROM)
	ln -s $(CHIPYARD_ROM) $(DROMAJO_ROM)

DROMAJO_REQS = $(DROMAJO_H) $(DROMAJO_ROM) $(DROMAJO_DTB)

firesim_lib_dir = $(firesim_base_dir)/firesim-lib/src/main/cc
include $(firesim_base_dir)/firesim-lib/src/main/cc/Makefrag

driver_dir = $(firesim_base_dir)/src/main/cc

DRIVER_H = \
	$(shell find $(driver_dir) -name "*.h") \
	$(DROMAJO_REQS) \
	$(TESTCHIPIP_CSRC_DIR)/testchip_tsi.h

DRIVER_CC = $(driver_dir)/firesim/firesim_top.cc

# Disable missing override warning for testchipip.
DRIVER_CXX_FLAGS += \
	-isystem $(RISCV)/include \
	-isystem $(TESTCHIPIP_CSRC_DIR) \
	-isystem $(DROMAJO_INCLUDE_DIR) \
	-I$(firesim_lib_dir) \
	-I$(GENERATED_DIR) \
	-Wno-inconsistent-missing-override

DRIVER_LIBS := $(BRIDGES_LIB)
TARGET_LD_FLAGS += $(BRIDGES_LDFLAGS)

# DOC include end: Bridge Build System Changes
