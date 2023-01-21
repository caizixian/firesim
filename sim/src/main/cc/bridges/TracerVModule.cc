// See LICENSE for license details.

#include "BridgeHarness.h"
#include "TestHarness.h"
#include "bridges/peek_poke.h"
#include "bridges/tracerv.h"
#include "core/bridge_driver.h"
#include "core/simif.h"

#include <iostream>

static std::vector<bool> get_contiguous(const unsigned bits,
                                        const unsigned total) {
  std::vector<bool> ret;
  for (unsigned i = 0; i < total; i++) {
    const bool value = (i < bits);
    ret.emplace_back(value);
  }

  return ret;
}

static std::vector<uint64_t> get_iaddrs(const unsigned step,
                                        const unsigned total) {
  constexpr uint64_t offset =
      1024; // should be larger than total but doesn't really matter
  std::vector<uint64_t> ret;

  for (unsigned i = 0; i < total; i++) {
    ret.emplace_back(step * offset + i);
  }

  return ret;
}

static std::string namei(const unsigned x) {
  std::stringstream ss;
  ss << "io_insns_" << x << "_iaddr";
  return ss.str();
};

static std::string namev(const unsigned x) {
  std::stringstream ss;
  ss << "io_insns_" << x << "_valid";
  return ss.str();
};

class TracerVModule final : public simulation_t {
public:
  TracerVModule(const std::vector<std::string> &args, simif_t *simif)
      : simulation_t(*simif, args), simif(simif) {
    for (auto &arg : args) {
      if (arg.find("+seed=") == 0) {
        seed = strtoll(arg.c_str() + 6, NULL, 10);
        fprintf(stderr, "Using custom SEED: %ld\n", seed);
      }

      if (arg.find("+tracerv-expected-output=") == 0) {
        const std::string fname =
            arg.c_str() + 25; // 25 is the length of the argument to find
        expected = fopen(fname.c_str(), "w");
        if (!expected) {
          fprintf(stderr,
                  "Could not open expected test output file: %s\n",
                  fname.c_str());
          abort();
        }
      }
    }
    gen.seed(seed);
  }

  virtual ~TracerVModule() {}

  void add_bridge_driver(bridge_driver_t *bridge) {
    bridges.emplace_back(bridge);
  }

  void add_bridge_driver(peek_poke_t *bridge) { peek_poke.reset(bridge); }

  void add_bridge_driver(tracerv_t *bridge) {
    assert(!tracerv && "multiple bridges registered");
    tracerv.reset(bridge);
  }

  void simulation_init() override {
#include "core/constructor.h"
    for (auto &bridge : bridges) {
      bridge->init();
    }
    if (tracerv) {
      tracerv->init();

      // write the header to our expected test output file
      tracerv->write_header(expected);
    }
  }

  // return the final values we will feed into MMIO
  std::pair<std::vector<uint64_t>, std::vector<bool>>
  get_final_values(const unsigned tracerv_width) {

    std::vector<bool> final_valid;
    std::vector<uint64_t> final_iaddr;
    for (unsigned i = 0; i < tracerv_width; i++) {
      final_valid.emplace_back(0);
      final_iaddr.emplace_back(0xffff);
    }

    return std::make_pair(final_iaddr, final_valid);
  }

  bool steps(const unsigned s) {
    simif->take_steps(s, /*blocking=*/false);
    const unsigned timeout = 10000 + s;
    bool was_done = false;
    for (unsigned i = 0; i < timeout; i++) {

      for (auto &bridge : bridges) {
        bridge->tick();
      }
      tracerv->tick();

      if (simif->done()) {
        was_done = true;
        break;
      }
    }

    if (!was_done) {
      std::cout << "Hit timeout of " << timeout
                << " tick loops afer a requested " << s << " steps"
                << std::endl;
    }

    return was_done;
  }

  std::vector<std::pair<uint64_t, std::vector<uint64_t>>> expected_pair;

  int simulation_run() override {
    if (!tracerv) {
      std::cout << "tracerv was never set" << std::endl;
    }

    // Reset the DUT.
    peek_poke->poke("reset", 1, /*blocking=*/true);
    simif->take_steps(1, /*blocking=*/true);
    peek_poke->poke("reset", 0, /*blocking=*/true);
    simif->take_steps(1, /*blocking=*/true);

    // the value of the first cycle as returned from TracerV
    const uint64_t cycle_offset = 3;

    // modified as we go
    uint64_t e_cycle = cycle_offset;

    const unsigned tracerv_width = tracerv->max_core_ipc;

    // load MMIO and capture expected outputs
    auto load = [&](std::vector<uint64_t> iad, std::vector<bool> bit) {
      std::vector<uint64_t> valid_i;
      assert(iad.size() == bit.size());
      for (unsigned i = 0; i < iad.size(); i++) {
        // std::cout << "loading " << i << " with " << iad[i] << "," << bit[i]
        // << std::endl;
        peek_poke->poke(namei(i), iad[i], true);
        peek_poke->poke(namev(i), bit[i], true);

        // calculate what TraverV should output, and save it
        if (bit[i]) {
          valid_i.emplace_back(iad[i]);
        }
      }

      // place instructions onto the vector 7 at a time
      for(size_t i = 0; i < valid_i.size(); i += 7) {
        auto last = std::min(valid_i.size(), i + 7);
        std::vector<uint64_t> chunk = std::vector<uint64_t>(valid_i.begin() + i, valid_i.begin() + last);
        expected_pair.emplace_back(std::make_pair(e_cycle, chunk));
      }

      e_cycle++;
    };

    // loop over tests. choose random valids with a simple pattern of iaddr
    // load into MMIO, and tick the system
    for (unsigned test_step = 0; test_step < get_total_trace_tests();
         test_step++) {
      const uint64_t pull = rand_next(tracerv_width + 1);

      auto pull_iaddr = get_iaddrs(test_step, tracerv_width);
      auto pull_bits = get_contiguous(pull, tracerv_width);

      load(pull_iaddr, pull_bits);
      steps(1);
    }

    const auto &[final_iaddr, final_valid] = get_final_values(tracerv_width);

    // load final values (which are not valid and thus not checked)
    load(final_iaddr, final_valid);

    tracerv->flush();

    steps(100);

    // write out a file which contains the expected output
    write_expected_file();

    return EXIT_SUCCESS;
  }

  /**
   * Writes the contents of the member variable expected_pair
   * to the expected file pointer. This is done via the tracerv_t::serialize
   * function.
   */
  void write_expected_file() {
    uint64_t OUTBUF[8];

    for (const auto &beat : expected_pair) {
      const auto &[cycle, insns] = beat;
      memset(OUTBUF, 0, sizeof(OUTBUF));
      OUTBUF[0] = cycle;
      assert(insns.size() < 8);
      for (int i = 0; i < insns.size(); i++) {
        OUTBUF[i + 1] = insns[i] | tracerv_t::valid_mask;
      }

      // because expected_pair doesn't contain the instruction value for non
      // valid instructions only the human readable output will compare
      // correctly
      tracerv_t::serialize(OUTBUF,
                           sizeof(OUTBUF),
                           expected,
                           NULL,
                           tracerv->max_core_ipc,
                           true,
                           false,
                           false);
    }

    fclose(expected);
  }

  void simulation_finish() override {
    for (auto &bridge : bridges) {
      bridge->finish();
    }
  }

  /**
   * Returns the next available random number, modulo limit.
   */
  uint64_t rand_next(uint64_t limit) { return gen() % limit; }

private:
  simif_t *simif;
  std::vector<std::unique_ptr<bridge_driver_t>> bridges;
  std::unique_ptr<peek_poke_t> peek_poke;

  unsigned get_total_trace_tests() const { return 128; }

  std::unique_ptr<tracerv_t> tracerv;

  // random numbers
  uint64_t seed = 0;
  std::mt19937_64 gen;

  // expected test output
  FILE *expected = NULL;
};

TEST_MAIN(TracerVModule)
