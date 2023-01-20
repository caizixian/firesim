// See LICENSE for license details.

#include "BridgeHarness.h"
#include "bridges/blockdev.h"
#include "bridges/uart.h"
#include "core/bridge_driver.h"

#include <limits>

BridgeHarness::BridgeHarness(const std::vector<std::string> &args, simif_t &sim)
    : simulation_t(sim, args) {}

BridgeHarness::~BridgeHarness() = default;

void BridgeHarness::simulation_init() {
  for (auto &bridge : sim.get_registry().get_all_bridges()) {
    bridge->init();
  }
}

int BridgeHarness::simulation_run() {
  auto &registry = sim.get_registry();
  auto &peek_poke = registry.get_widget<peek_poke_t>();
  auto &clock = registry.get_widget<clockmodule_t>();

  // Let the design run.
  clock.credit(std::numeric_limits<uint32_t>::max());

  // Reset the DUT.
  peek_poke.poke("reset", 1, /*blocking=*/true);
  peek_poke.step(1, /*blocking=*/true);
  peek_poke.poke("reset", 0, /*blocking=*/true);
  peek_poke.step(1, /*blocking=*/true);

  // Tick until all requests are serviced.
  peek_poke.step(get_step_limit(), /*blocking=*/false);
  for (unsigned i = 0; i < get_tick_limit() && !peek_poke.is_done(); ++i) {
    for (auto &bridge : sim.get_registry().get_all_bridges()) {
      bridge->tick();
    }
  }

  // Cleanup.
  return EXIT_SUCCESS;
}

void BridgeHarness::simulation_finish() {
  for (auto &bridge : sim.get_registry().get_all_bridges()) {
    bridge->finish();
  }
}
