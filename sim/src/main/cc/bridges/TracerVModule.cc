// See LICENSE for license details.

#ifndef RTLSIM
#include "simif_f1.h"
#define SIMIF simif_f1_t
#else
#include "simif_emul.h"
#define SIMIF simif_emul_t
#endif

#include "bridges/tracerv.h"
#include "BridgeHarness.h"

class TracerVModuleTest final : public BridgeHarness {
public:
  using BridgeHarness::BridgeHarness;

private:
  unsigned get_step_limit() const override { return 300000; }
  unsigned get_tick_limit() const override { return 100000; }
};
TEST_MAIN(TracerVModuleTest)
