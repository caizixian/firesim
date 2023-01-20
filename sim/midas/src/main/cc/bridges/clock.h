// See LICENSE for license details.

#ifndef __CLOCK_H
#define __CLOCK_H

#include "core/widget.h"

#include <cstdint>

class simif_t;

struct CLOCKBRIDGEMODULE_struct {
  uint64_t CREDIT;
  uint64_t CREDIT_COUNT;
  uint64_t hCycle_0;
  uint64_t hCycle_1;
  uint64_t hCycle_latch;
  uint64_t tCycle_0;
  uint64_t tCycle_1;
  uint64_t tCycle_latch;
};

class clockmodule_t final : public widget_t {
public:
  /// The identifier for the bridge type.
  static char KIND;

  clockmodule_t(simif_t &simif, const CLOCKBRIDGEMODULE_struct &mmio_addrs);

  /**
   * Provides the current target cycle of the fastest clock.
   *
   * The target cycle is based on the number of clock tokens enqueued
   * (will report a larger number).
   */
  uint64_t tcycle();

  /**
   * Returns the current host cycle as measured by a hardware counter
   */
  uint64_t hcycle();

  /**
   * Credit the system to generate tokens for a given number of host cycles.
   */
  void credit(uint32_t credits);

  /**
   * Check whether there are any credits remaining.
   */
  bool has_credits();

private:
  const CLOCKBRIDGEMODULE_struct mmio_addrs;
};

#endif // __CLOCK_H
