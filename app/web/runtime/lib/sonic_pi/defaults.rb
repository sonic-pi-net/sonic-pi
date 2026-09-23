# SPDX-License-Identifier: AGPL-3.0-or-later
# The runtime's defaults that the oracle keeps too (oracle/harness/oracle.rb loads this file), so a trace and the
# runtime it is held to are taken under the same settings: said once, here.
module SonicPi
  # How far ahead of the moment a run starts its sounds are scheduled: a thread's sched_ahead unless the program sets
  # its own (use_sched_ahead_time, set_sched_ahead_time!). Everything that takes the default into account (the start
  # Link's beat 0 counts from, a freed node's moment, a trace's frame) reads it from here.
  DEFAULT_SCHED_AHEAD = 0.05
end
