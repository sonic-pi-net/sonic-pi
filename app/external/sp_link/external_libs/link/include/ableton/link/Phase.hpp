/* Copyright 2016, Ableton AG, Berlin. All rights reserved.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  If you would like to incorporate Link into a proprietary software application,
 *  please contact <link-devs@ableton.com>.
 */

#pragma once

#include <ableton/link/Beats.hpp>
#include <ableton/link/Timeline.hpp>
#include <chrono>

namespace ableton
{
namespace link
{

// Returns a value in the range [0,quantum) corresponding to beats %
// quantum except that negative beat values are handled correctly.
// If the given quantum is zero, returns zero.
inline Beats phase(const Beats beats, const Beats quantum)
{
  if (quantum == Beats{INT64_C(0)})
  {
    return Beats{INT64_C(0)};
  }
  else
  {
    // Handle negative beat values by doing the computation relative to an
    // origin that is on the nearest quantum boundary less than -(abs(x))
    const auto quantumMicros = quantum.microBeats();
    const auto quantumBins = (llabs(beats.microBeats()) + quantumMicros) / quantumMicros;
    const std::int64_t quantumBeats{quantumBins * quantumMicros};
    return (beats + Beats{quantumBeats}) % quantum;
  }
}

// Return the least value greater than x that matches the phase of
// target with respect to the given quantum. If the given quantum
// quantum is 0, x is returned.
inline Beats nextPhaseMatch(const Beats x, const Beats target, const Beats quantum)
{
  const auto desiredPhase = phase(target, quantum);
  const auto xPhase = phase(x, quantum);
  const auto phaseDiff = (desiredPhase - xPhase + quantum) % quantum;
  return x + phaseDiff;
}

// Return the closest value to x that matches the phase of the target
// with respect to the given quantum. The result deviates from x by at
// most quantum/2, but may be less than x.
inline Beats closestPhaseMatch(const Beats x, const Beats target, const Beats quantum)
{
  return nextPhaseMatch(x - Beats{0.5 * quantum.floating()}, target, quantum);
}

// Interprets the given timeline as encoding a quantum boundary at its
// origin. Given such a timeline, returns a phase-encoded beat value
// relative to the given quantum that corresponds to the given
// time. The phase of the resulting beat value can be calculated with
// phase(beats, quantum). The result will deviate by up to +-
// (quantum/2) beats compared to the result of tl.toBeats(time).
inline Beats toPhaseEncodedBeats(
  const Timeline& tl, const std::chrono::microseconds time, const Beats quantum)
{
  const auto beat = tl.toBeats(time);
  return closestPhaseMatch(beat, beat - tl.beatOrigin, quantum);
}

// The inverse of toPhaseEncodedBeats. Given a phase encoded beat
// value from the given timeline and quantum, find the time value that
// it maps to.
inline std::chrono::microseconds fromPhaseEncodedBeats(
  const Timeline& tl, const Beats beat, const Beats quantum)
{
  const auto fromOrigin = beat - tl.beatOrigin;
  const auto originOffset = fromOrigin - phase(fromOrigin, quantum);
  // invert the phase calculation so that it always rounds up in the
  // middle instead of down like closestPhaseMatch. Otherwise we'll
  // end up rounding down twice when a value is at phase quantum/2.
  const auto inversePhaseOffset = closestPhaseMatch(
    quantum - phase(fromOrigin, quantum), quantum - phase(beat, quantum), quantum);
  return tl.fromBeats(tl.beatOrigin + originOffset + quantum - inversePhaseOffset);
}

} // namespace link
} // namespace ableton
