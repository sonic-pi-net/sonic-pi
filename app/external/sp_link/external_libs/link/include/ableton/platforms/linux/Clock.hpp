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

#include <chrono>
#include <cmath>
#include <ctime>

namespace ableton
{
namespace platforms
{

#ifdef linux
#undef linux
#endif

namespace linux
{

template <clockid_t CLOCK>
class Clock
{
public:
  std::chrono::microseconds micros() const
  {
    ::timespec ts;
    ::clock_gettime(CLOCK, &ts);
    std::uint64_t ns = ts.tv_sec * 1000000000ULL + ts.tv_nsec;
    return std::chrono::microseconds(ns / 1000ULL);
  }
};

using ClockMonotonic = Clock<CLOCK_MONOTONIC>;
using ClockMonotonicRaw = Clock<CLOCK_MONOTONIC_RAW>;

} // namespace linux
} // namespace platforms
} // namespace ableton
