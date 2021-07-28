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
#include <cstdint>

namespace ableton
{
namespace platforms
{
namespace windows
{

struct Clock
{
  using Ticks = std::int64_t;
  using Micros = std::chrono::microseconds;

  Clock()
  {
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);
    mTicksToMicros = 1.0e6 / static_cast<double>(frequency.QuadPart);
  }

  Micros ticksToMicros(const Ticks ticks) const
  {
    return Micros{llround(mTicksToMicros * static_cast<double>(ticks))};
  }

  Ticks microsToTicks(const Micros micros) const
  {
    return static_cast<Ticks>(static_cast<double>(micros.count()) / mTicksToMicros);
  }

  Ticks ticks() const
  {
    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    return count.QuadPart;
  }

  std::chrono::microseconds micros() const
  {
    return ticksToMicros(ticks());
  }

  double mTicksToMicros;
};

} // namespace windows
} // namespace platforms
} // namespace ableton
