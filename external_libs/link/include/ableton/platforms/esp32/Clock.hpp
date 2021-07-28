/* Copyright 2019, Mathias Bredholt, Torso Electronics, Copenhagen. All rights reserved.
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
 */

#pragma once

#include "esp_timer.h"

namespace ableton
{
namespace platforms
{
namespace esp32
{
struct Clock
{
  std::chrono::microseconds micros() const
  {
    return static_cast<std::chrono::microseconds>(esp_timer_get_time());
  }
};
} // namespace esp32
} // namespace platforms
} // namespace ableton
