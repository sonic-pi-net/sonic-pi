/* Copyright 2019, Ableton AG, Berlin. All rights reserved.
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

namespace ableton
{
namespace platforms
{
namespace esp32
{

struct Random
{
  uint8_t operator()()
  {
    return static_cast<uint8_t>((esp_random() % 93) + 33); // printable ascii chars
  }
};

} // namespace esp32
} // namespace platforms
} // namespace ableton
