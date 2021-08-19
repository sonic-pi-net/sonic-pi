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

namespace ableton
{
namespace link
{

using std::chrono::microseconds;

struct GhostXForm
{
  microseconds hostToGhost(const microseconds hostTime) const
  {
    return microseconds{llround(slope * static_cast<double>(hostTime.count()))}
           + intercept;
  }

  microseconds ghostToHost(const microseconds ghostTime) const
  {
    return microseconds{
      llround(static_cast<double>((ghostTime - intercept).count()) / slope)};
  }

  friend bool operator==(const GhostXForm lhs, const GhostXForm rhs)
  {
    return lhs.slope == rhs.slope && lhs.intercept == rhs.intercept;
  }

  friend bool operator!=(const GhostXForm lhs, const GhostXForm rhs)
  {
    return !(lhs == rhs);
  }

  double slope;
  microseconds intercept;
};

} // namespace link
} // namespace ableton
