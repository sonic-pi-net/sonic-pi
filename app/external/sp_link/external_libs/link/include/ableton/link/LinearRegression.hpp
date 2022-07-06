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

#include <cassert>
#include <utility>

namespace ableton
{
namespace link
{

template <typename It>
std::pair<double, double> linearRegression(It begin, It end)
{
  double sumX = 0.0;
  double sumXX = 0.0;
  double sumXY = 0.0;
  double sumY = 0.0;
  for (auto i = begin; i != end; ++i)
  {
    sumX += i->first;
    sumXX += i->first * i->first;
    sumXY += i->first * i->second;
    sumY += i->second;
  }

  const double numPoints = static_cast<double>(distance(begin, end));
  assert(numPoints > 0);
  const double denominator = numPoints * sumXX - sumX * sumX;
  const double slope =
    denominator == 0.0 ? 0.0 : (numPoints * sumXY - sumX * sumY) / denominator;
  const double intercept = (sumY - slope * sumX) / numPoints;

  return std::make_pair(slope, intercept);
}

} // namespace link
} // namespace ableton
