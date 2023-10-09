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
#include <iterator>
#include <utility>

namespace ableton
{
namespace link
{

template <typename It>
typename std::iterator_traits<It>::value_type linearRegression(It begin, It end)
{
  using NumberType =
    typename std::tuple_element<0, typename std::iterator_traits<It>::value_type>::type;

  NumberType sumX = 0.0;
  NumberType sumXX = 0.0;
  NumberType sumXY = 0.0;
  NumberType sumY = 0.0;
  for (auto i = begin; i != end; ++i)
  {
    sumX += i->first;
    sumXX += i->first * i->first;
    sumXY += i->first * i->second;
    sumY += i->second;
  }

  const NumberType numPoints = static_cast<NumberType>(distance(begin, end));
  assert(numPoints > 0);
  const NumberType denominator = numPoints * sumXX - sumX * sumX;
  const NumberType slope = denominator == NumberType{0}
                             ? NumberType{0}
                             : (numPoints * sumXY - sumX * sumY) / denominator;
  const NumberType intercept = (sumY - slope * sumX) / numPoints;

  return std::make_pair(slope, intercept);
}

} // namespace link
} // namespace ableton
