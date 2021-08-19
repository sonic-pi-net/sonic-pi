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

#include <cfloat>
#include <cmath>
#include <numeric>
#include <utility>

namespace ableton
{
namespace link
{

template <typename It>
std::pair<double, double> linearRegression(It begin, It end)
{
  using namespace std;
  using Point = pair<double, double>;

  const double numPoints = static_cast<double>(distance(begin, end));

  const double meanX = accumulate(begin, end, 0.0, [](double a, Point b) {
    return a + b.first;
  }) / numPoints;

  const double productXX = accumulate(begin, end, 0.0,
    [&meanX](double a, Point b) { return a + pow(b.first - meanX, 2.0); });

  const double meanY = accumulate(begin, end, 0.0, [](double a, Point b) {
    return a + b.second;
  }) / numPoints;

  const double productXY =
    inner_product(begin, end, begin, 0.0, [](double a, double b) { return a + b; },
      [&meanX, &meanY](
        Point a, Point b) { return ((a.first - meanX) * (b.second - meanY)); });

  const double slope = productXX == 0.0 ? 0.0 : productXY / productXX;

  const double intercept = meanY - (slope * meanX);

  return make_pair(slope, intercept);
}

} // namespace link
} // namespace ableton
