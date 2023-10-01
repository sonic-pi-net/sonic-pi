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

#include <ableton/link/LinearRegression.hpp>
#include <chrono>
#include <cmath>
#include <utility>
#include <vector>

namespace ableton
{
namespace link
{

template <typename Clock, typename NumberType, std::size_t kNumPoints = 512>
class BasicHostTimeFilter
{
  using Points = std::vector<std::pair<NumberType, NumberType>>;
  using PointIt = typename Points::iterator;

public:
  BasicHostTimeFilter()
    : mIndex(0)
  {
    mPoints.reserve(kNumPoints);
  }

  ~BasicHostTimeFilter() = default;

  void reset()
  {
    mIndex = 0;
    mPoints.clear();
  }

  std::chrono::microseconds sampleTimeToHostTime(const NumberType sampleTime)
  {
    const auto micros = static_cast<NumberType>(mHostTimeSampler.micros().count());
    const auto point = std::make_pair(sampleTime, micros);

    if (mPoints.size() < kNumPoints)
    {
      mPoints.push_back(point);
    }
    else
    {
      mPoints[mIndex] = point;
    }
    mIndex = (mIndex + 1) % kNumPoints;

    const auto result = linearRegression(mPoints.begin(), mPoints.end());

    const auto hostTime = (result.first * sampleTime) + result.second;

    return std::chrono::microseconds(llround(hostTime));
  }

private:
  std::size_t mIndex;
  Points mPoints;
  Clock mHostTimeSampler;
};

template <typename Clock>
using HostTimeFilter = BasicHostTimeFilter<Clock, double, 512>;

} // namespace link
} // namespace ableton
