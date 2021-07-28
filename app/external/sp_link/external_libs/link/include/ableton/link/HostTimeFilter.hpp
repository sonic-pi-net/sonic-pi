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
#include <vector>

namespace ableton
{
namespace link
{

template <class Clock>
class HostTimeFilter
{
  static const std::size_t kNumPoints = 512;
  using Points = std::vector<std::pair<double, double>>;
  using PointIt = typename Points::iterator;

public:
  HostTimeFilter()
    : mIndex(0)
  {
    mPoints.reserve(kNumPoints);
  }

  ~HostTimeFilter() = default;

  void reset()
  {
    mIndex = 0;
    mPoints.clear();
  }

  std::chrono::microseconds sampleTimeToHostTime(const double sampleTime)
  {
    const auto micros = static_cast<double>(mHostTimeSampler.micros().count());
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

} // namespace link
} // namespace ableton
