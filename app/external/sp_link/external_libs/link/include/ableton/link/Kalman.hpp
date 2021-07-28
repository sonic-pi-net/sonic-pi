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

#include <array>
#include <cfloat>
#include <cmath>


namespace ableton
{
namespace link
{

template <std::size_t n>
struct Kalman
{
  Kalman()
    : mValue(0)
    , mCoVariance(1)
    , mVarianceLength(n)
    , mCounter(mVarianceLength)
  {
  }

  double getValue()
  {
    return mValue;
  }

  double calculateVVariance()
  {
    auto vVar = 0.;
    auto meanOfDiffs = 0.;

    for (size_t k = 0; k < (mVarianceLength); k++)
    {
      meanOfDiffs += (mMeasuredValues[k] - mFilterValues[k]);
    }

    meanOfDiffs /= static_cast<double>(mVarianceLength);

    for (size_t i = 0; i < (mVarianceLength); i++)
    {
      vVar += (pow(mMeasuredValues[i] - mFilterValues[i] - meanOfDiffs, 2.0));
    }

    vVar /= static_cast<double>(mVarianceLength - 1);

    return vVar;
  }

  double calculateWVariance()
  {
    auto wVar = 0.;
    auto meanOfDiffs = 0.;

    for (size_t k = 0; k < (mVarianceLength); k++)
    {
      meanOfDiffs += (mFilterValues[(mCounter - k - 1) % mVarianceLength]
                      - mFilterValues[(mCounter - k - 2) % mVarianceLength]);
    }

    meanOfDiffs /= static_cast<double>(mVarianceLength);

    for (size_t i = 0; i < (mVarianceLength); i++)
    {
      wVar += (pow(mFilterValues[(mCounter - i - 1) % mVarianceLength]
                     - mFilterValues[(mCounter - i - 2) % mVarianceLength] - meanOfDiffs,
        2.0));
    }

    wVar /= static_cast<double>(mVarianceLength - 1);

    return wVar;
  }

  void iterate(const double value)
  {
    const std::size_t currentIndex = mCounter % mVarianceLength;
    mMeasuredValues[currentIndex] = value;

    if (mCounter < (mVarianceLength + mVarianceLength))
    {
      if (mCounter == mVarianceLength)
      {
        mValue = value;
      }
      else
      {
        mValue = (mValue + value) / 2;
      }
    }
    else
    {
      // prediction equations
      const double prevFilterValue = mFilterValues[(mCounter - 1) % mVarianceLength];
      mFilterValues[currentIndex] = prevFilterValue;
      const auto wVariance = calculateWVariance();
      const double coVarianceEstimation = mCoVariance + wVariance;

      // update equations
      const auto vVariance = calculateVVariance();
      // Gain defines how easily the filter will adjust to a new condition
      // With gain = 1 the output equals the input, with gain = 0 the input
      // is ignored and the output equals the last filtered value
      const auto divisor = coVarianceEstimation + vVariance;
      const auto gain = divisor != 0. ? coVarianceEstimation / divisor : 0.7;
      mValue = prevFilterValue + gain * (value - prevFilterValue);
      mCoVariance = (1 - gain) * coVarianceEstimation;
    }
    mFilterValues[currentIndex] = mValue;

    ++mCounter;
  }

  double mValue;
  double mCoVariance;
  size_t mVarianceLength;
  size_t mCounter;
  std::array<double, n> mFilterValues;
  std::array<double, n> mMeasuredValues;
};

} // namespace link
} // namespace ableton
