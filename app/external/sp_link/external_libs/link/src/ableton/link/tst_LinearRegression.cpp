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

#include <ableton/link/LinearRegression.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <array>
#include <vector>

namespace ableton
{
namespace link
{

TEST_CASE("LinearRegression")
{
  using Array = std::array<std::pair<double, double>, 1>;
  using Vector = std::vector<std::pair<double, double>>;
  using FloatVector = std::vector<std::pair<float, float>>;

  SECTION("OnePoint")
  {
    Array data;
    data[0] = {0., 0.};
    const auto result = linearRegression(data.begin(), data.end());
    CHECK_THAT(result.first, Catch::Matchers::WithinAbs(0, 0.00001));
    CHECK_THAT(result.second, Catch::Matchers::WithinAbs(0, 0.00001));
  }

  SECTION("TwoPoints")
  {
    Vector data;
    data.emplace_back(0.0, 0.0);
    data.emplace_back(666666.6, 66666.6);

    const auto result = linearRegression(data.begin(), data.end());
    CHECK_THAT(result.first, Catch::Matchers::WithinAbs(0.1, 0.00001));
    CHECK_THAT(result.second, Catch::Matchers::WithinAbs(0.0, 0.00001));
  }

  SECTION("10000Points")
  {
    Vector data;
    const double slope = -0.2;
    const double intercept = -357.53456;

    for (int i = 1; i < 10000; ++i)
    {
      data.emplace_back(i, i * slope + intercept);
    }

    const auto result = linearRegression(data.begin(), data.end());
    CHECK_THAT(slope, Catch::Matchers::WithinAbs(result.first, 1e-7));
    CHECK_THAT(intercept, Catch::Matchers::WithinAbs(result.second, 1e-7));
  }

  SECTION("TwoPoints Float")
  {
    FloatVector data;
    data.emplace_back(0.f, 0.f);
    data.emplace_back(666666.6f, 66666.6f);

    const auto result = linearRegression(data.begin(), data.end());
    CHECK_THAT(static_cast<double>(result.first), Catch::Matchers::WithinAbs(0.1, 0.002));
    CHECK_THAT(static_cast<double>(result.second), Catch::Matchers::WithinAbs(0., 0.002));
  }

  SECTION("10000Points Float")
  {
    FloatVector data;
    const float slope = -0.2f;
    const float intercept = -357.53456f;

    for (int i = 1; i < 500; ++i)
    {
      data.emplace_back(static_cast<float>(i), static_cast<float>(i) * slope + intercept);
    }

    const auto result = linearRegression(data.begin(), data.end());
    CHECK_THAT(
      slope, Catch::Matchers::WithinAbs(static_cast<double>(result.first), 1e-3));
    CHECK_THAT(
      intercept, Catch::Matchers::WithinAbs(static_cast<double>(result.second), 1e-3));
  }
}

} // namespace link
} // namespace ableton
