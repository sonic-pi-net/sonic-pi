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

using Vector = std::vector<std::pair<double, double>>;

TEST_CASE("LinearRegression | EmptyVector", "[LinearRegression]")
{
  Vector data;
  const auto result = linearRegression(data.begin(), data.end());
  CHECK(0 == Approx(result.first));
}

TEST_CASE("LinearRegression | OnePoint", "[LinearRegression]")
{
  using Array = std::array<std::pair<double, double>, 1>;
  Array data;
  data[0] = {{}, {}};
  const auto result = linearRegression(data.begin(), data.end());
  CHECK(0 == Approx(result.first));
  CHECK(0 == Approx(result.second));
}

TEST_CASE("LinearRegression | TwoPoints", "[LinearRegression]")
{
  Vector data;
  data.emplace_back(0.0, 0.0);
  data.emplace_back(666666.6, 66666.6);

  const auto result = linearRegression(data.begin(), data.end());
  CHECK(0.1 == Approx(result.first));
  CHECK(0.0 == Approx(result.second));
}

TEST_CASE("LinearRegression | 10000Points", "[LinearRegression]")
{
  Vector data;
  const double slope = -0.2;
  const double intercept = -357.53456;

  for (int i = 1; i < 10000; ++i)
  {
    data.emplace_back(i, i * slope + intercept);
  }

  const auto result = linearRegression(data.begin(), data.end());
  CHECK(slope == Approx(result.first));
  CHECK(intercept == Approx(result.second));
}

} // namespace link
} // namespace ableton
