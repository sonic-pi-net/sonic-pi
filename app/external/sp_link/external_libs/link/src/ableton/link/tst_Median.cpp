/* Copyright 2021, Ableton AG, Berlin. All rights reserved.
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

#include <ableton/link/Median.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <array>
#include <vector>

namespace ableton
{
namespace link
{

TEST_CASE("Median")
{
  using Vector = std::vector<double>;

  SECTION("ArrayWithThreePoints")
  {
    auto data = std::array<double, 3>{{5., 0., 6.}};
    CHECK_THAT(5, Catch::Matchers::WithinAbs(median(data.begin(), data.end()), 1e-10));
  }

  SECTION("VectorWithFourPoints")
  {
    auto data = Vector{{0., 2., 1., 3., 2.}};
    CHECK_THAT(2.0, Catch::Matchers::WithinAbs(median(data.begin(), data.end()), 1e-10));
  }

  SECTION("VectorWithFivePoints")
  {
    auto data = Vector{{0., 1., 3., 2.}};
    CHECK_THAT(1.5, Catch::Matchers::WithinAbs(median(data.begin(), data.end()), 1e-10));
  }

  SECTION("VectorWith9999Points")
  {
    Vector data;
    const double slope = -0.2;
    const double intercept = -357.53456;
    for (int i = 1; i < 10000; ++i)
    {
      data.emplace_back(i * slope + intercept);
    }
    CHECK_THAT(slope * 5000 + intercept,
      Catch::Matchers::WithinAbs(median(data.begin(), data.end()), 1e-10));
  }
}

} // namespace link
} // namespace ableton
