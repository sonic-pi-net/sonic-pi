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

#include <ableton/link/Kalman.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <array>
#include <vector>

namespace ableton
{
namespace link
{

TEST_CASE("Kalman | Check1", "[Kalman]")
{
  int peerTimeDiff = 0;
  Kalman<16> filter;

  for (int i = 0; i < 5; ++i)
  {
    filter.iterate(peerTimeDiff);
  }

  CHECK(peerTimeDiff == Approx(filter.getValue()));

  filter.iterate(100);

  CHECK(peerTimeDiff != Approx(filter.getValue()));
}

TEST_CASE("Kalman | Check2", "[Kalman]")
{
  double peerTimeDiff = 3e11;
  Kalman<5> filter;

  for (int i = 0; i < 15; ++i)
  {
    filter.iterate(peerTimeDiff);
  }

  CHECK(peerTimeDiff == Approx(filter.getValue()));

  for (int i = 0; i < 15; ++i)
  {
    filter.iterate(11);
  }

  CHECK(peerTimeDiff != Approx(filter.getValue()));
}

} // namespace link
} // namespace ableton
