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

#include <ableton/link/HostTimeFilter.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <chrono>

namespace ableton
{
namespace link
{

struct MockClock
{
  MockClock()
    : time(std::chrono::microseconds(0))
  {
  }

  std::chrono::microseconds micros()
  {
    const auto current = time;
    time += std::chrono::microseconds(1);
    return current;
  }

  std::chrono::microseconds time;
};

using Filter = ableton::link::HostTimeFilter<MockClock>;

TEST_CASE("HostTimeFilter | OneValue", "[HostTimeFilter]")
{
  Filter filter;
  const auto ht = filter.sampleTimeToHostTime(5);
  CHECK(0 == ht.count());
}

TEST_CASE("HostTimeFilter | MultipleValues", "[HostTimeFilter]")
{
  Filter filter;
  const auto numValues = 600;
  auto ht = std::chrono::microseconds(0);

  for (int i = 0; i <= numValues; ++i)
  {
    ht = filter.sampleTimeToHostTime(i);
  }

  CHECK(numValues == ht.count());
}

TEST_CASE("HostTimeFilter | Reset", "[HostTimeFilter]")
{
  Filter filter;
  auto ht = filter.sampleTimeToHostTime(0);
  ht = filter.sampleTimeToHostTime(-230);
  ht = filter.sampleTimeToHostTime(40);
  REQUIRE(2 != ht.count());

  filter.reset();
  ht = filter.sampleTimeToHostTime(0);
  CHECK(3 == ht.count());
}

} // namespace link
} // namespace ableton
