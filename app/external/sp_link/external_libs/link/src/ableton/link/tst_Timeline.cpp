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

#include <ableton/link/Timeline.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{

TEST_CASE("Timeline")
{
  const auto tl60 = Timeline{Tempo{60.}, Beats{-1.}, std::chrono::microseconds{1000000}};
  const auto tl120 =
    Timeline{Tempo{120.}, Beats{5.5}, std::chrono::microseconds{12558940}};

  SECTION("TimeToBeats")
  {
    CHECK(Beats{2.5} == tl60.toBeats(std::chrono::microseconds{4500000}));
  }

  SECTION("BeatsToTime")
  {
    CHECK(std::chrono::microseconds{5200000} == tl60.fromBeats(Beats{3.2}));
  }

  SECTION("RoundtripByteStreamEncoding")
  {
    std::vector<std::uint8_t> bytes(sizeInByteStream(tl120));
    const auto end = toNetworkByteStream(tl120, begin(bytes));
    const auto result = Timeline::fromNetworkByteStream(begin(bytes), end);
    CHECK(tl120 == result.first);
  }
}

} // namespace link
} // namespace ableton
