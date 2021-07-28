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

TEST_CASE("Timeline | TimeToBeats", "[Timeline]")
{
  const auto tl = Timeline{Tempo{60.}, Beats{-1.}, std::chrono::microseconds{1000000}};
  CHECK(Beats{2.5} == tl.toBeats(std::chrono::microseconds{4500000}));
}

TEST_CASE("Timeline | BeatsToTime", "[Timeline]")
{
  const auto tl = Timeline{Tempo{60.}, Beats{-1.}, std::chrono::microseconds{1000000}};
  CHECK(std::chrono::microseconds{5200000} == tl.fromBeats(Beats{3.2}));
}

TEST_CASE("Timeline | RoundtripByteStreamEncoding", "[Timeline]")
{
  const auto tl = Timeline{Tempo{120.}, Beats{5.5}, std::chrono::microseconds{12558940}};
  std::vector<std::uint8_t> bytes(sizeInByteStream(tl));
  const auto end = toNetworkByteStream(tl, begin(bytes));
  const auto result = Timeline::fromNetworkByteStream(begin(bytes), end);
  CHECK(tl == result.first);
}

} // namespace link
} // namespace ableton
