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

#include <ableton/link/Tempo.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{

TEST_CASE("Tempo")
{
  SECTION("ConstructFromBpm")
  {
    const auto tempo = Tempo{120.};
    CHECK_THAT(120., Catch::Matchers::WithinAbs(tempo.bpm(), 1e-10));
    CHECK(std::chrono::microseconds{500000} == tempo.microsPerBeat());
  }

  SECTION("ConstructFromMicros")
  {
    const auto tempo = Tempo{std::chrono::microseconds{500000}};
    CHECK_THAT(120., Catch::Matchers::WithinAbs(tempo.bpm(), 1e-10));
    CHECK(std::chrono::microseconds{500000} == tempo.microsPerBeat());
  }

  SECTION("MicrosToBeats")
  {
    const auto tempo = Tempo{120.};
    CHECK(Beats{2.} == tempo.microsToBeats(std::chrono::microseconds{1000000}));
  }

  SECTION("BeatsToMicros")
  {
    const auto tempo = Tempo{120.};
    CHECK(std::chrono::microseconds{1000000} == tempo.beatsToMicros(Beats{2.}));
  }

  SECTION("Comparison")
  {
    const auto tempo1 = Tempo{100.};
    const auto tempo2 = Tempo{200.};
    CHECK(tempo1 < tempo2);
    CHECK(tempo2 > tempo1);
    CHECK(tempo1 <= tempo2);
    CHECK(tempo2 <= tempo2);
    CHECK(tempo2 >= tempo1);
    CHECK(tempo2 >= tempo2);
    CHECK(Tempo{100.} == tempo1);
    CHECK(tempo1 != tempo2);
  }

  SECTION("RoundtripByteStreamEncoding")
  {
    const auto tempo = Tempo{120.};
    std::vector<std::uint8_t> bytes(sizeInByteStream(tempo));
    const auto end = toNetworkByteStream(tempo, begin(bytes));
    const auto result = Tempo::fromNetworkByteStream(begin(bytes), end);
    CHECK(tempo == result.first);
  }
}

} // namespace link
} // namespace ableton
