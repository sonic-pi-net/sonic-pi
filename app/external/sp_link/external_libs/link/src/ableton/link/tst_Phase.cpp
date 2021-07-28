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

#include <ableton/link/Phase.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{
namespace
{

const auto zero = Beats{0.};
const auto one = Beats{1.};
const auto two = Beats{2.};
const auto three = Beats{3.};
const auto four = Beats{4.};

using std::chrono::microseconds;
const auto tl0 = Timeline{Tempo{120.}, one, microseconds{0}};
const auto tl1 = Timeline{Tempo{60.}, Beats{-9.5}, microseconds{2000000}};
} // namespace

TEST_CASE("phase | phase(x, 0) == 0", "[Phase]")
{
  CHECK(phase(zero, zero) == zero);
  CHECK(phase(Beats{0.1}, zero) == zero);
  CHECK(phase(one, zero) == zero);
  CHECK(phase(-one, zero) == zero);
}

TEST_CASE("phase | phase(x, y) == x % y when x and y >= 0", "[Phase]")
{
  CHECK(phase(zero, zero) == zero % zero);
  CHECK(phase(one, zero) == one % zero);
  CHECK(phase(zero, one) == zero % one);
  CHECK(phase(Beats{0.1}, one) == Beats{0.1} % one);
  CHECK(phase(Beats{2.3}, one) == Beats{2.3} % one);
  CHECK(phase(Beats{9.5}, Beats{2.3}) == Beats{9.5} % Beats{2.3});
}

TEST_CASE("phase | phase of negatives is not mirrored around zero", "[Phase]")
{
  CHECK(phase(-one, one) == zero);
  CHECK(phase(Beats{-0.1}, one) == Beats{0.9});
  CHECK(phase(Beats{-2.3}, one) == Beats{0.7});
  CHECK(phase(Beats{-9.5}, Beats{2.3}) == Beats{2.});
}

TEST_CASE("nextPhaseMatch | result == x when quantum == 0", "[Phase]")
{
  CHECK(nextPhaseMatch(Beats{0.1}, one, zero) == Beats{0.1});
  CHECK(nextPhaseMatch(Beats{2.3}, Beats{9.5}, zero) == Beats{2.3});
  CHECK(nextPhaseMatch(Beats{-0.1}, Beats{-2.3}, zero) == Beats{-0.1});
}

TEST_CASE("nextPhaseMatch | result == target when 0 <= x < target < quantum", "[Phase]")
{
  CHECK(nextPhaseMatch(zero, Beats{0.1}, one) == Beats{0.1});
  CHECK(nextPhaseMatch(Beats{0.1}, one, two) == one);
  CHECK(nextPhaseMatch(one, two, Beats{2.3}) == two);
  CHECK(nextPhaseMatch(two, Beats{2.3}, three) == Beats{2.3});
}

TEST_CASE("nextPhaseMatch | some example cases", "[Phase]")
{
  CHECK(nextPhaseMatch(one, Beats{2.3}, two) == Beats{2.3});
  CHECK(nextPhaseMatch(Beats{2.3}, Beats{-0.1}, two) == Beats{3.9});
  CHECK(nextPhaseMatch(Beats{-9.5}, Beats{0.1}, two) == Beats{-7.9});
  CHECK(nextPhaseMatch(Beats{-2.3}, Beats{0.1}, Beats{9.5}) == Beats{0.1});
}

TEST_CASE("toPhaseEncodedBeats | result == tl.toBeats when quantum == 0", "[Phase]")
{
  const auto t0 = microseconds{0};
  const auto t1 = microseconds{2000000};
  const auto t2 = microseconds{-3200000};
  CHECK(toPhaseEncodedBeats(tl1, t0, zero) == tl1.toBeats(t0));
  CHECK(toPhaseEncodedBeats(tl0, t1, zero) == tl0.toBeats(t1));
  CHECK(toPhaseEncodedBeats(tl0, t2, zero) == tl0.toBeats(t2));
}

TEST_CASE("toPhaseEncodedBeats | result is the nearest quantum boundary", "[Phase]")
{
  const auto sec = microseconds{1000000};

  // Takes the previous boundary
  CHECK(toPhaseEncodedBeats(tl0, sec, Beats{2.2}) == two);
  // Takes the next boundary
  CHECK(toPhaseEncodedBeats(tl0, sec, Beats{1.8}) == Beats{3.8});
  // Takes the previous boundary when exactly in the middle
  CHECK(toPhaseEncodedBeats(tl0, sec, two) == two);
}

TEST_CASE("toPhaseEncodedBeats | some example cases", "[Phase]")
{
  CHECK(toPhaseEncodedBeats(tl0, microseconds{-2000000}, four) == -four);
  CHECK(toPhaseEncodedBeats(tl0, microseconds{-3000000}, four) == Beats{-6.});
  CHECK(toPhaseEncodedBeats(tl0, microseconds{3200000}, three) == Beats{6.4});
  CHECK(toPhaseEncodedBeats(tl1, microseconds{0}, three) == Beats{-11.});
  CHECK(toPhaseEncodedBeats(tl1, microseconds{1500000}, Beats{2.4}) == Beats{-10.1});
}

namespace
{

std::chrono::microseconds phaseEncodingRoundtrip(
  const Timeline& tl, const std::chrono::microseconds t, const Beats quantum)
{
  return fromPhaseEncodedBeats(tl, toPhaseEncodedBeats(tl, t, quantum), quantum);
}
} // namespace

TEST_CASE("fromPhaseEncodedBeats | inverse of toPhaseEncodedBeats", "[Phase]")
{
  const auto t0 = microseconds{0};
  const auto t1 = microseconds{2000000};
  const auto t2 = microseconds{-3200000};
  const auto t3 = microseconds{87654321};

  CHECK(phaseEncodingRoundtrip(tl0, t0, zero) == t0);
  CHECK(phaseEncodingRoundtrip(tl0, t1, zero) == t1);
  CHECK(phaseEncodingRoundtrip(tl0, t2, zero) == t2);
  CHECK(phaseEncodingRoundtrip(tl0, t3, zero) == t3);

  CHECK(phaseEncodingRoundtrip(tl0, t0, one) == t0);
  CHECK(phaseEncodingRoundtrip(tl0, t1, one) == t1);
  CHECK(phaseEncodingRoundtrip(tl0, t2, one) == t2);
  CHECK(phaseEncodingRoundtrip(tl0, t3, one) == t3);

  CHECK(phaseEncodingRoundtrip(tl0, t0, two) == t0);
  CHECK(phaseEncodingRoundtrip(tl0, t1, two) == t1);
  CHECK(phaseEncodingRoundtrip(tl0, t2, two) == t2);
  CHECK(phaseEncodingRoundtrip(tl0, t3, two) == t3);

  CHECK(phaseEncodingRoundtrip(tl0, t0, three) == t0);
  CHECK(phaseEncodingRoundtrip(tl0, t1, three) == t1);
  CHECK(phaseEncodingRoundtrip(tl0, t2, three) == t2);
  CHECK(phaseEncodingRoundtrip(tl0, t3, three) == t3);

  CHECK(phaseEncodingRoundtrip(tl1, t0, zero) == t0);
  CHECK(phaseEncodingRoundtrip(tl1, t1, zero) == t1);
  CHECK(phaseEncodingRoundtrip(tl1, t2, zero) == t2);
  CHECK(phaseEncodingRoundtrip(tl1, t3, zero) == t3);

  CHECK(phaseEncodingRoundtrip(tl1, t0, one) == t0);
  CHECK(phaseEncodingRoundtrip(tl1, t1, one) == t1);
  CHECK(phaseEncodingRoundtrip(tl1, t2, one) == t2);
  CHECK(phaseEncodingRoundtrip(tl1, t3, one) == t3);

  CHECK(phaseEncodingRoundtrip(tl1, t0, two) == t0);
  CHECK(phaseEncodingRoundtrip(tl1, t1, two) == t1);
  CHECK(phaseEncodingRoundtrip(tl1, t2, two) == t2);
  CHECK(phaseEncodingRoundtrip(tl1, t3, two) == t3);

  CHECK(phaseEncodingRoundtrip(tl1, t0, three) == t0);
  CHECK(phaseEncodingRoundtrip(tl1, t1, three) == t1);
  CHECK(phaseEncodingRoundtrip(tl1, t2, three) == t2);
  CHECK(phaseEncodingRoundtrip(tl1, t3, three) == t3);
}

} // namespace link
} // namespace ableton
