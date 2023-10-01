/* Copyright 2023, Ableton AG, Berlin. All rights reserved.
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

#include <ableton/Link.hpp>
#include <ableton/platforms/stl/Clock.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{

TEST_CASE("SessionState")
{
  SECTION("ForceBeatTime")
  {
    using namespace std::chrono;
    using namespace ableton::link;

    const auto beats = 0.;
    const auto time = microseconds{23456789};
    const auto quantum = 4.;

    using SessionState = ableton::BasicLink<platforms::stl::Clock>::SessionState;

    for (auto tempo = 20.; tempo < 999.; tempo += 0.8)
    {
      const auto tl = Timeline{Tempo{tempo}, Beats{12345678.}, microseconds{1234567}};
      auto sessionState = SessionState({{tl, {}}, false});
      sessionState.forceBeatAtTime(beats, time, quantum);

      CHECK(beats >= sessionState.beatAtTime(time, quantum));
    }
  }
}

} // namespace ableton
