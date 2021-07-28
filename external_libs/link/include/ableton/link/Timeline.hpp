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

#pragma once

#include <ableton/discovery/NetworkByteStreamSerializable.hpp>
#include <ableton/link/Beats.hpp>
#include <ableton/link/Tempo.hpp>
#include <cmath>
#include <cstdint>
#include <tuple>

namespace ableton
{
namespace link
{

// A tuple of (tempo, beats, time), with integral units
// based on microseconds. This type establishes a bijection between
// beats and wall time, given a valid tempo. It also serves as a
// payload entry.

struct Timeline
{
  static const std::int32_t key = 'tmln';
  static_assert(key == 0x746d6c6e, "Unexpected byte order");

  Beats toBeats(const std::chrono::microseconds time) const
  {
    return beatOrigin + tempo.microsToBeats(time - timeOrigin);
  }

  std::chrono::microseconds fromBeats(const Beats beats) const
  {
    return timeOrigin + tempo.beatsToMicros(beats - beatOrigin);
  }

  friend bool operator==(const Timeline& lhs, const Timeline& rhs)
  {
    return std::tie(lhs.tempo, lhs.beatOrigin, lhs.timeOrigin)
           == std::tie(rhs.tempo, rhs.beatOrigin, rhs.timeOrigin);
  }

  friend bool operator!=(const Timeline& lhs, const Timeline& rhs)
  {
    return !(lhs == rhs);
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const Timeline& tl)
  {
    return discovery::sizeInByteStream(std::tie(tl.tempo, tl.beatOrigin, tl.timeOrigin));
  }

  template <typename It>
  friend It toNetworkByteStream(const Timeline& tl, It out)
  {
    return discovery::toNetworkByteStream(
      std::tie(tl.tempo, tl.beatOrigin, tl.timeOrigin), std::move(out));
  }

  template <typename It>
  static std::pair<Timeline, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    using namespace discovery;
    Timeline timeline;
    auto result =
      Deserialize<tuple<Tempo, Beats, chrono::microseconds>>::fromNetworkByteStream(
        std::move(begin), std::move(end));
    tie(timeline.tempo, timeline.beatOrigin, timeline.timeOrigin) = std::move(result.first);
    return make_pair(std::move(timeline), std::move(result.second));
  }

  Tempo tempo;
  Beats beatOrigin;
  std::chrono::microseconds timeOrigin;
};

} // namespace link
} // namespace ableton
