/* Copyright 2017, Ableton AG, Berlin. All rights reserved.
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
#include <chrono>
#include <tuple>

namespace ableton
{
namespace link
{

// A tuple of (isPlaying, time) that represents the playing state
// with an according timestamp in microseconds. It also serves as a
// payload entry.

struct StartStopState
{
  static const std::int32_t key = 'stst';
  static_assert(key == 0x73747374, "Unexpected byte order");

  using StartStopStateTuple = std::tuple<bool, Beats, std::chrono::microseconds>;

  StartStopState() = default;

  StartStopState(
    const bool aIsPlaying, const Beats aBeats, const std::chrono::microseconds aTimestamp)
    : isPlaying(aIsPlaying)
    , beats(aBeats)
    , timestamp(aTimestamp)
  {
  }

  friend bool operator==(const StartStopState& lhs, const StartStopState& rhs)
  {
    return std::tie(lhs.isPlaying, lhs.beats, lhs.timestamp)
           == std::tie(rhs.isPlaying, rhs.beats, rhs.timestamp);
  }

  friend bool operator!=(const StartStopState& lhs, const StartStopState& rhs)
  {
    return !(lhs == rhs);
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const StartStopState& state)
  {
    return discovery::sizeInByteStream(state.asTuple());
  }

  template <typename It>
  friend It toNetworkByteStream(const StartStopState& state, It out)
  {
    return discovery::toNetworkByteStream(state.asTuple(), std::move(out));
  }

  template <typename It>
  static std::pair<StartStopState, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    using namespace discovery;
    auto result =
      Deserialize<StartStopStateTuple>::fromNetworkByteStream(std::move(begin), std::move(end));
    auto state =
      StartStopState{get<0>(result.first), get<1>(result.first), get<2>(result.first)};
    return make_pair(std::move(state), std::move(result.second));
  }

  bool isPlaying{false};
  Beats beats{0.};
  std::chrono::microseconds timestamp{0};

private:
  StartStopStateTuple asTuple() const
  {
    return std::make_tuple(isPlaying, beats, timestamp);
  }
};

struct ClientStartStopState
{
  ClientStartStopState() = default;

  ClientStartStopState(const bool aIsPlaying,
    const std::chrono::microseconds aTime,
    const std::chrono::microseconds aTimestamp)
    : isPlaying(aIsPlaying)
    , time(aTime)
    , timestamp(aTimestamp)
  {
  }

  friend bool operator==(const ClientStartStopState& lhs, const ClientStartStopState& rhs)
  {
    return std::tie(lhs.isPlaying, lhs.time, lhs.timestamp)
           == std::tie(rhs.isPlaying, rhs.time, rhs.timestamp);
  }

  friend bool operator!=(const ClientStartStopState& lhs, const ClientStartStopState& rhs)
  {
    return !(lhs == rhs);
  }

  bool isPlaying{false};
  std::chrono::microseconds time{0};
  std::chrono::microseconds timestamp{0};
};

struct ApiStartStopState
{
  ApiStartStopState() = default;

  ApiStartStopState(const bool aIsPlaying, const std::chrono::microseconds aTime)
    : isPlaying(aIsPlaying)
    , time(aTime)
  {
  }

  friend bool operator==(const ApiStartStopState& lhs, const ApiStartStopState& rhs)
  {
    return std::tie(lhs.isPlaying, lhs.time) == std::tie(rhs.isPlaying, rhs.time);
  }

  friend bool operator!=(const ApiStartStopState& lhs, const ApiStartStopState& rhs)
  {
    return !(lhs == rhs);
  }

  bool isPlaying{false};
  std::chrono::microseconds time{0};
};
} // namespace link
} // namespace ableton
