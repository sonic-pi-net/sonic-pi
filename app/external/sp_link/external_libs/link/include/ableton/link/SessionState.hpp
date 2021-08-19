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

#include <ableton/link/Optional.hpp>
#include <ableton/link/StartStopState.hpp>
#include <ableton/link/Timeline.hpp>

namespace ableton
{
namespace link
{

using OptionalTimeline = Optional<Timeline>;
using OptionalStartStopState = Optional<StartStopState>;
using OptionalClientStartStopState = Optional<ClientStartStopState>;

struct SessionState
{
  Timeline timeline;
  StartStopState startStopState;
  GhostXForm ghostXForm;
};

struct ClientState
{
  friend bool operator==(const ClientState& lhs, const ClientState& rhs)
  {
    return std::tie(lhs.timeline, lhs.startStopState)
           == std::tie(rhs.timeline, rhs.startStopState);
  }

  friend bool operator!=(const ClientState& lhs, const ClientState& rhs)
  {
    return !(lhs == rhs);
  }

  Timeline timeline;
  ClientStartStopState startStopState;
};

struct RtClientState
{
  Timeline timeline;
  ClientStartStopState startStopState;
  std::chrono::microseconds timelineTimestamp;
  std::chrono::microseconds startStopStateTimestamp;
};

struct IncomingClientState
{
  OptionalTimeline timeline;
  OptionalClientStartStopState startStopState;
  std::chrono::microseconds timelineTimestamp;
};

struct ApiState
{
  Timeline timeline;
  ApiStartStopState startStopState;
};

} // namespace link
} // namespace ableton
