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

#include <ableton/discovery/Payload.hpp>
#include <ableton/link/NodeId.hpp>
#include <ableton/link/SessionId.hpp>
#include <ableton/link/StartStopState.hpp>
#include <ableton/link/Timeline.hpp>

namespace ableton
{
namespace link
{

struct NodeState
{
  using Payload =
    decltype(discovery::makePayload(Timeline{}, SessionMembership{}, StartStopState{}));

  NodeId ident() const
  {
    return nodeId;
  }

  friend bool operator==(const NodeState& lhs, const NodeState& rhs)
  {
    return std::tie(lhs.nodeId, lhs.sessionId, lhs.timeline, lhs.startStopState)
           == std::tie(rhs.nodeId, rhs.sessionId, rhs.timeline, rhs.startStopState);
  }

  friend Payload toPayload(const NodeState& state)
  {
    return discovery::makePayload(
      state.timeline, SessionMembership{state.sessionId}, state.startStopState);
  }

  template <typename It>
  static NodeState fromPayload(NodeId nodeId, It begin, It end)
  {
    using namespace std;
    auto nodeState = NodeState{std::move(nodeId), {}, {}, {}};
    discovery::parsePayload<Timeline, SessionMembership, StartStopState>(std::move(begin),
      std::move(end), [&nodeState](Timeline tl) { nodeState.timeline = std::move(tl); },
      [&nodeState](SessionMembership membership) {
        nodeState.sessionId = std::move(membership.sessionId);
      },
      [&nodeState](StartStopState ststst) { nodeState.startStopState = std::move(ststst); });
    return nodeState;
  }

  NodeId nodeId;
  SessionId sessionId;
  Timeline timeline;
  StartStopState startStopState;
};

} // namespace link
} // namespace ableton
