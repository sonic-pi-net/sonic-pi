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
#include <ableton/link/MeasurementEndpointV4.hpp>
#include <ableton/link/NodeState.hpp>

namespace ableton
{
namespace link
{

// A state type for peers. PeerState stores the normal NodeState plus
// additional information (the remote endpoint at which to find its
// ping/pong measurement server).

struct PeerState
{
  using IdType = NodeId;

  IdType ident() const
  {
    return nodeState.ident();
  }

  SessionId sessionId() const
  {
    return nodeState.sessionId;
  }

  Timeline timeline() const
  {
    return nodeState.timeline;
  }

  StartStopState startStopState() const
  {
    return nodeState.startStopState;
  }

  friend bool operator==(const PeerState& lhs, const PeerState& rhs)
  {
    return lhs.nodeState == rhs.nodeState && lhs.endpoint == rhs.endpoint;
  }

  friend auto toPayload(const PeerState& state)
    -> decltype(std::declval<NodeState::Payload>()
                + discovery::makePayload(MeasurementEndpointV4{{}}))
  {
    return toPayload(state.nodeState)
           + discovery::makePayload(MeasurementEndpointV4{state.endpoint});
  }

  template <typename It>
  static PeerState fromPayload(NodeId id, It begin, It end)
  {
    using namespace std;
    auto peerState = PeerState{NodeState::fromPayload(std::move(id), begin, end), {}};

    discovery::parsePayload<MeasurementEndpointV4>(std::move(begin), std::move(end),
      [&peerState](MeasurementEndpointV4 me4) { peerState.endpoint = std::move(me4.ep); });
    return peerState;
  }

  NodeState nodeState;
  asio::ip::udp::endpoint endpoint;
};

} // namespace link
} // namespace ableton
