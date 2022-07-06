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

namespace ableton
{
namespace discovery
{

// Message types used in the Ableton service discovery protocol. There
// are two logical messages: a state dump and a bye bye.
//
// A state dump provides all relevant information about the peer's
// current state as well as a Time To Live (TTL) value that indicates
// how many seconds this state should be considered valid.
//
// The bye bye indicates that the sender is leaving the session.

template <typename NodeState>
struct PeerState
{
  NodeState peerState;
  int ttl;
};

template <typename NodeId>
struct ByeBye
{
  NodeId peerId;
};

} // namespace discovery
} // namespace ableton
