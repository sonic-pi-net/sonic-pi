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

#include <ableton/link/NodeId.hpp>

namespace ableton
{
namespace link
{

// SessionIds occupy the same value space as NodeIds and are
// identified by their founding node.
using SessionId = NodeId;

// A payload entry indicating membership in a particular session
struct SessionMembership
{
  static const std::int32_t key = 'sess';
  static_assert(key == 0x73657373, "Unexpected byte order");

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const SessionMembership& sm)
  {
    return discovery::sizeInByteStream(sm.sessionId);
  }

  template <typename It>
  friend It toNetworkByteStream(const SessionMembership& sm, It out)
  {
    return discovery::toNetworkByteStream(sm.sessionId, std::move(out));
  }

  template <typename It>
  static std::pair<SessionMembership, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto idRes = SessionId::fromNetworkByteStream(std::move(begin), std::move(end));
    return make_pair(SessionMembership{std::move(idRes.first)}, std::move(idRes.second));
  }

  SessionId sessionId;
};

} // namespace link
} // namespace ableton
