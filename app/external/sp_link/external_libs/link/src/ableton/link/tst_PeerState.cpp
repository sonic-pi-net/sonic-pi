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

#include <ableton/discovery/AsioTypes.hpp>
#include <ableton/link/NodeId.hpp>
#include <ableton/link/PeerState.hpp>
#include <ableton/link/StartStopState.hpp>
#include <ableton/link/Timeline.hpp>
#include <ableton/platforms/stl/Random.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{

TEST_CASE("PeerState")
{
  auto nodeId = NodeId::random<platforms::stl::Random>();
  const auto timeline = Timeline{Tempo{60.}, Beats{1.}, std::chrono::microseconds{100}};
  const auto startStop =
    StartStopState{true, Beats{1234.}, std::chrono::microseconds{5678}};
  const auto nodeState = NodeState{nodeId, nodeId, timeline, startStop};

  SECTION("V4RoundtripByteStreamEncoding")
  {
    PeerState state{nodeState,
      discovery::UdpEndpoint(
        {discovery::makeAddress<discovery::IpAddressV4>("123.123.1.2"), 6780})};

    const auto payload = toPayload(state);
    std::vector<std::uint8_t> bytes(sizeInByteStream(payload));
    const auto end = toNetworkByteStream(payload, begin(bytes));

    const auto result = PeerState::fromPayload(nodeId, bytes.begin(), end);
    CHECK(state == result);
  }

  SECTION("V6RoundtripByteStreamEncoding")
  {
    PeerState state{
      nodeState, discovery::UdpEndpoint(
                   {::LINK_ASIO_NAMESPACE::ip::make_address("fe80::8080"), 6780})};

    const auto payload = toPayload(state);
    std::vector<std::uint8_t> bytes(sizeInByteStream(payload));
    const auto end = toNetworkByteStream(payload, begin(bytes));

    const auto result = PeerState::fromPayload(nodeId, bytes.begin(), end);
    CHECK(state == result);
  }
}

} // namespace link
} // namespace ableton
