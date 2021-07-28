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

#include <ableton/discovery/UdpMessenger.hpp>
#include <ableton/discovery/test/Interface.hpp>
#include <ableton/discovery/test/PayloadEntries.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/test/serial_io/Fixture.hpp>
#include <array>

namespace ableton
{
namespace discovery
{
namespace
{

struct TestNodeState
{
  using IdType = uint8_t;

  IdType ident() const
  {
    return nodeId;
  }

  friend auto toPayload(const TestNodeState& state) -> decltype(makePayload(test::Foo{}))
  {
    return makePayload(test::Foo{state.fooVal});
  }

  template <typename It>
  static TestNodeState fromPayload(const uint8_t id, It begin, It end)
  {
    TestNodeState state = {id, 0};
    parsePayload<test::Foo>(
      begin, end, [&state](const test::Foo& foo) { state.fooVal = foo.fooVal; });
    return state;
  }

  IdType nodeId;
  int32_t fooVal;
};

// Test data for a simulated peer
const TestNodeState peerState = {5, 15};

const asio::ip::udp::endpoint peerEndpoint = {
  asio::ip::address::from_string("123.123.234.234"), 1900};

} // anonymous namespace

TEST_CASE("UdpMessenger | BroadcastsStateOnConstruction", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  auto context = io.makeIoContext();
  const auto state = TestNodeState{3, 10};
  auto iface = test::Interface{};
  auto messenger =
    makeUdpMessenger(util::injectRef(iface), state, util::injectRef(context), 1, 1);

  REQUIRE(1 == iface.sentMessages.size());

  const auto messageBuffer = iface.sentMessages[0].first;
  const auto sentTo = iface.sentMessages[0].second;
  const auto result = v1::parseMessageHeader<TestNodeState::IdType>(
    begin(messageBuffer), end(messageBuffer));

  // Expect an Alive header
  CHECK(v1::kAlive == result.first.messageType);
  CHECK(state.nodeId == result.first.ident);
  CHECK(1 == result.first.ttl);
  // Sent to the multicast endpoint
  CHECK(multicastEndpoint() == sentTo);

  // And the payload should parse to equal to the original state
  const auto actualState =
    TestNodeState::fromPayload(state.nodeId, result.second, end(messageBuffer));
  CHECK(state.fooVal == actualState.fooVal);
}

TEST_CASE("UdpMessenger | Heartbeat", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  const auto state = TestNodeState{3, 10};
  auto iface = test::Interface{};
  auto messenger = makeUdpMessenger(
    util::injectRef(iface), state, util::injectVal(io.makeIoContext()), 4, 2);

  REQUIRE(1 == iface.sentMessages.size());
  // At two seconds the messenger should have broadcasted its state again
  io.advanceTime(std::chrono::seconds(3));
  CHECK(2 == iface.sentMessages.size());
}

TEST_CASE("UdpMessenger | Response", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  const auto state = TestNodeState{3, 10};
  auto iface = test::Interface{};
  auto messenger = makeUdpMessenger(
    util::injectRef(iface), state, util::injectVal(io.makeIoContext()), 1, 1);

  // Simulate state broadcast from peer, leaving out details like payload
  v1::MessageBuffer buffer;
  const auto messageEnd =
    v1::aliveMessage(peerState.ident(), 0, makePayload(), begin(buffer));
  iface.incomingMessage(peerEndpoint, begin(buffer), messageEnd);

  // The messenger should have responded to the alive message with its
  // current state
  REQUIRE(2 == iface.sentMessages.size());
  const auto messageBuffer = iface.sentMessages[1].first;
  const auto sentTo = iface.sentMessages[1].second;
  const auto result = v1::parseMessageHeader<TestNodeState::IdType>(
    begin(messageBuffer), end(messageBuffer));

  CHECK(v1::kResponse == result.first.messageType);
  CHECK(state.nodeId == result.first.ident);
  CHECK(1 == result.first.ttl);
  CHECK(peerEndpoint == sentTo);
}

namespace
{

struct TestHandler
{
  void operator()(PeerState<TestNodeState> state)
  {
    peerStates.push_back(std::move(state));
  }

  void operator()(ByeBye<TestNodeState::IdType> byeBye)
  {
    byeByes.push_back(std::move(byeBye));
  }

  std::vector<PeerState<TestNodeState>> peerStates;
  std::vector<ByeBye<TestNodeState::IdType>> byeByes;
};

} // namespace

TEST_CASE("UdpMessenger | Receive", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  auto iface = test::Interface{};
  auto tmpMessenger = makeUdpMessenger(
    util::injectRef(iface), TestNodeState{}, util::injectVal(io.makeIoContext()), 1, 1);
  auto messenger = std::move(tmpMessenger);
  auto handler = TestHandler{};
  messenger.receive(std::ref(handler));

  v1::MessageBuffer buffer;
  // Receive an alive message
  auto end = v1::aliveMessage(peerState.nodeId, 3, toPayload(peerState), begin(buffer));
  iface.incomingMessage(peerEndpoint, begin(buffer), end);
  // And a bye bye message
  end = v1::byeByeMessage(peerState.nodeId, begin(buffer));
  messenger.receive(std::ref(handler));
  iface.incomingMessage(peerEndpoint, begin(buffer), end);

  REQUIRE(1 == handler.peerStates.size());
  CHECK(3 == handler.peerStates[0].ttl);
  CHECK(peerState.nodeId == handler.peerStates[0].peerState.nodeId);
  CHECK(peerState.fooVal == handler.peerStates[0].peerState.fooVal);
  REQUIRE(1 == handler.byeByes.size());
  CHECK(peerState.nodeId == handler.byeByes[0].peerId);
}

TEST_CASE("UdpMessenger | SendByeByeOnDestruction", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  auto iface = test::Interface{};
  {
    auto messenger = makeUdpMessenger(util::injectRef(iface), TestNodeState{5, 10},
      util::injectVal(io.makeIoContext()), 1, 1);
  }
  REQUIRE(2 == iface.sentMessages.size());
  const auto messageBuffer = iface.sentMessages[1].first;
  const auto sentTo = iface.sentMessages[1].second;
  const auto result = v1::parseMessageHeader<TestNodeState::IdType>(
    begin(messageBuffer), end(messageBuffer));
  CHECK(v1::kByeBye == result.first.messageType);
  CHECK(multicastEndpoint() == sentTo);
}

namespace
{

template <typename Messenger>
struct MessengerWrapper
{
  MessengerWrapper(Messenger messenger)
    : mMessenger(std::move(messenger))
  {
  }

  Messenger mMessenger;
};

template <typename Messenger>
MessengerWrapper<Messenger> wrapMessenger(Messenger messenger)
{
  return {std::move(messenger)};
}
} // namespace

TEST_CASE("UdpMessenger | MovingMessengerDoesntSendByeBye", "[UdpMessenger]")
{
  ::ableton::test::serial_io::Fixture io;
  // The destructor for the messenger is written so that if an
  // instance is moved from, it won't send the bye bye message.
  auto iface = test::Interface{};
  {
    auto messenger = makeUdpMessenger(util::injectRef(iface), TestNodeState{5, 10},
      util::injectVal(io.makeIoContext()), 1, 1);
    auto wrapper = wrapMessenger(std::move(messenger));
  }
  // We should have an initial Alive and then a single ByeBye
  CHECK(2 == iface.sentMessages.size());
}

} // namespace discovery
} // namespace ableton
