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

#include <ableton/discovery/PeerGateway.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/test/serial_io/Fixture.hpp>
#include <ableton/util/Log.hpp>

namespace ableton
{
namespace discovery
{
namespace
{

struct TestNodeState : std::tuple<std::string, double>
{
  TestNodeState(std::string ident, double tempo)
    : std::tuple<std::string, double>(std::move(ident), std::move(tempo))
  {
  }

  std::string ident() const
  {
    return std::get<0>(*this);
  }
};

struct TestMessenger
{
  template <typename Handler>
  void receive(Handler handler)
  {
    receivePeerState = [handler](const PeerState<TestNodeState>& msg) { handler(msg); };

    receiveByeBye = [handler](const ByeBye<std::string>& msg) { handler(msg); };
  }

  std::function<void(const PeerState<TestNodeState>&)> receivePeerState;
  std::function<void(const ByeBye<std::string>&)> receiveByeBye;
};

struct TestObserver
{
  using GatewayObserverNodeState = TestNodeState;
  using GatewayObserverNodeId = std::string;

  friend void sawPeer(TestObserver& observer, const GatewayObserverNodeState& peer)
  {
    observer.mPeersSeen.push_back(peer);
  }

  friend void peerLeft(TestObserver& observer, const GatewayObserverNodeId& id)
  {
    observer.mPeersLeft.push_back(id);
  }

  friend void peerTimedOut(TestObserver& observer, const GatewayObserverNodeId& id)
  {
    observer.mPeersTimedOut.push_back(id);
  }

  std::vector<GatewayObserverNodeState> mPeersSeen;
  std::vector<GatewayObserverNodeId> mPeersLeft;
  std::vector<GatewayObserverNodeId> mPeersTimedOut;
};

void expectPeersSeen(std::vector<TestNodeState> expected, const TestObserver& observer)
{
  CHECK(expected == observer.mPeersSeen);
}

void expectPeersLeft(std::vector<std::string> expected, const TestObserver& observer)
{
  CHECK(expected == observer.mPeersLeft);
}

void expectPeersTimedOut(std::vector<std::string> expected, const TestObserver& observer)
{
  CHECK(expected == observer.mPeersTimedOut);
}

// Test peers
const auto peerA = TestNodeState{"peerA", 120};
const auto peerB = TestNodeState{"peerB", 150};

} // anonymous namespace

TEST_CASE("PeerGateway | NoActivity", "[PeerGateway]")
{
  test::serial_io::Fixture io;
  TestObserver observer;
  auto listener = makePeerGateway(util::injectVal(TestMessenger{}),
    util::injectRef(observer), util::injectVal(io.makeIoContext()));
  io.advanceTime(std::chrono::seconds(10));

  // Without any outside interaction but the passage of time, our
  // listener should not have seen any peers.
  CHECK(observer.mPeersSeen.empty());
  CHECK(observer.mPeersLeft.empty());
  CHECK(observer.mPeersTimedOut.empty());
}

TEST_CASE("PeerGateway | ReceivedPeerState", "[PeerGateway]")
{
  test::serial_io::Fixture io;
  TestObserver observer;
  TestMessenger messenger;
  auto listener = makePeerGateway(util::injectRef(messenger), util::injectRef(observer),
    util::injectVal(io.makeIoContext()));

  messenger.receivePeerState({peerA, 5});
  io.flush();

  expectPeersSeen({peerA}, observer);
}

TEST_CASE("PeerGateway | TwoPeersOneLeaves", "[PeerGateway]")
{
  test::serial_io::Fixture io;
  TestObserver observer;
  TestMessenger messenger;
  auto listener = makePeerGateway(util::injectRef(messenger), util::injectRef(observer),
    util::injectVal(io.makeIoContext()));

  messenger.receivePeerState({peerA, 5});
  messenger.receivePeerState({peerB, 5});
  messenger.receiveByeBye({peerA.ident()});
  io.flush();

  expectPeersSeen({peerA, peerB}, observer);
  expectPeersLeft({peerA.ident()}, observer);
}

TEST_CASE("PeerGateway | TwoPeersOneTimesOut", "[PeerGateway]")
{
  test::serial_io::Fixture io;
  TestObserver observer;
  TestMessenger messenger;
  auto listener = makePeerGateway(util::injectRef(messenger), util::injectRef(observer),
    util::injectVal(io.makeIoContext()));

  messenger.receivePeerState({peerA, 5});
  messenger.receivePeerState({peerB, 10});

  io.advanceTime(std::chrono::seconds(3));
  expectPeersTimedOut({}, observer);
  io.advanceTime(std::chrono::seconds(4));
  expectPeersTimedOut({peerA.ident()}, observer);
}

TEST_CASE("PeerGateway | PeerTimesOutAndIsSeenAgain", "[PeerGateway]")
{
  test::serial_io::Fixture io;
  TestObserver observer;
  TestMessenger messenger;
  auto listener = makePeerGateway(util::injectRef(messenger), util::injectRef(observer),
    util::injectVal(io.makeIoContext()));

  messenger.receivePeerState({peerA, 5});
  io.advanceTime(std::chrono::seconds(7));

  expectPeersTimedOut({peerA.ident()}, observer);

  messenger.receivePeerState({peerA, 5});
  io.advanceTime(std::chrono::seconds(3));

  expectPeersSeen({peerA, peerA}, observer);
  expectPeersTimedOut({peerA.ident()}, observer);
}

} // namespace discovery
} // namespace ableton
