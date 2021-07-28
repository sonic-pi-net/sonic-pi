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

#include <ableton/discovery/UdpMessenger.hpp>
#include <ableton/discovery/v1/Messages.hpp>
#include <ableton/util/SafeAsyncHandler.hpp>
#include <memory>

namespace ableton
{
namespace discovery
{

template <typename Messenger, typename PeerObserver, typename IoContext>
class PeerGateway
{
public:
  // The peer types are defined by the observer but must match with those
  // used by the Messenger
  using ObserverT = typename util::Injected<PeerObserver>::type;
  using NodeState = typename ObserverT::GatewayObserverNodeState;
  using NodeId = typename ObserverT::GatewayObserverNodeId;
  using Timer = typename util::Injected<IoContext>::type::Timer;
  using TimerError = typename Timer::ErrorCode;

  PeerGateway(util::Injected<Messenger> messenger,
    util::Injected<PeerObserver> observer,
    util::Injected<IoContext> io)
    : mpImpl(new Impl(std::move(messenger), std::move(observer), std::move(io)))
  {
    mpImpl->listen();
  }

  PeerGateway(const PeerGateway&) = delete;
  PeerGateway& operator=(const PeerGateway&) = delete;

  PeerGateway(PeerGateway&& rhs)
    : mpImpl(std::move(rhs.mpImpl))
  {
  }

  void updateState(NodeState state)
  {
    mpImpl->updateState(std::move(state));
  }

private:
  using PeerTimeout = std::pair<std::chrono::system_clock::time_point, NodeId>;
  using PeerTimeouts = std::vector<PeerTimeout>;

  struct Impl : std::enable_shared_from_this<Impl>
  {
    Impl(util::Injected<Messenger> messenger,
      util::Injected<PeerObserver> observer,
      util::Injected<IoContext> io)
      : mMessenger(std::move(messenger))
      , mObserver(std::move(observer))
      , mIo(std::move(io))
      , mPruneTimer(mIo->makeTimer())
    {
    }

    void updateState(NodeState state)
    {
      mMessenger->updateState(std::move(state));
      try
      {
        mMessenger->broadcastState();
      }
      catch (const std::runtime_error& err)
      {
        info(mIo->log()) << "State broadcast failed on gateway: " << err.what();
      }
    }

    void listen()
    {
      mMessenger->receive(util::makeAsyncSafe(this->shared_from_this()));
    }

    // Operators for handling incoming messages
    void operator()(const PeerState<NodeState>& msg)
    {
      onPeerState(msg.peerState, msg.ttl);
      listen();
    }

    void operator()(const ByeBye<NodeId>& msg)
    {
      onByeBye(msg.peerId);
      listen();
    }

    void onPeerState(const NodeState& nodeState, const int ttl)
    {
      using namespace std;
      const auto peerId = nodeState.ident();
      const auto existing = findPeer(peerId);
      if (existing != end(mPeerTimeouts))
      {
        // If the peer is already present in our timeout list, remove it
        // as it will be re-inserted below.
        mPeerTimeouts.erase(existing);
      }

      auto newTo = make_pair(mPruneTimer.now() + std::chrono::seconds(ttl), peerId);
      mPeerTimeouts.insert(
        upper_bound(begin(mPeerTimeouts), end(mPeerTimeouts), newTo, TimeoutCompare{}),
        std::move(newTo));

      sawPeer(*mObserver, nodeState);
      scheduleNextPruning();
    }

    void onByeBye(const NodeId& peerId)
    {
      const auto it = findPeer(peerId);
      if (it != mPeerTimeouts.end())
      {
        peerLeft(*mObserver, it->second);
        mPeerTimeouts.erase(it);
      }
    }

    void pruneExpiredPeers()
    {
      using namespace std;

      const auto test = make_pair(mPruneTimer.now(), NodeId{});
      debug(mIo->log()) << "pruning peers @ " << test.first.time_since_epoch().count();

      const auto endExpired =
        lower_bound(begin(mPeerTimeouts), end(mPeerTimeouts), test, TimeoutCompare{});

      for_each(begin(mPeerTimeouts), endExpired, [this](const PeerTimeout& pto) {
        info(mIo->log()) << "pruning peer " << pto.second;
        peerTimedOut(*mObserver, pto.second);
      });
      mPeerTimeouts.erase(begin(mPeerTimeouts), endExpired);
      scheduleNextPruning();
    }

    void scheduleNextPruning()
    {
      // Find the next peer to expire and set the timer based on it
      if (!mPeerTimeouts.empty())
      {
        // Add a second of padding to the timer to avoid over-eager timeouts
        const auto t = mPeerTimeouts.front().first + std::chrono::seconds(1);

        debug(mIo->log()) << "scheduling next pruning for "
                          << t.time_since_epoch().count() << " because of peer "
                          << mPeerTimeouts.front().second;

        mPruneTimer.expires_at(t);
        mPruneTimer.async_wait([this](const TimerError e) {
          if (!e)
          {
            pruneExpiredPeers();
          }
        });
      }
    }

    struct TimeoutCompare
    {
      bool operator()(const PeerTimeout& lhs, const PeerTimeout& rhs) const
      {
        return lhs.first < rhs.first;
      }
    };

    typename PeerTimeouts::iterator findPeer(const NodeId& peerId)
    {
      return std::find_if(begin(mPeerTimeouts), end(mPeerTimeouts),
        [&peerId](const PeerTimeout& pto) { return pto.second == peerId; });
    }

    util::Injected<Messenger> mMessenger;
    util::Injected<PeerObserver> mObserver;
    util::Injected<IoContext> mIo;
    Timer mPruneTimer;
    PeerTimeouts mPeerTimeouts; // Invariant: sorted by time_point
  };

  std::shared_ptr<Impl> mpImpl;
};

template <typename Messenger, typename PeerObserver, typename IoContext>
PeerGateway<Messenger, PeerObserver, IoContext> makePeerGateway(
  util::Injected<Messenger> messenger,
  util::Injected<PeerObserver> observer,
  util::Injected<IoContext> io)
{
  return {std::move(messenger), std::move(observer), std::move(io)};
}

// IpV4 gateway types
template <typename StateQuery, typename IoContext>
using IpV4Messenger = UdpMessenger<
  IpV4Interface<typename util::Injected<IoContext>::type&, v1::kMaxMessageSize>,
  StateQuery,
  IoContext>;

template <typename PeerObserver, typename StateQuery, typename IoContext>
using IpV4Gateway =
  PeerGateway<IpV4Messenger<StateQuery, typename util::Injected<IoContext>::type&>,
    PeerObserver,
    IoContext>;

// Factory function to bind a PeerGateway to an IpV4Interface with the given address.
template <typename PeerObserver, typename NodeState, typename IoContext>
IpV4Gateway<PeerObserver, NodeState, IoContext> makeIpV4Gateway(
  util::Injected<IoContext> io,
  const asio::ip::address_v4& addr,
  util::Injected<PeerObserver> observer,
  NodeState state)
{
  using namespace std;
  using namespace util;

  const uint8_t ttl = 5;
  const uint8_t ttlRatio = 20;

  auto iface = makeIpV4Interface<v1::kMaxMessageSize>(injectRef(*io), addr);

  auto messenger =
    makeUdpMessenger(injectVal(std::move(iface)), std::move(state), injectRef(*io), ttl, ttlRatio);
  return {injectVal(std::move(messenger)), std::move(observer), std::move(io)};
}

} // namespace discovery
} // namespace ableton
