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

#include <ableton/link/PeerState.hpp>
#include <ableton/util/Injected.hpp>
#include <cassert>

namespace ableton
{
namespace link
{

// SessionMembershipCallback is invoked when any change to session
// membership occurs (when any peer joins or leaves a session)
//
// SessionTimelineCallback is invoked with a session id and a timeline
// whenever a new combination of these values is seen
//
// SessionStartStopStateCallback is invoked with a session id and a startStopState
// whenever a new combination of these values is seen

template <typename IoContext,
  typename SessionMembershipCallback,
  typename SessionTimelineCallback,
  typename SessionStartStopStateCallback>
class Peers
{
  // non-movable private implementation type
  struct Impl;

public:
  using Peer = std::pair<PeerState, asio::ip::address>;

  Peers(util::Injected<IoContext> io,
    SessionMembershipCallback membership,
    SessionTimelineCallback timeline,
    SessionStartStopStateCallback startStop)
    : mpImpl(std::make_shared<Impl>(
        std::move(io), std::move(membership), std::move(timeline), std::move(startStop)))
  {
  }

  // The set of peers for a given session, ordered by (peerId, addr).
  // The result will possibly contain multiple entries for the same
  // peer if it is visible through multiple gateways.
  std::vector<Peer> sessionPeers(const SessionId& sid) const
  {
    using namespace std;
    vector<Peer> result;
    auto& peerVec = mpImpl->mPeers;
    copy_if(begin(peerVec), end(peerVec), back_inserter(result), SessionMemberPred{sid});
    return result;
  }

  // Number of individual for a given session.
  std::size_t uniqueSessionPeerCount(const SessionId& sid) const
  {
    using namespace std;
    auto peerVec = sessionPeers(sid);
    auto last = unique(begin(peerVec), end(peerVec),
      [](const Peer& a, const Peer& b) { return a.first.ident() == b.first.ident(); });
    return static_cast<size_t>(distance(begin(peerVec), last));
  }

  void setSessionTimeline(const SessionId& sid, const Timeline& tl)
  {
    // Set the cached timeline for all peers to a new client-specified
    // timeline. When we make a timeline change, we do so
    // optimistically and clients assume that all peers in a session
    // have adopted the newly specified timeline. We must represent
    // this in our cache or else we risk failing to notify about a
    // higher-priority peer timeline that was already seen.
    for (auto& peer : mpImpl->mPeers)
    {
      if (peer.first.sessionId() == sid)
      {
        peer.first.nodeState.timeline = tl;
      }
    }
  }

  // Purge all cached peers that are members of the given session
  void forgetSession(const SessionId& sid)
  {
    using namespace std;
    auto& peerVec = mpImpl->mPeers;
    peerVec.erase(
      remove_if(begin(peerVec), end(peerVec), SessionMemberPred{sid}), end(peerVec));
  }

  void resetPeers()
  {
    mpImpl->mPeers.clear();
  }

  // Observer type that monitors peer discovery on a particular
  // gateway and relays the information to a Peers instance.
  // Models the PeerObserver concept from the discovery module.
  struct GatewayObserver
  {
    using GatewayObserverNodeState = PeerState;
    using GatewayObserverNodeId = NodeId;

    GatewayObserver(std::shared_ptr<Impl> pImpl, asio::ip::address addr)
      : mpImpl(std::move(pImpl))
      , mAddr(std::move(addr))
    {
    }
    GatewayObserver(const GatewayObserver&) = delete;

    GatewayObserver(GatewayObserver&& rhs)
      : mpImpl(std::move(rhs.mpImpl))
      , mAddr(std::move(rhs.mAddr))
    {
    }

    ~GatewayObserver()
    {
      // Check to handle the moved from case
      if (mpImpl)
      {
        auto& io = *mpImpl->mIo;
        io.async(Deleter{*this});
      }
    }

    // model the PeerObserver concept from discovery
    friend void sawPeer(GatewayObserver& observer, const PeerState& state)
    {
      auto pImpl = observer.mpImpl;
      auto addr = observer.mAddr;
      assert(pImpl);
      pImpl->mIo->async([pImpl, addr, state] {
        pImpl->sawPeerOnGateway(std::move(state), std::move(addr));
      });
    }

    friend void peerLeft(GatewayObserver& observer, const NodeId& id)
    {
      auto pImpl = observer.mpImpl;
      auto addr = observer.mAddr;
      pImpl->mIo->async(
        [pImpl, addr, id] { pImpl->peerLeftGateway(std::move(id), std::move(addr)); });
    }

    friend void peerTimedOut(GatewayObserver& observer, const NodeId& id)
    {
      auto pImpl = observer.mpImpl;
      auto addr = observer.mAddr;
      pImpl->mIo->async(
        [pImpl, addr, id] { pImpl->peerLeftGateway(std::move(id), std::move(addr)); });
    }

    struct Deleter
    {
      Deleter(GatewayObserver& observer)
        : mpImpl(std::move(observer.mpImpl))
        , mAddr(std::move(observer.mAddr))
      {
      }

      void operator()()
      {
        mpImpl->gatewayClosed(mAddr);
      }

      std::shared_ptr<Impl> mpImpl;
      asio::ip::address mAddr;
    };

    std::shared_ptr<Impl> mpImpl;
    asio::ip::address mAddr;
  };

  // Factory function for the gateway observer
  friend GatewayObserver makeGatewayObserver(Peers& peers, asio::ip::address addr)
  {
    return GatewayObserver{peers.mpImpl, std::move(addr)};
  }

private:
  struct Impl
  {
    Impl(util::Injected<IoContext> io,
      SessionMembershipCallback membership,
      SessionTimelineCallback timeline,
      SessionStartStopStateCallback startStop)
      : mIo(std::move(io))
      , mSessionMembershipCallback(std::move(membership))
      , mSessionTimelineCallback(std::move(timeline))
      , mSessionStartStopStateCallback(std::move(startStop))
    {
    }

    void sawPeerOnGateway(PeerState peerState, asio::ip::address gatewayAddr)
    {
      using namespace std;

      const auto peerSession = peerState.sessionId();
      const auto peerTimeline = peerState.timeline();
      const auto peerStartStopState = peerState.startStopState();
      bool isNewSessionTimeline = false;
      bool isNewSessionStartStopState = false;
      bool didSessionMembershipChange = false;
      {
        isNewSessionTimeline = !sessionTimelineExists(peerSession, peerTimeline);
        isNewSessionStartStopState =
          !sessionStartStopStateExists(peerSession, peerStartStopState);

        auto peer = make_pair(std::move(peerState), std::move(gatewayAddr));
        const auto idRange = equal_range(begin(mPeers), end(mPeers), peer, PeerIdComp{});

        if (idRange.first == idRange.second)
        {
          // This peer is not currently known on any gateway
          didSessionMembershipChange = true;
          mPeers.insert(std::move(idRange.first), std::move(peer));
        }
        else
        {
          // We've seen this peer before... does it have a new session?
          didSessionMembershipChange =
            all_of(idRange.first, idRange.second, [&peerSession](const Peer& test) {
              return test.first.sessionId() != peerSession;
            });

          // was it on this gateway?
          const auto addrRange =
            equal_range(idRange.first, idRange.second, peer, AddrComp{});

          if (addrRange.first == addrRange.second)
          {
            // First time on this gateway, add it
            mPeers.insert(std::move(addrRange.first), std::move(peer));
          }
          else
          {
            // We have an entry for this peer on this gateway, update it
            *addrRange.first = std::move(peer);
          }
        }
      } // end lock

      // Invoke callbacks outside the critical section
      if (isNewSessionTimeline)
      {
        mSessionTimelineCallback(peerSession, peerTimeline);
      }

      // Pass the start stop state to the Controller after it processed the timeline.
      // A new timeline can cause a session Id change which will prevent processing the
      // new start stop state. By handling the start stop state after the timeline we
      // assure that the start stop state is processed with the correct session Id.
      if (isNewSessionStartStopState)
      {
        mSessionStartStopStateCallback(peerSession, peerStartStopState);
      }

      if (didSessionMembershipChange)
      {
        mSessionMembershipCallback();
      }
    }

    void peerLeftGateway(const NodeId& nodeId, const asio::ip::address& gatewayAddr)
    {
      using namespace std;

      bool didSessionMembershipChange = false;
      {
        auto it = find_if(begin(mPeers), end(mPeers), [&](const Peer& peer) {
          return peer.first.ident() == nodeId && peer.second == gatewayAddr;
        });

        if (it != end(mPeers))
        {
          mPeers.erase(std::move(it));
          didSessionMembershipChange = true;
        }
      } // end lock

      if (didSessionMembershipChange)
      {
        mSessionMembershipCallback();
      }
    }

    void gatewayClosed(const asio::ip::address& gatewayAddr)
    {
      using namespace std;

      {
        mPeers.erase(
          remove_if(begin(mPeers), end(mPeers),
            [&gatewayAddr](const Peer& peer) { return peer.second == gatewayAddr; }),
          end(mPeers));
      } // end lock

      mSessionMembershipCallback();
    }

    template <typename Predicate>
    bool hasPeerWith(const SessionId& sessionId, Predicate predicate)
    {
      using namespace std;
      return find_if(begin(mPeers), end(mPeers), [&](const Peer& peer) {
        return peer.first.sessionId() == sessionId && predicate(peer.first);
      }) != end(mPeers);
    }

    bool sessionTimelineExists(const SessionId& session, const Timeline& timeline)
    {
      return hasPeerWith(session,
        [&](const PeerState& peerState) { return peerState.timeline() == timeline; });
    }

    bool sessionStartStopStateExists(
      const SessionId& sessionId, const StartStopState& startStopState)
    {
      return hasPeerWith(sessionId, [&](const PeerState& peerState) {
        return peerState.startStopState() == startStopState;
      });
    }

    struct PeerIdComp
    {
      bool operator()(const Peer& lhs, const Peer& rhs) const
      {
        return lhs.first.ident() < rhs.first.ident();
      }
    };

    struct AddrComp
    {
      bool operator()(const Peer& lhs, const Peer& rhs) const
      {
        return lhs.second < rhs.second;
      }
    };

    util::Injected<IoContext> mIo;
    SessionMembershipCallback mSessionMembershipCallback;
    SessionTimelineCallback mSessionTimelineCallback;
    SessionStartStopStateCallback mSessionStartStopStateCallback;
    std::vector<Peer> mPeers; // sorted by peerId, unique by (peerId, addr)
  };

  struct SessionMemberPred
  {
    bool operator()(const Peer& peer) const
    {
      return peer.first.sessionId() == sid;
    }

    const SessionId& sid;
  };

  std::shared_ptr<Impl> mpImpl;
};

template <typename Io,
  typename SessionMembershipCallback,
  typename SessionTimelineCallback,
  typename SessionStartStopStateCallback>
Peers<Io,
  SessionMembershipCallback,
  SessionTimelineCallback,
  SessionStartStopStateCallback>
makePeers(util::Injected<Io> io,
  SessionMembershipCallback membershipCallback,
  SessionTimelineCallback timelineCallback,
  SessionStartStopStateCallback startStopStateCallback)
{
  return {std::move(io), std::move(membershipCallback), std::move(timelineCallback),
    std::move(startStopStateCallback)};
}

} // namespace link
} // namespace ableton
