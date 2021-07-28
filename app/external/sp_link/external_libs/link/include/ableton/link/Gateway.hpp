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

#include <ableton/discovery/PeerGateway.hpp>
#include <ableton/link/MeasurementService.hpp>
#include <ableton/link/PeerState.hpp>

namespace ableton
{
namespace link
{

template <typename PeerObserver, typename Clock, typename IoContext>
class Gateway
{
public:
  Gateway(util::Injected<IoContext> io,
    asio::ip::address_v4 addr,
    util::Injected<PeerObserver> observer,
    NodeState nodeState,
    GhostXForm ghostXForm,
    Clock clock)
    : mIo(std::move(io))
    , mMeasurement(addr,
        nodeState.sessionId,
        std::move(ghostXForm),
        std::move(clock),
        util::injectVal(mIo->clone()))
    , mPeerGateway(discovery::makeIpV4Gateway(util::injectRef(*mIo),
        std::move(addr),
        std::move(observer),
        PeerState{std::move(nodeState), mMeasurement.endpoint()}))
  {
  }

  Gateway(const Gateway& rhs) = delete;
  Gateway& operator=(const Gateway& rhs) = delete;

  Gateway(Gateway&& rhs)
    : mIo(std::move(rhs.mIo))
    , mMeasurement(std::move(rhs.mMeasurement))
    , mPeerGateway(std::move(rhs.mPeerGateway))
  {
  }

  Gateway& operator=(Gateway&& rhs)
  {
    mIo = std::move(rhs.mIo);
    mMeasurement = std::move(rhs.mMeasurement);
    mPeerGateway = std::move(rhs.mPeerGateway);
    return *this;
  }

  void updateNodeState(std::pair<NodeState, GhostXForm> state)
  {
    mMeasurement.updateNodeState(state.first.sessionId, state.second);
    mPeerGateway.updateState(PeerState{std::move(state.first), mMeasurement.endpoint()});
  }

  template <typename Handler>
  void measurePeer(const PeerState& peer, Handler handler)
  {
    mMeasurement.measurePeer(peer, std::move(handler));
  }

private:
  util::Injected<IoContext> mIo;
  MeasurementService<Clock, typename std::remove_reference<IoContext>::type> mMeasurement;
  discovery::
    IpV4Gateway<PeerObserver, PeerState, typename util::Injected<IoContext>::type&>
      mPeerGateway;
};

} // namespace link
} // namespace ableton
