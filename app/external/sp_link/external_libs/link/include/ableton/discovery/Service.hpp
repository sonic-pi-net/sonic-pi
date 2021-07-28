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

#include <ableton/discovery/PeerGateways.hpp>

namespace ableton
{
namespace discovery
{

template <typename NodeState, typename GatewayFactory, typename IoContext>
class Service
{
public:
  using ServicePeerGateways = PeerGateways<NodeState, GatewayFactory, IoContext>;

  Service(NodeState state, GatewayFactory factory, util::Injected<IoContext> io)
    : mGateways(
        std::chrono::seconds(5), std::move(state), std::move(factory), std::move(io))
  {
  }

  void enable(const bool bEnable)
  {
    mGateways.enable(bEnable);
  }

  // Asynchronously operate on the current set of peer gateways. The
  // handler will be invoked in the service's io context.
  template <typename Handler>
  void withGatewaysAsync(Handler handler)
  {
    mGateways.withGatewaysAsync(std::move(handler));
  }

  void updateNodeState(const NodeState& state)
  {
    mGateways.updateNodeState(state);
  }

  // Repair the gateway with the given address if possible. Its
  // sockets may have been closed, for example, and the gateway needs
  // to be regenerated.
  void repairGateway(const asio::ip::address& gatewayAddr)
  {
    mGateways.repairGateway(gatewayAddr);
  }

private:
  ServicePeerGateways mGateways;
};

} // namespace discovery
} // namespace ableton
