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

#include <ableton/discovery/InterfaceScanner.hpp>
#include <ableton/platforms/asio/AsioWrapper.hpp>
#include <map>

namespace ableton
{
namespace discovery
{

// GatewayFactory must have an operator()(NodeState, IoRef, asio::ip::address)
// that constructs a new PeerGateway on a given interface address.
template <typename NodeState, typename GatewayFactory, typename IoContext>
class PeerGateways
{
public:
  using IoType = typename util::Injected<IoContext>::type;
  using Gateway = typename std::result_of<GatewayFactory(
    NodeState, util::Injected<IoType&>, asio::ip::address)>::type;
  using GatewayMap = std::map<asio::ip::address, Gateway>;

  PeerGateways(const std::chrono::seconds rescanPeriod,
    NodeState state,
    GatewayFactory factory,
    util::Injected<IoContext> io)
    : mIo(std::move(io))
  {
    mpScannerCallback =
      std::make_shared<Callback>(std::move(state), std::move(factory), *mIo);
    mpScanner = std::make_shared<Scanner>(
      rescanPeriod, util::injectShared(mpScannerCallback), util::injectRef(*mIo));
  }

  ~PeerGateways()
  {
    // Release the callback in the io thread so that gateway cleanup
    // doesn't happen in the client thread
    mIo->async(Deleter{*this});
  }

  PeerGateways(const PeerGateways&) = delete;
  PeerGateways& operator=(const PeerGateways&) = delete;

  PeerGateways(PeerGateways&&) = delete;
  PeerGateways& operator=(PeerGateways&&) = delete;

  void enable(const bool bEnable)
  {
    auto pCallback = mpScannerCallback;
    auto pScanner = mpScanner;

    if (pCallback && pScanner)
    {
      mIo->async([pCallback, pScanner, bEnable] {
        pCallback->mGateways.clear();
        pScanner->enable(bEnable);
      });
    }
  }

  template <typename Handler>
  void withGatewaysAsync(Handler handler)
  {
    auto pCallback = mpScannerCallback;
    if (pCallback)
    {
      mIo->async([pCallback, handler] {
        handler(pCallback->mGateways.begin(), pCallback->mGateways.end());
      });
    }
  }

  void updateNodeState(const NodeState& state)
  {
    auto pCallback = mpScannerCallback;
    if (pCallback)
    {
      mIo->async([pCallback, state] {
        pCallback->mState = state;
        for (const auto& entry : pCallback->mGateways)
        {
          entry.second->updateNodeState(state);
        }
      });
    }
  }

  // If a gateway has become non-responsive or is throwing exceptions,
  // this method can be invoked to either fix it or discard it.
  void repairGateway(const asio::ip::address& gatewayAddr)
  {
    auto pCallback = mpScannerCallback;
    auto pScanner = mpScanner;
    if (pCallback && pScanner)
    {
      mIo->async([pCallback, pScanner, gatewayAddr] {
        if (pCallback->mGateways.erase(gatewayAddr))
        {
          // If we erased a gateway, rescan again immediately so that
          // we will re-initialize it if it's still present
          pScanner->scan();
        }
      });
    }
  }

private:
  struct Callback
  {
    Callback(NodeState state, GatewayFactory factory, IoType& io)
      : mState(std::move(state))
      , mFactory(std::move(factory))
      , mIo(io)
    {
    }

    template <typename AddrRange>
    void operator()(const AddrRange& range)
    {
      using namespace std;
      // Get the set of current addresses.
      vector<asio::ip::address> curAddrs;
      curAddrs.reserve(mGateways.size());
      transform(std::begin(mGateways), std::end(mGateways), back_inserter(curAddrs),
        [](const typename GatewayMap::value_type& vt) { return vt.first; });

      // Now use set_difference to determine the set of addresses that
      // are new and the set of cur addresses that are no longer there
      vector<asio::ip::address> newAddrs;
      set_difference(std::begin(range), std::end(range), std::begin(curAddrs),
        std::end(curAddrs), back_inserter(newAddrs));

      vector<asio::ip::address> staleAddrs;
      set_difference(std::begin(curAddrs), std::end(curAddrs), std::begin(range),
        std::end(range), back_inserter(staleAddrs));

      // Remove the stale addresses
      for (const auto& addr : staleAddrs)
      {
        mGateways.erase(addr);
      }

      // Add the new addresses
      for (const auto& addr : newAddrs)
      {
        try
        {
          // Only handle v4 for now
          if (addr.is_v4())
          {
            info(mIo.log()) << "initializing peer gateway on interface " << addr;
            mGateways.emplace(addr, mFactory(mState, util::injectRef(mIo), addr.to_v4()));
          }
        }
        catch (const runtime_error& e)
        {
          warning(mIo.log()) << "failed to init gateway on interface " << addr
                             << " reason: " << e.what();
        }
      }
    }

    NodeState mState;
    GatewayFactory mFactory;
    IoType& mIo;
    GatewayMap mGateways;
  };

  using Scanner = InterfaceScanner<std::shared_ptr<Callback>, IoType&>;

  struct Deleter
  {
    Deleter(PeerGateways& gateways)
      : mpScannerCallback(std::move(gateways.mpScannerCallback))
      , mpScanner(std::move(gateways.mpScanner))
    {
    }

    void operator()()
    {
      mpScanner.reset();
      mpScannerCallback.reset();
    }

    std::shared_ptr<Callback> mpScannerCallback;
    std::shared_ptr<Scanner> mpScanner;
  };

  std::shared_ptr<Callback> mpScannerCallback;
  std::shared_ptr<Scanner> mpScanner;
  util::Injected<IoContext> mIo;
};

// Factory function
template <typename NodeState, typename GatewayFactory, typename IoContext>
std::unique_ptr<PeerGateways<NodeState, GatewayFactory, IoContext>> makePeerGateways(
  const std::chrono::seconds rescanPeriod,
  NodeState state,
  GatewayFactory factory,
  util::Injected<IoContext> io)
{
  using namespace std;
  using Gateways = PeerGateways<NodeState, GatewayFactory, IoContext>;
  return unique_ptr<Gateways>{
    new Gateways{rescanPeriod, std::move(state), std::move(factory), std::move(io)}};
}

} // namespace discovery
} // namespace ableton
