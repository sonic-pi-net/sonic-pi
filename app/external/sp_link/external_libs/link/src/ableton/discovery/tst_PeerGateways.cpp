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

#include <ableton/discovery/PeerGateways.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/test/serial_io/Fixture.hpp>


namespace ableton
{
namespace discovery
{
namespace
{

struct Gateway
{
  asio::ip::address addr;
};

struct NodeState
{
};

struct Factory
{
  template <typename IoContext>
  Gateway operator()(NodeState, util::Injected<IoContext>, const asio::ip::address& addr)
  {
    return {addr};
  }
};

const asio::ip::address addr1 = asio::ip::address::from_string("192.192.192.1");

const asio::ip::address addr2 = asio::ip::address::from_string("192.192.192.2");

template <typename Gateways>
void expectGatewaysAsync(
  Gateways& gateways, test::serial_io::Fixture& io, std::vector<asio::ip::address> addrs)

{
  using namespace std;
  using GatewayIt = typename Gateways::GatewayMap::iterator;
  bool bTested = false;

  gateways.withGatewaysAsync([addrs, &bTested](GatewayIt begin, const GatewayIt end) {
    bTested = true;
    REQUIRE(static_cast<size_t>(distance(begin, end)) == addrs.size());
    std::size_t i = 0;
    for (; begin != end; ++begin)
    {
      CHECK(begin->first == addrs[i++]);
    }
  });
  io.flush();
  CHECK(bTested);
}

} // anonymous namespace

TEST_CASE("PeerGateways | EmptyIfNoInterfaces", "[PeerGateways]")
{
  test::serial_io::Fixture io;
  auto pGateways = makePeerGateways(
    std::chrono::seconds(2), NodeState{}, Factory{}, util::injectVal(io.makeIoContext()));
  pGateways->enable(true);
  expectGatewaysAsync(*pGateways, io, {});
}


TEST_CASE("PeerGateways | MatchesAfterInitialScan", "[PeerGateways]")
{
  test::serial_io::Fixture io;
  io.setNetworkInterfaces({addr1, addr2});
  auto pGateways = makePeerGateways(
    std::chrono::seconds(2), NodeState{}, Factory{}, util::injectVal(io.makeIoContext()));
  pGateways->enable(true);
  expectGatewaysAsync(*pGateways, io, {addr1, addr2});
}

TEST_CASE("PeerGateways | GatewayAppears", "[PeerGateways]")
{
  test::serial_io::Fixture io;
  io.setNetworkInterfaces({addr1});

  auto pGateways = makePeerGateways(
    std::chrono::seconds(2), NodeState{}, Factory{}, util::injectVal(io.makeIoContext()));
  pGateways->enable(true);
  expectGatewaysAsync(*pGateways, io, {addr1});

  io.setNetworkInterfaces({addr1, addr2});
  io.advanceTime(std::chrono::seconds(3));
  expectGatewaysAsync(*pGateways, io, {addr1, addr2});
}

TEST_CASE("PeerGateways | GatewayDisappears", "[PeerGateways]")
{
  test::serial_io::Fixture io;
  io.setNetworkInterfaces({addr1, addr2});

  auto pGateways = makePeerGateways(
    std::chrono::seconds(2), NodeState{}, Factory{}, util::injectVal(io.makeIoContext()));
  pGateways->enable(true);
  expectGatewaysAsync(*pGateways, io, {addr1, addr2});

  io.setNetworkInterfaces({addr1});
  io.advanceTime(std::chrono::seconds(3));
  expectGatewaysAsync(*pGateways, io, {addr1});
}

TEST_CASE("PeerGateways | GatewayChangesAddress", "[PeerGateways]")
{
  test::serial_io::Fixture io;
  io.setNetworkInterfaces({addr1});
  auto pGateways = makePeerGateways(
    std::chrono::seconds(2), NodeState{}, Factory{}, util::injectVal(io.makeIoContext()));
  pGateways->enable(true);
  expectGatewaysAsync(*pGateways, io, {addr1});

  io.setNetworkInterfaces({addr2});
  io.advanceTime(std::chrono::seconds(3));
  expectGatewaysAsync(*pGateways, io, {addr2});
}

} // namespace discovery
} // namespace ableton
