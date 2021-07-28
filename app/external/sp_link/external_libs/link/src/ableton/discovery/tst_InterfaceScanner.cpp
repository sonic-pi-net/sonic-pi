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

#include <ableton/discovery/InterfaceScanner.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/test/serial_io/Fixture.hpp>

namespace ableton
{
namespace link
{
namespace
{

struct TestCallback
{
  template <typename AddrRange>
  void operator()(AddrRange addrs)
  {
    addrRanges.emplace_back(begin(addrs), end(addrs));
  }

  std::vector<std::vector<asio::ip::address>> addrRanges;
};

// some test addresses
const asio::ip::address addr1 = asio::ip::address::from_string("123.123.123.1");
const asio::ip::address addr2 = asio::ip::address::from_string("123.123.123.2");

} // anonymous namespace

TEST_CASE("InterfaceScanner | NoInterfacesThenOne", "[InterfaceScanner]")
{
  test::serial_io::Fixture io;
  auto callback = TestCallback{};
  {
    auto scanner = discovery::makeInterfaceScanner(std::chrono::seconds(2),
      util::injectRef(callback), util::injectVal(io.makeIoContext()));
    scanner.enable(true);
    CHECK(1 == callback.addrRanges.size());
    io.setNetworkInterfaces({addr1});
    io.advanceTime(std::chrono::seconds(3));
  }
  REQUIRE(2 == callback.addrRanges.size());
  CHECK(0 == callback.addrRanges[0].size());
  REQUIRE(1 == callback.addrRanges[1].size());
  CHECK(addr1 == callback.addrRanges[1].front());
}

TEST_CASE("InterfaceScanner | InterfaceGoesAway", "[InterfaceScanner]")
{
  test::serial_io::Fixture io;
  io.setNetworkInterfaces({addr1});
  auto callback = TestCallback{};
  {
    auto scanner = discovery::makeInterfaceScanner(std::chrono::seconds(2),
      util::injectRef(callback), util::injectVal(io.makeIoContext()));
    scanner.enable(true);
    io.setNetworkInterfaces({});
    io.advanceTime(std::chrono::seconds(3));
  }
  REQUIRE(2 == callback.addrRanges.size());
  REQUIRE(1 == callback.addrRanges[0].size());
  CHECK(addr1 == callback.addrRanges[0].front());
  CHECK(0 == callback.addrRanges[1].size());
}

} // namespace link
} // namespace ableton
