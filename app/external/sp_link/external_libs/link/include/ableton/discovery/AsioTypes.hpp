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

#pragma once

#include <ableton/platforms/asio/AsioWrapper.hpp>

namespace ableton
{
namespace discovery
{

using IpAddress = LINK_ASIO_NAMESPACE::ip::address;
using IpAddressV4 = LINK_ASIO_NAMESPACE::ip::address_v4;
using IpAddressV6 = LINK_ASIO_NAMESPACE::ip::address_v6;
using UdpSocket = LINK_ASIO_NAMESPACE::ip::udp::socket;
using UdpEndpoint = LINK_ASIO_NAMESPACE::ip::udp::endpoint;

template <typename AsioAddrType>
AsioAddrType makeAddress(const char* pAddr)
{
  using namespace std;
  typename AsioAddrType::bytes_type bytes;
  copy(pAddr, pAddr + bytes.size(), begin(bytes));
  return AsioAddrType{bytes};
}

template <typename AsioAddrType>
AsioAddrType makeAddress(const char* pAddr, uint32_t scopeId)
{
  using namespace std;
  typename AsioAddrType::bytes_type bytes;
  copy(pAddr, pAddr + bytes.size(), begin(bytes));
  return AsioAddrType{bytes, scopeId};
}

} // namespace discovery
} // namespace ableton
