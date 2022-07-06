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

#include <ableton/platforms/asio/AsioWrapper.hpp>
#include <ableton/platforms/asio/Util.hpp>
#include <iphlpapi.h>
#include <stdio.h>
#include <vector>
#include <winsock2.h>
#include <ws2tcpip.h>

#pragma comment(lib, "iphlpapi.lib")
#pragma comment(lib, "ws2_32.lib")

namespace ableton
{
namespace platforms
{
namespace windows
{
namespace detail
{
// RAII type to make [get,free]ifaddrs function pairs exception safe
class GetIfAddrs
{
public:
  GetIfAddrs()
  {
    const int MAX_TRIES = 3;               // MSFT recommendation
    const int WORKING_BUFFER_SIZE = 15000; // MSFT recommendation

    DWORD adapter_addrs_buffer_size = WORKING_BUFFER_SIZE;
    for (int i = 0; i < MAX_TRIES; i++)
    {
      adapter_addrs = (IP_ADAPTER_ADDRESSES*)malloc(adapter_addrs_buffer_size);
      assert(adapter_addrs);

      DWORD error = ::GetAdaptersAddresses(AF_UNSPEC,
        GAA_FLAG_SKIP_ANYCAST | GAA_FLAG_SKIP_MULTICAST | GAA_FLAG_SKIP_DNS_SERVER
          | GAA_FLAG_SKIP_FRIENDLY_NAME,
        NULL, adapter_addrs, &adapter_addrs_buffer_size);

      if (error == ERROR_SUCCESS)
      {
        break;
      }
      // if buffer too small, use new buffer size in next iteration
      if (error == ERROR_BUFFER_OVERFLOW)
      {
        free(adapter_addrs);
        adapter_addrs = NULL;
        continue;
      }
    }
  }
  ~GetIfAddrs()
  {
    if (adapter_addrs)
      free(adapter_addrs);
  }

  // RAII must not copy
  GetIfAddrs(GetIfAddrs&) = delete;
  GetIfAddrs& operator=(GetIfAddrs&) = delete;

  template <typename Function>
  void withIfAddrs(Function f)
  {
    if (adapter_addrs)
      f(*adapter_addrs);
  }

private:
  IP_ADAPTER_ADDRESSES* adapter_addrs;
  IP_ADAPTER_ADDRESSES* adapter;
};

} // namespace detail

struct ScanIpIfAddrs
{
  // Scan active network interfaces and return corresponding addresses
  // for all ip-based interfaces.
  std::vector<::asio::ip::address> operator()()
  {
    std::vector<::asio::ip::address> addrs;

    detail::GetIfAddrs getIfAddrs;
    getIfAddrs.withIfAddrs([&addrs](const IP_ADAPTER_ADDRESSES& interfaces) {
      const IP_ADAPTER_ADDRESSES* networkInterface;
      for (networkInterface = &interfaces; networkInterface;
           networkInterface = networkInterface->Next)
      {
        for (IP_ADAPTER_UNICAST_ADDRESS* address = networkInterface->FirstUnicastAddress;
             NULL != address; address = address->Next)
        {
          auto family = address->Address.lpSockaddr->sa_family;
          if (AF_INET == family)
          {
            // IPv4
            SOCKADDR_IN* addr4 =
              reinterpret_cast<SOCKADDR_IN*>(address->Address.lpSockaddr);
            auto bytes = reinterpret_cast<const char*>(&addr4->sin_addr);
            addrs.emplace_back(asio::makeAddress<::asio::ip::address_v4>(bytes));
          }
          else if (AF_INET6 == family)
          {
            SOCKADDR_IN6* addr6 =
              reinterpret_cast<SOCKADDR_IN6*>(address->Address.lpSockaddr);
            auto bytes = reinterpret_cast<const char*>(&addr6->sin6_addr);
            addrs.emplace_back(asio::makeAddress<::asio::ip::address_v6>(bytes));
          }
        }
      }
    });
    return addrs;
  }
};

} // namespace windows
} // namespace platforms
} // namespace ableton
