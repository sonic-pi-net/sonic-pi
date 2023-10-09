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

#include <ableton/discovery/NetworkByteStreamSerializable.hpp>
#include <ableton/platforms/asio/AsioWrapper.hpp>
#include <cassert>

namespace ableton
{
namespace link
{

struct MeasurementEndpointV6
{
  static const std::int32_t key = 'mep6';
  static_assert(key == 0x6d657036, "Unexpected byte order");

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const MeasurementEndpointV6 mep)
  {
    if (mep.ep.address().is_v4())
    {
      return 0;
    }
    return discovery::sizeInByteStream(mep.ep.address().to_v6().to_bytes())
           + discovery::sizeInByteStream(mep.ep.port());
  }

  template <typename It>
  friend It toNetworkByteStream(const MeasurementEndpointV6 mep, It out)
  {
    assert(mep.ep.address().is_v6());
    return discovery::toNetworkByteStream(
      mep.ep.port(), discovery::toNetworkByteStream(
                       mep.ep.address().to_v6().to_bytes(), std::move(out)));
  }

  template <typename It>
  static std::pair<MeasurementEndpointV6, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto addrRes =
      discovery::Deserialize<discovery::IpAddressV6::bytes_type>::fromNetworkByteStream(
        std::move(begin), end);
    auto portRes = discovery::Deserialize<std::uint16_t>::fromNetworkByteStream(
      std::move(addrRes.second), end);
    return make_pair(
      MeasurementEndpointV6{
        {discovery::IpAddressV6{std::move(addrRes.first)}, std::move(portRes.first)}},
      std::move(portRes.second));
  }

  discovery::UdpEndpoint ep;
};

} // namespace link
} // namespace ableton
