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

#include <ableton/discovery/NetworkByteStreamSerializable.hpp>
#include <ableton/platforms/asio/AsioWrapper.hpp>

namespace ableton
{
namespace link
{

struct MeasurementEndpointV4
{
  static const std::int32_t key = 'mep4';
  static_assert(key == 0x6d657034, "Unexpected byte order");

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const MeasurementEndpointV4 mep)
  {
    return discovery::sizeInByteStream(
             static_cast<std::uint32_t>(mep.ep.address().to_v4().to_ulong()))
           + discovery::sizeInByteStream(mep.ep.port());
  }

  template <typename It>
  friend It toNetworkByteStream(const MeasurementEndpointV4 mep, It out)
  {
    return discovery::toNetworkByteStream(mep.ep.port(),
      discovery::toNetworkByteStream(
        static_cast<std::uint32_t>(mep.ep.address().to_v4().to_ulong()), std::move(out)));
  }

  template <typename It>
  static std::pair<MeasurementEndpointV4, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto addrRes =
      discovery::Deserialize<std::uint32_t>::fromNetworkByteStream(std::move(begin), end);
    auto portRes = discovery::Deserialize<std::uint16_t>::fromNetworkByteStream(
      std::move(addrRes.second), end);
    return make_pair(MeasurementEndpointV4{{asio::ip::address_v4{std::move(addrRes.first)},
                       std::move(portRes.first)}},
      std::move(portRes.second));
  }

  asio::ip::udp::endpoint ep;
};

} // namespace link
} // namespace ableton
