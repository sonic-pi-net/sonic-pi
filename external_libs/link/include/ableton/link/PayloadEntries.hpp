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
#include <cmath>
#include <cstdint>

namespace ableton
{
namespace link
{

struct HostTime
{
  static const std::int32_t key = '__ht';
  static_assert(key == 0x5f5f6874, "Unexpected byte order");

  HostTime() = default;

  HostTime(const std::chrono::microseconds tm)
    : time(tm)
  {
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const HostTime& sht)
  {
    return discovery::sizeInByteStream(std::move(sht.time));
  }

  template <typename It>
  friend It toNetworkByteStream(const HostTime& sht, It out)
  {
    return discovery::toNetworkByteStream(std::move(sht.time), std::move(out));
  }

  template <typename It>
  static std::pair<HostTime, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto result = discovery::Deserialize<chrono::microseconds>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return make_pair(HostTime{std::move(result.first)}, std::move(result.second));
  }

  std::chrono::microseconds time;
};

struct GHostTime
{
  static const std::int32_t key = '__gt';
  static_assert(key == 0x5f5f6774, "Unexpected byte order");

  GHostTime() = default;

  GHostTime(const std::chrono::microseconds tm)
    : time(tm)
  {
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const GHostTime& dgt)
  {
    return discovery::sizeInByteStream(std::move(dgt.time));
  }

  template <typename It>
  friend It toNetworkByteStream(const GHostTime& dgt, It out)
  {
    return discovery::toNetworkByteStream(std::move(dgt.time), std::move(out));
  }

  template <typename It>
  static std::pair<GHostTime, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto result = discovery::Deserialize<chrono::microseconds>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return make_pair(GHostTime{std::move(result.first)}, std::move(result.second));
  }

  std::chrono::microseconds time;
};

struct PrevGHostTime
{
  static const std::int32_t key = '_pgt';
  static_assert(key == 0x5f706774, "Unexpected byte order");

  PrevGHostTime() = default;

  PrevGHostTime(const std::chrono::microseconds tm)
    : time(tm)
  {
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const PrevGHostTime& dgt)
  {
    return discovery::sizeInByteStream(std::move(dgt.time));
  }

  template <typename It>
  friend It toNetworkByteStream(const PrevGHostTime& pdgt, It out)
  {
    return discovery::toNetworkByteStream(std::move(pdgt.time), std::move(out));
  }

  template <typename It>
  static std::pair<PrevGHostTime, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto result = discovery::Deserialize<chrono::microseconds>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return make_pair(PrevGHostTime{std::move(result.first)}, std::move(result.second));
  }

  std::chrono::microseconds time;
};

} // namespace link
} // namespace ableton
