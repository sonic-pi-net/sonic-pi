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
#include <cstdint>
#include <tuple>
#include <utility>

namespace ableton
{
namespace discovery
{
namespace test
{

// Test payload entries

// A fixed-size entry type
struct Foo
{
  static const std::int32_t key = '_foo';
  static_assert(key == 0x5f666f6f, "Unexpected byte order");

  std::int32_t fooVal;

  friend std::uint32_t sizeInByteStream(const Foo& foo)
  {
    // Namespace qualification is needed to avoid ambiguous function definitions
    return discovery::sizeInByteStream(foo.fooVal);
  }

  template <typename It>
  friend It toNetworkByteStream(const Foo& foo, It out)
  {
    return discovery::toNetworkByteStream(foo.fooVal, std::move(out));
  }

  template <typename It>
  static std::pair<Foo, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = Deserialize<decltype(fooVal)>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return std::make_pair(Foo{std::move(result.first)}, std::move(result.second));
  }
};

// A variable-size entry type
struct Bar
{
  static const std::int32_t key = '_bar';
  static_assert(key == 0x5f626172, "Unexpected byte order");

  std::vector<std::uint64_t> barVals;

  friend std::uint32_t sizeInByteStream(const Bar& bar)
  {
    return discovery::sizeInByteStream(bar.barVals);
  }

  template <typename It>
  friend It toNetworkByteStream(const Bar& bar, It out)
  {
    return discovery::toNetworkByteStream(bar.barVals, out);
  }

  template <typename It>
  static std::pair<Bar, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = Deserialize<decltype(barVals)>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return std::make_pair(Bar{std::move(result.first)}, std::move(result.second));
  }
};

// An entry type with two vectors
struct Foobar
{
  static const std::int32_t key = 'fbar';
  static_assert(key == 0x66626172, "Unexpected byte order");

  using FoobarVector = std::vector<std::uint64_t>;
  using FoobarTuple = std::tuple<FoobarVector, FoobarVector>;

  FoobarVector fooVals;
  FoobarVector barVals;

  friend std::uint32_t sizeInByteStream(const Foobar& foobar)
  {
    return discovery::sizeInByteStream(foobar.asTuple());
  }

  template <typename It>
  friend It toNetworkByteStream(const Foobar& foobar, It out)
  {
    return discovery::toNetworkByteStream(foobar.asTuple(), out);
  }

  template <typename It>
  static std::pair<Foobar, It> fromNetworkByteStream(It begin, It end)
  {
    const auto result =
      Deserialize<FoobarTuple>::fromNetworkByteStream(std::move(begin), std::move(end));
    const auto foobar = Foobar{std::get<0>(result.first), std::get<1>(result.first)};
    return std::make_pair(std::move(foobar), std::move(result.second));
  }

  FoobarTuple asTuple() const
  {
    return std::make_tuple(fooVals, barVals);
  }
};

} // namespace test
} // namespace discovery
} // namespace ableton
