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
#if defined(LINK_PLATFORM_MACOSX)
#include <ableton/platforms/darwin/Darwin.hpp>
#elif defined(LINK_PLATFORM_LINUX)
#include <ableton/platforms/linux/Linux.hpp>
#elif defined(LINK_PLATFORM_WINDOWS)
#include <ableton/platforms/windows/Windows.hpp>
#elif defined(ESP_PLATFORM)
#include <ableton/platforms/esp32/Esp32.hpp>
#endif

#include <chrono>
#include <cstdint>
#include <type_traits>
#include <utility>
#include <vector>

#if defined(LINK_PLATFORM_WINDOWS)
#include <WS2tcpip.h>
#include <WinSock2.h>
#include <Windows.h>
#endif

namespace ableton
{
namespace discovery
{

// Concept: NetworkByteStreamSerializable
//
// A type that can be encoded to a stream of bytes and decoded from a
// stream of bytes in network byte order. The following type is for
// documentation purposes only.

struct NetworkByteStreamSerializable
{
  friend std::uint32_t sizeInByteStream(const NetworkByteStreamSerializable&);

  // The byte stream pointed to by 'out' must have sufficient space to
  // hold this object, as defined by sizeInByteStream.
  template <typename It>
  friend It toNetworkByteStream(const NetworkByteStreamSerializable&, It out);
};

// Deserialization aspect of the concept. Outside of the demonstration
// type above because clients must specify the type
// explicitly. Default implementation just defers to a class static
// method on T. For types that can't provide such a method, specialize
// this template.
template <typename T>
struct Deserialize
{
  // Throws std::runtime_exception if parsing the type from the given
  // byte range fails. Returns a pair of the correctly parsed value
  // and an iterator to the next byte to parse.
  template <typename It>
  static std::pair<T, It> fromNetworkByteStream(It begin, It end)
  {
    return T::fromNetworkByteStream(std::move(begin), std::move(end));
  }
};


// Default size implementation. Works for primitive types.

template <typename T,
  typename std::enable_if<std::is_fundamental<T>::value>::type* = nullptr>
std::uint32_t sizeInByteStream(T)
{
  return sizeof(T);
}

namespace detail
{

// utilities for implementing concept for primitive types

template <typename T, typename It>
It copyToByteStream(T t, It out)
{
  using namespace std;
  return copy_n(
    reinterpret_cast<typename iterator_traits<It>::pointer>(&t), sizeof(t), out);
}

template <typename T, typename It>
std::pair<T, It> copyFromByteStream(It begin, const It end)
{
  using namespace std;
  using ItDiff = typename iterator_traits<It>::difference_type;

  if (distance(begin, end) < static_cast<ItDiff>(sizeof(T)))
  {
    throw range_error("Parsing type from byte stream failed");
  }
  else
  {
    T t;
    const auto n = sizeof(t);
    copy_n(begin, n, reinterpret_cast<uint8_t*>(&t));
    return make_pair(t, begin + n);
  }
}

} // namespace detail


// Model the concept for unsigned integral types

// uint8_t
template <typename It>
It toNetworkByteStream(const uint8_t byte, It out)
{
  return detail::copyToByteStream(byte, std::move(out));
}

template <>
struct Deserialize<uint8_t>
{
  template <typename It>
  static std::pair<uint8_t, It> fromNetworkByteStream(It begin, It end)
  {
    return detail::copyFromByteStream<uint8_t>(std::move(begin), std::move(end));
  }
};

// uint16_t
template <typename It>
It toNetworkByteStream(uint16_t s, It out)
{
  return detail::copyToByteStream(htons(s), std::move(out));
}

template <>
struct Deserialize<uint16_t>
{
  template <typename It>
  static std::pair<uint16_t, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = detail::copyFromByteStream<uint16_t>(std::move(begin), std::move(end));
    result.first = ntohs(result.first);
    return result;
  }
};

// uint32_t
template <typename It>
It toNetworkByteStream(uint32_t l, It out)
{
  return detail::copyToByteStream(htonl(l), std::move(out));
}

template <>
struct Deserialize<uint32_t>
{
  template <typename It>
  static std::pair<uint32_t, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = detail::copyFromByteStream<uint32_t>(std::move(begin), std::move(end));
    result.first = ntohl(result.first);
    return result;
  }
};

// int32_t in terms of uint32_t
template <typename It>
It toNetworkByteStream(int32_t l, It out)
{
  return toNetworkByteStream(reinterpret_cast<const uint32_t&>(l), std::move(out));
}

template <>
struct Deserialize<int32_t>
{
  template <typename It>
  static std::pair<int32_t, It> fromNetworkByteStream(It begin, It end)
  {
    auto result =
      Deserialize<uint32_t>::fromNetworkByteStream(std::move(begin), std::move(end));
    return std::make_pair(reinterpret_cast<const int32_t&>(result.first), result.second);
  }
};

// uint64_t
template <typename It>
It toNetworkByteStream(uint64_t ll, It out)
{
  return detail::copyToByteStream(htonll(ll), std::move(out));
}

template <>
struct Deserialize<uint64_t>
{
  template <typename It>
  static std::pair<uint64_t, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = detail::copyFromByteStream<uint64_t>(std::move(begin), std::move(end));
    result.first = ntohll(result.first);
    return result;
  }
};

// int64_t in terms of uint64_t
template <typename It>
It toNetworkByteStream(int64_t ll, It out)
{
  return toNetworkByteStream(reinterpret_cast<const uint64_t&>(ll), std::move(out));
}

template <>
struct Deserialize<int64_t>
{
  template <typename It>
  static std::pair<int64_t, It> fromNetworkByteStream(It begin, It end)
  {
    auto result =
      Deserialize<uint64_t>::fromNetworkByteStream(std::move(begin), std::move(end));
    return std::make_pair(reinterpret_cast<const int64_t&>(result.first), result.second);
  }
};

// bool
inline std::uint32_t sizeInByteStream(bool)
{
  return sizeof(uint8_t);
}

template <typename It>
It toNetworkByteStream(bool bl, It out)
{
  return toNetworkByteStream(static_cast<uint8_t>(bl), std::move(out));
}

template <>
struct Deserialize<bool>
{
  template <typename It>
  static std::pair<bool, It> fromNetworkByteStream(It begin, It end)
  {
    auto result =
      Deserialize<uint8_t>::fromNetworkByteStream(std::move(begin), std::move(end));
    return std::make_pair(result.first != 0, result.second);
  }
};

// std::chrono::microseconds
inline std::uint32_t sizeInByteStream(const std::chrono::microseconds micros)
{
  return sizeInByteStream(micros.count());
}

template <typename It>
It toNetworkByteStream(const std::chrono::microseconds micros, It out)
{
  static_assert(sizeof(int64_t) == sizeof(std::chrono::microseconds::rep),
    "The size of microseconds::rep must matche the size of int64_t.");
  return toNetworkByteStream(static_cast<int64_t>(micros.count()), std::move(out));
}

template <>
struct Deserialize<std::chrono::microseconds>
{
  template <typename It>
  static std::pair<std::chrono::microseconds, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto result = Deserialize<int64_t>::fromNetworkByteStream(std::move(begin), std::move(end));
    return make_pair(chrono::microseconds{result.first}, result.second);
  }
};

namespace detail
{

// Generic serialize/deserialize utilities for containers

template <typename Container>
std::uint32_t containerSizeInByteStream(const Container& container)
{
  std::uint32_t totalSize = 0;
  for (const auto& val : container)
  {
    totalSize += sizeInByteStream(val);
  }
  return totalSize;
}

template <typename Container, typename It>
It containerToNetworkByteStream(const Container& container, It out)
{
  for (const auto& val : container)
  {
    out = toNetworkByteStream(val, out);
  }
  return out;
}

template <typename T, typename BytesIt, typename InsertIt>
BytesIt deserializeContainer(BytesIt bytesBegin,
  const BytesIt bytesEnd,
  InsertIt contBegin,
  const std::uint32_t maxElements)
{
  using namespace std;
  std::uint32_t numElements = 0;
  while (bytesBegin < bytesEnd && numElements < maxElements)
  {
    T newVal;
    tie(newVal, bytesBegin) = Deserialize<T>::fromNetworkByteStream(bytesBegin, bytesEnd);
    *contBegin++ = newVal;
    ++numElements;
  }
  return bytesBegin;
}

} // namespace detail

// Need specific overloads for each container type, but use above
// utilities for common implementation

// array
template <typename T, std::size_t Size>
std::uint32_t sizeInByteStream(const std::array<T, Size>& arr)
{
  return detail::containerSizeInByteStream(arr);
}

template <typename T, std::size_t Size, typename It>
It toNetworkByteStream(const std::array<T, Size>& arr, It out)
{
  return detail::containerToNetworkByteStream(arr, std::move(out));
}

template <typename T, std::size_t Size>
struct Deserialize<std::array<T, Size>>
{
  template <typename It>
  static std::pair<std::array<T, Size>, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    array<T, Size> result{};
    auto resultIt =
      detail::deserializeContainer<T>(std::move(begin), std::move(end), std::move(result.begin()), Size);
    return make_pair(std::move(result), std::move(resultIt));
  }
};

// vector
template <typename T, typename Alloc>
std::uint32_t sizeInByteStream(const std::vector<T, Alloc>& vec)
{
  return sizeof(uint32_t) + detail::containerSizeInByteStream(vec);
}

template <typename T, typename Alloc, typename It>
It toNetworkByteStream(const std::vector<T, Alloc>& vec, It out)
{
  out = toNetworkByteStream(static_cast<uint32_t>(vec.size()), out);
  return detail::containerToNetworkByteStream(vec, std::move(out));
}

template <typename T, typename Alloc>
struct Deserialize<std::vector<T, Alloc>>
{
  template <typename It>
  static std::pair<std::vector<T, Alloc>, It> fromNetworkByteStream(
    It bytesBegin, It bytesEnd)
  {
    using namespace std;
    auto result_size =
      Deserialize<uint32_t>::fromNetworkByteStream(std::move(bytesBegin), bytesEnd);
    vector<T, Alloc> result;
    auto resultIt = detail::deserializeContainer<T>(
      std::move(result_size.second), std::move(bytesEnd), back_inserter(result), result_size.first);
    return make_pair(std::move(result), std::move(resultIt));
  }
};

// 2-tuple
template <typename X, typename Y>
std::uint32_t sizeInByteStream(const std::tuple<X, Y>& tup)
{
  return sizeInByteStream(std::get<0>(tup)) + sizeInByteStream(std::get<1>(tup));
}

template <typename X, typename Y, typename It>
It toNetworkByteStream(const std::tuple<X, Y>& tup, It out)
{
  return toNetworkByteStream(
    std::get<1>(tup), toNetworkByteStream(std::get<0>(tup), std::move(out)));
}

template <typename X, typename Y>
struct Deserialize<std::tuple<X, Y>>
{
  template <typename It>
  static std::pair<std::tuple<X, Y>, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto xres = Deserialize<X>::fromNetworkByteStream(begin, end);
    auto yres = Deserialize<Y>::fromNetworkByteStream(xres.second, end);
    return make_pair(make_tuple(std::move(xres.first), std::move(yres.first)), std::move(yres.second));
  }
};

// 3-tuple
template <typename X, typename Y, typename Z>
std::uint32_t sizeInByteStream(const std::tuple<X, Y, Z>& tup)
{
  return sizeInByteStream(std::get<0>(tup)) + sizeInByteStream(std::get<1>(tup))
         + sizeInByteStream(std::get<2>(tup));
}

template <typename X, typename Y, typename Z, typename It>
It toNetworkByteStream(const std::tuple<X, Y, Z>& tup, It out)
{
  return toNetworkByteStream(
    std::get<2>(tup), toNetworkByteStream(std::get<1>(tup),
                        toNetworkByteStream(std::get<0>(tup), std::move(out))));
}

template <typename X, typename Y, typename Z>
struct Deserialize<std::tuple<X, Y, Z>>
{
  template <typename It>
  static std::pair<std::tuple<X, Y, Z>, It> fromNetworkByteStream(It begin, It end)
  {
    using namespace std;
    auto xres = Deserialize<X>::fromNetworkByteStream(begin, end);
    auto yres = Deserialize<Y>::fromNetworkByteStream(xres.second, end);
    auto zres = Deserialize<Z>::fromNetworkByteStream(yres.second, end);
    return make_pair(make_tuple(std::move(xres.first), std::move(yres.first), std::move(zres.first)),
      std::move(zres.second));
  }
};

} // namespace discovery
} // namespace ableton
