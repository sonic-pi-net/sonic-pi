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
#include <functional>
#include <sstream>
#include <unordered_map>

namespace ableton
{
namespace discovery
{

struct PayloadEntryHeader
{
  using Key = std::uint32_t;
  using Size = std::uint32_t;

  Key key;
  Size size;

  friend Size sizeInByteStream(const PayloadEntryHeader& header)
  {
    return sizeInByteStream(header.key) + sizeInByteStream(header.size);
  }

  template <typename It>
  friend It toNetworkByteStream(const PayloadEntryHeader& header, It out)
  {
    return toNetworkByteStream(
      header.size, toNetworkByteStream(header.key, std::move(out)));
  }

  template <typename It>
  static std::pair<PayloadEntryHeader, It> fromNetworkByteStream(It begin, const It end)
  {
    using namespace std;
    Key key;
    Size size;
    tie(key, begin) = Deserialize<Key>::fromNetworkByteStream(begin, end);
    tie(size, begin) = Deserialize<Size>::fromNetworkByteStream(begin, end);
    return make_pair(PayloadEntryHeader{std::move(key), std::move(size)}, std::move(begin));
  }
};

template <typename EntryType>
struct PayloadEntry
{
  PayloadEntry(EntryType entryVal)
    : value(std::move(entryVal))
  {
    header = {EntryType::key, sizeInByteStream(value)};
  }

  PayloadEntryHeader header;
  EntryType value;

  friend std::uint32_t sizeInByteStream(const PayloadEntry& entry)
  {
    return sizeInByteStream(entry.header) + sizeInByteStream(entry.value);
  }

  template <typename It>
  friend It toNetworkByteStream(const PayloadEntry& entry, It out)
  {
    return toNetworkByteStream(
      entry.value, toNetworkByteStream(entry.header, std::move(out)));
  }
};

namespace detail
{

template <typename It>
using HandlerMap =
  std::unordered_map<typename PayloadEntryHeader::Key, std::function<void(It, It)>>;

// Given an index of handlers and a byte range, parse the bytes as a
// sequence of payload entries and invoke the appropriate handler for
// each entry type. Entries that are encountered that do not have a
// corresponding handler in the map are ignored. Throws
// std::runtime_error if parsing fails for any entry. Note that if an
// exception is thrown, some of the handlers may have already been called.
template <typename It>
void parseByteStream(HandlerMap<It>& map, It bsBegin, const It bsEnd)
{
  using namespace std;

  while (bsBegin < bsEnd)
  {
    // Try to parse an entry header at this location in the byte stream
    PayloadEntryHeader header;
    It valueBegin;
    tie(header, valueBegin) =
      Deserialize<PayloadEntryHeader>::fromNetworkByteStream(bsBegin, bsEnd);

    // Ensure that the reported size of the entry does not exceed the
    // length of the byte stream
    It valueEnd = valueBegin + header.size;
    if (bsEnd < valueEnd)
    {
      throw range_error("Payload with incorrect size.");
    }

    // The next entry will start at the end of this one
    bsBegin = valueEnd;

    // Use the appropriate handler for this entry, if available
    auto handlerIt = map.find(header.key);
    if (handlerIt != end(map))
    {
      handlerIt->second(std::move(valueBegin), std::move(valueEnd));
    }
  }
}

} // namespace detail


// Payload encoding
template <typename... Entries>
struct Payload;

template <typename First, typename Rest>
struct Payload<First, Rest>
{
  Payload(First first, Rest rest)
    : mFirst(std::move(first))
    , mRest(std::move(rest))
  {
  }

  Payload(PayloadEntry<First> first, Rest rest)
    : mFirst(std::move(first))
    , mRest(std::move(rest))
  {
  }

  template <typename RhsFirst, typename RhsRest>
  using PayloadSum =
    Payload<First, typename Rest::template PayloadSum<RhsFirst, RhsRest>>;

  // Concatenate payloads together into a single payload
  template <typename RhsFirst, typename RhsRest>
  friend PayloadSum<RhsFirst, RhsRest> operator+(
    Payload lhs, Payload<RhsFirst, RhsRest> rhs)
  {
    return {std::move(lhs.mFirst), std::move(lhs.mRest) + std::move(rhs)};
  }

  friend std::size_t sizeInByteStream(const Payload& payload)
  {
    return sizeInByteStream(payload.mFirst) + sizeInByteStream(payload.mRest);
  }

  template <typename It>
  friend It toNetworkByteStream(const Payload& payload, It streamIt)
  {
    return toNetworkByteStream(
      payload.mRest, toNetworkByteStream(payload.mFirst, std::move(streamIt)));
  }

  PayloadEntry<First> mFirst;
  Rest mRest;
};

template <>
struct Payload<>
{
  template <typename RhsFirst, typename RhsRest>
  using PayloadSum = Payload<RhsFirst, RhsRest>;

  template <typename RhsFirst, typename RhsRest>
  friend PayloadSum<RhsFirst, RhsRest> operator+(Payload, Payload<RhsFirst, RhsRest> rhs)
  {
    return rhs;
  }

  friend std::size_t sizeInByteStream(const Payload&)
  {
    return 0;
  }

  template <typename It>
  friend It toNetworkByteStream(const Payload&, It streamIt)
  {
    return streamIt;
  }
};

template <typename... Entries>
struct PayloadBuilder;

// Payload factory function
template <typename... Entries>
auto makePayload(Entries... entries)
  -> decltype(PayloadBuilder<Entries...>{}(std::move(entries)...))
{
  return PayloadBuilder<Entries...>{}(std::move(entries)...);
}

template <typename First, typename... Rest>
struct PayloadBuilder<First, Rest...>
{
  auto operator()(First first, Rest... rest)
    -> Payload<First, decltype(makePayload(std::move(rest)...))>
  {
    return {std::move(first), makePayload(std::move(rest)...)};
  }
};

template <>
struct PayloadBuilder<>
{
  Payload<> operator()()
  {
    return {};
  }
};

// Parse payloads to values
template <typename... Entries>
struct ParsePayload;

template <typename First, typename... Rest>
struct ParsePayload<First, Rest...>
{
  template <typename It, typename... Handlers>
  static void parse(It begin, It end, Handlers... handlers)
  {
    detail::HandlerMap<It> map;
    collectHandlers(map, std::move(handlers)...);
    detail::parseByteStream(map, std::move(begin), std::move(end));
  }

  template <typename It, typename FirstHandler, typename... RestHandlers>
  static void collectHandlers(
    detail::HandlerMap<It>& map, FirstHandler handler, RestHandlers... rest)
  {
    using namespace std;
    map[First::key] = [handler](const It begin, const It end) {
      const auto res = First::fromNetworkByteStream(begin, end);
      if (res.second != end)
      {
        std::ostringstream stringStream;
        stringStream << "Parsing payload entry " << First::key
                     << " did not consume the expected number of bytes. "
                     << " Expected: " << distance(begin, end)
                     << ", Actual: " << distance(begin, res.second);
        throw range_error(stringStream.str());
      }
      handler(res.first);
    };

    ParsePayload<Rest...>::collectHandlers(map, std::move(rest)...);
  }
};

template <>
struct ParsePayload<>
{
  template <typename It>
  static void collectHandlers(detail::HandlerMap<It>&)
  {
  }
};

template <typename... Entries, typename It, typename... Handlers>
void parsePayload(It begin, It end, Handlers... handlers)
{
  using namespace std;
  ParsePayload<Entries...>::parse(std::move(begin), std::move(end), std::move(handlers)...);
}

} // namespace discovery
} // namespace ableton
