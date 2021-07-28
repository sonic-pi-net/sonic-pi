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

#include <ableton/discovery/Payload.hpp>
#include <array>

namespace ableton
{
namespace discovery
{
namespace v1
{

// The maximum size of a message, in bytes
const std::size_t kMaxMessageSize = 512;
// Utility typedef for an array of bytes of maximum message size
using MessageBuffer = std::array<uint8_t, v1::kMaxMessageSize>;

using MessageType = uint8_t;
using SessionGroupId = uint16_t;

const MessageType kInvalid = 0;
const MessageType kAlive = 1;
const MessageType kResponse = 2;
const MessageType kByeBye = 3;

template <typename NodeId>
struct MessageHeader
{
  MessageType messageType;
  uint8_t ttl;
  SessionGroupId groupId;
  NodeId ident;

  friend std::uint32_t sizeInByteStream(const MessageHeader& header)
  {
    return discovery::sizeInByteStream(header.messageType)
           + discovery::sizeInByteStream(header.ttl)
           + discovery::sizeInByteStream(header.groupId)
           + discovery::sizeInByteStream(header.ident);
  }

  template <typename It>
  friend It toNetworkByteStream(const MessageHeader& header, It out)
  {
    return discovery::toNetworkByteStream(header.ident,
      discovery::toNetworkByteStream(header.groupId,
        discovery::toNetworkByteStream(header.ttl,
          discovery::toNetworkByteStream(header.messageType, std::move(out)))));
  }

  template <typename It>
  static std::pair<MessageHeader, It> fromNetworkByteStream(It begin, const It end)
  {
    using namespace std;

    MessageHeader header;
    tie(header.messageType, begin) =
      Deserialize<decltype(header.messageType)>::fromNetworkByteStream(begin, end);
    tie(header.ttl, begin) =
      Deserialize<decltype(header.ttl)>::fromNetworkByteStream(begin, end);
    tie(header.groupId, begin) =
      Deserialize<decltype(header.groupId)>::fromNetworkByteStream(begin, end);
    tie(header.ident, begin) =
      Deserialize<decltype(header.ident)>::fromNetworkByteStream(begin, end);

    return make_pair(std::move(header), std::move(begin));
  }
};

namespace detail
{

// Types that are only used in the sending/parsing of messages, not
// publicly exposed.
using ProtocolHeader = std::array<char, 8>;
const ProtocolHeader kProtocolHeader = {{'_', 'a', 's', 'd', 'p', '_', 'v', 1}};

// Must have at least kMaxMessageSize bytes available in the output stream
template <typename NodeId, typename Payload, typename It>
It encodeMessage(NodeId from,
  const uint8_t ttl,
  const MessageType messageType,
  const Payload& payload,
  It out)
{
  using namespace std;
  const MessageHeader<NodeId> header = {messageType, ttl, 0, std::move(from)};
  const auto messageSize =
    kProtocolHeader.size() + sizeInByteStream(header) + sizeInByteStream(payload);

  if (messageSize < kMaxMessageSize)
  {
    return toNetworkByteStream(
      payload, toNetworkByteStream(
                 header, copy(begin(kProtocolHeader), end(kProtocolHeader), std::move(out))));
  }
  else
  {
    throw range_error("Exceeded maximum message size");
  }
}

} // namespace detail

template <typename NodeId, typename Payload, typename It>
It aliveMessage(NodeId from, const uint8_t ttl, const Payload& payload, It out)
{
  return detail::encodeMessage(std::move(from), ttl, kAlive, payload, std::move(out));
}

template <typename NodeId, typename Payload, typename It>
It responseMessage(NodeId from, const uint8_t ttl, const Payload& payload, It out)
{
  return detail::encodeMessage(std::move(from), ttl, kResponse, payload, std::move(out));
}

template <typename NodeId, typename It>
It byeByeMessage(NodeId from, It out)
{
  return detail::encodeMessage(
    std::move(from), 0, kByeBye, makePayload(), std::move(out));
}

template <typename NodeId, typename It>
std::pair<MessageHeader<NodeId>, It> parseMessageHeader(It bytesBegin, const It bytesEnd)
{
  using namespace std;
  using ItDiff = typename iterator_traits<It>::difference_type;

  MessageHeader<NodeId> header = {};
  const auto protocolHeaderSize = discovery::sizeInByteStream(detail::kProtocolHeader);
  const auto minMessageSize =
    static_cast<ItDiff>(protocolHeaderSize + sizeInByteStream(header));

  // If there are enough bytes in the stream to make a header and if
  // the first bytes in the stream are the protocol header, then
  // proceed to parse the stream.
  if (distance(bytesBegin, bytesEnd) >= minMessageSize
      && equal(begin(detail::kProtocolHeader), end(detail::kProtocolHeader), bytesBegin))
  {
    tie(header, bytesBegin) = MessageHeader<NodeId>::fromNetworkByteStream(
      bytesBegin + protocolHeaderSize, bytesEnd);
  }
  return make_pair(std::move(header), std::move(bytesBegin));
}

} // namespace v1
} // namespace discovery
} // namespace ableton
