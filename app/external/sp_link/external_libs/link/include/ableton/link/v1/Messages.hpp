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
namespace link
{
namespace v1
{

// The maximum size of a message, in bytes
const std::size_t kMaxMessageSize = 512;
// Utility typedef for an array of bytes of maximum message size
using MessageBuffer = std::array<uint8_t, v1::kMaxMessageSize>;

using MessageType = uint8_t;

const MessageType kPing = 1;
const MessageType kPong = 2;

struct MessageHeader
{
  MessageType messageType;

  friend std::uint32_t sizeInByteStream(const MessageHeader& header)
  {
    return discovery::sizeInByteStream(header.messageType);
  }

  template <typename It>
  friend It toNetworkByteStream(const MessageHeader& header, It out)
  {
    return discovery::toNetworkByteStream(header.messageType, std::move(out));
  }

  template <typename It>
  static std::pair<MessageHeader, It> fromNetworkByteStream(It begin, const It end)
  {
    using namespace discovery;

    MessageHeader header;
    std::tie(header.messageType, begin) =
      Deserialize<decltype(header.messageType)>::fromNetworkByteStream(begin, end);

    return std::make_pair(std::move(header), std::move(begin));
  }
};

namespace detail
{

// Types that are only used in the sending/parsing of messages, not
// publicly exposed.
using ProtocolHeader = std::array<char, 8>;
const ProtocolHeader kProtocolHeader = {{'_', 'l', 'i', 'n', 'k', '_', 'v', 1}};

// Must have at least kMaxMessageSize bytes available in the output stream
template <typename Payload, typename It>
It encodeMessage(const MessageType messageType, const Payload& payload, It out)
{
  using namespace std;
  const MessageHeader header = {messageType};
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

template <typename Payload, typename It>
It pingMessage(const Payload& payload, It out)
{
  return detail::encodeMessage(kPing, payload, std::move(out));
}

template <typename Payload, typename It>
It pongMessage(const Payload& payload, It out)
{
  return detail::encodeMessage(kPong, payload, std::move(out));
}

template <typename It>
std::pair<MessageHeader, It> parseMessageHeader(It bytesBegin, const It bytesEnd)
{
  using ItDiff = typename std::iterator_traits<It>::difference_type;

  MessageHeader header = {};
  const auto protocolHeaderSize = discovery::sizeInByteStream(detail::kProtocolHeader);
  const auto minMessageSize =
    static_cast<ItDiff>(protocolHeaderSize + sizeInByteStream(header));

  // If there are enough bytes in the stream to make a header and if
  // the first bytes in the stream are the protocol header, then
  // proceed to parse the stream.
  if (std::distance(bytesBegin, bytesEnd) >= minMessageSize
      && std::equal(
           begin(detail::kProtocolHeader), end(detail::kProtocolHeader), bytesBegin))
  {
    std::tie(header, bytesBegin) =
      MessageHeader::fromNetworkByteStream(bytesBegin + protocolHeaderSize, bytesEnd);
  }
  return std::make_pair(std::move(header), std::move(bytesBegin));
}

} // namespace v1
} // namespace link
} // namespace ableton
