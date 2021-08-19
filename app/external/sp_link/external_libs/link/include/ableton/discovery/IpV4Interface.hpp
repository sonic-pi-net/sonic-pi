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
#include <ableton/util/Injected.hpp>

namespace ableton
{
namespace discovery
{

inline asio::ip::udp::endpoint multicastEndpoint()
{
  return {asio::ip::address_v4::from_string("224.76.78.75"), 20808};
}

// Type tags for dispatching between unicast and multicast packets
struct MulticastTag
{
};
struct UnicastTag
{
};

template <typename IoContext, std::size_t MaxPacketSize>
class IpV4Interface
{
public:
  using Socket = typename util::Injected<IoContext>::type::template Socket<MaxPacketSize>;

  IpV4Interface(util::Injected<IoContext> io, const asio::ip::address_v4& addr)
    : mIo(std::move(io))
    , mMulticastReceiveSocket(mIo->template openMulticastSocket<MaxPacketSize>(addr))
    , mSendSocket(mIo->template openUnicastSocket<MaxPacketSize>(addr))
  {
  }

  IpV4Interface(const IpV4Interface&) = delete;
  IpV4Interface& operator=(const IpV4Interface&) = delete;

  IpV4Interface(IpV4Interface&& rhs)
    : mIo(std::move(rhs.mIo))
    , mMulticastReceiveSocket(std::move(rhs.mMulticastReceiveSocket))
    , mSendSocket(std::move(rhs.mSendSocket))
  {
  }


  std::size_t send(
    const uint8_t* const pData, const size_t numBytes, const asio::ip::udp::endpoint& to)
  {
    return mSendSocket.send(pData, numBytes, to);
  }

  template <typename Handler>
  void receive(Handler handler, UnicastTag)
  {
    mSendSocket.receive(SocketReceiver<UnicastTag, Handler>{std::move(handler)});
  }

  template <typename Handler>
  void receive(Handler handler, MulticastTag)
  {
    mMulticastReceiveSocket.receive(
      SocketReceiver<MulticastTag, Handler>(std::move(handler)));
  }

  asio::ip::udp::endpoint endpoint() const
  {
    return mSendSocket.endpoint();
  }

private:
  template <typename Tag, typename Handler>
  struct SocketReceiver
  {
    SocketReceiver(Handler handler)
      : mHandler(std::move(handler))
    {
    }

    template <typename It>
    void operator()(
      const asio::ip::udp::endpoint& from, const It messageBegin, const It messageEnd)
    {
      mHandler(Tag{}, from, messageBegin, messageEnd);
    }

    Handler mHandler;
  };

  util::Injected<IoContext> mIo;
  Socket mMulticastReceiveSocket;
  Socket mSendSocket;
};

template <std::size_t MaxPacketSize, typename IoContext>
IpV4Interface<IoContext, MaxPacketSize> makeIpV4Interface(
  util::Injected<IoContext> io, const asio::ip::address_v4& addr)
{
  return {std::move(io), addr};
}

} // namespace discovery
} // namespace ableton
