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

#include <ableton/discovery/IpV4Interface.hpp>
#include <ableton/platforms/asio/AsioTimer.hpp>
#include <ableton/platforms/asio/AsioWrapper.hpp>
#include <ableton/platforms/asio/LockFreeCallbackDispatcher.hpp>
#include <ableton/platforms/asio/Socket.hpp>
#include <thread>

namespace ableton
{
namespace platforms
{
namespace asio
{

template <typename ScanIpIfAddrs, typename LogT>
class Context
{
public:
  using Timer = AsioTimer;
  using Log = LogT;

  template <typename Handler, typename Duration>
  using LockFreeCallbackDispatcher = LockFreeCallbackDispatcher<Handler, Duration>;

  template <std::size_t BufferSize>
  using Socket = asio::Socket<BufferSize>;

  Context()
    : Context(DefaultHandler{})
  {
  }

  template <typename ExceptionHandler>
  explicit Context(ExceptionHandler exceptHandler)
    : mpService(new ::asio::io_service())
    , mpWork(new ::asio::io_service::work(*mpService))
  {
    mThread =
      std::thread{[](::asio::io_service& service, ExceptionHandler handler) {
                    for (;;)
                    {
                      try
                      {
                        service.run();
                        break;
                      }
                      catch (const typename ExceptionHandler::Exception& exception)
                      {
                        handler(exception);
                      }
                    }
                  },
        std::ref(*mpService), std::move(exceptHandler)};
  }

  Context(const Context&) = delete;

  Context(Context&& rhs)
    : mpService(std::move(rhs.mpService))
    , mpWork(std::move(rhs.mpWork))
    , mThread(std::move(rhs.mThread))
    , mLog(std::move(rhs.mLog))
    , mScanIpIfAddrs(std::move(rhs.mScanIpIfAddrs))
  {
  }

  ~Context()
  {
    if (mpService && mpWork)
    {
      mpWork.reset();
      mThread.join();
    }
  }

  void stop()
  {
    if (mpService && mpWork)
    {
      mpWork.reset();
      mpService->stop();
      mThread.join();
    }
  }


  template <std::size_t BufferSize>
  Socket<BufferSize> openUnicastSocket(const ::asio::ip::address_v4& addr)
  {
    auto socket = Socket<BufferSize>{*mpService};
    socket.mpImpl->mSocket.set_option(
      ::asio::ip::multicast::enable_loopback(addr.is_loopback()));
    socket.mpImpl->mSocket.set_option(::asio::ip::multicast::outbound_interface(addr));
    socket.mpImpl->mSocket.bind(::asio::ip::udp::endpoint{addr, 0});
    return socket;
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openMulticastSocket(const ::asio::ip::address_v4& addr)
  {
    auto socket = Socket<BufferSize>{*mpService};
    socket.mpImpl->mSocket.set_option(::asio::ip::udp::socket::reuse_address(true));
    socket.mpImpl->mSocket.set_option(
      ::asio::socket_base::broadcast(!addr.is_loopback()));
    socket.mpImpl->mSocket.set_option(
      ::asio::ip::multicast::enable_loopback(addr.is_loopback()));
    socket.mpImpl->mSocket.set_option(::asio::ip::multicast::outbound_interface(addr));
    socket.mpImpl->mSocket.bind({::asio::ip::address::from_string("0.0.0.0"),
      discovery::multicastEndpoint().port()});
    socket.mpImpl->mSocket.set_option(::asio::ip::multicast::join_group(
      discovery::multicastEndpoint().address().to_v4(), addr));
    return socket;
  }

  std::vector<::asio::ip::address> scanNetworkInterfaces()
  {
    return mScanIpIfAddrs();
  }

  Timer makeTimer() const
  {
    return {*mpService};
  }

  Log& log()
  {
    return mLog;
  }

  template <typename Handler>
  void async(Handler handler)
  {
    mpService->post(std::move(handler));
  }

  Context clone() const
  {
    return {};
  }

  template <typename ExceptionHandler>
  Context clone(ExceptionHandler handler) const
  {
    return Context{std::move(handler)};
  }

private:
  // Default handler is hidden and defines a hidden exception type
  // that will never be thrown by other code, so it effectively does
  // not catch.
  struct DefaultHandler
  {
    struct Exception
    {
    };

    void operator()(const Exception&)
    {
    }
  };

  std::unique_ptr<::asio::io_service> mpService;
  std::unique_ptr<::asio::io_service::work> mpWork;
  std::thread mThread;
  Log mLog;
  ScanIpIfAddrs mScanIpIfAddrs;
};

} // namespace asio
} // namespace platforms
} // namespace ableton
