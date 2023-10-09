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

#include <ableton/discovery/IpInterface.hpp>
#include <ableton/platforms/asio/AsioTimer.hpp>
#include <ableton/platforms/asio/LockFreeCallbackDispatcher.hpp>
#include <ableton/platforms/asio/Socket.hpp>
#include <thread>
#include <utility>

namespace ableton
{
namespace platforms
{
namespace LINK_ASIO_NAMESPACE
{
namespace
{

struct ThreadFactory
{
  template <typename Callable, typename... Args>
  static std::thread makeThread(std::string, Callable&& f, Args&&... args)
  {
    return std::thread(std::forward<Callable>(f), std::forward<Args>(args)...);
  }
};

} // namespace

template <typename ScanIpIfAddrs, typename LogT, typename ThreadFactoryT = ThreadFactory>
class Context
{
public:
  using Timer = LINK_ASIO_NAMESPACE::AsioTimer;
  using Log = LogT;

  template <typename Handler, typename Duration>
  using LockFreeCallbackDispatcher =
    LockFreeCallbackDispatcher<Handler, Duration, ThreadFactoryT>;

  template <std::size_t BufferSize>
  using Socket = Socket<BufferSize>;
  using IoService = ::LINK_ASIO_NAMESPACE::io_service;
  using Work = IoService::work;

  Context()
    : Context(DefaultHandler{})
  {
  }

  template <typename ExceptionHandler>
  explicit Context(ExceptionHandler exceptHandler)
    : mpService(new IoService())
    , mpWork(new Work(*mpService))
  {
    mThread = ThreadFactoryT::makeThread("Link Main",
      [](IoService& service, ExceptionHandler handler) {
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
      std::ref(*mpService), std::move(exceptHandler));
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
  Socket<BufferSize> openUnicastSocket(const discovery::IpAddress addr)
  {
    auto socket =
      addr.is_v4() ? Socket<BufferSize>{*mpService, ::LINK_ASIO_NAMESPACE::ip::udp::v4()}
                   : Socket<BufferSize>{*mpService, ::LINK_ASIO_NAMESPACE::ip::udp::v6()};
    socket.mpImpl->mSocket.set_option(
      ::LINK_ASIO_NAMESPACE::ip::multicast::enable_loopback(addr.is_loopback()));
    if (addr.is_v4())
    {
      socket.mpImpl->mSocket.set_option(
        ::LINK_ASIO_NAMESPACE::ip::multicast::outbound_interface(addr.to_v4()));
      socket.mpImpl->mSocket.bind(
        ::LINK_ASIO_NAMESPACE::ip::udp::endpoint{addr.to_v4(), 0});
    }
    else if (addr.is_v6())
    {
      const auto scopeId = addr.to_v6().scope_id();
      socket.mpImpl->mSocket.set_option(
        ::LINK_ASIO_NAMESPACE::ip::multicast::outbound_interface(
          static_cast<unsigned int>(scopeId)));
      socket.mpImpl->mSocket.bind(
        ::LINK_ASIO_NAMESPACE::ip::udp::endpoint{addr.to_v6(), 0});
    }
    else
    {
      throw(std::runtime_error("Unknown Protocol"));
    }
    return socket;
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openMulticastSocket(const discovery::IpAddress& addr)
  {
    auto socket =
      addr.is_v4() ? Socket<BufferSize>{*mpService, ::LINK_ASIO_NAMESPACE::ip::udp::v4()}
                   : Socket<BufferSize>{*mpService, ::LINK_ASIO_NAMESPACE::ip::udp::v6()};

    socket.mpImpl->mSocket.set_option(
      ::LINK_ASIO_NAMESPACE::ip::udp::socket::reuse_address(true));
    socket.mpImpl->mSocket.set_option(
      ::LINK_ASIO_NAMESPACE::socket_base::broadcast(!addr.is_loopback()));
    socket.mpImpl->mSocket.set_option(
      ::LINK_ASIO_NAMESPACE::ip::multicast::enable_loopback(addr.is_loopback()));

    if (addr.is_v4())
    {
      socket.mpImpl->mSocket.set_option(
        ::LINK_ASIO_NAMESPACE::ip::multicast::outbound_interface(addr.to_v4()));
      socket.mpImpl->mSocket.bind({::LINK_ASIO_NAMESPACE::ip::address_v4::any(),
        discovery::multicastEndpointV4().port()});
      socket.mpImpl->mSocket.set_option(::LINK_ASIO_NAMESPACE::ip::multicast::join_group(
        discovery::multicastEndpointV4().address().to_v4(), addr.to_v4()));
    }
    else if (addr.is_v6())
    {
      const auto scopeId = addr.to_v6().scope_id();
      socket.mpImpl->mSocket.set_option(
        ::LINK_ASIO_NAMESPACE::ip::multicast::outbound_interface(
          static_cast<unsigned int>(scopeId)));
      const auto multicastEndpoint = discovery::multicastEndpointV6(scopeId);
      socket.mpImpl->mSocket.bind(
        {::LINK_ASIO_NAMESPACE::ip::address_v6::any(), multicastEndpoint.port()});
      socket.mpImpl->mSocket.set_option(::LINK_ASIO_NAMESPACE::ip::multicast::join_group(
        multicastEndpoint.address().to_v6(), scopeId));
    }
    else
    {
      throw(std::runtime_error("Unknown Protocol"));
    }
    return socket;
  }

  std::vector<discovery::IpAddress> scanNetworkInterfaces()
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

  std::unique_ptr<::LINK_ASIO_NAMESPACE::io_service> mpService;
  std::unique_ptr<::LINK_ASIO_NAMESPACE::io_service::work> mpWork;
  std::thread mThread;
  Log mLog;
  ScanIpIfAddrs mScanIpIfAddrs;
};

} // namespace LINK_ASIO_NAMESPACE
} // namespace platforms
} // namespace ableton
