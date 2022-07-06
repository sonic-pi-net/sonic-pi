/* Copyright 2020, Ableton AG, Berlin. All rights reserved.
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
#include <ableton/platforms/asio/Socket.hpp>
#include <ableton/platforms/esp32/LockFreeCallbackDispatcher.hpp>
#include <driver/timer.h>
#include <freertos/FreeRTOS.h>
#include <freertos/task.h>

namespace ableton
{
namespace platforms
{
namespace esp32
{

template <typename ScanIpIfAddrs, typename LogT>
class Context
{
  class ServiceRunner
  {

    static void run(void* userParams)
    {
      auto runner = static_cast<ServiceRunner*>(userParams);
      for (;;)
      {
        try
        {
          ulTaskNotifyTake(pdTRUE, portMAX_DELAY);
          runner->mpService->poll_one();
        }
        catch (...)
        {
        }
      }
    }

    static void IRAM_ATTR timerIsr(void* userParam)
    {
      static BaseType_t xHigherPriorityTaskWoken = pdFALSE;

      timer_group_clr_intr_status_in_isr(TIMER_GROUP_0, TIMER_1);
      timer_group_enable_alarm_in_isr(TIMER_GROUP_0, TIMER_1);

      vTaskNotifyGiveFromISR(userParam, &xHigherPriorityTaskWoken);
      if (xHigherPriorityTaskWoken)
      {
        portYIELD_FROM_ISR();
      }
    }

  public:
    ServiceRunner()
      : mpService(new ::asio::io_service())
      , mpWork(new ::asio::io_service::work(*mpService))
    {
      xTaskCreatePinnedToCore(run, "link", 8192, this, 2 | portPRIVILEGE_BIT,
        &mTaskHandle, LINK_ESP_TASK_CORE_ID);

      timer_config_t config = {.alarm_en = TIMER_ALARM_EN,
        .counter_en = TIMER_PAUSE,
        .intr_type = TIMER_INTR_LEVEL,
        .counter_dir = TIMER_COUNT_UP,
        .auto_reload = TIMER_AUTORELOAD_EN,
        .divider = 80};

      timer_init(TIMER_GROUP_0, TIMER_1, &config);
      timer_set_counter_value(TIMER_GROUP_0, TIMER_1, 0);
      timer_set_alarm_value(TIMER_GROUP_0, TIMER_1, 100);
      timer_enable_intr(TIMER_GROUP_0, TIMER_1);
      timer_isr_register(TIMER_GROUP_0, TIMER_1, &timerIsr, mTaskHandle, 0, nullptr);

      timer_start(TIMER_GROUP_0, TIMER_1);
    }

    ~ServiceRunner()
    {
      vTaskDelete(mTaskHandle);
    }

    template <typename Handler>
    void async(Handler handler)
    {
      mpService->post(std::move(handler));
    }

    ::asio::io_service& service() const
    {
      return *mpService;
    }

  private:
    TaskHandle_t mTaskHandle;
    std::unique_ptr<::asio::io_service> mpService;
    std::unique_ptr<::asio::io_service::work> mpWork;
  };

public:
  using Timer = ::ableton::platforms::asio::AsioTimer;
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
  {
  }

  Context(const Context&) = delete;

  Context(Context&& rhs)
    : mLog(std::move(rhs.mLog))
    , mScanIpIfAddrs(std::move(rhs.mScanIpIfAddrs))
  {
  }

  void stop()
  {
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openUnicastSocket(const ::asio::ip::address_v4& addr)
  {
    auto socket = Socket<BufferSize>{serviceRunner().service()};
    socket.mpImpl->mSocket.set_option(
      ::asio::ip::multicast::enable_loopback(addr.is_loopback()));
    socket.mpImpl->mSocket.set_option(::asio::ip::multicast::outbound_interface(addr));
    socket.mpImpl->mSocket.bind(::asio::ip::udp::endpoint{addr, 0});
    return socket;
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openMulticastSocket(const ::asio::ip::address_v4& addr)
  {
    auto socket = Socket<BufferSize>{serviceRunner().service()};
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
    return {serviceRunner().service()};
  }

  Log& log()
  {
    return mLog;
  }

  template <typename Handler>
  void async(Handler handler)
  {
    serviceRunner().service().post(std::move(handler));
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

  static ServiceRunner& serviceRunner()
  {
    static ServiceRunner runner;
    return runner;
  }

  Log mLog;
  ScanIpIfAddrs mScanIpIfAddrs;
};

} // namespace esp32
} // namespace platforms
} // namespace ableton
