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
#include <ableton/util/SafeAsyncHandler.hpp>
#include <functional>

namespace ableton
{
namespace platforms
{
namespace asio
{

// This implementation is based on the boost::asio::system_timer concept.
// Since boost::system_timer doesn't support move semantics, we create a wrapper
// with a unique_ptr to get a movable type. It also handles an inconvenient
// aspect of asio timers, which is that you must explicitly guard against the
// handler firing after cancellation. We handle this by use of the SafeAsyncHandler
// utility. AsioTimer therefore guarantees that a handler will not be called after
// the destruction of the timer, or after the timer has been canceled.

class AsioTimer
{
public:
  using ErrorCode = ::asio::error_code;
  using TimePoint = std::chrono::system_clock::time_point;

  AsioTimer(::asio::io_service& io)
    : mpTimer(new ::asio::system_timer(io))
    , mpAsyncHandler(std::make_shared<AsyncHandler>())
  {
  }

  ~AsioTimer()
  {
    // The timer may not be valid anymore if this instance was moved from
    if (mpTimer != nullptr)
    {
      // Ignore errors during cancellation
      cancel();
    }
  }

  AsioTimer(const AsioTimer&) = delete;
  AsioTimer& operator=(const AsioTimer&) = delete;

  // Enable move construction but not move assignment. Move assignment
  // would get weird - would have to handle outstanding handlers
  AsioTimer(AsioTimer&& rhs)
    : mpTimer(std::move(rhs.mpTimer))
    , mpAsyncHandler(std::move(rhs.mpAsyncHandler))
  {
  }

  void expires_at(std::chrono::system_clock::time_point tp)
  {
    mpTimer->expires_at(std::move(tp));
  }

  template <typename T>
  void expires_from_now(T duration)
  {
    mpTimer->expires_from_now(std::move(duration));
  }

  ErrorCode cancel()
  {
    ErrorCode ec;
    mpTimer->cancel(ec);
    mpAsyncHandler->mpHandler = nullptr;
    return ec;
  }

  template <typename Handler>
  void async_wait(Handler handler)
  {
    *mpAsyncHandler = std::move(handler);
    mpTimer->async_wait(util::makeAsyncSafe(mpAsyncHandler));
  }

  TimePoint now() const
  {
    return std::chrono::system_clock::now();
  }

private:
  struct AsyncHandler
  {
    template <typename Handler>
    AsyncHandler& operator=(Handler handler)
    {
      mpHandler = [handler](ErrorCode ec) { handler(std::move(ec)); };
      return *this;
    }

    void operator()(ErrorCode ec)
    {
      if (mpHandler)
      {
        mpHandler(std::move(ec));
      }
    }

    std::function<void(const ErrorCode)> mpHandler;
  };

  std::unique_ptr<::asio::system_timer> mpTimer;
  std::shared_ptr<AsyncHandler> mpAsyncHandler;
};

} // namespace asio
} // namespace platforms
} // namespace ableton
