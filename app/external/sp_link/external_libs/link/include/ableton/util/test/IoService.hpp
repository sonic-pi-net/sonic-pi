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

#include <ableton/util/test/Timer.hpp>

namespace ableton
{
namespace util
{
namespace test
{

struct IoService
{
  // Wrapper around the internal util::test::Timer in the list
  struct Timer
  {
    using ErrorCode = test::Timer::ErrorCode;
    using TimePoint = test::Timer::TimePoint;

    Timer(util::test::Timer* pTimer)
      : mpTimer(pTimer)
    {
    }

    void expires_at(std::chrono::system_clock::time_point t)
    {
      mpTimer->expires_at(t);
    }

    template <typename T, typename Rep>
    void expires_from_now(std::chrono::duration<T, Rep> duration)
    {
      mpTimer->expires_from_now(duration);
    }

    ErrorCode cancel()
    {
      return mpTimer->cancel();
    }

    template <typename Handler>
    void async_wait(Handler handler)
    {
      mpTimer->async_wait(std::move(handler));
    }

    TimePoint now() const
    {
      return mpTimer->now();
    }

    util::test::Timer* mpTimer;
  };

  IoService() = default;

  Timer makeTimer()
  {
    mTimers.emplace_back();
    return Timer{&mTimers.back()};
  }

  template <typename Handler>
  void post(Handler handler)
  {
    mHandlers.emplace_back(std::move(handler));
  }

  template <typename T, typename Rep>
  void advance(std::chrono::duration<T, Rep> duration)
  {
    runHandlers();

    for (auto& timer : mTimers)
    {
      timer.advance(duration);
    }
  }

  void runHandlers()
  {
    for (auto& handler : mHandlers)
    {
      handler();
    }
    mHandlers.clear();
  }

  std::vector<std::function<void()>> mHandlers;
  std::vector<util::test::Timer> mTimers;
};

} // namespace test
} // namespace util
} // namespace ableton
