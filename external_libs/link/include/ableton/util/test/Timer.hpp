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

#include <chrono>
#include <functional>

namespace ableton
{
namespace util
{
namespace test
{

struct Timer
{
  using ErrorCode = int;
  using TimePoint = std::chrono::system_clock::time_point;

  // Initialize timer with an arbitrary large value to simulate the
  // time_since_epoch of a real clock.
  Timer()
    : mNow{std::chrono::milliseconds{123456789}}
  {
  }

  void expires_at(std::chrono::system_clock::time_point t)
  {
    cancel();
    mFireAt = std::move(t);
  }

  template <typename T, typename Rep>
  void expires_from_now(std::chrono::duration<T, Rep> duration)
  {
    cancel();
    mFireAt = now() + duration;
  }

  ErrorCode cancel()
  {
    if (mHandler)
    {
      mHandler(1); // call existing handler with truthy error code
    }
    mHandler = nullptr;
    return 0;
  }

  template <typename Handler>
  void async_wait(Handler handler)
  {
    mHandler = [handler](ErrorCode ec) { handler(ec); };
  }

  std::chrono::system_clock::time_point now() const
  {
    return mNow;
  }

  template <typename T, typename Rep>
  void advance(std::chrono::duration<T, Rep> duration)
  {
    mNow += duration;
    if (mHandler && mFireAt < mNow)
    {
      mHandler(0);
      mHandler = nullptr;
    }
  }

  std::function<void(ErrorCode)> mHandler;
  std::chrono::system_clock::time_point mFireAt;
  std::chrono::system_clock::time_point mNow;
};

} // namespace test
} // namespace util
} // namespace ableton
