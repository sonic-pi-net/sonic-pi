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

#include <ableton/test/serial_io/SchedulerTree.hpp>

namespace ableton
{
namespace test
{
namespace serial_io
{

struct Timer
{
  using ErrorCode = SchedulerTree::TimerErrorCode;
  using TimePoint = SchedulerTree::TimePoint;

  Timer(const SchedulerTree::TimerId timerId,
    const TimePoint& now,
    std::shared_ptr<SchedulerTree> pScheduler)
    : mId(timerId)
    , mNow(now)
    , mpScheduler(std::move(pScheduler))
  {
  }

  ~Timer()
  {
    if (!mbMovedFrom)
    {
      cancel();
    }
  }

  Timer(const Timer&) = delete;

  Timer(Timer&& rhs)
    : mId(rhs.mId)
    , mNow(rhs.mNow)
    , mExpiration(std::move(rhs.mExpiration))
    , mpScheduler(std::move(rhs.mpScheduler))
  {
    rhs.mbMovedFrom = true;
  }

  void expires_at(const TimePoint t)
  {
    if (t < mNow)
    {
      throw std::runtime_error("Setting timer in the past");
    }
    else
    {
      cancel();
      mExpiration = t;
    }
  }

  template <typename T, typename Rep>
  void expires_from_now(std::chrono::duration<T, Rep> duration)
  {
    expires_at(mNow + duration);
  }

  void cancel()
  {
    auto pScheduler = mpScheduler.lock();
    pScheduler->cancelTimer(mId);
  }

  template <typename Handler>
  void async_wait(Handler handler)
  {
    auto pScheduler = mpScheduler.lock();
    pScheduler->setTimer(mId, mExpiration, std::move(handler));
  }

  TimePoint now() const
  {
    return mNow;
  }

  const SchedulerTree::TimerId mId;
  const TimePoint& mNow;
  TimePoint mExpiration;
  std::weak_ptr<SchedulerTree> mpScheduler;
  bool mbMovedFrom = false;
};

} // namespace serial_io
} // namespace test
} // namespace ableton
