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
#include <list>
#include <map>
#include <memory>

namespace ableton
{
namespace test
{
namespace serial_io
{

class SchedulerTree
{
public:
  using TimePoint = std::chrono::system_clock::time_point;
  using TimerId = std::size_t;
  using TimerErrorCode = int;

  void run();

  std::shared_ptr<SchedulerTree> makeChild();

  template <typename Handler>
  void async(Handler handler)
  {
    mPendingHandlers.push_back(std::move(handler));
  }

  template <typename Handler>
  void setTimer(const TimerId timerId, const TimePoint expiration, Handler handler)
  {
    using namespace std;
    mTimers[make_pair(std::move(expiration), timerId)] = std::move(handler);
  }

  void cancelTimer(const TimerId timerId);

  // returns the time that the next timer in the subtree expires
  TimePoint nextTimerExpiration();

  // triggers all timers in the subtree that expire at time t or before
  void triggerTimersUntil(const TimePoint t);

private:
  // returns true if some work was done, false if there was none to do
  bool handlePending();

  // returns the time that the next timer from this node expires
  TimePoint nextOwnTimerExpiration();

  // Traversal function over children that cleans up children that
  // have been destroyed.
  template <typename Fn>
  void withChildren(Fn fn)
  {
    auto it = begin(mChildren);
    while (it != end(mChildren))
    {
      const auto childIt = it++;
      auto pChild = childIt->lock();
      if (pChild)
      {
        fn(*pChild);
      }
      else
      {
        mChildren.erase(childIt);
      }
    }
  }

  using TimerHandler = std::function<void(TimerErrorCode)>;
  using TimerMap = std::map<std::pair<TimePoint, TimerId>, TimerHandler>;
  TimerMap mTimers;
  std::list<std::function<void()>> mPendingHandlers;
  std::list<std::weak_ptr<SchedulerTree>> mChildren;
};

} // namespace serial_io
} // namespace test
} // namespace ableton
