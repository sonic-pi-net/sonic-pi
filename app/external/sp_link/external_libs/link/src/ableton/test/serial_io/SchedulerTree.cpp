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

#include <ableton/test/serial_io/SchedulerTree.hpp>
#include <algorithm>

namespace ableton
{
namespace test
{
namespace serial_io
{

void SchedulerTree::run()
{
  while (handlePending())
  {
  }
}

std::shared_ptr<SchedulerTree> SchedulerTree::makeChild()
{
  auto newChild = std::make_shared<SchedulerTree>();
  mChildren.push_back(newChild);
  return newChild;
}

void SchedulerTree::cancelTimer(const TimerId timerId)
{
  const auto it = std::find_if(
    begin(mTimers), end(mTimers), [timerId](const TimerMap::value_type& timer) {
      return timer.first.second == timerId;
    });
  if (it != end(mTimers))
  {
    auto handler = std::move(it->second);
    mPendingHandlers.push_back([handler]() {
      handler(1); // truthy indicates error
    });
    mTimers.erase(it);
  }
}

SchedulerTree::TimePoint SchedulerTree::nextTimerExpiration()
{
  auto nextTimePoint = TimePoint::max();
  withChildren([&nextTimePoint](SchedulerTree& child) {
    nextTimePoint = (std::min)(nextTimePoint, child.nextOwnTimerExpiration());
  });
  return (std::min)(nextTimePoint, nextOwnTimerExpiration());
}

void SchedulerTree::triggerTimersUntil(const TimePoint t)
{
  using namespace std;
  withChildren([t](SchedulerTree& child) { child.triggerTimersUntil(t); });

  const auto it = mTimers.upper_bound(make_pair(t, numeric_limits<TimerId>::max()));
  for_each(begin(mTimers), it, [this](const TimerMap::value_type& timer) {
    mPendingHandlers.push_back([timer]() {
      timer.second(0); // 0 indicates no error
    });
  });
  mTimers.erase(begin(mTimers), it);
}

bool SchedulerTree::handlePending()
{
  bool subtreeWorked = false;
  withChildren(
    [&subtreeWorked](SchedulerTree& child) { subtreeWorked |= child.handlePending(); });

  if (mPendingHandlers.empty())
  {
    return subtreeWorked;
  }
  else
  {
    mPendingHandlers.front()();
    mPendingHandlers.pop_front();
    return true;
  }
}

SchedulerTree::TimePoint SchedulerTree::nextOwnTimerExpiration()
{
  return mTimers.empty() ? TimePoint::max() : begin(mTimers)->first.first;
}

} // namespace serial_io
} // namespace test
} // namespace ableton
