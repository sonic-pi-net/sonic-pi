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
#include <ableton/test/serial_io/SchedulerTree.hpp>
#include <ableton/test/serial_io/Timer.hpp>
#include <ableton/util/Log.hpp>
#include <chrono>
#include <memory>

namespace ableton
{
namespace test
{
namespace serial_io
{

class Context
{
public:
  Context(const SchedulerTree::TimePoint& now,
    const std::vector<::asio::ip::address>& ifAddrs,
    std::shared_ptr<SchedulerTree> pScheduler)
    : mNow(now)
    , mIfAddrs(ifAddrs)
    , mpScheduler(std::move(pScheduler))
    , mNextTimerId(0)
  {
  }

  ~Context()
  {
    if (mpScheduler != nullptr)
    {
      // Finish any pending tasks before shutting down
      mpScheduler->run();
    }
  }

  Context(const Context&) = delete;
  Context& operator=(const Context&) = delete;

  Context(Context&& rhs)
    : mNow(rhs.mNow)
    , mIfAddrs(rhs.mIfAddrs)
    , mpScheduler(std::move(rhs.mpScheduler))
    , mLog(std::move(rhs.mLog))
    , mNextTimerId(rhs.mNextTimerId)
  {
  }

  void stop()
  {
  }

  template <typename Handler>
  void async(Handler handler)
  {
    mpScheduler->async(std::move(handler));
  }

  Context clone()
  {
    return {mNow, mIfAddrs, mpScheduler->makeChild()};
  }

  using Timer = serial_io::Timer;

  Timer makeTimer()
  {
    return {mNextTimerId++, mNow, mpScheduler};
  }

  using Log = util::NullLog;

  Log& log()
  {
    return mLog;
  }

  std::vector<::asio::ip::address> scanNetworkInterfaces()
  {
    return mIfAddrs;
  }

private:
  const SchedulerTree::TimePoint& mNow;
  const std::vector<::asio::ip::address>& mIfAddrs;
  std::shared_ptr<SchedulerTree> mpScheduler;
  Log mLog;
  SchedulerTree::TimerId mNextTimerId;
};

} // namespace serial_io
} // namespace test
} // namespace ableton
