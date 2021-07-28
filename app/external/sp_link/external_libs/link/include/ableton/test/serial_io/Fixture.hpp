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
#include <ableton/test/serial_io/Context.hpp>
#include <chrono>
#include <memory>

namespace ableton
{
namespace test
{
namespace serial_io
{

class Fixture
{
public:
  Fixture()
    : mpScheduler(std::make_shared<SchedulerTree>())
    , mNow(std::chrono::milliseconds{123456789})
  {
  }

  ~Fixture()
  {
    flush();
  }

  Fixture(const Fixture&) = delete;
  Fixture& operator=(const Fixture&) = delete;
  Fixture(Fixture&&) = delete;
  Fixture& operator=(Fixture&&) = delete;

  void setNetworkInterfaces(std::vector<::asio::ip::address> ifAddrs)
  {
    mIfAddrs = std::move(ifAddrs);
  }

  Context makeIoContext()
  {
    return {mNow, mIfAddrs, mpScheduler};
  }

  void flush()
  {
    mpScheduler->run();
  }

  template <typename T, typename Rep>
  void advanceTime(std::chrono::duration<T, Rep> duration)
  {
    const auto target = mNow + duration;
    mpScheduler->run();
    auto nextTimer = mpScheduler->nextTimerExpiration();
    while (nextTimer <= target)
    {
      mNow = nextTimer;
      mpScheduler->triggerTimersUntil(mNow);
      mpScheduler->run();
      nextTimer = mpScheduler->nextTimerExpiration();
    }
    mNow = target;
  }

private:
  std::shared_ptr<SchedulerTree> mpScheduler;
  SchedulerTree::TimePoint mNow;
  std::vector<::asio::ip::address> mIfAddrs;
};

} // namespace serial_io
} // namespace test
} // namespace ableton
