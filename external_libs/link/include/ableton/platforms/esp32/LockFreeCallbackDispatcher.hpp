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

#include <atomic>
#include <condition_variable>
#include <freertos/task.h>

namespace ableton
{
namespace platforms
{
namespace esp32
{

// Utility to signal invocation of a callback on another thread in a lock free manner.
// The callback is evoked on a thread owned by the instance of this class.
//
// A condition variable is used to notify a waiting thread, but only if the required
// lock can be acquired immediately. If that fails, we fall back on signaling
// after a timeout. This gives us a guaranteed minimum signaling rate which is defined
// by the fallbackPeriod parameter.

template <typename Callback, typename Duration>
class LockFreeCallbackDispatcher
{
public:
  LockFreeCallbackDispatcher(Callback callback, Duration fallbackPeriod)
    : mCallback(std::move(callback))
    , mFallbackPeriod(std::move(fallbackPeriod))
    , mRunning(true)
  {
    xTaskCreate(run, "link", 4096, this, tskIDLE_PRIORITY, &mTaskHandle);
  }

  ~LockFreeCallbackDispatcher()
  {
    mRunning = false;
    mCondition.notify_one();
    vTaskDelete(mTaskHandle);
  }

  void invoke()
  {
    if (mMutex.try_lock())
    {
      mCondition.notify_one();
      mMutex.unlock();
    }
  }

private:
  static void run(void* userData)
  {
    auto dispatcher = static_cast<LockFreeCallbackDispatcher*>(userData);
    while (dispatcher->mRunning.load())
    {
      {
        std::unique_lock<std::mutex> lock(dispatcher->mMutex);
        dispatcher->mCondition.wait_for(lock, dispatcher->mFallbackPeriod);
      }
      dispatcher->mCallback();
      portYIELD();
    }
  }

  Callback mCallback;
  Duration mFallbackPeriod;
  std::atomic<bool> mRunning;
  std::mutex mMutex;
  std::condition_variable mCondition;
  TaskHandle_t mTaskHandle;
};

} // namespace esp32
} // namespace platforms
} // namespace ableton
