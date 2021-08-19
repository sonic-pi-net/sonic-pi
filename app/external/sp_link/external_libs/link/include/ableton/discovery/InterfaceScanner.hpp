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
#include <ableton/util/Injected.hpp>
#include <chrono>
#include <vector>

namespace ableton
{
namespace discovery
{

// Callback takes a range of asio::ip:address which is
// guaranteed to be sorted and unique
template <typename Callback, typename IoContext>
class InterfaceScanner
{
public:
  using Timer = typename util::Injected<IoContext>::type::Timer;

  InterfaceScanner(const std::chrono::seconds period,
    util::Injected<Callback> callback,
    util::Injected<IoContext> io)
    : mPeriod(period)
    , mCallback(std::move(callback))
    , mIo(std::move(io))
    , mTimer(mIo->makeTimer())
  {
  }

  void enable(const bool bEnable)
  {
    if (bEnable)
    {
      scan();
    }
    else
    {
      mTimer.cancel();
    }
  }

  void scan()
  {
    using namespace std;
    debug(mIo->log()) << "Scanning network interfaces";
    // Rescan the hardware for available network interface addresses
    vector<asio::ip::address> addrs = mIo->scanNetworkInterfaces();
    // Sort and unique them to guarantee consistent comparison
    sort(begin(addrs), end(addrs));
    addrs.erase(unique(begin(addrs), end(addrs)), end(addrs));
    // Pass them to the callback
    (*mCallback)(std::move(addrs));
    // setup the next scanning
    mTimer.expires_from_now(mPeriod);
    using ErrorCode = typename Timer::ErrorCode;
    mTimer.async_wait([this](const ErrorCode e) {
      if (!e)
      {
        scan();
      }
    });
  }

private:
  const std::chrono::seconds mPeriod;
  util::Injected<Callback> mCallback;
  util::Injected<IoContext> mIo;
  Timer mTimer;
};

// Factory function
template <typename Callback, typename IoContext>
InterfaceScanner<Callback, IoContext> makeInterfaceScanner(
  const std::chrono::seconds period,
  util::Injected<Callback> callback,
  util::Injected<IoContext> io)
{
  return {period, std::move(callback), std::move(io)};
}

} // namespace discovery
} // namespace ableton
