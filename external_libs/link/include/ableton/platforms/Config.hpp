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

#include <ableton/link/Controller.hpp>
#include <ableton/util/Log.hpp>

#if defined(LINK_PLATFORM_WINDOWS)
#include <ableton/platforms/asio/Context.hpp>
#include <ableton/platforms/stl/Random.hpp>
#include <ableton/platforms/windows/Clock.hpp>
#include <ableton/platforms/windows/ScanIpIfAddrs.hpp>
#elif defined(LINK_PLATFORM_MACOSX)
#include <ableton/platforms/asio/Context.hpp>
#include <ableton/platforms/darwin/Clock.hpp>
#include <ableton/platforms/posix/ScanIpIfAddrs.hpp>
#include <ableton/platforms/stl/Random.hpp>
#elif defined(LINK_PLATFORM_LINUX)
#include <ableton/platforms/asio/Context.hpp>
#include <ableton/platforms/linux/Clock.hpp>
#include <ableton/platforms/posix/ScanIpIfAddrs.hpp>
#include <ableton/platforms/stl/Random.hpp>
#elif defined(ESP_PLATFORM)
#include <ableton/platforms/esp32/Clock.hpp>
#include <ableton/platforms/esp32/Context.hpp>
#include <ableton/platforms/esp32/Random.hpp>
#include <ableton/platforms/esp32/ScanIpIfAddrs.hpp>
#endif

namespace ableton
{
namespace link
{
namespace platform
{

#if defined(LINK_PLATFORM_WINDOWS)
using Clock = platforms::windows::Clock;
using IoContext =
  platforms::asio::Context<platforms::windows::ScanIpIfAddrs, util::NullLog>;
using Random = platforms::stl::Random;

#elif defined(LINK_PLATFORM_MACOSX)
using Clock = platforms::darwin::Clock;
using IoContext =
  platforms::asio::Context<platforms::posix::ScanIpIfAddrs, util::NullLog>;
using Random = platforms::stl::Random;

#elif defined(LINK_PLATFORM_LINUX)
using Clock = platforms::linux::ClockMonotonicRaw;
using IoContext =
  platforms::asio::Context<platforms::posix::ScanIpIfAddrs, util::NullLog>;
using Random = platforms::stl::Random;

#elif defined(ESP_PLATFORM)
using Clock = platforms::esp32::Clock;
using IoContext =
  platforms::esp32::Context<platforms::esp32::ScanIpIfAddrs, util::NullLog>;
using Random = platforms::esp32::Random;
#endif

} // namespace platform
} // namespace link
} // namespace ableton
