/* Copyright 2021, Ableton AG, Berlin. All rights reserved.
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

#include <pthread.h>
#include <thread>

namespace ableton
{
namespace platforms
{
namespace linux
{

struct ThreadFactory
{
  template <typename Callable, typename... Args>
  static std::thread makeThread(std::string name, Callable&& f, Args&&... args)
  {
    auto thread = std::thread(std::forward<Callable>(f), std::forward<Args>(args)...);
    pthread_setname_np(thread.native_handle(), name.c_str());
    return thread;
  }
};

} // namespace linux
} // namespace platforms
} // namespace ableton
