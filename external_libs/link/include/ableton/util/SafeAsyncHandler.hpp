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

#include <memory>

namespace ableton
{
namespace util
{

// A utility handler for passing to async functions that may call the
// handler past the lifetime of the wrapped delegate object.
// The need for this is particularly driven by boost::asio timer
// objects, which explicitly document that they may be called without
// an error code after they have been cancelled. This has led to
// several crashes. This handler wrapper implements a useful idiom for
// avoiding this problem.

template <typename Delegate>
struct SafeAsyncHandler
{
  SafeAsyncHandler(const std::shared_ptr<Delegate>& pDelegate)
    : mpDelegate(pDelegate)
  {
  }

  template <typename... T>
  void operator()(T&&... t) const
  {
    std::shared_ptr<Delegate> pDelegate = mpDelegate.lock();
    if (pDelegate)
    {
      (*pDelegate)(std::forward<T>(t)...);
    }
  }

  std::weak_ptr<Delegate> mpDelegate;
};

// Factory function for easily wrapping a shared_ptr to a handler
template <typename Delegate>
SafeAsyncHandler<Delegate> makeAsyncSafe(const std::shared_ptr<Delegate>& pDelegate)
{
  return {pDelegate};
}

} // namespace util
} // namespace ableton
