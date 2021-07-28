/* Copyright 2017, Ableton AG, Berlin. All rights reserved.
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

#include <cassert>
#include <utility>

namespace ableton
{
namespace link
{

// Subset of the C++ 17 std::optional API. T has to be default constructible.
template <typename T>
struct Optional
{
  Optional()
    : mHasValue(false)
  {
  }

  explicit Optional(T value)
    : mValue(std::move(value))
    , mHasValue(true)
  {
  }

  Optional(const Optional&) = default;

  Optional(Optional&& other)
    : mValue(std::move(other.mValue))
    , mHasValue(other.mHasValue)
  {
  }

  Optional& operator=(const Optional&) = default;

  Optional& operator=(Optional&& other)
  {
    mValue = std::move(other.mValue);
    mHasValue = other.mHasValue;
    return *this;
  }

  explicit operator bool() const
  {
    return mHasValue;
  }

  const T& operator*() const
  {
    assert(mHasValue);
    return mValue;
  }

  T& operator*()
  {
    assert(mHasValue);
    return mValue;
  }

  const T* operator->() const
  {
    assert(mHasValue);
    return &mValue;
  }

  T* operator->()
  {
    assert(mHasValue);
    return &mValue;
  }

private:
  T mValue;
  bool mHasValue;
};

} // namespace link
} // namespace ableton
