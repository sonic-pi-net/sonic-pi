/* Copyright 2022, Ableton AG, Berlin. All rights reserved.
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

#include <ableton/link/Optional.hpp>

#include <array>
#include <atomic>
#include <cassert>
#include <cstdint>

namespace ableton
{
namespace link
{

template <typename T>
struct TripleBuffer
{
public:
  TripleBuffer()
    : mBuffers{{{}, {}, {}}}
  {
    assert(mState.is_lock_free());
  }

  explicit TripleBuffer(const T& initial)
    : mBuffers{{initial, initial, initial}}
  {
    assert(mState.is_lock_free());
  }

  TripleBuffer(const TripleBuffer&) = delete;
  TripleBuffer& operator=(const TripleBuffer&) = delete;

  T read() noexcept
  {
    loadReadBuffer();
    return mBuffers[mReadIndex];
  }

  Optional<T> readNew()
  {
    if (loadReadBuffer())
    {
      return Optional<T>(mBuffers[mReadIndex]);
    }
    return {};
  }

  template <typename U>
  void write(U&& value)
  {
    mBuffers[mWriteIndex] = std::forward<U>(value);

    const auto prevState =
      mState.exchange(makeState(mWriteIndex, true), std::memory_order_acq_rel);

    mWriteIndex = backIndex(prevState);
  }

private:
  bool loadReadBuffer()
  {
    auto state = mState.load(std::memory_order_acquire);
    auto isNew = isNewWrite(state);
    if (isNew)
    {
      const auto prevState =
        mState.exchange(makeState(mReadIndex, false), std::memory_order_acq_rel);

      mReadIndex = backIndex(prevState);
    }
    return isNew;
  }

  using BackingState = uint32_t;

  static constexpr bool isNewWrite(const BackingState state)
  {
    return (state & 0x0000FFFFu) != 0;
  }

  static constexpr uint32_t backIndex(const BackingState state)
  {
    return state >> 16;
  }

  static constexpr BackingState makeState(
    const uint32_t backBufferIndex, const bool isWrite)
  {
    return (backBufferIndex << 16) | uint32_t(isWrite);
  }

  std::atomic<BackingState> mState{makeState(1u, false)}; // Reader and writer
  uint32_t mReadIndex = 0u;                               // Reader only
  uint32_t mWriteIndex = 2u;                              // Writer only

  std::array<T, 3> mBuffers{};
};

} // namespace link
} // namespace ableton
