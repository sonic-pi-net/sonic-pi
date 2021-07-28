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

#include <array>
#include <atomic>
#include <cassert>
#include <cstddef>

#include <ableton/link/Optional.hpp>

namespace ableton
{
namespace link
{

// Single producer, single consumer lockfree Fifo

template <typename Type, std::size_t size>
class CircularFifo
{
public:
  CircularFifo()
    : tail(0)
    , head(0)
  {
    assert(head.is_lock_free() && tail.is_lock_free());
  }

  bool push(Type item)
  {
    const auto currentTail = tail.load();
    const auto nextTail = nextIndex(currentTail);
    if (nextTail != head.load())
    {
      data[currentTail] = std::move(item);
      tail.store(nextTail);
      return true;
    }
    return false;
  }

  Optional<Type> pop()
  {
    const auto currentHead = head.load();
    if (currentHead == tail.load())
    {
      return {};
    }

    auto item = data[currentHead];
    head.store(nextIndex(currentHead));
    return Optional<Type>{std::move(item)};
  }

  bool isEmpty() const
  {
    return tail == head;
  }

private:
  size_t nextIndex(const size_t index) const
  {
    return (index + 1) % (size + 1);
  }

  size_t previousIndex(const size_t index) const
  {
    return (index + size) % (size + 1);
  }

  std::atomic_size_t tail;
  std::atomic_size_t head;
  std::array<Type, size + 1> data;
};

} // namespace link
} // namespace ableton
