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

// Utility type for aiding in dependency injection.

// Base template and implementation for injected valued
template <typename T>
struct Injected
{
  using type = T;

  Injected() = default;

  explicit Injected(T t)
    : val(std::move(t))
  {
  }

  Injected(const Injected&) = default;
  Injected& operator=(const Injected&) = default;

  Injected(Injected&& rhs)
    : val(std::move(rhs.val))
  {
  }

  Injected& operator=(Injected&& rhs)
  {
    val = std::move(rhs.val);
    return *this;
  }

  T* operator->()
  {
    return &val;
  }

  const T* operator->() const
  {
    return &val;
  }

  T& operator*()
  {
    return val;
  }

  const T& operator*() const
  {
    return val;
  }

  T val;
};

// Utility function for injecting values
template <typename T>
Injected<T> injectVal(T t)
{
  return Injected<T>(std::move(t));
}

// Specialization for injected references
template <typename T>
struct Injected<T&>
{
  using type = T;

  explicit Injected(T& t)
    : ref(std::ref(t))
  {
  }

  Injected(const Injected&) = default;
  Injected& operator=(const Injected&) = default;

  Injected(Injected&& rhs)
    : ref(std::move(rhs.ref))
  {
  }

  Injected& operator=(Injected&& rhs)
  {
    ref = std::move(rhs.ref);
    return *this;
  }

  T* operator->()
  {
    return &ref.get();
  }

  const T* operator->() const
  {
    return &ref.get();
  }

  T& operator*()
  {
    return ref;
  }

  const T& operator*() const
  {
    return ref;
  }

  std::reference_wrapper<T> ref;
};

// Utility function for injecting references
template <typename T>
Injected<T&> injectRef(T& t)
{
  return Injected<T&>(t);
}

// Specialization for injected shared_ptr
template <typename T>
struct Injected<std::shared_ptr<T>>
{
  using type = T;

  explicit Injected(std::shared_ptr<T> pT)
    : shared(std::move(pT))
  {
  }

  Injected(const Injected&) = default;
  Injected& operator=(const Injected&) = default;

  Injected(Injected&& rhs)
    : shared(std::move(rhs.shared))
  {
  }

  Injected& operator=(Injected&& rhs)
  {
    shared = std::move(rhs.shared);
    return *this;
  }

  T* operator->()
  {
    return shared.get();
  }

  const T* operator->() const
  {
    return shared.get();
  }

  T& operator*()
  {
    return *shared;
  }

  const T& operator*() const
  {
    return *shared;
  }

  std::shared_ptr<T> shared;
};

// Utility function for injected shared_ptr
template <typename T>
Injected<std::shared_ptr<T>> injectShared(std::shared_ptr<T> shared)
{
  return Injected<std::shared_ptr<T>>(std::move(shared));
}

// Specialization for injected unique_ptr
template <typename T>
struct Injected<std::unique_ptr<T>>
{
  using type = T;

  explicit Injected(std::unique_ptr<T> pT)
    : unique(std::move(pT))
  {
  }

  Injected(const Injected&) = default;
  Injected& operator=(const Injected&) = default;

  Injected(Injected&& rhs)
    : unique(std::move(rhs.unique))
  {
  }

  Injected& operator=(Injected&& rhs)
  {
    unique = std::move(rhs.unique);
    return *this;
  }

  T* operator->()
  {
    return unique.get();
  }

  const T* operator->() const
  {
    return unique.get();
  }

  T& operator*()
  {
    return *unique;
  }

  const T& operator*() const
  {
    return *unique;
  }

  std::unique_ptr<T> unique;
};

// Utility function for injected unique_ptr
template <typename T>
Injected<std::unique_ptr<T>> injectUnique(std::unique_ptr<T> unique)
{
  return Injected<std::unique_ptr<T>>(std::move(unique));
}

} // namespace util
} // namespace ableton
