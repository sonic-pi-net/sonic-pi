//
// cpp14/require_static.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2020 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "asio/require.hpp"
#include <cassert>

template <int>
struct prop
{
  template <typename> static constexpr bool is_applicable_property_v = true;
  static constexpr bool is_requirable = true;
  template <typename> static constexpr bool static_query_v = true;
  static constexpr bool value() { return true; }
};

template <int>
struct object
{
};

int main()
{
  object<1> o1 = {};
  object<1> o2 = asio::require(o1, prop<1>());
  object<1> o3 = asio::require(o1, prop<1>(), prop<1>());
  object<1> o4 = asio::require(o1, prop<1>(), prop<1>(), prop<1>());
  (void)o2;
  (void)o3;
  (void)o4;

  const object<1> o5 = {};
  object<1> o6 = asio::require(o5, prop<1>());
  object<1> o7 = asio::require(o5, prop<1>(), prop<1>());
  object<1> o8 = asio::require(o5, prop<1>(), prop<1>(), prop<1>());
  (void)o6;
  (void)o7;
  (void)o8;

  constexpr object<1> o9 = asio::require(object<1>(), prop<1>());
  constexpr object<1> o10 = asio::require(object<1>(), prop<1>(), prop<1>());
  constexpr object<1> o11 = asio::require(object<1>(), prop<1>(), prop<1>(), prop<1>());
  (void)o9;
  (void)o10;
  (void)o11;
}
