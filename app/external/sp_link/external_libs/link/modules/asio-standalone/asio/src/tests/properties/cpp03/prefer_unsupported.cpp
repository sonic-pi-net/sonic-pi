//
// cpp03/prefer_unsupported.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "asio/prefer.hpp"
#include <cassert>

template <int>
struct prop
{
  static const bool is_preferable = true;
};

template <int>
struct object
{
};

namespace asio {

template<int N, int M>
struct is_applicable_property<object<N>, prop<M> >
{
  static const bool value = true;
};

} // namespace asio

int main()
{
  object<1> o1 = {};
  const object<1>& o2 = asio::prefer(o1, prop<1>());
  assert(&o1 == &o2);
  (void)o2;

  const object<1> o3 = {};
  const object<1>& o4 = asio::prefer(o3, prop<1>());
  assert(&o3 == &o4);
  (void)o4;
}
