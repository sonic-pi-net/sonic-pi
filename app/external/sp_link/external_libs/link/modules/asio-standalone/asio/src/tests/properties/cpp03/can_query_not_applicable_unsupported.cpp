//
// cpp03/can_query_not_applicable_unsupported.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "asio/query.hpp"
#include <cassert>

struct prop
{
};

struct object
{
};

int main()
{
  assert((!asio::can_query<object, prop>::value));
  assert((!asio::can_query<const object, prop>::value));
}
