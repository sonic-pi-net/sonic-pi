//
// timer.cpp
// ~~~~~~~~~
//
// Copyright (c) 2003-2020 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>
#include <asio.hpp>

void print(const asio::error_code& /*e*/)
{
  std::cout << "Hello, world!" << std::endl;
}

int main()
{
  asio::io_context io;

  asio::steady_timer t(io, asio::chrono::seconds(5));
  t.async_wait(&print);

  io.run();

  return 0;
}
