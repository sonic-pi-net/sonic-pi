//
// main.cpp
// ~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "line_reader.hpp"
#include "sleep.hpp"
#include "stdin_line_reader.hpp"
#include <asio/co_spawn.hpp>
#include <asio/detached.hpp>
#include <asio/io_context.hpp>
#include <asio/use_awaitable.hpp>
#include <iostream>

asio::awaitable<void> do_read(line_reader& reader)
{
  for (int i = 0; i < 10; ++i)
  {
    std::cout << co_await reader.async_read_line("Enter something: ", asio::use_awaitable);
    co_await async_sleep(co_await asio::this_coro::executor, std::chrono::seconds(1), asio::use_awaitable);
  }
}

int main()
{
  asio::io_context ctx{1};
  stdin_line_reader reader{ctx.get_executor()};
  co_spawn(ctx, do_read(reader), asio::detached);
  ctx.run();
}
