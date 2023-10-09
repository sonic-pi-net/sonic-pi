//
// timeout.cpp
// ~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <asio/experimental/awaitable_operators.hpp>

using namespace asio;
using namespace asio::experimental::awaitable_operators;
using time_point = std::chrono::steady_clock::time_point;
using ip::tcp;

awaitable<void> echo(tcp::socket& sock, time_point& deadline)
{
  char data[4196];
  for (;;)
  {
    deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
    auto n = co_await sock.async_read_some(buffer(data), use_awaitable);
    co_await async_write(sock, buffer(data, n), use_awaitable);
  }
}

awaitable<void> watchdog(time_point& deadline)
{
  steady_timer timer(co_await this_coro::executor);
  auto now = std::chrono::steady_clock::now();
  while (deadline > now)
  {
    timer.expires_at(deadline);
    co_await timer.async_wait(use_awaitable);
    now = std::chrono::steady_clock::now();
  }
  throw std::system_error(std::make_error_code(std::errc::timed_out));
}

awaitable<void> handle_connection(tcp::socket sock)
{
  time_point deadline{};
  co_await (echo(sock, deadline) && watchdog(deadline));
}

awaitable<void> listen(tcp::acceptor& acceptor)
{
  for (;;)
  {
    co_spawn(
        acceptor.get_executor(),
        handle_connection(co_await acceptor.async_accept(use_awaitable)),
        detached);
  }
}

int main()
{
  io_context ctx;
  tcp::acceptor acceptor(ctx, {tcp::v4(), 54321});
  co_spawn(ctx, listen(acceptor), detached);
  ctx.run();
}
