//
// echo_server.cpp
// ~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio/co_spawn.hpp>
#include <asio/deferred.hpp>
#include <asio/detached.hpp>
#include <asio/io_context.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/signal_set.hpp>
#include <asio/write.hpp>
#include <cstdio>

using asio::ip::tcp;
using asio::awaitable;
using asio::co_spawn;
using asio::detached;
using default_token = asio::deferred_t;
using tcp_acceptor = default_token::as_default_on_t<tcp::acceptor>;
using tcp_socket = default_token::as_default_on_t<tcp::socket>;
namespace this_coro = asio::this_coro;

awaitable<void> echo(tcp_socket socket)
{
  try
  {
    char data[1024];
    for (;;)
    {
      std::size_t n = co_await socket.async_read_some(asio::buffer(data));
      co_await async_write(socket, asio::buffer(data, n));
    }
  }
  catch (std::exception& e)
  {
    std::printf("echo Exception: %s\n", e.what());
  }
}

awaitable<void> listener()
{
  auto executor = co_await this_coro::executor;
  tcp_acceptor acceptor(executor, {tcp::v4(), 55555});
  for (;;)
  {
    tcp::socket socket = co_await acceptor.async_accept();
    co_spawn(executor, echo(std::move(socket)), detached);
  }
}

int main()
{
  try
  {
    asio::io_context io_context(1);

    asio::signal_set signals(io_context, SIGINT, SIGTERM);
    signals.async_wait([&](auto, auto){ io_context.stop(); });

    co_spawn(io_context, listener(), detached);

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::printf("Exception: %s\n", e.what());
  }
}
