//
// range_based_for.cpp
// ~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2020 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio/co_spawn.hpp>
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
using asio::use_awaitable;

class connection_iter
{
  friend class connections;
  tcp::acceptor* acceptor_ = nullptr;
  tcp::socket socket_;

  connection_iter(tcp::acceptor& a, tcp::socket s)
    : acceptor_(&a), socket_(std::move(s)) {}

public:
  tcp::socket operator*()
  {
    return std::move(socket_);
  }

  awaitable<void> operator++()
  {
    socket_ = co_await acceptor_->async_accept(use_awaitable);
  }

  bool operator==(const connection_iter&) const noexcept
  {
    return false;
  }

  bool operator!=(const connection_iter&) const noexcept
  {
    return true;
  }
};

class connections
{
  tcp::acceptor& acceptor_;

public:
  explicit connections(tcp::acceptor& a) : acceptor_(a) {}

  awaitable<connection_iter> begin()
  {
    tcp::socket s = co_await acceptor_.async_accept(use_awaitable);
    co_return connection_iter(acceptor_, std::move(s));
  }

  connection_iter end()
  {
    return connection_iter(acceptor_,
        tcp::socket(acceptor_.get_executor()));
  }
};

awaitable<void> listener(tcp::acceptor acceptor)
{
  for co_await (tcp::socket s : connections(acceptor))
  {
    co_await asio::async_write(s, asio::buffer("hello\r\n", 7), use_awaitable);
  }
}

int main()
{
  try
  {
    asio::io_context io_context(1);

    asio::signal_set signals(io_context, SIGINT, SIGTERM);
    signals.async_wait([&](auto, auto){ io_context.stop(); });

    tcp::acceptor acceptor(io_context, {tcp::v4(), 55555});
    co_spawn(io_context, listener(std::move(acceptor)), detached);

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::printf("Exception: %s\n", e.what());
  }
}
