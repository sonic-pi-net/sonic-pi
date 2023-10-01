//
// throttling_proxy.cpp
// ~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio.hpp>
#include <asio/experimental/awaitable_operators.hpp>
#include <asio/experimental/channel.hpp>
#include <iostream>

using asio::as_tuple;
using asio::awaitable;
using asio::buffer;
using asio::co_spawn;
using asio::detached;
using asio::experimental::channel;
using asio::io_context;
using asio::ip::tcp;
using asio::steady_timer;
using asio::use_awaitable;
namespace this_coro = asio::this_coro;
using namespace asio::experimental::awaitable_operators;
using namespace std::literals::chrono_literals;

using token_channel = channel<void(asio::error_code, std::size_t)>;

awaitable<void> produce_tokens(std::size_t bytes_per_token,
    steady_timer::duration token_interval, token_channel& tokens)
{
  steady_timer timer(co_await this_coro::executor);
  for (;;)
  {
    co_await tokens.async_send(
        asio::error_code{}, bytes_per_token,
        use_awaitable);

    timer.expires_after(token_interval);
    co_await timer.async_wait(use_awaitable);
  }
}

awaitable<void> transfer(tcp::socket& from,
    tcp::socket& to, token_channel& tokens)
{
  std::array<unsigned char, 4096> data;
  for (;;)
  {
    std::size_t bytes_available = co_await tokens.async_receive(use_awaitable);
    while (bytes_available > 0)
    {
      std::size_t n = co_await from.async_read_some(
          buffer(data, bytes_available), use_awaitable);

      co_await async_write(to, buffer(data, n), use_awaitable);

      bytes_available -= n;
    }
  }
}

awaitable<void> proxy(tcp::socket client, tcp::endpoint target)
{
  constexpr std::size_t number_of_tokens = 100;
  constexpr size_t bytes_per_token = 20 * 1024;
  constexpr steady_timer::duration token_interval = 100ms;

  auto ex = client.get_executor();
  tcp::socket server(ex);
  token_channel client_tokens(ex, number_of_tokens);
  token_channel server_tokens(ex, number_of_tokens);

  co_await server.async_connect(target, use_awaitable);
  co_await (
      produce_tokens(bytes_per_token, token_interval, client_tokens) &&
      transfer(client, server, client_tokens) &&
      produce_tokens(bytes_per_token, token_interval, server_tokens) &&
      transfer(server, client, server_tokens)
    );
}

awaitable<void> listen(tcp::acceptor& acceptor, tcp::endpoint target)
{
  for (;;)
  {
    auto [e, client] = co_await acceptor.async_accept(as_tuple(use_awaitable));
    if (!e)
    {
      auto ex = client.get_executor();
      co_spawn(ex, proxy(std::move(client), target), detached);
    }
    else
    {
      std::cerr << "Accept failed: " << e.message() << "\n";
      steady_timer timer(co_await this_coro::executor);
      timer.expires_after(100ms);
      co_await timer.async_wait(use_awaitable);
    }
  }
}

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 5)
    {
      std::cerr << "Usage: throttling_proxy";
      std::cerr << " <listen_address> <listen_port>";
      std::cerr << " <target_address> <target_port>\n";
      return 1;
    }

    io_context ctx;

    auto listen_endpoint =
      *tcp::resolver(ctx).resolve(argv[1], argv[2],
          tcp::resolver::passive).begin();

    auto target_endpoint =
      *tcp::resolver(ctx).resolve(argv[3], argv[4]).begin();

    tcp::acceptor acceptor(ctx, listen_endpoint);
    co_spawn(ctx, listen(acceptor, target_endpoint), detached);
    ctx.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }
}
