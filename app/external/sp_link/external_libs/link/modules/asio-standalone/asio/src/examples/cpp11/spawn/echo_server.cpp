//
// echo_server.cpp
// ~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <asio/detached.hpp>
#include <asio/io_context.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/spawn.hpp>
#include <asio/steady_timer.hpp>
#include <asio/write.hpp>
#include <iostream>
#include <memory>

using asio::ip::tcp;

class session : public std::enable_shared_from_this<session>
{
public:
  explicit session(asio::io_context& io_context, tcp::socket socket)
    : socket_(std::move(socket)),
      timer_(io_context),
      strand_(io_context.get_executor())
  {
  }

  void go()
  {
    auto self(shared_from_this());
    asio::spawn(strand_,
        [this, self](asio::yield_context yield)
        {
          try
          {
            char data[128];
            for (;;)
            {
              timer_.expires_after(std::chrono::seconds(10));
              std::size_t n = socket_.async_read_some(asio::buffer(data), yield);
              asio::async_write(socket_, asio::buffer(data, n), yield);
            }
          }
          catch (std::exception& e)
          {
            socket_.close();
            timer_.cancel();
          }
        }, asio::detached);

    asio::spawn(strand_,
        [this, self](asio::yield_context yield)
        {
          while (socket_.is_open())
          {
            asio::error_code ignored_ec;
            timer_.async_wait(yield[ignored_ec]);
            if (timer_.expiry() <= asio::steady_timer::clock_type::now())
              socket_.close();
          }
        }, asio::detached);
  }

private:
  tcp::socket socket_;
  asio::steady_timer timer_;
  asio::strand<asio::io_context::executor_type> strand_;
};

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 2)
    {
      std::cerr << "Usage: echo_server <port>\n";
      return 1;
    }

    asio::io_context io_context;

    asio::spawn(io_context,
        [&](asio::yield_context yield)
        {
          tcp::acceptor acceptor(io_context,
            tcp::endpoint(tcp::v4(), std::atoi(argv[1])));

          for (;;)
          {
            asio::error_code ec;
            tcp::socket socket(io_context);
            acceptor.async_accept(socket, yield[ec]);
            if (!ec)
            {
              std::make_shared<session>(io_context, std::move(socket))->go();
            }
          }
        },
        [](std::exception_ptr e)
        {
          if (e)
            std::rethrow_exception(e);
        });

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
