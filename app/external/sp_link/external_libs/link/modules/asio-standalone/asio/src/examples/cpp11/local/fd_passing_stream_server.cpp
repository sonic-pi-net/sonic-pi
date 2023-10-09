//
// fd_passing_stream_server.cpp
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
// Copyright (c) 2021 Heiko Hund (heiko at openvpn dot net)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

// Demonstrates how to pass file descriptors between processes with Asio.
// The client sends a file name to the server. The server opens the file and
// passes the associated file descriptor back to the client.

#include <array>
#include <cstdio>
#include <cassert>
#include <iostream>
#include <memory>
#include "asio.hpp"

#if defined(ASIO_HAS_LOCAL_SOCKETS)

using asio::local::stream_protocol;

class session
  : public std::enable_shared_from_this<session>
{
public:
  session(stream_protocol::socket sock)
    : socket_(std::move(sock))
  {
  }

  void start()
  {
    do_read();
  }

private:
  void do_read()
  {
    auto self(shared_from_this());
    socket_.async_read_some(asio::buffer(data_),
        [this, self](std::error_code ec, std::size_t length)
        {
          if (ec)
            return;

          assert(length < data_.size());
          data_[length] = 0;
          do_write(data_.data());
        });
  }

  void do_write(const char* filename)
  {
    auto self(shared_from_this());
    socket_.async_wait(stream_protocol::socket::wait_write,
        [this, self, filename](std::error_code ec)
        {
          if (ec)
            return;

          FILE* f(::fopen(filename, "w+"));
          if (!f)
            return;

          struct msghdr msg = {};
          char buf[] = { 0 };
          struct iovec iov = { buf, sizeof(buf) };
          msg.msg_iov = &iov;
          msg.msg_iovlen = 1;

          union
          {
            struct cmsghdr align;
            char buf[CMSG_SPACE(sizeof(int))];
          } cmsgu;
          msg.msg_control = cmsgu.buf;
          msg.msg_controllen = sizeof(cmsgu.buf);

          struct cmsghdr* cmsg = CMSG_FIRSTHDR(&msg);
          cmsg->cmsg_len = CMSG_LEN(sizeof(int));
          cmsg->cmsg_level = SOL_SOCKET;
          cmsg->cmsg_type = SCM_RIGHTS;
          int fd = ::fileno(f);
          std::memcpy(CMSG_DATA(cmsg), &fd, sizeof(int));

          ssize_t s(::sendmsg(socket_.native_handle(), &msg, 0));
          ::fclose(f);
          if (s != -1)
            do_read();
        });
  }

  // The socket used to communicate with the client.
  stream_protocol::socket socket_;

  // Buffer used to store data received from the client.
  std::array<char, 1024> data_;
};

class server
{
public:
  server(asio::io_context& io_context, const std::string& file)
    : acceptor_(io_context, stream_protocol::endpoint(file))
  {
    do_accept();
  }

private:
  void do_accept()
  {
    acceptor_.async_accept(
        [this](std::error_code ec, stream_protocol::socket socket)
        {
          if (!ec)
          {
            std::make_shared<session>(std::move(socket))->start();
          }

          do_accept();
        });
  }

  stream_protocol::acceptor acceptor_;
};

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 2)
    {
      std::cerr << "Usage: fd_passing_stream_server <socketfile>\n";
      std::cerr << "*** WARNING: existing file is removed ***\n";
      return 1;
    }

    asio::io_context io_context;

    std::remove(argv[1]);
    server s(io_context, argv[1]);

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}

#else // defined(ASIO_HAS_LOCAL_SOCKETS)
# error Local sockets not available on this platform.
#endif // defined(ASIO_HAS_LOCAL_SOCKETS)
