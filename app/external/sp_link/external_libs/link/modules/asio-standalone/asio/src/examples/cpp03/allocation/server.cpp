//
// server.cpp
// ~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <cstdlib>
#include <iostream>
#include <boost/aligned_storage.hpp>
#include <boost/array.hpp>
#include <boost/bind/bind.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include "asio.hpp"

using asio::ip::tcp;

// Class to manage the memory to be used for handler-based custom allocation.
// It contains a single block of memory which may be returned for allocation
// requests. If the memory is in use when an allocation request is made, the
// allocator delegates allocation to the global heap.
class handler_memory
  : private boost::noncopyable
{
public:
  handler_memory()
    : in_use_(false)
  {
  }

  void* allocate(std::size_t size)
  {
    if (!in_use_ && size < storage_.size)
    {
      in_use_ = true;
      return storage_.address();
    }
    else
    {
      return ::operator new(size);
    }
  }

  void deallocate(void* pointer)
  {
    if (pointer == storage_.address())
    {
      in_use_ = false;
    }
    else
    {
      ::operator delete(pointer);
    }
  }

private:
  // Storage space used for handler-based custom memory allocation.
  boost::aligned_storage<1024> storage_;

  // Whether the handler-based custom allocation storage has been used.
  bool in_use_;
};

// The allocator to be associated with the handler objects. This allocator only
// needs to satisfy the C++11 minimal allocator requirements, plus rebind when
// targeting C++03.
template <typename T>
class handler_allocator
{
public:
  typedef T value_type;

  explicit handler_allocator(handler_memory& mem)
    : memory_(mem)
  {
  }

  template <typename U>
  handler_allocator(const handler_allocator<U>& other)
    : memory_(other.memory_)
  {
  }

  template <typename U>
  struct rebind
  {
    typedef handler_allocator<U> other;
  };

  bool operator==(const handler_allocator& other) const
  {
    return &memory_ == &other.memory_;
  }

  bool operator!=(const handler_allocator& other) const
  {
    return &memory_ != &other.memory_;
  }

  T* allocate(std::size_t n) const
  {
    return static_cast<T*>(memory_.allocate(sizeof(T) * n));
  }

  void deallocate(T* p, std::size_t /*n*/) const
  {
    return memory_.deallocate(p);
  }

//private:
  // The underlying memory.
  handler_memory& memory_;
};

class session
  : public boost::enable_shared_from_this<session>
{
public:
  session(asio::io_context& io_context)
    : socket_(io_context)
  {
  }

  tcp::socket& socket()
  {
    return socket_;
  }

  void start()
  {
    socket_.async_read_some(asio::buffer(data_),
        asio::bind_allocator(
          handler_allocator<int>(handler_memory_),
          boost::bind(&session::handle_read,
            shared_from_this(),
            asio::placeholders::error,
            asio::placeholders::bytes_transferred)));
  }

  void handle_read(const asio::error_code& error,
      size_t bytes_transferred)
  {
    if (!error)
    {
      asio::async_write(socket_,
          asio::buffer(data_, bytes_transferred),
          asio::bind_allocator(
            handler_allocator<int>(handler_memory_),
            boost::bind(&session::handle_write,
              shared_from_this(),
              asio::placeholders::error)));
    }
  }

  void handle_write(const asio::error_code& error)
  {
    if (!error)
    {
      socket_.async_read_some(asio::buffer(data_),
          asio::bind_allocator(
            handler_allocator<int>(handler_memory_),
            boost::bind(&session::handle_read,
              shared_from_this(),
              asio::placeholders::error,
              asio::placeholders::bytes_transferred)));
    }
  }

private:
  // The socket used to communicate with the client.
  tcp::socket socket_;

  // Buffer used to store data received from the client.
  boost::array<char, 1024> data_;

  // The memory to use for handler-based custom memory allocation.
  handler_memory handler_memory_;
};

typedef boost::shared_ptr<session> session_ptr;

class server
{
public:
  server(asio::io_context& io_context, short port)
    : io_context_(io_context),
      acceptor_(io_context, tcp::endpoint(tcp::v4(), port))
  {
    session_ptr new_session(new session(io_context_));
    acceptor_.async_accept(new_session->socket(),
        boost::bind(&server::handle_accept, this, new_session,
          asio::placeholders::error));
  }

  void handle_accept(session_ptr new_session,
      const asio::error_code& error)
  {
    if (!error)
    {
      new_session->start();
    }

    new_session.reset(new session(io_context_));
    acceptor_.async_accept(new_session->socket(),
        boost::bind(&server::handle_accept, this, new_session,
          asio::placeholders::error));
  }

private:
  asio::io_context& io_context_;
  tcp::acceptor acceptor_;
};

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 2)
    {
      std::cerr << "Usage: server <port>\n";
      return 1;
    }

    asio::io_context io_context;

    using namespace std; // For atoi.
    server s(io_context, atoi(argv[1]));

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
  }

  return 0;
}
