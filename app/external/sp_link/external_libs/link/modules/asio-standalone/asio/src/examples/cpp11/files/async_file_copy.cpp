//
// async_file_copy.cpp
// ~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>
#include "asio.hpp"

#if defined(ASIO_HAS_FILE)

class file_copier
{
public:
  file_copier(asio::io_context& io_context,
      const char* from, const char* to)
    : from_file_(io_context, from,
        asio::stream_file::read_only),
      to_file_(io_context, to,
        asio::stream_file::write_only
          | asio::stream_file::create
          | asio::stream_file::truncate)
  {
  }

  void start()
  {
    do_read();
  }

private:
  void do_read()
  {
    from_file_.async_read_some(asio::buffer(data_),
        [this](std::error_code error, std::size_t n)
        {
          if (!error)
          {
            do_write(n);
          }
          else if (error != asio::error::eof)
          {
            std::cerr << "Error copying file: " << error.message() << "\n";
          }
        });
  }

  void do_write(std::size_t n)
  {
    asio::async_write(to_file_, asio::buffer(data_, n),
        [this](std::error_code error, std::size_t /*n*/)
        {
          if (!error)
          {
            do_read();
          }
          else
          {
            std::cerr << "Error copying file: " << error.message() << "\n";
          }
        });
  }

  asio::stream_file from_file_;
  asio::stream_file to_file_;
  char data_[4096];
};

int main(int argc, char* argv[])
{
  try
  {
    if (argc != 3)
    {
      std::cerr << "Usage: async_file_copy <from> <to>\n";
      return 1;
    }

    asio::io_context io_context;

    file_copier copier(io_context, argv[1], argv[2]);
    copier.start();

    io_context.run();
  }
  catch (std::exception& e)
  {
    std::cerr << "Exception: " << e.what() << "\n";
    return 1;
  }

  return 0;
}

#else // defined(ASIO_HAS_FILE)
int main() {}
#endif // defined(ASIO_HAS_FILE)
