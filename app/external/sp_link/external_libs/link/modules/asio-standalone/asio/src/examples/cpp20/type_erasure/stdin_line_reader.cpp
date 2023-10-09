//
// stdin_line_reader.cpp
// ~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "stdin_line_reader.hpp"
#include <asio/deferred.hpp>
#include <asio/read_until.hpp>
#include <iostream>

stdin_line_reader::stdin_line_reader(asio::any_io_executor ex)
  : stdin_(ex, ::dup(STDIN_FILENO))
{
}

void stdin_line_reader::async_read_line_impl(std::string prompt,
    asio::any_completion_handler<void(asio::error_code, std::string)> handler)
{
  std::cout << prompt;
  std::cout.flush();

  asio::async_read_until(stdin_, asio::dynamic_buffer(buffer_), '\n',
      asio::deferred(
        [this](asio::error_code ec, std::size_t n)
        {
          if (!ec)
          {
            std::string result = buffer_.substr(0, n);
            buffer_.erase(0, n);
            return asio::deferred.values(ec, std::move(result));
          }
          else
          {
            return asio::deferred.values(ec, std::string{});
          }
        }
      )
    )(std::move(handler));
}
