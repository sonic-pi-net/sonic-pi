//
// stdin_line_reader.hpp
// ~~~~~~~~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#ifndef STDIN_LINE_READER_HPP
#define STDIN_LINE_READER_HPP

#include "line_reader.hpp"
#include <asio/posix/stream_descriptor.hpp>

class stdin_line_reader : public line_reader
{
public:
  explicit stdin_line_reader(asio::any_io_executor ex);

private:
  void async_read_line_impl(std::string prompt,
      asio::any_completion_handler<void(asio::error_code, std::string)> handler) override;

  asio::posix::stream_descriptor stdin_;
  std::string buffer_;
};

#endif
