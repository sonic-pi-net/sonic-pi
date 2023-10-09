//
// timer.cpp
// ~~~~~~~~~
//
// Copyright (c) 2003-2023 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include <iostream>
#include <asio.hpp>
#include <boost/bind/bind.hpp>

class printer
{
public:
  printer(asio::io_context& io)
    : strand_(asio::make_strand(io)),
      timer1_(io, asio::chrono::seconds(1)),
      timer2_(io, asio::chrono::seconds(1)),
      count_(0)
  {
    timer1_.async_wait(asio::bind_executor(strand_,
          boost::bind(&printer::print1, this)));

    timer2_.async_wait(asio::bind_executor(strand_,
          boost::bind(&printer::print2, this)));
  }

  ~printer()
  {
    std::cout << "Final count is " << count_ << std::endl;
  }

  void print1()
  {
    if (count_ < 10)
    {
      std::cout << "Timer 1: " << count_ << std::endl;
      ++count_;

      timer1_.expires_at(timer1_.expiry() + asio::chrono::seconds(1));

      timer1_.async_wait(asio::bind_executor(strand_,
            boost::bind(&printer::print1, this)));
    }
  }

  void print2()
  {
    if (count_ < 10)
    {
      std::cout << "Timer 2: " << count_ << std::endl;
      ++count_;

      timer2_.expires_at(timer2_.expiry() + asio::chrono::seconds(1));

      timer2_.async_wait(asio::bind_executor(strand_,
            boost::bind(&printer::print2, this)));
    }
  }

private:
  asio::strand<asio::io_context::executor_type> strand_;
  asio::steady_timer timer1_;
  asio::steady_timer timer2_;
  int count_;
};

int main()
{
  asio::io_context io;
  printer p(io);
  asio::thread t(boost::bind(&asio::io_context::run, &io));
  io.run();
  t.join();

  return 0;
}
