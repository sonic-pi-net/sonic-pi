// Copyright 2019 Hans Dembinski
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_HISTOGRAM_DETAIL_COUNTING_STREAMBUF_HPP
#define BOOST_HISTOGRAM_DETAIL_COUNTING_STREAMBUF_HPP

#include <streambuf>

namespace boost {
namespace histogram {
namespace detail {

// detect how many characters will be printed by formatted output
template <class CharT, class Traits = std::char_traits<CharT>>
struct counting_streambuf : std::basic_streambuf<CharT, Traits> {
  using base_t = std::basic_streambuf<CharT, Traits>;
  using typename base_t::char_type;
  using typename base_t::int_type;

  std::streamsize count = 0;

  std::streamsize xsputn(const char_type* /* s */, std::streamsize n) override {
    count += n;
    return n;
  }

  int_type overflow(int_type ch) override {
    ++count;
    return ch;
  }
};

} // namespace detail
} // namespace histogram
} // namespace boost

#endif
