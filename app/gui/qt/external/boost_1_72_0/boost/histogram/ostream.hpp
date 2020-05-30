// Copyright 2015-2019 Hans Dembinski
// Copyright 2019 Przemyslaw Bartosik
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt
// or copy at http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_HISTOGRAM_OSTREAM_HPP
#define BOOST_HISTOGRAM_OSTREAM_HPP

#include <boost/histogram/accumulators/ostream.hpp>
#include <boost/histogram/axis/ostream.hpp>
#include <boost/histogram/axis/variant.hpp>
#include <boost/histogram/detail/axes.hpp>
#include <boost/histogram/detail/counting_streambuf.hpp>
#include <boost/histogram/detail/detect.hpp>
#include <boost/histogram/detail/static_if.hpp>
#include <boost/histogram/indexed.hpp>
#include <cmath>
#include <iomanip>
#include <ios>
#include <limits>
#include <numeric>
#include <ostream>
#include <streambuf>
#include <type_traits>

/**
  \file boost/histogram/ostream.hpp

  A simple streaming operator for the histogram type. The text representation is
  rudimentary and not guaranteed to be stable between versions of Boost.Histogram. This
  header is not included by any other header and must be explicitly included to use the
  streaming operator.

  To you use your own, simply include your own implementation instead of this header.
 */

namespace boost {
namespace histogram {
namespace detail {

template <class OStream, unsigned N>
class tabular_ostream_wrapper : public std::array<int, N> {
  using base_t = std::array<int, N>;
  using char_type = typename OStream::char_type;
  using traits_type = typename OStream::traits_type;

public:
  template <class T>
  tabular_ostream_wrapper& operator<<(const T& t) {
    if (collect_) {
      if (static_cast<std::size_t>(iter_ - base_t::begin()) == size_) {
        ++size_;
        BOOST_ASSERT(size_ <= N);
        BOOST_ASSERT(iter_ != end());
        *iter_ = 0;
      }
      cbuf_.count = 0;
      os_ << t;
      *iter_ = std::max(*iter_, static_cast<int>(cbuf_.count));
    } else {
      BOOST_ASSERT(iter_ != end());
      os_ << std::setw(*iter_) << t;
    }
    ++iter_;
    return *this;
  }

  tabular_ostream_wrapper& operator<<(decltype(std::setprecision(0)) t) {
    os_ << t;
    return *this;
  }

  tabular_ostream_wrapper& operator<<(decltype(std::fixed) t) {
    os_ << t;
    return *this;
  }

  tabular_ostream_wrapper& row() {
    iter_ = base_t::begin();
    return *this;
  }

  explicit tabular_ostream_wrapper(OStream& os) : os_(os), orig_(os_.rdbuf(&cbuf_)) {}

  auto end() { return base_t::begin() + size_; }
  auto end() const { return base_t::begin() + size_; }
  auto cend() const { return base_t::cbegin() + size_; }

  void complete() {
    BOOST_ASSERT(collect_); // only call this once
    collect_ = false;
    os_.rdbuf(orig_);
  }

private:
  typename base_t::iterator iter_ = base_t::begin();
  std::size_t size_ = 0;
  bool collect_ = true;
  OStream& os_;
  counting_streambuf<char_type, traits_type> cbuf_;
  std::basic_streambuf<char_type, traits_type>* orig_;
};

template <class OStream, class T>
void ostream_value(OStream& os, const T& val) {
  // a value from bin or histogram cell
  os << std::left;
  static_if_c<(std::is_convertible<T, double>::value && !std::is_integral<T>::value)>(
      [](auto& os, const auto& val) {
        const auto d = static_cast<double>(val);
        if (std::isfinite(d)) {
          const auto i = static_cast<std::int64_t>(d);
          if (i == d) {
            os << i;
            return;
          }
        }
        os << std::defaultfloat << std::setprecision(4) << d;
      },
      [](auto& os, const auto& val) { os << val; }, os, val);
}

template <class OStream, class Axis>
void ostream_bin(OStream& os, const Axis& ax, const int i) {
  os << std::right;
  static_if<has_method_value<Axis>>(
      [&](const auto& ax) {
        static_if<axis::traits::is_continuous<Axis>>(
            [&](const auto& ax) {
              os << std::defaultfloat << std::setprecision(4);
              auto a = ax.value(i);
              auto b = ax.value(i + 1);
              // round bin edge to zero if deviation from zero is absolut and relatively
              // small
              const auto eps = 1e-8 * std::abs(b - a);
              if (std::abs(a) < 1e-14 && std::abs(a) < eps) a = 0;
              if (std::abs(b) < 1e-14 && std::abs(b) < eps) b = 0;
              os << "[" << a << ", " << b << ")";
            },
            [&](const auto& ax) { os << ax.value(i); }, ax);
      },
      [&](const auto&) { os << i; }, ax);
}

template <class OStream, class... Ts>
void ostream_bin(OStream& os, const axis::category<Ts...>& ax, const int i) {
  os << std::right;
  if (i < ax.size())
    os << ax.value(i);
  else
    os << "other";
}

template <class CharT>
struct line_t {
  CharT ch;
  int size;
};

template <class CharT>
auto line(CharT c, int n) {
  return line_t<CharT>{c, n};
}

template <class C, class T>
std::basic_ostream<C, T>& operator<<(std::basic_ostream<C, T>& os, line_t<C>&& l) {
  for (int i = 0; i < l.size; ++i) os << l.ch;
  return os;
}

template <class OStream, class Axis, class T>
void stream_head(OStream& os, const Axis& ax, int index, const T& val) {
  axis::visit(
      [&](const auto& ax) {
        ostream_bin(os, ax, index);
        os << ' ';
        ostream_value(os, val);
      },
      ax);
}

template <class OStream, class Histogram>
void ascii_plot(OStream& os, const Histogram& h, int w_total) {
  if (w_total == 0) w_total = 78; // TODO detect actual width of terminal

  const auto& ax = h.axis();

  // value range; can be integer or float, positive or negative
  double vmin = 0;
  double vmax = 0;
  tabular_ostream_wrapper<OStream, 7> tos(os);
  // first pass to get widths
  for (auto&& v : indexed(h, coverage::all)) {
    stream_head(tos.row(), ax, v.index(), *v);
    vmin = std::min(vmin, static_cast<double>(*v));
    vmax = std::max(vmax, static_cast<double>(*v));
  }
  tos.complete();
  if (vmax == 0) vmax = 1;

  // calculate width useable by bar (notice extra space at top)
  // <-- head --> |<--- bar ---> |
  // w_head + 2 + 2
  const int w_head = std::accumulate(tos.begin(), tos.end(), 0);
  const int w_bar = w_total - 4 - w_head;
  if (w_bar < 0) return;

  // draw upper line
  os << '\n' << line(' ', w_head + 1) << '+' << line('-', w_bar + 1) << "+\n";

  const int zero_offset = static_cast<int>(std::lround((-vmin) / (vmax - vmin) * w_bar));
  for (auto&& v : indexed(h, coverage::all)) {
    stream_head(tos.row(), ax, v.index(), *v);
    // rest uses os, not tos
    os << " |";
    const int k = static_cast<int>(std::lround(*v / (vmax - vmin) * w_bar));
    if (k < 0) {
      os << line(' ', zero_offset + k) << line('=', -k) << line(' ', w_bar - zero_offset);
    } else {
      os << line(' ', zero_offset) << line('=', k) << line(' ', w_bar - zero_offset - k);
    }
    os << " |\n";
  }

  // draw lower line
  os << line(' ', w_head + 1) << '+' << line('-', w_bar + 1) << "+\n";
}

template <class OStream, class Histogram>
void ostream(OStream& os, const Histogram& h, const bool show_values = true) {
  os << "histogram(";

  unsigned iaxis = 0;
  const auto rank = h.rank();
  h.for_each_axis([&](const auto& ax) {
    using A = std::decay_t<decltype(ax)>;
    if ((show_values && rank > 0) || rank > 1) os << "\n  ";
    static_if<is_streamable<A>>([&](const auto& ax) { os << ax; },
                                [&](const auto&) { os << "<unstreamable>"; }, ax);
  });

  if (show_values && rank > 0) {
    tabular_ostream_wrapper<OStream, (BOOST_HISTOGRAM_DETAIL_AXES_LIMIT + 1)> tos(os);
    for (auto&& v : indexed(h, coverage::all)) {
      tos.row();
      for (auto i : v.indices()) tos << std::right << i;
      ostream_value(tos, *v);
    }
    tos.complete();

    const int w_item = std::accumulate(tos.begin(), tos.end(), 0) + 4 + h.rank();
    const int nrow = std::max(1, 65 / w_item);
    int irow = 0;
    for (auto&& v : indexed(h, coverage::all)) {
      os << (irow == 0 ? "\n  (" : " (");
      tos.row();
      iaxis = 0;
      for (auto i : v.indices()) {
        tos << std::right << i;
        os << (++iaxis == h.rank() ? "):" : " ");
      }
      os << ' ';
      ostream_value(tos, *v);
      ++irow;
      if (nrow > 0 && irow == nrow) irow = 0;
    }
    os << '\n';
  }
  os << ')';
}

} // namespace detail

#ifndef BOOST_HISTOGRAM_DOXYGEN_INVOKED

template <typename CharT, typename Traits, typename A, typename S>
std::basic_ostream<CharT, Traits>& operator<<(std::basic_ostream<CharT, Traits>& os,
                                              const histogram<A, S>& h) {
  // save fmt
  const auto flags = os.flags();

  os.flags(std::ios::dec | std::ios::left);

  const auto w = static_cast<int>(os.width());
  os.width(0);

  using value_type = typename histogram<A, S>::value_type;
  detail::static_if<std::is_convertible<value_type, double>>(
      [&os, w](const auto& h) {
        if (h.rank() == 1) {
          detail::ostream(os, h, false);
          detail::ascii_plot(os, h, w);
        } else
          detail::ostream(os, h);
      },
      [&os](const auto& h) { detail::ostream(os, h); }, h);

  // restore fmt
  os.flags(flags);
  return os;
}

} // namespace histogram
} // namespace boost

#endif // BOOST_HISTOGRAM_DOXYGEN_INVOKED

#endif
