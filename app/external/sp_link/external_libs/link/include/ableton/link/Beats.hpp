/* Copyright 2016, Ableton AG, Berlin. All rights reserved.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  If you would like to incorporate Link into a proprietary software application,
 *  please contact <link-devs@ableton.com>.
 */

#pragma once

#include <ableton/discovery/NetworkByteStreamSerializable.hpp>
#include <cmath>
#include <cstdint>

namespace ableton
{
namespace link
{

struct Beats
{
  Beats() = default;

  explicit Beats(const double beats)
    : mValue(std::llround(beats * 1e6))
  {
  }

  explicit Beats(const std::int64_t microBeats)
    : mValue(microBeats)
  {
  }

  double floating() const
  {
    return static_cast<double>(mValue) / 1e6;
  }

  std::int64_t microBeats() const
  {
    return mValue;
  }

  Beats operator-() const
  {
    return Beats{-mValue};
  }

  friend Beats abs(const Beats b)
  {
    return Beats{std::abs(b.mValue)};
  }

  friend Beats operator+(const Beats lhs, const Beats rhs)
  {
    return Beats{lhs.mValue + rhs.mValue};
  }

  friend Beats operator-(const Beats lhs, const Beats rhs)
  {
    return Beats{lhs.mValue - rhs.mValue};
  }

  friend Beats operator%(const Beats lhs, const Beats rhs)
  {
    return Beats{rhs.mValue == 0 ? 0 : (lhs.mValue % rhs.mValue)};
  }

  friend bool operator<(const Beats lhs, const Beats rhs)
  {
    return lhs.mValue < rhs.mValue;
  }

  friend bool operator>(const Beats lhs, const Beats rhs)
  {
    return lhs.mValue > rhs.mValue;
  }

  friend bool operator==(const Beats lhs, const Beats rhs)
  {
    return lhs.mValue == rhs.mValue;
  }

  friend bool operator!=(const Beats lhs, const Beats rhs)
  {
    return lhs.mValue != rhs.mValue;
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const Beats beats)
  {
    return discovery::sizeInByteStream(beats.microBeats());
  }

  template <typename It>
  friend It toNetworkByteStream(const Beats beats, It out)
  {
    return discovery::toNetworkByteStream(beats.microBeats(), std::move(out));
  }

  template <typename It>
  static std::pair<Beats, It> fromNetworkByteStream(It begin, It end)
  {
    auto result = discovery::Deserialize<std::int64_t>::fromNetworkByteStream(
      std::move(begin), std::move(end));
    return std::make_pair(Beats{result.first}, std::move(result.second));
  }

private:
    std::int64_t mValue = 0;
};

} // namespace link
} // namespace ableton
