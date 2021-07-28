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

#include <ableton/link/Beats.hpp>
#include <chrono>

namespace ableton
{
namespace link
{

struct Tempo
{
  Tempo() = default;

  // Beats per minute
  explicit Tempo(const double bpm)
    : mValue(bpm)
  {
  }

  Tempo(const std::chrono::microseconds microsPerBeat)
    : mValue(60. * 1e6 / static_cast<double>(microsPerBeat.count()))
  {
  }

  double bpm() const
  {
    return mValue;
  }

  std::chrono::microseconds microsPerBeat() const
  {
    return std::chrono::microseconds{std::llround(60. * 1e6 / bpm())};
  }

  // Given the tempo, convert a time to a beat value
  Beats microsToBeats(const std::chrono::microseconds micros) const
  {
    return Beats{
      static_cast<double>(micros.count()) / static_cast<double>(microsPerBeat().count())};
  }

  // Given the tempo, convert a beat to a time value
  std::chrono::microseconds beatsToMicros(const Beats beats) const
  {
    return std::chrono::microseconds{
      std::llround(beats.floating() * static_cast<double>(microsPerBeat().count()))};
  }

  // Model the NetworkByteStreamSerializable concept
  friend std::uint32_t sizeInByteStream(const Tempo tempo)
  {
    return discovery::sizeInByteStream(tempo.microsPerBeat());
  }

  template <typename It>
  friend It toNetworkByteStream(const Tempo tempo, It out)
  {
    return discovery::toNetworkByteStream(tempo.microsPerBeat(), std::move(out));
  }

  template <typename It>
  static std::pair<Tempo, It> fromNetworkByteStream(It begin, It end)
  {
    auto result =
      discovery::Deserialize<std::chrono::microseconds>::fromNetworkByteStream(
        std::move(begin), std::move(end));
    return std::make_pair(Tempo{std::move(result.first)}, std::move(result.second));
  }

  friend bool operator==(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue == rhs.mValue;
  }

  friend bool operator!=(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue != rhs.mValue;
  }

  friend bool operator<(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue < rhs.mValue;
  }

  friend bool operator>(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue > rhs.mValue;
  }

  friend bool operator<=(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue <= rhs.mValue;
  }

  friend bool operator>=(const Tempo lhs, const Tempo rhs)
  {
    return lhs.mValue >= rhs.mValue;
  }

private:
  double mValue = 0;
};

} // namespace link
} // namespace ableton
