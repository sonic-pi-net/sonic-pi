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

#include <ableton/discovery/v1/Messages.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <array>

namespace ableton
{
namespace discovery
{
namespace v1
{
namespace
{

// For testing just use a single byte identifier
using NodeId = uint8_t;

} // anonymous namespace

TEST_CASE("ParseEmptyBuffer", "[Messages]")
{
  const std::array<char, 0> buffer{};
  auto result = parseMessageHeader<NodeId>(begin(buffer), end(buffer));
  CHECK(kInvalid == result.first.messageType);
  CHECK(begin(buffer) == result.second);
}

TEST_CASE("ParseTruncatedMessageHeader", "[Messages]")
{
  const std::array<char, 10> truncated = {
    {'_', 'a', 's', 'd', 'p', '_', 'v', 1, 'x', 'y'}};
  const auto result = parseMessageHeader<NodeId>(begin(truncated), end(truncated));
  CHECK(kInvalid == result.first.messageType);
  const auto consumedBytes = (begin(truncated) != result.second);
  CHECK_FALSE(consumedBytes);
}

TEST_CASE("MissingProtocolHeader", "[Messages]")
{
  // Buffer should be large enough to proceed but shouldn't match the protocol header
  const std::array<char, 32> zeros{};
  auto result = parseMessageHeader<NodeId>(begin(zeros), end(zeros));
  CHECK(kInvalid == result.first.messageType);
  CHECK(begin(zeros) == result.second);
}

TEST_CASE("RoundtripAliveNoPayload", "[Messages]")
{
  std::array<char, 32> buffer{};
  const uint8_t ident = 1;
  const auto endAlive = aliveMessage(ident, 5, makePayload(), begin(buffer));
  const auto result = parseMessageHeader<NodeId>(begin(buffer), endAlive);
  CHECK(endAlive == result.second);
  CHECK(kAlive == result.first.messageType);
  CHECK(5 == result.first.ttl);
  CHECK(ident == result.first.ident);
}

} // namespace v1
} // namespace discovery
} // namespace ableton
