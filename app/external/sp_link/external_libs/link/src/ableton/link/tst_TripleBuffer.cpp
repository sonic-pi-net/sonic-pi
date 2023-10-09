/* Copyright 2022, Ableton AG, Berlin. All rights reserved.
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


#include <ableton/link/TripleBuffer.hpp>
#include <ableton/test/CatchWrapper.hpp>

#include <array>
#include <cstdint>
#include <functional>
#include <random>
#include <thread>

namespace ableton
{
namespace link
{
namespace
{

constexpr auto kNumTestOps = 1u << 18u;

struct BigValue
{
  BigValue(const uint32_t s)
    : seed{s}
  {
    // Generate random values to take some time and test read/write consistency.
    std::minstd_rand generator{s};
    std::generate(values.begin(), values.end(), generator);
  }

  uint32_t seed;                   // Seed that identifies this value
  std::array<uint32_t, 40> values; // Seed-derived data larger than a cache line
};

void writeValues(TripleBuffer<BigValue>& buffer, const uint32_t numOps)
{
  for (uint32_t i = 0; i < numOps; ++i)
  {
    buffer.write(i);
  }
}

void readValues(TripleBuffer<BigValue>& buffer, const uint32_t numOps)
{
  auto prevValueSeed = 0u;

  for (uint32_t i = 0; i < numOps; ++i)
  {
    const auto thisValue = buffer.read();

    CHECK(thisValue.seed >= prevValueSeed);
    CHECK(thisValue.values == BigValue{thisValue.seed}.values);

    prevValueSeed = thisValue.seed;
  }
}

} // namespace

TEST_CASE("TripleBuffer")
{
  SECTION("Construction")
  {
    TripleBuffer<int> buffer;
  }

  SECTION("Reads default value before any writes")
  {
    TripleBuffer<int> buffer;

    CHECK(buffer.read() == int{});
    CHECK(buffer.read() == int{});
    CHECK(buffer.read() == int{});
  }

  SECTION("Reads initial value before any writes")
  {
    TripleBuffer<int> buffer{42};

    CHECK(buffer.read() == 42);
    CHECK(buffer.read() == 42);
    CHECK(buffer.read() == 42);
  }

  SECTION("Reads last written value")
  {
    TripleBuffer<int> buffer;

    buffer.write(42);
    CHECK(buffer.read() == 42);
    CHECK(buffer.read() == 42);
    CHECK(buffer.read() == 42);

    buffer.write(43);
    CHECK(buffer.read() == 43);
    CHECK(buffer.read() == 43);
    CHECK(buffer.read() == 43);

    buffer.write(44);
    CHECK(buffer.read() == 44);
    CHECK(buffer.read() == 44);
    CHECK(buffer.read() == 44);

    buffer.write(45);
    CHECK(buffer.read() == 45);
    CHECK(buffer.read() == 45);
    CHECK(buffer.read() == 45);
  }

  SECTION("Reads new last written value")
  {
    TripleBuffer<int> buffer;

    buffer.write(42);
    CHECK(*buffer.readNew() == 42);
    CHECK(!buffer.readNew());
    CHECK(buffer.read() == 42);

    buffer.write(43);
    CHECK(buffer.read() == 43);
    CHECK(!buffer.readNew());
    CHECK(buffer.read() == 43);

    buffer.write(44);
    CHECK(*buffer.readNew() == 44);
    CHECK(!buffer.readNew());
    CHECK(buffer.read() == 44);

    buffer.write(45);
    CHECK(buffer.read() == 45);
    CHECK(!buffer.readNew());
    CHECK(buffer.read() == 45);
  }

  SECTION("Threaded read and write")
  {
    TripleBuffer<BigValue> buffer{0u};
    std::thread writer{writeValues, std::ref(buffer), kNumTestOps};
    std::thread reader{readValues, std::ref(buffer), kNumTestOps};

    writer.join();
    reader.join();
  }
}

} // namespace link
} // namespace ableton
