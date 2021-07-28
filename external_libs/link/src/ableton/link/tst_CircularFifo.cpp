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

#include <ableton/link/CircularFifo.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{

TEST_CASE("CircularFifo | PushNPop", "[CircularFifo]")
{
  CircularFifo<int, 2> cf;

  for (int i = 0; i < 2; ++i)
  {
    CHECK(cf.push(i));
  }

  CHECK(!cf.push(0));

  for (int i = 0; i < 2; ++i)
  {
    auto result = cf.pop();
    CHECK(result);
    CHECK(*result == i);
  }

  CHECK(!cf.pop());
}

TEST_CASE("CircularFifo | Wrap", "[CircularFifo]")
{
  CircularFifo<int, 2> cf;

  for (int i = 0; i < 5; ++i)
  {
    CHECK(cf.push(i));
    auto result = cf.pop();
    CHECK(result);
    CHECK(*result == i);
  }
}

TEST_CASE("CircularFifo | IsEmpty", "[CircularFifo]")
{
  CircularFifo<int, 2> cf;
  CHECK(cf.isEmpty());

  CHECK(cf.push(1));
  CHECK(!cf.isEmpty());
  CHECK(cf.push(2));
  CHECK(!cf.isEmpty());
  CHECK(!cf.push(3));
  CHECK(!cf.isEmpty());

  CHECK(cf.pop());
  CHECK(!cf.isEmpty());
  CHECK(cf.pop());
  CHECK(cf.isEmpty());
  CHECK(!cf.pop());
  CHECK(cf.isEmpty());
}

} // namespace link
} // namespace ableton
