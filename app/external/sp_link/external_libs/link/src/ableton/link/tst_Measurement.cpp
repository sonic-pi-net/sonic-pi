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

#include <ableton/discovery/Payload.hpp>
#include <ableton/discovery/test/Socket.hpp>
#include <ableton/link/Measurement.hpp>
#include <ableton/link/SessionId.hpp>
#include <ableton/link/v1/Messages.hpp>
#include <ableton/platforms/stl/Random.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/util/test/IoService.hpp>
#include <array>

namespace ableton
{
namespace link
{
namespace
{

struct MockClock
{
  std::chrono::microseconds micros() const
  {
    return std::chrono::microseconds{4};
  }
};

struct MockIoContext
{
  template <std::size_t BufferSize>
  using Socket = discovery::test::Socket;

  template <std::size_t BufferSize>
  Socket<BufferSize> openUnicastSocket(const asio::ip::address_v4&)
  {
    return Socket<BufferSize>(mIo);
  }

  using Timer = util::test::Timer;

  Timer makeTimer()
  {
    return {};
  }

  using Log = util::NullLog;

  Log log() const
  {
    return {};
  }

  ableton::util::test::IoService mIo;
};

struct TFixture
{
  TFixture()
    : mMeasurement(mStateQuery(),
        [](std::vector<std::pair<double, double>>) {},
        {},
        MockClock{},
        MockIoContext{})
  {
  }

  discovery::test::Socket socket()
  {
    return mMeasurement.mpImpl->mSocket;
  }

  struct StateQuery
  {
    StateQuery()
    {
      using Random = ableton::platforms::stl::Random;
      mState.nodeState.sessionId = NodeId::random<Random>();
      mState.endpoint =
        asio::ip::udp::endpoint(asio::ip::address_v4::from_string("127.0.0.1"), 9999);
    }

    PeerState operator()()
    {
      return mState;
    }
    PeerState mState;
  };

  StateQuery mStateQuery;
  Measurement<MockClock, MockIoContext> mMeasurement;
};

} // anonymous namespace

TEST_CASE("PeerMeasurement | SendPingsOnConstruction", "[PeerMeasurement]")
{
  TFixture fixture;
  CHECK(1 == fixture.socket().sentMessages.size());

  const auto messageBuffer = fixture.socket().sentMessages[0].first;
  const auto result =
    v1::parseMessageHeader(std::begin(messageBuffer), std::end(messageBuffer));
  const auto& header = result.first;

  std::chrono::microseconds gt{0};
  std::chrono::microseconds ht{0};
  discovery::parsePayload<GHostTime, HostTime>(result.second, std::end(messageBuffer),
    [&gt](GHostTime ghostTime) { gt = std::move(ghostTime.time); },
    [&ht](HostTime hostTime) { ht = std::move(hostTime.time); });

  CHECK(v1::kPing == header.messageType);
  CHECK(std::chrono::microseconds{4} == ht);
  CHECK(std::chrono::microseconds{0} == gt);
}

TEST_CASE("PeerMeasurement | ReceiveInitPong", "[PeerMeasurement]")
{
  using Micros = std::chrono::microseconds;
  TFixture fixture;

  const auto id = SessionMembership{fixture.mStateQuery.mState.nodeState.sessionId};
  const auto ht = HostTime{Micros(0)};
  const auto gt = GHostTime{Micros(0)};
  const auto payload = discovery::makePayload(id, gt, ht);

  v1::MessageBuffer buffer;
  const auto msgBegin = std::begin(buffer);
  const auto msgEnd = v1::pongMessage(payload, msgBegin);

  const auto endpoint =
    asio::ip::udp::endpoint(asio::ip::address_v4::from_string("127.0.0.1"), 8888);
  fixture.socket().incomingMessage(endpoint, msgBegin, msgEnd);

  CHECK(2 == fixture.socket().sentMessages.size());
  CHECK(0 == fixture.mMeasurement.mpImpl->mData.size());
}

TEST_CASE("PeerMeasurement | ReceivePong", "[PeerMeasurement]")
{
  using Micros = std::chrono::microseconds;
  TFixture fixture;

  const auto id = SessionMembership{fixture.mStateQuery.mState.nodeState.sessionId};
  const auto ht = HostTime{Micros(2)};
  const auto gt = GHostTime{Micros(3)};
  const auto pgt = PrevGHostTime{Micros(1)};
  const auto payload = discovery::makePayload(id, gt, ht, pgt);

  v1::MessageBuffer buffer;
  const auto msgBegin = std::begin(buffer);
  const auto msgEnd = v1::pongMessage(payload, msgBegin);

  const auto endpoint =
    asio::ip::udp::endpoint(asio::ip::address_v4::from_string("127.0.0.1"), 8888);
  fixture.socket().incomingMessage(endpoint, msgBegin, msgEnd);

  CHECK(2 == fixture.socket().sentMessages.size());
  CHECK(2 == fixture.mMeasurement.mpImpl->mData.size());
}

} // namespace link
} // namespace ableton
