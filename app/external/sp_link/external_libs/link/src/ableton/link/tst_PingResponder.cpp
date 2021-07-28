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
#include <ableton/link/GhostXForm.hpp>
#include <ableton/link/NodeState.hpp>
#include <ableton/link/PayloadEntries.hpp>
#include <ableton/link/PingResponder.hpp>
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

using Random = ableton::platforms::stl::Random;

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

  using Log = util::NullLog;

  Log log() const
  {
    return {};
  }

  template <typename Handler>
  void async(Handler handler) const
  {
    handler();
  }

  ableton::util::test::IoService mIo;
};

struct RpFixture
{
  RpFixture()
    : mAddress(asio::ip::address_v4::from_string("127.0.0.1"))
    , mResponder(mAddress,
        NodeId::random<Random>(),
        GhostXForm{1.0, std::chrono::microseconds{0}},
        MockClock{},
        util::injectRef(*mIo))
  {
  }

  discovery::test::Socket responderSocket()
  {
    return mResponder.socket();
  }

  std::size_t numSentMessages()
  {
    return responderSocket().sentMessages.size();
  }

  asio::ip::address_v4 mAddress = asio::ip::address_v4::from_string("127.0.0.1");
  util::Injected<MockIoContext> mIo;
  PingResponder<MockClock, MockIoContext> mResponder;
};

} // anonymous namespace

TEST_CASE("PingResponder | ReplyToPing", "[PingResponder]")
{
  using std::chrono::microseconds;

  RpFixture fixture;

  // Construct and send Ping Message. Check if Responder sent back a Message
  const auto payload =
    discovery::makePayload(HostTime{microseconds(2)}, PrevGHostTime{microseconds(1)});

  v1::MessageBuffer buffer;
  const auto msgBegin = std::begin(buffer);
  const auto msgEnd = v1::pingMessage(payload, msgBegin);
  const auto endpoint = asio::ip::udp::endpoint(fixture.mAddress, 8888);

  fixture.responderSocket().incomingMessage(endpoint, msgBegin, msgEnd);

  CHECK(1 == fixture.numSentMessages());

  // Check Responder's message
  const auto messageBuffer = fixture.responderSocket().sentMessages[0].first;
  const auto result = v1::parseMessageHeader(begin(messageBuffer), end(messageBuffer));
  const auto& hdr = result.first;

  std::chrono::microseconds ghostTime{0};
  std::chrono::microseconds prevGHostTime{0};
  std::chrono::microseconds hostTime{0};
  discovery::parsePayload<GHostTime, PrevGHostTime, HostTime>(result.second,
    std::end(messageBuffer),
    [&ghostTime](GHostTime gt) { ghostTime = std::move(gt.time); },
    [&prevGHostTime](PrevGHostTime gt) { prevGHostTime = std::move(gt.time); },
    [&hostTime](HostTime ht) { hostTime = std::move(ht.time); });

  CHECK(v1::kPong == hdr.messageType);
  CHECK(std::chrono::microseconds{2} == hostTime);
  CHECK(std::chrono::microseconds{1} == prevGHostTime);
  CHECK(std::chrono::microseconds{4} == ghostTime);
}

TEST_CASE("PingResponder | PingSizeExceeding", "[PingResponder]")
{
  using std::chrono::microseconds;

  RpFixture fixture;

  const auto ht = HostTime{microseconds(2)};
  const auto payload = discovery::makePayload(ht, ht, ht);

  v1::MessageBuffer buffer;
  const auto msgBegin = std::begin(buffer);
  const auto msgEnd = v1::pingMessage(payload, msgBegin);

  const auto endpoint = asio::ip::udp::endpoint(fixture.mAddress, 8888);

  fixture.responderSocket().incomingMessage(endpoint, msgBegin, msgEnd);

  CHECK(0 == fixture.numSentMessages());
}

} // namespace link
} // namespace ableton
