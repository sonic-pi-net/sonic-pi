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

#include <ableton/discovery/IpV4Interface.hpp>
#include <ableton/discovery/MessageTypes.hpp>
#include <ableton/discovery/v1/Messages.hpp>
#include <ableton/platforms/asio/AsioWrapper.hpp>
#include <ableton/util/Injected.hpp>
#include <ableton/util/SafeAsyncHandler.hpp>
#include <algorithm>
#include <memory>

namespace ableton
{
namespace discovery
{

// An exception thrown when sending a udp message fails. Stores the
// interface through which the sending failed.
struct UdpSendException : std::runtime_error
{
  UdpSendException(const std::runtime_error& e, asio::ip::address ifAddr)
    : std::runtime_error(e.what())
    , interfaceAddr(std::move(ifAddr))
  {
  }

  asio::ip::address interfaceAddr;
};

// Throws UdpSendException
template <typename Interface, typename NodeId, typename Payload>
void sendUdpMessage(Interface& iface,
  NodeId from,
  const uint8_t ttl,
  const v1::MessageType messageType,
  const Payload& payload,
  const asio::ip::udp::endpoint& to)
{
  using namespace std;
  v1::MessageBuffer buffer;
  const auto messageBegin = begin(buffer);
  const auto messageEnd =
    v1::detail::encodeMessage(std::move(from), ttl, messageType, payload, messageBegin);
  const auto numBytes = static_cast<size_t>(distance(messageBegin, messageEnd));
  try
  {
    iface.send(buffer.data(), numBytes, to);
  }
  catch (const std::runtime_error& err)
  {
    throw UdpSendException{err, iface.endpoint().address()};
  }
}

// UdpMessenger uses a "shared_ptr pImpl" pattern to make it movable
// and to support safe async handler callbacks when receiving messages
// on the given interface.
template <typename Interface, typename NodeStateT, typename IoContext>
class UdpMessenger
{
public:
  using NodeState = NodeStateT;
  using NodeId = typename NodeState::IdType;
  using Timer = typename util::Injected<IoContext>::type::Timer;
  using TimerError = typename Timer::ErrorCode;
  using TimePoint = typename Timer::TimePoint;

  UdpMessenger(util::Injected<Interface> iface,
    NodeState state,
    util::Injected<IoContext> io,
    const uint8_t ttl,
    const uint8_t ttlRatio)
    : mpImpl(std::make_shared<Impl>(
        std::move(iface), std::move(state), std::move(io), ttl, ttlRatio))
  {
    // We need to always listen for incoming traffic in order to
    // respond to peer state broadcasts
    mpImpl->listen(MulticastTag{});
    mpImpl->listen(UnicastTag{});
    mpImpl->broadcastState();
  }

  UdpMessenger(const UdpMessenger&) = delete;
  UdpMessenger& operator=(const UdpMessenger&) = delete;

  UdpMessenger(UdpMessenger&& rhs)
    : mpImpl(std::move(rhs.mpImpl))
  {
  }

  ~UdpMessenger()
  {
    if (mpImpl != nullptr)
    {
      try
      {
        mpImpl->sendByeBye();
      }
      catch (const UdpSendException& err)
      {
        debug(mpImpl->mIo->log()) << "Failed to send bye bye message: " << err.what();
      }
    }
  }

  void updateState(NodeState state)
  {
    mpImpl->updateState(std::move(state));
  }

  // Broadcast the current state of the system to all peers. May throw
  // std::runtime_error if assembling a broadcast message fails or if
  // there is an error at the transport layer. Throws on failure.
  void broadcastState()
  {
    mpImpl->broadcastState();
  }

  // Asynchronous receive function for incoming messages from peers. Will
  // return immediately and the handler will be invoked when a message
  // is received. Handler must have operator() overloads for PeerState and
  // ByeBye messages.
  template <typename Handler>
  void receive(Handler handler)
  {
    mpImpl->setReceiveHandler(std::move(handler));
  }

private:
  struct Impl : std::enable_shared_from_this<Impl>
  {
    Impl(util::Injected<Interface> iface,
      NodeState state,
      util::Injected<IoContext> io,
      const uint8_t ttl,
      const uint8_t ttlRatio)
      : mIo(std::move(io))
      , mInterface(std::move(iface))
      , mState(std::move(state))
      , mTimer(mIo->makeTimer())
      , mLastBroadcastTime{}
      , mTtl(ttl)
      , mTtlRatio(ttlRatio)
      , mPeerStateHandler([](PeerState<NodeState>) {})
      , mByeByeHandler([](ByeBye<NodeId>) {})
    {
    }

    template <typename Handler>
    void setReceiveHandler(Handler handler)
    {
      mPeerStateHandler = [handler](
                            PeerState<NodeState> state) { handler(std::move(state)); };

      mByeByeHandler = [handler](ByeBye<NodeId> byeBye) { handler(std::move(byeBye)); };
    }

    void sendByeBye()
    {
      sendUdpMessage(
        *mInterface, mState.ident(), 0, v1::kByeBye, makePayload(), multicastEndpoint());
    }

    void updateState(NodeState state)
    {
      mState = std::move(state);
    }

    void broadcastState()
    {
      using namespace std::chrono;

      const auto minBroadcastPeriod = milliseconds{50};
      const auto nominalBroadcastPeriod = milliseconds(mTtl * 1000 / mTtlRatio);
      const auto timeSinceLastBroadcast =
        duration_cast<milliseconds>(mTimer.now() - mLastBroadcastTime);

      // The rate is limited to maxBroadcastRate to prevent flooding the network.
      const auto delay = minBroadcastPeriod - timeSinceLastBroadcast;

      // Schedule the next broadcast before we actually send the
      // message so that if sending throws an exception we are still
      // scheduled to try again. We want to keep trying at our
      // interval as long as this instance is alive.
      mTimer.expires_from_now(delay > milliseconds{0} ? delay : nominalBroadcastPeriod);
      mTimer.async_wait([this](const TimerError e) {
        if (!e)
        {
          broadcastState();
        }
      });

      // If we're not delaying, broadcast now
      if (delay < milliseconds{1})
      {
        debug(mIo->log()) << "Broadcasting state";
        sendPeerState(v1::kAlive, multicastEndpoint());
      }
    }

    void sendPeerState(
      const v1::MessageType messageType, const asio::ip::udp::endpoint& to)
    {
      sendUdpMessage(
        *mInterface, mState.ident(), mTtl, messageType, toPayload(mState), to);
      mLastBroadcastTime = mTimer.now();
    }

    void sendResponse(const asio::ip::udp::endpoint& to)
    {
      sendPeerState(v1::kResponse, to);
    }

    template <typename Tag>
    void listen(Tag tag)
    {
      mInterface->receive(util::makeAsyncSafe(this->shared_from_this()), tag);
    }

    template <typename Tag, typename It>
    void operator()(Tag tag,
      const asio::ip::udp::endpoint& from,
      const It messageBegin,
      const It messageEnd)
    {
      auto result = v1::parseMessageHeader<NodeId>(messageBegin, messageEnd);

      const auto& header = result.first;
      // Ignore messages from self and other groups
      if (header.ident != mState.ident() && header.groupId == 0)
      {
        debug(mIo->log()) << "Received message type "
                          << static_cast<int>(header.messageType) << " from peer "
                          << header.ident;

        switch (header.messageType)
        {
        case v1::kAlive:
          sendResponse(from);
          receivePeerState(std::move(result.first), result.second, messageEnd);
          break;
        case v1::kResponse:
          receivePeerState(std::move(result.first), result.second, messageEnd);
          break;
        case v1::kByeBye:
          receiveByeBye(std::move(result.first.ident));
          break;
        default:
          info(mIo->log()) << "Unknown message received of type: " << header.messageType;
        }
      }
      listen(tag);
    }

    template <typename It>
    void receivePeerState(
      v1::MessageHeader<NodeId> header, It payloadBegin, It payloadEnd)
    {
      try
      {
        auto state = NodeState::fromPayload(
          std::move(header.ident), std::move(payloadBegin), std::move(payloadEnd));

        // Handlers must only be called once
        auto handler = std::move(mPeerStateHandler);
        mPeerStateHandler = [](PeerState<NodeState>) {};
        handler(PeerState<NodeState>{std::move(state), header.ttl});
      }
      catch (const std::runtime_error& err)
      {
        info(mIo->log()) << "Ignoring peer state message: " << err.what();
      }
    }

    void receiveByeBye(NodeId nodeId)
    {
      // Handlers must only be called once
      auto byeByeHandler = std::move(mByeByeHandler);
      mByeByeHandler = [](ByeBye<NodeId>) {};
      byeByeHandler(ByeBye<NodeId>{std::move(nodeId)});
    }

    util::Injected<IoContext> mIo;
    util::Injected<Interface> mInterface;
    NodeState mState;
    Timer mTimer;
    TimePoint mLastBroadcastTime;
    uint8_t mTtl;
    uint8_t mTtlRatio;
    std::function<void(PeerState<NodeState>)> mPeerStateHandler;
    std::function<void(ByeBye<NodeId>)> mByeByeHandler;
  };

  std::shared_ptr<Impl> mpImpl;
};

// Factory function
template <typename Interface, typename NodeState, typename IoContext>
UdpMessenger<Interface, NodeState, IoContext> makeUdpMessenger(
  util::Injected<Interface> iface,
  NodeState state,
  util::Injected<IoContext> io,
  const uint8_t ttl,
  const uint8_t ttlRatio)
{
  return UdpMessenger<Interface, NodeState, IoContext>{
    std::move(iface), std::move(state), std::move(io), ttl, ttlRatio};
}

} // namespace discovery
} // namespace ableton
