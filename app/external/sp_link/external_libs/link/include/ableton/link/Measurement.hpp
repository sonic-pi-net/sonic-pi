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

#include <ableton/discovery/Payload.hpp>
#include <ableton/link/PayloadEntries.hpp>
#include <ableton/link/PeerState.hpp>
#include <ableton/link/SessionId.hpp>
#include <ableton/link/v1/Messages.hpp>
#include <ableton/util/Injected.hpp>
#include <ableton/util/SafeAsyncHandler.hpp>
#include <chrono>
#include <memory>

namespace ableton
{
namespace link
{

template <typename Clock, typename IoContext>
struct Measurement
{
  using Point = std::pair<double, double>;
  using Callback = std::function<void(std::vector<Point>)>;
  using Micros = std::chrono::microseconds;

  static const std::size_t kNumberDataPoints = 100;
  static const std::size_t kNumberMeasurements = 5;

  Measurement(const PeerState& state,
    Callback callback,
    asio::ip::address_v4 address,
    Clock clock,
    IoContext io)
    : mIo(std::move(io))
    , mpImpl(std::make_shared<Impl>(std::move(state),
        std::move(callback),
        std::move(address),
        std::move(clock),
        mIo))
  {
    mpImpl->listen();
  }

  Measurement(const Measurement&) = delete;
  Measurement& operator=(Measurement&) = delete;
  Measurement(const Measurement&&) = delete;
  Measurement& operator=(Measurement&&) = delete;

  struct Impl : std::enable_shared_from_this<Impl>
  {
    using Socket = typename IoContext::template Socket<v1::kMaxMessageSize>;
    using Timer = typename IoContext::Timer;
    using Log = typename IoContext::Log;

    Impl(const PeerState& state,
      Callback callback,
      asio::ip::address_v4 address,
      Clock clock,
      IoContext& io)
      : mSocket(io.template openUnicastSocket<v1::kMaxMessageSize>(address))
      , mSessionId(state.nodeState.sessionId)
      , mEndpoint(state.endpoint)
      , mCallback(std::move(callback))
      , mClock(std::move(clock))
      , mTimer(io.makeTimer())
      , mMeasurementsStarted(0)
      , mLog(channel(io.log(), "Measurement on gateway@" + address.to_string()))
      , mSuccess(false)
    {
      const auto ht = HostTime{mClock.micros()};
      sendPing(mEndpoint, discovery::makePayload(ht));
      resetTimer();
    }

    void resetTimer()
    {
      mTimer.cancel();
      mTimer.expires_from_now(std::chrono::milliseconds(50));
      mTimer.async_wait([this](const typename Timer::ErrorCode e) {
        if (!e)
        {
          if (mMeasurementsStarted < kNumberMeasurements)
          {
            const auto ht = HostTime{mClock.micros()};
            sendPing(mEndpoint, discovery::makePayload(ht));
            ++mMeasurementsStarted;
            resetTimer();
          }
          else
          {
            fail();
          }
        }
      });
    }

    void listen()
    {
      mSocket.receive(util::makeAsyncSafe(this->shared_from_this()));
    }

    // Operator to handle incoming messages on the interface
    template <typename It>
    void operator()(
      const asio::ip::udp::endpoint& from, const It messageBegin, const It messageEnd)
    {
      using namespace std;
      const auto result = v1::parseMessageHeader(messageBegin, messageEnd);
      const auto& header = result.first;
      const auto payloadBegin = result.second;

      if (header.messageType == v1::kPong)
      {
        debug(mLog) << "Received Pong message from " << from;

        // parse for all entries
        SessionId sessionId{};
        std::chrono::microseconds ghostTime{0};
        std::chrono::microseconds prevGHostTime{0};
        std::chrono::microseconds prevHostTime{0};

        try
        {
          discovery::parsePayload<SessionMembership, GHostTime, PrevGHostTime, HostTime>(
            payloadBegin, messageEnd,
            [&sessionId](const SessionMembership& sms) { sessionId = sms.sessionId; },
            [&ghostTime](GHostTime gt) { ghostTime = std::move(gt.time); },
            [&prevGHostTime](PrevGHostTime gt) { prevGHostTime = std::move(gt.time); },
            [&prevHostTime](HostTime ht) { prevHostTime = std::move(ht.time); });
        }
        catch (const std::runtime_error& err)
        {
          warning(mLog) << "Failed parsing payload, caught exception: " << err.what();
          listen();
          return;
        }

        if (mSessionId == sessionId)
        {
          const auto hostTime = mClock.micros();

          const auto payload =
            discovery::makePayload(HostTime{hostTime}, PrevGHostTime{ghostTime});

          sendPing(from, payload);
          listen();

          if (prevGHostTime != Micros{0})
          {
            mData.push_back(
              std::make_pair(static_cast<double>((hostTime + prevHostTime).count()) * 0.5,
                static_cast<double>(ghostTime.count())));
            mData.push_back(std::make_pair(static_cast<double>(prevHostTime.count()),
              static_cast<double>((ghostTime + prevGHostTime).count()) * 0.5));
          }

          if (mData.size() > kNumberDataPoints)
          {
            finish();
          }
          else
          {
            resetTimer();
          }
        }
        else
        {
          fail();
        }
      }
      else
      {
        debug(mLog) << "Received invalid message from " << from;
        listen();
      }
    }

    template <typename Payload>
    void sendPing(asio::ip::udp::endpoint to, const Payload& payload)
    {
      v1::MessageBuffer buffer;
      const auto msgBegin = std::begin(buffer);
      const auto msgEnd = v1::pingMessage(payload, msgBegin);
      const auto numBytes = static_cast<size_t>(std::distance(msgBegin, msgEnd));

      try
      {
        mSocket.send(buffer.data(), numBytes, to);
      }
      catch (const std::runtime_error& err)
      {
        info(mLog) << "Failed to send Ping to " << to.address().to_string() << ": "
                   << err.what();
      }
    }

    void finish()
    {
      mTimer.cancel();
      mCallback(std::move(mData));
      mData = {};
      mSuccess = true;
      debug(mLog) << "Measuring " << mEndpoint << " done.";
    }

    void fail()
    {
      mCallback(std::vector<Point>{});
      mData = {};
      debug(mLog) << "Measuring " << mEndpoint << " failed.";
    }

    Socket mSocket;
    SessionId mSessionId;
    asio::ip::udp::endpoint mEndpoint;
    std::vector<std::pair<double, double>> mData;
    Callback mCallback;
    Clock mClock;
    Timer mTimer;
    std::size_t mMeasurementsStarted;
    Log mLog;
    bool mSuccess;
  };

  struct ImplDeleter
  {
    ImplDeleter(Measurement& measurement)
      : mpImpl(std::move(measurement.mpImpl))
    {
    }

    void operator()()
    {
      // Notify callback that the measurement has failed if it did
      // not succeed before destruction
      if (!mpImpl->mSuccess)
      {
        mpImpl->fail();
      }
      mpImpl.reset();
    }

    std::shared_ptr<Impl> mpImpl;
  };

  IoContext mIo;
  std::shared_ptr<Impl> mpImpl;
};

} // namespace link
} // namespace ableton
