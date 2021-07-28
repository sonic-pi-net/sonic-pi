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

#include <ableton/link/GhostXForm.hpp>
#include <ableton/link/Kalman.hpp>
#include <ableton/link/LinearRegression.hpp>
#include <ableton/link/Measurement.hpp>
#include <ableton/link/PeerState.hpp>
#include <ableton/link/PingResponder.hpp>
#include <ableton/link/SessionId.hpp>
#include <ableton/link/v1/Messages.hpp>
#include <map>
#include <memory>

namespace ableton
{
namespace link
{

template <typename Clock, typename IoContext>
class MeasurementService
{
public:
  using IoType = util::Injected<IoContext>;
  using Point = std::pair<double, double>;
  using MeasurementInstance = Measurement<Clock, IoContext>;

  MeasurementService(asio::ip::address_v4 address,
    SessionId sessionId,
    GhostXForm ghostXForm,
    Clock clock,
    IoType io)
    : mClock(std::move(clock))
    , mIo(std::move(io))
    , mPingResponder(std::move(address),
        std::move(sessionId),
        std::move(ghostXForm),
        mClock,
        util::injectRef(*mIo))
  {
  }

  MeasurementService(const MeasurementService&) = delete;
  MeasurementService(MeasurementService&&) = delete;

  ~MeasurementService()
  {
    // Clear the measurement map in the IoContext so that whatever
    // cleanup code executes in response to the destruction of the
    // measurement objects still have access to the IoContext.
    mIo->async([this] { mMeasurementMap.clear(); });
  }

  void updateNodeState(const SessionId& sessionId, const GhostXForm& xform)
  {
    mPingResponder.updateNodeState(sessionId, xform);
  }

  asio::ip::udp::endpoint endpoint() const
  {
    return mPingResponder.endpoint();
  }

  // Measure the peer and invoke the handler with a GhostXForm
  template <typename Handler>
  void measurePeer(const PeerState& state, const Handler handler)
  {
    using namespace std;

    mIo->async([this, state, handler] {
      const auto nodeId = state.nodeState.nodeId;
      auto addr = mPingResponder.endpoint().address().to_v4();
      auto callback = CompletionCallback<Handler>{*this, nodeId, handler};

      try
      {

        mMeasurementMap[nodeId] =
          std::unique_ptr<MeasurementInstance>(new MeasurementInstance{
            state, std::move(callback), std::move(addr), mClock, mIo->clone()});
      }
      catch (const runtime_error& err)
      {
        info(mIo->log()) << "gateway@" + addr.to_string()
                         << " Failed to measure. Reason: " << err.what();
        handler(GhostXForm{});
      }
    });
  }

  static GhostXForm filter(
    std::vector<Point>::const_iterator begin, std::vector<Point>::const_iterator end)
  {
    using namespace std;
    using std::chrono::microseconds;

    Kalman<5> kalman;
    for (auto it = begin; it != end; ++it)
    {
      kalman.iterate(it->second - it->first);
    }

    return GhostXForm{1, microseconds(llround(kalman.getValue()))};
  }

private:
  template <typename Handler>
  struct CompletionCallback
  {
    void operator()(const std::vector<Point> data)
    {
      using namespace std;
      using std::chrono::microseconds;

      // Post this to the measurement service's IoContext so that we
      // don't delete the measurement object in its stack. Capture all
      // needed data separately from this, since this object may be
      // gone by the time the block gets executed.
      auto nodeId = mNodeId;
      auto handler = mHandler;
      auto& measurementMap = mMeasurementService.mMeasurementMap;
      mMeasurementService.mIo->async([nodeId, handler, &measurementMap, data] {
        const auto it = measurementMap.find(nodeId);
        if (it != measurementMap.end())
        {
          if (data.empty())
          {
            handler(GhostXForm{});
          }
          else
          {
            handler(MeasurementService::filter(begin(data), end(data)));
          }
          measurementMap.erase(it);
        }
      });
    }

    MeasurementService& mMeasurementService;
    NodeId mNodeId;
    Handler mHandler;
  };

  // Make sure the measurement map outlives the IoContext so that the rest of
  // the members are guaranteed to be valid when any final handlers
  // are begin run.
  using MeasurementMap = std::map<NodeId, std::unique_ptr<MeasurementInstance>>;
  MeasurementMap mMeasurementMap;
  Clock mClock;
  IoType mIo;
  PingResponder<Clock, IoContext> mPingResponder;
};

} // namespace link
} // namespace ableton
