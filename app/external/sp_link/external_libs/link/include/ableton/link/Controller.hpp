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

#include <ableton/discovery/Service.hpp>
#include <ableton/link/ClientSessionTimelines.hpp>
#include <ableton/link/Gateway.hpp>
#include <ableton/link/GhostXForm.hpp>
#include <ableton/link/NodeState.hpp>
#include <ableton/link/Peers.hpp>
#include <ableton/link/SessionState.hpp>
#include <ableton/link/Sessions.hpp>
#include <ableton/link/StartStopState.hpp>
#include <ableton/link/TripleBuffer.hpp>
#include <condition_variable>
#include <mutex>

namespace ableton
{
namespace link
{
namespace detail
{

template <typename Clock>
GhostXForm initXForm(const Clock& clock)
{
  // Make the current time map to a ghost time of 0 with ghost time
  // increasing at the same rate as clock time
  return {1.0, -clock.micros()};
}

template <typename Clock>
inline SessionState initSessionState(const Tempo tempo, const Clock& clock)
{
  using namespace std::chrono;
  return {clampTempo(Timeline{tempo, Beats{0.}, microseconds{0}}),
    StartStopState{false, Beats{0.}, microseconds{0}}, initXForm(clock)};
}

inline ClientState initClientState(const SessionState& sessionState)
{
  const auto hostTime = sessionState.ghostXForm.ghostToHost(std::chrono::microseconds{0});
  return {
    Timeline{sessionState.timeline.tempo, sessionState.timeline.beatOrigin, hostTime},
    ClientStartStopState{sessionState.startStopState.isPlaying, hostTime, hostTime}};
}

inline RtClientState initRtClientState(const ClientState& clientState)
{
  using namespace std::chrono;
  return {
    clientState.timeline, clientState.startStopState, microseconds{0}, microseconds{0}};
}

// The timespan in which local modifications to the timeline will be
// preferred over any modifications coming from the network.
const auto kLocalModGracePeriod = std::chrono::milliseconds(1000);
const auto kRtHandlerFallbackPeriod = kLocalModGracePeriod / 2;

inline ClientStartStopState selectPreferredStartStopState(
  const ClientStartStopState currentStartStopState,
  const ClientStartStopState startStopState)
{
  return startStopState.timestamp > currentStartStopState.timestamp
           ? startStopState
           : currentStartStopState;
}

inline ClientStartStopState mapStartStopStateFromSessionToClient(
  const StartStopState& sessionStartStopState,
  const Timeline& sessionTimeline,
  const GhostXForm& xForm)
{
  const auto time =
    xForm.ghostToHost(sessionTimeline.fromBeats(sessionStartStopState.beats));
  const auto timestamp = xForm.ghostToHost(sessionStartStopState.timestamp);
  return ClientStartStopState{sessionStartStopState.isPlaying, time, timestamp};
}

inline StartStopState mapStartStopStateFromClientToSession(
  const ClientStartStopState& clientStartStopState,
  const Timeline& sessionTimeline,
  const GhostXForm& xForm)
{
  const auto sessionBeats =
    sessionTimeline.toBeats(xForm.hostToGhost(clientStartStopState.time));
  const auto timestamp = xForm.hostToGhost(clientStartStopState.timestamp);
  return StartStopState{clientStartStopState.isPlaying, sessionBeats, timestamp};
}

} // namespace detail

// function types corresponding to the Controller callback type params
using PeerCountCallback = std::function<void(std::size_t)>;
using TempoCallback = std::function<void(ableton::link::Tempo)>;
using StartStopStateCallback = std::function<void(bool)>;


// The main Link controller
template <typename PeerCountCallback,
  typename TempoCallback,
  typename StartStopStateCallback,
  typename Clock,
  typename Random,
  typename IoContext>
class Controller
{
public:
  Controller(Tempo tempo,
    PeerCountCallback peerCallback,
    TempoCallback tempoCallback,
    StartStopStateCallback startStopStateCallback,
    Clock clock)
    : mTempoCallback(std::move(tempoCallback))
    , mStartStopStateCallback(std::move(startStopStateCallback))
    , mClock(std::move(clock))
    , mNodeId(NodeId::random<Random>())
    , mSessionId(mNodeId)
    , mSessionState(detail::initSessionState(tempo, mClock))
    , mClientState(detail::initClientState(mSessionState))
    , mLastIsPlayingForStartStopStateCallback(false)
    , mRtClientState(detail::initRtClientState(mClientState.get()))
    , mHasPendingRtClientStates(false)
    , mSessionPeerCounter(*this, std::move(peerCallback))
    , mEnabled(false)
    , mStartStopSyncEnabled(false)
    , mIo(IoContext{UdpSendExceptionHandler{this}})
    , mRtClientStateSetter(*this)
    , mPeers(util::injectRef(*mIo),
        std::ref(mSessionPeerCounter),
        SessionTimelineCallback{*this},
        SessionStartStopStateCallback{*this})
    , mSessions(
        {mSessionId, mSessionState.timeline, {mSessionState.ghostXForm, mClock.micros()}},
        util::injectRef(mPeers),
        MeasurePeer{*this},
        JoinSessionCallback{*this},
        util::injectRef(*mIo),
        mClock)
    , mDiscovery(std::make_pair(NodeState{mNodeId, mSessionId, mSessionState.timeline,
                                  mSessionState.startStopState},
                   mSessionState.ghostXForm),
        GatewayFactory{*this},
        util::injectRef(*mIo))
  {
  }

  Controller(const Controller&) = delete;
  Controller(Controller&&) = delete;

  Controller& operator=(const Controller&) = delete;
  Controller& operator=(Controller&&) = delete;

  ~Controller()
  {
    std::mutex mutex;
    std::condition_variable condition;
    auto stopped = false;

    mIo->async([this, &mutex, &condition, &stopped]() {
      enable(false);
      std::unique_lock<std::mutex> lock(mutex);
      stopped = true;
      condition.notify_one();
    });

    std::unique_lock<std::mutex> lock(mutex);
    condition.wait(lock, [&stopped] { return stopped; });

    mIo->stop();
  }

  void enable(const bool bEnable)
  {
    const bool bWasEnabled = mEnabled.exchange(bEnable);
    if (bWasEnabled != bEnable)
    {
      mIo->async([this, bEnable] {
        if (bEnable)
        {
          // Process the pending client states to make sure we don't push one after we
          // have joined a running session
          mRtClientStateSetter.processPendingClientStates();
          // Always reset when first enabling to avoid hijacking
          // tempo in existing sessions
          resetState();
        }
        mDiscovery.enable(bEnable);
      });
    }
  }

  bool isEnabled() const
  {
    return mEnabled;
  }

  void enableStartStopSync(const bool bEnable)
  {
    mStartStopSyncEnabled = bEnable;
  }

  bool isStartStopSyncEnabled() const
  {
    return mStartStopSyncEnabled;
  }

  std::size_t numPeers() const
  {
    return mSessionPeerCounter.mSessionPeerCount;
  }

  // Get the current Link client state. Thread-safe but may block, so
  // it cannot be used from audio thread.
  ClientState clientState() const
  {
    return mClientState.get();
  }

  // Set the client state to be used, starting at the given time.
  // Thread-safe but may block, so it cannot be used from audio thread.
  void setClientState(IncomingClientState newClientState)
  {
    mClientState.update([&](ClientState& clientState) {
      if (newClientState.timeline)
      {
        *newClientState.timeline = clampTempo(*newClientState.timeline);
        clientState.timeline = *newClientState.timeline;
      }
      if (newClientState.startStopState)
      {
        // Prevent updating client start stop state with an outdated start stop state
        *newClientState.startStopState = detail::selectPreferredStartStopState(
          clientState.startStopState, *newClientState.startStopState);
        clientState.startStopState = *newClientState.startStopState;
      }
    });
    mIo->async([this, newClientState] { handleClientState(newClientState); });
  }

  // Non-blocking client state access for a realtime context. NOT
  // thread-safe. Must not be called from multiple threads
  // concurrently and must not be called concurrently with setClientStateRtSafe.
  ClientState clientStateRtSafe() const
  {
    // Respect the session state guard and the client state guard but don't
    // block on them. If we can't access one or both because of concurrent modification
    // we fall back to our cached version of the timeline and/or start stop state.

    if (!mHasPendingRtClientStates)
    {
      const auto now = mClock.micros();
      const auto timelineGracePeriodOver =
        now - mRtClientState.timelineTimestamp > detail::kLocalModGracePeriod;
      const auto startStopStateGracePeriodOver =
        now - mRtClientState.startStopStateTimestamp > detail::kLocalModGracePeriod;

      if (timelineGracePeriodOver || startStopStateGracePeriodOver)
      {
        const auto clientState = mClientState.getRt();

        if (timelineGracePeriodOver && clientState.timeline != mRtClientState.timeline)
        {
          mRtClientState.timeline = clientState.timeline;
        }

        if (startStopStateGracePeriodOver
            && clientState.startStopState != mRtClientState.startStopState)
        {
          mRtClientState.startStopState = clientState.startStopState;
        }
      }
    }

    return {mRtClientState.timeline, mRtClientState.startStopState};
  }

  // should only be called from the audio thread
  void setClientStateRtSafe(IncomingClientState newClientState)
  {
    if (!newClientState.timeline && !newClientState.startStopState)
    {
      return;
    }

    if (newClientState.timeline)
    {
      *newClientState.timeline = clampTempo(*newClientState.timeline);
    }

    if (newClientState.startStopState)
    {
      // Prevent updating client start stop state with an outdated start stop state
      *newClientState.startStopState = detail::selectPreferredStartStopState(
        mRtClientState.startStopState, *newClientState.startStopState);
    }

    // This flag ensures that mRtClientState is only updated after all incoming
    // client states were processed
    mHasPendingRtClientStates = true;
    mRtClientStateSetter.push(newClientState);
    const auto now = mClock.micros();
    // Cache the new timeline and StartStopState for serving back to the client
    if (newClientState.timeline)
    {
      // Cache the new timeline and StartStopState for serving back to the client
      mRtClientState.timeline = *newClientState.timeline;
      mRtClientState.timelineTimestamp = makeRtTimestamp(now);
    }
    if (newClientState.startStopState)
    {
      mRtClientState.startStopState = *newClientState.startStopState;
      mRtClientState.startStopStateTimestamp = makeRtTimestamp(now);
    }
  }

private:
  std::chrono::microseconds makeRtTimestamp(const std::chrono::microseconds now) const
  {
    return isEnabled() ? now : std::chrono::microseconds(0);
  }

  void invokeStartStopStateCallbackIfChanged()
  {
    bool shouldInvokeCallback = false;

    mClientState.update([&](ClientState& clientState) {
      shouldInvokeCallback =
        mLastIsPlayingForStartStopStateCallback != clientState.startStopState.isPlaying;
      mLastIsPlayingForStartStopStateCallback = clientState.startStopState.isPlaying;
    });

    if (shouldInvokeCallback)
    {
      mStartStopStateCallback(mLastIsPlayingForStartStopStateCallback);
    }
  }

  void updateDiscovery()
  {
    // Push the change to the discovery service
    mDiscovery.updateNodeState(
      std::make_pair(NodeState{mNodeId, mSessionId, mSessionState.timeline,
                       mSessionState.startStopState},
        mSessionState.ghostXForm));
  }

  void updateSessionTiming(Timeline newTimeline, const GhostXForm newXForm)
  {
    // Clamp the session tempo because it may slightly overshoot (999 bpm is
    // transferred as 60606 us/beat and received as 999.000999... bpm).
    newTimeline = clampTempo(newTimeline);
    const auto oldTimeline = mSessionState.timeline;
    const auto oldXForm = mSessionState.ghostXForm;

    if (oldTimeline != newTimeline || oldXForm != newXForm)
    {
      {
        std::lock_guard<std::mutex> lock(mSessionStateGuard);
        mSessionState.timeline = newTimeline;
        mSessionState.ghostXForm = newXForm;
      }

      // Update the client timeline and start stop state based on the new session timing
      mClientState.update([&](ClientState& clientState) {
        clientState.timeline = updateClientTimelineFromSession(clientState.timeline,
          mSessionState.timeline, mClock.micros(), mSessionState.ghostXForm);
        // Don't pass the start stop state to the client when start stop sync is disabled
        // or when we have a default constructed start stop state
        if (mStartStopSyncEnabled && mSessionState.startStopState != StartStopState{})
        {
          std::lock_guard<std::mutex> startStopStateLock(mSessionStateGuard);
          clientState.startStopState =
            detail::mapStartStopStateFromSessionToClient(mSessionState.startStopState,
              mSessionState.timeline, mSessionState.ghostXForm);
        }
      });

      if (oldTimeline.tempo != newTimeline.tempo)
      {
        mTempoCallback(newTimeline.tempo);
      }
    }
  }

  void handleTimelineFromSession(SessionId id, Timeline timeline)
  {
    debug(mIo->log()) << "Received timeline with tempo: " << timeline.tempo.bpm()
                      << " for session: " << id;
    updateSessionTiming(mSessions.sawSessionTimeline(std::move(id), std::move(timeline)),
      mSessionState.ghostXForm);
    updateDiscovery();
  }

  void resetSessionStartStopState()
  {
    mSessionState.startStopState = StartStopState{};
  }

  void handleStartStopStateFromSession(SessionId sessionId, StartStopState startStopState)
  {
    debug(mIo->log()) << "Received start stop state. isPlaying: "
                      << startStopState.isPlaying
                      << ", beats: " << startStopState.beats.floating()
                      << ", time: " << startStopState.timestamp.count()
                      << " for session: " << sessionId;
    if (sessionId == mSessionId
        && startStopState.timestamp > mSessionState.startStopState.timestamp)
    {
      mSessionState.startStopState = startStopState;

      // Always propagate the session start stop state so even a client that doesn't have
      // the feature enabled can function as a relay.
      updateDiscovery();

      if (mStartStopSyncEnabled)
      {
        mClientState.update([&](ClientState& clientState) {
          clientState.startStopState = detail::mapStartStopStateFromSessionToClient(
            startStopState, mSessionState.timeline, mSessionState.ghostXForm);
        });
        invokeStartStopStateCallbackIfChanged();
      }
    }
  }

  void handleClientState(const IncomingClientState clientState)
  {
    auto mustUpdateDiscovery = false;

    if (clientState.timeline)
    {
      auto sessionTimeline = updateSessionTimelineFromClient(mSessionState.timeline,
        *clientState.timeline, clientState.timelineTimestamp, mSessionState.ghostXForm);

      mSessions.resetTimeline(sessionTimeline);
      mPeers.setSessionTimeline(mSessionId, sessionTimeline);
      updateSessionTiming(std::move(sessionTimeline), mSessionState.ghostXForm);

      mustUpdateDiscovery = true;
    }

    if (mStartStopSyncEnabled && clientState.startStopState)
    {
      // Prevent updating with an outdated start stop state
      const auto newGhostTime =
        mSessionState.ghostXForm.hostToGhost(clientState.startStopState->timestamp);
      if (newGhostTime > mSessionState.startStopState.timestamp)
      {
        mClientState.update([&](ClientState& currentClientState) {
          mSessionState.startStopState =
            detail::mapStartStopStateFromClientToSession(*clientState.startStopState,
              mSessionState.timeline, mSessionState.ghostXForm);
          currentClientState.startStopState = *clientState.startStopState;
        });

        mustUpdateDiscovery = true;
      }
    }

    if (mustUpdateDiscovery)
    {
      updateDiscovery();
    }

    invokeStartStopStateCallbackIfChanged();
  }

  void handleRtClientState(IncomingClientState clientState)
  {
    mClientState.update([&](ClientState& currentClientState) {
      if (clientState.timeline)
      {
        currentClientState.timeline = *clientState.timeline;
      }
      if (clientState.startStopState)
      {
        // Prevent updating client start stop state with an outdated start stop state
        *clientState.startStopState = detail::selectPreferredStartStopState(
          currentClientState.startStopState, *clientState.startStopState);
        currentClientState.startStopState = *clientState.startStopState;
      }
    });

    handleClientState(clientState);
    mHasPendingRtClientStates = false;
  }

  void joinSession(const Session& session)
  {
    const bool sessionIdChanged = mSessionId != session.sessionId;
    mSessionId = session.sessionId;

    // Prevent passing the state of the previous session to the new one.
    if (sessionIdChanged)
    {
      mRtClientStateSetter.processPendingClientStates();
      resetSessionStartStopState();
    }

    updateSessionTiming(session.timeline, session.measurement.xform);
    updateDiscovery();

    if (sessionIdChanged)
    {
      debug(mIo->log()) << "Joining session " << session.sessionId << " with tempo "
                        << session.timeline.tempo.bpm();
      mSessionPeerCounter();
    }
  }

  void resetState()
  {
    mNodeId = NodeId::random<Random>();
    mSessionId = mNodeId;

    const auto xform = detail::initXForm(mClock);
    const auto hostTime = -xform.intercept;
    // When creating the new timeline, make it continuous by finding
    // the beat on the old session timeline corresponding to the
    // current host time and mapping it to the new ghost time
    // representation of the current host time.
    const auto newTl = Timeline{mSessionState.timeline.tempo,
      mSessionState.timeline.toBeats(mSessionState.ghostXForm.hostToGhost(hostTime)),
      xform.hostToGhost(hostTime)};

    resetSessionStartStopState();

    updateSessionTiming(newTl, xform);
    updateDiscovery();

    mSessions.resetSession({mNodeId, newTl, {xform, hostTime}});
    mPeers.resetPeers();
  }

  struct SessionTimelineCallback
  {
    void operator()(SessionId id, Timeline timeline)
    {
      mController.handleTimelineFromSession(std::move(id), std::move(timeline));
    }

    Controller& mController;
  };

  struct RtClientStateSetter
  {
    using CallbackDispatcher =
      typename IoContext::template LockFreeCallbackDispatcher<std::function<void()>,
        std::chrono::milliseconds>;

    RtClientStateSetter(Controller& controller)
      : mController(controller)
      , mCallbackDispatcher(
          [this] { mController.mIo->async([this]() { processPendingClientStates(); }); },
          detail::kRtHandlerFallbackPeriod)
    {
    }

    void push(const IncomingClientState clientState)
    {
      if (clientState.timeline)
      {
        mTimelineBuffer.write(
          std::make_pair(clientState.timelineTimestamp, *clientState.timeline));
      }

      if (clientState.startStopState)
      {
        mStartStopStateBuffer.write(*clientState.startStopState);
      }

      if (clientState.timeline || clientState.startStopState)
      {
        mCallbackDispatcher.invoke();
      }
    }

    void processPendingClientStates()
    {
      const auto clientState = buildMergedPendingClientState();
      mController.handleRtClientState(clientState);
    }

  private:
    IncomingClientState buildMergedPendingClientState()
    {
      auto clientState = IncomingClientState{};
      if (auto tl = mTimelineBuffer.readNew())
      {
        clientState.timelineTimestamp = (*tl).first;
        clientState.timeline = OptionalTimeline{(*tl).second};
      }
      if (auto sss = mStartStopStateBuffer.readNew())
      {
        clientState.startStopState = sss;
      }
      return clientState;
    }

    Controller& mController;
    // Use separate TripleBuffers for the Timeline and the StartStopState so we read the
    // latest set value from either optional.
    TripleBuffer<std::pair<std::chrono::microseconds, Timeline>> mTimelineBuffer;
    TripleBuffer<ClientStartStopState> mStartStopStateBuffer;
    CallbackDispatcher mCallbackDispatcher;
  };

  struct SessionStartStopStateCallback
  {
    void operator()(SessionId sessionId, StartStopState startStopState)
    {
      mController.handleStartStopStateFromSession(sessionId, startStopState);
    }

    Controller& mController;
  };

  struct SessionPeerCounter
  {
    SessionPeerCounter(Controller& controller, PeerCountCallback callback)
      : mController(controller)
      , mCallback(std::move(callback))
      , mSessionPeerCount(0)
    {
    }

    void operator()()
    {
      const auto count =
        mController.mPeers.uniqueSessionPeerCount(mController.mSessionId);
      const auto oldCount = mSessionPeerCount.exchange(count);
      if (oldCount != count)
      {
        if (count == 0)
        {
          // When the count goes down to zero, completely reset the
          // state, effectively founding a new session
          mController.mIo->async([this] { mController.resetState(); });
        }
        mCallback(count);
      }
    }

    Controller& mController;
    PeerCountCallback mCallback;
    std::atomic<std::size_t> mSessionPeerCount;
  };

  struct MeasurePeer
  {
    template <typename Peer, typename Handler>
    void operator()(Peer peer, Handler handler)
    {
      using It = typename Discovery::ServicePeerGateways::GatewayMap::iterator;
      using ValueType = typename Discovery::ServicePeerGateways::GatewayMap::value_type;
      mController.mDiscovery.withGateways([peer, handler](It begin, const It end) {
        const auto addr = peer.second;
        const auto it = std::find_if(
          begin, end, [&addr](const ValueType& vt) { return vt.first == addr; });
        if (it != end)
        {
          it->second->measurePeer(std::move(peer.first), std::move(handler));
        }
        else
        {
          // invoke the handler with an empty result if we couldn't
          // find the peer's gateway
          handler(GhostXForm{});
        }
      });
    }

    Controller& mController;
  };

  struct JoinSessionCallback
  {
    void operator()(Session session)
    {
      mController.joinSession(std::move(session));
    }

    Controller& mController;
  };

  using IoType = typename util::Injected<IoContext>::type;

  using ControllerPeers = Peers<IoType&,
    std::reference_wrapper<SessionPeerCounter>,
    SessionTimelineCallback,
    SessionStartStopStateCallback>;

  using ControllerGateway =
    Gateway<typename ControllerPeers::GatewayObserver, Clock, IoType&>;
  using GatewayPtr = std::shared_ptr<ControllerGateway>;

  struct GatewayFactory
  {
    GatewayPtr operator()(std::pair<NodeState, GhostXForm> state,
      util::Injected<IoType&> io,
      const asio::ip::address& addr)
    {
      if (addr.is_v4())
      {
        return GatewayPtr{new ControllerGateway{std::move(io), addr.to_v4(),
          util::injectVal(makeGatewayObserver(mController.mPeers, addr)),
          std::move(state.first), std::move(state.second), mController.mClock}};
      }
      else
      {
        throw std::runtime_error("Could not create peer gateway on non-ipV4 address");
      }
    }

    Controller& mController;
  };

  struct UdpSendExceptionHandler
  {
    using Exception = discovery::UdpSendException;

    void operator()(const Exception exception)
    {
      mpController->mIo->async([this, exception] {
        mpController->mDiscovery.repairGateway(exception.interfaceAddr);
      });
    }

    Controller* mpController;
  };

  TempoCallback mTempoCallback;
  StartStopStateCallback mStartStopStateCallback;
  Clock mClock;
  NodeId mNodeId;
  SessionId mSessionId;

  mutable std::mutex mSessionStateGuard;
  SessionState mSessionState;

  ControllerClientState mClientState;
  bool mLastIsPlayingForStartStopStateCallback;

  mutable RtClientState mRtClientState;
  std::atomic<bool> mHasPendingRtClientStates;

  SessionPeerCounter mSessionPeerCounter;

  std::atomic<bool> mEnabled;

  std::atomic<bool> mStartStopSyncEnabled;

  util::Injected<IoContext> mIo;

  RtClientStateSetter mRtClientStateSetter;

  ControllerPeers mPeers;

  using ControllerSessions = Sessions<ControllerPeers&,
    MeasurePeer,
    JoinSessionCallback,
    typename util::Injected<IoContext>::type&,
    Clock>;
  ControllerSessions mSessions;

  using Discovery = discovery::Service<std::pair<NodeState, GhostXForm>,
    GatewayFactory,
    typename util::Injected<IoContext>::type&>;
  Discovery mDiscovery;
};

} // namespace link
} // namespace ableton
