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
#include <ableton/link/SessionId.hpp>
#include <ableton/link/Timeline.hpp>

namespace ableton
{
namespace link
{

struct SessionMeasurement
{
  GhostXForm xform;
  std::chrono::microseconds timestamp;
};

struct Session
{
  SessionId sessionId;
  Timeline timeline;
  SessionMeasurement measurement;
};

template <typename Peers,
  typename MeasurePeer,
  typename JoinSessionCallback,
  typename IoContext,
  typename Clock>
class Sessions
{
public:
  using Timer = typename util::Injected<IoContext>::type::Timer;

  Sessions(Session init,
    util::Injected<Peers> peers,
    MeasurePeer measure,
    JoinSessionCallback join,
    util::Injected<IoContext> io,
    Clock clock)
    : mPeers(std::move(peers))
    , mMeasure(std::move(measure))
    , mCallback(std::move(join))
    , mCurrent(std::move(init))
    , mIo(std::move(io))
    , mTimer(mIo->makeTimer())
    , mClock(std::move(clock))
  {
  }

  void resetSession(Session session)
  {
    mCurrent = std::move(session);
    mOtherSessions.clear();
  }

  void resetTimeline(Timeline timeline)
  {
    mCurrent.timeline = std::move(timeline);
  }

  // Consider the observed session/timeline pair and return a possibly
  // new timeline that should be used going forward.
  Timeline sawSessionTimeline(SessionId sid, Timeline timeline)
  {
    using namespace std;
    if (sid == mCurrent.sessionId)
    {
      // matches our current session, update the timeline if necessary
      updateTimeline(mCurrent, std::move(timeline));
    }
    else
    {
      auto session = Session{std::move(sid), std::move(timeline), {}};
      const auto range =
        equal_range(begin(mOtherSessions), end(mOtherSessions), session, SessionIdComp{});
      if (range.first == range.second)
      {
        // brand new session, insert it into our list of known
        // sessions and launch a measurement
        launchSessionMeasurement(session);
        mOtherSessions.insert(range.first, std::move(session));
      }
      else
      {
        // we've seen this session before, update its timeline if necessary
        updateTimeline(*range.first, std::move(timeline));
      }
    }
    return mCurrent.timeline;
  }

private:
  void launchSessionMeasurement(Session& session)
  {
    using namespace std;
    auto peers = mPeers->sessionPeers(session.sessionId);
    if (!peers.empty())
    {
      // first criteria: always prefer the founding peer
      const auto it = find_if(begin(peers), end(peers),
        [&session](const Peer& peer) { return session.sessionId == peer.first.ident(); });
      // TODO: second criteria should be degree. We don't have that
      // represented yet so just use the first peer for now
      auto peer = it == end(peers) ? peers.front() : *it;
      // mark that a session is in progress by clearing out the
      // session's timestamp
      session.measurement.timestamp = {};
      mMeasure(std::move(peer), MeasurementResultsHandler{*this, session.sessionId});
    }
  }

  void handleSuccessfulMeasurement(const SessionId& id, GhostXForm xform)
  {
    using namespace std;

    debug(mIo->log()) << "Session " << id << " measurement completed with result "
                      << "(" << xform.slope << ", " << xform.intercept.count() << ")";

    auto measurement = SessionMeasurement{std::move(xform), mClock.micros()};

    if (mCurrent.sessionId == id)
    {
      mCurrent.measurement = std::move(measurement);
      mCallback(mCurrent);
    }
    else
    {
      const auto range = equal_range(
        begin(mOtherSessions), end(mOtherSessions), Session{id, {}, {}}, SessionIdComp{});

      if (range.first != range.second)
      {
        const auto SESSION_EPS = chrono::microseconds{500000};
        // should we join this session?
        const auto hostTime = mClock.micros();
        const auto curGhost = mCurrent.measurement.xform.hostToGhost(hostTime);
        const auto newGhost = measurement.xform.hostToGhost(hostTime);
        // update the measurement for the session entry
        range.first->measurement = std::move(measurement);
        // If session times too close - fall back to session id order
        const auto ghostDiff = newGhost - curGhost;
        if (ghostDiff > SESSION_EPS
            || (std::abs(ghostDiff.count()) < SESSION_EPS.count()
                 && id < mCurrent.sessionId))
        {
          // The new session wins, switch over to it
          auto current = mCurrent;
          mCurrent = std::move(*range.first);
          mOtherSessions.erase(range.first);
          // Put the old current session back into our list of known
          // sessions so that we won't re-measure it
          const auto it = upper_bound(
            begin(mOtherSessions), end(mOtherSessions), current, SessionIdComp{});
          mOtherSessions.insert(it, std::move(current));
          // And notify that we have a new session and make sure that
          // we remeasure it periodically.
          mCallback(mCurrent);
          scheduleRemeasurement();
        }
      }
    }
  }

  void scheduleRemeasurement()
  {
    // set a timer to re-measure the active session after a period
    mTimer.expires_from_now(std::chrono::microseconds{30000000});
    mTimer.async_wait([this](const typename Timer::ErrorCode e) {
      if (!e)
      {
        launchSessionMeasurement(mCurrent);
        scheduleRemeasurement();
      }
    });
  }

  void handleFailedMeasurement(const SessionId& id)
  {
    using namespace std;

    debug(mIo->log()) << "Session " << id << " measurement failed.";

    // if we failed to measure for our current session, schedule a
    // retry in the future. Otherwise, remove the session from our set
    // of known sessions (if it is seen again it will be measured as
    // if new).
    if (mCurrent.sessionId == id)
    {
      scheduleRemeasurement();
    }
    else
    {
      const auto range = equal_range(
        begin(mOtherSessions), end(mOtherSessions), Session{id, {}, {}}, SessionIdComp{});
      if (range.first != range.second)
      {
        mOtherSessions.erase(range.first);
        mPeers->forgetSession(id);
      }
    }
  }

  void updateTimeline(Session& session, Timeline timeline)
  {
    // We use beat origin magnitude to prioritize sessions.
    if (timeline.beatOrigin > session.timeline.beatOrigin)
    {
      debug(mIo->log()) << "Adopting peer timeline (" << timeline.tempo.bpm() << ", "
                        << timeline.beatOrigin.floating() << ", "
                        << timeline.timeOrigin.count() << ")";

      session.timeline = std::move(timeline);
    }
    else
    {
      debug(mIo->log()) << "Rejecting peer timeline with beat origin: "
                        << timeline.beatOrigin.floating()
                        << ". Current timeline beat origin: "
                        << session.timeline.beatOrigin.floating();
    }
  }

  struct MeasurementResultsHandler
  {
    void operator()(GhostXForm xform) const
    {
      Sessions& sessions = mSessions;
      const SessionId& sessionId = mSessionId;
      if (xform == GhostXForm{})
      {
        mSessions.mIo->async([&sessions, sessionId] {
          sessions.handleFailedMeasurement(std::move(sessionId));
        });
      }
      else
      {
        mSessions.mIo->async([&sessions, sessionId, xform] {
          sessions.handleSuccessfulMeasurement(std::move(sessionId), std::move(xform));
        });
      }
    }

    Sessions& mSessions;
    SessionId mSessionId;
  };

  struct SessionIdComp
  {
    bool operator()(const Session& lhs, const Session& rhs) const
    {
      return lhs.sessionId < rhs.sessionId;
    }
  };

  using Peer = typename util::Injected<Peers>::type::Peer;
  util::Injected<Peers> mPeers;
  MeasurePeer mMeasure;
  JoinSessionCallback mCallback;
  Session mCurrent;
  util::Injected<IoContext> mIo;
  Timer mTimer;
  Clock mClock;
  std::vector<Session> mOtherSessions; // sorted/unique by session id
};

template <typename Peers,
  typename MeasurePeer,
  typename JoinSessionCallback,
  typename IoContext,
  typename Clock>
Sessions<Peers, MeasurePeer, JoinSessionCallback, IoContext, Clock> makeSessions(
  Session init,
  util::Injected<Peers> peers,
  MeasurePeer measure,
  JoinSessionCallback join,
  util::Injected<IoContext> io,
  Clock clock)
{
  using namespace std;
  return {std::move(init), std::move(peers), std::move(measure), std::move(join), std::move(io), std::move(clock)};
}

} // namespace link
} // namespace ableton
