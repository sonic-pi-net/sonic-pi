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

#include <ableton/link/Phase.hpp>

namespace ableton
{
namespace detail
{

template <typename Clock>
inline typename BasicLink<Clock>::SessionState toSessionState(
  const link::ClientState& state, const bool isConnected)
{
  return {{state.timeline, {state.startStopState.isPlaying, state.startStopState.time}},
    isConnected};
}

inline link::IncomingClientState toIncomingClientState(const link::ApiState& state,
  const link::ApiState& originalState,
  const std::chrono::microseconds timestamp)
{
  const auto timeline = originalState.timeline != state.timeline
                          ? link::OptionalTimeline{state.timeline}
                          : link::OptionalTimeline{};
  const auto startStopState =
    originalState.startStopState != state.startStopState
      ? link::OptionalClientStartStopState{{state.startStopState.isPlaying,
        state.startStopState.time, timestamp}}
      : link::OptionalClientStartStopState{};
  return {timeline, startStopState, timestamp};
}

} // namespace detail

template <typename Clock>
inline BasicLink<Clock>::BasicLink(const double bpm)
  : mController(
      link::Tempo(bpm),
      [this](const std::size_t peers) {
        std::lock_guard<std::mutex> lock(mCallbackMutex);
        mPeerCountCallback(peers);
      },
      [this](const link::Tempo tempo) {
        std::lock_guard<std::mutex> lock(mCallbackMutex);
        mTempoCallback(tempo);
      },
      [this](const bool isPlaying) {
        std::lock_guard<std::mutex> lock(mCallbackMutex);
        mStartStopCallback(isPlaying);
      },
      mClock,
      util::injectVal(link::platform::IoContext{}))
{
}

template <typename Clock>
inline bool BasicLink<Clock>::isEnabled() const
{
  return mController.isEnabled();
}

template <typename Clock>
inline void BasicLink<Clock>::enable(const bool bEnable)
{
  mController.enable(bEnable);
}

template <typename Clock>
inline bool BasicLink<Clock>::isStartStopSyncEnabled() const
{
  return mController.isStartStopSyncEnabled();
}

template <typename Clock>
inline void BasicLink<Clock>::enableStartStopSync(bool bEnable)
{
  mController.enableStartStopSync(bEnable);
}

template <typename Clock>
inline std::size_t BasicLink<Clock>::numPeers() const
{
  return mController.numPeers();
}

template <typename Clock>
template <typename Callback>
void BasicLink<Clock>::setNumPeersCallback(Callback callback)
{
  std::lock_guard<std::mutex> lock(mCallbackMutex);
  mPeerCountCallback = [callback](const std::size_t numPeers) { callback(numPeers); };
}

template <typename Clock>
template <typename Callback>
void BasicLink<Clock>::setTempoCallback(Callback callback)
{
  std::lock_guard<std::mutex> lock(mCallbackMutex);
  mTempoCallback = [callback](const link::Tempo tempo) { callback(tempo.bpm()); };
}

template <typename Clock>
template <typename Callback>
void BasicLink<Clock>::setStartStopCallback(Callback callback)
{
  std::lock_guard<std::mutex> lock(mCallbackMutex);
  mStartStopCallback = callback;
}

template <typename Clock>
inline Clock BasicLink<Clock>::clock() const
{
  return mClock;
}

template <typename Clock>
inline typename BasicLink<Clock>::SessionState BasicLink<Clock>::captureAudioSessionState() const
{
  return detail::toSessionState<Clock>(mController.clientStateRtSafe(), numPeers() > 0);
}

template <typename Clock>
inline void BasicLink<Clock>::commitAudioSessionState(const typename BasicLink<Clock>::SessionState state)
{
  mController.setClientStateRtSafe(
    detail::toIncomingClientState(state.mState, state.mOriginalState, mClock.micros()));
}

template <typename Clock>
inline typename BasicLink<Clock>::SessionState BasicLink<Clock>::captureAppSessionState() const
{
  return detail::toSessionState<Clock>(mController.clientState(), numPeers() > 0);
}

template <typename Clock>
inline void BasicLink<Clock>::commitAppSessionState(const typename BasicLink<Clock>::SessionState state)
{
  mController.setClientState(
    detail::toIncomingClientState(state.mState, state.mOriginalState, mClock.micros()));
}

// Link::SessionState

template <typename Clock>
inline BasicLink<Clock>::SessionState::SessionState(
  const link::ApiState state, const bool bRespectQuantum)
  : mOriginalState(state)
  , mState(state)
  , mbRespectQuantum(bRespectQuantum)
{
}

template <typename Clock>
inline double BasicLink<Clock>::SessionState::tempo() const
{
  return mState.timeline.tempo.bpm();
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::setTempo(
  const double bpm, const std::chrono::microseconds atTime)
{
  const auto desiredTl = link::clampTempo(
    link::Timeline{link::Tempo(bpm), mState.timeline.toBeats(atTime), atTime});
  mState.timeline.tempo = desiredTl.tempo;
  mState.timeline.timeOrigin = desiredTl.fromBeats(mState.timeline.beatOrigin);
}

template <typename Clock>
inline double BasicLink<Clock>::SessionState::beatAtTime(
  const std::chrono::microseconds time, const double quantum) const
{
  return link::toPhaseEncodedBeats(mState.timeline, time, link::Beats{quantum})
    .floating();
}

template <typename Clock>
inline double BasicLink<Clock>::SessionState::phaseAtTime(
  const std::chrono::microseconds time, const double quantum) const
{
  return link::phase(link::Beats{beatAtTime(time, quantum)}, link::Beats{quantum})
    .floating();
}

template <typename Clock>
inline std::chrono::microseconds BasicLink<Clock>::SessionState::timeAtBeat(
  const double beat, const double quantum) const
{
  return link::fromPhaseEncodedBeats(
    mState.timeline, link::Beats{beat}, link::Beats{quantum});
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::requestBeatAtTime(
  const double beat, std::chrono::microseconds time, const double quantum)
{
  if (mbRespectQuantum)
  {
    time = timeAtBeat(link::nextPhaseMatch(link::Beats{beatAtTime(time, quantum)},
                        link::Beats{beat}, link::Beats{quantum})
                        .floating(),
      quantum);
  }
  forceBeatAtTime(beat, time, quantum);
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::forceBeatAtTime(
  const double beat, const std::chrono::microseconds time, const double quantum)
{
  // There are two components to the beat adjustment: a phase shift
  // and a beat magnitude adjustment.
  const auto curBeatAtTime = link::Beats{beatAtTime(time, quantum)};
  const auto closestInPhase =
    link::closestPhaseMatch(curBeatAtTime, link::Beats{beat}, link::Beats{quantum});
  mState.timeline = shiftClientTimeline(mState.timeline, closestInPhase - curBeatAtTime);
  // Now adjust the magnitude
  mState.timeline.beatOrigin =
    mState.timeline.beatOrigin + (link::Beats{beat} - closestInPhase);
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::setIsPlaying(
  const bool isPlaying, const std::chrono::microseconds time)
{
  mState.startStopState = {isPlaying, time};
}

template <typename Clock>
inline bool BasicLink<Clock>::SessionState::isPlaying() const
{
  return mState.startStopState.isPlaying;
}

template <typename Clock>
inline std::chrono::microseconds BasicLink<Clock>::SessionState::timeForIsPlaying() const
{
  return mState.startStopState.time;
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::requestBeatAtStartPlayingTime(
  const double beat, const double quantum)
{
  if (isPlaying())
  {
    requestBeatAtTime(beat, mState.startStopState.time, quantum);
  }
}

template <typename Clock>
inline void BasicLink<Clock>::SessionState::setIsPlayingAndRequestBeatAtTime(
  bool isPlaying, std::chrono::microseconds time, double beat, double quantum)
{
  mState.startStopState = {isPlaying, time};
  requestBeatAtStartPlayingTime(beat, quantum);
}

} // namespace ableton
