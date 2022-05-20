/* Copyright 2021, Ableton AG, Berlin. All rights reserved.
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

#include <abl_link.h>
#include <ableton/Link.hpp>

extern "C"
{
  abl_link abl_link_create(double bpm)
  {
    return abl_link{reinterpret_cast<void *>(new ableton::Link(bpm))};
  }

  void abl_link_destroy(abl_link link)
  {
    delete reinterpret_cast<ableton::Link *>(link.impl);
  }

  bool abl_link_is_enabled(abl_link link)
  {
    return reinterpret_cast<ableton::Link *>(link.impl)->isEnabled();
  }

  void abl_link_enable(abl_link link, bool enabled)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->enable(enabled);
  }

  bool abl_link_is_start_stop_sync_enabled(abl_link link)
  {
    return reinterpret_cast<ableton::Link *>(link.impl)->isStartStopSyncEnabled();
  }

  void abl_link_enable_start_stop_sync(abl_link link, bool enabled)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->enableStartStopSync(enabled);
  }

  uint64_t abl_link_num_peers(abl_link link)
  {
    return reinterpret_cast<ableton::Link *>(link.impl)->numPeers();
  }

  void abl_link_set_num_peers_callback(
    abl_link link, abl_link_num_peers_callback callback, void *context)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->setNumPeersCallback(
      [callback, context](
        std::size_t numPeers) { (*callback)(static_cast<uint64_t>(numPeers), context); });
  }

  void abl_link_set_tempo_callback(
    abl_link link, abl_link_tempo_callback callback, void *context)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->setTempoCallback(
      [callback, context](double tempo) { (*callback)(tempo, context); });
  }

  void abl_link_set_start_stop_callback(
    abl_link link, abl_link_start_stop_callback callback, void *context)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->setStartStopCallback(
      [callback, context](bool isPlaying) { (*callback)(isPlaying, context); });
  }

  int64_t abl_link_clock_micros(abl_link link)
  {
    return reinterpret_cast<ableton::Link *>(link.impl)->clock().micros().count();
  }

  abl_link_session_state abl_link_create_session_state(void)
  {
    return abl_link_session_state{reinterpret_cast<void *>(
      new ableton::Link::SessionState{ableton::link::ApiState{}, {}})};
  }

  void abl_link_destroy_session_state(abl_link_session_state session_state)
  {
    delete reinterpret_cast<ableton::Link::SessionState *>(session_state.impl);
  }

  void abl_link_capture_app_session_state(
    abl_link link, abl_link_session_state session_state)
  {
    *reinterpret_cast<ableton::Link::SessionState *>(session_state.impl) =
      reinterpret_cast<ableton::Link *>(link.impl)->captureAppSessionState();
  }

  void abl_link_commit_app_session_state(
    abl_link link, abl_link_session_state session_state)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->commitAppSessionState(
      *reinterpret_cast<ableton::Link::SessionState *>(session_state.impl));
  }

  void abl_link_capture_audio_session_state(
    abl_link link, abl_link_session_state session_state)
  {
    *reinterpret_cast<ableton::Link::SessionState *>(session_state.impl) =
      reinterpret_cast<ableton::Link *>(link.impl)->captureAudioSessionState();
  }

  void abl_link_commit_audio_session_state(
    abl_link link, abl_link_session_state session_state)
  {
    reinterpret_cast<ableton::Link *>(link.impl)->commitAudioSessionState(
      *reinterpret_cast<ableton::Link::SessionState *>(session_state.impl));
  }

  double abl_link_tempo(abl_link_session_state session_state)
  {
    return reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)->tempo();
  }

  void abl_link_set_tempo(
    abl_link_session_state session_state, double bpm, int64_t at_time)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->setTempo(bpm, std::chrono::microseconds{at_time});
  }

  double abl_link_beat_at_time(
    abl_link_session_state session_state, int64_t time, double quantum)
  {
    auto micros = std::chrono::microseconds{time};
    return reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->beatAtTime(micros, quantum);
  }

  double abl_link_phase_at_time(
    abl_link_session_state session_state, int64_t time, double quantum)
  {
    return reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->phaseAtTime(std::chrono::microseconds{time}, quantum);
  }

  int64_t abl_link_time_at_beat(
    abl_link_session_state session_state, double beat, double quantum)
  {
    return reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->timeAtBeat(beat, quantum)
      .count();
  }

  void abl_link_request_beat_at_time(
    abl_link_session_state session_state, double beat, int64_t time, double quantum)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->requestBeatAtTime(beat, std::chrono::microseconds{time}, quantum);
  }

  void abl_link_force_beat_at_time(
    abl_link_session_state session_state, double beat, uint64_t time, double quantum)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->forceBeatAtTime(beat, std::chrono::microseconds{time}, quantum);
  }

  void abl_link_set_is_playing(
    abl_link_session_state session_state, bool is_playing, uint64_t time)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->setIsPlaying(is_playing, std::chrono::microseconds(time));
  }

  bool abl_link_is_playing(abl_link_session_state session_state)
  {
    return reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->isPlaying();
  }

  uint64_t abl_link_time_for_is_playing(abl_link_session_state session_state)
  {
    return static_cast<uint64_t>(
      reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
        ->timeForIsPlaying()
        .count());
  }

  void abl_link_request_beat_at_start_playing_time(
    abl_link_session_state session_state, double beat, double quantum)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->requestBeatAtStartPlayingTime(beat, quantum);
  }

  void abl_link_set_is_playing_and_request_beat_at_time(
    abl_link_session_state session_state,
    bool is_playing,
    uint64_t time,
    double beat,
    double quantum)
  {
    reinterpret_cast<ableton::Link::SessionState *>(session_state.impl)
      ->setIsPlayingAndRequestBeatAtTime(
        is_playing, std::chrono::microseconds{time}, beat, quantum);
  }
}
