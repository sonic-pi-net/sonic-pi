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

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif // __cplusplus

  /*!
   *  @discussion Each abl_link instance has its own session state which
   *  represents a beat timeline and a transport start/stop state. The
   *  timeline starts running from beat 0 at the initial tempo when
   *  constructed. The timeline always advances at a speed defined by
   *  its current tempo, even if transport is stopped. Synchronizing to the
   *  transport start/stop state of Link is optional for every peer.
   *  The transport start/stop state is only shared with other peers when
   *  start/stop synchronization is enabled.
   *
   *  An abl_link instance is initially disabled after construction, which
   *  means that it will not communicate on the network. Once enabled,
   *  an abl_link instance initiates network communication in an effort to
   *  discover other peers. When peers are discovered, they immediately
   *  become part of a shared Link session.
   *
   *  Each function documents its thread-safety and
   *  realtime-safety properties. When a function is marked thread-safe,
   *  it means it is safe to call from multiple threads
   *  concurrently. When a function is marked realtime-safe, it means that
   *  it does not block and is appropriate for use in the thread that
   *  performs audio IO.
   *
   *  One session state capture/commit function pair for use
   *  in the audio thread and one for all other application contexts is provided.
   *  In general, modifying the session state should be done in the audio
   *  thread for the most accurate timing results. The ability to modify
   *  the session state from application threads should only be used in
   *  cases where an application's audio thread is not actively running
   *  or if it doesn't generate audio at all. Modifying the Link session
   *  state from both the audio thread and an application thread
   *  concurrently is not advised and will potentially lead to unexpected
   *  behavior.
   */

  /*! @brief The representation of an abl_link instance*/
  typedef struct abl_link
  {
    void *impl;
  } abl_link;

  /*! @brief Construct a new abl_link instance with an initial tempo.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  abl_link abl_link_create(double bpm);

  /*! @brief Delete an abl_link instance.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void abl_link_destroy(abl_link link);

  /*! @brief Is Link currently enabled?
   *  Thread-safe: yes
   *  Realtime-safe: yes
   */
  bool abl_link_is_enabled(abl_link link);

  /*! @brief Enable/disable Link.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void abl_link_enable(abl_link link, bool enable);

  /*! @brief: Is start/stop synchronization enabled?
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  bool abl_link_is_start_stop_sync_enabled(abl_link link);

  /*! @brief: Enable start/stop synchronization.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void abl_link_enable_start_stop_sync(abl_link link, bool enabled);

  /*! @brief How many peers are currently connected in a Link session?
   *  Thread-safe: yes
   *  Realtime-safe: yes
   */
  uint64_t abl_link_num_peers(abl_link link);

  /*! @brief Register a callback to be notified when the number of
   *  peers in the Link session changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   */
  typedef void (*abl_link_num_peers_callback)(uint64_t num_peers, void *context);
  void abl_link_set_num_peers_callback(
    abl_link link, abl_link_num_peers_callback callback, void *context);

  /*! @brief Register a callback to be notified when the session
   *  tempo changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   */
  typedef void (*abl_link_tempo_callback)(double tempo, void *context);
  void abl_link_set_tempo_callback(
    abl_link link, abl_link_tempo_callback callback, void *context);

  /*! brief: Register a callback to be notified when the state of
   *  start/stop isPlaying changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   */
  typedef void (*abl_link_start_stop_callback)(bool is_playing, void *context);
  void abl_link_set_start_stop_callback(
    abl_link link, abl_link_start_stop_callback callback, void *context);

  /*! brief: Get the current link clock time in microseconds.
   *  Thread-safe: yes
   *  Realtime-safe: yes
   */
  int64_t abl_link_clock_micros(abl_link link);

  /*! @brief The representation of the current local state of a client in a Link Session
   *
   *  @discussion A session state represents a timeline and the start/stop
   *  state. The timeline is a representation of a mapping between time and
   *  beats for varying quanta. The start/stop state represents the user
   *  intention to start or stop transport at a specific time. Start stop
   *  synchronization is an optional feature that allows to share the user
   *  request to start or stop transport between a subgroup of peers in a
   *  Link session. When observing a change of start/stop state, audio
   *  playback of a peer should be started or stopped the same way it would
   *  have happened if the user had requested that change at the according
   *  time locally. The start/stop state can only be changed by the user.
   *  This means that the current local start/stop state persists when
   *  joining or leaving a Link session. After joining a Link session
   *  start/stop change requests will be communicated to all connected peers.
   */
  typedef struct abl_link_session_state
  {
    void *impl;
  } abl_link_session_state;

  /*! @brief Create a new session_state instance.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The session_state is to be used with the abl_link_capture... and
   *  abl_link_commit... functions to capture snapshots of the current link state and pass
   *  changes to the link session.
   */
  abl_link_session_state abl_link_create_session_state(void);

  /*! @brief Delete a session_state instance.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void abl_link_destroy_session_state(abl_link_session_state abl_link_session_state);

  /*! @brief Capture the current Link Session State from the audio thread.
   *  Thread-safe: no
   *  Realtime-safe: yes
   *
   *  @discussion This function should ONLY be called in the audio thread and must not be
   *  accessed from any other threads. After capturing the session_state holds a snapshot
   *  of the current Link Session State, so it should be used in a local scope. The
   *  session_state should not be created on the audio thread.
   */
  void abl_link_capture_audio_session_state(
    abl_link link, abl_link_session_state session_state);

  /*! @brief Commit the given Session State to the Link session from the
   *  audio thread.
   *  Thread-safe: no
   *  Realtime-safe: yes
   *
   *  @discussion This function should ONLY be called in the audio thread. The given
   *  session_state will replace the current Link state. Modifications will be
   *  communicated to other peers in the session.
   */
  void abl_link_commit_audio_session_state(
    abl_link link, abl_link_session_state session_state);

  /*! @brief Capture the current Link Session State from an application thread.
   *  Thread-safe: no
   *  Realtime-safe: yes
   *
   *  @discussion Provides a mechanism for capturing the Link Session State from an
   *  application thread (other than the audio thread). After capturing the session_state
   *  contains a snapshot of the current Link state, so it should be used in a local
   *  scope.
   */
  void abl_link_capture_app_session_state(
    abl_link link, abl_link_session_state session_state);

  /*! @brief Commit the given Session State to the Link session from an
   *  application thread.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The given session_state will replace the current Link Session State.
   *  Modifications of the Session State will be communicated to other peers in the
   *  session.
   */
  void abl_link_commit_app_session_state(
    abl_link link, abl_link_session_state session_state);

  /*! @brief: The tempo of the timeline, in Beats Per Minute.
   *
   *  @discussion This is a stable value that is appropriate for display to the user. Beat
   *  time progress will not necessarily match this tempo exactly because of clock drift
   *  compensation.
   */
  double abl_link_tempo(abl_link_session_state session_state);

  /*! @brief: Set the timeline tempo to the given bpm value, taking effect at the given
   *  time.
   */
  void abl_link_set_tempo(
    abl_link_session_state session_state, double bpm, int64_t at_time);

  /*! @brief: Get the beat value corresponding to the given time for the given quantum.
   *
   *  @discussion: The magnitude of the resulting beat value is unique to this Link
   *  client, but its phase with respect to the provided quantum is shared among all
   *  session peers. For non-negative beat values, the following property holds:
   *  fmod(beatAtTime(t, q), q) == phaseAtTime(t, q)
   */
  double abl_link_beat_at_time(
    abl_link_session_state session_state, int64_t time, double quantum);

  /*! @brief: Get the session phase at the given time for the given quantum.
   *
   *  @discussion: The result is in the interval [0, quantum). The result is equivalent to
   *  fmod(beatAtTime(t, q), q) for non-negative beat values. This function is convenient
   *  if the client application is only interested in the phase and not the beat
   *  magnitude. Also, unlike fmod, it handles negative beat values correctly.
   */
  double abl_link_phase_at_time(
    abl_link_session_state session_state, int64_t time, double quantum);

  /*! @brief: Get the time at which the given beat occurs for the given quantum.
   *
   *  @discussion: The inverse of beatAtTime, assuming a constant tempo.
   *  beatAtTime(timeAtBeat(b, q), q) === b.
   */
  int64_t abl_link_time_at_beat(
    abl_link_session_state session_state, double beat, double quantum);

  /*! @brief: Attempt to map the given beat to the given time in the context of the given
   * quantum.
   *
   *  @discussion: This function behaves differently depending on the state of the
   *  session. If no other peers are connected, then this abl_link instance is in a
   *  session by itself and is free to re-map the beat/time relationship whenever it
   *  pleases. In this case, beatAtTime(time, quantum) == beat after this funtion has been
   *  called.
   *
   *  If there are other peers in the session, this abl_link instance should not abruptly
   *  re-map the beat/time relationship in the session because that would lead to beat
   *  discontinuities among the other peers. In this case, the given beat will be mapped
   *  to the next time value greater than the given time with the same phase as the given
   *  beat.
   *
   *  This function is specifically designed to enable the concept of "quantized launch"
   *  in client applications. If there are no other peers in the session, then an event
   *  (such as starting transport) happens immediately when it is requested. If there are
   *  other peers, however, we wait until the next time at which the session phase matches
   *  the phase of the event, thereby executing the event in-phase with the other peers in
   *  the session. The client application only needs to invoke this function to achieve
   *  this behavior and should not need to explicitly check the number of peers.
   */
  void abl_link_request_beat_at_time(
    abl_link_session_state session_state, double beat, int64_t time, double quantum);

  /*! @brief: Rudely re-map the beat/time relationship for all peers in a session.
   *
   *  @discussion: DANGER: This function should only be needed in certain special
   *  circumstances. Most applications should not use it. It is very similar to
   *  requestBeatAtTime except that it does not fall back to the quantizing behavior when
   *  it is in a session with other peers. Calling this function will unconditionally map
   *  the given beat to the given time and broadcast the result to the session. This is
   *  very anti-social behavior and should be avoided.
   *
   *  One of the few legitimate uses of this function is to synchronize a Link session
   *  with an external clock source. By periodically forcing the beat/time mapping
   *  according to an external clock source, a peer can effectively bridge that clock into
   *  a Link session. Much care must be taken at the application layer when implementing
   *  such a feature so that users do not accidentally disrupt Link sessions that they may
   *  join.
   */
  void abl_link_force_beat_at_time(
    abl_link_session_state session_state, double beat, uint64_t time, double quantum);

  /*! @brief: Set if transport should be playing or stopped, taking effect at the given
   *  time.
   */
  void abl_link_set_is_playing(
    abl_link_session_state session_state, bool is_playing, uint64_t time);

  /*! @brief: Is transport playing? */
  bool abl_link_is_playing(abl_link_session_state session_state);

  /*! @brief: Get the time at which a transport start/stop occurs */
  uint64_t abl_link_time_for_is_playing(abl_link_session_state session_state);

  /*! @brief: Convenience function to attempt to map the given beat to the time
   *  when transport is starting to play in context of the given quantum.
   *  This function evaluates to a no-op if abl_link_is_playing equals false.
   */
  void abl_link_request_beat_at_start_playing_time(
    abl_link_session_state session_state, double beat, double quantum);

  /*! @brief: Convenience function to start or stop transport at a given time and attempt
   *  to map the given beat to this time in context of the given quantum. */
  void abl_link_set_is_playing_and_request_beat_at_time(
    abl_link_session_state session_state,
    bool is_playing,
    uint64_t time,
    double beat,
    double quantum);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus
