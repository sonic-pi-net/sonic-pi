/*! @file Link.hpp
 *  @copyright 2016, Ableton AG, Berlin. All rights reserved.
 *  @brief Library for cross-device shared tempo and quantized beat grid
 *
 *  @license:
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

#include <ableton/platforms/Config.hpp>
#include <chrono>
#include <mutex>

namespace ableton
{

/*! @class Link and BasicLink
 *  @brief Classes representing a participant in a Link session.
 *  The BasicLink type allows to customize the clock. The Link type
 *  uses the recommended platform-dependent representation of the
 *  system clock as defined in platforms/Config.hpp.
 *  It's preferred to use Link instead of BasicLink.
 *
 *  @discussion Each Link instance has its own session state which
 *  represents a beat timeline and a transport start/stop state. The
 *  timeline starts running from beat 0 at the initial tempo when
 *  constructed. The timeline always advances at a speed defined by
 *  its current tempo, even if transport is stopped. Synchronizing to the
 *  transport start/stop state of Link is optional for every peer.
 *  The transport start/stop state is only shared with other peers when
 *  start/stop synchronization is enabled.
 *
 *  A Link instance is initially disabled after construction, which
 *  means that it will not communicate on the network. Once enabled,
 *  a Link instance initiates network communication in an effort to
 *  discover other peers. When peers are discovered, they immediately
 *  become part of a shared Link session.
 *
 *  Each method of the Link type documents its thread-safety and
 *  realtime-safety properties. When a method is marked thread-safe,
 *  it means it is safe to call from multiple threads
 *  concurrently. When a method is marked realtime-safe, it means that
 *  it does not block and is appropriate for use in the thread that
 *  performs audio IO.
 *
 *  Link provides one session state capture/commit method pair for use
 *  in the audio thread and one for all other application contexts. In
 *  general, modifying the session state should be done in the audio
 *  thread for the most accurate timing results. The ability to modify
 *  the session state from application threads should only be used in
 *  cases where an application's audio thread is not actively running
 *  or if it doesn't generate audio at all. Modifying the Link session
 *  state from both the audio thread and an application thread
 *  concurrently is not advised and will potentially lead to unexpected
 *  behavior.
 *
 *  Only use the BasicLink class if the default platform clock does not
 *  fulfill other requirements of the client application. Please note this
 *  will require providing a custom Clock implementation. See the clock()
 *  documentation for details.
 */
template <typename Clock>
class BasicLink
{
public:
  class SessionState;

  /*! @brief Construct with an initial tempo. */
  BasicLink(double bpm);

  /*! @brief Link instances cannot be copied or moved */
  BasicLink(const BasicLink<Clock>&) = delete;
  BasicLink& operator=(const BasicLink<Clock>&) = delete;
  BasicLink(BasicLink<Clock>&&) = delete;
  BasicLink& operator=(BasicLink<Clock>&&) = delete;

  /*! @brief Is Link currently enabled?
   *  Thread-safe: yes
   *  Realtime-safe: yes
   */
  bool isEnabled() const;

  /*! @brief Enable/disable Link.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void enable(bool bEnable);

  /*! @brief: Is start/stop synchronization enabled?
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  bool isStartStopSyncEnabled() const;

  /*! @brief: Enable start/stop synchronization.
   *  Thread-safe: yes
   *  Realtime-safe: no
   */
  void enableStartStopSync(bool bEnable);

  /*! @brief How many peers are currently connected in a Link session?
   *  Thread-safe: yes
   *  Realtime-safe: yes
   */
  std::size_t numPeers() const;

  /*! @brief Register a callback to be notified when the number of
   *  peers in the Link session changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   *
   *  @param callback The callback signature is:
   *  void (std::size_t numPeers)
   */
  template <typename Callback>
  void setNumPeersCallback(Callback callback);

  /*! @brief Register a callback to be notified when the session
   *  tempo changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   *
   *  @param callback The callback signature is: void (double bpm)
   */
  template <typename Callback>
  void setTempoCallback(Callback callback);

  /*! brief: Register a callback to be notified when the state of
   *  start/stop isPlaying changes.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The callback is invoked on a Link-managed thread.
   *
   *  @param callback The callback signature is:
   *  void (bool isPlaying)
   */
  template <typename Callback>
  void setStartStopCallback(Callback callback);

  /*! @brief The clock used by Link.
   *  Thread-safe: yes
   *  Realtime-safe: yes
   *
   *  @discussion The Clock type is a platform-dependent representation
   *  of the system clock. It exposes a micros() method, which is a
   *  normalized representation of the current system time in
   *  std::chrono::microseconds.
   */
  Clock clock() const;

  /*! @brief Capture the current Link Session State from the audio thread.
   *  Thread-safe: no
   *  Realtime-safe: yes
   *
   *  @discussion This method should ONLY be called in the audio thread
   *  and must not be accessed from any other threads. The returned
   *  object stores a snapshot of the current Link Session State, so it
   *  should be captured and used in a local scope. Storing the
   *  Session State for later use in a different context is not advised
   *  because it will provide an outdated view.
   */
  SessionState captureAudioSessionState() const;

  /*! @brief Commit the given Session State to the Link session from the
   *  audio thread.
   *  Thread-safe: no
   *  Realtime-safe: yes
   *
   *  @discussion This method should ONLY be called in the audio
   *  thread. The given Session State will replace the current Link
   *  state. Modifications will be communicated to other peers in the
   *  session.
   */
  void commitAudioSessionState(SessionState state);

  /*! @brief Capture the current Link Session State from an application
   *  thread.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion Provides a mechanism for capturing the Link Session
   *  State from an application thread (other than the audio thread).
   *  The returned Session State stores a snapshot of the current Link
   *  state, so it should be captured and used in a local scope.
   *  Storing the it for later use in a different context is not
   *  advised because it will provide an outdated view.
   */
  SessionState captureAppSessionState() const;

  /*! @brief Commit the given Session State to the Link session from an
   *  application thread.
   *  Thread-safe: yes
   *  Realtime-safe: no
   *
   *  @discussion The given Session State will replace the current Link
   *  Session State. Modifications of the Session State will be
   *  communicated to other peers in the session.
   */
  void commitAppSessionState(SessionState state);

  /*! @class SessionState
   *  @brief Representation of a timeline and the start/stop state
   *
   *  @discussion A SessionState object is intended for use in a local scope within
   *  a single thread - none of its methods are thread-safe. All of its methods are
   *  non-blocking, so it is safe to use from a realtime thread.
   *  It provides functions to observe and manipulate the timeline and start/stop
   *  state.
   *
   *  The timeline is a representation of a mapping between time and beats for varying
   *  quanta.
   *  The start/stop state represents the user intention to start or stop transport at
   *  a specific time. Start stop synchronization is an optional feature that allows to
   *  share the user request to start or stop transport between a subgroup of peers in
   *  a Link session. When observing a change of start/stop state, audio playback of a
   *  peer should be started or stopped the same way it would have happened if the user
   *  had requested that change at the according time locally. The start/stop state can
   *  only be changed by the user. This means that the current local start/stop state
   *  persists when joining or leaving a Link session. After joining a Link session
   *  start/stop change requests will be communicated to all connected peers.
   */
  class SessionState
  {
  public:
    SessionState(const link::ApiState state, const bool bRespectQuantum);

    /*! @brief: The tempo of the timeline, in Beats Per Minute.
     *
     *  @discussion This is a stable value that is appropriate for display
     *  to the user. Beat time progress will not necessarily match this tempo
     *  exactly because of clock drift compensation.
     */
    double tempo() const;

    /*! @brief: Set the timeline tempo to the given bpm value, taking
     *  effect at the given time.
     */
    void setTempo(double bpm, std::chrono::microseconds atTime);

    /*! @brief: Get the beat value corresponding to the given time
     *  for the given quantum.
     *
     *  @discussion: The magnitude of the resulting beat value is
     *  unique to this Link instance, but its phase with respect to
     *  the provided quantum is shared among all session
     *  peers. For non-negative beat values, the following
     *  property holds: fmod(beatAtTime(t, q), q) == phaseAtTime(t, q)
     */
    double beatAtTime(std::chrono::microseconds time, double quantum) const;

    /*! @brief: Get the session phase at the given time for the given
     *  quantum.
     *
     *  @discussion: The result is in the interval [0, quantum). The
     *  result is equivalent to fmod(beatAtTime(t, q), q) for
     *  non-negative beat values. This method is convenient if the
     *  client is only interested in the phase and not the beat
     *  magnitude. Also, unlike fmod, it handles negative beat values
     *  correctly.
     */
    double phaseAtTime(std::chrono::microseconds time, double quantum) const;

    /*! @brief: Get the time at which the given beat occurs for the
     *  given quantum.
     *
     *  @discussion: The inverse of beatAtTime, assuming a constant
     *  tempo. beatAtTime(timeAtBeat(b, q), q) === b.
     */
    std::chrono::microseconds timeAtBeat(double beat, double quantum) const;

    /*! @brief: Attempt to map the given beat to the given time in the
     *  context of the given quantum.
     *
     *  @discussion: This method behaves differently depending on the
     *  state of the session. If no other peers are connected,
     *  then this instance is in a session by itself and is free to
     *  re-map the beat/time relationship whenever it pleases. In this
     *  case, beatAtTime(time, quantum) == beat after this method has
     *  been called.
     *
     *  If there are other peers in the session, this instance
     *  should not abruptly re-map the beat/time relationship in the
     *  session because that would lead to beat discontinuities among
     *  the other peers. In this case, the given beat will be mapped
     *  to the next time value greater than the given time with the
     *  same phase as the given beat.
     *
     *  This method is specifically designed to enable the concept of
     *  "quantized launch" in client applications. If there are no other
     *  peers in the session, then an event (such as starting
     *  transport) happens immediately when it is requested. If there
     *  are other peers, however, we wait until the next time at which
     *  the session phase matches the phase of the event, thereby
     *  executing the event in-phase with the other peers in the
     *  session. The client only needs to invoke this method to
     *  achieve this behavior and should not need to explicitly check
     *  the number of peers.
     */
    void requestBeatAtTime(double beat, std::chrono::microseconds time, double quantum);

    /*! @brief: Rudely re-map the beat/time relationship for all peers
     *  in a session.
     *
     *  @discussion: DANGER: This method should only be needed in
     *  certain special circumstances. Most applications should not
     *  use it. It is very similar to requestBeatAtTime except that it
     *  does not fall back to the quantizing behavior when it is in a
     *  session with other peers. Calling this method will
     *  unconditionally map the given beat to the given time and
     *  broadcast the result to the session. This is very anti-social
     *  behavior and should be avoided.
     *
     *  One of the few legitimate uses of this method is to
     *  synchronize a Link session with an external clock source. By
     *  periodically forcing the beat/time mapping according to an
     *  external clock source, a peer can effectively bridge that
     *  clock into a Link session. Much care must be taken at the
     *  application layer when implementing such a feature so that
     *  users do not accidentally disrupt Link sessions that they may
     *  join.
     */
    void forceBeatAtTime(double beat, std::chrono::microseconds time, double quantum);

    /*! @brief: Set if transport should be playing or stopped, taking effect
     *  at the given time.
     */
    void setIsPlaying(bool isPlaying, std::chrono::microseconds time);

    /*! @brief: Is transport playing? */
    bool isPlaying() const;

    /*! @brief: Get the time at which a transport start/stop occurs */
    std::chrono::microseconds timeForIsPlaying() const;

    /*! @brief: Convenience function to attempt to map the given beat to the time
     *  when transport is starting to play in context of the given quantum.
     *  This function evaluates to a no-op if isPlaying() equals false.
     */
    void requestBeatAtStartPlayingTime(double beat, double quantum);

    /*! @brief: Convenience function to start or stop transport at a given time and
     *  attempt to map the given beat to this time in context of the given quantum.
     */
    void setIsPlayingAndRequestBeatAtTime(
      bool isPlaying, std::chrono::microseconds time, double beat, double quantum);

  private:
    friend BasicLink<Clock>;
    link::ApiState mOriginalState;
    link::ApiState mState;
    bool mbRespectQuantum;
  };

private:
  using Controller = ableton::link::Controller<
    link::PeerCountCallback,
    link::TempoCallback,
    link::StartStopStateCallback,
    Clock,
    link::platform::Random,
    link::platform::IoContext>;

  std::mutex mCallbackMutex;
  link::PeerCountCallback mPeerCountCallback = [](std::size_t) {};
  link::TempoCallback mTempoCallback = [](link::Tempo) {};
  link::StartStopStateCallback mStartStopCallback = [](bool) {};
  Clock mClock;
  Controller mController;
};

class Link : public BasicLink<link::platform::Clock>
{
public:
  using Clock = link::platform::Clock;

  Link(double bpm)
    : BasicLink(bpm)
  {
  }
};

} // namespace ableton

#include <ableton/Link.ipp>
