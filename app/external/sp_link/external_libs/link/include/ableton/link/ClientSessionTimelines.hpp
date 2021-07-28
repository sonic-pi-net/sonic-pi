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
#include <ableton/link/Timeline.hpp>

namespace ableton
{
namespace link
{

// Clamp the tempo of the given timeline to the valid Link range
inline Timeline clampTempo(const Timeline timeline)
{
  const double kMinBpm = 20.0;
  const double kMaxBpm = 999.0;
  return {Tempo{(std::min)((std::max)(timeline.tempo.bpm(), kMinBpm), kMaxBpm)},
    timeline.beatOrigin, timeline.timeOrigin};
}

// Given an existing client timeline, a session timeline, and the
// global host transform of the session, return a new version of the client
// timeline. The resulting new client timeline is continuous with the
// previous timeline so that curClient.toBeats(atTime) ==
// result.toBeats(atTime).
inline Timeline updateClientTimelineFromSession(const Timeline curClient,
  const Timeline session,
  const std::chrono::microseconds atTime,
  const GhostXForm xform)
{
  // An intermediate timeline representing the continuation of the
  // existing client timeline with the tempo from the session timeline.
  const auto tempTl = Timeline{session.tempo, curClient.toBeats(atTime), atTime};
  // The host time corresponding to beat 0 on the session
  // timeline. Beat 0 on the session timeline is important because it
  // serves as the origin of the quantization grid for all participants.
  const auto hostBeatZero = xform.ghostToHost(session.fromBeats(Beats{INT64_C(0)}));
  // The new client timeline becomes the result of sliding the
  // intermediate timeline back so that it's anchor corresponds to
  // beat zero on the session timeline. The result preserves the
  // magnitude of beats on the client timeline while encoding the
  // quantization reference point in the time and beatOrigins.
  return {tempTl.tempo, tempTl.toBeats(hostBeatZero), hostBeatZero};
}


inline Timeline updateSessionTimelineFromClient(const Timeline curSession,
  const Timeline client,
  const std::chrono::microseconds atTime,
  const GhostXForm xform)
{
  // The client timeline was constructed so that it's timeOrigin
  // corresponds to beat 0 on the session timeline.
  const auto ghostBeat0 = xform.hostToGhost(client.timeOrigin);

  const auto zero = Beats{INT64_C(0)};
  // If beat 0 was not shifted and there is not a new tempo, an update
  // of the session timeline is not required. Don't create an
  // equivalent timeline with different anchor points if not needed as
  // this will trigger other unnecessary changes.
  if (curSession.toBeats(ghostBeat0) == zero && client.tempo == curSession.tempo)
  {
    return curSession;
  }
  else
  {
    // An intermediate timeline representing the new tempo, the
    // effective time, and a possibly adjusted origin.
    const auto tempTl = Timeline{client.tempo, zero, ghostBeat0};
    // The final session timeline must have the beat corresponding to
    // atTime on the old session timeline as its beatOrigin because this is
    // used for prioritization of timelines among peers - we can't let a
    // modification applied by the client artificially increase or
    // reduce the timeline's priority in the session. The new beat
    // origin should be as close as possible to lining up with atTime,
    // but we must also make sure that it's > curSession.beatOrigin
    // because otherwise it will get ignored.
    const auto newBeatOrigin = (std::max)(curSession.toBeats(xform.hostToGhost(atTime)),
      curSession.beatOrigin + Beats{INT64_C(1)});
    return {client.tempo, newBeatOrigin, tempTl.fromBeats(newBeatOrigin)};
  }
}

// Shift timeline so result.toBeats(t) == client.toBeats(t) +
// shift. This takes into account the fact that the timeOrigin
// corresponds to beat 0 on the session timeline. Using this function
// and then setting the session timeline with the result will change
// the phase of the session by the given shift amount.
inline Timeline shiftClientTimeline(Timeline client, const Beats shift)
{
  const auto timeDelta = client.fromBeats(shift) - client.fromBeats(Beats{INT64_C(0)});
  client.timeOrigin = client.timeOrigin - timeDelta;
  return client;
}

} // namespace link
} // namespace ableton
