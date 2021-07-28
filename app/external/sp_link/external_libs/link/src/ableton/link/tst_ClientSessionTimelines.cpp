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

#include <ableton/link/ClientSessionTimelines.hpp>
#include <ableton/test/CatchWrapper.hpp>

namespace ableton
{
namespace link
{
namespace
{
using std::chrono::microseconds;

// Make the constants non-zero to make sure we're not hiding bugs
const auto b0 = Beats{-1.};
const auto t0 = microseconds{-1};
const auto xform = GhostXForm{1., microseconds{-1000000}};
} // namespace

TEST_CASE("session->client | UpdatesTempo", "[ClientSessionTimelines]")
{
  const auto curClient = Timeline{Tempo{60.}, b0, t0};
  const auto session = Timeline{Tempo{90.}, b0, xform.hostToGhost(t0)};
  const auto newClient = updateClientTimelineFromSession(curClient, session, t0, xform);
  CHECK(newClient.tempo == Tempo{90.});
}

TEST_CASE("session->client | PreservesClientBeatMagnitude", "[ClientSessionTimelines]")
{
  const auto curClient = Timeline{Tempo{60.}, b0, t0};
  const auto session = Timeline{Tempo{120.}, Beats{12.32}, microseconds{-123456789}};
  // Transition to the session timeline at t1
  const auto t1 = t0 + microseconds{5000000};
  const auto b1 = curClient.toBeats(t1);
  const auto newClient = updateClientTimelineFromSession(curClient, session, t1, xform);
  // The new client timeline should have the same magnitude as the old
  // one at the transition point
  CHECK(newClient.toBeats(t1) == b1);
  // At t1 + dt, the new tempo should be in effect
  CHECK(newClient.toBeats(t1 + microseconds{1000000}) == b1 + Beats{2.});
}

TEST_CASE("session->client | AtDifferentTimesGivesSameResult", "[ClientSessionTimelines]")
{
  const auto curClient = Timeline{Tempo{60.}, b0, t0};
  const auto session = Timeline{Tempo{120.}, Beats{-11.}, microseconds{-12356789}};

  const auto t1 = t0 + microseconds{5000000};
  const auto t2 = t1 + microseconds{6849353};

  // Once a given session timeline has been converted to a client
  // timeline, converting again from the same session at different
  // times should not change the result because the result is colinear
  // and the origin should always be anchored at the session's beat 0.
  const auto updated1 = updateClientTimelineFromSession(curClient, session, t1, xform);
  const auto updated2 = updateClientTimelineFromSession(updated1, session, t2, xform);
  CHECK(updated1 == updated2);
}

TEST_CASE("session->client | EncodesSessionOrigin", "[ClientSessionTimelines]")
{
  const auto curClient = Timeline{Tempo{60.}, b0, t0};
  const auto session = Timeline{Tempo{21.3}, Beats{-421.3}, microseconds{15003240}};
  const auto newClient =
    updateClientTimelineFromSession(curClient, session, microseconds{-1345298}, xform);
  // The new client timeline's origin should be at beat 0 on the
  // session timeline. This is how we encode the session origin in the
  // client timeline.
  CHECK(xform.hostToGhost(newClient.timeOrigin) == session.fromBeats(Beats{0.}));
}


TEST_CASE("client->session | ShiftedForward", "[ClientSessionTimeline]")
{
  const auto session = Timeline{Tempo{60.}, b0, t0};
  auto client = Timeline{Tempo{60.}, Beats{111.}, xform.ghostToHost(t0)};
  client = updateClientTimelineFromSession(client, session, t0, xform);
  // Shift the phase one beat forward
  client = shiftClientTimeline(client, Beats{1.});

  const auto newSession =
    updateSessionTimelineFromClient(session, client, xform.ghostToHost(t0), xform);
  CHECK(newSession.toBeats(t0) == b0 + Beats{1.});
}

TEST_CASE("client->session | ShiftedBackward", "[ClientSessionTimeline]")
{
  const auto session = Timeline{Tempo{60.}, b0, t0};
  auto client = Timeline{Tempo{60.}, Beats{983.}, xform.ghostToHost(t0)};
  client = updateClientTimelineFromSession(client, session, t0, xform);
  // Shift the phase one beat backward
  client = shiftClientTimeline(client, Beats{-1.});

  const auto newSession =
    updateSessionTimelineFromClient(session, client, xform.ghostToHost(t0), xform);
  CHECK(newSession.toBeats(t0) == b0 - Beats{1.});
}

TEST_CASE("session->client->session | Roundtrip", "[ClientSessionTimeline]")
{
  const auto session = Timeline{Tempo{60.}, b0, t0};
  // Initial client timeline values shouldn't matter
  auto client = Timeline{Tempo{213.5}, Beats{432.}, microseconds{5143503}};

  client = updateClientTimelineFromSession(client, session, t0, xform);
  auto newSession =
    updateSessionTimelineFromClient(session, client, microseconds{42905944}, xform);

  CHECK(session == newSession);

  // Now verify that modifying the client timeline and then routing it
  // through a session update results in the same client timeline.
  const auto updateTime = microseconds{-35023900};
  newSession = updateSessionTimelineFromClient(newSession, client, updateTime, xform);
  const auto newClient =
    updateClientTimelineFromSession(client, newSession, updateTime, xform);

  CHECK(client == newClient);
}

TEST_CASE("shiftClientTimelineOrigin | Shift", "[ClientSessionTimeline]")
{
  const auto shift = Beats{1.1};
  const auto timeline = Timeline{Tempo{60.}, b0, t0};
  CHECK(shiftClientTimeline(timeline, shift).toBeats(t0) == timeline.toBeats(t0) + shift);
  CHECK(
    shiftClientTimeline(timeline, -shift).toBeats(t0) == timeline.toBeats(t0) - shift);
}

} // namespace link
} // namespace ableton
