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

#include <ableton/Link.hpp>

namespace ableton
{
namespace linkaudio
{

class AudioPlatform
{
  class AudioEngine
  {
  public:
    AudioEngine(Link& link)
      : mLink(link)
      , mQuantum(4.)
    {
    }

    void startPlaying()
    {
      auto sessionState = mLink.captureAppSessionState();
      sessionState.setIsPlayingAndRequestBeatAtTime(true, now(), 0., mQuantum);
      mLink.commitAppSessionState(sessionState);
    }

    void stopPlaying()
    {
      auto sessionState = mLink.captureAppSessionState();
      sessionState.setIsPlaying(false, now());
      mLink.commitAppSessionState(sessionState);
    }

    bool isPlaying() const
    {
      return mLink.captureAppSessionState().isPlaying();
    }

    double beatTime() const
    {
      auto sessionState = mLink.captureAppSessionState();
      return sessionState.beatAtTime(now(), mQuantum);
    }

    void setTempo(double tempo)
    {
      auto sessionState = mLink.captureAppSessionState();
      sessionState.setTempo(tempo, now());
      mLink.commitAppSessionState(sessionState);
    }

    double quantum() const
    {
      return mQuantum;
    }

    void setQuantum(double quantum)
    {
      mQuantum = quantum;
    }

    bool isStartStopSyncEnabled() const
    {
      return mLink.isStartStopSyncEnabled();
    }

    void setStartStopSyncEnabled(bool enabled)
    {
      mLink.enableStartStopSync(enabled);
    }

  private:
    std::chrono::microseconds now() const
    {
      return mLink.clock().micros();
    }

    Link& mLink;
    double mQuantum;
  };

public:
  AudioPlatform(Link& link)
    : mEngine(link)
  {
  }

  AudioEngine mEngine;
};

} // namespace linkaudio
} // namespace ableton
