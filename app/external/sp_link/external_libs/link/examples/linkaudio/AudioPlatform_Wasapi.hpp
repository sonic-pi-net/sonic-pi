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

#include "AudioEngine.hpp"
#include <Audioclient.h>
#include <Mmdeviceapi.h>
#include <ableton/link/HostTimeFilter.hpp>
#include <atomic>

// WARNING: This file provides an audio driver for Windows using WASAPI. This driver is
// considered experimental and has problems with low-latency playback. Please consider
// using the ASIO driver instead.

namespace ableton
{
namespace linkaudio
{

// Convenience function to look up the human-readable WinAPI error code, print it out,
// and then terminate the application.
void fatalError(HRESULT result, LPCTSTR context);

DWORD renderAudioRunloop(LPVOID);

class AudioPlatform
{
public:
  AudioPlatform(Link& link);
  ~AudioPlatform();

  DWORD audioRunloop();

  AudioEngine mEngine;

private:
  UINT32 bufferSize();

  void initialize();
  void start();

  link::HostTimeFilter<platforms::windows::Clock> mHostTimeFilter;
  double mSampleTime;

  IMMDevice* mDevice;
  IAudioClient* mAudioClient;
  IAudioRenderClient* mRenderClient;
  WAVEFORMATEX* mStreamFormat;
  HANDLE mEventHandle;
  HANDLE mAudioThreadHandle;
  std::atomic<bool> mIsRunning;
};

} // namespace linkaudio
} // namespace ableton
