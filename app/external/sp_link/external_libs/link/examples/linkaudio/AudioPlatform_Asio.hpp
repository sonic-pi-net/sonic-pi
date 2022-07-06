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

#include <ableton/Link.hpp>
#include <ableton/link/HostTimeFilter.hpp>

#include "asiosys.h" // Should be included before asio.h

#include "asio.h"
#include "asiodrivers.h"

// External functions in the ASIO SDK which aren't declared in the SDK headers
extern AsioDrivers* asioDrivers;
bool loadAsioDriver(char* name);

namespace ableton
{
namespace linkaudio
{

#ifndef LINK_ASIO_DRIVER_NAME
#define LINK_ASIO_DRIVER_NAME "ASIO4ALL v2"
#endif
#ifndef LINK_ASIO_INPUT_CHANNELS
#define LINK_ASIO_INPUT_CHANNELS 0
#endif
#ifndef LINK_ASIO_OUTPUT_CHANNELS
#define LINK_ASIO_OUTPUT_CHANNELS 2
#endif

struct DriverInfo
{
  ASIODriverInfo driverInfo;
  long inputChannels;
  long outputChannels;
  long preferredSize;
  ASIOSampleRate sampleRate;
  bool outputReady;
  long numBuffers;
  ASIOBufferInfo bufferInfos[LINK_ASIO_INPUT_CHANNELS + LINK_ASIO_OUTPUT_CHANNELS];
  ASIOChannelInfo channelInfos[LINK_ASIO_INPUT_CHANNELS + LINK_ASIO_OUTPUT_CHANNELS];
};

// Helper functions

// Convenience function to print out an ASIO error code along with the function called
void fatalError(const ASIOError result, const std::string& function);
double asioSamplesToDouble(const ASIOSamples& samples);

ASIOTime* bufferSwitchTimeInfo(ASIOTime* timeInfo, long index, ASIOBool);
void bufferSwitch(long index, ASIOBool processNow);

class AudioPlatform
{
public:
  AudioPlatform(Link& link);
  ~AudioPlatform();

  void audioCallback(ASIOTime* timeInfo, long index);

  AudioEngine mEngine;

  // Unfortunately, the ASIO SDK does not allow passing void* user data to callback
  // functions, so we need to keep a singleton instance of the audio engine
  static AudioPlatform* singleton();
  static void setSingleton(AudioPlatform* platform);

private:
  void createAsioBuffers();
  void initializeDriverInfo();
  void initialize();
  void start();
  void stop();

  DriverInfo mDriverInfo;
  ASIOCallbacks mAsioCallbacks;
  link::HostTimeFilter<platforms::windows::Clock> mHostTimeFilter;

  static AudioPlatform* _singleton;
};

} // namespace linkaudio
} // namespace ableton
