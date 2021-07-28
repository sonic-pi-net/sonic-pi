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

#include "AudioPlatform_CoreAudio.hpp"
#include <chrono>
#include <iostream>
#include <mach/mach_time.h>

namespace ableton
{
namespace linkaudio
{

AudioPlatform::AudioPlatform(Link& link)
  : mEngine(link)
{
  initialize();
  start();
}

AudioPlatform::~AudioPlatform()
{
  stop();
  uninitialize();
}

OSStatus AudioPlatform::audioCallback(void* inRefCon,
  AudioUnitRenderActionFlags*,
  const AudioTimeStamp* inTimeStamp,
  UInt32,
  UInt32 inNumberFrames,
  AudioBufferList* ioData)
{
  AudioEngine* engine = static_cast<AudioEngine*>(inRefCon);

  const auto bufferBeginAtOutput =
    engine->mLink.clock().ticksToMicros(inTimeStamp->mHostTime) + engine->mOutputLatency.load();

  engine->audioCallback(bufferBeginAtOutput, inNumberFrames);

  for (std::size_t i = 0; i < inNumberFrames; ++i)
  {
    for (UInt32 j = 0; j < ioData->mNumberBuffers; ++j)
    {
      SInt16* bufData = static_cast<SInt16*>(ioData->mBuffers[j].mData);
      bufData[i] = static_cast<SInt16>(32761. * engine->mBuffer[i]);
    }
  }

  return noErr;
}

void AudioPlatform::initialize()
{
  AudioComponentDescription cd = {};
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags = 0;
  cd.componentFlagsMask = 0;
  cd.componentType = kAudioUnitType_Output;
  cd.componentSubType = kAudioUnitSubType_DefaultOutput;

  AudioComponent component = AudioComponentFindNext(nullptr, &cd);
  OSStatus result = AudioComponentInstanceNew(component, &mIoUnit);
  if (result)
  {
    std::cerr << "Could not get Audio Unit. " << result << std::endl;
    std::terminate();
  }

  UInt32 size = sizeof(mEngine.mSampleRate);
  result = AudioUnitGetProperty(mIoUnit, kAudioUnitProperty_SampleRate,
    kAudioUnitScope_Output, 0, &mEngine.mSampleRate, &size);
  if (result)
  {
    std::cerr << "Could not get sample rate. " << result << std::endl;
    std::terminate();
  }
  std::clog << "SAMPLE RATE: " << mEngine.mSampleRate << std::endl;

  AudioStreamBasicDescription asbd = {};
  asbd.mFormatID = kAudioFormatLinearPCM;
  asbd.mFormatFlags = kAudioFormatFlagIsSignedInteger | kAudioFormatFlagIsPacked
                      | kAudioFormatFlagsNativeEndian | kAudioFormatFlagIsNonInterleaved;
  asbd.mChannelsPerFrame = 2;
  asbd.mBytesPerPacket = sizeof(SInt16);
  asbd.mFramesPerPacket = 1;
  asbd.mBytesPerFrame = sizeof(SInt16);
  asbd.mBitsPerChannel = 8 * sizeof(SInt16);
  asbd.mSampleRate = mEngine.mSampleRate;

  result = AudioUnitSetProperty(mIoUnit, kAudioUnitProperty_StreamFormat,
    kAudioUnitScope_Input, 0, &asbd, sizeof(asbd));
  if (result)
  {
    std::cerr << "Could not set stream format. " << result << std::endl;
  }

  char deviceName[512];
  size = sizeof(deviceName);
  result = AudioUnitGetProperty(mIoUnit, kAudioDevicePropertyDeviceName,
    kAudioUnitScope_Global, 0, &deviceName, &size);
  if (result)
  {
    std::cerr << "Could not get device name. " << result << std::endl;
    std::terminate();
  }
  std::clog << "DEVICE NAME: " << deviceName << std::endl;

  UInt32 bufferSize = 512;
  size = sizeof(bufferSize);
  result = AudioUnitSetProperty(mIoUnit, kAudioDevicePropertyBufferFrameSize,
    kAudioUnitScope_Global, 0, &bufferSize, size);
  if (result)
  {
    std::cerr << "Could not set buffer size. " << result << std::endl;
    std::terminate();
  }
  mEngine.setBufferSize(bufferSize);

  UInt32 propertyResult = 0;
  size = sizeof(propertyResult);
  result = AudioUnitGetProperty(mIoUnit, kAudioDevicePropertyBufferFrameSize,
    kAudioUnitScope_Global, 0, &propertyResult, &size);
  if (result)
  {
    std::cerr << "Could not get buffer size. " << result << std::endl;
    std::terminate();
  }
  std::clog << "BUFFER SIZE: " << propertyResult << " samples, "
            << propertyResult / mEngine.mSampleRate * 1e3 << " ms." << std::endl;

  // the buffer, stream and safety-offset latencies are part of inTimeStamp->mHostTime
  // within the audio callback.
  UInt32 deviceLatency = 0;
  size = sizeof(deviceLatency);
  result = AudioUnitGetProperty(mIoUnit, kAudioDevicePropertyLatency,
    kAudioUnitScope_Output, 0, &deviceLatency, &size);
  if (result)
  {
    std::cerr << "Could not get output device latency. " << result << std::endl;
    std::terminate();
  }
  std::clog << "OUTPUT DEVICE LATENCY: " << deviceLatency << " samples, "
            << deviceLatency / mEngine.mSampleRate * 1e3 << " ms." << std::endl;

  using namespace std::chrono;
  const double latency = static_cast<double>(deviceLatency) / mEngine.mSampleRate;
  mEngine.mOutputLatency.store(duration_cast<microseconds>(duration<double>{latency}));

  AURenderCallbackStruct ioRemoteInput;
  ioRemoteInput.inputProc = audioCallback;
  ioRemoteInput.inputProcRefCon = &mEngine;

  result = AudioUnitSetProperty(mIoUnit, kAudioUnitProperty_SetRenderCallback,
    kAudioUnitScope_Input, 0, &ioRemoteInput, sizeof(ioRemoteInput));
  if (result)
  {
    std::cerr << "Could not set render callback. " << result << std::endl;
  }

  result = AudioUnitInitialize(mIoUnit);
  if (result)
  {
    std::cerr << "Could not initialize audio unit. " << result << std::endl;
  }
}

void AudioPlatform::uninitialize()
{
  OSStatus result = AudioUnitUninitialize(mIoUnit);
  if (result)
  {
    std::cerr << "Could not uninitialize Audio Unit. " << result << std::endl;
  }
}

void AudioPlatform::start()
{
  OSStatus result = AudioOutputUnitStart(mIoUnit);
  if (result)
  {
    std::cerr << "Could not start Audio Unit. " << result << std::endl;
    std::terminate();
  }
}

void AudioPlatform::stop()
{
  OSStatus result = AudioOutputUnitStop(mIoUnit);
  if (result)
  {
    std::cerr << "Could not stop Audio Unit. " << result << std::endl;
  }
}

} // namespace linkaudio
} // namespace ableton
