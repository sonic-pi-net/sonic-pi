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

#include "AudioPlatform_Asio.hpp"

namespace ableton
{
namespace linkaudio
{

void fatalError(const ASIOError result, const std::string& function)
{
  std::cerr << "Call to ASIO function " << function << " failed";
  if (result != ASE_OK)
  {
    std::cerr << " (ASIO error code " << result << ")";
  }
  std::cerr << std::endl;
  std::terminate();
}

double asioSamplesToDouble(const ASIOSamples& samples)
{
  return samples.lo + samples.hi * std::pow(2, 32);
}

// ASIO processing callbacks
ASIOTime* bufferSwitchTimeInfo(ASIOTime* timeInfo, long index, ASIOBool)
{
  AudioPlatform* platform = AudioPlatform::singleton();
  if (platform)
  {
    platform->audioCallback(timeInfo, index);
  }
  return nullptr;
}

void bufferSwitch(long index, ASIOBool processNow)
{
  ASIOTime timeInfo{};
  ASIOError result = ASIOGetSamplePosition(
    &timeInfo.timeInfo.samplePosition, &timeInfo.timeInfo.systemTime);
  if (result != ASE_OK)
  {
    std::cerr << "ASIOGetSamplePosition failed with ASIO error: " << result << std::endl;
  }
  else
  {
    timeInfo.timeInfo.flags = kSystemTimeValid | kSamplePositionValid;
  }

  bufferSwitchTimeInfo(&timeInfo, index, processNow);
}

AudioPlatform* AudioPlatform::_singleton = nullptr;

AudioPlatform* AudioPlatform::singleton()
{
  return _singleton;
}

void AudioPlatform::setSingleton(AudioPlatform* platform)
{
  _singleton = platform;
}

AudioPlatform::AudioPlatform(Link& link)
  : mEngine(link)
{
  initialize();
  mEngine.setBufferSize(mDriverInfo.preferredSize);
  mEngine.setSampleRate(mDriverInfo.sampleRate);
  setSingleton(this);
  start();
}

AudioPlatform::~AudioPlatform()
{
  stop();
  ASIODisposeBuffers();
  ASIOExit();
  if (asioDrivers != nullptr)
  {
    asioDrivers->removeCurrentDriver();
  }

  setSingleton(nullptr);
}

void AudioPlatform::audioCallback(ASIOTime* timeInfo, long index)
{
  auto hostTime = std::chrono::microseconds(0);
  if (timeInfo->timeInfo.flags & kSystemTimeValid)
  {
    hostTime = mHostTimeFilter.sampleTimeToHostTime(
      asioSamplesToDouble(timeInfo->timeInfo.samplePosition));
  }

  const auto bufferBeginAtOutput = hostTime + mEngine.mOutputLatency.load();

  ASIOBufferInfo* bufferInfos = mDriverInfo.bufferInfos;
  const long numSamples = mDriverInfo.preferredSize;
  const long numChannels = mDriverInfo.numBuffers;
  const double maxAmp = std::numeric_limits<int>::max();

  mEngine.audioCallback(bufferBeginAtOutput, numSamples);

  for (long i = 0; i < numSamples; ++i)
  {
    for (long j = 0; j < numChannels; ++j)
    {
      int* buffer = static_cast<int*>(bufferInfos[j].buffers[index]);
      buffer[i] = static_cast<int>(mEngine.mBuffer[i] * maxAmp);
    }
  }

  if (mDriverInfo.outputReady)
  {
    ASIOOutputReady();
  }
}

void AudioPlatform::createAsioBuffers()
{
  DriverInfo& driverInfo = mDriverInfo;
  ASIOBufferInfo* bufferInfo = driverInfo.bufferInfos;
  driverInfo.numBuffers = 0;

  // Prepare input channels. Though this is not necessarily required, the opened input
  // channels will not work.
  int numInputBuffers;
  if (driverInfo.inputChannels > LINK_ASIO_INPUT_CHANNELS)
  {
    numInputBuffers = LINK_ASIO_INPUT_CHANNELS;
  }
  else
  {
    numInputBuffers = driverInfo.inputChannels;
  }

  for (long i = 0; i < numInputBuffers; ++i, ++bufferInfo)
  {
    bufferInfo->isInput = ASIOTrue;
    bufferInfo->channelNum = i;
    bufferInfo->buffers[0] = bufferInfo->buffers[1] = nullptr;
  }

  // Prepare output channels
  int numOutputBuffers;
  if (driverInfo.outputChannels > LINK_ASIO_OUTPUT_CHANNELS)
  {
    numOutputBuffers = LINK_ASIO_OUTPUT_CHANNELS;
  }
  else
  {
    numOutputBuffers = driverInfo.outputChannels;
  }

  for (long i = 0; i < numOutputBuffers; i++, bufferInfo++)
  {
    bufferInfo->isInput = ASIOFalse;
    bufferInfo->channelNum = i;
    bufferInfo->buffers[0] = bufferInfo->buffers[1] = nullptr;
  }

  driverInfo.numBuffers = numInputBuffers + numOutputBuffers;
  ASIOError result = ASIOCreateBuffers(driverInfo.bufferInfos, driverInfo.numBuffers,
    driverInfo.preferredSize, &(mAsioCallbacks));
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOCreateBuffers");
  }

  // Now get all buffer details, sample word length, name, word clock group and latency
  for (long i = 0; i < driverInfo.numBuffers; ++i)
  {
    driverInfo.channelInfos[i].channel = driverInfo.bufferInfos[i].channelNum;
    driverInfo.channelInfos[i].isInput = driverInfo.bufferInfos[i].isInput;

    result = ASIOGetChannelInfo(&driverInfo.channelInfos[i]);
    if (result != ASE_OK)
    {
      fatalError(result, "ASIOGetChannelInfo");
    }

    std::clog << "ASIOGetChannelInfo successful, type: "
              << (driverInfo.bufferInfos[i].isInput ? "input" : "output")
              << ", channel: " << i
              << ", sample type: " << driverInfo.channelInfos[i].type << std::endl;

    if (driverInfo.channelInfos[i].type != ASIOSTInt32LSB)
    {
      fatalError(ASE_OK, "Unsupported sample type!");
    }
  }

  long inputLatency, outputLatency;
  result = ASIOGetLatencies(&inputLatency, &outputLatency);
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOGetLatencies");
  }
  std::clog << "Driver input latency: " << inputLatency << "usec"
            << ", output latency: " << outputLatency << "usec" << std::endl;

  const double bufferSize = driverInfo.preferredSize / driverInfo.sampleRate;
  auto outputLatencyMicros =  
    std::chrono::microseconds(llround(outputLatency / driverInfo.sampleRate));
  outputLatencyMicros += std::chrono::microseconds(llround(1.0e6 * bufferSize));
  
  mEngine.mOutputLatency.store(outputLatencyMicros);

  std::clog << "Total latency: " << outputLatencyMicros.count() << "usec" << std::endl;
}

void AudioPlatform::initializeDriverInfo()
{
  ASIOError result =
    ASIOGetChannels(&mDriverInfo.inputChannels, &mDriverInfo.outputChannels);
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOGetChannels");
  }
  std::clog << "ASIOGetChannels succeeded, inputs:" << mDriverInfo.inputChannels
            << ", outputs: " << mDriverInfo.outputChannels << std::endl;

  long minSize, maxSize, granularity;
  result =
    ASIOGetBufferSize(&minSize, &maxSize, &mDriverInfo.preferredSize, &granularity);
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOGetBufferSize");
  }
  std::clog << "ASIOGetBufferSize succeeded, min: " << minSize << ", max: " << maxSize
            << ", preferred: " << mDriverInfo.preferredSize
            << ", granularity: " << granularity << std::endl;

  result = ASIOGetSampleRate(&mDriverInfo.sampleRate);
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOGetSampleRate");
  }
  std::clog << "ASIOGetSampleRate succeeded, sampleRate: " << mDriverInfo.sampleRate
            << "Hz" << std::endl;

  // Check wether the driver requires the ASIOOutputReady() optimization, which can be
  // used by the driver to reduce output latency by one block
  mDriverInfo.outputReady = (ASIOOutputReady() == ASE_OK);
  std::clog << "ASIOOutputReady optimization is "
            << (mDriverInfo.outputReady ? "enabled" : "disabled") << std::endl;
}

void AudioPlatform::initialize()
{
  if (!loadAsioDriver(LINK_ASIO_DRIVER_NAME))
  {
    std::cerr << "Failed opening ASIO driver for device named '" << LINK_ASIO_DRIVER_NAME
              << "', is the driver installed?" << std::endl;
    std::terminate();
  }

  ASIOError result = ASIOInit(&mDriverInfo.driverInfo);
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOInit");
  }

  std::clog << "ASIOInit succeeded, asioVersion: " << mDriverInfo.driverInfo.asioVersion
            << ", driverVersion: " << mDriverInfo.driverInfo.driverVersion
            << ", name: " << mDriverInfo.driverInfo.name << std::endl;

  initializeDriverInfo();

  ASIOCallbacks* callbacks = &(mAsioCallbacks);
  callbacks->bufferSwitch = &bufferSwitch;
  callbacks->bufferSwitchTimeInfo = &bufferSwitchTimeInfo;
  createAsioBuffers();
}

void AudioPlatform::start()
{
  ASIOError result = ASIOStart();
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOStart");
  }
}

void AudioPlatform::stop()
{
  ASIOError result = ASIOStop();
  if (result != ASE_OK)
  {
    fatalError(result, "ASIOStop");
  }
}

} // namespace linkaudio
} // namespace ableton
