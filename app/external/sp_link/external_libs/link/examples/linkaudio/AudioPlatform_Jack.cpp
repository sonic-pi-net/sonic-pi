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

#include "AudioPlatform_Jack.hpp"
#include <chrono>
#include <iostream>
#include <string>

namespace ableton
{
namespace linkaudio
{

AudioPlatform::AudioPlatform(Link& link)
  : mEngine(link)
  , mSampleTime(0.)
  , mpJackClient(nullptr)
  , mpJackPorts(nullptr)
{
  initialize();
  start();
}

AudioPlatform::~AudioPlatform()
{
  stop();
  uninitialize();
}

int AudioPlatform::audioCallback(jack_nframes_t nframes, void* pvUserData)
{
  AudioPlatform* pAudioPlatform = static_cast<AudioPlatform*>(pvUserData);
  return pAudioPlatform->audioCallback(nframes);
}

void AudioPlatform::latencyCallback(jack_latency_callback_mode_t, void* pvUserData)
{
  AudioPlatform* pAudioPlatform = static_cast<AudioPlatform*>(pvUserData);
  pAudioPlatform->updateLatency();
}

void AudioPlatform::updateLatency()
{
  jack_latency_range_t latencyRange;
  jack_port_get_latency_range(mpJackPorts[0], JackPlaybackLatency, &latencyRange);
  mEngine.mOutputLatency.store(
    std::chrono::microseconds(llround(1.0e6 * latencyRange.max / mEngine.mSampleRate)));
}

int AudioPlatform::audioCallback(jack_nframes_t nframes)
{
  using namespace std::chrono;
  AudioEngine& engine = mEngine;

  const auto hostTime = mHostTimeFilter.sampleTimeToHostTime(mSampleTime);

  mSampleTime += nframes;

  const auto bufferBeginAtOutput = hostTime + engine.mOutputLatency.load();

  engine.audioCallback(bufferBeginAtOutput, nframes);

  for (int k = 0; k < 2; ++k)
  {
    float* buffer = static_cast<float*>(jack_port_get_buffer(mpJackPorts[k], nframes));
    for (unsigned long i = 0; i < nframes; ++i)
      buffer[i] = static_cast<float>(engine.mBuffer[i]);
  }

  return 0;
}

void AudioPlatform::initialize()
{
  jack_status_t status = JackFailure;
  mpJackClient = jack_client_open("LinkHut", JackNullOption, &status);
  if (mpJackClient == nullptr)
  {
    std::cerr << "Could not initialize Audio Engine. ";
    std::cerr << "JACK: " << std::endl;
    if (status & JackFailure)
      std::cerr << "Overall operation failed." << std::endl;
    if (status & JackInvalidOption)
      std::cerr << "Invalid or unsupported option." << std::endl;
    if (status & JackNameNotUnique)
      std::cerr << "Client name not unique." << std::endl;
    if (status & JackServerStarted)
      std::cerr << "Server is started." << std::endl;
    if (status & JackServerFailed)
      std::cerr << "Unable to connect to server." << std::endl;
    if (status & JackServerError)
      std::cerr << "Server communication error." << std::endl;
    if (status & JackNoSuchClient)
      std::cerr << "Client does not exist." << std::endl;
    if (status & JackLoadFailure)
      std::cerr << "Unable to load internal client." << std::endl;
    if (status & JackInitFailure)
      std::cerr << "Unable to initialize client." << std::endl;
    if (status & JackShmFailure)
      std::cerr << "Unable to access shared memory." << std::endl;
    if (status & JackVersionError)
      std::cerr << "Client protocol version mismatch." << std::endl;
    std::cerr << std::endl;
    std::terminate();
  }

  const double bufferSize = jack_get_buffer_size(mpJackClient);
  const double sampleRate = jack_get_sample_rate(mpJackClient);
  mEngine.setBufferSize(static_cast<std::size_t>(bufferSize));
  mEngine.setSampleRate(sampleRate);

  jack_set_latency_callback(mpJackClient, AudioPlatform::latencyCallback, this);

  mpJackPorts = new jack_port_t*[2];

  for (int k = 0; k < 2; ++k)
  {
    const std::string port_name = "out_" + std::to_string(k + 1);
    mpJackPorts[k] = jack_port_register(
      mpJackClient, port_name.c_str(), JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
    if (mpJackPorts[k] == nullptr)
    {
      std::cerr << "Could not get Audio Device. " << std::endl;
      jack_client_close(mpJackClient);
      std::terminate();
    }
  }

  jack_set_process_callback(mpJackClient, AudioPlatform::audioCallback, this);
}

void AudioPlatform::uninitialize()
{
  for (int k = 0; k < 2; ++k)
  {
    jack_port_unregister(mpJackClient, mpJackPorts[k]);
    mpJackPorts[k] = nullptr;
  }
  delete[] mpJackPorts;
  mpJackPorts = nullptr;

  jack_client_close(mpJackClient);
  mpJackClient = nullptr;
}

void AudioPlatform::start()
{
  jack_activate(mpJackClient);

  const char** playback_ports = jack_get_ports(
    mpJackClient, nullptr, JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput | JackPortIsPhysical);

  if (playback_ports)
  {
    const std::string client_name = jack_get_client_name(mpJackClient);
    for (int k = 0; k < 2; ++k)
    {
      const std::string port_name = "out_" + std::to_string(k + 1);
      const std::string client_port = client_name + ':' + port_name;
      jack_connect(mpJackClient, client_port.c_str(), playback_ports[k]);
    }
    jack_free(playback_ports);
  }
}

void AudioPlatform::stop()
{
  jack_deactivate(mpJackClient);
}

} // namespace linkaudio
} // namespace ableton
