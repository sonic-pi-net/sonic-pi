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

#include "AudioPlatform_Wasapi.hpp"
#include <Comdef.h>
#include <chrono>

// WARNING: This file provides an audio driver for Windows using WASAPI. This driver is
// considered experimental and has problems with low-latency playback. Please consider
// using the ASIO driver instead.

namespace ableton
{
namespace linkaudio
{

// GUID identifiers used to when looking up COM enumerators and devices
static const IID kMMDeviceEnumeratorId = __uuidof(MMDeviceEnumerator);
static const IID kIMMDeviceEnumeratorId = __uuidof(IMMDeviceEnumerator);
static const IID kAudioClientId = __uuidof(IAudioClient);
static const IID kAudioRenderClientId = __uuidof(IAudioRenderClient);

// Controls how large the driver's ring buffer will be, expressed in terms of
// 100-nanosecond units. This value also influences the overall driver latency.
static const REFERENCE_TIME kBufferDuration = 1000000;

// How long to block the runloop while waiting for an event callback.
static const DWORD kWaitTimeoutInMs = 2000;

void fatalError(HRESULT result, LPCTSTR context)
{
  if (result > 0)
  {
    _com_error error(result);
    LPCTSTR errorMessage = error.ErrorMessage();
    std::cerr << context << ": " << errorMessage << std::endl;
  }
  else
  {
    std::cerr << context << std::endl;
  }

  std::terminate();
}

DWORD renderAudioRunloop(LPVOID lpParam)
{
  AudioPlatform* platform = static_cast<AudioPlatform*>(lpParam);
  return platform->audioRunloop();
}

AudioPlatform::AudioPlatform(Link& link)
  : mEngine(link)
  , mSampleTime(0)
  , mDevice(nullptr)
  , mAudioClient(nullptr)
  , mRenderClient(nullptr)
  , mStreamFormat(nullptr)
  , mEventHandle(nullptr)
  , mAudioThreadHandle(nullptr)
  , mIsRunning(false)
{
  initialize();
  mEngine.setBufferSize(bufferSize());
  mEngine.setSampleRate(mStreamFormat->nSamplesPerSec);
  start();
}

AudioPlatform::~AudioPlatform()
{
  // WARNING: Here be dragons!
  // The WASAPI driver is not thread-safe, and crashes may occur when shutting down due
  // to these fields being concurrently accessed in the audio thread. Introducing a mutex
  // in the audio thread is not an appropriate solution to fix this race condition; a more
  // robust solution needs to be considered instead.

  if (mDevice != nullptr)
  {
    mDevice->Release();
  }
  if (mAudioClient != nullptr)
  {
    mAudioClient->Release();
  }
  if (mRenderClient != nullptr)
  {
    mRenderClient->Release();
  }
  CoTaskMemFree(mStreamFormat);
}

UINT32 AudioPlatform::bufferSize()
{
  UINT32 bufferSize;
  HRESULT result = mAudioClient->GetBufferSize(&bufferSize);
  if (FAILED(result))
  {
    fatalError(result, "Could not get buffer size");
    return 0; // not reached
  }

  return bufferSize;
}

void AudioPlatform::initialize()
{
  HRESULT result = CoInitialize(nullptr);
  if (FAILED(result))
  {
    fatalError(result, "Could not initialize COM library");
  }

  IMMDeviceEnumerator* enumerator = nullptr;
  result = CoCreateInstance(kMMDeviceEnumeratorId, nullptr, CLSCTX_ALL,
    kIMMDeviceEnumeratorId, (void**)&enumerator);
  if (FAILED(result))
  {
    fatalError(result, "Could not create device instance");
  }

  result = enumerator->GetDefaultAudioEndpoint(eRender, eConsole, &(mDevice));
  if (FAILED(result))
  {
    fatalError(result, "Could not get default audio endpoint");
  }
  else
  {
    enumerator->Release();
    enumerator = nullptr;
  }

  result =
    mDevice->Activate(kAudioClientId, CLSCTX_ALL, nullptr, (void**)&(mAudioClient));
  if (FAILED(result))
  {
    fatalError(result, "Could not activate audio device");
  }

  result = mAudioClient->GetMixFormat(&(mStreamFormat));
  if (FAILED(result))
  {
    fatalError(result, "Could not get mix format");
  }

  if (mStreamFormat->wFormatTag == WAVE_FORMAT_EXTENSIBLE)
  {
    WAVEFORMATEXTENSIBLE* streamFormatEx =
      reinterpret_cast<WAVEFORMATEXTENSIBLE*>(mStreamFormat);
    if (streamFormatEx->SubFormat != KSDATAFORMAT_SUBTYPE_IEEE_FLOAT)
    {
      fatalError(0, "Sorry, only IEEE floating point streams are supported");
    }
  }
  else
  {
    fatalError(0, "Sorry, only extensible wave streams are supported");
  }

  result = mAudioClient->Initialize(AUDCLNT_SHAREMODE_SHARED,
    AUDCLNT_STREAMFLAGS_EVENTCALLBACK, kBufferDuration, 0, mStreamFormat, nullptr);
  if (FAILED(result))
  {
    fatalError(result, "Could not initialize audio device");
  }

  mEventHandle = CreateEvent(nullptr, false, false, nullptr);
  if (mEventHandle == nullptr)
  {
    fatalError(result, "Could not create event handle");
  }

  result = mAudioClient->GetService(kAudioRenderClientId, (void**)&(mRenderClient));
  if (FAILED(result))
  {
    fatalError(result, "Could not get audio render service");
  }

  mIsRunning = true;
  LPTHREAD_START_ROUTINE threadEntryPoint =
    reinterpret_cast<LPTHREAD_START_ROUTINE>(renderAudioRunloop);
  mAudioThreadHandle = CreateThread(nullptr, 0, threadEntryPoint, this, 0, nullptr);
  if (mAudioThreadHandle == nullptr)
  {
    fatalError(GetLastError(), "Could not create audio thread");
  }
}

void AudioPlatform::start()
{
  UINT32 bufSize = bufferSize();
  BYTE* buffer;
  HRESULT result = mRenderClient->GetBuffer(bufSize, &buffer);
  if (FAILED(result))
  {
    fatalError(result, "Could not get render client buffer (in start audio engine)");
  }

  result = mRenderClient->ReleaseBuffer(bufSize, 0);
  if (FAILED(result))
  {
    fatalError(result, "Could not release buffer");
  }

  result = mAudioClient->SetEventHandle(mEventHandle);
  if (FAILED(result))
  {
    fatalError(result, "Could not set event handle to audio client");
  }

  REFERENCE_TIME latency;
  result = mAudioClient->GetStreamLatency(&latency);
  if (FAILED(result))
  {
    fatalError(result, "Could not get stream latency");
  }

  result = mAudioClient->Start();
  if (FAILED(result))
  {
    fatalError(result, "Could not start audio client");
  }
}

DWORD AudioPlatform::audioRunloop()
{
  while (mIsRunning)
  {
    DWORD wait = WaitForSingleObject(mEventHandle, kWaitTimeoutInMs);
    if (wait != WAIT_OBJECT_0)
    {
      mIsRunning = false;
      mAudioClient->Stop();
      return wait;
    }

    // Get the amount of padding, which basically is the amount of data in the driver's
    // ring buffer that is filled with unread data. Thus, subtracting this amount from
    // the buffer size gives the effective buffer size, which is the amount of frames
    // that can be safely written to the driver.
    UINT32 paddingFrames;
    HRESULT result = mAudioClient->GetCurrentPadding(&paddingFrames);
    if (FAILED(result))
    {
      fatalError(result, "Could not get number of padding frames");
    }

    const UINT32 numSamples = bufferSize() - paddingFrames;

    BYTE* buffer;
    result = mRenderClient->GetBuffer(numSamples, &buffer);
    if (FAILED(result))
    {
      fatalError(result, "Could not get render client buffer (in callback)");
    }

    const double sampleRate = static_cast<double>(mStreamFormat->nSamplesPerSec);
    using namespace std::chrono;
    const auto bufferDuration =
      duration_cast<microseconds>(duration<double>{numSamples / sampleRate});

    const auto hostTime = mHostTimeFilter.sampleTimeToHostTime(mSampleTime);

    mSampleTime += numSamples;

    const auto bufferBeginAtOutput = hostTime + mEngine.mOutputLatency.load();

    mEngine.audioCallback(bufferBeginAtOutput, numSamples);

    float* floatBuffer = reinterpret_cast<float*>(buffer);
    for (WORD i = 0; i < numSamples; ++i)
    {
      if (i >= mEngine.mBuffer.size())
      {
        break;
      }
      for (WORD j = 0; j < mStreamFormat->nChannels; ++j)
      {
        floatBuffer[j + (i * mStreamFormat->nChannels)] =
          static_cast<float>(mEngine.mBuffer[i]);
      }
    }

    // Write the buffer to the audio driver and subsequently free the buffer memory
    result = mRenderClient->ReleaseBuffer(numSamples, 0);
    if (FAILED(result))
    {
      fatalError(result, "Error rendering data");
    }
  } // end of runloop

  mIsRunning = false;
  return 0;
}


// void fillBuffer(MetronomeSynth& metronome,
//  const UINT32 startFrame,
//  const UINT32 numSamples,
//  const UINT32 numChannels,
//  BYTE* buffer)
//{
//  float* floatBuffer = reinterpret_cast<float*>(buffer);
//  UINT32 frame = startFrame;
//  while (frame < numSamples * numChannels)
//  {
//    const float sample = static_cast<float>(metronome.getSample());
//    for (UINT32 channel = 0; channel < numChannels; ++channel)
//    {
//      floatBuffer[frame++] = sample;
//    }
//  }
//}

} // namespace linkaudio
} // namespace ableton
