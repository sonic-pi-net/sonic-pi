//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <complex>
#include <memory>
#include <string>
#include <thread>
#include <mutex>
#include <vector>
#include "server_shm.hpp"

#include "kiss_fft/kiss_fft.h"

#include "api/sonicpi_api.h"

namespace SonicPi
{

struct IAPIClient;

class AudioProcessor
{
public:
    AudioProcessor(IAPIClient* pAPI, int synthPort);
    ~AudioProcessor();

    void Run();

    ProcessedAudio& GetCurrentProcessedAudio();
    void EnableFFT(bool enable);
    void Enable(bool start);
    void SetConsumed(bool consumed);
    void SetMaxBuckets(int maxBuckets);
    void Quit();

private:
    void ResetConnection();
    void GenLogSpace(uint32_t limit, uint32_t n);
    void GenLinSpace(uint32_t limit, uint32_t n);
    void SetupFFT();
    void CalculateFFT(ProcessedAudio& audio);

private:
    std::unique_ptr<server_shared_memory_client> m_shmClient;
    scope_buffer_reader m_shmReader;

    unsigned int m_emptyFrames = 0;
    int m_scSynthPort = 0;

    std::atomic<bool> m_calculateFFT = { false };
    std::atomic<bool> m_running = { false };
    std::atomic<int> m_maxBuckets = { 0 };
    std::atomic<bool> m_quit = { false };
    std::atomic<bool> m_consumed = { false };

    // FFT
    mkiss_fft_cfg m_cfg;
    std::vector<std::complex<float>> m_fftIn[2];
    std::vector<std::complex<float>> m_fftOut[2];
    std::vector<float> m_fftMag[2];
    std::vector<float> m_window;
    std::vector<float> m_spectrumPartitions;
    std::pair<uint32_t, uint32_t> m_lastSpectrumPartitions = { 0, 0 };

    // Output data, double buffered
    ProcessedAudio m_processedAudio;

    float m_totalWin = 0.0;
    bool m_paused = false;

    std::thread m_thread;
    std::mutex m_mutex;
    IAPIClient* m_pClient;
};

} // Sonic Pi

