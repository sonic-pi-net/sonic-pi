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

#include "kiss_fft.h"

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
    // Force re-attach to the scope shared memory — public so callers can
    // invoke after a cold-swap device change to refresh the stale reader.
    void ResetConnection();

    // Direct pointer to a slot in the shm_audio_buffer array. Slot 0 is
    // the master output (driven by a supersonic-audio-out AudioOut2
    // instance); slots 1..N-1 are written by user AudioOut2 UGens.
    // Returns nullptr if the shm client is not initialised. The pointer
    // is into supersonic's shm mapping, so callers must keep the owning
    // AudioProcessor alive for its lifetime.
    shm_audio_buffer* GetAudioBufferSlot(unsigned int slot);

    // Flat pointer to the PerformanceMetrics region in supersonic's shm
    // mapping (METRICS_FIELD_COUNT contiguous uint32 fields), or nullptr
    // if the shm client is not connected. Same lifetime caveat as
    // GetAudioBufferSlot — the pointer is owned by this AudioProcessor.
    const std::atomic<uint32_t>* GetMetrics();

    // Passive views onto the OSC/debug transport rings and the node-tree
    // mirror, for the SuperSonic observability panel. Empty (null bases) when
    // the shm client is not connected. Call on the GUI thread (same thread as
    // ResetConnection); the views point into the shm mapping.
    ring_view GetInRing();      // OSC host→engine (what Sonic Pi sent)
    ring_view GetOutRing();     // OSC engine→host (replies)
    ring_view GetDebugRing();   // engine debug/log text
    node_tree_view GetNodeTree();
    native_stats GetNativeStats();  // synthdef count, allocated buffers + bytes

private:
    void GenLogSpace(uint32_t limit, uint32_t n);
    void GenLinSpace(uint32_t limit, uint32_t n);
    void SetupFFT();
    void CalculateFFT(ProcessedAudio& audio);

private:
    std::unique_ptr<server_shared_memory_client> m_shmClient;
    shm_scope_buffer_reader m_shmReader;

    // Previous validity for transition-only logging in Run().
    bool m_shmReaderLastValid = false;

    int m_scSynthPort = 0;

    std::atomic<bool> m_calculateFFT = { false };
    std::atomic<bool> m_running = { false };
    std::atomic<int> m_maxBuckets = { 0 };
    std::atomic<bool> m_quit = { false };
    std::atomic<bool> m_consumed = { false };

    // FFT
    kiss_fft_cfg m_cfg;
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

