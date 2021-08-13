// MIT License

// Copyright (c) 2021 Luis Lloret

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <stdexcept>
#include <chrono>
#include <iostream>
#include <atomic>
#include <vector>
#include <mutex>

#include "ableton/Link.hpp"
#include "sp_link_nif_callbacks.h"
#include "sp_link.h"
#include "version.h"
#include "monitorlogger.h"

using namespace std;

int g_monitor_level = 6;
static atomic<bool> g_initialized{ false };
static ableton::Link* g_link = nullptr;
static mutex g_init_mutex;



// The Link callbacks
std::function<void(size_t)> peer_count_callback = [](std::size_t num_peers) {
    if (is_link_callback_registered()){
        send_to_erlang_num_peers(num_peers);
    }
};


std::function<void(double)> tempo_callback = [](double bpm) {
    if (is_link_callback_registered()){
        send_to_erlang_tempo(bpm);
    }
};


std::function<void(bool)> start_stop_callback = [](bool is_playing) {
    if (is_link_callback_registered()){
        send_to_erlang_start_stop(is_playing);
    }

};


// Need to pass the bpm, as Link requires it.
int sp_link_init(double bpm)
{
    lock_guard<std::mutex> lock (g_init_mutex);
    if (g_initialized){
        return 0;
    }
    g_link = new ableton::Link(bpm);
    g_initialized = true;
    //MonitorLogger::getInstance().setLogLevel(g_monitor_level);

    g_link->setNumPeersCallback(peer_count_callback);
    g_link->setStartStopCallback(start_stop_callback);
    g_link->setTempoCallback(tempo_callback);

    return 0;
}

void sp_link_deinit()
{
    if (!g_initialized){
        return;
    }
    delete g_link;
    g_initialized = false;
}


int sp_link_enable(bool enable)
{
    if (!g_initialized){
        return -1;
    }

    g_link->enable(enable);
    return 0;
}


int sp_link_is_enabled(bool* enabled)
{
    if (!g_initialized){
        return -1;
    }

    *enabled = g_link->isEnabled() ? 1 : 0;
    return 0;
}


int sp_link_set_tempo(double bpm, ErlNifSInt64 micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setTempo(bpm, std::chrono::microseconds(micros));
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_get_tempo(double* bpm)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *bpm = state.tempo();
    return 0;
}


int sp_link_set_is_playing(bool is_playing, ErlNifSInt64 micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setIsPlaying(is_playing, std::chrono::microseconds(micros));
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_is_playing(bool* is_playing)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *is_playing = state.isPlaying();
    return 0;
}


int sp_link_get_time_for_is_playing(ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *micros = state.timeForIsPlaying().count();
    return 0;
}


int sp_link_get_num_peers(int* num_peers)
{
    if (!g_initialized){
        return -1;
    }

    *num_peers = static_cast<int>(g_link->numPeers());
    return 0;
}


int sp_link_start_stop_sync_enable(bool enable)
{
    if (!g_initialized){
        return -1;
    }

    g_link->enableStartStopSync(enable);
    return 0;
}


int sp_link_is_start_stop_sync_enabled(bool* enabled)
{
    if (!g_initialized){
        return -1;
    }

    *enabled = g_link->isStartStopSyncEnabled();
    return 0;
}


int sp_link_get_beat_at_time(ErlNifSInt64 micros, double quantum, double* beat)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *beat = state.beatAtTime(std::chrono::microseconds(micros), quantum);
    return 0;
}


int sp_link_get_phase_at_time(ErlNifSInt64 micros, double quantum, double* beat)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *beat = state.phaseAtTime(std::chrono::microseconds(micros), quantum);
    return 0;
}


int sp_link_get_time_at_beat(double beat, double quantum, ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    *micros = state.timeAtBeat(beat, quantum).count();
    return 0;
}


int sp_link_request_beat_at_time(double beat, ErlNifSInt64 micros, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.requestBeatAtTime(beat, std::chrono::microseconds(micros), quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_force_beat_at_time(double beat, ErlNifSInt64 micros, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.forceBeatAtTime(beat, std::chrono::microseconds(micros), quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_request_beat_at_start_playing_time(double beat, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.requestBeatAtStartPlayingTime(beat, quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


int sp_link_set_is_playing_and_request_beat_at_time(bool is_playing, ErlNifSInt64 micros, double beat, double quantum)
{
    if (!g_initialized){
        return -1;
    }

    auto state = g_link->captureAppSessionState();
    state.setIsPlayingAndRequestBeatAtTime(is_playing, std::chrono::microseconds(micros), beat, quantum);
    g_link->commitAppSessionState(state);
    return 0;
}


// TODO: Keep in mind that different C++ clocks might have different values. Make sure that this is consistent with other clocks (need to test)
int sp_link_get_current_time_microseconds(ErlNifSInt64* micros)
{
    if (!g_initialized){
        return -1;
    }

    *micros = g_link->clock().micros().count();
    return 0;
}
