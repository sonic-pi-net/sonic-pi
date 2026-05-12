//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
//
// spout-receiver: native smoke-test for Sonic Pi's Spout publisher.
// Subscribes to a named Spout sender (default "Sonic Pi") and prints
// one status line per second until Ctrl+C. Built when configured with
// -DBUILD_SPOUT_SMOKETEST=ON.
//
// Usage:
//   spout-receiver.exe              # waits for "Sonic Pi"
//   spout-receiver.exe "Other Name" # subscribes to a different sender

#include <Windows.h>
#include "SpoutDX.h"

#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstdio>
#include <string>
#include <thread>

namespace {

std::atomic<bool> g_stop{false};

BOOL WINAPI ConsoleCtrlHandler(DWORD type)
{
    if (type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT || type == CTRL_CLOSE_EVENT) {
        g_stop = true;
        return TRUE;
    }
    return FALSE;
}

} // namespace

int main(int argc, char** argv)
{
    // Unbuffered stdout so each printf is visible immediately on a
    // console kill or pipe.
    std::setvbuf(stdout, nullptr, _IONBF, 0);

    const char* senderName = (argc > 1) ? argv[1] : "Sonic Pi";
    SetConsoleCtrlHandler(ConsoleCtrlHandler, TRUE);

    spoutDX rx;
    if (!rx.OpenDirectX11()) {
        std::fprintf(stderr, "OpenDirectX11 failed — no D3D11 device available?\n");
        return 1;
    }
    rx.SetReceiverName(senderName);

    std::printf("spout-receiver: waiting for sender '%s' (Ctrl+C to exit)\n", senderName);

    using clock = std::chrono::steady_clock;
    auto lastReport = clock::now();
    std::uint64_t framesThisSecond = 0;
    bool wasConnected = false;

    while (!g_stop) {
        // ReceiveTexture also drives SpoutDX's connection state machine.
        if (rx.ReceiveTexture()) {
            ++framesThisSecond;
        }

        const auto now = clock::now();
        if (now - lastReport >= std::chrono::seconds(1)) {
            const bool connected = rx.IsConnected();
            if (connected) {
                unsigned int w = 0, h = 0;
                HANDLE handle = nullptr;
                DWORD fmt = 0;
                if (rx.GetSenderInfo(senderName, w, h, handle, fmt)) {
                    std::printf("[ok] %s: %ux%u fmt=0x%lx frames/sec=%llu\n",
                                senderName, w, h, (unsigned long)fmt,
                                (unsigned long long)framesThisSecond);
                } else {
                    std::printf("[ok] %s: connected, info pending, frames/sec=%llu\n",
                                senderName, (unsigned long long)framesThisSecond);
                }
            } else {
                std::printf("[..] waiting for sender '%s'\n", senderName);
            }
            if (connected != wasConnected) {
                std::printf("[**] state change: %s\n",
                            connected ? "CONNECTED" : "DISCONNECTED");
                wasConnected = connected;
            }
            framesThisSecond = 0;
            lastReport = now;
        }

        // ~60Hz poll; SpoutDX has its own sync.
        std::this_thread::sleep_for(std::chrono::milliseconds(16));
    }

    rx.ReleaseReceiver();
    rx.CloseDirectX11();
    std::printf("exit\n");
    return 0;
}
