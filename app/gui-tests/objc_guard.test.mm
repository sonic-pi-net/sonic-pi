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

// An Objective-C exception inside a system callback is fatal to the whole
// app. On 2026-10-06 AVFoundation refused a 5120x2880 H.264 writer, threw
// inside the ScreenCaptureKit completion handler, and Sonic Pi aborted
// mid-session. Every block handed to a system API goes through
// SonicPi::objc::guarded; this file proves the guard holds against the real
// exception, and lints the Objective-C++ sources so no block escapes it.

#include <catch2/catch_test_macros.hpp>

#import <AVFoundation/AVFoundation.h>
#import <Foundation/Foundation.h>

#include <fstream>
#include <regex>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "platform/objc_guard.h"
#include "platform/recorder_limits.h"
#include "platform/recorder_audio_mix.h"
#include "platform/recorder_settings.h"

using namespace SonicPi::objc;

TEST_CASE("the exception that killed the app is caught by the guard", "[objc][recorder]")
{
    // The MOTU's 8-channel OUT tap, handed straight to an AAC track: AVFoundation
    // throws NSInvalidArgumentException ("Missing required key
    // AVChannelLayoutKey") rather than refuse politely.
    __block bool reached = false;
    __block bool completed = false;
    void (^handler)(NSNumber*) = guarded("asset writer input", ^(NSNumber* channels) {
        reached = true;
        AVAssetWriterInput* input = [AVAssetWriterInput
            assetWriterInputWithMediaType:AVMediaTypeAudio
                           outputSettings:SonicPi::recorderAudioSettings(channels.unsignedIntValue, 48000)];
        (void)input;
        completed = true;   // never reached with 8 channels: the line above throws
    });
    handler(@8);
    CHECK(reached);
    CHECK_FALSE(completed);
    // The recording takes the stereo mix of that device, which is accepted.
    handler(@(SonicPi::recordedChannels(8)));
    CHECK(completed);
}

TEST_CASE("the video settings are accepted at every display size, 5K included", "[objc][recorder]")
{
    // AVFoundation takes a 5K (and larger) H.264 writer; the recording is
    // scaled to 4K for load and file size (recorder_limits.h), not because
    // it must be.
    for (const auto size : { std::pair<size_t, size_t>{ 5120, 2880 }, { 1512, 982 }, { 4096, 2304 } })
    {
        const bool accepted = guard("video settings", ^{
            (void)[AVAssetWriterInput assetWriterInputWithMediaType:AVMediaTypeVideo
                                                    outputSettings:SonicPi::recorderVideoSettings(size.first, size.second)];
        });
        INFO(size.first << "x" << size.second);
        CHECK(accepted);
    }
}

TEST_CASE("guard runs a body now and answers whether it threw", "[objc]")
{
    CHECK(guard("fine", ^{ (void)[NSArray array]; }));
    CHECK_FALSE(guard("throws", ^{ [NSException raise:@"Test" format:@"on purpose"]; }));
}

TEST_CASE("a guarded block that answers something answers the fallback when it threw", "[objc]")
{
    NSNumber* (^answer)(BOOL) = guardedOr("answer", ^NSNumber*(BOOL fail) {
        if (fail) [NSException raise:@"Test" format:@"on purpose"];
        return @42;
    }, (NSNumber*)@-1);
    CHECK([answer(NO) intValue] == 42);
    CHECK([answer(YES) intValue] == -1);
}

// ─── The lint ───────────────────────────────────────────────────────────
//
// Every block literal (^{ or ^( ) in the GUI's Objective-C++ sources is
// passed through guarded(...) or guardedOr(...) — or run now by guard(...)
// — on the same line, so the exception it may raise is reported, not
// fatal. The guard's own header is the one place a bare block is the point.

namespace
{
std::vector<std::string> unguardedBlocks(const std::string& path)
{
    std::vector<std::string> found;
    std::ifstream in(path);
    REQUIRE(in.good());
    static const std::regex blockLiteral(R"(\^\s*(\(|\{|[A-Za-z_][A-Za-z0-9_<>*: ]*\s*\())");
    static const std::regex guardedCall(R"(guard(ed|edOr)?\s*\()");
    std::string line;
    int n = 0;
    while (std::getline(in, line))
    {
        n++;
        const auto comment = line.find("//");
        const std::string code = comment == std::string::npos ? line : line.substr(0, comment);
        if (!std::regex_search(code, blockLiteral))
            continue;
        if (std::regex_search(code, guardedCall))
            continue;
        std::ostringstream ss;
        ss << path.substr(path.rfind('/') + 1) << ":" << n << ": " << code;
        found.push_back(ss.str());
    }
    return found;
}
} // namespace

TEST_CASE("every block handed to a system API in the GUI's Objective-C++ is guarded", "[objc][style]")
{
    const std::string gui = std::string(SP_ROOT) + "/app/gui/";
    for (const char* file : { "platform/recorder.mm", "platform/capture_target.mm",
                              "platform/syphon_publisher.mm", "platform/macos.mm" })
    {
        const auto strays = unguardedBlocks(gui + file);
        std::ostringstream ss;
        for (const auto& s : strays) ss << "\n  " << s;
        INFO("Bare blocks (wrap in SonicPi::objc::guarded):" << ss.str());
        CHECK(strays.empty());
    }
}
