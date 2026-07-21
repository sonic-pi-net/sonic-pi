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

// Regression guarded against: on 2026-07-21 a submodule bump changed the shm
// handshake MAGIC (0x5C09E007 -> 0x5C09E008, scope slots became cursor-ring
// streams). The engine binary was not rebuilt, so it kept writing the old
// layout while the GUI read the new one. The reader threw "Invalid shared
// memory magic", the attach path swallowed it as a boot race, and the scope
// and SuperSonic debug pane went dead for hours with nothing in any log —
// audio kept working because that path is OSC, not shm.
//
// The strings below are the ones server_shm.hpp actually throws. If they
// change there, this test is where it should surface.

#include <string>

#include <catch2/catch_test_macros.hpp>

#include <api/audio/shm_attach_diagnostics.hpp>

using sonic_pi::audio::IsShmAttachRetryable;

TEST_CASE("the boot race stays quiet", "[shm][diagnostics]")
{
    // supersonic simply hasn't published the segment yet. The processor
    // retries and it resolves — logging this would be noise every launch.
    CHECK(IsShmAttachRetryable("shm_open(open) failed for /sonic-pi-shm"));
    CHECK(IsShmAttachRetryable("OpenFileMapping failed for sonic-pi-shm"));
}

TEST_CASE("version skew is reported, not swallowed", "[shm][diagnostics]")
{
    // These mean the segment EXISTS but this reader cannot use it. Retrying
    // never fixes them, so they must not be classified as retryable.
    CHECK_FALSE(IsShmAttachRetryable(
        "Invalid shared memory magic — is the audio engine running?"));
    CHECK_FALSE(IsShmAttachRetryable(
        "Shared memory segment smaller than expected — stale or foreign segment?"));
    CHECK_FALSE(IsShmAttachRetryable(
        "Shared memory layout mismatch — engine and reader were built "
        "with different memory profiles (test-sized build staged as "
        "production? see memory_profile.h / BUILD_TESTS sizing)"));

    // mmap/view failures are also permanent for this attempt.
    CHECK_FALSE(IsShmAttachRetryable("mmap failed for /sonic-pi-shm"));
    CHECK_FALSE(IsShmAttachRetryable("MapViewOfFile failed for sonic-pi-shm"));
}

TEST_CASE("an unrecognised failure defaults to being reported", "[shm][diagnostics]")
{
    // Fail loud, not silent: an unknown message is more likely a real fault
    // than a boot race, and silence is what made the original bug expensive.
    CHECK_FALSE(IsShmAttachRetryable("something nobody anticipated"));
    CHECK_FALSE(IsShmAttachRetryable(""));
}
