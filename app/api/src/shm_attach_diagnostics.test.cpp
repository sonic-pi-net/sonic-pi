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
// The segment is anonymous now: the reader asks the engine's attach endpoint
// for it (clockwork's shm_attach.hpp), and AudioProcessor::AttachLocked wraps
// whatever that reports as "shm attach: <reason>"; a segment that arrives is
// then validated by shm_segment_client's constructor (shm_segment.hpp). The
// strings below are the ones those two actually produce. If they change
// there, this test is where it should surface.

#include <string>

#include <catch2/catch_test_macros.hpp>

#include <api/audio/shm_attach_diagnostics.hpp>

using sonic_pi::audio::IsShmAttachRetryable;

TEST_CASE("the boot race stays quiet", "[shm][diagnostics]")
{
    // The engine is not serving its attach endpoint yet. The processor
    // retries and it resolves — logging this would be noise every launch.
    // POSIX: the socket path is not there yet, or nothing listens on it.
    CHECK(IsShmAttachRetryable(
        "shm attach: connect /tmp/sonic-pi-4560.shm: Connection refused"));
    CHECK(IsShmAttachRetryable(
        "shm attach: connect /tmp/sonic-pi-4560.shm: No such file or directory"));
    // Windows: the named pipe has not been created.
    CHECK(IsShmAttachRetryable(
        "shm attach: open \\\\.\\pipe\\sonic-pi-4560.shm: "
        "The system cannot find the file specified."));
}

TEST_CASE("a hand-off that cannot be used is reported, not swallowed", "[shm][diagnostics]")
{
    // The endpoint answered, but with something this process cannot use.
    // Retrying never fixes these, so they must not be classified as retryable.
    CHECK_FALSE(IsShmAttachRetryable(
        "shm attach: malformed hand-off from /tmp/sonic-pi-4560.shm"));
    CHECK_FALSE(IsShmAttachRetryable(
        "shm attach: the engine at /tmp/sonic-pi-4560.shm is not running as this user"));
    CHECK_FALSE(IsShmAttachRetryable(
        "shm attach: the engine could not duplicate its segment into this process"));
}

TEST_CASE("version skew is reported, not swallowed", "[shm][diagnostics]")
{
    // The segment arrived but this reader cannot use it: it was written by
    // an engine built from different sources. Retrying never fixes that.
    CHECK_FALSE(IsShmAttachRetryable(
        "Invalid shared memory magic — is the audio engine running?"));
    CHECK_FALSE(IsShmAttachRetryable(
        "Shared memory segment smaller than its header — stale or foreign segment?"));
    CHECK_FALSE(IsShmAttachRetryable(
        "Shared memory segment smaller than its header claims — truncated or foreign segment?"));
    CHECK_FALSE(IsShmAttachRetryable("Shared memory peer plane outside the segment"));

    // Mapping the received handle failing is also permanent for this attempt.
    CHECK_FALSE(IsShmAttachRetryable("mmap (received handle) failed"));
    CHECK_FALSE(IsShmAttachRetryable(
        "MapViewOfFile (received handle) failed: Access is denied."));
}

TEST_CASE("an unrecognised failure defaults to being reported", "[shm][diagnostics]")
{
    // Fail loud, not silent: an unknown message is more likely a real fault
    // than a boot race, and silence is what made the original bug expensive.
    CHECK_FALSE(IsShmAttachRetryable("something nobody anticipated"));
    CHECK_FALSE(IsShmAttachRetryable(""));
}
