// Tests for the screen-reader announcement policy — the per-category gate that
// lets a user silence "Run started"/"Stopped" speech (so VoiceOver stops ducking
// the start of the audio) without losing error announcements.

#include <catch2/catch_test_macros.hpp>

#include "utils/announcementpolicy.h"

using SonicPi::Announcement;
using SonicPi::AnnouncementPolicy;

TEST_CASE("default policy speaks everything", "[a11y][announce]")
{
    AnnouncementPolicy p;
    CHECK(p.shouldSpeak(Announcement::Transport));
    CHECK(p.shouldSpeak(Announcement::Navigation));
    CHECK(p.shouldSpeak(Announcement::General));
    CHECK(p.shouldSpeak(Announcement::Error));
}

TEST_CASE("transport speech follows its flag", "[a11y][announce]")
{
    AnnouncementPolicy p;
    p.speakTransport = false;
    CHECK_FALSE(p.shouldSpeak(Announcement::Transport));
    p.speakTransport = true;
    CHECK(p.shouldSpeak(Announcement::Transport));
}

TEST_CASE("navigation always speaks", "[a11y][announce]")
{
    AnnouncementPolicy p;
    p.speakTransport = false;
    CHECK(p.shouldSpeak(Announcement::Navigation));
}

TEST_CASE("errors and general always speak, even with transport off", "[a11y][announce]")
{
    AnnouncementPolicy p;
    p.speakTransport = false;
    CHECK(p.shouldSpeak(Announcement::Error));
    CHECK(p.shouldSpeak(Announcement::General));
}
