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

// The screen captures — the Syphon and Spout feeds and the session recorder —
// show the app, not its chrome. Both platforms hand the window over with its
// title bar (ScreenCaptureKit crops a display, Windows.Graphics.Capture gives
// the whole window), so the crop is to the window's content area. Until
// 2026-10-14 it was the whole frame, title bar and all.

#include <catch2/catch_test_macros.hpp>

#include "platform/capture_crop.h"

using SonicPi::CaptureBox;
using SonicPi::CaptureRect;
using SonicPi::ScreenRect;
using SonicPi::captureRectOnDisplay;
using SonicPi::clientBoxInFrame;

// ─── macOS: AppKit's global bottom-left points to a display-local, top-left sourceRect ───

TEST_CASE("macOS: the crop is the content area, so the title bar is above it", "[capture]")
{
    // A window 800 wide whose frame is 628 tall: 600 of content under a 28 pt title bar,
    // its bottom-left at (100, 200) on a 1440x900 primary display.
    const CaptureRect frame{ 100, 200, 800, 628 };
    const CaptureRect content{ 100, 200, 800, 600 };
    const CaptureRect primary{ 0, 0, 1440, 900 };
    const CaptureRect crop = captureRectOnDisplay(content, 900, primary);
    CHECK(crop == CaptureRect{ 100, 100, 800, 600 });
    // the frame's crop would start 28 pt higher, on the title bar
    CHECK(captureRectOnDisplay(frame, 900, primary) == CaptureRect{ 100, 72, 800, 628 });
}

TEST_CASE("macOS: a full-screen window has no title bar, and its crop is its whole frame", "[capture]")
{
    const CaptureRect whole{ 0, 0, 1440, 900 };
    CHECK(captureRectOnDisplay(whole, 900, whole) == CaptureRect{ 0, 0, 1440, 900 });
}

TEST_CASE("macOS: a display to the left of the primary gets display-local coordinates", "[capture]")
{
    // CoreGraphics puts the left display at x = -1920, same top as the primary
    const CaptureRect display{ -1920, 0, 1920, 1080 };
    const CaptureRect content{ -1500, 100, 800, 600 };
    CHECK(captureRectOnDisplay(content, 900, display) == CaptureRect{ 420, 200, 800, 600 });
}

TEST_CASE("macOS: a display above the primary, where AppKit's y runs on past the primary's top", "[capture]")
{
    // CoreGraphics: the display above spans y -1080..0. AppKit: y 900..1980 from the primary's bottom.
    const CaptureRect display{ 0, -1080, 1920, 1080 };
    const CaptureRect content{ 300, 1000, 800, 600 };   // its top at AppKit 1600: 380 pt below that display's top
    CHECK(captureRectOnDisplay(content, 900, display) == CaptureRect{ 300, 380, 800, 600 });
}

// ─── Windows: the client area's box inside the captured frame ───

TEST_CASE("Windows: the crop is the client area's box inside the captured window", "[capture]")
{
    // The captured frame is the window's extended frame bounds: 1920x1080 at (100, 100). Its
    // client area sits 8 px in and 31 px down (a border and a title bar).
    const ScreenRect frame{ 100, 100, 1920, 1080 };
    const ScreenRect client{ 108, 131, 1904, 1041 };
    CHECK(clientBoxInFrame(frame, client) == CaptureBox{ 8, 31, 1904, 1041 });
}

TEST_CASE("Windows: a frameless window's client area is the whole frame", "[capture]")
{
    const ScreenRect frame{ 0, 0, 1920, 1080 };
    CHECK(clientBoxInFrame(frame, frame) == CaptureBox{ 0, 0, 1920, 1080 });
}

TEST_CASE("Windows: a client area reaching past the frame is clamped to it", "[capture]")
{
    // The frame's texture a pixel or two short of what the window reports: the box stays inside it.
    const ScreenRect frame{ 0, 0, 800, 600 };
    const ScreenRect client{ 8, 31, 800, 600 };
    CHECK(clientBoxInFrame(frame, client) == CaptureBox{ 8, 31, 792, 569 });
}

TEST_CASE("Windows: a client area starting before the frame starts at the frame's edge", "[capture]")
{
    const ScreenRect frame{ 100, 100, 800, 600 };
    const ScreenRect client{ 96, 90, 700, 500 };
    CHECK(clientBoxInFrame(frame, client) == CaptureBox{ 0, 0, 700, 500 });
}

TEST_CASE("Windows: no client area, or no frame, is an empty box", "[capture]")
{
    const ScreenRect frame{ 100, 100, 800, 600 };
    CHECK(clientBoxInFrame(frame, ScreenRect{ 108, 131, 0, 0 }).empty());        // minimised: no client area
    CHECK(clientBoxInFrame(ScreenRect{ 0, 0, 0, 0 }, frame).empty());            // nothing captured
    CHECK(clientBoxInFrame(frame, ScreenRect{ 900, 700, 100, 100 }).empty());    // client wholly outside the frame
    CHECK_FALSE(clientBoxInFrame(frame, frame).empty());
}
