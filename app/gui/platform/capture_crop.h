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

#ifndef CAPTURE_CROP_H
#define CAPTURE_CROP_H

// What the screen captures crop to, on every platform: the main window's
// content, not its chrome. The Syphon and Spout feeds and the session
// recorder all get the window with its title bar — ScreenCaptureKit crops
// a display, Windows.Graphics.Capture hands over the whole window — and the
// picture is the app, so each crops to the content area. Pure, so a test
// can pin the geometry; the platform code only gathers the rectangles.
namespace SonicPi
{

// ─── macOS ───

// A rectangle in points.
struct CaptureRect
{
    double x = 0;
    double y = 0;
    double width = 0;
    double height = 0;
    bool operator==(const CaptureRect& o) const
    {
        return x == o.x && y == o.y && width == o.width && height == o.height;
    }
};

// ScreenCaptureKit's sourceRect: points, local to the captured display, origin
// top-left. AppKit gives the window's content in global points with the origin
// at the primary display's bottom-left; CoreGraphics gives the display's
// bounds in global points with the origin at the primary's top-left.
inline CaptureRect captureRectOnDisplay(const CaptureRect& content,
                                        double primaryHeight,
                                        const CaptureRect& display)
{
    const double top = primaryHeight - (content.y + content.height);
    return { content.x - display.x, top - display.y, content.width, content.height };
}

// ─── Windows ───

// A rectangle in screen pixels, origin top-left.
struct ScreenRect
{
    long left = 0;
    long top = 0;
    unsigned width = 0;
    unsigned height = 0;
};

// A box inside a captured frame, in its pixels.
struct CaptureBox
{
    unsigned left = 0;
    unsigned top = 0;
    unsigned width = 0;
    unsigned height = 0;
    bool empty() const { return width == 0 || height == 0; }
    bool operator==(const CaptureBox& o) const
    {
        return left == o.left && top == o.top && width == o.width && height == o.height;
    }
};

// The client area's box inside the captured frame, clamped to the frame:
// `frame` is where the captured texture sits on screen (the window's extended
// frame bounds) and how big the texture is; `client` the window's client area
// on screen. Empty when there is no client area inside the frame (a minimised
// window has none), and the caller shows the whole frame.
inline CaptureBox clientBoxInFrame(const ScreenRect& frame, const ScreenRect& client)
{
    const unsigned left = client.left > frame.left ? static_cast<unsigned>(client.left - frame.left) : 0;
    const unsigned top = client.top > frame.top ? static_cast<unsigned>(client.top - frame.top) : 0;
    if (left >= frame.width || top >= frame.height)
        return {};
    // no std::min: windows.h's min macro would eat it
    const unsigned width = frame.width - left, height = frame.height - top;
    return { left, top,
             client.width < width ? client.width : width,
             client.height < height ? client.height : height };
}

} // namespace SonicPi

#endif // CAPTURE_CROP_H
