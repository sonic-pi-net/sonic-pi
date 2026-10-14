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
// Shared glue between Windows.Graphics.Capture and Direct3D11. Used
// by spout_publisher.cpp (live VJ feed) and recorder_win.cpp (session
// recording) — both consume WGC frames as D3D11 textures.

#pragma once

#include <Windows.h>
#include <d3d11.h>
#include <dwmapi.h>
#include <dxgi1_2.h>
#include <inspectable.h>
#include <windows.graphics.directx.direct3d11.interop.h>

#include "capture_crop.h"

#include <winrt/base.h>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.Graphics.h>
#include <winrt/Windows.Graphics.Capture.h>
#include <winrt/Windows.Graphics.DirectX.h>
#include <winrt/Windows.Graphics.DirectX.Direct3D11.h>

#include <iostream>
#include <sstream>

// Pull the WGC + D3D11-interop names into the top-level winrt namespace
// so callers can write `winrt::IDirect3DDevice` instead of the full
// `winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice`.
// Adding `using namespace` to a foreign namespace isn't beautiful, but
// it's the long-standing pattern in both spout_publisher.cpp and
// recorder_win.cpp; consolidated here so the aliases stay in one place.
namespace winrt {
    using namespace ::winrt::Windows::Foundation;
    using namespace ::winrt::Windows::Graphics;
    using namespace ::winrt::Windows::Graphics::Capture;
    using namespace ::winrt::Windows::Graphics::DirectX;
    using namespace ::winrt::Windows::Graphics::DirectX::Direct3D11;
}

namespace SonicPi::wgc {

// Wrap a D3D11 device as a WinRT IDirect3DDevice for use with WGC's
// CreateFreeThreaded / CreateCaptureSession.
inline winrt::IDirect3DDevice CreateDirect3DDevice(ID3D11Device* d3dDevice)
{
    winrt::com_ptr<IDXGIDevice> dxgiDevice;
    winrt::check_hresult(d3dDevice->QueryInterface(
        __uuidof(IDXGIDevice), dxgiDevice.put_void()));
    winrt::com_ptr<::IInspectable> inspectable;
    winrt::check_hresult(CreateDirect3D11DeviceFromDXGIDevice(
        dxgiDevice.get(), inspectable.put()));
    return inspectable.as<winrt::IDirect3DDevice>();
}

// Unwrap a WGC capture frame back to its underlying D3D11 texture.
inline winrt::com_ptr<ID3D11Texture2D>
GetFrameTexture(const winrt::Direct3D11CaptureFrame& frame)
{
    auto access = frame.Surface().as<
        ::Windows::Graphics::DirectX::Direct3D11::IDirect3DDxgiInterfaceAccess>();
    winrt::com_ptr<ID3D11Texture2D> tex;
    winrt::check_hresult(access->GetInterface(
        __uuidof(ID3D11Texture2D), tex.put_void()));
    return tex;
}

// The window's client area as a box inside its captured frame: what the
// Spout feed and the recorder show (capture_crop.h). A window capture is
// the window's extended frame bounds — title bar and borders in — and
// that is where its texture sits on screen. Empty when the window has no
// client area inside the frame (minimised), and the caller shows the
// whole frame.
inline SonicPi::CaptureBox ClientBoxOf(HWND hwnd, const winrt::SizeInt32& frameSize)
{
    RECT frame{};
    if (FAILED(DwmGetWindowAttribute(hwnd, DWMWA_EXTENDED_FRAME_BOUNDS,
                                     &frame, sizeof(frame)))
        && !GetWindowRect(hwnd, &frame)) {
        return {};
    }
    RECT client{};
    POINT origin{};
    if (!GetClientRect(hwnd, &client) || !ClientToScreen(hwnd, &origin)) {
        return {};
    }
    auto px = [](LONG v) { return v > 0 ? static_cast<unsigned>(v) : 0u; };
    return SonicPi::clientBoxInFrame(
        { frame.left, frame.top, px(frameSize.Width), px(frameSize.Height) },
        { origin.x, origin.y, px(client.right), px(client.bottom) });
}

// A texture of our own on `device`, `box` big, for a cropped copy of a
// capture frame to go in (and to outlive the pool's recycling of it).
inline bool AllocCropTexture(ID3D11Device* device, const SonicPi::CaptureBox& box,
                             winrt::com_ptr<ID3D11Texture2D>& out)
{
    D3D11_TEXTURE2D_DESC desc = {};
    desc.Width            = box.width;
    desc.Height           = box.height;
    desc.MipLevels        = 1;
    desc.ArraySize        = 1;
    desc.Format           = DXGI_FORMAT_B8G8R8A8_UNORM;
    desc.SampleDesc.Count = 1;
    desc.Usage            = D3D11_USAGE_DEFAULT;
    desc.BindFlags        = D3D11_BIND_RENDER_TARGET | D3D11_BIND_SHADER_RESOURCE;
    out = nullptr;
    return SUCCEEDED(device->CreateTexture2D(&desc, nullptr, out.put()));
}

// `box` of `from` copied into `to`, which is `box` big.
inline void CopyCrop(ID3D11DeviceContext* context, ID3D11Texture2D* to,
                     ID3D11Texture2D* from, const SonicPi::CaptureBox& box)
{
    D3D11_BOX src = {};
    src.left   = box.left;
    src.top    = box.top;
    src.right  = box.left + box.width;
    src.bottom = box.top + box.height;
    src.front  = 0;
    src.back   = 1;
    context->CopySubresourceRegion(to, 0, 0, 0, 0, from, 0, &src);
}

// Qt's main thread initialises COM as STA via OleInitialize. Calling
// init_apartment(single_threaded) on the same thread is idempotent
// (apartment refcount++). RPC_E_CHANGED_MODE is swallowed for hosts
// that have already chosen MTA — WinRT activation works in either.
inline void EnsureApartment()
{
    try {
        winrt::init_apartment(winrt::apartment_type::single_threaded);
    } catch (const winrt::hresult_error& e) {
        if (e.code() != RPC_E_CHANGED_MODE) {
            std::ostringstream ss;
            ss << "[wgc] init_apartment failed: 0x"
               << std::hex << e.code() << "\n";
            std::cerr << ss.str();
            OutputDebugStringA(ss.str().c_str());
        }
    }
}

} // namespace SonicPi::wgc
