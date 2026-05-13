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
#include <dxgi1_2.h>
#include <inspectable.h>
#include <windows.graphics.directx.direct3d11.interop.h>

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
