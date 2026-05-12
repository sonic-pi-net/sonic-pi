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

#include "windows.h"

#include <Windows.h>
#include <d3d11.h>
#include <dxgi1_2.h>
#include <inspectable.h>
#include <windows.graphics.directx.direct3d11.interop.h>
#include <windows.graphics.capture.interop.h>

#include <winrt/base.h>
#include <winrt/Windows.Foundation.h>
#include <winrt/Windows.Graphics.h>
#include <winrt/Windows.Graphics.Capture.h>
#include <winrt/Windows.Graphics.DirectX.h>
#include <winrt/Windows.Graphics.DirectX.Direct3D11.h>

#include "SpoutDX.h"

#include <atomic>
#include <iostream>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>

namespace winrt {
    using namespace ::winrt::Windows::Foundation;
    using namespace ::winrt::Windows::Graphics;
    using namespace ::winrt::Windows::Graphics::Capture;
    using namespace ::winrt::Windows::Graphics::DirectX;
    using namespace ::winrt::Windows::Graphics::DirectX::Direct3D11;
}

// stderr reaches gui.log (Qt's std::cerr is captured there); also
// OutputDebugString so DebugView and the Visual Studio Output pane
// see the same lines during live debugging.
#define SPOUTPUB_LOG(expr) do { \
    std::ostringstream _ss; _ss << "[spout] " << expr << "\n"; \
    std::cerr << _ss.str(); \
    OutputDebugStringA(_ss.str().c_str()); \
} while (0)

namespace {

winrt::IDirect3DDevice CreateDirect3DDevice(ID3D11Device* d3dDevice)
{
    winrt::com_ptr<IDXGIDevice> dxgiDevice;
    winrt::check_hresult(d3dDevice->QueryInterface(__uuidof(IDXGIDevice), dxgiDevice.put_void()));
    winrt::com_ptr<::IInspectable> inspectable;
    winrt::check_hresult(CreateDirect3D11DeviceFromDXGIDevice(dxgiDevice.get(), inspectable.put()));
    return inspectable.as<winrt::IDirect3DDevice>();
}

winrt::com_ptr<ID3D11Texture2D> GetFrameTexture(const winrt::Direct3D11CaptureFrame& frame)
{
    auto access = frame.Surface().as<::Windows::Graphics::DirectX::Direct3D11::IDirect3DDxgiInterfaceAccess>();
    winrt::com_ptr<ID3D11Texture2D> tex;
    winrt::check_hresult(access->GetInterface(__uuidof(ID3D11Texture2D), tex.put_void()));
    return tex;
}

class SonicPiSpoutPublisher
{
public:
    bool Start(HWND hwnd, const std::string& name, bool showCursor)
    {
        m_name = name;
        m_showCursor = showCursor;

        UINT flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;
        D3D_FEATURE_LEVEL fl = D3D_FEATURE_LEVEL_11_0;
        HRESULT hr = D3D11CreateDevice(
            nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, flags,
            nullptr, 0, D3D11_SDK_VERSION,
            m_d3dDevice.put(), &fl, nullptr);
        if (FAILED(hr)) {
            SPOUTPUB_LOG("D3D11CreateDevice failed: 0x" << std::hex << hr);
            return false;
        }
        try {
            m_winrtDevice = CreateDirect3DDevice(m_d3dDevice.get());
        } catch (const winrt::hresult_error& e) {
            SPOUTPUB_LOG("CreateDirect3DDevice failed: " << winrt::to_string(e.message()));
            return false;
        }

        auto interopFactory = winrt::get_activation_factory<winrt::GraphicsCaptureItem,
            ::IGraphicsCaptureItemInterop>();
        winrt::GraphicsCaptureItem item{ nullptr };
        hr = interopFactory->CreateForWindow(hwnd,
            winrt::guid_of<winrt::GraphicsCaptureItem>(),
            winrt::put_abi(item));
        if (FAILED(hr) || !item) {
            SPOUTPUB_LOG("IGraphicsCaptureItemInterop::CreateForWindow failed: 0x" << std::hex << hr);
            return false;
        }
        m_item = item;

        winrt::SizeInt32 size = m_item.Size();
        m_lastSize = size;
        m_framePool = winrt::Direct3D11CaptureFramePool::CreateFreeThreaded(
            m_winrtDevice,
            winrt::DirectXPixelFormat::B8G8R8A8UIntNormalized,
            2,
            size);
        m_session = m_framePool.CreateCaptureSession(m_item);

        // Both optional — Win10 < 2004 lacks IsCursorCaptureEnabled,
        // Win11 < 22H2 lacks IsBorderRequired. Best-effort, swallow errors.
        try { m_session.IsCursorCaptureEnabled(m_showCursor); } catch (...) {}
        try { m_session.IsBorderRequired(false); } catch (...) {}

        if (!m_spout.OpenDirectX11(m_d3dDevice.get())) {
            SPOUTPUB_LOG("SpoutDX::OpenDirectX11 failed");
            ResetCaptureState();
            return false;
        }
        m_spout.SetSenderName(m_name.c_str());
        m_spout.SetSenderFormat(DXGI_FORMAT_B8G8R8A8_UNORM);

        m_frameArrivedToken = m_framePool.FrameArrived({ this, &SonicPiSpoutPublisher::OnFrameArrived });
        m_itemClosedToken   = m_item.Closed({ this, &SonicPiSpoutPublisher::OnItemClosed });

        m_session.StartCapture();
        m_running.store(true);
        SPOUTPUB_LOG("publishing HWND " << hwnd << " as '" << m_name << "' ("
                     << size.Width << "x" << size.Height << ")");
        return true;
    }

    void Stop()
    {
        const bool wasRunning = m_running.exchange(false);

        // Wait for any in-flight OnFrameArrived (runs on a WGC thread-pool
        // thread) to finish before tearing down m_spout / m_d3dDevice.
        { std::lock_guard<std::mutex> lk(m_callbackMutex); }

        if (m_framePool) m_framePool.FrameArrived(m_frameArrivedToken);
        if (m_item)      m_item.Closed(m_itemClosedToken);
        ResetCaptureState();

        m_spout.ReleaseSender();
        m_spout.CloseDirectX11();

        m_winrtDevice = nullptr;
        m_d3dDevice = nullptr;
        if (wasRunning) SPOUTPUB_LOG("stopped");
    }

    void SetShowCursor(bool show)
    {
        m_showCursor = show;
        if (m_session) {
            try { m_session.IsCursorCaptureEnabled(show); } catch (...) {}
        }
    }

private:
    void ResetCaptureState()
    {
        if (m_session)   { m_session.Close();   m_session = nullptr; }
        if (m_framePool) { m_framePool.Close(); m_framePool = nullptr; }
        m_item = nullptr;
    }

    void OnFrameArrived(const winrt::Direct3D11CaptureFramePool& sender,
                        const winrt::IInspectable&)
    {
        std::lock_guard<std::mutex> lk(m_callbackMutex);
        if (!m_running.load()) return;

        auto frame = sender.TryGetNextFrame();
        if (!frame) return;

        const auto size = frame.ContentSize();
        if (size.Width <= 0 || size.Height <= 0) return;

        if (size.Width != m_lastSize.Width || size.Height != m_lastSize.Height) {
            m_lastSize = size;
            sender.Recreate(m_winrtDevice,
                            winrt::DirectXPixelFormat::B8G8R8A8UIntNormalized,
                            2, size);
            return;
        }

        try {
            auto tex = GetFrameTexture(frame);
            if (tex) m_spout.SendTexture(tex.get());
        } catch (const winrt::hresult_error&) {
            // Surface vanished mid-frame (window closed); the Closed
            // event handles teardown.
        }
    }

    void OnItemClosed(const winrt::GraphicsCaptureItem&, const winrt::IInspectable&)
    {
        SPOUTPUB_LOG("capture item closed by OS");
        m_running.store(false);
    }

    winrt::com_ptr<ID3D11Device>      m_d3dDevice;
    winrt::IDirect3DDevice            m_winrtDevice{ nullptr };
    winrt::GraphicsCaptureItem        m_item{ nullptr };
    winrt::Direct3D11CaptureFramePool m_framePool{ nullptr };
    winrt::GraphicsCaptureSession     m_session{ nullptr };
    winrt::event_token                m_frameArrivedToken{};
    winrt::event_token                m_itemClosedToken{};

    spoutDX           m_spout;
    std::string       m_name;
    winrt::SizeInt32  m_lastSize{};
    std::atomic<bool> m_running{ false };
    bool              m_showCursor{ false };
    std::mutex        m_callbackMutex;
};

std::unique_ptr<SonicPiSpoutPublisher> g_publisher;
std::mutex                              g_publisherMutex;

void EnsureApartment()
{
    // Qt's main thread initialises COM as STA via OleInitialize. Calling
    // init_apartment(single_threaded) on the same thread is idempotent
    // (apartment refcount++). Swallow RPC_E_CHANGED_MODE if the host is
    // already MTA — WinRT activation works in either apartment. Log any
    // other COM init failure so it doesn't disappear silently.
    try {
        winrt::init_apartment(winrt::apartment_type::single_threaded);
    } catch (const winrt::hresult_error& e) {
        if (e.code() != RPC_E_CHANGED_MODE) {
            SPOUTPUB_LOG("init_apartment failed: 0x" << std::hex << e.code()
                         << " " << winrt::to_string(e.message()));
        }
    }
}

} // namespace

namespace SonicPi {

bool startWindowSpoutPublishing(void* hwndPtr, const std::string& serverName, bool showCursor)
{
    if (!hwndPtr) {
        SPOUTPUB_LOG("null HWND");
        return false;
    }
    HWND hwnd = static_cast<HWND>(hwndPtr);

    EnsureApartment();
    if (!winrt::GraphicsCaptureSession::IsSupported()) {
        SPOUTPUB_LOG("Windows.Graphics.Capture not supported (requires Windows 10 1903+)");
        return false;
    }

    std::lock_guard<std::mutex> lock(g_publisherMutex);
    if (g_publisher) {
        g_publisher->Stop();
        g_publisher.reset();
    }
    auto pub = std::make_unique<SonicPiSpoutPublisher>();
    if (!pub->Start(hwnd, serverName, showCursor)) {
        return false;
    }
    g_publisher = std::move(pub);
    return true;
}

void stopWindowSpoutPublishing()
{
    std::lock_guard<std::mutex> lock(g_publisherMutex);
    if (g_publisher) {
        g_publisher->Stop();
        g_publisher.reset();
    }
}

void setSpoutShowCursor(bool showCursor)
{
    std::lock_guard<std::mutex> lock(g_publisherMutex);
    if (g_publisher) g_publisher->SetShowCursor(showCursor);
}

} // namespace SonicPi
