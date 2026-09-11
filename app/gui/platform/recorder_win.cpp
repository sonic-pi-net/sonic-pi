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
// Windows session recorder — captures the Sonic Pi window via
// Windows.Graphics.Capture and muxes it with the master audio mix
// (pulled from supersonic's shm_audio_buffer) into a fragmented .mp4
// (H.264 + AAC) via Media Foundation's IMFSinkWriter.

#include "windows.h"
#include "wgc_d3d_interop.h"
#include "mp4_soft_remux.h"

#include <d3d11_4.h>
#include <windows.graphics.capture.interop.h>

#include <mfapi.h>
#include <mferror.h>
#include <mfidl.h>
#include <mfreadwrite.h>

#include <algorithm>
#include <atomic>
#include <chrono>
#include <climits>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <thread>
#include <vector>

#define RECORDER_LOG(expr) do {                                       \
    std::ostringstream _ss; _ss << "[recorder] " << expr << "\n";     \
    std::cerr << _ss.str();                                            \
    OutputDebugStringA(_ss.str().c_str());                             \
} while (0)

namespace {

using SonicPi::wgc::CreateDirect3DDevice;
using SonicPi::wgc::GetFrameTexture;

// MF timestamps are 100-ns units. Both video and audio use one
// QPC-derived clock so the streams share a reference — avoids
// reconciling WGC's SystemRelativeTime against QPC.

LONGLONG QpcFrequency()
{
    static const LONGLONG freq = [] {
        LARGE_INTEGER f; QueryPerformanceFrequency(&f); return f.QuadPart;
    }();
    return freq;
}

LONGLONG NowIn100ns()
{
    LARGE_INTEGER c; QueryPerformanceCounter(&c);
    // (c * 10_000_000) / freq, but compute as (c / freq) * 10M + remainder
    // scaled to avoid 64-bit overflow on long-running boots.
    const LONGLONG freq = QpcFrequency();
    const LONGLONG whole = c.QuadPart / freq;
    const LONGLONG rem   = c.QuadPart % freq;
    return whole * 10000000LL + (rem * 10000000LL) / freq;
}



// ─── Recorder ────────────────────────────────────────────────────────────

class SonicPiSessionRecorder
{
public:
    SonicPiSessionRecorder() = default;

    // Static-storage tear-down would otherwise leave the audio-pump
    // std::thread joinable → std::terminate. Stop() is idempotent.
    ~SonicPiSessionRecorder() { Stop(); }

    bool Start(HWND hwnd, const std::wstring& filePath, bool showCursor,
               shm_audio_buffer* audioSlot)
    {
        m_filePath        = filePath;
        m_fragmentedPath  = filePath + L".frag";
        m_showCursor      = showCursor;
        m_audioReader     = shm_audio_buffer_reader(audioSlot);
        m_audioChannels   = audioSlot ? audioSlot->channels : 0;

        // MFStartup is refcounted; balanced by MFShutdown in Stop().
        HRESULT hr = MFStartup(MF_VERSION, MFSTARTUP_LITE);
        if (FAILED(hr)) {
            RECORDER_LOG("MFStartup failed: 0x" << std::hex << hr);
            return false;
        }
        m_mfStarted = true;

        if (!CreateD3DDevice())            return AbortStart();
        if (!CreateDxgiDeviceManager())    return AbortStart();
        if (!OpenCapture(hwnd))            return AbortStart();
        if (!CreateSinkWriterAndStreams()) return AbortStart();
        if (!ConfigureVideoInput())        return AbortStart();
        if (m_hasAudioStream && !ConfigureAudioInput()) {
            RECORDER_LOG("audio input config failed — video-only");
            m_hasAudioStream = false;
        }

        if (FAILED(m_writer->BeginWriting())) {
            RECORDER_LOG("BeginWriting failed");
            return AbortStart();
        }

        // Hold display + system awake. On wake the encoder typically
        // returns DXGI_ERROR_DEVICE_REMOVED and the session is dead.
        // Reset in Stop().
        SetThreadExecutionState(ES_CONTINUOUS | ES_SYSTEM_REQUIRED
                                | ES_DISPLAY_REQUIRED);
        m_powerStateSet = true;

        // Hook WGC callbacks AFTER BeginWriting so any in-flight frame
        // delivery has a fully-initialised pipeline to write into.
        m_frameArrivedToken = m_framePool.FrameArrived(
            { this, &SonicPiSessionRecorder::OnFrameArrived });
        m_itemClosedToken = m_item.Closed(
            { this, &SonicPiSessionRecorder::OnItemClosed });

        m_session.StartCapture();
        m_running.store(true);

        if (m_hasAudioStream) {
            m_audioPullBuf.assign(kAudioPullFrames * m_audioChannels, 0.0f);
            // Discard any pre-roll written between /s_new and now.
            m_audioReader.seek_to_live();
            m_audioThreadStop.store(false);
            m_audioThread = std::thread(
                &SonicPiSessionRecorder::AudioPumpLoop, this);
        }

        RECORDER_LOG("recording HWND " << hwnd << " to "
                     << WideToUtf8(m_filePath) << " ("
                     << m_size.Width << "x" << m_size.Height << ")");
        return true;
    }

    void Stop()
    {
        const bool wasRunning = m_running.exchange(false);

        // Release the power-state request first so we don't hold the
        // system / display awake any longer than necessary, even if
        // Finalize takes a moment.
        if (m_powerStateSet) {
            SetThreadExecutionState(ES_CONTINUOUS);
            m_powerStateSet = false;
        }

        // Stop the audio pump first so no more samples land after Finalize.
        m_audioThreadStop.store(true);
        if (m_audioThread.joinable()) m_audioThread.join();

        if (m_framePool) m_framePool.FrameArrived(m_frameArrivedToken);
        if (m_item)      m_item.Closed(m_itemClosedToken);

        // Wait for any in-flight OnFrameArrived (WGC thread pool) to
        // unwind before tearing down the sink writer / D3D device.
        { std::lock_guard<std::mutex> lk(m_callbackMutex); }

        if (m_session)   { m_session.Close();   m_session = nullptr; }
        if (m_framePool) { m_framePool.Close(); m_framePool = nullptr; }
        m_item = nullptr;

        // Finalize the fragmented sink. Blocks until the last fragment
        // is flushed.
        bool finalizedOk = false;
        if (m_writer && m_writerState.load() == kStarted) {
            std::lock_guard<std::mutex> lk(m_writerMutex);
            HRESULT hr = m_writer->Finalize();
            if (FAILED(hr)) {
                RECORDER_LOG("Finalize failed: 0x" << std::hex << hr);
            } else {
                finalizedOk = true;
            }
        }
        m_writer = nullptr;

        // Soft-remux the fragmented intermediate to a standard MP4 at
        // the user-facing path. On failure, fall back to renaming the
        // fragmented file in place — VLC and modern players still open
        // it, just not legacy WMP.
        if (finalizedOk && !m_fragmentedPath.empty()) {
            if (SonicPi::remuxToStandardMp4(m_fragmentedPath, m_filePath)) {
                DeleteFileW(m_fragmentedPath.c_str());
            } else {
                RECORDER_LOG("remux failed — keeping fragmented file at "
                             << WideToUtf8(m_filePath));
                MoveFileExW(m_fragmentedPath.c_str(), m_filePath.c_str(),
                            MOVEFILE_REPLACE_EXISTING);
            }
        }

        m_dxgiManager = nullptr;
        m_winrtDevice = nullptr;
        m_d3dDevice = nullptr;

        if (m_mfStarted) { MFShutdown(); m_mfStarted = false; }

        if (wasRunning) RECORDER_LOG("recording finalised: "
                                     << WideToUtf8(m_filePath));
    }

    void SetShowCursor(bool show)
    {
        m_showCursor = show;
        if (m_session) {
            try { m_session.IsCursorCaptureEnabled(show); } catch (...) {}
        }
    }

    bool IsRunning() const { return m_running.load(); }

private:
    enum WriterState : uint8_t {
        kIdle = 0, kStarting = 1, kStarted = 2, kFailed = 3
    };

    static std::string WideToUtf8(const std::wstring& w)
    {
        if (w.empty()) return {};
        int n = WideCharToMultiByte(CP_UTF8, 0, w.data(), (int)w.size(),
                                    nullptr, 0, nullptr, nullptr);
        std::string s(n, '\0');
        WideCharToMultiByte(CP_UTF8, 0, w.data(), (int)w.size(),
                            s.data(), n, nullptr, nullptr);
        return s;
    }

    bool AbortStart()
    {
        Stop();
        return false;
    }

    bool CreateD3DDevice()
    {
        // VIDEO_SUPPORT lets MF's video processor MFT (BGRA→NV12) run on
        // this device; BGRA_SUPPORT lets WGC's frame pool produce BGRA.
        UINT flags = D3D11_CREATE_DEVICE_BGRA_SUPPORT
                   | D3D11_CREATE_DEVICE_VIDEO_SUPPORT;
        D3D_FEATURE_LEVEL fl = D3D_FEATURE_LEVEL_11_0;
        HRESULT hr = D3D11CreateDevice(
            nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, flags,
            nullptr, 0, D3D11_SDK_VERSION,
            m_d3dDevice.put(), &fl, m_d3dContext.put());
        if (FAILED(hr)) {
            RECORDER_LOG("D3D11CreateDevice failed: 0x" << std::hex << hr);
            return false;
        }
        // MF's internal threads will touch the device — enable the
        // built-in mutex so concurrent CopyResource (our thread) and
        // VideoProcessorBlt (MF thread) don't trip the debug layer.
        winrt::com_ptr<ID3D11Multithread> mt;
        if (SUCCEEDED(m_d3dDevice->QueryInterface(
                __uuidof(ID3D11Multithread), mt.put_void()))) {
            mt->SetMultithreadProtected(TRUE);
        }
        try {
            m_winrtDevice = CreateDirect3DDevice(m_d3dDevice.get());
        } catch (const winrt::hresult_error& e) {
            RECORDER_LOG("CreateDirect3DDevice failed: "
                         << winrt::to_string(e.message()));
            return false;
        }
        return true;
    }

    bool CreateDxgiDeviceManager()
    {
        HRESULT hr = MFCreateDXGIDeviceManager(&m_dxgiResetToken,
                                               m_dxgiManager.put());
        if (FAILED(hr)) {
            RECORDER_LOG("MFCreateDXGIDeviceManager failed: 0x"
                         << std::hex << hr);
            return false;
        }
        hr = m_dxgiManager->ResetDevice(m_d3dDevice.get(), m_dxgiResetToken);
        if (FAILED(hr)) {
            RECORDER_LOG("ResetDevice failed: 0x" << std::hex << hr);
            return false;
        }
        return true;
    }

    bool OpenCapture(HWND hwnd)
    {
        auto interopFactory = winrt::get_activation_factory<
            winrt::GraphicsCaptureItem, ::IGraphicsCaptureItemInterop>();
        winrt::GraphicsCaptureItem item{ nullptr };
        HRESULT hr = interopFactory->CreateForWindow(
            hwnd, winrt::guid_of<winrt::GraphicsCaptureItem>(),
            winrt::put_abi(item));
        if (FAILED(hr) || !item) {
            RECORDER_LOG("CreateForWindow failed: 0x" << std::hex << hr);
            return false;
        }
        m_item = item;
        m_size = m_item.Size();
        if (m_size.Width <= 0 || m_size.Height <= 0) {
            RECORDER_LOG("window has zero size — minimised? aborting");
            return false;
        }

        m_framePool = winrt::Direct3D11CaptureFramePool::CreateFreeThreaded(
            m_winrtDevice,
            winrt::DirectXPixelFormat::B8G8R8A8UIntNormalized, 2, m_size);
        m_session = m_framePool.CreateCaptureSession(m_item);
        try { m_session.IsCursorCaptureEnabled(m_showCursor); } catch (...) {}
        try { m_session.IsBorderRequired(false); } catch (...) {}
        // Win11 24H2+: excludes overlay windows owned by other
        // processes (Start Menu, notification flyout, taskbar acrylic)
        // from per-window capture. Best-effort on older SDKs.
        try { m_session.IncludeSecondaryWindows(false); } catch (...) {}
        return true;
    }

    bool BuildVideoOutputType(winrt::com_ptr<IMFMediaType>& out)
    {
        const UINT32 w = (UINT32)m_size.Width;
        const UINT32 h = (UINT32)m_size.Height;
        // Bitrate model copied from recorder.mm: width*height*60*4 / 100
        // — ~4% of raw BGRA bandwidth, which lands around 8 Mbps at
        // 1080p60 and 32 Mbps at 4K60. Good quality without bloating
        // long capture sessions.
        const UINT32 bitrate = (w * h * 60u * 4u) / 100u;

        if (FAILED(MFCreateMediaType(out.put()))) return false;
        out->SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Video);
        out->SetGUID(MF_MT_SUBTYPE, MFVideoFormat_H264);
        out->SetUINT32(MF_MT_AVG_BITRATE, bitrate);
        out->SetUINT32(MF_MT_INTERLACE_MODE, MFVideoInterlace_Progressive);
        MFSetAttributeSize (out.get(), MF_MT_FRAME_SIZE, w, h);
        MFSetAttributeRatio(out.get(), MF_MT_FRAME_RATE, 60, 1);
        MFSetAttributeRatio(out.get(), MF_MT_PIXEL_ASPECT_RATIO, 1, 1);
        return true;
    }

    bool BuildAudioOutputType(winrt::com_ptr<IMFMediaType>& out)
    {
        if (FAILED(MFCreateMediaType(out.put()))) return false;
        // 192 kbps — highest the MS AAC encoder MFT supports for
        // 2-channel @ 48000 Hz (the documented set is 96/128/160/192).
        // recorder.mm uses 256 kbps because the macOS AAC encoder
        // accepts a broader range; we can't match that on Windows.
        constexpr UINT32 kAacBytesPerSec = 24000;
        out->SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
        out->SetGUID(MF_MT_SUBTYPE, MFAudioFormat_AAC);
        out->SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
        out->SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, SHM_AUDIO_SAMPLE_RATE);
        out->SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, m_audioChannels);
        out->SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND, kAacBytesPerSec);
        out->SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT, 1);
        // Required by the MS AAC encoder MFT — without these the encoder
        // rejects the output type during SetInputMediaType negotiation.
        out->SetUINT32(MF_MT_AAC_PAYLOAD_TYPE, 0);                 // raw AAC
        out->SetUINT32(MF_MT_AAC_AUDIO_PROFILE_LEVEL_INDICATION, 0x29);  // AAC-LC
        return true;
    }

    // Fragmented MP4 sink during recording — each moof+mdat is
    // self-contained, so an interrupted recording stays valid up to
    // the last fragment. On Stop we soft-remux the intermediate
    // .frag file (at m_fragmentedPath) into a standard moov-at-end
    // MP4 at m_filePath. Streams are auto-discovered from the sink:
    // video = stream 0, audio = stream 1 (if present).
    bool CreateSinkWriterAndStreams()
    {
        winrt::com_ptr<IMFMediaType> videoOut;
        if (!BuildVideoOutputType(videoOut)) return false;

        winrt::com_ptr<IMFMediaType> audioOut;
        if (m_audioReader.valid()) {
            if (!BuildAudioOutputType(audioOut)) {
                RECORDER_LOG("audio output type build failed — video-only");
                audioOut = nullptr;
            }
        }

        winrt::com_ptr<IMFByteStream> byteStream;
        HRESULT hr = MFCreateFile(
            MF_ACCESSMODE_WRITE, MF_OPENMODE_DELETE_IF_EXIST,
            MF_FILEFLAGS_NONE, m_fragmentedPath.c_str(), byteStream.put());
        if (FAILED(hr)) {
            RECORDER_LOG("MFCreateFile failed: 0x" << std::hex << hr);
            return false;
        }

        winrt::com_ptr<IMFMediaSink> sink;
        hr = MFCreateFMPEG4MediaSink(byteStream.get(),
                                      videoOut.get(), audioOut.get(),
                                      sink.put());
        if (FAILED(hr)) {
            RECORDER_LOG("MFCreateFMPEG4MediaSink failed: 0x"
                         << std::hex << hr);
            return false;
        }

        winrt::com_ptr<IMFAttributes> writerAttrs;
        if (FAILED(MFCreateAttributes(writerAttrs.put(), 4))) return false;
        writerAttrs->SetUnknown(MF_SINK_WRITER_D3D_MANAGER,
                                 m_dxgiManager.get());
        writerAttrs->SetUINT32(MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS, TRUE);
        writerAttrs->SetUINT32(MF_LOW_LATENCY, FALSE);
        writerAttrs->SetUINT32(MF_SINK_WRITER_DISABLE_THROTTLING, TRUE);

        hr = MFCreateSinkWriterFromMediaSink(sink.get(),
                                              writerAttrs.get(),
                                              m_writer.put());
        if (FAILED(hr)) {
            RECORDER_LOG("MFCreateSinkWriterFromMediaSink failed: 0x"
                         << std::hex << hr);
            return false;
        }

        m_videoStreamIdx = 0;
        if (audioOut) {
            m_audioStreamIdx = 1;
            m_hasAudioStream = true;
        }
        return true;
    }

    bool ConfigureVideoInput()
    {
        const UINT32 w = (UINT32)m_size.Width;
        const UINT32 h = (UINT32)m_size.Height;
        winrt::com_ptr<IMFMediaType> in;
        if (FAILED(MFCreateMediaType(in.put()))) return false;
        in->SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Video);
        in->SetGUID(MF_MT_SUBTYPE, MFVideoFormat_ARGB32);  // = BGRA byte order
        in->SetUINT32(MF_MT_INTERLACE_MODE, MFVideoInterlace_Progressive);
        MFSetAttributeSize (in.get(), MF_MT_FRAME_SIZE, w, h);
        MFSetAttributeRatio(in.get(), MF_MT_FRAME_RATE, 60, 1);
        MFSetAttributeRatio(in.get(), MF_MT_PIXEL_ASPECT_RATIO, 1, 1);
        if (FAILED(m_writer->SetInputMediaType(m_videoStreamIdx,
                                                in.get(), nullptr))) {
            RECORDER_LOG("SetInputMediaType(video) failed");
            return false;
        }
        return true;
    }

    bool ConfigureAudioInput()
    {
        // S16 PCM interleaved. The MS AAC encoder MFT only accepts
        // PCM (not float) as input, and the sink writer's auto-MFT
        // chain doesn't insert a float-to-PCM resampler reliably
        // here — supplying PCM directly avoids the negotiation
        // failure. We convert in DrainAudio.
        winrt::com_ptr<IMFMediaType> in;
        if (FAILED(MFCreateMediaType(in.put()))) return false;
        in->SetGUID(MF_MT_MAJOR_TYPE, MFMediaType_Audio);
        in->SetGUID(MF_MT_SUBTYPE, MFAudioFormat_PCM);
        in->SetUINT32(MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
        in->SetUINT32(MF_MT_AUDIO_SAMPLES_PER_SECOND, SHM_AUDIO_SAMPLE_RATE);
        in->SetUINT32(MF_MT_AUDIO_NUM_CHANNELS, m_audioChannels);
        in->SetUINT32(MF_MT_AUDIO_BLOCK_ALIGNMENT,
                      m_audioChannels * sizeof(int16_t));
        in->SetUINT32(MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
                      SHM_AUDIO_SAMPLE_RATE * m_audioChannels * sizeof(int16_t));
        const HRESULT setAudioInHr = m_writer->SetInputMediaType(
            m_audioStreamIdx, in.get(), nullptr);
        if (FAILED(setAudioInHr)) {
            RECORDER_LOG("SetInputMediaType(audio) failed: 0x"
                         << std::hex << setAudioInHr);
            return false;
        }
        return true;
    }

    // Elects one of the WGC video callback and the audio-pump thread
    // to stamp m_sessionStart100ns; the loser spins on yield until the
    // winner publishes kStarted.
    bool MaybeStartSessionAt(LONGLONG ts100ns)
    {
        uint8_t expected = kIdle;
        if (!m_writerState.compare_exchange_strong(
                expected, kStarting,
                std::memory_order_acq_rel, std::memory_order_acquire)) {
            while (expected == kStarting) {
                std::this_thread::yield();
                expected = m_writerState.load(std::memory_order_acquire);
            }
            return expected == kStarted;
        }
        m_sessionStart100ns = ts100ns;
        m_writerState.store(kStarted, std::memory_order_release);
        return true;
    }

    // ─── Video path ───────────────────────────────────────────────────

    void OnFrameArrived(const winrt::Direct3D11CaptureFramePool& sender,
                        const winrt::IInspectable&)
    {
        std::lock_guard<std::mutex> lk(m_callbackMutex);
        if (!m_running.load()) return;

        auto frame = sender.TryGetNextFrame();
        if (!frame) return;
        const auto size = frame.ContentSize();
        if (size.Width <= 0 || size.Height <= 0) return;

        // Resize handling — same as spout_publisher: recreate the pool
        // and skip this frame. The sink writer was configured with the
        // initial size; we'd need a stream-restart to honour a resize
        // mid-recording, so we just keep encoding the original size.
        // Window-resize-while-recording is a rare path; document it
        // and move on.
        if (size.Width != m_size.Width || size.Height != m_size.Height) {
            sender.Recreate(m_winrtDevice,
                            winrt::DirectXPixelFormat::B8G8R8A8UIntNormalized,
                            2, m_size);
            return;
        }

        winrt::com_ptr<ID3D11Texture2D> wgcTex;
        try { wgcTex = GetFrameTexture(frame); }
        catch (const winrt::hresult_error&) { return; }
        if (!wgcTex) return;

        // WGC recycles its pool texture when `frame` goes out of scope,
        // but the encoder MFT may hold the sample for several frames.
        // CopyResource into our own texture so it survives recycling.
        winrt::com_ptr<ID3D11Texture2D> ownedTex;
        if (!AllocFrameTexture(size, ownedTex)) return;
        m_d3dContext->CopyResource(ownedTex.get(), wgcTex.get());

        // QPC delivery time. We deliberately avoid frame.SystemRelativeTime
        // here — measured empirically it's ~27ms *ahead* of QPC at the
        // callback, which means it lives in a different time domain
        // (presentation time, or a different epoch). Mixing it with
        // the audio anchor (which uses NowIn100ns / QPC) silently
        // injects a constant offset into AV sync.
        const LONGLONG nowQpc = NowIn100ns();
        if (!MaybeStartSessionAt(nowQpc)) return;
        if (!m_videoFirstPtsLogged) {
            RECORDER_LOG("video first frame: nowQpc=" << nowQpc
                         << " sessionStart=" << m_sessionStart100ns
                         << " ptsMs=" << (nowQpc - m_sessionStart100ns) / 10000);
            m_videoFirstPtsLogged = true;
        }
        const LONGLONG pts = nowQpc - m_sessionStart100ns;
        // 60fps fixed-cadence duration (~16.67ms).
        constexpr LONGLONG kFrameDuration100ns = 10000000LL / 60LL;

        winrt::com_ptr<IMFMediaBuffer> buf;
        HRESULT hr = MFCreateDXGISurfaceBuffer(
            __uuidof(ID3D11Texture2D), ownedTex.get(), 0, FALSE, buf.put());
        if (FAILED(hr)) {
            RECORDER_LOG("MFCreateDXGISurfaceBuffer failed: 0x"
                         << std::hex << hr);
            return;
        }

        // Surface buffer needs its 2D buffer length set explicitly —
        // it's zero-initialised, and a zero-length buffer makes the
        // encoder reject the sample.
        winrt::com_ptr<IMF2DBuffer> buf2d;
        if (SUCCEEDED(buf->QueryInterface(__uuidof(IMF2DBuffer),
                                           buf2d.put_void()))) {
            DWORD len = 0;
            buf2d->GetContiguousLength(&len);
            buf->SetCurrentLength(len);
        }

        winrt::com_ptr<IMFSample> sample;
        if (FAILED(MFCreateSample(sample.put()))) return;
        sample->AddBuffer(buf.get());
        sample->SetSampleTime(pts);
        sample->SetSampleDuration(kFrameDuration100ns);

        std::lock_guard<std::mutex> wlk(m_writerMutex);
        hr = m_writer->WriteSample(m_videoStreamIdx, sample.get());
        if (FAILED(hr)) {
            // MF_E_SAMPLE_HAS_TOO_LARGE_TIMESTAMP / encoder hiccups
            // shouldn't kill the whole recording — log and keep going.
            RECORDER_LOG("WriteSample(video) failed: 0x" << std::hex << hr);
        }
    }

    bool AllocFrameTexture(const winrt::SizeInt32& size,
                           winrt::com_ptr<ID3D11Texture2D>& out)
    {
        D3D11_TEXTURE2D_DESC desc = {};
        desc.Width            = (UINT)size.Width;
        desc.Height           = (UINT)size.Height;
        desc.MipLevels        = 1;
        desc.ArraySize        = 1;
        desc.Format           = DXGI_FORMAT_B8G8R8A8_UNORM;
        desc.SampleDesc.Count = 1;
        desc.Usage            = D3D11_USAGE_DEFAULT;
        desc.BindFlags        = D3D11_BIND_RENDER_TARGET
                              | D3D11_BIND_SHADER_RESOURCE;
        // MISC_SHARED would let other processes consume the texture; not
        // needed here — only MF on our own device sees it.
        return SUCCEEDED(m_d3dDevice->CreateTexture2D(&desc, nullptr,
                                                       out.put()));
    }

    void OnItemClosed(const winrt::GraphicsCaptureItem&,
                      const winrt::IInspectable&)
    {
        RECORDER_LOG("capture item closed by OS");
        m_running.store(false);
    }

    // ─── Audio path ───────────────────────────────────────────────────

    void AudioPumpLoop()
    {
        // 5ms cadence via a high-resolution waitable timer. Plain
        // sleep_for is at the mercy of the system timer (15.6 ms default
        // on Windows without timeBeginPeriod), which would drift the
        // pump to 3× the intended cadence and cause audio reader gaps.
        // CREATE_WAITABLE_TIMER_HIGH_RESOLUTION needs Win10 1803+; if it
        // fails we fall back to sleep_for.
        HANDLE timer = CreateWaitableTimerExW(
            nullptr, nullptr,
            CREATE_WAITABLE_TIMER_MANUAL_RESET
                | CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,
            TIMER_ALL_ACCESS);
        constexpr LONGLONG kPeriod100ns = -5LL * 10000LL;  // 5ms, relative
        if (timer) {
            LARGE_INTEGER due; due.QuadPart = kPeriod100ns;
            SetWaitableTimer(timer, &due, 5 /*ms period*/,
                             nullptr, nullptr, FALSE);
        }
        while (!m_audioThreadStop.load()) {
            DrainAudio();
            if (timer) {
                WaitForSingleObject(timer, 5);
            } else {
                using namespace std::chrono_literals;
                std::this_thread::sleep_for(5ms);
            }
        }
        if (timer) {
            CancelWaitableTimer(timer);
            CloseHandle(timer);
        }
        // One last ungated drain after the stop flag — pick up any tail
        // frames between the final tick and join.
        DrainAudio();
    }

    void DrainAudio()
    {
        if (!m_hasAudioStream) return;

        for (int iter = 0; iter < 8; ++iter) {
            uint64_t gap = 0;
            uint32_t got = m_audioReader.pull(
                m_audioPullBuf.data(), kAudioPullFrames, &gap);
            if (gap > 0) {
                RECORDER_LOG("audio reader gap: " << gap
                             << " frames dropped");
            }
            if (got == 0) break;

            const uint64_t readerPos = m_audioReader.last_read_position();
            if (m_audioAnchor100ns == LLONG_MIN) {
                m_audioAnchor100ns = NowIn100ns();
                m_audioAnchorFrame = readerPos - got;
            }
            if (!MaybeStartSessionAt(m_audioAnchor100ns)) return;

            const uint64_t startFrame = readerPos - got;
            const LONGLONG anchorRel = m_audioAnchor100ns - m_sessionStart100ns;

            if (!m_audioFirstPtsLogged) {
                RECORDER_LOG("audio first pull: anchorQpc="
                             << m_audioAnchor100ns
                             << " sessionStart=" << m_sessionStart100ns
                             << " anchorRelMs=" << (anchorRel / 10000)
                             << " firstGot=" << got);
                m_audioFirstPtsLogged = true;
            }

            // The fMP4 sink normalises each stream's first written sample
            // to PTS 0, collapsing any anchor offset between recorder
            // start and the first pulled frame. To preserve the offset we explicitly
            // pre-fill the audio stream with silence from PTS 0 to
            // PTS anchorRel — the first sample is then at 0 and the
            // sink has nothing to collapse.
            if (!m_audioPrerollWritten) {
                m_audioPrerollWritten = true;
                if (anchorRel > 0) {
                    std::vector<float> silence(
                        kAudioPullFrames * m_audioChannels, 0.0f);
                    constexpr LONGLONG kChunk100ns =
                        (LONGLONG)kAudioPullFrames * 10000000LL
                        / SHM_AUDIO_SAMPLE_RATE;
                    LONGLONG silencePts = 0;
                    while (silencePts + kChunk100ns <= anchorRel) {
                        WriteAudioSample(silence.data(), kAudioPullFrames,
                                          silencePts, kChunk100ns);
                        silencePts += kChunk100ns;
                    }
                    const LONGLONG remaining = anchorRel - silencePts;
                    if (remaining > 0) {
                        const uint32_t framesLast = (uint32_t)(
                            remaining * SHM_AUDIO_SAMPLE_RATE / 10000000LL);
                        if (framesLast > 0) {
                            WriteAudioSample(silence.data(), framesLast,
                                              silencePts, remaining);
                        }
                    }
                }
            }

            const LONGLONG frameOffset = (LONGLONG)(startFrame - m_audioAnchorFrame);
            const LONGLONG pts = anchorRel
                + (frameOffset * 10000000LL) / SHM_AUDIO_SAMPLE_RATE;
            const LONGLONG duration =
                ((LONGLONG)got * 10000000LL) / SHM_AUDIO_SAMPLE_RATE;

            WriteAudioSample(m_audioPullBuf.data(), got, pts, duration);
        }
    }

    void WriteAudioSample(const float* data, uint32_t frames,
                          LONGLONG pts, LONGLONG duration)
    {
        // Convert float [-1, 1] → S16 PCM for the AAC encoder.
        const DWORD bytes = frames * m_audioChannels * sizeof(int16_t);
        winrt::com_ptr<IMFMediaBuffer> buf;
        if (FAILED(MFCreateMemoryBuffer(bytes, buf.put()))) return;
        BYTE* dst = nullptr;
        if (FAILED(buf->Lock(&dst, nullptr, nullptr))) return;
        int16_t* dst16 = reinterpret_cast<int16_t*>(dst);
        const uint32_t samples = frames * m_audioChannels;
        for (uint32_t i = 0; i < samples; ++i) {
            const float f = std::clamp(data[i], -1.0f, 1.0f);
            dst16[i] = static_cast<int16_t>(std::lrintf(f * 32767.0f));
        }
        buf->Unlock();
        buf->SetCurrentLength(bytes);

        winrt::com_ptr<IMFSample> sample;
        if (FAILED(MFCreateSample(sample.put()))) return;
        sample->AddBuffer(buf.get());
        sample->SetSampleTime(pts);
        sample->SetSampleDuration(duration);

        std::lock_guard<std::mutex> lk(m_writerMutex);
        HRESULT hr = m_writer->WriteSample(m_audioStreamIdx, sample.get());
        if (FAILED(hr)) {
            RECORDER_LOG("WriteSample(audio) failed: 0x" << std::hex << hr);
        }
    }

private:
    // D3D11
    winrt::com_ptr<ID3D11Device>           m_d3dDevice;
    winrt::com_ptr<ID3D11DeviceContext>    m_d3dContext;
    winrt::IDirect3DDevice                 m_winrtDevice{ nullptr };
    winrt::com_ptr<IMFDXGIDeviceManager>   m_dxgiManager;
    UINT                                   m_dxgiResetToken{ 0 };

    // WGC
    winrt::GraphicsCaptureItem             m_item{ nullptr };
    winrt::Direct3D11CaptureFramePool      m_framePool{ nullptr };
    winrt::GraphicsCaptureSession          m_session{ nullptr };
    winrt::event_token                     m_frameArrivedToken{};
    winrt::event_token                     m_itemClosedToken{};
    winrt::SizeInt32                       m_size{};

    // Media Foundation
    winrt::com_ptr<IMFSinkWriter>          m_writer;
    DWORD                                  m_videoStreamIdx{ 0 };
    DWORD                                  m_audioStreamIdx{ 0 };
    bool                                   m_hasAudioStream{ false };
    bool                                   m_mfStarted{ false };

    // Audio
    shm_audio_buffer_reader                m_audioReader;
    // The tap's live channel count (the device's, up to the ring's ceiling):
    // the recording's channels and the stride of every pulled frame.
    uint32_t                               m_audioChannels = 0;
    std::thread                            m_audioThread;
    std::atomic<bool>                      m_audioThreadStop{ false };
    LONGLONG                               m_audioAnchor100ns{ LLONG_MIN };
    uint64_t                               m_audioAnchorFrame{ 0 };
    // Allocated once in Start so DrainAudio's 5ms tick doesn't churn.
    static constexpr uint32_t              kAudioPullFrames = 1024;
    std::vector<float>                     m_audioPullBuf;

    // Lifecycle / synchronisation
    std::atomic<bool>                      m_running{ false };
    std::atomic<uint8_t>                   m_writerState{ kIdle };
    LONGLONG                               m_sessionStart100ns{ 0 };
    std::mutex                             m_writerMutex;   // sink writer
    std::mutex                             m_callbackMutex; // WGC re-entrancy

    std::wstring                           m_filePath;        // final, user-facing
    std::wstring                           m_fragmentedPath;  // intermediate .frag, soft-remuxed on Stop
    bool                                   m_showCursor{ false };
    bool                                   m_powerStateSet{ false };
    bool                                   m_videoFirstPtsLogged{ false };
    bool                                   m_audioFirstPtsLogged{ false };
    bool                                   m_audioPrerollWritten{ false };
};

std::unique_ptr<SonicPiSessionRecorder> g_recorder;
std::mutex                              g_recorderMutex;

std::wstring Utf8ToWide(const std::string& s)
{
    if (s.empty()) return {};
    int n = MultiByteToWideChar(CP_UTF8, 0, s.data(), (int)s.size(),
                                 nullptr, 0);
    std::wstring w(n, L'\0');
    MultiByteToWideChar(CP_UTF8, 0, s.data(), (int)s.size(), w.data(), n);
    return w;
}

} // namespace

namespace SonicPi {

bool startSessionRecording(void* hwndPtr, const std::string& filePath,
                           bool showCursor, shm_audio_buffer* audioSlot)
{
    if (!hwndPtr) {
        RECORDER_LOG("null HWND");
        return false;
    }
    HWND hwnd = static_cast<HWND>(hwndPtr);

    SonicPi::wgc::EnsureApartment();
    if (!winrt::GraphicsCaptureSession::IsSupported()) {
        RECORDER_LOG("Windows.Graphics.Capture not supported "
                     "(requires Windows 10 1903+)");
        return false;
    }

    std::lock_guard<std::mutex> lk(g_recorderMutex);
    if (g_recorder) {
        g_recorder->Stop();
        g_recorder.reset();
    }
    auto rec = std::make_unique<SonicPiSessionRecorder>();
    if (!rec->Start(hwnd, Utf8ToWide(filePath), showCursor, audioSlot)) {
        return false;
    }
    g_recorder = std::move(rec);
    return true;
}

void stopSessionRecording()
{
    std::lock_guard<std::mutex> lk(g_recorderMutex);
    if (g_recorder) {
        g_recorder->Stop();
        g_recorder.reset();
    }
}

bool isSessionRecording()
{
    std::lock_guard<std::mutex> lk(g_recorderMutex);
    return g_recorder && g_recorder->IsRunning();
}

void setRecordShowCursor(bool showCursor)
{
    std::lock_guard<std::mutex> lk(g_recorderMutex);
    if (g_recorder) g_recorder->SetShowCursor(showCursor);
}

} // namespace SonicPi
