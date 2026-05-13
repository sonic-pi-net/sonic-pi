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

#include "mp4_soft_remux.h"

#include <Windows.h>
#include <mfapi.h>
#include <mferror.h>
#include <mfidl.h>
#include <mfreadwrite.h>

#include <winrt/base.h>

#include <iostream>
#include <map>
#include <sstream>

#define REMUX_LOG(expr) do {                                          \
    std::ostringstream _ss; _ss << "[remux] " << expr << "\n";        \
    std::cerr << _ss.str();                                            \
    OutputDebugStringA(_ss.str().c_str());                             \
} while (0)

namespace SonicPi {

bool remuxToStandardMp4(const std::wstring& inputPath,
                        const std::wstring& outputPath)
{
    winrt::com_ptr<IMFSourceReader> reader;
    HRESULT hr = MFCreateSourceReaderFromURL(
        inputPath.c_str(), nullptr, reader.put());
    if (FAILED(hr)) {
        REMUX_LOG("MFCreateSourceReaderFromURL failed: 0x" << std::hex << hr);
        return false;
    }

    winrt::com_ptr<IMFSinkWriter> writer;
    hr = MFCreateSinkWriterFromURL(
        outputPath.c_str(), nullptr, nullptr, writer.put());
    if (FAILED(hr)) {
        REMUX_LOG("MFCreateSinkWriterFromURL failed: 0x" << std::hex << hr);
        return false;
    }

    // Source-reader → sink-writer index mapping. Reader stream indices
    // are not guaranteed to be 0,1,...; the writer's are assigned by
    // AddStream in our enumeration order.
    std::map<DWORD, DWORD> streamMap;

    for (DWORD readerIdx = 0; ; ++readerIdx) {
        winrt::com_ptr<IMFMediaType> nativeType;
        hr = reader->GetNativeMediaType(readerIdx, 0, nativeType.put());
        if (hr == MF_E_INVALIDSTREAMNUMBER) break;
        if (FAILED(hr)) {
            REMUX_LOG("GetNativeMediaType(" << readerIdx
                      << ") failed: 0x" << std::hex << hr);
            return false;
        }

        // Tell the reader to deliver this stream in its native encoded
        // form (no decode). SetCurrentMediaType to the native type
        // bypasses the default decoder insertion.
        reader->SetStreamSelection(readerIdx, TRUE);
        hr = reader->SetCurrentMediaType(readerIdx, nullptr, nativeType.get());
        if (FAILED(hr)) {
            REMUX_LOG("SetCurrentMediaType(reader, " << readerIdx
                      << ") failed: 0x" << std::hex << hr);
            continue;
        }

        DWORD writerIdx = 0;
        hr = writer->AddStream(nativeType.get(), &writerIdx);
        if (FAILED(hr)) {
            REMUX_LOG("writer AddStream failed for reader stream "
                      << readerIdx << ": 0x" << std::hex << hr);
            return false;
        }
        hr = writer->SetInputMediaType(writerIdx, nativeType.get(), nullptr);
        if (FAILED(hr)) {
            REMUX_LOG("writer SetInputMediaType failed for stream "
                      << writerIdx << ": 0x" << std::hex << hr);
            return false;
        }
        streamMap[readerIdx] = writerIdx;
    }

    if (streamMap.empty()) {
        REMUX_LOG("no streams to copy from " << inputPath.size() << " chars");
        return false;
    }

    hr = writer->BeginWriting();
    if (FAILED(hr)) {
        REMUX_LOG("BeginWriting failed: 0x" << std::hex << hr);
        return false;
    }

    // Per-stream sample count + first/last PTS for post-pass diagnostics.
    struct StreamStats { DWORD count = 0; LONGLONG first = -1, last = -1; };
    std::map<DWORD, StreamStats> stats;

    for (;;) {
        DWORD readerIdx = 0;
        DWORD flags = 0;
        LONGLONG ts = 0;
        winrt::com_ptr<IMFSample> sample;
        hr = reader->ReadSample(
            MF_SOURCE_READER_ANY_STREAM, 0,
            &readerIdx, &flags, &ts, sample.put());
        if (FAILED(hr)) {
            REMUX_LOG("ReadSample failed: 0x" << std::hex << hr);
            break;
        }
        if (flags & MF_SOURCE_READERF_ENDOFSTREAM) break;
        if (!sample) continue;

        auto it = streamMap.find(readerIdx);
        if (it == streamMap.end()) continue;

        // Preserve both time and duration. The sample already carries
        // both from ReadSample, but re-setting is defensive — some
        // source-reader paths null out duration on passthrough.
        sample->SetSampleTime(ts);
        LONGLONG dur = 0;
        if (SUCCEEDED(sample->GetSampleDuration(&dur)) && dur > 0) {
            sample->SetSampleDuration(dur);
        }

        auto& s = stats[readerIdx];
        if (s.count == 0) s.first = ts;
        s.last = ts;
        ++s.count;

        hr = writer->WriteSample(it->second, sample.get());
        if (FAILED(hr)) {
            REMUX_LOG("WriteSample failed (reader stream "
                      << readerIdx << "): 0x" << std::hex << hr);
            break;
        }
    }

    for (const auto& [readerIdx, s] : stats) {
        REMUX_LOG("stream " << readerIdx << ": " << s.count
                  << " samples, pts " << (s.first / 10000) << "ms .. "
                  << (s.last / 10000) << "ms");
    }

    hr = writer->Finalize();
    if (FAILED(hr)) {
        REMUX_LOG("Finalize failed: 0x" << std::hex << hr);
        return false;
    }

    return true;
}

} // namespace SonicPi
