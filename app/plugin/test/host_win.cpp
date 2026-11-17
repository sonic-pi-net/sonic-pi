// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// A Windows host for "Sonic Pi.vst3", as test/host.mm is the Mac's, smaller: it loads the bundle's binary for its
// own architecture, starts the plugin's audio as a host does (so the editor has its instance), opens the editor in
// a window of its own and runs the window's messages for a while, rendering silence as a host would. With
// SONIC_PI_PLUGIN_PROBE=1 the page reports what it sees to the plugin's log (test/probe.js), SONIC_PI_PLUGIN_LOG
// names that log, and the caller reads it: in WebView2 the page loaded, is a secure context, is cross-origin
// isolated, and shows the editor.
//
//   host_win.exe "<bundle>.vst3" [seconds] [noeditor]
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#include <objbase.h>

#include "pluginterfaces/base/ipluginbase.h"
#include "pluginterfaces/gui/iplugview.h"
#include "pluginterfaces/gui/iplugviewcontentscalesupport.h"
#include "pluginterfaces/vst/ivstaudioprocessor.h"
#include "pluginterfaces/vst/ivstcomponent.h"
#include "pluginterfaces/vst/ivsteditcontroller.h"

#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

using namespace Steinberg;
using namespace Steinberg::Vst;

namespace {

int fail(const char* what)
{
    std::fprintf(stderr, "host_win: %s\n", what);
    return 1;
}

// The frame a host gives a view: this one does what the view asks.
class Frame : public IPlugFrame
{
public:
    explicit Frame(HWND window) : window(window) {}
    tresult PLUGIN_API queryInterface(const TUID, void** obj) override { *obj = nullptr; return kNoInterface; }
    uint32 PLUGIN_API addRef() override { return 1; }
    uint32 PLUGIN_API release() override { return 1; }
    tresult PLUGIN_API resizeView(IPlugView* view, ViewRect* size) override
    {
        RECT r = { 0, 0, size->getWidth(), size->getHeight() };
        AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
        SetWindowPos(window, nullptr, 0, 0, r.right - r.left, r.bottom - r.top, SWP_NOMOVE | SWP_NOZORDER);
        return view->onSize(size);
    }
    HWND window;
};

} // namespace

int wmain(int argc, wchar_t** argv)
{
    if (argc < 2) return fail("usage: host_win <bundle>.vst3 [seconds]");
    const double seconds = argc > 2 ? _wtof(argv[2]) : 10.0;
    const bool editor = !(argc > 3 && std::wcscmp(argv[3], L"noeditor") == 0);
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    if (FAILED(OleInitialize(nullptr))) return fail("OleInitialize");

#if defined(_M_ARM64)
    const wchar_t* folder = L"arm64-win";
#else
    const wchar_t* folder = L"x86_64-win";
#endif
    std::wstring bundle = argv[1];
    std::wstring binary = bundle + L"\\Contents\\" + folder + L"\\Sonic Pi.vst3";
    HMODULE module = LoadLibraryW(binary.c_str());
    if (!module) return fail("cannot load the bundle's binary for this architecture");
    using GetFactory = IPluginFactory* (PLUGIN_API*)();
    auto getFactory = reinterpret_cast<GetFactory>(GetProcAddress(module, "GetPluginFactory"));
    if (!getFactory) return fail("no GetPluginFactory");
    IPluginFactory* factory = getFactory();
    if (!factory) return fail("no factory");

    // The first audio module class.
    PClassInfo info = {};
    bool found = false;
    for (int32 i = 0; i < factory->countClasses(); ++i) {
        if (factory->getClassInfo(i, &info) == kResultOk && std::strcmp(info.category, kVstAudioEffectClass) == 0) { found = true; break; }
    }
    if (!found) return fail("no audio module class");
    std::printf("plugin: %s\n", info.name);

    IComponent* component = nullptr;
    if (factory->createInstance(info.cid, IComponent_iid, reinterpret_cast<void**>(&component)) != kResultOk || !component)
        return fail("createInstance");
    if (component->initialize(nullptr) != kResultOk) return fail("initialize");
    IAudioProcessor* processor = nullptr;
    component->queryInterface(IAudioProcessor_iid, reinterpret_cast<void**>(&processor));
    IEditController* controller = nullptr;
    component->queryInterface(IEditController_iid, reinterpret_cast<void**>(&controller));
    if (!processor || !controller) return fail("no processor or no controller");

    const int32 frames = 512;
    ProcessSetup setup = { kRealtime, kSample32, frames, 48000.0 };
    if (processor->setupProcessing(setup) != kResultOk) return fail("setupProcessing");
    component->activateBus(kAudio, kOutput, 0, true);
    if (component->setActive(true) != kResultOk) return fail("setActive");
    processor->setProcessing(true);

    IPlugView* view = controller->createView(ViewType::kEditor);
    if (!view) return fail("no editor");
    ViewRect size;
    view->getSize(&size);
    RECT r = { 0, 0, size.getWidth(), size.getHeight() };
    AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
    HWND window = CreateWindowExW(0, L"STATIC", L"Sonic Pi (host_win)", WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                                  CW_USEDEFAULT, CW_USEDEFAULT, r.right - r.left, r.bottom - r.top, nullptr, nullptr, nullptr, nullptr);
    if (!window) return fail("CreateWindow");
    Frame frame(window);
    view->setFrame(&frame);
    const UINT dpi = GetDpiForWindow(window);
    IPlugViewContentScaleSupport* scale = nullptr;
    if (view->queryInterface(IPlugViewContentScaleSupport_iid, reinterpret_cast<void**>(&scale)) == kResultOk && scale) {
        scale->setContentScaleFactor(dpi / 96.0f);
        scale->release();
    }
    if (editor && view->attached(window, kPlatformTypeHWND) != kResultOk) return fail("attached");
    view->getSize(&size);
    std::printf("editor: %dx%d pixels at %u dpi\n", size.getWidth(), size.getHeight(), dpi);

    // Silence in, rendered out, a block at a time as the clock allows, with the window's messages between.
    std::vector<float> left(frames), right(frames), inL(frames), inR(frames);
    float* outs[2] = { left.data(), right.data() };
    float* ins[2] = { inL.data(), inR.data() };
    AudioBusBuffers out = {}, in = {};
    out.numChannels = 2; out.channelBuffers32 = outs;
    in.numChannels = 2; in.channelBuffers32 = ins;
    ProcessData data;
    data.processMode = kRealtime;
    data.symbolicSampleSize = kSample32;
    data.numSamples = frames;
    data.numInputs = 1; data.inputs = &in;
    data.numOutputs = 1; data.outputs = &out;
    LARGE_INTEGER freq, start, now;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&start);
    long long rendered = 0;
    for (;;) {
        MSG msg;
        while (PeekMessageW(&msg, nullptr, 0, 0, PM_REMOVE)) { TranslateMessage(&msg); DispatchMessageW(&msg); }
        QueryPerformanceCounter(&now);
        const double t = double(now.QuadPart - start.QuadPart) / double(freq.QuadPart);
        if (t >= seconds) break;
        while ((rendered + frames) / 48000.0 <= t) { processor->process(data); rendered += frames; }
        MsgWaitForMultipleObjects(0, nullptr, FALSE, 2, QS_ALLINPUT);
    }
    std::printf("rendered %lld frames\n", rendered);

    if (editor) view->removed();
    view->release();
    processor->setProcessing(false);
    component->setActive(false);
    DestroyWindow(window);
    controller->release();
    processor->release();
    component->terminate();
    component->release();
    factory->release();
    OleUninitialize();
    return 0;
}
