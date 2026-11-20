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
//   host_win.exe "<bundle>.vst3" [seconds] [noeditor|fullscreen]
//
// fullscreen: the checks of the plugin's own full screen (core/win.cpp), as host.mm has them for the Mac's. It
// needs the probe and its log (both variables above), which is all it knows of the page: there is no running a
// script in WebView2 from here, so the page's button is clicked, and Escape pressed, by messages posted to
// WebView2's window, and what came of it read from the windows and from the probe's lines. `seconds` is then how
// long to wait for the page. Its exit code is the number of checks that failed.
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
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <sstream>
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

// ── What the full screen checks look at ─────────────────────────────────────
int failures = 0;

void check(const char* what, bool ok, const std::string& detail = {})
{
    if (!ok) ++failures;
    std::printf("%s  %s%s%s\n", ok ? "ok    " : "FAILED", what, detail.empty() ? "" : ": ", detail.c_str());
    std::fflush(stdout);
}

// The plugin's log so far (SONIC_PI_PLUGIN_LOG), which the plugin has open for appending.
std::string logText()
{
    const wchar_t* path = _wgetenv(L"SONIC_PI_PLUGIN_LOG");
    if (!path) return {};
    std::ifstream in(path, std::ios::binary);
    std::stringstream all;
    all << in.rdbuf();
    return all.str();
}

// The last of the log's lines with `text` in it, from `from` on; empty if none.
std::string lastLine(const std::string& text, size_t from = 0)
{
    const std::string log = logText();
    const size_t at = log.rfind(text);
    if (at == std::string::npos || at < from) return {};
    const size_t start = log.rfind('\n', at), end = log.find('\n', at);
    return log.substr(start == std::string::npos ? 0 : start + 1, (end == std::string::npos ? log.size() : end) - (start == std::string::npos ? 0 : start + 1));
}

// The number after "name": in a line of the probe's, or `none`; and whether "name":true is in it.
long numberIn(const std::string& line, const char* name, long none = -1)
{
    const size_t at = line.find(std::string("\"") + name + "\":");
    return at == std::string::npos ? none : std::strtol(line.c_str() + at + std::strlen(name) + 3, nullptr, 10);
}
bool trueIn(const std::string& line, const char* name)
{
    return line.find(std::string("\"") + name + "\":true") != std::string::npos;
}

HWND childOfClass(HWND parent, const wchar_t* name)
{
    struct Find { const wchar_t* name; HWND found; } find = { name, nullptr };
    EnumChildWindows(parent, [](HWND w, LPARAM l) -> BOOL {
        auto* f = reinterpret_cast<Find*>(l);
        wchar_t cls[64] = {};
        GetClassNameW(w, cls, 64);
        if (std::wcscmp(cls, f->name) != 0) return TRUE;
        f->found = w;
        return FALSE;
    }, reinterpret_cast<LPARAM>(&find));
    return find.found;
}

// The page's full screen window (core/win.cpp), if this thread has one showing.
HWND fullWindow()
{
    HWND found = nullptr;
    EnumThreadWindows(GetCurrentThreadId(), [](HWND w, LPARAM l) -> BOOL {
        wchar_t cls[64] = {};
        GetClassNameW(w, cls, 64);
        if (std::wcscmp(cls, L"SonicPiPluginFullScreen") != 0 || !IsWindowVisible(w)) return TRUE;
        *reinterpret_cast<HWND*>(l) = w;
        return FALSE;
    }, reinterpret_cast<LPARAM>(&found));
    return found;
}

std::string rectText(const RECT& r)
{
    return std::to_string(r.left) + "," + std::to_string(r.top) + " " + std::to_string(r.right - r.left) + "x" + std::to_string(r.bottom - r.top);
}

// A click of the left button in the web view under `parent`, at a point of the web view's own pixels: to the
// window Chromium takes the mouse in. False if there is no such window.
bool clickPage(HWND parent, long x, long y)
{
    HWND target = childOfClass(parent, L"Chrome_RenderWidgetHostHWND");
    if (!target) return false;
    const LPARAM at = MAKELPARAM(x, y);
    PostMessageW(target, WM_MOUSEMOVE, 0, at);
    PostMessageW(target, WM_LBUTTONDOWN, MK_LBUTTON, at);
    PostMessageW(target, WM_LBUTTONUP, 0, at);
    return true;
}

// A key down and up to the window that has the keyboard, if it is the web view's under `parent`; else to the
// window Chromium takes its keys in there.
bool pressKey(HWND parent, UINT vk)
{
    GUITHREADINFO info = {};
    info.cbSize = sizeof info;
    HWND target = GetGUIThreadInfo(GetCurrentThreadId(), &info) && info.hwndFocus && IsChild(parent, info.hwndFocus) ? info.hwndFocus : nullptr;
    if (!target) target = childOfClass(parent, L"Chrome_WidgetWin_1");
    if (!target) return false;
    const UINT scan = MapVirtualKeyW(vk, MAPVK_VK_TO_VSC);
    PostMessageW(target, WM_KEYDOWN, vk, 1 | (scan << 16));
    PostMessageW(target, WM_KEYUP, vk, 1 | (scan << 16) | (1u << 30) | (1u << 31));
    return true;
}

} // namespace

int wmain(int argc, wchar_t** argv)
{
    if (argc < 2) return fail("usage: host_win <bundle>.vst3 [seconds]");
    const double seconds = argc > 2 ? _wtof(argv[2]) : 10.0;
    const bool editor = !(argc > 3 && std::wcscmp(argv[3], L"noeditor") == 0);
    const bool fullScreen = argc > 3 && std::wcscmp(argv[3], L"fullscreen") == 0;
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
    const auto clock = [&] { QueryPerformanceCounter(&now); return double(now.QuadPart - start.QuadPart) / double(freq.QuadPart); };
    // The window's messages and the audio for `length` seconds, or until `done` says so. True if it did.
    const auto pump = [&](double length, const std::function<bool()>& done = nullptr) {
        const double until = clock() + length;
        for (;;) {
            MSG msg;
            while (PeekMessageW(&msg, nullptr, 0, 0, PM_REMOVE)) { TranslateMessage(&msg); DispatchMessageW(&msg); }
            if (done && done()) return true;
            const double t = clock();
            if (t >= until) return false;
            while ((rendered + frames) / 48000.0 <= t) { processor->process(data); rendered += frames; }
            MsgWaitForMultipleObjects(0, nullptr, FALSE, 2, QS_ALLINPUT);
        }
    };

    bool attached = editor;
    if (!fullScreen) {
        pump(seconds);
    } else if (!_wgetenv(L"SONIC_PI_PLUGIN_PROBE") || !_wgetenv(L"SONIC_PI_PLUGIN_LOG")) {
        check("the full screen checks have the probe and its log", false, "set SONIC_PI_PLUGIN_PROBE=1 and SONIC_PI_PLUGIN_LOG");
    } else {
        // Full screen is the plugin's own (core/win.cpp), as on the Mac (host.mm): the page goes into a window over
        // the whole monitor and comes back to the host's view.
        const char* const zen = "\"t\":\"zen\"";
        const auto web = [](HWND parent) { return FindWindowExW(parent, nullptr, L"WRY_WEBVIEW", nullptr); };
        const auto zenIs = [&](bool on, size_t from = 0) { const std::string l = lastLine(zen, from); return !l.empty() && trueIn(l, "on") == on; };
        const auto clickZen = [&](HWND parent) {
            const std::string l = lastLine(zen);
            return !l.empty() && clickPage(parent, numberIn(l, "x"), numberIn(l, "y"));
        };
        const auto webFills = [&](HWND parent) {
            RECT c = {}, w = {};
            GetClientRect(parent, &c);
            if (HWND v = web(parent)) GetClientRect(v, &w);
            return std::pair<bool, std::string>(web(parent) && EqualRect(&c, &w), "the view " + rectText(c) + ", the page " + rectText(w));
        };
        const bool ready = pump(seconds, [&] { return !lastLine("\"t\":\"ready\"").empty() && !lastLine(zen).empty(); });
        check("the page is ready, and says where its full screen button is", ready, lastLine(zen));
        pump(0.5);

        // 1. The button.
        size_t from = logText().size();
        check("the button can be clicked", clickZen(window));
        pump(3, [&] { return fullWindow() && zenIs(true, from); });
        pump(0.5);
        HWND full = fullWindow();
        RECT on = {}, whole = {};
        MONITORINFO mi = {};
        mi.cbSize = sizeof mi;
        GetMonitorInfoW(MonitorFromWindow(window, MONITOR_DEFAULTTONEAREST), &mi);
        whole = mi.rcMonitor;
        if (full) GetWindowRect(full, &on);
        check("the full screen button puts the page in a window over the whole monitor",
              full && EqualRect(&on, &whole) && web(full) && !web(window),
              full ? "window " + rectText(on) + ", monitor " + rectText(whole) : "no such window");
        check("which is not topmost, and is owned by the host's window",
              full && !(GetWindowLongPtrW(full, GWL_EXSTYLE) & WS_EX_TOPMOST) && GetWindow(full, GW_OWNER) == window);
        if (full) { const auto f = webFills(full); check("the page fills it", f.first, f.second); }
        // The keyboard: this thread's focus is in the page's window, and the page says it has the focus. A harness
        // started in the background may not be let in front of the application in use, which is said apart.
        GUITHREADINFO gi = {};
        gi.cbSize = sizeof gi;
        GetGUIThreadInfo(GetCurrentThreadId(), &gi);
        const std::string z = lastLine(zen, from);
        check("it has the keyboard and gives it to the page, whose button shows on",
              full && gi.hwndFocus && IsChild(full, gi.hwndFocus) && trueIn(z, "on") && trueIn(z, "focus"), z);
        std::printf("note    the full screen window %s the foreground window\n", full && GetForegroundWindow() == full ? "is" : "is NOT");
        std::printf("note    the page is %ldx%ld pixels in it\n", numberIn(z, "vw"), numberIn(z, "vh"));

        // 2. Escape, which the page leaves.
        from = logText().size();
        check("Escape can be pressed", full && pressKey(full, VK_ESCAPE));
        pump(3, [&] { return !fullWindow() && zenIs(false, from); });
        pump(0.5);
        check("Escape brings the page back to the host's view, and its button shows off",
              !fullWindow() && web(window) && zenIs(false, from), std::string(fullWindow() ? "window still there" : "window gone") + "; " + lastLine(zen, from));
        { const auto f = webFills(window); check("at the size of the host's view", f.first, f.second); }

        // 3. The button on, and the button again.
        from = logText().size();
        clickZen(window);
        pump(3, [&] { return fullWindow() && zenIs(true, from); });
        pump(0.5);
        full = fullWindow();
        check("the button takes it full screen a second time", full && web(full) && zenIs(true, from), lastLine(zen, from));
        from = logText().size();
        if (full) clickZen(full);
        pump(3, [&] { return !fullWindow() && zenIs(false, from); });
        pump(0.5);
        check("and the button again brings it back to the host's view",
              !fullWindow() && web(window) && zenIs(false, from), std::string(fullWindow() ? "window still there" : "window gone") + "; " + lastLine(zen, from));

        // 4. The editor closed while full screen: nothing is left on the display, and the web view goes with it.
        from = logText().size();
        clickZen(window);
        pump(3, [&] { return fullWindow() && zenIs(true, from); });
        pump(0.5);
        full = fullWindow();
        const bool was = full != nullptr;
        view->removed();
        attached = false;
        pump(0.5);
        check("an editor removed while full screen leaves no window and no web view",
              was && !fullWindow() && !IsWindow(full) && !web(window),
              std::string(was ? "" : "it was not full screen; ") + (IsWindow(full) ? "window still there" : "window gone") + (web(window) ? ", web view still in the host's" : ""));
        check("and the plugin says nothing went wrong", lastLine("panic").empty() && lastLine("could not").empty(), lastLine("panic") + lastLine("could not"));
        std::printf("%d failed\n", failures);
    }
    std::printf("rendered %lld frames\n", rendered);

    if (attached) view->removed();
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
    return failures;
}
