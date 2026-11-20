// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// The Win32 side of the editor, as mac.mm is the AppKit side: the main thread, for what the instance says, and the
// plugin's own path.
//
// Keys: WebView2 is a window of its own (its own process draws it), and while the page has the focus Windows sends
// keys to it, not to the host's window, so there is nothing to route as mac.mm must. The page has every key; the
// host's own shortcuts work when the host has the focus. A host's offer of a key through VST3 (onKeyDown) is
// declined, so the host keeps any it is offered. Ctrl-= / Ctrl-- / Ctrl-0 are WebView2's own zoom.
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>

#include <mutex>
#include <string>

extern "C" void sp_host_note(const char* text);

namespace {

struct Monitor {
    HWND host = nullptr;
    void* editor = nullptr;   // lib.rs's
};

// The main thread's mailbox: a message-only window, made on the main thread (a window's messages are its maker's
// thread's), while any editor is open. What the instance says is posted to it from the instance's threads.
constexpr UINT kWork = WM_APP + 1;
constexpr wchar_t kClass[] = L"SonicPiPluginMainThread";
std::mutex postLock;            // the window, against its going while a thread posts to it
HWND postWindow = nullptr;
int editorsOpen = 0;            // on the main thread

LRESULT CALLBACK postProc(HWND window, UINT message, WPARAM w, LPARAM l)
{
    if (message == kWork) {
        auto work = reinterpret_cast<void (*)(void*)>(w);
        work(reinterpret_cast<void*>(l));
        return 0;
    }
    return DefWindowProcW(window, message, w, l);
}

HINSTANCE ownModule()
{
    HMODULE module = nullptr;
    GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                       reinterpret_cast<LPCWSTR>(&postProc), &module);
    return module;
}

void openPostWindow()
{
    if (editorsOpen++ > 0) return;
    WNDCLASSEXW wc = {};
    wc.cbSize = sizeof wc;
    wc.lpfnWndProc = postProc;
    wc.hInstance = ownModule();
    wc.lpszClassName = kClass;
    RegisterClassExW(&wc);   // fails harmlessly when it is registered already
    HWND window = CreateWindowExW(0, kClass, L"", 0, 0, 0, 0, 0, HWND_MESSAGE, nullptr, wc.hInstance, nullptr);
    if (!window) sp_host_note("no main thread window: the editor will not hear its instance");
    std::lock_guard<std::mutex> lk(postLock);
    postWindow = window;
}

// The last editor closed: the window goes, and its class, so nothing of this module is left in the host's
// windowing once the host unloads it. Work still queued for it is dropped, which only an open editor would need.
void closePostWindow()
{
    if (editorsOpen == 0 || --editorsOpen > 0) return;
    HWND window;
    {
        std::lock_guard<std::mutex> lk(postLock);
        window = postWindow;
        postWindow = nullptr;
    }
    if (window) DestroyWindow(window);
    UnregisterClassW(kClass, ownModule());
}

std::string utf8(const std::wstring& wide)
{
    if (wide.empty()) return {};
    const int n = WideCharToMultiByte(CP_UTF8, 0, wide.data(), (int) wide.size(), nullptr, 0, nullptr, nullptr);
    std::string out(n, '\0');
    WideCharToMultiByte(CP_UTF8, 0, wide.data(), (int) wide.size(), out.data(), n, nullptr, nullptr);
    return out;
}

} // namespace

extern "C" int sp_parent_ready(void* parent)
{
    return IsWindow(static_cast<HWND>(parent)) ? 1 : 0;
}

extern "C" void* sp_monitor_install(void* hostView, void* editor)
{
    openPostWindow();
    auto* monitor = new Monitor();
    monitor->host = static_cast<HWND>(hostView);
    monitor->editor = editor;
    return monitor;
}

// The chords the host keeps (the page's list): the host has them whenever it has the focus, which on Windows is
// the only time it sees a key at all.
extern "C" void sp_monitor_host_keys(void*, const char*) {}

// A host that offers keys through IPlugView acts on any we decline: all of them, as the page has its own already.
extern "C" int sp_monitor_take_key(void*, int) { return 0; }
// Full screen is the Mac's so far (mac.mm): here the page stays where it is.
extern "C" int sp_monitor_full_screen(void*, int) { return 0; }
extern "C" int sp_monitor_is_full_screen(void*) { return 0; }

extern "C" void sp_monitor_remove(void* opaque)
{
    auto* monitor = static_cast<Monitor*>(opaque);
    if (!monitor) return;
    delete monitor;
    closePostWindow();
}

// `work(ctx)` on the main thread, soon: what the instance says reaches the page there. With no editor open there
// is no page to reach, and it runs where it is (lib.rs's deliver finds no editor, and gives back its count).
extern "C" void sp_main_async(void* ctx, void (*work)(void*))
{
    {
        std::lock_guard<std::mutex> lk(postLock);
        if (postWindow && PostMessageW(postWindow, kWork, reinterpret_cast<WPARAM>(work), reinterpret_cast<LPARAM>(ctx)))
            return;
    }
    work(ctx);
}

extern "C" const char* sp_own_path()
{
    static std::string path = [] {
        std::wstring buffer(MAX_PATH, L'\0');
        for (;;) {
            const DWORD n = GetModuleFileNameW(ownModule(), buffer.data(), (DWORD) buffer.size());
            if (n == 0) return std::string();
            if (n < buffer.size()) { buffer.resize(n); break; }
            buffer.resize(buffer.size() * 2);
        }
        return utf8(buffer);
    }();
    return path.c_str();
}
