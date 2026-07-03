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

// ---------------------------------------------------------------------------
// Accessibility self-test (--selftest-accessibility), Windows edition.
//
// Exercises the real UI Automation bridge in-process — the layer Narrator,
// NVDA and JAWS actually talk to, which headless unit tests can't reach.
// Mirrors the macOS NSAccessibility self-test in platform/macos.mm:
//
//   Move 1: the completion popup window must be pruned from the UIA tree
//           (offscreen, no descendants) so screen readers stay in the editor.
//   Move 2: a navigation announcement (QAccessibleAnnouncementEvent) must
//           arrive as a UIA notification event on the host window.
//
// UIA client calls marshal through the message queue of the thread that owns
// the provider windows, so calling them from the GUI thread would deadlock on
// our own windows. All client-side work therefore runs on an MTA worker
// thread while the GUI thread keeps pumping events.
// ---------------------------------------------------------------------------

#include "windows_a11y.h"

#include <QAccessible>
#include <QApplication>
#include <QList>
#include <QPoint>
#include <QWidget>

#include "widgets/completionpopup.h"
#include "utils/scintilla_api.h"

// WIN32_LEAN_AND_MEAN is defined project-wide, so windows.h skips the COM
// base headers uiautomation.h depends on (objbase.h's `interface` macro et
// al.) — include them explicitly.
#include <windows.h>
#include <objbase.h>
#include <oleauto.h>
#include <uiautomation.h>

#include <atomic>
#include <cstdio>
#include <iostream>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace
{

std::string narrow(const std::wstring& w)
{
    if (w.empty())
        return {};
    const int n = WideCharToMultiByte(CP_UTF8, 0, w.data(), (int)w.size(), nullptr, 0, nullptr, nullptr);
    std::string s(n, '\0');
    WideCharToMultiByte(CP_UTF8, 0, w.data(), (int)w.size(), s.data(), n, nullptr, nullptr);
    return s;
}

// Collects the display strings of UIA notification events (the transport Qt
// uses for QAccessibleAnnouncementEvent on Windows).
class AnnouncementSink : public IUIAutomationNotificationEventHandler
{
public:
    std::vector<std::wstring> take()
    {
        std::lock_guard<std::mutex> lock(m_mutex);
        return m_captured;
    }

    // IUnknown
    ULONG STDMETHODCALLTYPE AddRef() override { return ++m_ref; }
    ULONG STDMETHODCALLTYPE Release() override
    {
        const ULONG r = --m_ref;
        if (r == 0)
            delete this;
        return r;
    }
    HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid, void** ppv) override
    {
        if (!ppv)
            return E_POINTER;
        if (riid == __uuidof(IUnknown) || riid == __uuidof(IUIAutomationNotificationEventHandler))
        {
            *ppv = static_cast<IUIAutomationNotificationEventHandler*>(this);
            AddRef();
            return S_OK;
        }
        *ppv = nullptr;
        return E_NOINTERFACE;
    }

    // IUIAutomationNotificationEventHandler
    HRESULT STDMETHODCALLTYPE HandleNotificationEvent(IUIAutomationElement* /*sender*/,
                                                      NotificationKind /*kind*/,
                                                      NotificationProcessing /*processing*/,
                                                      BSTR displayString,
                                                      BSTR /*activityId*/) override
    {
        if (displayString)
        {
            std::lock_guard<std::mutex> lock(m_mutex);
            m_captured.emplace_back(displayString, SysStringLen(displayString));
        }
        return S_OK;
    }

private:
    std::atomic<ULONG> m_ref{ 1 };
    std::mutex m_mutex;
    std::vector<std::wstring> m_captured;
};

CompletionItem makeItem(const QString& text, const QString& kind)
{
    CompletionItem it;
    it.text = text;
    it.kind = kind;
    return it;
}

} // namespace

namespace SonicPi
{

int runAccessibilitySelfTest()
{
    using std::cout;
    using std::endl;

    // sonic-pi.exe is a WIN32-subsystem binary, so stdout is disconnected by
    // default; re-attach to the launching console so the verdict is visible.
    // Leave stdout alone when the caller already redirected it (pipe/file) —
    // pointing it at CONOUT$ then would silently discard the captured output.
    const HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    const bool stdoutConnected = out != nullptr && out != INVALID_HANDLE_VALUE;
    if (!stdoutConnected && AttachConsole(ATTACH_PARENT_PROCESS))
    {
        FILE* f = nullptr;
        freopen_s(&f, "CONOUT$", "w", stdout);
        freopen_s(&f, "CONOUT$", "w", stderr);
    }

    cout << "=== Sonic Pi accessibility self-test (UI Automation) ===" << endl;

    QAccessible::setActive(true);

    // Realise a host window + the completion popup — same fixture as macOS.
    QWidget host;
    host.setWindowTitle(QStringLiteral("SonicPi A11y Selftest"));
    host.resize(420, 240);
    host.show();

    CompletionPopup popup(&host);
    QList<CompletionItem> items;
    items << makeItem("pretty_bells", "synth")
          << makeItem("prophet", "synth")
          << makeItem("pulse", "synth");
    popup.showItems(items, QPoint(120, 120), 14);

    for (int i = 0; i < 25; ++i)
    {
        QApplication::processEvents();
        Sleep(20);
    }

    const HWND hostHwnd = reinterpret_cast<HWND>(host.winId());
    const HWND popupHwnd = reinterpret_cast<HWND>(popup.winId());

    std::atomic<int> failures{ 0 };
    std::atomic<bool> readyForMove2{ false };
    std::atomic<bool> workerDone{ false };

    std::thread worker([&]() {
        // Serialise the worker's own prints through cout; the GUI thread only
        // pumps, so interleaving isn't a concern.
        HRESULT hr = CoInitializeEx(nullptr, COINIT_MULTITHREADED);
        if (FAILED(hr))
        {
            cout << "  FAIL: CoInitializeEx hr=0x" << std::hex << hr << std::dec << endl;
            ++failures;
            workerDone = true;
            return;
        }

        IUIAutomation* uia = nullptr;
        hr = CoCreateInstance(__uuidof(CUIAutomation8), nullptr, CLSCTX_INPROC_SERVER,
                              __uuidof(IUIAutomation), reinterpret_cast<void**>(&uia));
        if (FAILED(hr))
            hr = CoCreateInstance(__uuidof(CUIAutomation), nullptr, CLSCTX_INPROC_SERVER,
                                  __uuidof(IUIAutomation), reinterpret_cast<void**>(&uia));
        if (FAILED(hr) || !uia)
        {
            cout << "  FAIL: could not create CUIAutomation (hr=0x" << std::hex << hr
                 << std::dec << ")" << endl;
            ++failures;
            CoUninitialize();
            workerDone = true;
            return;
        }

        // ---- Move 1: popup pruned from the UIA tree ------------------------
        cout << "\n--- Move 1: popup hidden from the UIA tree ---" << endl;
        IUIAutomationElement* popupEl = nullptr;
        hr = uia->ElementFromHandle(popupHwnd, &popupEl);
        if (SUCCEEDED(hr) && popupEl)
        {
            BOOL offscreen = FALSE;
            popupEl->get_CurrentIsOffscreen(&offscreen);
            CONTROLTYPEID ct = 0;
            popupEl->get_CurrentControlType(&ct);
            cout << "  popup element IsOffscreen = " << (offscreen ? "TRUE" : "FALSE")
                 << ", ControlType = " << ct << endl;

            int descendants = -1;
            IUIAutomationCondition* trueCond = nullptr;
            if (SUCCEEDED(uia->CreateTrueCondition(&trueCond)) && trueCond)
            {
                IUIAutomationElementArray* found = nullptr;
                if (SUCCEEDED(popupEl->FindAll(TreeScope_Descendants, trueCond, &found)) && found)
                {
                    found->get_Length(&descendants);
                    found->Release();
                }
                trueCond->Release();
            }
            cout << "  popup element descendants = " << descendants << endl;

            if (offscreen && descendants == 0)
            {
                cout << "  PASS: popup is ignored by the UIA bridge (offscreen, childless)" << endl;
            }
            else
            {
                cout << "  FAIL: popup leaks into the UIA tree" << endl;
                ++failures;
            }
            popupEl->Release();
        }
        else
        {
            cout << "  FAIL: ElementFromHandle(popup) hr=0x" << std::hex << hr << std::dec << endl;
            ++failures;
        }

        // ---- Move 2: navigation announcement delivered via the bridge ------
        cout << "\n--- Move 2: navigation announcements ---" << endl;
#if QT_VERSION >= QT_VERSION_CHECK(6, 8, 0)
        IUIAutomation5* uia5 = nullptr;
        hr = uia->QueryInterface(__uuidof(IUIAutomation5), reinterpret_cast<void**>(&uia5));
        if (SUCCEEDED(hr) && uia5)
        {
            IUIAutomationElement* hostEl = nullptr;
            hr = uia5->ElementFromHandle(hostHwnd, &hostEl);
            if (SUCCEEDED(hr) && hostEl)
            {
                AnnouncementSink* sink = new AnnouncementSink();
                hr = uia5->AddNotificationEventHandler(hostEl, TreeScope_Subtree, nullptr, sink);
                if (SUCCEEDED(hr))
                {
                    // Hand the GUI thread its cue to move the popup selection,
                    // then wait for the notification to round-trip.
                    readyForMove2 = true;

                    const std::wstring expected = L"prophet, synth, 2 of 3";
                    bool gotExpected = false;
                    std::vector<std::wstring> seen;
                    for (int i = 0; i < 150 && !gotExpected; ++i) // up to ~3s
                    {
                        Sleep(20);
                        seen = sink->take();
                        for (const std::wstring& s : seen)
                            if (s == expected)
                                gotExpected = true;
                    }

                    cout << "  captured " << seen.size() << " notification(s):" << endl;
                    for (const std::wstring& s : seen)
                        cout << "    \"" << narrow(s) << "\"" << endl;

                    if (gotExpected)
                    {
                        cout << "  PASS: navigation announcement delivered through UIA" << endl;
                    }
                    else
                    {
                        cout << "  FAIL: expected notification \"prophet, synth, 2 of 3\" not observed" << endl;
                        ++failures;
                    }

                    uia5->RemoveNotificationEventHandler(hostEl, sink);
                }
                else
                {
                    cout << "  FAIL: AddNotificationEventHandler hr=0x" << std::hex << hr
                         << std::dec << endl;
                    ++failures;
                }
                sink->Release();
                hostEl->Release();
            }
            else
            {
                cout << "  FAIL: ElementFromHandle(host) hr=0x" << std::hex << hr << std::dec << endl;
                ++failures;
            }
            uia5->Release();
        }
        else
        {
            cout << "  FAIL: IUIAutomation5 unavailable (needs Windows 10 1709+), hr=0x"
                 << std::hex << hr << std::dec << endl;
            ++failures;
        }
#else
        cout << "  SKIP: built against Qt < 6.8 — announcements are compiled out" << endl;
#endif

        uia->Release();
        CoUninitialize();
        workerDone = true;
    });

#if QT_VERSION >= QT_VERSION_CHECK(6, 8, 0)
    // Mimic MainWindow: relay the popup's announceRequested to the bridge.
    QObject::connect(&popup, &CompletionPopup::announceRequested, &popup,
                     [&host](const QString& s) {
                         QAccessibleAnnouncementEvent ev(&host, s);
                         QAccessible::updateAccessibility(&ev);
                     });
#endif

    // Pump the GUI so the UIA provider side can answer the worker, and fire
    // the selection move once the worker's event handler is in place. The
    // worker's own waits are bounded, but its synchronous UIA client calls
    // are not — a wedged accessibility stack must fail the test, not hang CI.
    const ULONGLONG deadline = GetTickCount64() + 30000;
    bool moved = false;
    while (!workerDone.load())
    {
        QApplication::processEvents(QEventLoop::AllEvents, 20);
        if (readyForMove2.load() && !moved)
        {
            moved = true;
            popup.moveSelection(+1); // expect "prophet, synth, 2 of 3"
        }
        if (GetTickCount64() > deadline)
        {
            cout << "\n=== FAIL (timeout: UIA worker made no progress in 30s) ===" << endl;
            cout.flush();
            // Can't join a thread stuck inside a UIA call; end the process.
            ExitProcess(2);
        }
        Sleep(5);
    }
    worker.join();

    const int f = failures.load();
    cout << "\n=== " << (f == 0 ? "PASS" : "FAIL") << " (" << f << " failure(s)) ===" << endl;
    return f == 0 ? 0 : 1;
}

} // namespace SonicPi
