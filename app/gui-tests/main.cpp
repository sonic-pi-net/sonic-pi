// Test entry point for the GUI unit tests. The widgets under test need a live
// QApplication, so we own main() (Catch2::Catch2, not ...WithMain) and run the
// Catch session after constructing the app. Forces the offscreen Qt platform so
// the suite runs headless in CI on every OS.

#include <QAccessible>
#include <QApplication>
#include <QFont>
#include <QStyle>
#include <QTemporaryDir>
#include <catch2/catch_session.hpp>
#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>

#include <cstdio>
#include <cstdlib>
#include <cstring>

#ifdef Q_OS_WIN
#define NOMINMAX
#include <crtdbg.h>
#include <windows.h>
#endif

// A Qt warning is a fault in the app — a layout asked of a widget that has none,
// a signal connected to nothing, a painter opened on a dead device — and every
// one would be lost in the output of a passing suite. Here it ends the run, as
// QT_FATAL_WARNINGS would, except for the notices the OFFSCREEN PLATFORM itself
// emits about what it cannot do: those are the test platform's, not the app's,
// and are listed below with the reason, so the list is a thing to review rather
// than a blanket.
namespace {
const char* const kPlatformNotices[] = {
    "This plugin does not support ",          // the offscreen platform's phrase for what it has no window manager for: raise(), propagateSizeHints(), ...
    "Populating font family aliases took",    // the font database's one-time note on a generic family name
};

void failOnQtWarning(QtMsgType type, const QMessageLogContext& ctx, const QString& msg)
{
    const QByteArray text = msg.toLocal8Bit();
    if (type == QtDebugMsg || type == QtInfoMsg)
    {
        std::fprintf(stderr, "%s\n", text.constData());
        return;
    }
    for (const char* notice : kPlatformNotices)
        if (std::strstr(text.constData(), notice) != nullptr)
        {
            std::fprintf(stderr, "[platform notice] %s\n", text.constData());
            return;
        }
    std::fprintf(stderr, "\nQt %s treated as a test failure: %s\n  at %s:%d (%s)\n",
                 type == QtWarningMsg ? "warning" : type == QtCriticalMsg ? "critical" : "fatal",
                 text.constData(), ctx.file ? ctx.file : "?", ctx.line, ctx.function ? ctx.function : "?");
    std::abort();
}

// What a test sets for the whole application is put back as the run began after it, so no test sees another's: the
// app's stylesheet (a styled box's border, padding and scroll bars), its font, and its style. Left on, a case passed
// alone and failed after one that had set the stylesheet, and the suite's verdict hung on its order.
class ApplicationState : public Catch::EventListenerBase
{
public:
    using Catch::EventListenerBase::EventListenerBase;

    void testRunStarting(const Catch::TestRunInfo&) override
    {
        m_font = QApplication::font();
        m_style = QApplication::style()->name();
        m_styleClass = QApplication::style()->metaObject()->className();
    }

    void testCaseEnded(const Catch::TestCaseStats&) override
    {
        if (!qApp->styleSheet().isEmpty())
            qApp->setStyleSheet(QString());
        if (QByteArray(QApplication::style()->metaObject()->className()) != m_styleClass)
            QApplication::setStyle(m_style);
        if (QApplication::font() != m_font)
            QApplication::setFont(m_font);
    }

private:
    QFont m_font;
    QString m_style;
    QByteArray m_styleClass;
};
CATCH_REGISTER_LISTENER(ApplicationState)

// SONIC_PI_TEST_NAMES: each case's name as it starts, on stderr. Catch2 says nothing of a case that passes, so a run
// that stops part-way (a hang, a crash with no message) otherwise leaves no word of where: CI sets it.
class CaseNames : public Catch::EventListenerBase
{
public:
    using Catch::EventListenerBase::EventListenerBase;

    void testCaseStarting(const Catch::TestCaseInfo& info) override
    {
        if (qEnvironmentVariableIsEmpty("SONIC_PI_TEST_NAMES"))
            return;
        std::fprintf(stderr, "case: %s\n", info.name.c_str());
        std::fflush(stderr);
    }
};
CATCH_REGISTER_LISTENER(CaseNames)
} // namespace

#ifdef Q_OS_MACOS
// completionpopup.cpp calls SonicPi::setPopupBelowSwitcher (defined in
// platform/macos.mm) when it shows on macOS. We don't link the .mm here — it
// drags in AppKit/AVFoundation/Syphon — so satisfy the symbol with a no-op.
namespace SonicPi { void setPopupBelowSwitcher(void*) {} void setWindowAccessibilityIgnored(void*) {} }
#endif

// SONIC_PI_TEST_NAMES also has the program say how far its own start has got, before any case.
static void started(const char* what)
{
    if (qEnvironmentVariableIsEmpty("SONIC_PI_TEST_NAMES"))
        return;
    std::fprintf(stderr, "gui-tests: %s\n", what);
    std::fflush(stderr);
}

int main(int argc, char** argv)
{
    started("main entered");
#ifdef Q_OS_WIN
    // On Windows a failure that ends the process (abort, a failed assertion in the C runtime, a crash) first asks
    // about it in a dialog, and a run with nobody to answer waits on it for ever: CI's did, for six hours a time.
    // The report goes to stderr and the process ends, as on every other platform.
    SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX);
    _set_abort_behavior(0, _WRITE_ABORT_MSG | _CALL_REPORTFAULT);
    for (int kind : { _CRT_WARN, _CRT_ERROR, _CRT_ASSERT })
    {
        _CrtSetReportMode(kind, _CRTDBG_MODE_FILE);
        _CrtSetReportFile(kind, _CRTDBG_FILE_STDERR);
    }
#endif
    if (qEnvironmentVariableIsEmpty("QT_QPA_PLATFORM"))
        qputenv("QT_QPA_PLATFORM", "offscreen");
    // Qt wants a private runtime directory and warns when it has to make one
    // up, as it does in a container running as root with no XDG_RUNTIME_DIR.
    // The run gets one of its own instead, for its lifetime.
    QTemporaryDir runtimeDir;
    if (qEnvironmentVariableIsEmpty("XDG_RUNTIME_DIR") && runtimeDir.isValid())
        qputenv("XDG_RUNTIME_DIR", runtimeDir.path().toLocal8Bit());
#ifdef Q_OS_WIN
    // The offscreen platform has no fonts of its own on Windows: it looks in Qt's lib/fonts, which no Qt install
    // has, and warns that there are none. It is given the system's, so text is measured in real fonts.
    if (qEnvironmentVariableIsEmpty("QT_QPA_FONTDIR"))
        qputenv("QT_QPA_FONTDIR", (qEnvironmentVariable("WINDIR", QStringLiteral("C:\\Windows")) + QStringLiteral("\\Fonts")).toLocal8Bit());
#endif
    qInstallMessageHandler(failOnQtWarning);
    QApplication app(argc, argv);
    started("application made");
    // SONIC_PI_TEST_SCREEN_READER: accessibility on for the whole run, as a running screen reader has it from before
    // any widget is made. A widget's interface is then made as the widget is, ahead of what is set on it afterwards,
    // which a run with accessibility off never sees (CMakeLists.txt runs the [a11y] cases a second time so). Qt 6.4's
    // setActive only tells its observers, and there the run is as the first; that Qt makes the interface early anyway.
    if (!qEnvironmentVariableIsEmpty("SONIC_PI_TEST_SCREEN_READER"))
        QAccessible::setActive(true);
    return Catch::Session().run(argc, argv);
}
