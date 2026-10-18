// Test entry point for the GUI unit tests. The widgets under test need a live
// QApplication, so we own main() (Catch2::Catch2, not ...WithMain) and run the
// Catch session after constructing the app. Forces the offscreen Qt platform so
// the suite runs headless in CI on every OS.

#include <QApplication>
#include <QFont>
#include <QStyle>
#include <QTemporaryDir>
#include <catch2/catch_session.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>

#include <cstdio>
#include <cstdlib>
#include <cstring>

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
} // namespace

#ifdef Q_OS_MACOS
// completionpopup.cpp calls SonicPi::setPopupBelowSwitcher (defined in
// platform/macos.mm) when it shows on macOS. We don't link the .mm here — it
// drags in AppKit/AVFoundation/Syphon — so satisfy the symbol with a no-op.
namespace SonicPi { void setPopupBelowSwitcher(void*) {} void setWindowAccessibilityIgnored(void*) {} }
#endif

int main(int argc, char** argv)
{
    if (qEnvironmentVariableIsEmpty("QT_QPA_PLATFORM"))
        qputenv("QT_QPA_PLATFORM", "offscreen");
    // Qt wants a private runtime directory and warns when it has to make one
    // up, as it does in a container running as root with no XDG_RUNTIME_DIR.
    // The run gets one of its own instead, for its lifetime.
    QTemporaryDir runtimeDir;
    if (qEnvironmentVariableIsEmpty("XDG_RUNTIME_DIR") && runtimeDir.isValid())
        qputenv("XDG_RUNTIME_DIR", runtimeDir.path().toLocal8Bit());
    qInstallMessageHandler(failOnQtWarning);
    QApplication app(argc, argv);
    return Catch::Session().run(argc, argv);
}
