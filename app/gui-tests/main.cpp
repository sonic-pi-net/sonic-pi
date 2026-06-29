// Test entry point for the GUI unit tests. The widgets under test need a live
// QApplication, so we own main() (Catch2::Catch2, not ...WithMain) and run the
// Catch session after constructing the app. Forces the offscreen Qt platform so
// the suite runs headless in CI on every OS.

#include <QApplication>
#include <catch2/catch_session.hpp>

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
    QApplication app(argc, argv);
    return Catch::Session().run(argc, argv);
}
