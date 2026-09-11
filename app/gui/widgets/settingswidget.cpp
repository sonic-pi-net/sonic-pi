#include "settingswidget.h"
#include "devicelistwidget.h"
#include "mainwindow.h"
#include "utils/reducedmotion.h"
#include "utils/sonicpi_i18n.h"
#include "dpi.h"
#include "utils/fontroles.h"
#include <api/audio/audio_driver_select.hpp>
#include <QTreeWidget>
#include <QHeaderView>
#include <QRadioButton>
#include <QFileDialog>
#include <QColorDialog>
#include <QListWidget>
#include <QDial>
#include <QScrollArea>
#include "arcdial.h"
#include "theme_card.h"
#include <QDialog>
#include <QStyledItemDelegate>
#include <QLineEdit>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QFocusEvent>
#include <QStyle>
#include <QStyleOption>
#include <QVariantAnimation>
#include <QScopedValueRollback>
#include <memory>
#include "utils/audiodevicepolicy.h"
#if defined(Q_OS_DARWIN)
#include "platform/macos.h"
#endif
#ifdef Q_OS_WIN
#define WIN32_LEAN_AND_MEAN
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#endif

#include <QSettings>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QGroupBox>
#include <QButtonGroup>
#include <QToolButton>
#include <QNetworkInterface>
#include <QDesktopServices>
#include <QCheckBox>
#include <QComboBox>
#include <QRadioButton>
#include <QDial>
#include <QTimer>
#include <QPainter>
#include <QPixmap>
#include <QIcon>
#include <QPen>
#include <QFontMetrics>
#include <QUrl>
#include <iostream>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QSignalMapper>
#include <QVBoxLayout>
#include <QMessageBox>
#include <QProcess>
#include <QFileInfo>
#include <QCoreApplication>
#include <QSize>
#include <QSvgRenderer>
#include <QApplication>

#include "arcdial.h"

namespace {

// The Level box. Volume keeps the dial and is the control you reach for; Drive
// and the limiter meter are horizontal strips beside it, narrower than the box
// so they read as subordinate rather than as the dominant controls.
constexpr int kVolDialPx = 140;
constexpr int kStripWidthPx = 130;
// Enough for the Levels strip inside the Level box.
constexpr int kLevelScopeHeightPx = 56;

// Checkbox glyphs (Tabler icons, MIT), one per preference so a setting can be
// found by its shape before its label is read. %1 = the render colour.
// Wrapped in the same header the recording glyphs use so makeSvgPixmap can
// tint them; the paths are the icon's own, unaltered.
#define SP_TABLER_SVG(paths)                                                   \
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' " \
    "stroke='%1' stroke-width='2' stroke-linecap='round' "                     \
    "stroke-linejoin='round'>" paths "</svg>"

// switch-horizontal: the two channels crossing over.
const char* kInvertStereoSvg = SP_TABLER_SVG(
    "<path d='M16 3l4 4l-4 4'/><path d='M10 7l10 0'/>"
    "<path d='M8 13l-4 4l4 4'/><path d='M4 17l9 0'/>");

// circle-dot: two channels collapsed to one point.
const char* kForceMonoSvg = SP_TABLER_SVG(
    "<path d='M11 12a1 1 0 1 0 2 0a1 1 0 1 0 -2 0'/>"
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0'/>");

// shield: arguments checked before they reach a synth.
const char* kSafeModeSvg = SP_TABLER_SVG(
    "<path d='M12 3a12 12 0 0 0 8.5 3a12 12 0 0 1 -8.5 15a12 12 0 0 1 -8.5 "
    "-15a12 12 0 0 0 8.5 -3' />");

// clock-shield: protecting the clock from late triggers.
const char* kTimingGuaranteesSvg = SP_TABLER_SVG(
    "<path d='M21 12a9 9 0 1 0 -8.98 9' />"
    "<path d='M12 7v5l1 1' />"
    "<path d='M22 16c0 4 -2.5 6 -3.5 6s-3.5 -2 -3.5 -6c1 0 2.5 -.5 3.5 "
    "-1.5c1 1 2.5 1.5 3.5 1.5' />");

// microphone: sound coming in
const char* kAudioInputsSvg = SP_TABLER_SVG(
    "<path d='M9 5a3 3 0 0 1 3 -3a3 3 0 0 1 3 3v5a3 3 0 0 1 -3 3a3 3 0 0 1 -3 "
    "-3l0 -5'/><path d='M5 10a7 7 0 0 0 14 0'/><path d='M8 21l8 0'/>"
    "<path d='M12 17l0 4'/>");

// package-import: synths and FX brought in from outside
const char* kExternalSynthsSvg = SP_TABLER_SVG(
    "<path d='M12 21l-8 -4.5v-9l8 -4.5l8 4.5v4.5' />"
    "<path d='M12 12l8 -4.5' />"
    "<path d='M12 12v9' />"
    "<path d='M12 12l-8 -4.5' />"
    "<path d='M22 18h-7' />"
    "<path d='M18 15l-3 3l3 3' />"
);

// antenna: listening for OSC
const char* kOscServerSvg = SP_TABLER_SVG(
    "<path d='M20 4v8' />"
    "<path d='M16 4.5v7' />"
    "<path d='M12 5v16' />"
    "<path d='M8 5.5v5' />"
    "<path d='M4 6v4' />"
    "<path d='M20 8h-16' />"
);

// world: OSC from beyond this machine
const char* kOscPublicSvg = SP_TABLER_SVG(
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0' />"
    "<path d='M3.6 9h16.8' />"
    "<path d='M3.6 15h16.8' />"
    "<path d='M11.5 3a17 17 0 0 0 0 18' />"
    "<path d='M12.5 3a17 17 0 0 1 0 18' />");

// piano: notes arriving from a MIDI instrument
const char* kMidiCuesSvg = SP_TABLER_SVG(
    "<path d='M3 7a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v10a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2v-10' />"
    "<path d='M9 19v-6' />"
    "<path d='M8 5v8h2v-8' />"
    "<path d='M15 19v-6' />"
    "<path d='M14 5v8h2v-8' />"
);

// device-gamepad-2: gamepad input as cues
const char* kGamepadCuesSvg = SP_TABLER_SVG(
    "<path d='M12 5h3.5a5 5 0 0 1 0 10h-5.5l-4.015 4.227a2.3 2.3 0 0 1 -3.923 -2.035l1.634 -8.173a5 5 0 0 1 4.904 -4.019h3.4' />"
    "<path d='M14 15l4.07 4.284a2.3 2.3 0 0 0 3.925 -2.023l-1.6 -8.232' />"
    "<path d='M8 9v2' />"
    "<path d='M7 10h2' />"
    "<path d='M14 10h2' />"
);

// align-left: code aligned on run
const char* kAutoAlignSvg = SP_TABLER_SVG(
    "<path d='M4 6l16 0' />"
    "<path d='M4 12l10 0' />"
    "<path d='M4 18l14 0' />"
);

// list-numbers: numbered lines
const char* kLineNumbersSvg = SP_TABLER_SVG(
    "<path d='M11 6h9' />"
    "<path d='M11 12h9' />"
    "<path d='M12 18h8' />"
    "<path d='M4 16a2 2 0 1 1 4 0c0 .591 -.5 1 -1 1.5l-3 2.5h4' />"
    "<path d='M6 10v-6l-2 2' />"
);

// wand: completions offered as you type
const char* kAutocompletionSvg = SP_TABLER_SVG(
    "<path d='M6 21l15 -15l-3 -3l-15 15l3 3' />"
    "<path d='M15 6l3 3' />"
    "<path d='M9 3a2 2 0 0 0 2 2a2 2 0 0 0 -2 2a2 2 0 0 0 -2 -2a2 2 0 0 0 2 -2' />"
    "<path d='M19 13a2 2 0 0 0 2 2a2 2 0 0 0 -2 2a2 2 0 0 0 -2 -2a2 2 0 0 0 2 -2' />"
);

// help-circle: help alongside a completion
const char* kCompletionHelpSvg = SP_TABLER_SVG(
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0' />"
    "<path d='M12 16v.01' />"
    "<path d='M12 13a2 2 0 0 0 .914 -3.782a1.98 1.98 0 0 0 -2.414 .483' />"
);

// info-circle: context for the code under the caret
const char* kCodeContextSvg = SP_TABLER_SVG(
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 0 0 -18 0' />"
    "<path d='M12 9h.01' />"
    "<path d='M11 12h1v4h1' />"
);

// volume: run and stop spoken aloud
const char* kSpeakTransportSvg = SP_TABLER_SVG(
    "<path d='M15 8a5 5 0 0 1 0 8' />"
    "<path d='M17.7 5a9 9 0 0 1 0 14' />"
    "<path d='M6 15h-2a1 1 0 0 1 -1 -1v-4a1 1 0 0 1 1 -1h2l3.5 -4.5a.8 .8 0 0 1 1.5 .5v14a.8 .8 0 0 1 -1.5 .5l-3.5 -4.5' />"
);

// player-pause: animation held back
const char* kReduceMotionSvg = SP_TABLER_SVG(
    "<path d='M6 6a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1l0 -12' />"
    "<path d='M14 6a1 1 0 0 1 1 -1h2a1 1 0 0 1 1 1v12a1 1 0 0 1 -1 1h-2a1 1 0 0 1 -1 -1l0 -12' />"
);

// terminal-2: the run log
const char* kShowLogSvg = SP_TABLER_SVG(
    "<path d='M8 9l3 3l-3 3' />"
    "<path d='M13 15l3 0' />"
    "<path d='M3 6a2 2 0 0 1 2 -2h14a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-14a2 2 0 0 1 -2 -2l0 -12' />"
);

// speakerphone: the cue log pane, matching the Log cues switch
const char* kShowCuesSvg = SP_TABLER_SVG(
    "<path d='M18 8a3 3 0 0 1 0 6' />"
    "<path d='M10 8v11a1 1 0 0 1 -1 1h-1a1 1 0 0 1 -1 -1v-5' />"
    "<path d='M12 8l4.524 -3.77a.9 .9 0 0 1 1.476 .692v12.156a.9 .9 0 0 1 "
    "-1.476 .692l-4.524 -3.77h-8a1 1 0 0 1 -1 -1v-4a1 1 0 0 1 1 -1h8' />"
);

// metronome: Link metronome controls
const char* kShowMetroSvg = SP_TABLER_SVG(
    "<path d='M14.153 8.188l-.72 -3.236a2.493 2.493 0 0 0 -4.867 0l-3.025 13.614a2 2 0 0 0 1.952 2.434h7.014a2 2 0 0 0 1.952 -2.434l-.524 -2.357m-4.935 1.791l9 -13' />"
    "<path d='M19 5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
);

// square-rounded: the toolbar buttons
const char* kShowButtonsSvg = SP_TABLER_SVG(
    "<path d='M12 3c7.2 0 9 1.8 9 9c0 7.2 -1.8 9 -9 9c-7.2 0 -9 -1.8 -9 -9c0 -7.2 1.8 -9 9 -9' />"
);

// layout-navbar: the editor toolbar strip
const char* kEditorToolbarSvg = SP_TABLER_SVG(
    "<path d='M4 6a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2l0 -12' />"
    "<path d='M4 9l16 0' />"
);

// browser: buffers as tabs across the top
const char* kShowTabsSvg = SP_TABLER_SVG(
    "<path d='M4 8h16' />"
    "<path d='M4 6a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2l0 -12' />"
    "<path d='M8 4v4' />"
);

// maximize: filling the screen
const char* kFullScreenSvg = SP_TABLER_SVG(
    "<path d='M4 8v-2a2 2 0 0 1 2 -2h2' />"
    "<path d='M4 16v2a2 2 0 0 0 2 2h2' />"
    "<path d='M16 4h2a2 2 0 0 1 2 2v2' />"
    "<path d='M16 20h2a2 2 0 0 0 2 -2v-2' />"
);

// text-caption: a caption naming each pane
const char* kShowTitlesSvg = SP_TABLER_SVG(
    "<path d='M4 15h16' />"
    "<path d='M4 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v4a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -4' />"
    "<path d='M4 20h12' />"
);

// layout-navbar-collapse: menu bar folded away in full screen
const char* kHideMenuBarSvg = SP_TABLER_SVG(
    "<path d='M4 18v-12a2 2 0 0 1 2 -2h12a2 2 0 0 1 2 2v12a2 2 0 0 1 -2 2h-12a2 2 0 0 1 -2 -2' />"
    "<path d='M4 9h16' />"
    "<path d='M10 16l2 -2l2 2' />"
);

// wave-sine: synth triggers in the log
const char* kLogSynthsSvg = SP_TABLER_SVG(
    "<path d='M21 12h-2c-.894 0 -1.662 -.857 -1.761 -2c-.296 -3.45 -.749 -6 -2.749 -6s-2.5 3.582 -2.5 8s-.5 8 -2.5 8s-2.452 -2.547 -2.749 -6c-.1 -1.147 -.867 -2 -1.763 -2h-2' />"
);

// eraser: log wiped at the start of a run
const char* kClearLogSvg = SP_TABLER_SVG(
    "<path d='M19 20h-10.5l-4.21 -4.3a1 1 0 0 1 0 -1.41l10 -10a1 1 0 0 1 1.41 0l5 5a1 1 0 0 1 0 1.41l-9.2 9.3' />"
    "<path d='M18 13.3l-6.3 -6.3' />"
);

// speakerphone: cues announcing themselves in the log
const char* kLogCuesSvg = SP_TABLER_SVG(
    "<path d='M18 8a3 3 0 0 1 0 6' />"
    "<path d='M10 8v11a1 1 0 0 1 -1 1h-1a1 1 0 0 1 -1 -1v-5' />"
    "<path d='M12 8l4.524 -3.77a.9 .9 0 0 1 1.476 .692v12.156a.9 .9 0 0 1 "
    "-1.476 .692l-4.524 -3.77h-8a1 1 0 0 1 -1 -1v-4a1 1 0 0 1 1 -1h8' />"
);

// arrow-autofit-down: log following the newest line
const char* kAutoScrollSvg = SP_TABLER_SVG(
    "<path d='M12 20h-6a2 2 0 0 1 -2 -2v-12a2 2 0 0 1 2 -2h8' />"
    "<path d='M18 4v17' />"
    "<path d='M15 18l3 3l3 -3' />"
);

// icons: the alternative icon set
const char* kProIconsSvg = SP_TABLER_SVG(
    "<path d='M3 6.5a3.5 3.5 0 1 0 7 0a3.5 3.5 0 1 0 -7 0' />"
    "<path d='M2.5 21h8l-4 -7l-4 7' />"
    "<path d='M14 3l7 7' />"
    "<path d='M14 10l7 -7' />"
    "<path d='M14 14h7v7h-7l0 -7' />"
);

// contrast: colour drained from the theme
const char* kMonochromeSvg = SP_TABLER_SVG(
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0' />"
    "<path d='M12 17a5 5 0 0 0 0 -10v10' />"
);

// circle-half-2: half light, half dark
const char* kInvertColoursSvg = SP_TABLER_SVG(
    "<path d='M3 12a9 9 0 1 0 18 0a9 9 0 1 0 -18 0' />"
    "<path d='M12 3v18' />"
    "<path d='M12 14l7 -7' />"
    "<path d='M12 19l8.5 -8.5' />"
    "<path d='M12 9l4.5 -4.5' />"
);

// wave-square: the audio oscilloscopes
const char* kShowScopesSvg = SP_TABLER_SVG(
    "<path d='M3 12h5v8h4v-16h4v8h5' />"
);

// tag: names on each scope
const char* kScopeLabelsSvg = SP_TABLER_SVG(
    "<path d='M6.5 7.5a1 1 0 1 0 2 0a1 1 0 1 0 -2 0' />"
    "<path d='M3 6v5.172a2 2 0 0 0 .586 1.414l7.71 7.71a2.41 2.41 0 0 0 3.408 0l5.592 -5.592a2.41 2.41 0 0 0 0 -3.408l-7.71 -7.71a2 2 0 0 0 -1.414 -.586h-5.172a3 3 0 0 0 -3 3' />"
);

// highlight: code lit up as it triggers
const char* kFlashCodeSvg = SP_TABLER_SVG(
    "<path d='M3 19h4l10.5 -10.5a2.828 2.828 0 1 0 -4 -4l-10.5 10.5v4' />"
    "<path d='M12.5 5.5l4 4' />"
    "<path d='M4.5 13.5l4 4' />"
    "<path d='M21 15v4h-8l4 -4l4 0' />"
);

// border-left: the gutter strip lit instead
const char* kFlashGutterSvg = SP_TABLER_SVG(
    "<path d='M4 20l0 -16' />"
    "<path d='M8 4l0 .01' />"
    "<path d='M12 4l0 .01' />"
    "<path d='M16 4l0 .01' />"
    "<path d='M20 4l0 .01' />"
    "<path d='M12 8l0 .01' />"
    "<path d='M20 8l0 .01' />"
    "<path d='M8 12l0 .01' />"
    "<path d='M12 12l0 .01' />"
    "<path d='M16 12l0 .01' />"
    "<path d='M20 12l0 .01' />"
    "<path d='M12 16l0 .01' />"
    "<path d='M20 16l0 .01' />"
    "<path d='M8 20l0 .01' />"
    "<path d='M12 20l0 .01' />"
    "<path d='M16 20l0 .01' />"
    "<path d='M20 20l0 .01' />"
);

// activity-heartbeat: a trace per live loop
const char* kLoopScopesSvg = SP_TABLER_SVG(
    "<path d='M3 12h4.5l1.5 -6l4 12l2 -9l1.5 3h4.5' />"
);

// chevrons-right: those traces travelling
const char* kLoopScrollSvg = SP_TABLER_SVG(
    "<path d='M7 7l5 5l-5 5' />"
    "<path d='M13 7l5 5l-5 5' />"
);

// refresh: looking for a newer version
const char* kCheckUpdatesSvg = SP_TABLER_SVG(
    "<path d='M20 11a8.1 8.1 0 0 0 -15.5 -2m-.5 -4v4h4' />"
    "<path d='M4 13a8.1 8.1 0 0 0 15.5 2m.5 4v-4h-4' />"
);

// zodiac-aquarius: two waves, one per channel
const char* kScopeStereoSvg = SP_TABLER_SVG(
    "<path d='M3 10l3 -3l3 3l3 -3l3 3l3 -3l3 3' />"
    "<path d='M3 17l3 -3l3 3l3 -3l3 3l3 -3l3 3' />"
);

// whirl: the figure the two channels trace together
const char* kScopeLissajousSvg = SP_TABLER_SVG(
    "<path d='M14 12a2 2 0 1 0 -4 0a2 2 0 0 0 4 0' />"
    "<path d='M12 21c-3.314 0 -6 -2.462 -6 -5.5s2.686 -5.5 6 -5.5' />"
    "<path d='M21 12c0 3.314 -2.462 6 -5.5 6s-5.5 -2.686 -5.5 -6' />"
    "<path d='M12 14c3.314 0 6 -2.462 6 -5.5s-2.686 -5.5 -6 -5.5' />"
    "<path d='M14 12c0 -3.314 -2.462 -6 -5.5 -6s-5.5 2.686 -5.5 6' />"
);

// chart-column: frequency bands as columns
const char* kScopeSpectrumSvg = SP_TABLER_SVG(
    "<path d='M4 20h3' />"
    "<path d='M17 20h3' />"
    "<path d='M10.5 20h3' />"
    "<path d='M4 16h3' />"
    "<path d='M17 16h3' />"
    "<path d='M10.5 16h3' />"
    "<path d='M4 12h3' />"
    "<path d='M17 12h3' />"
    "<path d='M10.5 12h3' />"
    "<path d='M4 8h3' />"
    "<path d='M17 8h3' />"
    "<path d='M4 4h3' />"
);

// chart-bar: level as a horizontal meter
const char* kScopeLevelsSvg = SP_TABLER_SVG(
    "<path d='M3 13a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v6a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -6' />"
    "<path d='M15 9a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v10a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -10' />"
    "<path d='M9 5a1 1 0 0 1 1 -1h4a1 1 0 0 1 1 1v14a1 1 0 0 1 -1 1h-4a1 1 0 0 1 -1 -1l0 -14' />"
    "<path d='M4 20h14' />"
);

// flip-vertical: left mirrored above right
const char* kScopeMirrorSvg = SP_TABLER_SVG(
    "<path d='M3 12l18 0' />"
    "<path d='M7 16l10 0l-10 5l0 -5' />"
    "<path d='M7 8l10 0l-10 -5l0 5' />"
);

// zodiac-aquarius with one wave instead of two, centred: the same mark as
// Stereo above with a channel taken away, which is what Mono is.
const char* kScopeMonoSvg = SP_TABLER_SVG(
    "<path d='M3 13.5l3 -3l3 3l3 -3l3 3l3 -3l3 3' />"
);

// Recording-selector glyphs (Tabler icons, MIT). %1 = the render colour.
// Audio Only = a waveform; Audio + Video = a camcorder.
const char* kWaveformSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M3 9v6'/>"
    "<path d='M7 5v14'/>"
    "<path d='M11 3v18'/>"
    "<path d='M15 6v12'/>"
    "<path d='M19 9v6'/>"
    "</svg>";

const char* kVideoSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M15 10l4.553 -2.276a1 1 0 0 1 1.447 .894v6.764a1 1 0 0 1 -1.447 .894l-4.553 -2.276v-4z'/>"
    "<path d='M3 8a2 2 0 0 1 2 -2h8a2 2 0 0 1 2 2v8a2 2 0 0 1 -2 2h-8a2 2 0 0 1 -2 -2z'/>"
    "</svg>";

// Window-publishing glyphs (Tabler icons, MIT). Send = cast (screen with
// broadcast waves — the universal screen-casting mark); Off = the same
// glyph struck through.
const char* kCastOffSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M3 19l.01 0'/>"
    "<path d='M7 19a4 4 0 0 0 -4 -4'/>"
    "<path d='M11 19a8 8 0 0 0 -8 -8'/>"
    "<path d='M15 19h3a3 3 0 0 0 3 -3v-8a3 3 0 0 0 -3 -3h-12a3 3 0 0 0 -2.8 2'/>"
    "<path d='M3 3l18 18'/>"
    "</svg>";

const char* kCastSvg =
    "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' "
    "stroke='%1' stroke-width='2.75' stroke-linecap='round' stroke-linejoin='round'>"
    "<path d='M3 19l.01 0'/>"
    "<path d='M7 19a4 4 0 0 0 -4 -4'/>"
    "<path d='M11 19a8 8 0 0 0 -8 -8'/>"
    "<path d='M15 19h3a3 3 0 0 0 3 -3v-8a3 3 0 0 0 -3 -3h-12a3 3 0 0 0 -2.8 2'/>"
    "</svg>";

// Render a tinted SVG glyph to a crisp (2x) pixmap of the given logical size.
QPixmap makeSvgPixmap(const char* svg, const QColor& color, int px)
{
    QPixmap pm(QSize(px, px) * 2);
    pm.setDevicePixelRatio(2);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing);
    const QByteArray bytes =
        QString::fromLatin1(svg).arg(color.name(QColor::HexRgb)).toUtf8();
    QSvgRenderer(bytes).render(&p, QRectF(0, 0, px, px));
    return pm;
}

// Two-state icon: `off` tint at rest, `on` tint while checked — Qt picks
// the QIcon::On pixmap automatically for checked buttons, so the glyph
// reads on both the grey segment and the highlight-filled one.
QIcon makeSvgToggleIcon(const char* svg, const QColor& off, const QColor& on, int px)
{
    QIcon icon;
    icon.addPixmap(makeSvgPixmap(svg, off, px), QIcon::Normal, QIcon::Off);
    icon.addPixmap(makeSvgPixmap(svg, on, px), QIcon::Normal, QIcon::On);
    icon.addPixmap(makeSvgPixmap(svg, off, px), QIcon::Active, QIcon::Off);
    icon.addPixmap(makeSvgPixmap(svg, on, px), QIcon::Active, QIcon::On);
    return icon;
}

// Size a checkbox's glyph from its text, so the two scale together with the
// type scale rather than the icon drifting as fonts change. A little over the
// cap height: at exactly the type size the glyph reads smaller than the label
// beside it, because a Tabler icon carries padding inside its 24px box.
int checkIconPx()
{
    return FontRolePx(FontRole::Base) * 5 / 4;
}

} // namespace

// Give a preference checkbox its glyph. QCheckBox inherits QAbstractButton, so
// the icon is drawn between the tick box and the label with no extra widget or
// layout of its own.
void SettingsWidget::setCheckIcon(QCheckBox* box, const char* svg)
{
    if (!box) return;
    m_checkIcons.append({ box, svg });
    applyCheckIcon(box, svg);
    // A two-state QIcon is no use here: QCommonStyle draws CE_CheckBoxLabel
    // with a mode but no state, so a checkbox always takes the QIcon::Off
    // pixmap and never the On one. (QToolButton, which the recording controls
    // are, does pass the state, which is why the same trick works there.) So
    // the glyph is re-rendered on each toggle instead.
    connect(box, &QCheckBox::toggled, this,
            [this, box, svg]() { applyCheckIcon(box, svg); });
}

// Render one glyph in the colour its current state calls for.
void SettingsWidget::applyCheckIcon(QCheckBox* box, const char* svg)
{
    if (!box) return;
    const int px = checkIconPx();
    const QColor colour = box->isChecked() ? checkIconOnColour() : checkIconColour();
    box->setIconSize(QSize(px, px));
    box->setIcon(QIcon(makeSvgPixmap(svg, colour, px)));
}

// Unchecked: muted rather than full-strength, so a column of glyphs does not
// compete with the ticks beside them for the eye.
QColor SettingsWidget::checkIconColour() const
{
    QColor c = palette().color(QPalette::WindowText);
    c.setAlpha(150);
    return c;
}

// Checked: the accent, matching the tick. The glyph then carries the state as
// well as the identity, so a lit row reads as on from its shape and its colour
// at once rather than from the tick alone.
QColor SettingsWidget::checkIconOnColour() const
{
    return palette().color(QPalette::Highlight);
}

// Re-render every glyph after a theme change; the pixmaps are baked at one
// colour, so they do not follow the palette on their own.
void SettingsWidget::retintCheckIcons()
{
    for (const auto& entry : m_checkIcons)
        applyCheckIcon(entry.box, entry.svg);
}

// Pulses the Audio Device group box border while a device/driver change is
// in flight — cold swaps take a few seconds and instant-looking controls
// would otherwise read as hung. A transparent, top-most child of the box
// that retraces its frame in the theme accent, breathing via a looped
// animation (held steady when reduced motion is preferred).
class DevicePulseOverlay : public QWidget
{
public:
    explicit DevicePulseOverlay(QGroupBox* box) : QWidget(box), m_box(box)
    {
        setAttribute(Qt::WA_TransparentForMouseEvents);
        hide();
        m_anim = new QVariantAnimation(this);
        m_anim->setStartValue(0.25);
        m_anim->setKeyValueAt(0.5, 1.0);
        m_anim->setEndValue(0.25);
        m_anim->setDuration(1600);
        m_anim->setEasingCurve(QEasingCurve::InOutSine);
        m_anim->setLoopCount(-1);
        connect(m_anim, &QVariantAnimation::valueChanged, this,
                [this](const QVariant&) { update(); });
        box->installEventFilter(this);
    }
    void start()
    {
        setGeometry(m_box->rect());
        raise();
        show();
        if (!SonicPi::prefersReducedMotion()
            && m_anim->state() != QAbstractAnimation::Running)
            m_anim->start();
    }
    void stop()
    {
        m_anim->stop();
        hide();
    }

protected:
    bool eventFilter(QObject* obj, QEvent* e) override
    {
        if (obj == m_box && e->type() == QEvent::Resize)
            setGeometry(m_box->rect());
        return QWidget::eventFilter(obj, e);
    }
    void paintEvent(QPaintEvent*) override
    {
        // The box's own frame/label geometry (QSS margins included), so the
        // pulse hugs the drawn border and skips the title like the border
        // itself does.
        QStyleOptionGroupBox opt;
        opt.initFrom(m_box);
        opt.text = m_box->title();
        opt.subControls = QStyle::SC_GroupBoxFrame | QStyle::SC_GroupBoxLabel;
        const QRect frame = m_box->style()->subControlRect(
            QStyle::CC_GroupBox, &opt, QStyle::SC_GroupBoxFrame, m_box);
        const QRect label = m_box->style()->subControlRect(
            QStyle::CC_GroupBox, &opt, QStyle::SC_GroupBoxLabel, m_box);
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        p.setClipRegion(QRegion(rect()).subtracted(QRegion(label)));
        QColor c = palette().color(QPalette::Highlight);
        c.setAlphaF(m_anim->state() == QAbstractAnimation::Running
                        ? m_anim->currentValue().toReal()
                        : 0.9);
        const qreal w = qMax(2, ScaleHeightForDPI(2));
        p.setPen(QPen(c, w));
        p.drawRect(QRectF(frame).adjusted(w / 2, w / 2, -w / 2, -w / 2));
    }

private:
    QGroupBox* m_box;
    QVariantAnimation* m_anim;
};

/**
 * Default Constructor
 */
namespace {
// Scroll container for one prefs tab. Its natural size is the page's own
// hint, so the pane still opens content-sized — but unlike a bare page it
// can shrink far below that, growing scrollbars instead of letting the
// form rows overlap (short 768px laptop screens).
class PrefsTabScroller : public QScrollArea
{
public:
    explicit PrefsTabScroller(QWidget* page)
    {
        setWidget(page);
        setWidgetResizable(true);
        setFrameShape(QFrame::NoFrame);
        viewport()->setAutoFillBackground(false);
        page->setAutoFillBackground(false);
    }
    QSize sizeHint() const override
    {
        return widget() ? widget()->sizeHint() : QScrollArea::sizeHint();
    }
    QSize minimumSizeHint() const override
    {
        return QSize(ScaleWidthForDPI(220), ScaleHeightForDPI(160));
    }
};

QWidget* wrapTabInScroller(QWidget* page)
{
    return new PrefsTabScroller(page);
}
} // namespace

SettingsWidget::SettingsWidget(int tau_osc_cues_port, bool i18n, SonicPiSettings *piSettings, SonicPii18n *sonicPii18n, const QString& shortcutConfigPath, QWidget *parent) {
    this->piSettings = piSettings;
    this->i18n = i18n;
    this->sonicPii18n = sonicPii18n;
    this->shortcutConfigPath = shortcutConfigPath;
    this->available_languages = sonicPii18n->getAvailableLanguages();
    this->tau_osc_cues_port = tau_osc_cues_port;

    // Safety timeout: if device switch takes longer than 15 seconds,
    // re-enable controls so the user isn't stuck forever.
    m_switchTimeoutTimer = new QTimer(this);
    m_switchTimeoutTimer->setSingleShot(true);
    connect(m_switchTimeoutTimer, &QTimer::timeout, this, [this]() {
        supersonic_version_label->setText(tr("Device switch timed out"));
        if (m_devicePulse) m_devicePulse->stop();
        m_reopenPending = false;
        if (audio_status_label) setAudioStatus(tr("Device switch timed out"));
        audio_output_combo->setEnabled(true);
        audio_input_combo->setEnabled(true);
        audio_sample_rate_combo->setEnabled(true);
        audio_buffer_size_combo->setEnabled(true);
        audio_driver_combo->setEnabled(true);
        if (reset_device_button) reset_device_button->setEnabled(true);
    });
    QSizePolicy prefsSizePolicy(QSizePolicy::Preferred, QSizePolicy::MinimumExpanding);

    setSizePolicy(prefsSizePolicy) ;
    prefTabs = new QTabWidget();

    QGridLayout *grid = new QGridLayout;
    grid->addWidget(prefTabs, 0, 0);

    QGroupBox *audio_prefs_box = createAudioPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(audio_prefs_box), tr("Audio")),
                            tr("Volume, audio inputs and outputs, safety checks and recording."));

    QGroupBox *ioTab = createIoPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(ioTab), tr("IO")),
                            tr("OSC networking, MIDI devices and game controllers."));

    QGroupBox *editorTab = createEditorPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(editorTab), tr("Editor")),
                            tr("Editor display, code completion, accessibility and pane visibility."));

    QGroupBox *visualizationTab = createVisualizationPrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(visualizationTab), tr("Visuals")),
                            tr("Themes, transparency, audio oscilloscopes and options useful when performing."));

    QGroupBox *shortcuts_prefs_box = createKeyboardShortcutsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(shortcuts_prefs_box), tr("Shortcuts")),
                            tr("View and customise the keyboard shortcuts."));

    QGroupBox *language_prefs_box = createLanguagePrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(language_prefs_box), tr("Language")),
                            tr("Change the language of the interface and tutorial."));

    // Updates last — it's the least-visited tab.
    QGroupBox *update_prefs_box = createUpdatePrefsTab();
    prefTabs->setTabToolTip(prefTabs->addTab(wrapTabInScroller(update_prefs_box), tr("Updates")),
                            tr("Version information and update checking."));


    settingsChanged();
    connectAll();
    setLayout(grid);
}

/**
 * Destructor
 */
void SettingsWidget::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);
    updateSupersonicBannerForScreen();
}

// On monitors too short for the full pane, trade the SuperSonic ascii art
// for a plain two-line credit ("Powered by SuperSonic" + the info line) so
// the audio device controls keep the height. Decided per show against the
// monitor the window is on; m_bannerCompact keeps the needed-height maths
// stable once the art is hidden (the hint no longer includes it).
void SettingsWidget::updateSupersonicBannerForScreen()
{
    QScreen* screen = window() ? window()->screen() : nullptr;
    if (!screen || !supersonic_ascii_label)
        return;
    const int artH = supersonic_ascii_label->sizeHint().height() + ScaleHeightForDPI(8);
    int need = sizeHint().height() + ScaleHeightForDPI(140);
    if (m_bannerCompact)
        need += artH;
    const bool compact = screen->availableGeometry().height() < need;
    if (compact == m_bannerCompact)
        return;
    m_bannerCompact = compact;
    supersonic_ascii_label->setVisible(!compact);
    powered_by_label->setText(compact ? tr("Powered by SuperSonic") : tr("Powered by"));
}

SettingsWidget::~SettingsWidget() {
}

// True when the GUI is running inside a remote desktop session (RDP).
// Local audio hardware is typically unavailable there — WASAPI endpoints
// are redirected to "Remote Audio", while installed ASIO drivers still
// enumerate from the registry regardless of session and then fail to
// start. Checked per call rather than cached: RDP attach/detach changes
// the session state mid-run, and each attach also changes the audio
// device list, which triggers the device report that re-reads this.
static bool isRemoteDesktopSession()
{
#ifdef Q_OS_WIN
    return GetSystemMetrics(SM_REMOTESESSION) != 0;
#else
    return false;
#endif
}

/**
 * Create Audio Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createAudioPrefsTab() {

    // --- Level (Volume + Drive) + Audio settings ---
    //
    // Two dials, deliberately paired, because they sit on opposite sides of
    // the main limiter and that is the whole distinction between them:
    // Volume cannot change how anything sounds, only how loud it is. Drive
    // can only change how it sounds, by pushing the limiter harder.
    QGroupBox *volBox = new QGroupBox(tr("Output"));
    volBox->setToolTip(tr("Configure output volume and how hard the mix drives the main limiter."));

    system_vol_slider = new ArcDial(this);
    system_vol_slider->setWrapping(false);
    system_vol_slider->setValueFontRole(FontRole::XLarge);
    system_vol_slider->setFixedSize(kVolDialPx, kVolDialPx);
    system_vol_slider->setRange(0, 100);
    system_vol_slider->setValueSuffix(tr("%"));
    system_vol_slider->setAccessibleName(tr("Volume"));
    system_vol_slider->setProperty("tipTitle", tr("Volume"));
    system_vol_slider->setToolTip(tr("Drag or scroll to change Sonic Pi's output volume. This is applied after the main limiter, so it changes how loud the output is without altering the mix."));

    // Percent, like every other dial here; 100 leaves the mix untouched.
    // Below 100 makes headroom, which is what layering a lot of sounds needs:
    // the sum runs well over full scale.
    // A horizontal slider rather than a second dial: Volume is the control you
    // reach for and keeps the dial, while Drive reads as the subordinate one.
    // Shares the global time-warp slider's styling (see app.qss) so it looks
    // like the rest of the app's horizontal controls.
    system_drive_slider = new QSlider(Qt::Horizontal, this);
    system_drive_slider->setObjectName("driveSlider");
    system_drive_slider->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    // Floor above zero: Drive is a pre-limiter gain, so 0 would mute Sonic Pi
    // outright, which reads as the app being broken rather than as a control
    // being turned down. Volume is the control that silences.
    system_drive_slider->setRange(25, 400);
    system_drive_slider->setAccessibleName(tr("Drive"));
    system_drive_slider->setProperty("tipTitle", tr("Drive"));
    system_drive_slider->setToolTip(tr("Slide to change how hard the mix is driven into the main limiter. At 100% the mix is left untouched.\n\nValues above 100% make things louder and denser, at the cost of more limiting. Values below 100% create headroom for layering many sounds together."));

    enable_scsynth_inputs = new QCheckBox(tr("Enable Audio Inputs"));
    enable_scsynth_inputs->setToolTip(tr("Toggle to enable or disable audio inputs."));
    setCheckIcon(enable_scsynth_inputs, kAudioInputsSvg);
    // Carries the "pick a device" prompt too: ASIO is the only driver that
    // records a pending pick without opening anything (it has no default
    // device), so this note is on screen exactly when that prompt applies —
    // no need for a second, competing line in the status row.
    asio_input_note = new QLabel(
        tr("ASIO uses one device for both input and output. Select device."));
    asio_input_note->setWordWrap(true);
    asio_input_note->setObjectName("asioInputNote");   // styled by app.qss (muted note)
    ApplyFontRole(asio_input_note, FontRole::Small);
    // Ignored horizontally, like the status line below: a wrapped label's
    // width hint is its whole text on one line, so a note that appears only
    // on ASIO would widen the grid's columns as it comes and goes — the
    // selectors would slide sideways on every driver change. Ignored means
    // it wraps into whatever width the selectors settle on instead of
    // driving that width.
    asio_input_note->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Fixed);
    asio_input_note->setVisible(false);
    mixer_invert_stereo = new QCheckBox(tr("Invert stereo"));
    mixer_invert_stereo->setToolTip(tr("If enabled, audio sent to the left speaker will be routed to the right speaker and vice versa."));
    setCheckIcon(mixer_invert_stereo, kInvertStereoSvg);
    mixer_force_mono = new QCheckBox(tr("Force mono"));
    mixer_force_mono->setToolTip(tr("If enabled, right and left audio is mixed and the same signal is sent to both speakers. Useful when working with external systems that can only handle mono."));
    setCheckIcon(mixer_force_mono, kForceMonoSvg);

    check_args = new QCheckBox(tr("Safe mode"));
    check_args->setToolTip(tr("Checks synth arguments before triggering. If disabled, certain synth opt values may create unexpectedly loud or uncomfortable sounds."));
    setCheckIcon(check_args, kSafeModeSvg);

    synth_trigger_timing_guarantees_cb = new QCheckBox(tr("Enforce timing guarantees"));
    synth_trigger_timing_guarantees_cb->setToolTip(tr("When enabled, Sonic Pi will refuse to trigger synths and FX if it is too late to do so.\n\nWhen disabled, Sonic Pi will always attempt to trigger synths and FX even when a little late."));
    setCheckIcon(synth_trigger_timing_guarantees_cb, kTimingGuaranteesSvg);

    enable_external_synths_cb = new QCheckBox(tr("Enable external synths/FX"));
    enable_external_synths_cb->setToolTip(tr("When enabled, Sonic Pi will allow synths and FX loaded via load_synthdefs to be triggered.\n\nWhen disabled, Sonic Pi will complain when you attempt to use a synth or FX which isn't recognised."));
    setCheckIcon(enable_external_synths_cb, kExternalSynthsSvg);

    QGroupBox *synthsGroup = new QGroupBox(tr("Synths and FX"));
    QVBoxLayout *synthsGroupLayout = new QVBoxLayout;
    synthsGroupLayout->setSpacing(ScaleHeightForDPI(2));
    synthsGroupLayout->addWidget(check_args);
    synthsGroupLayout->addWidget(synth_trigger_timing_guarantees_cb);
    synthsGroupLayout->addWidget(enable_external_synths_cb);
    synthsGroup->setLayout(synthsGroupLayout);

    // The two dials side by side, each under its own label so which is which
    // is unambiguous at a glance. The Audio and Synths groups are siblings in
    // the tab layout below rather than nested in here.
    // Captions follow the other dial columns in here (see the hue dial's
    // dialColumn and Flash Brightness): default font, centred by the layout
    // rather than by the label's own alignment.
    QLabel *vol_label = new QLabel(tr("Volume"));
    vol_label->setAlignment(Qt::AlignHCenter);
    QLabel *drive_label = new QLabel(tr("Drive"));
    drive_label->setAlignment(Qt::AlignHCenter);

    // The Levels meter itself goes here: an actual ScopeWindow handed over by
    // MainWindow, not a lookalike. Inserted by setLevelScope.
    level_scope_caption = new QLabel(tr("Level"));
    level_scope_caption->setAlignment(Qt::AlignHCenter);
    m_levelScopeSlot = new QVBoxLayout;

    // Raw pixels, as the volume dial has always been sized: ScaleWidthForDPI
    // is a DPI correction, not an enlargement, so routing these through it
    // can make the controls smaller than intended.
    system_drive_slider->setFixedWidth(kStripWidthPx);

    // Nested boxes rather than one grid: a dial spanning the strip rows makes
    // the grid share its height out among them, which squashes the strips to
    // whatever is left. Each side owning its own column keeps the strips at
    // their natural height and lets the gap between them be set explicitly.
    QVBoxLayout *volume_col = new QVBoxLayout;
    volume_col->addWidget(system_vol_slider, 0, Qt::AlignHCenter);
    volume_col->addWidget(vol_label, 0, Qt::AlignHCenter);

    QVBoxLayout *strips_col = new QVBoxLayout;
    strips_col->addStretch(1);
    strips_col->addWidget(level_scope_caption, 0, Qt::AlignHCenter);
    strips_col->addLayout(m_levelScopeSlot);
    strips_col->addSpacing(4);
    strips_col->addWidget(drive_label, 0, Qt::AlignHCenter);
    strips_col->addWidget(system_drive_slider, 0, Qt::AlignHCenter);
    strips_col->addStretch(1);

    // Three equal gaps rather than one in the middle, so the dial and the
    // strips sit spread across the box instead of meeting at its centre.
    QHBoxLayout *vol_box = new QHBoxLayout;
    vol_box->addStretch(1);
    vol_box->addLayout(volume_col);
    vol_box->addStretch(1);
    vol_box->addLayout(strips_col);
    vol_box->addStretch(1);

    volBox->setLayout(vol_box);

    // --- Audio Device (driver, device, sample rate, buffer size) ---
    QGroupBox *audioDeviceBox = new QGroupBox(tr("Audio Device"));
    audioDeviceBox->setToolTip(tr("Configure audio driver, device, sample rate and buffer size."));
    QGridLayout *audio_device_layout = new QGridLayout;
    // Gap between rows so the combos read as separate fields, not one block.
    audio_device_layout->setVerticalSpacing(ScaleHeightForDPI(8));

    // Fixed, uniform height so each combo's grey fill exactly matches its
    // focus/hover highlight (otherwise the widget floats taller than the
    // painted background) and every row is the same height (even spacing).
    // The ASIO note shares this height too — see the Input row below.
    const int comboHeight = ScaleHeightForDPI(28);

    QLabel *driverLabel = new QLabel(tr("Driver"));
    audio_driver_combo = new QComboBox();
    audio_driver_combo->setMinimumContentsLength(12);
    audio_driver_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    driverLabel->setBuddy(audio_driver_combo);
    audio_device_layout->addWidget(driverLabel, 0, 0);
    audio_device_layout->addWidget(audio_driver_combo, 0, 1);

    audio_output_label = new QLabel(tr("Output"));
    audio_output_combo = new QComboBox();
    audio_output_combo->setMinimumContentsLength(20);
    audio_output_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    audio_output_label->setBuddy(audio_output_combo);
    audio_device_layout->addWidget(audio_output_label, 1, 0);
    audio_device_layout->addWidget(audio_output_combo, 1, 1);

    audio_input_label = new QLabel(tr("Input"));
    audio_input_combo = new QComboBox();
    audio_input_combo->setMinimumContentsLength(20);
    audio_input_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    audio_input_label->setBuddy(audio_input_combo);
    // The input toggle sits below the selectors and level with Reset, so the
    // selectors stay an unbroken list. It lives in here because it genuinely
    // reconfigures the device (opens input streams); the stereo-image
    // toggles are mixer-stage and live in their own group below.
    audio_device_layout->addWidget(enable_scsynth_inputs, 8, 0, Qt::AlignVCenter);
    // The note keeps its own row, directly under Input. Sharing Input's row
    // looks tidier on paper but puts a column-spanning item in a cell that
    // already holds two single-cell ones, and the grid then drops that row's
    // spacing — Input sits flush under Output, and the gap reappears when the
    // note takes over, moving everything below by exactly one spacing.
    // Pinning the note to the combo height is what actually matters: it is
    // what makes the collapsed-Input row and the shown-note row measure the
    // same, so the swap moves nothing.
    asio_input_note->setFixedHeight(comboHeight);
    audio_device_layout->addWidget(audio_input_label, 2, 0);
    audio_device_layout->addWidget(audio_input_combo, 2, 1);
    audio_device_layout->addWidget(asio_input_note, 3, 0, 1, 2);

    QLabel *srLabel = new QLabel(tr("Sample Rate"));
    audio_sample_rate_combo = new QComboBox();
    audio_sample_rate_combo->setMinimumContentsLength(8);
    audio_sample_rate_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    srLabel->setBuddy(audio_sample_rate_combo);
    audio_device_layout->addWidget(srLabel, 4, 0);
    audio_device_layout->addWidget(audio_sample_rate_combo, 4, 1);

    QLabel *bsLabel = new QLabel(tr("Buffer Size"));
    audio_buffer_size_combo = new QComboBox();
    audio_buffer_size_combo->setMinimumContentsLength(8);
    audio_buffer_size_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);
    bsLabel->setBuddy(audio_buffer_size_combo);
    audio_device_layout->addWidget(bsLabel, 5, 0);
    audio_device_layout->addWidget(audio_buffer_size_combo, 5, 1);

    remote_session_note = new QLabel(
        tr("Remote desktop session: local audio hardware is usually "
           "unavailable, and ASIO devices may fail to start."));
    remote_session_note->setWordWrap(true);
    remote_session_note->setObjectName("remoteSessionNote");   // styled by app.qss (muted note)
    ApplyFontRole(remote_session_note, FontRole::Small);
    // Pinned to the two lines it actually wraps to, like the status line
    // below. A wrapped label's sizeHint is measured as though it had a long
    // single line, so an unpinned one under-reports its height — the box
    // then asks the column for less than it needs, the column obliges and
    // hands the slack to the stretches, and every row inside is squeezed
    // while blank space sits below the box.
    remote_session_note->setFixedHeight(
        2 * FontRolePx(FontRole::Small) + ScaleHeightForDPI(8));
    remote_session_note->setVisible(isRemoteDesktopSession());
    audio_device_layout->addWidget(remote_session_note, 6, 0, 1, 2);

    // Status line for in-flight or pending device changes, right-aligned
    // between the selectors and the input/routing toggles. Always present
    // at a fixed height so the message appearing/clearing never shifts the
    // layout.
    audio_status_label = new QLabel();
    audio_status_label->setObjectName("audioStatusNote");   // muted note (app.qss)
    ApplyFontRole(audio_status_label, FontRole::Small);
    audio_status_label->setFixedHeight(FontRolePx(FontRole::Small) + ScaleHeightForDPI(6));
    audio_status_label->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    // Ignored horizontally so the message never contributes to the box's
    // width; setAudioStatus elides to whatever width the combos settle on,
    // and the label re-elides when that width changes.
    audio_status_label->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Fixed);
    audio_status_label->installEventFilter(this);
    audio_device_layout->addWidget(audio_status_label, 7, 0, 1, 2);

    // Escape hatch: cold-swap the current device with its current settings —
    // for when the audio path wedges (post-sleep, hardware churn) and nothing
    // about the selection itself needs to change.
    reset_device_button = new QPushButton(tr("Reset"));
    reset_device_button->setFlat(true);
    reset_device_button->setToolTip(tr("Close and re-open the current audio device with the same settings (a full cold swap). Useful if audio has stopped behaving after sleep or hardware changes."));
    connect(reset_device_button, &QPushButton::clicked, this, [this] {
        // Disable up front like the combos: the engine refuses a second
        // reopen while one is running or inside its post-reopen cooldown.
        reset_device_button->setEnabled(false);
        m_reopenPending = true;
        beginDeviceSwitchFeedback();
        emit audioDeviceResetRequested();
    });
    audio_device_layout->addWidget(reset_device_button, 8, 1, Qt::AlignRight | Qt::AlignTop);

    for (QComboBox* c : { audio_driver_combo, audio_output_combo, audio_input_combo,
                          audio_sample_rate_combo, audio_buffer_size_combo }) {
        c->setFixedHeight(comboHeight);
    }

    // The box's frame absorbs the column's slack (see the tab layout), so
    // pin the rows to the top rather than letting them drift apart.
    audio_device_layout->setRowStretch(9, 1);
    audioDeviceBox->setLayout(audio_device_layout);
    m_devicePulse = new DevicePulseOverlay(audioDeviceBox);

    // --- Mix (invert stereo, force mono) ---
    // Mixer-stage transforms, not device settings: they shape what the mixer
    // emits, survive any device switch, and behave identically on every
    // driver — so they get their own group rather than riding in Audio
    // Device, where they'd read as properties of the selected hardware.
    QGroupBox *mixBox = new QGroupBox(tr("Mix"));
    mixBox->setToolTip(tr("Shape Sonic Pi's mix. Applied in the mixer, independently of the selected audio device."));
    QVBoxLayout *mix_layout = new QVBoxLayout;
    // Same tight spacing as the toggle columns elsewhere on this tab.
    mix_layout->setSpacing(ScaleHeightForDPI(2));
    mix_layout->addWidget(mixer_invert_stereo);
    mix_layout->addWidget(mixer_force_mono);
    mixBox->setLayout(mix_layout);

    // activated(int) — user-interaction only. currentIndexChanged fires
    // on programmatic setCurrentIndex() too, which would emit spurious
    // switches during updateAudioDevices() populate
    connect(audio_driver_combo, SIGNAL(activated(int)), this, SLOT(audioDriverChanged(int)));
    connect(audio_output_combo, SIGNAL(activated(int)), this, SLOT(audioDeviceChanged(int)));
    connect(audio_input_combo, SIGNAL(activated(int)), this, SLOT(audioInputDeviceChanged(int)));
    connect(audio_sample_rate_combo, SIGNAL(activated(int)), this, SLOT(audioSampleRateChanged(int)));
    connect(audio_buffer_size_combo, SIGNAL(activated(int)), this, SLOT(audioBufferSizeChanged(int)));

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // --- Recording mode — same setting is reachable from the IO menubar
    // submenu and the rec-button right-click menu. Segmented pill toggle
    // in the same style as the Shortcuts tab's mode control. ---
    QGroupBox *recordingGroup = new QGroupBox(tr("Recording"));
    recordingGroup->setToolTip(tr("Choose what the rec button captures."));

    // Icons tinted with the themed foreground so they read on both the resting
    // grey segment and the highlighted (checked) segment. QToolButton with
    // TextUnderIcon stacks a large glyph above the label — a QPushButton can
    // only put a small icon beside the text.
    const QColor segIconColor = QApplication::palette().color(QPalette::WindowText);
    const QColor segIconOnColor = QApplication::palette().color(QPalette::HighlightedText);
    const int segIconPx = ScaleHeightForDPI(32);

    recording_type_audio_radio = new QToolButton();
    recording_type_audio_radio->setText(tr("Record Audio Only"));
    recording_type_audio_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    recording_type_audio_radio->setIcon(makeSvgToggleIcon(kWaveformSvg, segIconColor, segIconOnColor, segIconPx));
    recording_type_audio_radio->setIconSize(QSize(segIconPx, segIconPx));
    recording_type_audio_radio->setToolTip(tr(
        "SuperSonic writes a .wav of the master mix"));

    recording_type_av_radio = new QToolButton();
    recording_type_av_radio->setText(tr("Record Audio + Video"));
    recording_type_av_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    recording_type_av_radio->setIcon(makeSvgToggleIcon(kVideoSvg, segIconColor, segIconOnColor, segIconPx));
    recording_type_av_radio->setIconSize(QSize(segIconPx, segIconPx));
#if defined(Q_OS_MAC)
    recording_type_av_radio->setToolTip(tr(
        "Captures the Sonic Pi window plus master mix into a .mov\n"
        "using GPU-accelerated screen capture"));
#else
    recording_type_av_radio->setToolTip(tr(
        "Captures the Sonic Pi window plus master mix into an .mp4\n"
        "using GPU-accelerated screen capture"));
#endif

    QWidget* recSegControl = new QWidget();
    recSegControl->setObjectName("recSegControl");
    // Styled by the shared "segmented control" rule in app.qss (house metrics).
    recSegControl->setProperty("segmented", true);
    QHBoxLayout* recSegLayout = new QHBoxLayout(recSegControl);
    recSegLayout->setContentsMargins(3, 3, 3, 3);
    recSegLayout->setSpacing(3);

    // Button IDs are the enum values so the idClicked(int) signal
    // delivers the chosen mode directly. idClicked only fires on user
    // clicks, so programmatic setChecked from settingsChanged() doesn't
    // echo back.
    recording_type_group = new QButtonGroup(this);
    recording_type_group->setExclusive(true);
    for (QToolButton* b : { recording_type_audio_radio, recording_type_av_radio }) {
        b->setCheckable(true);
        b->setCursor(Qt::PointingHandCursor);
        recSegLayout->addWidget(b);
    }
    recording_type_group->addButton(recording_type_audio_radio,
        static_cast<int>(SonicPiSettings::Audio));
    recording_type_group->addButton(recording_type_av_radio,
        static_cast<int>(SonicPiSettings::AudioAndVideo));

    QHBoxLayout *recordingGroupLayout = new QHBoxLayout;
    recordingGroupLayout->addStretch(1);
    recordingGroupLayout->addWidget(recSegControl);
    recordingGroupLayout->addStretch(1);
    recordingGroup->setLayout(recordingGroupLayout);

    connect(recording_type_group, SIGNAL(idClicked(int)),
            this, SLOT(recordingTypeChanged(int)));
#endif

    // --- SuperSonic info panel (ASCII art + version, tooltip = detailed info) ---
    supersonicBox = new QGroupBox();
    powered_by_label = new QLabel(tr("Powered by"));
    powered_by_label->setAlignment(Qt::AlignCenter);
    powered_by_label->setObjectName("poweredByLabel");   // styled by app.qss (muted note)
    ApplyFontRole(powered_by_label, FontRole::Small);
    supersonic_ascii_label = new QLabel(
        QString::fromUtf8(
            "\u2591\u2588\u2580\u2580\u2591\u2588\u2591\u2588\u2591\u2588\u2580\u2588\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2584\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2588\u2591\u2588\u2580\u2588\u2591\u2580\u2588\u2580\u2591\u2588\u2580\u2580\n"
            "\u2591\u2580\u2580\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2580\u2591\u2588\u2580\u2584\u2591\u2580\u2580\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2591\u2588\u2591\u2591\u2588\u2591\u2591\u2588\u2591\u2591\n"
            "\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2591\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2591\u2580\u2591\u2580\u2580\u2580\u2591\u2580\u2580\u2580"
        )
    );
    supersonic_ascii_label->setFont(QFont("Hack", 9));
    supersonic_ascii_label->setAlignment(Qt::AlignCenter);

    supersonic_version_label = new QLabel(tr("Waiting for SuperSonic..."));
    supersonic_version_label->setAlignment(Qt::AlignCenter);

    // Mic permission status line (macOS only — hidden on other platforms).
    // Polled from a QTimer so the user sees it flip to authorized as soon
    // as they grant access in System Settings, without needing to restart.
    mic_permission_label = new QLabel(tr(""));
    mic_permission_label->setAlignment(Qt::AlignCenter);
    mic_permission_label->setWordWrap(true);
    mic_permission_label->setVisible(false);
    mic_permission_settings_button = new QPushButton(tr("Open System Settings"));
    mic_permission_settings_button->setVisible(false);
    connect(mic_permission_settings_button, &QPushButton::clicked, this, []() {
#if defined(Q_OS_DARWIN)
        SonicPi::openSystemMicrophonePane();
#endif
    });

    QVBoxLayout *supersonic_layout = new QVBoxLayout;
    // No stretches around the content: this box is fixed-height and centred
    // under both columns, so anything that pads it just makes the tab taller.
    // Tight by design — this is a credit sharing a column with the audio
    // device selectors, and every pixel it takes comes off them.
    supersonic_layout->setContentsMargins(ScaleWidthForDPI(8), ScaleHeightForDPI(4),
                                          ScaleWidthForDPI(8), ScaleHeightForDPI(4));
    supersonic_layout->addWidget(powered_by_label);
    supersonic_layout->addSpacing(ScaleHeightForDPI(8));
    supersonic_layout->addWidget(supersonic_ascii_label);
    supersonic_layout->addWidget(supersonic_version_label);
    supersonic_layout->addWidget(mic_permission_label);
    supersonic_layout->addWidget(mic_permission_settings_button,
                                 0, Qt::AlignCenter);
    supersonicBox->setLayout(supersonic_layout);

#if defined(Q_OS_DARWIN)
    // Poll mic permission every 2 s. Cheap — a single AVFoundation call.
    m_micPermissionTimer = new QTimer(this);
    m_micPermissionTimer->setInterval(2000);
    connect(m_micPermissionTimer, &QTimer::timeout, this,
            &SettingsWidget::updateMicPermissionStatus);
    m_micPermissionTimer->start();
    // Also poll immediately so the initial state is correct.
    QTimer::singleShot(100, this, &SettingsWidget::updateMicPermissionStatus);
#endif

    // --- Assemble grid layout ---
    // Col 0: Output, Synths and FX, Recording (mac/win).
    // Col 1: Audio Device, SuperSonic panel.
    QGroupBox *audio_prefs_box = new QGroupBox();
    QGridLayout *audio_prefs_box_layout = new QGridLayout;

    // Every box keeps its natural height; each column's slack goes between
    // and after the boxes, never inside a frame.
    audioDeviceBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    mixBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    volBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    recordingGroup->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
#endif
    supersonicBox->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

    // Two independent columns, not a grid: nothing on the left lines up with
    // anything on the right, and a grid row would stretch its shorter box to
    // the taller one's height. Each column packs its own content, and the
    // tab is as tall as the taller of the two. Slack goes between the boxes,
    // as on the Editor and Visuals tabs, so the shorter column ends level
    // with the taller one while every box keeps its natural height.
    // Boxes pack from the top with a steady rhythm — fixed gaps, one
    // trailing stretch — rather than stretches between every box, which
    // spread them out unevenly as the tab grows.
    // Spacing 0 on both columns: the gap between boxes is the explicit
    // addSpacing below, and the layout's own default spacing was silently
    // adding itself on top of it — every gap was the intended 14dx plus a
    // style-provided one, which is height the tab cannot spare.
    QVBoxLayout *left_col = new QVBoxLayout;
    left_col->setSpacing(0);
    left_col->addWidget(volBox);
    left_col->addSpacing(ScaleHeightForDPI(14));
    left_col->addWidget(synthsGroup);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    left_col->addSpacing(ScaleHeightForDPI(14));
    left_col->addWidget(recordingGroup);
#endif
    left_col->addStretch(1);

    QVBoxLayout *right_col = new QVBoxLayout;
    right_col->setSpacing(0);
    right_col->addWidget(audioDeviceBox);
    right_col->addSpacing(ScaleHeightForDPI(14));
    right_col->addWidget(mixBox);
    // Stretches, not a stretch factor on the boxes: a stretch-grown box
    // shows the column's slack as framed blank space under its last row.
    right_col->addStretch(1);
    // A credit rather than a control, so it sits last, floating in its
    // share of the column rather than glued to the tab's bottom edge.
    right_col->addWidget(supersonicBox, 0, Qt::AlignHCenter);
    right_col->addStretch(1);

    audio_prefs_box_layout->addLayout(left_col, 0, 0);
    audio_prefs_box_layout->addLayout(right_col, 0, 1);
    audio_prefs_box->setLayout(audio_prefs_box_layout);

    return audio_prefs_box;
}

/**
 * create io tab of settings widget
 */
QGroupBox* SettingsWidget::createIoPrefsTab() {
    QGroupBox *ioTab = new QGroupBox();

    QGroupBox *network_box = new QGroupBox(tr("Networked OSC"));
    network_box->setToolTip(tr("Sonic Pi can send and receive Open Sound Control messages to and from other programs or computers via the currently connected network."));

    QLabel *network_ip_label = new QLabel();
    QString ip_address_trans = tr("Local IP address");
    QString port_num_trans = tr("Incoming OSC port");
    QString ip_address = "";
    QString all_ip_addresses  = "";

    QList<QHostAddress> list = QNetworkInterface::allAddresses();

    for(int nIter=0; nIter<list.count(); nIter++)
    {
        if(!list[nIter].isLoopback()) {
            if (list[nIter].protocol() == QAbstractSocket::IPv4Protocol ) {
                if (ip_address.isEmpty()) {
                    ip_address = list[nIter].toString();
                }
                all_ip_addresses = all_ip_addresses + list[nIter].toString() + "\n";
            }
        }
    }

    if (ip_address.isEmpty()) {
        ip_address = tr("Unavailable");
    }

    network_ip_label->setText(ip_address_trans + ": " + ip_address + "\n" + port_num_trans + + ": " + QString::number(tau_osc_cues_port));
    network_ip_label->setToolTip(all_ip_addresses);

    osc_public_check = new QCheckBox(tr("Allow OSC from other computers"));
    setCheckIcon(osc_public_check, kOscPublicSvg);
    osc_public_check->setToolTip(tr("When checked, Sonic Pi will let you send and receive OSC messages to and from remote machines. When unchecked, only sending and receiving from the local machine will be enabled."));

    osc_server_enabled_check = new QCheckBox(tr("Allow incoming OSC"));
    setCheckIcon(osc_server_enabled_check, kOscServerSvg);
    osc_server_enabled_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages. When unchecked, no OSC messages will be received."));

    QVBoxLayout *network_box_layout = new QVBoxLayout;
    network_box_layout->addWidget(osc_server_enabled_check);
    network_box_layout->addWidget(osc_public_check);
    network_box_layout->addWidget(network_ip_label);
    network_box->setLayout(network_box_layout);

    // One box, same shape as Game Controllers: the enable flag and default
    // channel at the top, the connected ports below.
    QGroupBox *midi_box = new QGroupBox(tr("MIDI"));
    midi_box->setToolTip(tr("Configure MIDI behaviour and list all connected MIDI ports"));

    midi_enable_check = new QCheckBox(tr("Enable incoming MIDI cues"));
    setCheckIcon(midi_enable_check, kMidiCuesSvg);
    midi_enable_check->setToolTip(tr("Enable or disable automatic conversion of incoming MIDI messages to cue events"));

    midi_default_channel_combo = new QComboBox();
    midi_default_channel_combo->addItem("* (" + tr("all") + ")");
    for (int ch = 1; ch <= 16; ++ch) {
        midi_default_channel_combo->addItem(QString::number(ch));
    }
    midi_default_channel_combo->setMaxVisibleItems(17);
    midi_default_channel_combo->setMinimumContentsLength(2);
    midi_default_channel_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon) ;

    QLabel *midi_default_channel_label = new QLabel;
    midi_default_channel_label->setText(tr("Default MIDI out channel"));
    midi_default_channel_label->setToolTip(tr("Default MIDI Channel to send messages to (* means all)"));

    QGridLayout *midi_default_channel_layout = new QGridLayout();

    midi_default_channel_combo->setToolTip(tr("Default MIDI Channel to send messages to  (* means all)"));

    midi_default_channel_layout->addWidget(midi_default_channel_combo, 0, 0);
    midi_default_channel_layout->addWidget(midi_default_channel_label, 0, 1);

    midi_in_ports_list = new DeviceListWidget(tr("No connected input devices"));
    midi_out_ports_list = new DeviceListWidget(tr("No connected output devices"));
    midi_in_ports_list->setObjectName("midi-in-ports-list");
    midi_in_ports_list->setAccessibleName(tr("MIDI input ports"));
    midi_out_ports_list->setObjectName("midi-out-ports-list");
    midi_out_ports_list->setAccessibleName(tr("MIDI output ports"));
    midi_in_ports_list->setToolTip(tr("MIDI input devices send MIDI messages directly to Sonic Pi and are received as cue events (similar to incoming OSC messages and internal cues)."));
    midi_out_ports_list->setToolTip(tr("MIDI output devices receive MIDI messages directly from Sonic Pi which can be sent via the midi_* fns."));

    QLabel *midi_in_header = new QLabel(tr("Inputs"));
    QLabel *midi_out_header = new QLabel(tr("Outputs"));
    midi_in_header->setStyleSheet("font-weight: bold;");
    midi_out_header->setStyleSheet("font-weight: bold;");

    connect(midi_in_ports_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit midiPortEnabledChanged("in", name, enabled); });
    connect(midi_out_ports_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit midiPortEnabledChanged("out", name, enabled); });

    QVBoxLayout *midi_box_layout = new QVBoxLayout;
    midi_box_layout->addWidget(midi_enable_check);
    midi_box_layout->addLayout(midi_default_channel_layout);
    midi_box_layout->addSpacing(8);
    midi_box_layout->addWidget(midi_in_header);
    midi_box_layout->addWidget(midi_in_ports_list);
    midi_box_layout->addSpacing(8);
    midi_box_layout->addWidget(midi_out_header);
    midi_box_layout->addWidget(midi_out_ports_list);
    midi_box_layout->addStretch(1);

    midi_box->setLayout(midi_box_layout);

    QGroupBox *gamepad_box = new QGroupBox(tr("Game Controllers"));
    gamepad_box->setToolTip(tr("Configure game controller behaviour"));

    gamepad_enable_check = new QCheckBox(tr("Enable incoming gamepad cues"));
    setCheckIcon(gamepad_enable_check, kGamepadCuesSvg);
    gamepad_enable_check->setToolTip(tr("Enable or disable automatic conversion of game controller button and axis events to cue events."));

    gamepad_devices_list = new DeviceListWidget(tr("No connected game controllers"));
    gamepad_devices_list->setObjectName("gamepad-devices-list");
    gamepad_devices_list->setAccessibleName(tr("Game controllers"));
    gamepad_devices_list->setToolTip(tr("Connected game controllers send button and axis events to Sonic Pi which are received as cue events."));

    connect(gamepad_devices_list, &DeviceListWidget::deviceToggled, this,
            [this](const QString& name, bool enabled) { emit gamepadDeviceEnabledChanged(name, enabled); });

    QVBoxLayout *gamepad_box_layout = new QVBoxLayout;
    gamepad_box_layout->addWidget(gamepad_enable_check);
    gamepad_box_layout->addWidget(gamepad_devices_list);
    gamepad_box->setLayout(gamepad_box_layout);

    QGridLayout *io_tab_layout = new QGridLayout();
    io_tab_layout->addWidget(midi_box, 0, 0, 2, 1);
    io_tab_layout->addWidget(gamepad_box, 0, 1);
    io_tab_layout->addWidget(network_box, 1, 1);

    ioTab->setLayout(io_tab_layout);
    return ioTab;
}

/**
 * create Editor Tab of Preferences Widget
 */
QGroupBox* SettingsWidget::createEditorPrefsTab() {
    QGroupBox *editor_box = new QGroupBox();
    QGroupBox *editor_show_panels_box = new QGroupBox(tr("Show Panels"));
    editor_show_panels_box->setToolTip(tr("Show and hide information panes such as the scope and log."));
    QGroupBox *editor_display_box = new QGroupBox(tr("Show and Hide"));
    editor_display_box->setToolTip(tr("Configure editor display options."));
    QGroupBox *automation_box = new QGroupBox(tr("Automation / Misc"));
    automation_box->setToolTip(tr("Configure automation and other features."));

    auto_indent_on_run = new QCheckBox(tr("Auto-align"));
    setCheckIcon(auto_indent_on_run, kAutoAlignSvg);
    auto_indent_on_run->setToolTip(tr("Automatically align code on Enter, Run and Tab.\nWhen disabled, Tab indents normally and code is only aligned via Code > Align Code."));

    show_line_numbers = new QCheckBox(tr("Show line numbers"));
    setCheckIcon(show_line_numbers, kLineNumbersSvg);
    show_line_numbers->setToolTip(tr("Toggle line number visibility."));

    show_autocompletion = new QCheckBox(tr("Show code completion"));
    setCheckIcon(show_autocompletion, kAutocompletionSvg);
    show_autocompletion->setToolTip(tr("When enabled, Sonic Pi's editor will attempt to autocomplete your code with suggestions. When disabled, these suggestions will not be visible."));

    show_completion_help = new QCheckBox(tr("Show code completion help"));
    setCheckIcon(show_completion_help, kCompletionHelpSvg);
    show_completion_help->setToolTip(tr("When enabled, the code completion popup includes helper panes - documentation, a note keyboard and value sliders. When disabled, it shows just the list of suggestions."));

    show_context = new QCheckBox(tr("Show code context"));
    setCheckIcon(show_context, kCodeContextSvg);
    show_context->setToolTip(tr("When enabled, Sonic Pi's editor will show a pane which will display context-specific information for the code such as the current line and position of the cursor."));

    speak_transport = new QCheckBox(tr("Speak run and stop"));
    setCheckIcon(speak_transport, kSpeakTransportSvg);
    speak_transport->setToolTip(tr("When enabled, a screen reader announces \"Run started\" and \"Stopped\". Disable this if you'd rather hear the very start of your audio without it being ducked by the announcement."));

    reduce_motion = new QCheckBox(tr("Reduce animations"));
    setCheckIcon(reduce_motion, kReduceMotionSvg);
    reduce_motion->setToolTip(tr("When enabled, Sonic Pi keeps its interface still: panes and popups appear in place instead of sliding or gliding. Also switched on automatically while your operating system's reduce-animations accessibility setting is active."));

    show_log = new QCheckBox(tr("Show log"));
    setCheckIcon(show_log, kShowLogSvg);
    show_log->setToolTip(tr("Toggle visibility of the log."));
    show_log->setProperty("tipShortcut", shortcutStrShiftMeta('L'));
    show_log->setChecked(true);

    show_cues = new QCheckBox(tr("Show cue log"));
    setCheckIcon(show_cues, kShowCuesSvg);
    show_cues->setToolTip(tr("Toggle visibility of cue log which displays internal cues & incoming OSC/MIDI messages."));
    show_cues->setProperty("tipShortcut", shortcutStrShiftMeta('C'));
    show_cues->setChecked(true);

    show_metro = new QCheckBox(tr("Show Link metronome controls"));
    setCheckIcon(show_metro, kShowMetroSvg);
    show_metro->setToolTip(tr("Toggle visibility of the Link metronome controls."));
    show_cues->setChecked(true);

    show_buttons = new QCheckBox(tr("Show buttons"));
    setCheckIcon(show_buttons, kShowButtonsSvg);
    show_buttons->setToolTip(tr("Toggle visibility of the control buttons."));
    show_buttons->setProperty("tipShortcut", shortcutStrShiftMeta('B'));
    show_buttons->setChecked(true);

    show_editor_toolbar = new QCheckBox(tr("Show editor toolbar"));
    setCheckIcon(show_editor_toolbar, kEditorToolbarSvg);
    show_editor_toolbar->setToolTip(tr("Toggle visibility of the editor's floating toolbar (undo/redo, cut/copy/paste, find)."));
    show_editor_toolbar->setChecked(true);
    show_tabs = new QCheckBox(tr("Show tabs"));
    setCheckIcon(show_tabs, kShowTabsSvg);
    show_tabs->setChecked(true);
    show_tabs->setToolTip(tr("Toggle visibility of the buffer selection tabs."));
    full_screen = new QCheckBox(tr("Full screen"));
    setCheckIcon(full_screen, kFullScreenSvg);
    full_screen->setToolTip(tr("Toggle full screen mode."));
    full_screen->setProperty("tipShortcut", shortcutStrShiftMeta('F'));

    show_titles = new QCheckBox(tr("Show titles"));
    setCheckIcon(show_titles, kShowTitlesSvg);
    show_titles->setToolTip(tr("Toggle the title visibility for the scope, log, cue and other information panes"));
    show_titles->setChecked(true);

    hide_menubar_in_fullscreen = new QCheckBox(tr("Hide menu bar in full screen mode"));
    setCheckIcon(hide_menubar_in_fullscreen, kHideMenuBarSvg);
    hide_menubar_in_fullscreen->setToolTip(tr("Automatically hide the menubar when the app is in full screen mode. Note that the menubar is always visible when not in full screen mode."));
    hide_menubar_in_fullscreen->setChecked(false);

    QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
    QVBoxLayout *editor_show_panels_box_layout = new QVBoxLayout;
    QVBoxLayout *automation_box_layout = new QVBoxLayout;

    editor_show_panels_box_layout->addWidget(show_log);
    editor_show_panels_box_layout->addWidget(show_cues);
    editor_show_panels_box_layout->addWidget(show_context);
    editor_show_panels_box_layout->addWidget(show_metro);

    editor_display_box_layout->addWidget(show_line_numbers);
    editor_display_box_layout->addWidget(show_autocompletion);
    editor_display_box_layout->addWidget(show_completion_help);
    editor_display_box_layout->addWidget(show_buttons);
    editor_display_box_layout->addWidget(show_editor_toolbar);
    editor_display_box_layout->addWidget(show_tabs);
    editor_display_box_layout->addWidget(show_titles);
#ifndef Q_OS_MAC
    // Don't enable this on Mac as macOS autohides the menubar on
    // fullscreen anyway
    editor_display_box_layout->addWidget(hide_menubar_in_fullscreen);
#endif

    editor_show_panels_box->setLayout(editor_show_panels_box_layout);
    editor_display_box->setLayout(editor_display_box_layout);

    QGroupBox *accessibility_box = new QGroupBox(tr("Accessibility"));
    accessibility_box->setToolTip(tr("Settings that support screen readers and other assistive tools."));
    QVBoxLayout *accessibility_box_layout = new QVBoxLayout;
    accessibility_box_layout->addWidget(speak_transport);
    accessibility_box_layout->addWidget(reduce_motion);
    accessibility_box->setLayout(accessibility_box_layout);


    automation_box_layout->addWidget(auto_indent_on_run);
    automation_box_layout->addWidget(full_screen);

    automation_box->setLayout(automation_box_layout);

    QGroupBox *debug_box = new QGroupBox(tr("Logging"));
    debug_box->setToolTip(tr("Configure debug behaviour"));

    log_synths = new QCheckBox(tr("Log synths"));
    setCheckIcon(log_synths, kLogSynthsSvg);
    log_synths->setToolTip(tr("If disabled, activity such as synth and sample triggering will not be printed to the log by default."));

    clear_output_on_run = new QCheckBox(tr("Clear log on run"));
    setCheckIcon(clear_output_on_run, kClearLogSvg);
    clear_output_on_run->setToolTip(tr("If enabled, the log is cleared each time the run button is pressed."));

    log_cues = new QCheckBox(tr("Log cues"));
    setCheckIcon(log_cues, kLogCuesSvg);
    log_cues->setToolTip(tr("If disabled, cues will still trigger. However, they will not be visible in the logs."));

    log_auto_scroll = new QCheckBox(tr("Auto-scroll log"));
    setCheckIcon(log_auto_scroll, kAutoScrollSvg);
    log_auto_scroll->setToolTip(tr("If enabled, the log is scrolled to the bottom after every new message is displayed."));

    QVBoxLayout *debug_box_layout = new QVBoxLayout;
    debug_box_layout->addWidget(log_synths);
    debug_box_layout->addWidget(log_cues);
    debug_box_layout->addWidget(log_auto_scroll);
    debug_box_layout->addWidget(clear_output_on_run);
    debug_box->setLayout(debug_box_layout);

    // Two independent columns rather than a shared grid: grid rows take the
    // taller of the two sides, stretching boxes to match their neighbours.
    // Independent columns let each side pack to its own content. The slack
    // goes between the boxes rather than below them, so the shorter column
    // spreads to end level with the taller one instead of leaving a hole at
    // the bottom.
    QVBoxLayout *leftEditorPrefs = new QVBoxLayout;
    leftEditorPrefs->addWidget(editor_display_box);
    leftEditorPrefs->addStretch(1);
    leftEditorPrefs->addWidget(automation_box);

    QVBoxLayout *rightEditorPrefs = new QVBoxLayout;
    rightEditorPrefs->addWidget(debug_box);
    rightEditorPrefs->addStretch(1);
    rightEditorPrefs->addWidget(editor_show_panels_box);
    rightEditorPrefs->addStretch(1);
    rightEditorPrefs->addWidget(accessibility_box);

    QHBoxLayout *editorPrefsColumns = new QHBoxLayout;
    editorPrefsColumns->addLayout(leftEditorPrefs, 1);
    editorPrefsColumns->addLayout(rightEditorPrefs, 1);

    editor_box->setLayout(editorPrefsColumns);
    return editor_box;
}

/**
 * Create Visualization Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createVisualizationPrefsTab() {
    QGroupBox *viz_box = new QGroupBox();
    viz_box->setToolTip(tr("Themes, transparency and settings useful for performing with Sonic Pi"));

    QGroupBox *theme_box = new QGroupBox(tr("Theme"));
    theme_box->setToolTip(tr("Configure the Sonic Pi colour scheme and look and feel."));

    // One checkable button per colour scheme, made mutually exclusive by the
    // button group. Icons are a separate choice (proIconsCheck) so any scheme
    // can pair with either icon set.
    colourModeButtonGroup = new QButtonGroup(this);
    lightModeCheck = new ThemeCard(tr("Light"));
    darkModeCheck = new ThemeCard(tr("Dark"));
    highContrastModeCheck = new ThemeCard(tr("High Contrast"));
    mildModeCheck = new ThemeCard(tr("Mild Dark"));
    phosphorModeCheck = new ThemeCard(tr("Phosphor"));
    signalModeCheck = new ThemeCard(tr("Signal"));
    colourModeButtonGroup->addButton(lightModeCheck, 0);
    colourModeButtonGroup->addButton(darkModeCheck, 1);
    colourModeButtonGroup->addButton(highContrastModeCheck, 2);
    colourModeButtonGroup->addButton(mildModeCheck, 3);
    colourModeButtonGroup->addButton(phosphorModeCheck, 4);
    colourModeButtonGroup->addButton(signalModeCheck, 5);

    lightModeCheck->setToolTip(tr("Light colour scheme."));
    darkModeCheck->setToolTip(tr("Dark colour scheme."));
    highContrastModeCheck->setToolTip(tr("High-contrast colour scheme for maximum legibility."));
    mildModeCheck->setToolTip(tr("Mild Dark: a softer, low-contrast dark colour scheme."));
    phosphorModeCheck->setToolTip(tr("Phosphor: a green-on-black CRT colour scheme."));
    signalModeCheck->setToolTip(tr("Signal: high-contrast blue-and-gold colour scheme."));

    // Orthogonal to the colour scheme: swap the classic toolbar icons for the
    // compact Pro set. High Contrast keeps its own icons, so this has no effect
    // there (disabled while High Contrast is selected).
    proIconsCheck = new QCheckBox(tr("Pro icons"));
    setCheckIcon(proIconsCheck, kProIconsSvg);
    proIconsCheck->setToolTip(tr("Use the compact Pro toolbar icon set instead of the classic icons."));

    // Global greyscale toggle: renders every interface colour as luma-matched
    // grey. Independent of the hue rotation above.
    monochromeCheck = new QCheckBox(tr("Monochrome"));
    setCheckIcon(monochromeCheck, kMonochromeSvg);
    monochromeCheck->setToolTip(tr("Show the whole interface in greyscale."));

    // Global colour inversion (photo-negative) over the whole interface.
    invertCheck = new QCheckBox(tr("Invert colours"));
    setCheckIcon(invertCheck, kInvertColoursSvg);
    invertCheck->setToolTip(tr("Invert every interface colour (photo-negative)."));

    // Theme picker: a grid of checkable cards, one per theme, each painted in its
    // theme's colours with a toolbar-icon preview above the theme name.
    QWidget* themeGrid = new QWidget();
    themeGrid->setObjectName("themeGrid");
    QGridLayout* themeGridLayout = new QGridLayout(themeGrid);
    themeGridLayout->setContentsMargins(0, 0, 0, 0);
    themeGridLayout->setHorizontalSpacing(6);
    themeGridLayout->setVerticalSpacing(6);

    // Card icon montage (the three Greek glyphs, tinted per card) is built by
    // makeThemeCardGlyphs() so it can be regenerated when the global colour
    // filters change; iconSize here drives the card sizing below.
    const QSize iconSize(ScaleWidthForDPI(80), ScaleHeightForDPI(26));

    // bg/fg: card background + name text. accent: the scheme's signature colour,
    // used to tint the glyphs so each card reads as its own theme. border: resting.
    struct ThemeSwatch { QPushButton* btn; const char* bg; const char* fg; const char* accent; const char* border; int row; int col; };
    const char* kGrey = "#5a7f7f7f";   // resting border colour (AARRGGBB)
    const ThemeSwatch swatches[] = {
        { lightModeCheck,        "#ffffff", "#3c3c3c", "#ff1493", kGrey,     0, 0 },
        { darkModeCheck,         "#1a1a1a", "#ededed", "#ff1493", kGrey,     0, 1 },
        { highContrastModeCheck, "#ffffff", "#000000", "#99004a", "#000000", 0, 2 },
        { mildModeCheck,         "#1e1e1e", "#d4d4d4", "#ce9178", kGrey,     1, 0 },
        { phosphorModeCheck,     "#0a0e0a", "#64d450", "#39ff14", kGrey,     1, 1 },
        { signalModeCheck,       "#000000", "#ffffff", "#1e90ff", "#ffd700", 1, 2 },   // black/white, blue + gold
    };
    // Card width is driven by the widest label (the icon sits above the name).
    QFont measureFont = lightModeCheck->font();
    measureFont.setPixelSize(ScaleHeightForDPI(19));   // matches the drawn "medium" size
    QFontMetrics fm(measureFont);
    int maxTextW = 0;
    for (const ThemeSwatch& s : swatches)
        maxTextW = qMax(maxTextW, fm.horizontalAdvance(s.btn->text()));
    // Wide enough for the label and the four-glyph icon row above it.
    const int btnMinW = qMax(maxTextW + ScaleWidthForDPI(28), iconSize.width() + ScaleWidthForDPI(18));
    // A QPushButton doesn't size to its child layout, so drive the height here.
    const int cardH = iconSize.height() + ScaleHeightForDPI(38);
    for (const ThemeSwatch& s : swatches) {
        s.btn->setCheckable(true);
        s.btn->setCursor(Qt::PointingHandCursor);
        s.btn->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        s.btn->setMinimumWidth(btnMinW);
        // Lock the height on the widget (not via QSS min-height/height): re-setting
        // a QSS height on a button with a child layout makes it creep taller on
        // every repolish, which refreshThemeCards() would trigger on each rotate.
        s.btn->setFixedHeight(cardH);
        // Card fill + border are custom-painted (antialiased) by ThemeCard.
        static_cast<ThemeCard*>(s.btn)->setCardColors(
            QColor(QString::fromLatin1(s.bg)), QColor(QString::fromLatin1(s.border)));
        s.btn->setStyleSheet("QPushButton { padding:0; } QLabel { background:transparent; }");

        // Icon above name, as child labels (a QPushButton lays its own icon+text
        // horizontally). Labels are click-through so the button receives the click.
        QVBoxLayout* card = new QVBoxLayout(s.btn);
        card->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(4),
                                 ScaleWidthForDPI(10), ScaleHeightForDPI(8));
        card->setSpacing(ScaleHeightForDPI(3));
        QLabel* iconLbl = new QLabel;
        iconLbl->setPixmap(makeThemeCardGlyphs(QColor(QString::fromLatin1(s.accent))));
        iconLbl->setAlignment(Qt::AlignCenter);
        iconLbl->setAttribute(Qt::WA_TransparentForMouseEvents);
        const QString name = s.btn->text();
        QLabel* nameLbl = new QLabel(name);
        nameLbl->setAlignment(Qt::AlignCenter);
        nameLbl->setAttribute(Qt::WA_TransparentForMouseEvents);
        nameLbl->setStyleSheet(QString("background:transparent; color:%1;")
                                   .arg(QColor(QString::fromLatin1(s.fg)).name()));
        // Remember each card's widgets + base colours so refreshThemeCards() can
        // re-preview them through the active filters (hue / mono / invert).
        m_themeCards.append({ s.btn, iconLbl, nameLbl,
            QColor(QString::fromLatin1(s.bg)), QColor(QString::fromLatin1(s.fg)),
            QColor(QString::fromLatin1(s.accent)), QColor(QString::fromLatin1(s.border)) });
        // Stretches above and below centre the icon+name group vertically.
        card->addStretch(1);
        card->addWidget(iconLbl);
        card->addWidget(nameLbl);
        card->addStretch(1);
        // Name is shown by nameLbl; set it as the accessible name before clearing
        // the button's own text.
        s.btn->setAccessibleName(name);
        s.btn->setText("");

        themeGridLayout->addWidget(s.btn, s.row, s.col);
    }
    // Equal minimum width and equal stretch on the three columns so the cards
    // share the full box width, keeping the grid flush with the rows below.
    for (int c = 0; c < 3; ++c) {
        themeGridLayout->setColumnMinimumWidth(c, btnMinW);
        themeGridLayout->setColumnStretch(c, 1);
    }

    QVBoxLayout *theme_box_layout = new QVBoxLayout;
    theme_box_layout->addWidget(themeGrid);
    // Breathing room between the theme cards and the rotate-colour dial below.
    theme_box_layout->addSpacing(ScaleHeightForDPI(26));

    // Colour hue-rotation dial (amp-style ArcDial). Drag vertically to set the
    // rotation; it stops hard at 0 and 359 (no wrap) and shows no value.
    m_hueDial = new ArcDial(this);
    m_hueDial->setRange(0, 359);
    m_hueDial->setWrapping(false);
    m_hueDial->setValueSuffix("°");
    m_hueDial->setValueFontRole(FontRole::XLarge);
    m_hueDial->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    m_hueDial->setAccessibleName(tr("Rotate hue"));
    m_hueDial->setProperty("tipTitle", tr("Rotate Hue"));
    m_hueDial->setToolTip(tr("Drag or scroll to rotate the hue of every colour in the interface. "
                             "Double-click the value to type an exact one."));
    m_hueTimer = new QTimer(this);
    m_hueTimer->setSingleShot(true);
    connect(m_hueDial, &QDial::valueChanged, this, &SettingsWidget::hueRotationChanged);
    connect(m_hueDial, &QAbstractSlider::sliderReleased, this, [this]() { emit themeChanged(); });

    // Hue spread: how far the other colours open out from the accent. 0 is the
    // theme as authored, the top is even spacing.
    m_spreadDial = new ArcDial(this);
    m_spreadDial->setRange(SonicPiTheme::kHueSpreadDefault, SonicPiTheme::kHueSpreadEven);
    m_spreadDial->setWrapping(false);
    m_spreadDial->setValueFontRole(FontRole::XLarge);
    m_spreadDial->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    m_spreadDial->setAccessibleName(tr("Spread hue"));
    m_spreadDial->setProperty("tipTitle", tr("Spread Hue"));
    m_spreadDial->setToolTip(tr("Drag or scroll to spread the interface's colours away from the "
                                "main colour. 0 is the theme as designed, 100 spaces every colour "
                                "evenly around the wheel. Double-click the value to type an exact one."));
    connect(m_spreadDial, &QDial::valueChanged, this, &SettingsWidget::hueSpreadChanged);
    connect(m_spreadDial, &QAbstractSlider::sliderReleased, this, [this]() { emit themeChanged(); });

    const QString kHueCaption = tr("Rotate Hue");
    const QString kSpreadCaption = tr("Spread Hue");
    m_hueCaption = new QLabel(kHueCaption);
    m_hueCaption->setAlignment(Qt::AlignHCenter);
    m_hueCaption->setToolTip(tr("Drag the dial to rotate the hue of every colour in the interface."));
    m_spreadCaption = new QLabel(kSpreadCaption);
    m_spreadCaption->setAlignment(Qt::AlignHCenter);
    m_spreadCaption->setToolTip(m_spreadDial->toolTip());

    // Reserve the wider of the two texts so the mid-drag swap to
    // "Release to apply" never reflows the row.
    const int dragCaptionW = m_hueCaption->fontMetrics().horizontalAdvance(tr("Release to apply"));
    m_hueCaption->setMinimumWidth(
        qMax(dragCaptionW, m_hueCaption->fontMetrics().horizontalAdvance(kHueCaption)));
    m_spreadCaption->setMinimumWidth(
        qMax(dragCaptionW, m_spreadCaption->fontMetrics().horizontalAdvance(kSpreadCaption)));

    // The re-theme is deferred to release (too expensive per drag step), so
    // mid-drag only the dial tracks; the caption says so rather than looking dead.
    connect(m_hueDial, &QAbstractSlider::sliderPressed, this,
            [this, kHueCaption]() { setDialCaptionDragging(m_hueCaption, kHueCaption, true); });
    connect(m_hueDial, &QAbstractSlider::sliderReleased, this,
            [this, kHueCaption]() { setDialCaptionDragging(m_hueCaption, kHueCaption, false); });
    connect(m_spreadDial, &QAbstractSlider::sliderPressed, this,
            [this, kSpreadCaption]() { setDialCaptionDragging(m_spreadCaption, kSpreadCaption, true); });
    connect(m_spreadDial, &QAbstractSlider::sliderReleased, this,
            [this, kSpreadCaption]() { setDialCaptionDragging(m_spreadCaption, kSpreadCaption, false); });

    auto dialColumn = [](ArcDial* dial, QLabel* caption) {
        QVBoxLayout* col = new QVBoxLayout;
        col->addStretch(1);
        col->addWidget(dial, 0, Qt::AlignHCenter);
        col->addWidget(caption, 0, Qt::AlignHCenter);
        col->addStretch(1);
        return col;
    };

    // Returns all four mods to their defaults; the scheme itself is kept.
    m_resetModsButton = new QPushButton(tr("Reset Theme"));
    m_resetModsButton->setToolTip(tr("Return hue rotation, hue spread, monochrome "
                                     "and invert to their defaults. The chosen "
                                     "scheme is kept."));
    connect(m_resetModsButton, &QPushButton::clicked, this, &SettingsWidget::resetThemeMods);

    QVBoxLayout* toggleCol = new QVBoxLayout;
    toggleCol->setSpacing(ScaleHeightForDPI(4));
    toggleCol->addWidget(proIconsCheck);
    toggleCol->addWidget(monochromeCheck);
    toggleCol->addWidget(invertCheck);

    // Window transparency as an amp-style ArcDial (same feel as the volume,
    // hue and flash dials), with the percentage shown in the hub. In the
    // Theme box beside the hue dials: all three are appearance dials.
    gui_transparency_slider = new ArcDial(this);
    gui_transparency_slider->setWrapping(false);
    gui_transparency_slider->setRange(0, 100);
    gui_transparency_slider->setValueSuffix("%");
    gui_transparency_slider->setValueFontRole(FontRole::XLarge);
    gui_transparency_slider->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    gui_transparency_slider->setAccessibleName(tr("Transparency"));
    gui_transparency_slider->setProperty("tipTitle", tr("Transparency"));
    gui_transparency_slider->setToolTip(tr("Drag or scroll to change how see-through the Sonic Pi window is."));
    QLabel* transparencyCaption = new QLabel(tr("Transparency"));
    transparencyCaption->setAlignment(Qt::AlignHCenter);
    transparencyCaption->setToolTip(gui_transparency_slider->toolTip());

    // The three dials in their own row directly beneath the theme cards,
    // then the toggles below with Reset on the right (mirrors the Audio
    // Device box).
    QHBoxLayout* dials_row = new QHBoxLayout;
    dials_row->addLayout(dialColumn(m_hueDial, m_hueCaption), 1);
    dials_row->addLayout(dialColumn(m_spreadDial, m_spreadCaption), 1);
    dials_row->addLayout(dialColumn(gui_transparency_slider, transparencyCaption), 1);
    theme_box_layout->addLayout(dials_row);
    theme_box_layout->addSpacing(ScaleHeightForDPI(12));

    QHBoxLayout* mods_row = new QHBoxLayout;
    mods_row->addLayout(toggleCol);
    mods_row->addStretch(1);
    mods_row->addWidget(m_resetModsButton, 0, Qt::AlignVCenter);
    theme_box_layout->addLayout(mods_row);
    theme_box->setLayout(theme_box_layout);

    QGroupBox *scope_box = new QGroupBox(tr("Show and Hide Scope"));
    QGroupBox *scope_box_kinds = new QGroupBox(tr("Scope Kinds"));

    scope_box_kinds_layout = new QVBoxLayout;

    QVBoxLayout *scope_box_layout = new QVBoxLayout;

    scopeSignalMap = new QSignalMapper(this);
    show_scopes = new QCheckBox(tr("Show scopes"));
    setCheckIcon(show_scopes, kShowScopesSvg);
    show_scopes->setToolTip(tr("Toggle the visibility of the audio oscilloscopes."));
    show_scope_labels = new QCheckBox(tr("Show scope labels"));
    setCheckIcon(show_scope_labels, kScopeLabelsSvg);
    show_scope_labels->setToolTip(tr("Toggle the visibility of the labels for the audio oscilloscopes"));
    show_scope_labels->setChecked(true);

    scope_box_kinds->setLayout(scope_box_kinds_layout);
    scope_box_kinds->setToolTip(tr("The audio oscilloscope comes in several flavours which may be viewed independently or all together:\n\nLissajous - illustrates the phase relationship between the left and right channels\nMirror Stereo - simple left/right composite wave, with left on top, right on bottom\nMono - shows a combined view of the left and right channels (using RMS)\nSpectrum - shows the sound frequencies as a spectrum, from low to high frequencies\nStereo - shows two independent scopes for left and right channels"));
    scope_box_layout->addWidget(show_scopes);
    scope_box_layout->addWidget(show_scope_labels);
    scope_box->setLayout(scope_box_layout);

    // In-editor visuals driven by the running audio: trigger flashes and the
    // per-live_loop mini scopes.
    QGroupBox *editor_visuals_box = new QGroupBox(tr("Editor Visuals"));
    QVBoxLayout *editor_visuals_box_layout = new QVBoxLayout;

    flash_code = new QCheckBox(tr("Flash code on sound trigger"));
    setCheckIcon(flash_code, kFlashCodeSvg);
    flash_code->setToolTip(tr("When enabled, the editor briefly washes the code responsible for each sound as it is triggered."));

    flash_gutter = new QCheckBox(tr("Flash gutter on sound trigger"));
    setCheckIcon(flash_gutter, kFlashGutterSvg);
    flash_gutter->setToolTip(tr("When enabled, the editor briefly shows a dot in the gutter next to the line responsible for each sound as it is triggered."));

    show_loop_scopes = new QCheckBox(tr("Show live loop scopes"));
    setCheckIcon(show_loop_scopes, kLoopScopesSvg);
    show_loop_scopes->setToolTip(tr("When enabled, each running live loop shows a small oscilloscope and spectrum of its own audio next to its line in the editor."));
    loop_scope_scroll = new QCheckBox(tr("Scrolling live loop scopes"));
    setCheckIcon(loop_scope_scroll, kLoopScrollSvg);
    loop_scope_scroll->setToolTip(tr("When enabled, live loop scopes scroll their recent audio like a strip chart. When disabled, they hold a steady waveform like the main scope."));

    // Brightness as an amp-style ArcDial (same feel as the volume + hue
    // dials), with the percentage shown in the hub.
    flash_brightness_slider = new ArcDial(this);
    flash_brightness_slider->setWrapping(false);
    flash_brightness_slider->setRange(5, 100);
    flash_brightness_slider->setValueSuffix("%");
    flash_brightness_slider->setValueFontRole(FontRole::XLarge);
    flash_brightness_slider->setFixedSize(ScaleWidthForDPI(108), ScaleHeightForDPI(108));
    flash_brightness_slider->setAccessibleName(tr("Flash brightness"));
    flash_brightness_slider->setProperty("tipTitle", tr("Flash Brightness"));
    flash_brightness_slider->setToolTip(tr("Drag or scroll to change how strongly the code flash washes the line."));

    QLabel *flash_brightness_label = new QLabel(tr("Flash Brightness"));
    flash_brightness_label->setAlignment(Qt::AlignHCenter);

    // Checkboxes on the left, dial to their right — same arrangement as the
    // hue dial in the Theme box. The box hugs its content (the columns pack
    // to their content), so both halves centre naturally.
    QVBoxLayout *flash_checks_col = new QVBoxLayout;
    flash_checks_col->addStretch(1);
    flash_checks_col->addWidget(flash_code);
    flash_checks_col->addWidget(flash_gutter);
    flash_checks_col->addWidget(show_loop_scopes);
    flash_checks_col->addWidget(loop_scope_scroll);
    flash_checks_col->addStretch(1);
    QVBoxLayout *flash_dial_col = new QVBoxLayout;
    flash_dial_col->addStretch(1);
    flash_dial_col->addWidget(flash_brightness_slider, 0, Qt::AlignHCenter);
    flash_dial_col->addWidget(flash_brightness_label, 0, Qt::AlignHCenter);
    flash_dial_col->addStretch(1);
    QHBoxLayout *editor_visuals_row = new QHBoxLayout;
    editor_visuals_row->addLayout(flash_checks_col);
    editor_visuals_row->addSpacing(ScaleWidthForDPI(40));
    editor_visuals_row->addLayout(flash_dial_col);
    editor_visuals_row->addStretch(1);
    editor_visuals_box_layout->addLayout(editor_visuals_row);
    editor_visuals_box->setLayout(editor_visuals_box_layout);

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // --- Window publishing — the same switch as the IO menu's publish
    // action. The technology (and so the name) differs per platform:
    // Syphon on macOS, Spout on Windows; neither exists on Linux, so the
    // group is absent there. MainWindow::setWindowPublishing owns the
    // publisher and keeps both views in step. Segmented pill in the
    // Recording control's style. ---
#ifdef Q_OS_MAC
    const QString publishTech = QStringLiteral("Syphon");
#else
    const QString publishTech = QStringLiteral("Spout");
#endif
    QGroupBox *publishGroup = new QGroupBox(tr("Publish Window via %1").arg(publishTech));
    publishGroup->setToolTip(tr("Share the Sonic Pi window with other applications as a %1 video feed.").arg(publishTech));

    const QColor pubIconColor = QApplication::palette().color(QPalette::WindowText);
    const QColor pubIconOnColor = QApplication::palette().color(QPalette::HighlightedText);
    const int pubIconPx = ScaleHeightForDPI(32);

    publish_off_radio = new QToolButton();
    publish_off_radio->setText(tr("Off"));
    publish_off_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    publish_off_radio->setIcon(makeSvgToggleIcon(kCastOffSvg, pubIconColor, pubIconOnColor, pubIconPx));
    publish_off_radio->setIconSize(QSize(pubIconPx, pubIconPx));
    publish_off_radio->setToolTip(tr("Stop sharing the Sonic Pi window."));

    publish_send_radio = new QToolButton();
    publish_send_radio->setText(tr("Publish via %1").arg(publishTech));
    publish_send_radio->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    publish_send_radio->setIcon(makeSvgToggleIcon(kCastSvg, pubIconColor, pubIconOnColor, pubIconPx));
    publish_send_radio->setIconSize(QSize(pubIconPx, pubIconPx));
    publish_send_radio->setToolTip(tr("Publish the Sonic Pi window as a %1 video feed for other applications to receive.").arg(publishTech));

    QWidget* publishSegControl = new QWidget();
    publishSegControl->setObjectName("publishSegControl");
    // Styled by the shared "segmented control" rule in app.qss (house metrics).
    publishSegControl->setProperty("segmented", true);
    QHBoxLayout* publishSegLayout = new QHBoxLayout(publishSegControl);
    publishSegLayout->setContentsMargins(3, 3, 3, 3);
    publishSegLayout->setSpacing(3);

    // Button IDs 0/1 = off/sending, so idClicked(int) carries the choice
    // directly. idClicked only fires on user clicks, so syncWindowPublishing's
    // programmatic setChecked doesn't echo back.
    window_publish_group = new QButtonGroup(this);
    window_publish_group->setExclusive(true);
    for (QToolButton* b : { publish_off_radio, publish_send_radio }) {
        b->setCheckable(true);
        b->setCursor(Qt::PointingHandCursor);
        publishSegLayout->addWidget(b);
    }
    window_publish_group->addButton(publish_off_radio, 0);
    window_publish_group->addButton(publish_send_radio, 1);
    // Publishing is a live switch, not a persisted pref: every session
    // starts with the feed off, matching the menubar action.
    publish_off_radio->setChecked(true);
    equalizePublishSegments();

    QHBoxLayout *publishGroupLayout = new QHBoxLayout;
    publishGroupLayout->addStretch(1);
    publishGroupLayout->addWidget(publishSegControl);
    publishGroupLayout->addStretch(1);
    publishGroup->setLayout(publishGroupLayout);

    connect(window_publish_group, SIGNAL(idClicked(int)),
            this, SLOT(windowPublishingToggled(int)));
#endif

    // Two independent columns, as on the Editor tab: appearance settings on
    // the left, audio-driven visuals on the right. Slack goes between the
    // boxes, not below them, so both columns end level.
    QVBoxLayout *leftVizPrefs = new QVBoxLayout;
    leftVizPrefs->addWidget(theme_box);
    leftVizPrefs->addStretch(1);
    leftVizPrefs->addWidget(scope_box);

    QVBoxLayout *rightVizPrefs = new QVBoxLayout;
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    rightVizPrefs->addWidget(publishGroup);
    rightVizPrefs->addStretch(1);
#endif
    rightVizPrefs->addWidget(scope_box_kinds);
    rightVizPrefs->addStretch(1);
    rightVizPrefs->addWidget(editor_visuals_box);

    QHBoxLayout *vizPrefsColumns = new QHBoxLayout;
    vizPrefsColumns->addLayout(leftVizPrefs, 1);
    vizPrefsColumns->addLayout(rightVizPrefs, 1);

    viz_box->setLayout(vizPrefsColumns);

    return viz_box;
}

/**
 * create Update Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createUpdatePrefsTab() {
    QGroupBox *update_box = new QGroupBox(tr("Updates"));
    QSizePolicy updatesPrefSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
    check_updates = new QCheckBox(tr("Check for updates"));
    setCheckIcon(check_updates, kCheckUpdatesSvg);
    check_updates->setToolTip(tr("This check involves sending anonymous information about your platform and version."));
    check_updates_now = new QPushButton(tr("Check now"));
    check_updates_now->setFlat(true);
    check_updates_now->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed));
    check_updates_now->setToolTip(tr("Force a check for updates now. This check involves sending anonymous information about your platform and version."));
    visit_sonic_pi_net = new QPushButton(tr("Get update"));
    visit_sonic_pi_net->setSizePolicy(QSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed));
    visit_sonic_pi_net->setToolTip(tr("Visit http://sonic-pi.net to download new version"));
    visit_sonic_pi_net->setVisible(false);



    QGroupBox *update_info_box = new QGroupBox(tr("Update Info"));
    update_info_box->setMaximumWidth(350);
    QVBoxLayout *update_info_box_layout = new QVBoxLayout;
    update_info = new QLabel(tr("Sonic Pi update info"));
    update_info->setWordWrap(true);
    update_info_box_layout->addWidget(update_info);
    update_info_box->setLayout(update_info_box_layout);

    QVBoxLayout *update_box_layout = new QVBoxLayout;
    update_box_layout->addWidget(check_updates);

    update_box_layout->addWidget(check_updates_now);
    update_box_layout->addWidget(visit_sonic_pi_net);
    update_box->setLayout(update_box_layout);



    QGroupBox *update_prefs_box = new QGroupBox();
    QGridLayout *update_prefs_box_layout = new QGridLayout;
    update_prefs_box_layout->addWidget(update_info_box, 0, 0);
    update_prefs_box_layout->addWidget(update_box, 0, 1);
    update_prefs_box->setLayout(update_prefs_box_layout);
    return update_prefs_box;
}

/**
 * create Language Preferences Tab of Settings Widget
 */
QGroupBox* SettingsWidget::createLanguagePrefsTab() {
    QGroupBox *language_box = new QGroupBox(tr("Language"));
    language_box->setToolTip(tr("Configure language settings"));
    QSizePolicy languagePrefSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
    language_box->setSizePolicy(languagePrefSizePolicy);

    language_option_label = new QLabel;
    language_option_label->setText(tr("UI & Tutorial Language (Requires a restart to take effect)"));
    language_option_label->setToolTip(tr("Change the language of the UI & Tutorial (Requires a restart to take effect)"));

    language_combo = new QComboBox();
    add_language_combo_box_entries(language_combo);
    language_combo->setToolTip(tr("Change the language of the UI & Tutorial"));
    language_combo->setMinimumContentsLength(2);
    language_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLengthWithIcon);

    language_details_label = new QLabel;

    language_info_label = new QLabel;
    language_info_label->setText(tr("Translations have been generously provided by volunteers \non https://hosted.weblate.org/projects/sonic-pi/. Thank you! :)"));

    QVBoxLayout *language_box_layout = new QVBoxLayout;

    language_box_layout->addWidget(language_option_label);
    language_box_layout->addWidget(language_combo);
    language_box_layout->addWidget(language_details_label);
    language_box_layout->addWidget(language_info_label);

    if (piSettings->language == "system_language") {
      if (!sonicPii18n->isSystemLanguageAvailable()) {
          QGroupBox *translation_box = new QGroupBox("Translation");
          QLabel *go_translate = new QLabel;
          go_translate->setOpenExternalLinks(true);
          go_translate->setText(
                  "Sonic Pi hasn't been translated to " +
                  QLocale::languageToString(QLocale::system().language()) +
                  " yet.<br/>" +
                  "We rely on crowdsourcing to help create and maintain translations.<br/>" +
                  "<a href=\"https://github.com/sonic-pi-net/sonic-pi/blob/main/TRANSLATION.md\">" +
                  "Please consider helping to translate Sonic Pi to your language.</a> "
                  );
          go_translate->setTextFormat(Qt::RichText);
          language_box_layout->addWidget(go_translate);
      }
    }


    language_box->setLayout(language_box_layout);

    QGroupBox *language_prefs_box = new QGroupBox();
    QGridLayout *language_prefs_box_layout = new QGridLayout;
    language_prefs_box_layout->addWidget(language_box, 0, 0, 0, 0);
    language_prefs_box->setLayout(language_prefs_box_layout);
    return language_prefs_box;
}

// Default key string for a command under a given base preset.
static QString baseKeyFor(const ShortcutDef& d, const QString& base) {
    if (base == "win") return QString(d.win);
    if (base == "emacs") return QString(d.emacs);
    return QString(d.mac);
}

// Read a shortcut .ini: its "base" preset (left at the caller's default if the
// file omits it) plus the per-command overrides it stores.
static void readShortcutIni(const QString& path, QString& base, QMap<QString, QString>& overrides) {
    QSettings cfg(path, QSettings::IniFormat);
    base = cfg.value("base", base).toString();
    overrides.clear();
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        if (cfg.contains(d.id)) overrides.insert(d.id, cfg.value(d.id).toString());
    }
}

// Normalise a shortcut string to a comparable form so equivalent chords written
// differently (e.g. "MetaShift+." vs "ShiftMeta+.") compare equal. Returns
// "<sorted-modifiers>|<key>"; empty for an unset binding.
static QString canonicalChord(const QString& raw) {
    QString s = raw.trimmed().toLower();
    if (s.isEmpty()) return QString();
    QStringList parts = s.split('+', Qt::SkipEmptyParts);
    if (parts.isEmpty()) return QString();
    QString key = parts.takeLast();
    QString modBlob = parts.join("");
    QStringList mods;
    for (const QString& m : { QStringLiteral("ctrl"), QStringLiteral("shift"),
                              QStringLiteral("alt"), QStringLiteral("meta") }) {
        if (modBlob.contains(m)) mods << m;
    }
    mods.sort();
    return mods.join("+") + "|" + key;
}

// Delegate that restricts editing to the Shortcut column of a command row.
// Base key text (no modifiers), e.g. "R", "Left", "F1", ",".
static QString shortcutKeyName(int key) {
    if (key == 0 || key == Qt::Key_unknown) return QString();
    return QKeySequence(key).toString(QKeySequence::PortableText);
}

// Convert a captured key chord into Sonic Pi shortcut notation that
// resolveShortcut() maps back to the identical QKeySequence. Each leading
// prefix (Meta/Ctrl/ShiftMeta/CtrlMeta/CtrlShift) consumes its modifier; the
// remainder is parsed by QKeySequence, so it must only carry Qt-native tokens.
static QString chordToSonicPiNotation(int key, Qt::KeyboardModifiers mods) {
    QString k = shortcutKeyName(key);
    if (k.isEmpty()) return QString();

#ifdef Q_OS_MAC
    const bool cmd   = mods & Qt::ControlModifier; // Cmd  -> "Meta"
    const bool ctrl  = mods & Qt::MetaModifier;    // ctrl -> "Ctrl"
    const bool alt   = mods & Qt::AltModifier;     // Option
    const bool shift = mods & Qt::ShiftModifier;

    QString prefix;
    bool shiftConsumed = false;
    if (cmd && ctrl)        prefix = "CtrlMeta";
    else if (cmd && shift)  { prefix = "ShiftMeta"; shiftConsumed = true; }
    else if (cmd)           prefix = "Meta";
    else if (ctrl && shift) { prefix = "CtrlShift"; shiftConsumed = true; }
    else if (ctrl)          prefix = "Ctrl";

    QStringList rest;
    if (alt) rest << "Alt";
    if (shift && !shiftConsumed) rest << "Shift";
    rest << k;
    QString tail = rest.join("+");
    return prefix.isEmpty() ? tail : prefix + "+" + tail;
#else
    // Non-mac: "Meta" maps to Alt, "Ctrl" to Ctrl (see metaKey()/ctrlKey()).
    const bool ctrl    = mods & Qt::ControlModifier;
    const bool metaTok = mods & Qt::AltModifier; // Alt -> "Meta"
    const bool shift   = mods & Qt::ShiftModifier;

    QString prefix;
    bool shiftConsumed = false;
    if (ctrl && metaTok)        prefix = "CtrlMeta";
    else if (metaTok && shift)  { prefix = "ShiftMeta"; shiftConsumed = true; }
    else if (metaTok)           prefix = "Meta";
    else if (ctrl && shift)     { prefix = "CtrlShift"; shiftConsumed = true; }
    else if (ctrl)              prefix = "Ctrl";

    QStringList rest;
    if (shift && !shiftConsumed) rest << "Shift";
    rest << k;
    QString tail = rest.join("+");
    return prefix.isEmpty() ? tail : prefix + "+" + tail;
#endif
}

// In-place editor that captures a pressed key chord rather than typed text.
class ShortcutRecorder : public QLineEdit {
public:
    explicit ShortcutRecorder(QWidget* parent = nullptr) : QLineEdit(parent) {
        setReadOnly(true);
        setAlignment(Qt::AlignCenter);
        setPlaceholderText(QObject::tr("Type shortcut…"));
        // Prominent "listening" look so it's obvious the cell is recording.
        setStyleSheet(
            "QLineEdit { background: palette(highlight); color: palette(highlighted-text);"
            " font-weight: bold; border: 2px solid palette(highlight); }");
    }
    bool captured() const { return m_captured; }
protected:
    // While recording, claim every chord so application/editor QAction
    // shortcuts don't swallow it before keyPressEvent() runs.
    bool event(QEvent* e) override {
        if (e->type() == QEvent::ShortcutOverride) {
            e->accept();
            return true;
        }
        return QLineEdit::event(e);
    }
    void keyPressEvent(QKeyEvent* e) override {
        switch (e->key()) {
        case Qt::Key_Shift: case Qt::Key_Control: case Qt::Key_Alt:
        case Qt::Key_Meta: case Qt::Key_AltGr: case Qt::Key_CapsLock:
        case Qt::Key_unknown:
            e->ignore();
            return;
        case Qt::Key_Escape:
            QLineEdit::keyPressEvent(e); // let the view cancel the edit
            return;
        case Qt::Key_Backspace: case Qt::Key_Delete:
            m_captured = true;
            setText(QString()); // clear the binding
            emit editingFinished();
            e->accept();
            return;
        default:
            break;
        }
        QString notation = chordToSonicPiNotation(e->key(), e->modifiers());
        if (!notation.isEmpty()) {
            m_captured = true;
            setText(notation);
            emit editingFinished();
        }
        e->accept();
    }
private:
    bool m_captured = false;
};

// Delegate: only the Shortcut column of a command row is editable, via the
// key recorder.
class ShortcutKeyDelegate : public QStyledItemDelegate {
public:
    using QStyledItemDelegate::QStyledItemDelegate;
    QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem&,
                          const QModelIndex& index) const override {
        if (index.column() != 1 || !index.parent().isValid()) return nullptr;
        ShortcutRecorder* rec = new ShortcutRecorder(parent);
        ShortcutKeyDelegate* self = const_cast<ShortcutKeyDelegate*>(this);
        // Commit exactly once, and only if a chord was actually recorded.
        // editingFinished can re-fire on focus-out (e.g. when the conflict
        // dialog opens); committing twice on the same live editor crashes.
        auto done = std::make_shared<bool>(false);
        QObject::connect(rec, &QLineEdit::editingFinished, self, [self, rec, done]() {
            if (*done) return;
            *done = true;
            if (rec->captured()) emit self->commitData(rec);
            emit self->closeEditor(rec);
        });
        return rec;
    }
    void setEditorData(QWidget* editor, const QModelIndex&) const override {
        // Start empty so the "Type shortcut…" prompt shows it is listening.
        static_cast<ShortcutRecorder*>(editor)->setText(QString());
    }
    void setModelData(QWidget* editor, QAbstractItemModel* model,
                      const QModelIndex& index) const override {
        model->setData(index, static_cast<ShortcutRecorder*>(editor)->text(), Qt::EditRole);
    }
    // Render the stored "Meta+R" notation as the platform-native chord (⌘R),
    // matching the menu bar. The underlying value stays in Sonic Pi notation.
    QString displayText(const QVariant& value, const QLocale&) const override {
        const QString s = value.toString().trimmed();
        if (s.isEmpty()) return s;
        const QString native = MainWindow::resolveShortcut(s).toString(QKeySequence::NativeText);
        return native.isEmpty() ? s : native;
    }
};

QGroupBox* SettingsWidget::createKeyboardShortcutsTab() {
    QGroupBox *shortcuts_box = new QGroupBox();

    // Segmented "pill" toggle: a subtle track with the active mode filled in
    // the accent colour. More visible than a dropdown, same row height.
    auto makeSeg = [](const QString& text) {
        QPushButton* b = new QPushButton(text);
        b->setCheckable(true);
        b->setCursor(Qt::PointingHandCursor);
        return b;
    };
    QPushButton* macBtn = makeSeg(tr("Mac"));
    QPushButton* winBtn = makeSeg(tr("Windows | Linux"));
    QPushButton* emacsBtn = makeSeg(tr("Emacs Live"));
    QPushButton* customBtn = makeSeg(tr("Custom"));
    shortcutSchemeGroup = new QButtonGroup(this);
    shortcutSchemeGroup->addButton(macBtn, 3);
    shortcutSchemeGroup->addButton(winBtn, 2);
    shortcutSchemeGroup->addButton(emacsBtn, 1);
    shortcutSchemeGroup->addButton(customBtn, 4);

    QWidget* segControl = new QWidget();
    segControl->setObjectName("segControl");
    // Styled by the shared "segmented control" rule in app.qss (house metrics).
    segControl->setProperty("segmented", true);
    QHBoxLayout* segLayout = new QHBoxLayout(segControl);
    segLayout->setContentsMargins(3, 3, 3, 3);
    segLayout->setSpacing(3);
    segLayout->addWidget(macBtn);
    segLayout->addWidget(winBtn);
    segLayout->addWidget(emacsBtn);
    segLayout->addWidget(customBtn);

    QHBoxLayout *schemeRow = new QHBoxLayout;
    schemeRow->addStretch();
    schemeRow->addWidget(new QLabel(tr("Mode:")));
    schemeRow->addWidget(segControl);
    schemeRow->addStretch();

    // Custom-only controls: base preset, manage buttons, and the edit hint.
    shortcutCustomControls = new QWidget();
    shortcutBaseCombo = new QComboBox();
    shortcutBaseCombo->addItem(tr("Mac"), "mac");
    shortcutBaseCombo->addItem(tr("Windows | Linux"), "win");
    shortcutBaseCombo->addItem(tr("Emacs Live"), "emacs");
    shortcutEditRowButton = new QPushButton(tr("Edit Shortcut"));
    shortcutEditRowButton->setEnabled(false);
    QPushButton *resetButton = new QPushButton(tr("Reset"));
    resetButton->setStyleSheet(
        "QPushButton { background: palette(highlight); color: palette(highlighted-text); }");
    QPushButton *importButton = new QPushButton(tr("Import…"));
    QPushButton *exportButton = new QPushButton(tr("Export…"));

    shortcutModifiedLabel = new QLabel();
    shortcutModifiedLabel->setStyleSheet("QLabel { font-style: italic; }");

    QHBoxLayout *ccTop = new QHBoxLayout;
    ccTop->setContentsMargins(0, 0, 0, 0);
    ccTop->addWidget(new QLabel(tr("Base:")));
    ccTop->addWidget(shortcutBaseCombo);
    ccTop->addWidget(shortcutEditRowButton);
    ccTop->addStretch();
    ccTop->addWidget(shortcutModifiedLabel);
    ccTop->addWidget(importButton);
    ccTop->addWidget(exportButton);
    ccTop->addWidget(resetButton);

    shortcutCustomControls->setLayout(ccTop);

    shortcutTree = new QTreeWidget();
    shortcutTree->setColumnCount(2);
    shortcutTree->setHeaderHidden(true);
    shortcutTree->setSelectionMode(QAbstractItemView::SingleSelection);
    shortcutTree->setAlternatingRowColors(true);
    shortcutTree->setStyleSheet(
        "QTreeView { alternate-background-color: rgba(127,127,127,26); }"
        "QTreeView::item { padding-top: 4px; padding-bottom: 4px; }");
    // Delegate only on the Shortcut column: it both records new chords and
    // renders the binding natively (⌘R) instead of the raw "Meta+R" notation.
    shortcutTree->setItemDelegateForColumn(1, new ShortcutKeyDelegate(shortcutTree));
    shortcutTree->setEditTriggers(QAbstractItemView::DoubleClicked
        | QAbstractItemView::SelectedClicked | QAbstractItemView::EditKeyPressed);
    shortcutTree->header()->setSectionResizeMode(0, QHeaderView::Stretch);
    shortcutTree->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);

    QVBoxLayout *layout = new QVBoxLayout;
    layout->addLayout(schemeRow);
    layout->addWidget(shortcutTree, 1);
    layout->addWidget(shortcutCustomControls);
    shortcuts_box->setLayout(layout);

    int mode = piSettings->shortcut_mode;
    if (QAbstractButton* active = shortcutSchemeGroup->button(mode)) active->setChecked(true);
    shortcutCustomControls->setVisible(mode == 4);
    reloadShortcutTree();

    connect(shortcutSchemeGroup, &QButtonGroup::idToggled, this,
        [this](int, bool checked) { if (checked) onShortcutSchemeToggled(); });
    connect(shortcutBaseCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
        [this](int) {
            QString newBase = shortcutBaseCombo->currentData().toString();
            // Keep the user's edits (diffs from the old base) as a layer over the new base.
            QMap<QString, QString> overrides = collectDiffsAgainst(shortcutEditBase);
            fillShortcutTree(shortcutTree, newBase, overrides, true);
            shortcutEditBase = newBase;
            applyShortcuts();
        });
    connect(shortcutTree, &QTreeWidget::itemChanged, this, &SettingsWidget::onShortcutItemChanged);
    // Enable Edit only when a command row (not a group header) is selected.
    connect(shortcutTree, &QTreeWidget::itemSelectionChanged, this, [this]() {
        QTreeWidgetItem* it = shortcutTree->currentItem();
        shortcutEditRowButton->setEnabled(it && it->parent() != nullptr
            && (it->flags() & Qt::ItemIsEditable));
    });
    // Edit / double-click both start recording the selected row's shortcut.
    connect(shortcutEditRowButton, &QPushButton::clicked, this, [this]() {
        QTreeWidgetItem* it = shortcutTree->currentItem();
        if (it && it->parent()) shortcutTree->editItem(it, 1);
    });
    connect(resetButton, &QPushButton::clicked, this, [this]() { resetShortcutsToBase(); });
    connect(importButton, &QPushButton::clicked, this, [this]() { importShortcuts(); });
    connect(exportButton, &QPushButton::clicked, this, [this]() { exportShortcuts(); });

    return shortcuts_box;
}

// Resolve the base preset + user overrides for the currently-active scheme.
void SettingsWidget::currentBaseAndOverrides(QString& base, QMap<QString, QString>& overrides) const {
    int mode = piSettings->shortcut_mode;
    base = (mode == 2) ? "win" : (mode == 1) ? "emacs" : "mac";
    overrides.clear();
    if (mode == 4 && !shortcutConfigPath.isEmpty() && QFile::exists(shortcutConfigPath)) {
        readShortcutIni(shortcutConfigPath, base, overrides);
    }
}

// Fill a tree grouped by menu category. editable=true marks command rows
// editable (the delegate confines editing to the Shortcut column).
void SettingsWidget::fillShortcutTree(QTreeWidget* tree, const QString& base,
                                      const QMap<QString, QString>& overrides, bool editable) {
    QSignalBlocker blocker(tree);
    tree->clear();
    QMap<QString, QTreeWidgetItem*> groups;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        QString grp(d.group);
        QTreeWidgetItem* parent = groups.value(grp, nullptr);
        if (!parent) {
            parent = new QTreeWidgetItem(tree, QStringList{ grp });
            parent->setFlags(Qt::ItemIsEnabled);
            parent->setFirstColumnSpanned(true);
            QFont gf = parent->font(0);
            gf.setBold(true);
            SetFontSizeValue(gf, FontSizeValue(gf) * 1.1);
            parent->setFont(0, gf);
            parent->setBackground(0, QColor(127, 127, 127, 120));
            parent->setExpanded(true);
            groups.insert(grp, parent);
        }
        QString baseKey = baseKeyFor(d, base);
        QString key = overrides.value(d.id, baseKey);
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0, QCoreApplication::translate("MainWindow", d.desc));
        item->setText(1, key);
        item->setData(0, Qt::UserRole, QString(d.id));
        item->setData(1, Qt::UserRole, key); // last-applied value, for revert-on-cancel
        if (editable) item->setFlags(item->flags() | Qt::ItemIsEditable);
    }
    restyleShortcutTree(tree, base);
}

// Colour + weight every command row: red for a binding shared by 2+ commands
// (a conflict), accent + bold for one changed from the base preset, default
// otherwise. Tooltips explain each.
void SettingsWidget::restyleShortcutTree(QTreeWidget* tree, const QString& base) {
    QMap<QString, QString> baseOf;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) baseOf.insert(d.id, baseKeyFor(d, base));

    QMap<QString, QList<QTreeWidgetItem*>> byVal;
    for (int g = 0; g < tree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* p = tree->topLevelItem(g);
        for (int i = 0; i < p->childCount(); ++i) {
            QString c = canonicalChord(p->child(i)->text(1));
            if (!c.isEmpty()) byVal[c].append(p->child(i));
        }
    }

    int modifiedCount = 0;
    QSignalBlocker b(tree);
    for (int g = 0; g < tree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* p = tree->topLevelItem(g);
        for (int i = 0; i < p->childCount(); ++i) {
            QTreeWidgetItem* it = p->child(i);
            QString id = it->data(0, Qt::UserRole).toString();
            QString v = it->text(1).trimmed();
            const QList<QTreeWidgetItem*>& sharing = byVal.value(canonicalChord(v));
            bool conflict = !v.isEmpty() && sharing.size() > 1;
            bool modified = baseOf.contains(id) && v != baseOf.value(id);
            if (modified) ++modifiedCount;

            QFont f = it->font(1);
            f.setBold(false);
            f.setItalic(modified);
            it->setFont(1, f);

            if (conflict) {
                QStringList others;
                for (QTreeWidgetItem* o : sharing) if (o != it) others << o->text(0);
                it->setForeground(1, QColor(0xE0, 0x52, 0x52));
                it->setToolTip(1, tr("Also assigned to: %1").arg(others.join(", ")));
            } else if (modified) {
                QString def = baseOf.value(id);
                it->setForeground(1, tree->palette().color(QPalette::Highlight));
                it->setToolTip(1, tr("Changed from default (%1)").arg(def.isEmpty() ? tr("unset") : def));
            } else {
                it->setData(1, Qt::ForegroundRole, QVariant());
                it->setToolTip(1, QString());
            }
        }
    }

    if (shortcutModifiedLabel) {
        shortcutModifiedLabel->setText(modifiedCount == 0
            ? tr("(no changes)")
            : tr("(%1 changed)").arg(modifiedCount));
    }
}

// Rebuild the single tree for the active scheme; editable only for Custom.
void SettingsWidget::reloadShortcutTree() {
    if (!shortcutTree) return;
    int mode = piSettings->shortcut_mode;
    bool editable = (mode == 4);
    // Presets are read-only: no selection / focus, so nothing looks interactive.
    shortcutTree->setSelectionMode(editable ? QAbstractItemView::SingleSelection
                                            : QAbstractItemView::NoSelection);
    shortcutTree->setFocusPolicy(editable ? Qt::StrongFocus : Qt::NoFocus);
    if (shortcutEditRowButton) shortcutEditRowButton->setEnabled(false);

    QString base;
    QMap<QString, QString> overrides;
    currentBaseAndOverrides(base, overrides);
    {
        QSignalBlocker b(shortcutBaseCombo);
        int i = shortcutBaseCombo->findData(base);
        if (i >= 0) shortcutBaseCombo->setCurrentIndex(i);
    }
    shortcutEditBase = base;
    fillShortcutTree(shortcutTree, base, overrides, editable);
}

// Gather the command rows whose current binding differs from the given base.
QMap<QString, QString> SettingsWidget::collectDiffsAgainst(const QString& base) const {
    QMap<QString, QString> current;
    for (int g = 0; g < shortcutTree->topLevelItemCount(); ++g) {
        QTreeWidgetItem* parent = shortcutTree->topLevelItem(g);
        for (int i = 0; i < parent->childCount(); ++i) {
            QTreeWidgetItem* it = parent->child(i);
            current.insert(it->data(0, Qt::UserRole).toString(), it->text(1).trimmed());
        }
    }
    QMap<QString, QString> diffs;
    for (const ShortcutDef& d : MainWindow::shortcutDefs()) {
        QString id(d.id);
        if (current.contains(id) && current.value(id) != baseKeyFor(d, base)) {
            diffs.insert(id, current.value(id));
        }
    }
    return diffs;
}

// Gather only the command rows that differ from the chosen base preset.
QMap<QString, QString> SettingsWidget::collectShortcutDiffs(QString* outBase) const {
    QString base = shortcutBaseCombo->currentData().toString();
    if (outBase) *outBase = base;
    return collectDiffsAgainst(base);
}

void SettingsWidget::onShortcutSchemeToggled() {
    int mode = shortcutSchemeGroup->checkedId();
    shortcutCustomControls->setVisible(mode == 4);
    emit shortcutSchemeChanged(mode); // MainWindow sets the mode + reapplies
    reloadShortcutTree();             // reflect the new mode (editable iff Custom)
}

void SettingsWidget::applyShortcuts() {
    QString base;
    QMap<QString, QString> diffs = collectShortcutDiffs(&base);
    emit shortcutsApplyRequested(base, diffs);
}

void SettingsWidget::onShortcutItemChanged(QTreeWidgetItem* item, int column) {
    if (column != 1 || !item->parent()) return;
    if (m_inShortcutChange) return; // editor focus-out can re-fire; ignore re-entry
    QScopedValueRollback<bool> guard(m_inShortcutChange, true);

    QString base = shortcutBaseCombo->currentData().toString();
    QString newVal = item->text(1).trimmed();
    QString prevVal = item->data(1, Qt::UserRole).toString();

    if (!newVal.isEmpty()) {
        QString canon = canonicalChord(newVal);
        QList<QTreeWidgetItem*> clashes;
        for (int g = 0; g < shortcutTree->topLevelItemCount(); ++g) {
            QTreeWidgetItem* p = shortcutTree->topLevelItem(g);
            for (int i = 0; i < p->childCount(); ++i) {
                QTreeWidgetItem* o = p->child(i);
                if (o != item && canonicalChord(o->text(1)) == canon) clashes.append(o);
            }
        }
        if (!clashes.isEmpty()) {
            QStringList names;
            for (QTreeWidgetItem* o : clashes) names << o->text(0);
            QMessageBox box(this);
            box.setIcon(QMessageBox::Warning);
            box.setWindowTitle(tr("Shortcut already in use"));
            box.setText(tr("\"%1\" is already assigned to: %2.").arg(newVal, names.join(", ")));
            box.setInformativeText(tr("What would you like to do?"));
            QPushButton* reassign = box.addButton(tr("Reassign to this"), QMessageBox::AcceptRole);
            box.addButton(tr("Keep both"), QMessageBox::ActionRole);
            QPushButton* cancel = box.addButton(QMessageBox::Cancel);
            box.exec();
            if (box.clickedButton() == cancel) {
                {
                    QSignalBlocker b(shortcutTree);
                    item->setText(1, prevVal);
                }
                restyleShortcutTree(shortcutTree, base);
                return; // nothing applied
            }
            if (box.clickedButton() == reassign) {
                QSignalBlocker b(shortcutTree);
                for (QTreeWidgetItem* o : clashes) {
                    o->setText(1, QString());
                    o->setData(1, Qt::UserRole, QString());
                }
            }
            // "Keep both" leaves the clash; restyle will flag it red.
        }
    }

    item->setData(1, Qt::UserRole, newVal);
    restyleShortcutTree(shortcutTree, base);
    applyShortcuts();
}

void SettingsWidget::resetShortcutsToBase() {
    if (QMessageBox::warning(this, tr("Reset shortcuts?"),
            tr("This discards all your custom changes and restores the base preset. Continue?"),
            QMessageBox::Yes | QMessageBox::No, QMessageBox::No) != QMessageBox::Yes) {
        return;
    }
    fillShortcutTree(shortcutTree, shortcutBaseCombo->currentData().toString(), {}, true);
    applyShortcuts();
}

void SettingsWidget::exportShortcuts() {
    QString path = QFileDialog::getSaveFileName(this, tr("Export Shortcuts"),
        "sonic-pi-shortcuts.ini", tr("Shortcut files (*.ini)"));
    if (path.isEmpty()) return;
    QString base;
    QMap<QString, QString> diffs = collectShortcutDiffs(&base);
    QSettings cfg(path, QSettings::IniFormat);
    cfg.clear();
    cfg.setValue("base", base);
    for (auto it = diffs.constBegin(); it != diffs.constEnd(); ++it) {
        cfg.setValue(it.key(), it.value());
    }
    cfg.sync();
}

void SettingsWidget::importShortcuts() {
    QString path = QFileDialog::getOpenFileName(this, tr("Import Shortcuts"),
        QString(), tr("Shortcut files (*.ini)"));
    if (path.isEmpty()) return;
    QString base = "mac";
    QMap<QString, QString> overrides;
    readShortcutIni(path, base, overrides);
    {
        QSignalBlocker b(shortcutSchemeGroup);
        if (QAbstractButton* customBtn = shortcutSchemeGroup->button(4)) customBtn->setChecked(true);
    }
    shortcutCustomControls->setVisible(true);
    {
        QSignalBlocker b(shortcutBaseCombo);
        int baseIdx = shortcutBaseCombo->findData(base);
        if (baseIdx >= 0) shortcutBaseCombo->setCurrentIndex(baseIdx);
    }
    fillShortcutTree(shortcutTree, base, overrides, true);
    shortcutEditBase = base;
    emit shortcutSchemeChanged(4);
    applyShortcuts();
}


// Display string for the tooltip popup's key-cap chip (set as the
// "tipShortcut" widget property — see sonicpitooltip.h).
QString SettingsWidget::shortcutStrShiftMeta(char key) {
#ifdef Q_OS_MAC
    return QString("⇧⌘%1").arg(key);
#else
    return QString("Shift+Alt+%1").arg(key);
#endif
}

void SettingsWidget::updateScopeNames( std::vector<QString> names ) {
    piSettings->scope_names = names;
    // Per-kind descriptions for the tooltip popup; keep in sync with the
    // scope kinds published by the scope window.
    QMap<QString, QString> scopeDescriptions;
    scopeDescriptions["Lissajous"]     = tr("Illustrates the phase relationship between the left and right channels.");
    scopeDescriptions["Mirror Stereo"] = tr("A simple left/right composite wave, with left on top, right on bottom.");
    scopeDescriptions["Mono"]          = tr("A combined view of the left and right channels (using RMS).");
    scopeDescriptions["Spectrum"]      = tr("The sound frequencies as a spectrum, from low to high.");
    scopeDescriptions["Stereo"]        = tr("Two independent scopes for the left and right channels.");
    // A glyph per scope kind, so the list can be read by shape. Keyed off the
    // same names the scope window publishes, so a kind without an entry simply
    // gets no icon rather than the wrong one.
    QMap<QString, const char*> scopeIcons;
    scopeIcons["Levels"]        = kScopeLevelsSvg;
    scopeIcons["Lissajous"]     = kScopeLissajousSvg;
    scopeIcons["Mirror Stereo"] = kScopeMirrorSvg;
    scopeIcons["Mono"]          = kScopeMonoSvg;
    scopeIcons["Spectrum"]      = kScopeSpectrumSvg;
    scopeIcons["Stereo"]        = kScopeStereoSvg;

    for( auto name : names ) {
        QCheckBox* cb = new QCheckBox( name );
        cb->setChecked( piSettings->isScopeActive(name));
        if (const char* svg = scopeIcons.value(name, nullptr))
            setCheckIcon(cb, svg);
        cb->setToolTip(scopeDescriptions.value(name,
            tr("Toggle the visibility of the %1 oscilloscope.").arg(name)));
        scope_box_kinds_layout->addWidget(cb);
        connect(cb, &QCheckBox::clicked, this, [=]() {
          toggleScope(cb);
        });

    }
}

void SettingsWidget::updateScopeKindVisibility() {
  for (int i = 0; i < scope_box_kinds_layout->count(); ++i) {
    QCheckBox *cb = qobject_cast<QCheckBox*>(scope_box_kinds_layout->itemAt(i)->widget());
    cb->setChecked(piSettings->isScopeActive(cb->text()));
  }
}

void SettingsWidget::updateSelectedUILanguage(QString lang) {
  int index = available_languages.indexOf(lang);
  language_combo->setCurrentIndex(index);
}

void SettingsWidget::toggleScope( QObject* qo ) {
  auto qw = (QWidget*) qo;
  QCheckBox* cb = static_cast<QCheckBox*>(qw);
  //QSettings settings(QSettings::IniFormat, QSettings::UserScope,    "sonic-pi.net", "gui-settings");
  //piSettings->setValue("prefs/scope/show-"+cb->text().toLower(), cb->isChecked() );
  QString name = cb->text();
  piSettings->setScopeState( name, cb->isChecked() );
  emit scopeChanged(name);
}



// TODO: Implement real-time language switching
void SettingsWidget::updateUILanguage(int index) {
    QString lang = available_languages[index];
    std::cout << "Changed language to " << lang.toUtf8().constData() << std::endl;
    if (lang != piSettings->language) {
        std::cout << "Current language:  " << piSettings->language.toUtf8().constData() << std::endl;
        std::cout << "New language selected: " << lang.toUtf8().constData() << std::endl;
        QString old_lang = sonicPii18n->getNativeLanguageName(piSettings->language);
        QString new_lang = sonicPii18n->getNativeLanguageName(lang);

        // Show confirmation box
        QMessageBox msgBox(this);
        msgBox.setText(QString(tr("You've selected a new language: %1")).arg(new_lang));
        QString info_text = (
          tr("Do you want to apply this language?")
          + "\n"
          + tr("The new language will be applied when you next start Sonic Pi.")
        );

        if (lang == "system_language") {
            // Determine the actual language to load
            QString actual_lang = sonicPii18n->determineUILanguage(lang);
            info_text = tr("System language found: %1").arg(sonicPii18n->getNativeLanguageName(actual_lang)) + "\n" + info_text;
        }

        msgBox.setInformativeText(info_text);
        QPushButton *applyButton = msgBox.addButton(tr("Apply"), QMessageBox::ActionRole);
        QPushButton *dismissButton = msgBox.addButton(tr("Cancel"), QMessageBox::RejectRole);
        msgBox.setDefaultButton(applyButton);
        msgBox.setIcon(QMessageBox::Question);
        msgBox.exec();

        if (msgBox.clickedButton() == (QAbstractButton*)applyButton) {
          piSettings->language = lang;
          updateSelectedUILanguage(piSettings->language);
          emit uiLanguageChanged(piSettings->language);

          language_details_label->setText(
            tr("<b>The new language will be applied when you next start Sonic Pi.</b><br>")
            + tr("Current UI language: %1\n").arg(sonicPii18n->getNativeLanguageName(sonicPii18n->currentlyLoadedLanguage()))
          );

          QMessageBox restartMsgBox(this);
          restartMsgBox.setText(QString(tr("Restart Sonic Pi?")));
          QString info_text = (tr("Do you want to restart Sonic Pi now? This will stop any current runs & recordings."));
          QPushButton *restartButton = restartMsgBox.addButton(tr("Restart"), QMessageBox::ActionRole);
          QPushButton *dismissButton = restartMsgBox.addButton(tr("Dismiss"), QMessageBox::RejectRole);
          restartMsgBox.setInformativeText(info_text);
          restartMsgBox.setDefaultButton(dismissButton);
          restartMsgBox.setIcon(QMessageBox::Question);
          restartMsgBox.exec();
          if (restartMsgBox.clickedButton() == (QAbstractButton*)restartButton) {
            emit restartApp();
          }
            //emit uiLanguageChanged(lang);
        } else if (msgBox.clickedButton() == (QAbstractButton*)dismissButton) {
            // Don't apply the new language settings
            updateSelectedUILanguage(piSettings->language);
            emit uiLanguageChanged(piSettings->language);
        }

    }
}

void SettingsWidget::updateEnableScsynthInputs() {
    bool inputsEnabled = enable_scsynth_inputs->isChecked();
    if (!inputsEnabled) {
        // Update the UI immediately — SuperSonic's broadcast can lag
        QSignalBlocker blocker(audio_input_combo);
        audio_input_combo->clear();
        audio_input_combo->addItem(tr("-- DISABLED --"), QString("__disabled__"));
        audio_input_combo->setCurrentIndex(0);
        audio_input_combo->setEnabled(false);
    } else {
        // Next /clockwork/input-devices broadcast will populate
        audio_input_combo->setEnabled(true);
    }
    emit enableScsynthInputsChanged();
    updateMicPermissionStatus(); // show/hide the mic notice to match the toggle
}

void SettingsWidget::update_mixer_invert_stereo() {
    emit mixerSettingsChanged();
}

void SettingsWidget::update_mixer_force_mono() {
    emit mixerSettingsChanged();
}

void SettingsWidget::toggleOscServer() {
    emit oscSettingsChanged();
}

void SettingsWidget::toggleMidi() {
    emit midiSettingsChanged();
}

void SettingsWidget::toggleGamepad() {
    emit gamepadSettingsChanged();
}

void SettingsWidget::updateMidiInPorts( QString in ) {
    midi_in_ports_list->setDevices( in );
}

void SettingsWidget::updateMidiOutPorts( QString out ) {
    midi_out_ports_list->setDevices( out );
}

void SettingsWidget::updateGamepadDevices( QString devices ) {
    gamepad_devices_list->setDevices( devices );
}

void SettingsWidget::updateScsynthInfo( QString scsynthInfo ) {
  supersonicBox->setToolTip(scsynthInfo);
  // Disable controls during device changes; the pulse + status line say why.
  // The summary label is left alone — it keeps showing the engine's last
  // known state rather than being hijacked as a busy indicator.
  if (scsynthInfo.contains("Switching audio")) {
    audio_output_combo->setEnabled(false);
    audio_input_combo->setEnabled(false);
    audio_sample_rate_combo->setEnabled(false);
    audio_buffer_size_combo->setEnabled(false);
    audio_driver_combo->setEnabled(false);
    reset_device_button->setEnabled(false);
    beginDeviceSwitchFeedback();
  }
}

// Immediate feedback for a device/driver change: pulse the Audio Device box
// border and explain the wait — cold swaps take a few seconds, and without
// a cue the delay reads as a hang. Runs both when the user picks something
// (dropdown slots) and when the engine announces its own swap (statechange).
void SettingsWidget::setAudioStatus(const QString& text, bool sticky) {
    // A failure notice has to outlive the routine clear that follows every
    // device-config broadcast: the engine reports its new config immediately
    // after a switch, success or not, and that clear would otherwise wipe the
    // explanation before it could be read. Sticky text is only replaced by
    // the next message with something to say — in practice the next attempt,
    // which starts with "Changing audio device...".
    if (text.isEmpty() && m_audioStatusSticky) return;
    m_audioStatusSticky = sticky && !text.isEmpty();
    m_audioStatusText = text;
    const int w = audio_status_label->width();
    audio_status_label->setText(
        w > 0 ? audio_status_label->fontMetrics().elidedText(text, Qt::ElideRight, w)
              : text);
    audio_status_label->setToolTip(text);
}

void SettingsWidget::beginDeviceSwitchFeedback() {
    setAudioStatus(tr("Changing audio device. This can take a few moments..."));
    m_devicePulse->start();
    m_switchTimeoutTimer->start(15000);
}

void SettingsWidget::deviceReopenRejected(const QString& reason) {
    std::cout << "[gui-audio] device reopen rejected: "
              << reason.toUtf8().constData() << std::endl;
    // Only our own reopen may cancel the feedback: the reply carries no
    // request id, so a rejection arriving while an unrelated switch is
    // running must not stop that switch's pulse or safety timer.
    if (!m_reopenPending) return;
    m_reopenPending = false;
    m_switchTimeoutTimer->stop();
    m_devicePulse->stop();
    reset_device_button->setEnabled(true);
    // The engine only refuses while a swap is in flight or settling
    // (3s cooldown) — not an error, just "not yet".
    setAudioStatus(tr("Audio device is still settling. Try again in a few seconds."));
}

// The Windows Audio mode variants (shared / exclusive / low-latency) expose
// the same endpoint names, so they count as one family when scoping a device
// list to the selected driver.
static QString driverFamily(const QString& t)
{
    return t.startsWith("Windows Audio") ? QString("Windows Audio") : t;
}

// Item role carrying a device row's capability flags (comma-separated
// tokens from the engine's device table, e.g. "follows-default").
static constexpr int kDeviceFlagsRole = Qt::UserRole + 1;

// True when the combo row at idx carries the given capability token.
static bool rowHasFlag(const QComboBox* combo, int idx, const QString& flag)
{
    return combo->itemData(idx, kDeviceFlagsRole).toString()
        .split(',').contains(flag);
}

// Locate the selected driver's group in the per-driver device table.
// Returns nullptr when the table is absent (engine predates it) or the
// driver has no entry — callers then fall back to filtering the flat list.
static const SonicPi::AudioDeviceTableInfo::DriverDevices*
tableGroupFor(const SonicPi::AudioDeviceTableInfo& table, bool hasTable,
              const QString& selectedDriver)
{
    if (!hasTable || selectedDriver.isEmpty())
        return nullptr;
    for (const auto& g : table.drivers)
        if (selectedDriver == QString::fromStdString(g.driver))
            return &g;
    return nullptr;
}

void SettingsWidget::updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo) {
    // Skip rebuild if nothing changed — /clockwork/devices fires several
    // times per boot and rebuilding invalidates the dropdown cache. The
    // deviceTypes compare matters: the driver filter below renders from
    // types, and a driver switch can re-type devices without renaming them.
    if (devicesInfo.devices == m_lastAudioDevicesInfo.devices &&
        devicesInfo.deviceTypes == m_lastAudioDevicesInfo.deviceTypes &&
        devicesInfo.currentDevice == m_lastAudioDevicesInfo.currentDevice &&
        devicesInfo.mode == m_lastAudioDevicesInfo.mode &&
        devicesInfo.sampleRate == m_lastAudioDevicesInfo.sampleRate) {
        return;
    }
    m_lastAudioDevicesInfo = devicesInfo;

    std::cout << "[gui-audio] updateAudioDevices: mode='" << devicesInfo.mode
              << "' currentDevice='" << devicesInfo.currentDevice
              << "' numDevices=" << devicesInfo.devices.size() << std::endl;
    QSignalBlocker blocker(audio_output_combo);

    audio_output_combo->clear();

    // The device table is the single source of truth when the engine
    // broadcasts capability flags: every row the user can pick comes from
    // the engine (including each driver's default-follow entry, synthetic
    // or native), rendered verbatim with semantics read from the flags —
    // the GUI invents nothing and sends literal device names back.
    // Pre-flags engines fall back to the legacy synthesized "OS Default"
    // row with its "__system__" sentinel.
    // ASIO has no OS-level "default device" concept — each ASIO driver IS
    // its single device. Substitute "-- None --" (sentinel `__none__`,
    // NO OSC fired when picked) so the user explicitly chooses an ASIO
    // device.
    QString selectedDriver = audio_driver_combo->currentText();
    bool isAsio = (selectedDriver == "ASIO");
    bool inSystemMode = (devicesInfo.mode.empty() || devicesInfo.mode == "system");
    const auto* tableGroup =
        tableGroupFor(m_audioDeviceTable, m_hasAudioDeviceTable, selectedDriver);
    const bool tableHasFlags = tableGroup != nullptr
        && tableGroup->outputFlags.size() == tableGroup->outputs.size();
    if (isAsio) {
        audio_output_combo->addItem(tr("-- None --"), QString("__none__"));
    } else if (!tableHasFlags) {
        QString systemDefaultLabel = tr("OS Default");
        if (inSystemMode && !devicesInfo.currentDevice.empty()) {
            systemDefaultLabel = tr("OS Default (%1)")
                .arg(QString::fromStdString(devicesInfo.currentDevice));
        }
        audio_output_combo->addItem(systemDefaultLabel, QString("__system__"));
    }

    // Prefer the per-driver table when the engine broadcasts one: it lists
    // each driver's devices directly (un-deduped), so no client-side type
    // inference is needed. Otherwise scope the flat list by its per-device
    // types — same rule as the input combo below; keeps each driver's
    // dropdown clean (e.g. Linux: PipeWire's friendly names never mix with
    // the ALSA compat-layer entries). ASIO stays a coarse yes/no bucket
    // (an ASIO driver is its own single device).
    if (tableGroup) {
        for (size_t i = 0; i < tableGroup->outputs.size(); ++i) {
            const QString name = QString::fromStdString(tableGroup->outputs[i]);
            const QString flags = tableHasFlags
                ? QString::fromStdString(tableGroup->outputFlags[i]) : QString();
            QString label = name;
            // The synthetic default-follow row's name is a wire sentinel
            // ("System Default"); display the translatable label the
            // legacy path always used. itemData still carries the wire
            // name.
            if (flags.contains(QStringLiteral("synthetic")))
                label = tr("OS Default");
            // Default-follow rows show what the default currently
            // resolves to when that is a different device (synthetic
            // rows; the PipeWire native entry resolves to itself).
            if (flags.contains(QStringLiteral("follows-default"))
                && inSystemMode && !devicesInfo.currentDevice.empty()
                && devicesInfo.currentDevice != tableGroup->outputs[i]) {
                label = tr("%1 (%2)").arg(label,
                    QString::fromStdString(devicesInfo.currentDevice));
            }
            // itemData carries the literal device name so the emit path
            // sends exactly what the engine published, whatever the label.
            audio_output_combo->addItem(label, name);
            audio_output_combo->setItemData(audio_output_combo->count() - 1,
                                            flags, kDeviceFlagsRole);
        }
    } else {
        bool haveTypes = devicesInfo.deviceTypes.size() == devicesInfo.devices.size();
        for (size_t i = 0; i < devicesInfo.devices.size(); ++i) {
            const auto& dev = devicesInfo.devices[i];
            if (haveTypes && !selectedDriver.isEmpty()) {
                QString qt = QString::fromStdString(devicesInfo.deviceTypes[i]);
                if (isAsio) {
                    if (qt != "ASIO") continue;
                } else if (qt == "ASIO") {
                    continue;
                } else if (driverFamily(qt) != driverFamily(selectedDriver)) {
                    continue;
                }
            }
            audio_output_combo->addItem(QString::fromStdString(dev));
        }
    }

    // Selection priority:
    //   non-ASIO + mode==system → OS Default sentinel
    //   non-ASIO                → concrete device name
    //   ASIO, engine on ASIO    → concrete device name
    //   ASIO, engine not on ASIO yet → leave on "-- None --"
    //     (currentDevice reflects whichever non-ASIO driver JUCE is
    //      still on, displaying it as the ASIO output would be wrong)
    bool selectedBySystem = false;
    bool engineIsOnAsio   = (m_engineActualDriver == "ASIO");
    if (!isAsio && inSystemMode) {
        // Legacy synthesized row first; otherwise the table's own
        // default-follow entry (flagged by the engine).
        int idx = audio_output_combo->findData(QString("__system__"));
        if (idx < 0) {
            for (int i = 0; i < audio_output_combo->count(); ++i) {
                if (rowHasFlag(audio_output_combo, i, QStringLiteral("follows-default"))) {
                    idx = i;
                    break;
                }
            }
        }
        if (idx >= 0) {
            audio_output_combo->setCurrentIndex(idx);
            selectedBySystem = true;
        }
    }
    // With the per-driver table in play, a combo showing a driver the
    // engine is NOT on represents a pending choice — selecting the engine's
    // current device there would misrepresent state (the same endpoint
    // name can exist under several drivers on Windows). Stay on the
    // placeholder instead.
    bool viewingActiveDriver = tableGroup == nullptr
                            || m_engineActualDriver.isEmpty()
                            || selectedDriver == m_engineActualDriver;
    bool selectByCurrent = !selectedBySystem
                        && !devicesInfo.currentDevice.empty()
                        && viewingActiveDriver
                        && (!isAsio || engineIsOnAsio);
    if (selectByCurrent) {
        // Table rows carry the literal device name as itemData (labels may
        // be decorated); legacy rows carry none and match by text.
        int idx = audio_output_combo->findData(
            QString::fromStdString(devicesInfo.currentDevice));
        if (idx < 0)
            idx = audio_output_combo->findText(
                QString::fromStdString(devicesInfo.currentDevice));
        if (idx >= 0) {
            audio_output_combo->setCurrentIndex(idx);
        }
    }
}

void SettingsWidget::updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo) {
    // No change-detection guard — the enable checkbox needs repopulation
    // even when the device list is unchanged
    m_lastAudioInputDevicesInfo = devicesInfo;
    QSignalBlocker blocker(audio_input_combo);

    QString previousSelection = audio_input_combo->currentText();
    bool inputsEnabled = enable_scsynth_inputs->isChecked();

    audio_input_combo->clear();

    if (!inputsEnabled) {
        audio_input_combo->addItem(tr("-- DISABLED --"), QString("__disabled__"));
        audio_input_combo->setCurrentIndex(0);
        audio_input_combo->setEnabled(false);
        return;
    }

    // "-- None --" = no specific input (keep SuperSonic's current); not the
    // same as DISABLED which the checkbox owns
    audio_input_combo->setEnabled(true);
    audio_input_combo->addItem(tr("-- None --"));
    QString selDriver = audio_driver_combo->currentText();
    bool isAsioDr = (selDriver == "ASIO");
    bool haveTypesIn = devicesInfo.deviceTypes.size() == devicesInfo.devices.size();
    // The engine resolves a swap's input name strictly within the active
    // driver, so an input typed under a different driver would be refused
    // ("unknown input device") — don't offer it. The per-driver table gives
    // that scoping directly when present; otherwise filter the flat list.
    // ASIO stays a coarse yes/no bucket: an ASIO device is its own driver
    // and we can't probe its inputs from here.
    const auto* tableGroupIn =
        tableGroupFor(m_audioDeviceTable, m_hasAudioDeviceTable, selDriver);
    if (tableGroupIn) {
        for (const auto& dev : tableGroupIn->inputs)
            audio_input_combo->addItem(QString::fromStdString(dev));
    } else {
        for (size_t i = 0; i < devicesInfo.devices.size(); ++i) {
            const auto& dev = devicesInfo.devices[i];
            if (haveTypesIn && !selDriver.isEmpty()) {
                QString qt = QString::fromStdString(devicesInfo.deviceTypes[i]);
                if (isAsioDr) {
                    if (qt != "ASIO") continue;
                } else if (qt == "ASIO") {
                    continue;
                } else if (driverFamily(qt) != driverFamily(selDriver)) {
                    continue;
                }
            }
            audio_input_combo->addItem(QString::fromStdString(dev));
        }
    }

    // Same pending-choice rule as the output combo: only mirror the
    // engine's current input while viewing the driver it is actually on.
    bool viewingActiveDriver = tableGroupIn == nullptr
                            || m_engineActualDriver.isEmpty()
                            || selDriver == m_engineActualDriver;
    if (!devicesInfo.currentDevice.empty() && viewingActiveDriver) {
        int idx = audio_input_combo->findText(QString::fromStdString(devicesInfo.currentDevice));
        audio_input_combo->setCurrentIndex(idx >= 0 ? idx : 0);
    } else {
        // No active input (or viewing another driver) — show None rather
        // than a stale selection
        audio_input_combo->setCurrentIndex(0);
    }
    (void)previousSelection;
}

void SettingsWidget::updateAudioDeviceTable(const SonicPi::AudioDeviceTableInfo& table) {
    m_audioDeviceTable = table;
    m_hasAudioDeviceTable = true;
    // Relay order isn't guaranteed, so re-render both combos from the
    // cached flat snapshots now that grouped data exists. Clear the output
    // guard first — the flat payload can be unchanged while the grouping
    // isn't.
    auto lastOutputs = m_lastAudioDevicesInfo;
    m_lastAudioDevicesInfo = {};
    updateAudioDevices(lastOutputs);
    updateAudioInputDevices(m_lastAudioInputDevicesInfo);
}

void SettingsWidget::updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo) {
    QSignalBlocker srBlocker(audio_sample_rate_combo);
    QSignalBlocker bsBlocker(audio_buffer_size_combo);
    QSignalBlocker drBlocker(audio_driver_combo);

    audio_sample_rate_combo->clear();
    for (int rate : configInfo.availableSampleRates) {
        audio_sample_rate_combo->addItem(QString::number(rate), rate);
    }
    if (configInfo.sampleRate > 0) {
        int idx = audio_sample_rate_combo->findData(configInfo.sampleRate);
        if (idx >= 0) {
            audio_sample_rate_combo->setCurrentIndex(idx);
        }
    }

    audio_buffer_size_combo->clear();
    for (int bs : configInfo.availableBufferSizes) {
        audio_buffer_size_combo->addItem(QString::number(bs), bs);
    }
    if (configInfo.bufferSize > 0) {
        int idx = audio_buffer_size_combo->findData(configInfo.bufferSize);
        if (idx >= 0) {
            audio_buffer_size_combo->setCurrentIndex(idx);
        }
    }

    // Driver dropdown doubles as pending user intent (Driver=ASIO isn't
    // committed until an Output device is also picked) and a mirror of the
    // engine's live driver. The report's intendedDriver settles the conflict
    // authoritatively: it carries the engine's own pending-pick record, which
    // survives failed swaps and recovery reopens — and when it's explicitly
    // empty, a differing local selection is stale and follows the engine.
    // Reports without the field (older engine) fall back to inference:
    // m_engineActualDriver still holds the driver from the previous report
    // here (refreshed below), which lets choose_driver_selection tell an
    // engine-synced value from a deliberate override.
    QString userSelection = audio_driver_combo->currentText();
    bool wasEmpty = audio_driver_combo->count() == 0;
    audio_driver_combo->clear();
    for (const auto& driver : configInfo.availableDrivers) {
        audio_driver_combo->addItem(QString::fromStdString(driver));
    }
    std::string driverToSelect = sonic_pi::audio::choose_driver_selection(
        wasEmpty,
        userSelection.toStdString(),
        m_engineActualDriver.toStdString(),
        configInfo.availableDrivers,
        configInfo.currentDriver,
        configInfo.hasIntendedDriver,
        configInfo.intendedDriver);
    if (!driverToSelect.empty()) {
        int idx = audio_driver_combo->findText(QString::fromStdString(driverToSelect));
        if (idx >= 0) {
            audio_driver_combo->setCurrentIndex(idx);
        }
    }

    // Cache the engine's actual driver. updateAudioDevices uses it to
    // distinguish the user-picked-ASIO-engine-on-ASIO case (select the
    // concrete device) from user-picked-ASIO-engine-still-elsewhere
    // (default Output to "-- None --" instead of the stale driver's
    // device).
    m_engineActualDriver = QString::fromStdString(configInfo.currentDriver);
    m_engineCurrentSampleRate = configInfo.sampleRate;
    m_engineCurrentBufferSize = configInfo.bufferSize;

    // Update SuperSonic summary with live config (matches SuperSonic's own format)
    QString versionText = QString("%1 Hz | buffer %2 | out %3 | in %4")
        .arg(configInfo.sampleRate)
        .arg(configInfo.bufferSize)
        .arg(configInfo.outputChannels)
        .arg(configInfo.inputChannels);
    if (!configInfo.currentDriver.empty()) {
        versionText += QString(" | %1").arg(QString::fromStdString(configInfo.currentDriver));
    }
    const bool remoteSession = isRemoteDesktopSession();
    if (remoteSession) {
        versionText += QString(" | %1").arg(tr("remote session"));
    }
    remote_session_note->setVisible(remoteSession);
    supersonic_version_label->setText(versionText);

    // Re-enable controls after device switch completes
    m_switchTimeoutTimer->stop();
    m_devicePulse->stop();
    m_reopenPending = false;
    audio_output_combo->setEnabled(true);
    audio_input_combo->setEnabled(true);
    audio_sample_rate_combo->setEnabled(true);
    audio_buffer_size_combo->setEnabled(true);
    audio_driver_combo->setEnabled(true);
    reset_device_button->setEnabled(true);

    // A pending driver pick (driver chosen, device not yet — the engine stays
    // on its old driver meanwhile) is the one settled state where the Driver
    // dropdown legitimately disagrees with the live summary below. Only ASIO
    // reaches it, since every other driver auto-selects a default device, and
    // the ASIO note already says "Select device" — so the status row stays
    // clear rather than carrying a second line saying the same thing.
    setAudioStatus(QString());

    // ASIO-driver constraints may have changed (e.g. driver swapped).
    // Re-apply so the input checkbox + dropdown reflect the new driver.
    applyAsioInputConstraints();

    // Force a re-render of the device dropdowns. activated(int) only
    // fires on user-interaction — programmatic setCurrentIndex above
    // doesn't trigger audioDriverChanged, so without a manual re-run
    // any /clockwork/devices message that arrived before the driver
    // combo was populated would have been processed with an empty
    // selectedDriver and bypassed the driver filter.
    {
        SonicPi::AudioDevicesInfo cached = m_lastAudioDevicesInfo;
        m_lastAudioDevicesInfo = SonicPi::AudioDevicesInfo{};
        if (!cached.devices.empty()) updateAudioDevices(cached);
        if (!m_lastAudioInputDevicesInfo.devices.empty()
            || !m_lastAudioInputDevicesInfo.currentDevice.empty()) {
            SonicPi::AudioInputDevicesInfo cachedIn = m_lastAudioInputDevicesInfo;
            m_lastAudioInputDevicesInfo = SonicPi::AudioInputDevicesInfo{};
            updateAudioInputDevices(cachedIn);
        }
    }
}

void SettingsWidget::audioDriverChanged(int index) {
    if (index < 0) return;
    // Re-render the output and input dropdowns with the new driver's
    // filter. Both populate functions read audio_driver_combo->currentText()
    // when filtering, so just calling them with the cached info applies
    // the new filter. updateAudioDevices has a change-detection guard
    // that short-circuits if the snapshot is identical — clear it first
    // so the re-render actually runs.
    SonicPi::AudioDevicesInfo cached = m_lastAudioDevicesInfo;
    m_lastAudioDevicesInfo = SonicPi::AudioDevicesInfo{};
    if (!cached.devices.empty()) updateAudioDevices(cached);
    if (!m_lastAudioInputDevicesInfo.devices.empty()
        || !m_lastAudioInputDevicesInfo.currentDevice.empty()) {
        updateAudioInputDevices(m_lastAudioInputDevicesInfo);
    }
    applyAsioInputConstraints();
    beginDeviceSwitchFeedback();
    emit driverChanged(audio_driver_combo->currentText());
}

void SettingsWidget::applyAsioInputConstraints() {
    QString driver = audio_driver_combo->currentText();
    bool isAsio = (driver == "ASIO");

    if (isAsio && !asio_constraint_applied) {
        // Entering ASIO: remember the user's previous checkbox state so
        // we can restore it on exit, then force-tick + grey out the
        // checkbox. ASIO is full-duplex by spec; users who want truly
        // input-free output need to switch to Windows Audio / DirectSound.
        asio_saved_input_checked = enable_scsynth_inputs->isChecked();
        asio_saved_input_tooltip = enable_scsynth_inputs->toolTip();
        asio_constraint_applied  = true;
    }

    if (isAsio) {
        QSignalBlocker b(enable_scsynth_inputs);
        enable_scsynth_inputs->setChecked(true);
        enable_scsynth_inputs->setEnabled(false);
        // Style label AND indicator. Sonic Pi's theme keeps the
        // indicator vivid when setEnabled(false), so the label
        // greying alone doesn't read as non-interactive.
        // Size-neutral by construction. Styling ::indicator at all switches
        // that sub-control to stylesheet box-model sizing, which need not
        // match the native indicator — the checkbox then changes size as the
        // constraint comes and goes, sliding every selector in column 0
        // sideways and nudging the rows below. Giving the indicator the
        // style's own metrics (less the border, which the box model adds
        // outside the content box) keeps it exactly the size it was. Colour
        // only, no italic: italic changes the text metrics for the same
        // reason.
        const int indW = enable_scsynth_inputs->style()->pixelMetric(
            QStyle::PM_IndicatorWidth, nullptr, enable_scsynth_inputs);
        const int indH = enable_scsynth_inputs->style()->pixelMetric(
            QStyle::PM_IndicatorHeight, nullptr, enable_scsynth_inputs);
        enable_scsynth_inputs->setStyleSheet(
            QString("QCheckBox { color: gray; }"
                    "QCheckBox::indicator {"
                    " width: %1px; height: %2px;"
                    " background-color: rgba(128, 128, 128, 80);"
                    " border: 1px solid rgba(128, 128, 128, 140);"
                    "}").arg(qMax(1, indW - 2)).arg(qMax(1, indH - 2)));
        enable_scsynth_inputs->setToolTip(
            tr("ASIO devices have linked input/output."));
        asio_input_note->setVisible(true);

        // One ASIO device serves both directions, so there is nothing for a
        // separate Input selector to choose: hide the row outright (its grid
        // row collapses and the note above takes the space) and say so on the
        // remaining selector by naming it Input/Output. A disabled combo
        // mirroring Output would only invite the user to try to change it.
        audio_output_label->setText(tr("Input/Output"));
        audio_input_label->setVisible(false);
        audio_input_combo->setVisible(false);

        // The hidden combo still carries the selection the switch path
        // reads, so keep it mirroring Output — including the "-- None --"
        // case, where no valid ASIO device has been picked yet.
        QString outName     = audio_output_combo->currentText();
        QString outData     = audio_output_combo->currentData().toString();
        bool noOutputPicked = outName.isEmpty()
                              || outData == "__none__"
                              || outData == "__system__"
                              || outName.startsWith(tr("OS Default"));
        QSignalBlocker ib(audio_input_combo);
        if (noOutputPicked) {
            int idx = audio_input_combo->findText(tr("-- None --"));
            if (idx >= 0) audio_input_combo->setCurrentIndex(idx);
        } else {
            int idx = audio_input_combo->findText(outName);
            if (idx >= 0) audio_input_combo->setCurrentIndex(idx);
        }
        audio_input_combo->setEnabled(false);
        audio_input_combo->setToolTip(tr("Mirrors Output (ASIO is full-duplex)."));
    } else if (asio_constraint_applied) {
        // Leaving ASIO: restore prior state.
        QSignalBlocker b(enable_scsynth_inputs);
        enable_scsynth_inputs->setChecked(asio_saved_input_checked);
        enable_scsynth_inputs->setEnabled(true);
        enable_scsynth_inputs->setStyleSheet(QString());
        enable_scsynth_inputs->setToolTip(asio_saved_input_tooltip.isEmpty()
            ? tr("Toggle to enable or disable audio inputs.")
            : asio_saved_input_tooltip);
        asio_input_note->setVisible(false);
        audio_output_label->setText(tr("Output"));
        audio_input_label->setVisible(true);
        audio_input_combo->setVisible(true);
        audio_input_combo->setEnabled(true);
        audio_input_combo->setToolTip(QString());
        asio_constraint_applied = false;
    }

}


void SettingsWidget::updateMicPermissionStatus() {
#if defined(Q_OS_DARWIN)
    // The microphone only feeds live_audio / :sound_in, so this notice is only
    // relevant when audio inputs are enabled. With inputs off, stay hidden.
    if (!enable_scsynth_inputs->isChecked()) {
        mic_permission_label->setVisible(false);
        mic_permission_settings_button->setVisible(false);
        m_lastMicPermissionStatus.clear(); // re-evaluate when inputs are re-enabled
        return;
    }

    std::string status = SonicPi::microphonePermissionStatus();
    if (status == m_lastMicPermissionStatus) return;  // no-op change
    m_lastMicPermissionStatus = status;
    std::cout << "[gui-mic] status now: " << status << std::endl;

    if (status == "authorized") {
        mic_permission_label->setVisible(false);
        mic_permission_settings_button->setVisible(false);
    } else {
        QString msg;
        if (status == "denied")
            msg = tr("Sonic Pi doesn't have microphone access yet, so live_audio and :sound_in will be silent. Click below to grant access in System Settings.");
        else if (status == "restricted")
            msg = tr("Microphone access is restricted by system policy, so live_audio and :sound_in will be silent.");
        else  // notDetermined
            msg = tr("Microphone access not yet granted — click below to open System Settings.");
        mic_permission_label->setText(msg);
        mic_permission_label->setStyleSheet(
            "QLabel { color: palette(highlight); font-weight: bold; }");
        mic_permission_label->setVisible(true);
        mic_permission_settings_button->setVisible(true);
    }
#endif
}

void SettingsWidget::audioDeviceChanged(int index) {
    if (index < 0) return;
    // If the selected item carries a non-empty itemData string (e.g. the
    // "OS Default" sentinel stores "__system__"), emit that instead of
    // the user-visible text. Regular device entries have no itemData so
    // they fall through to currentText() as before.
    QString data = audio_output_combo->currentData().toString();
    // "-- None --" entry (ASIO mode) carries `__none__` data and means
    // "do nothing": the user is on ASIO but hasn't picked an ASIO
    // device yet. Don't fire any OSC — keep the engine on whatever
    // device it was already on.
    if (data == "__none__") {
        std::cout << "[gui-audio] output dropdown picked -- None -- (ASIO no-op)" << std::endl;
        applyAsioInputConstraints();
        return;
    }
    QString emitted = data.isEmpty() ? audio_output_combo->currentText() : data;
    std::cout << "[gui-audio] output dropdown changed: index=" << index
              << " text='" << audio_output_combo->currentText().toUtf8().constData()
              << "' data='" << data.toUtf8().constData()
              << "' emitting='" << emitted.toUtf8().constData() << "'" << std::endl;
    beginDeviceSwitchFeedback();
    emit audioOutputDeviceChanged(emitted);
    // On ASIO, the input combo mirrors the output. Re-apply so the input
    // dropdown stays in sync with the just-picked output device.
    applyAsioInputConstraints();
}

void SettingsWidget::audioInputDeviceChanged(int index) {
    if (index < 0) return;
    QString data = audio_input_combo->currentData().toString();
    QString emitted = data.isEmpty() ? audio_input_combo->currentText() : data;

    // The list is populated for the driver SELECTED in the combo, but an
    // input-only switch is resolved by the engine against the output it
    // currently holds. Those disagree while a driver pick is pending, so ask
    // the policy whether this pick can travel as-is, needs to carry an output
    // to agree with, or cannot be honoured yet (see audioInputPickPlan).
    SonicPi::AudioInputPick pick;
    pick.input        = emitted.toStdString();
    pick.inputDriver  = audio_driver_combo->currentText().toStdString();
    pick.engineDriver = m_engineActualDriver.toStdString();
    {
        const QString outData = audio_output_combo->currentData().toString();
        const QString outName = outData.isEmpty()
                              ? audio_output_combo->currentText() : outData;
        // "-- None --" is "no ASIO device picked yet", not an output.
        if (outName != QLatin1String(SonicPi::kAudioNoInput)) {
            pick.selectedOutput = outName.toStdString();
            pick.selectedOutputDriver = pick.inputDriver;
        }
    }
    const auto plan = SonicPi::audioInputPickPlan(pick);

    std::cout << "[gui-audio] input dropdown changed: index=" << index
              << " text='" << audio_input_combo->currentText().toUtf8().constData()
              << "' data='" << data.toUtf8().constData()
              << "' emitting='" << emitted.toUtf8().constData()
              << "' send=" << (plan.send ? 1 : 0)
              << " withOutput='" << plan.output.c_str() << "'" << std::endl;

    if (!plan.send) {
        // Say what is missing, here, instead of firing a switch the engine
        // will refuse with an error naming an output the user never touched.
        setAudioStatus(QString::fromStdString(plan.refusalReason), true);
        // Put the dropdown back: nothing was applied, so it must not read as
        // though it was.
        QSignalBlocker b(audio_input_combo);
        audio_input_combo->setCurrentIndex(0);
        return;
    }

    beginDeviceSwitchFeedback();
    if (!plan.output.empty()) {
        // Cross-driver but coherent: send both names so the engine resolves
        // them under one driver.
        emit audioDeviceAndInputChanged(QString::fromStdString(plan.output),
                                        QString::fromStdString(plan.input));
        return;
    }
    emit audioInputDeviceChangedSignal(QString::fromStdString(plan.input));
}

void SettingsWidget::audioSampleRateChanged(int index) {
    if (index < 0) return;
    int rate = audio_sample_rate_combo->currentData().toInt();
    std::cout << "[gui-audio] sample-rate dropdown changed: index=" << index
              << " rate=" << rate << std::endl;
    beginDeviceSwitchFeedback();
    emit sampleRateChanged(rate);
}

void SettingsWidget::audioBufferSizeChanged(int index) {
    if (index < 0) return;
    int bs = audio_buffer_size_combo->currentData().toInt();
    std::cout << "[gui-audio] buffer-size dropdown changed: index=" << index
              << " bs=" << bs << std::endl;
    beginDeviceSwitchFeedback();
    emit bufferSizeChanged(bs);
}

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
void SettingsWidget::recordingTypeChanged(int mode) {
    if (mode < 0) return;  // no button checked
    // mode is the enum value (button IDs were set to it directly).
    // Persistence + sync is owned by MainWindow::setRecordingMode.
    emit recordingModeChangedFromPrefs(mode);
}
#endif

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
void SettingsWidget::windowPublishingToggled(int id) {
    if (id < 0) return;  // no button checked
    // The publisher + menubar sync are owned by MainWindow::setWindowPublishing.
    emit windowPublishingChangedFromPrefs(id == 1);
}

void SettingsWidget::syncWindowPublishing(bool on) {
    // setChecked emits toggled, not idClicked, so this doesn't echo back.
    (on ? publish_send_radio : publish_off_radio)->setChecked(true);
}

// Both segments share the wider natural width, so Off doesn't hug its short
// caption. Measured from polished size hints (not font metrics): the font
// and padding that set the hints come from the app stylesheet, which the
// widgets only carry once polished — and which can change with the theme,
// so refreshThemeCards re-runs this.
void SettingsWidget::equalizePublishSegments() {
    if (!publish_off_radio || !publish_send_radio) return;
    publish_off_radio->ensurePolished();
    publish_send_radio->ensurePolished();
    const int w = qMax(publish_off_radio->sizeHint().width(),
                       publish_send_radio->sizeHint().width());
    publish_off_radio->setFixedWidth(w);
    publish_send_radio->setFixedWidth(w);
}
#endif

void SettingsWidget::changeMainVolume(int vol) {
    emit volumeChanged(vol);
}

void SettingsWidget::changeMainDrive(int drive) {
    emit driveChanged(drive);
}

void SettingsWidget::syncMixerControls(int volumePct, int drivePct) {
    // Server-side truth arriving (set_volume! / set_drive! from code, or a
    // GUI change round-tripping). Move the dials without re-emitting: the
    // round trip must end here, and updateSettings would otherwise fire
    // too.
    {
        QSignalBlocker block(system_vol_slider);
        system_vol_slider->setValue(volumePct);
    }
    {
        QSignalBlocker block(system_drive_slider);
        system_drive_slider->setValue(drivePct);
    }
}

void SettingsWidget::setLevelScope(QWidget* scope) {
    if (!scope || !m_levelScopeSlot) return;
    scope->setAccessibleName(tr("Level"));
    scope->setProperty("tipTitle", tr("Level"));
    scope->setToolTip(tr("Shows the output level of the left and right channels. The bar reaches into the accent colour when the main limiter is reducing the level."));
    // A slim strip: same width as the Drive slider below it, and a height
    // suited to one meter rather than a whole scope dock.
    scope->setFixedWidth(kStripWidthPx);
    scope->setFixedHeight(kLevelScopeHeightPx);
    m_levelScopeSlot->addWidget(scope, 0, Qt::AlignHCenter);
}

void SettingsWidget::toggleLineNumbers() {
    emit showLineNumbersChanged();
}

void SettingsWidget::showAutoCompletion() {
  emit showAutoCompletionChanged();
}

void SettingsWidget::showCompletionHelp() {
  emit showCompletionHelpChanged();
}

void SettingsWidget::showContext() {
  emit showContextChanged();
}

void SettingsWidget::flashOnPlay() {
  emit flashSettingsChanged();
}

void SettingsWidget::speakTransport() {
  emit speakTransportChanged();
}

void SettingsWidget::reduceMotion() {
  emit reduceMotionChanged();
}

void SettingsWidget::toggleLog() {
    emit showLogChanged();
}

void SettingsWidget::toggleCuesLog() {
    emit showCuesChanged();
}

void SettingsWidget::toggleMetro() {
    emit showMetroChanged();
}

void SettingsWidget::toggleButtons() {
    emit showButtonsChanged();
}

void SettingsWidget::toggleEditorToolbar() {
    emit showEditorToolbarChanged();
}

void SettingsWidget::toggleFullScreen() {
    emit showFullscreenChanged();
}

void SettingsWidget::toggleTabs() {
    emit showTabsChanged();
}

void SettingsWidget::toggleLogAutoScroll() {
    emit logAutoScrollChanged();
}

void SettingsWidget::updateColourTheme() {
    emit themeChanged();
}

// Tint the dial to the theme's highlight/accent colour rotated by `degrees`, so
// dragging previews exactly what the accent will become. Repolishing this one
// small widget is cheap; the whole-page re-theme is deferred to release.
void SettingsWidget::updateHueDialTint(int degrees) {
    if (!m_hueDial || !m_huePreviewBase.isValid()) return;
    // Run the accent through the theme's transform pipeline with the dial's
    // in-flight rotation, so the dial previews the accent exactly as the rest
    // of the interface will render it under the current toggles.
    m_hueDial->setArcColor(SonicPiTheme::applyColourTransforms(
        m_huePreviewBase,
        piSettings && piSettings->invert_colours,
        piSettings && piSettings->monochrome,
        degrees));
}

void SettingsWidget::setHuePreviewBase(const QColor& baseAccent) {
    m_huePreviewBase = baseAccent;
    updateHueDialTint(piSettings ? piSettings->hue_rotation : 0);
}

void SettingsWidget::setSpreadPreviewBase(const QColor& secondary, SonicPiTheme* theme) {
    m_spreadPreviewBase = secondary;
    m_spreadPreviewTheme = theme;
    updateSpreadDialTint(piSettings ? piSettings->hue_spread
                                    : SonicPiTheme::kHueSpreadDefault);
}

// Tint the spread dial with the secondary as it renders at the dial's value:
// as authored at 0, opening away from the accent as the dial rises.
void SettingsWidget::updateSpreadDialTint(int amount) {
    if (!m_spreadDial || !m_spreadPreviewBase.isValid() || !m_spreadPreviewTheme) return;
    m_spreadDial->setArcColor(m_spreadPreviewTheme->previewWithSpread(
        m_spreadPreviewBase, amount, piSettings ? piSettings->hue_rotation : 0));
}

void SettingsWidget::hueRotationChanged(int degrees) {
    piSettings->hue_rotation = degrees;
    updateHueDialTint(degrees);   // live feedback on the dial only (arc + centre value)
    // Rotation moves the secondary too, so the spread swatch has to follow it.
    updateSpreadDialTint(piSettings ? piSettings->hue_spread
                                    : SonicPiTheme::kHueSpreadDefault);
    if (!m_hueDial || !m_hueDial->isSliderDown())
        emit themeStepChanged();  // keyboard/wheel step (not a drag): can autorepeat, so debounced
}

void SettingsWidget::hueSpreadChanged(int amount) {
    piSettings->hue_spread = amount;
    updateSpreadDialTint(amount);   // live feedback on the dial only
    if (!m_spreadDial || !m_spreadDial->isSliderDown())
        emit themeStepChanged();
}

// The interface only re-themes on release, so mid-drag the caption says so.
void SettingsWidget::setDialCaptionDragging(QLabel* caption, const QString& resting, bool dragging) {
    if (!caption) return;
    caption->setText(dragging ? tr("Release to apply") : resting);
}

void SettingsWidget::resetThemeMods() {
    piSettings->hue_rotation = 0;
    piSettings->hue_spread = SonicPiTheme::kHueSpreadDefault;
    piSettings->monochrome = false;
    piSettings->invert_colours = false;
    if (m_hueDial)     { QSignalBlocker b(m_hueDial);     m_hueDial->setValue(0); }
    if (m_spreadDial)  { QSignalBlocker b(m_spreadDial);  m_spreadDial->setValue(SonicPiTheme::kHueSpreadDefault); }
    if (monochromeCheck) { QSignalBlocker b(monochromeCheck); monochromeCheck->setChecked(false); }
    if (invertCheck)     { QSignalBlocker b(invertCheck);     invertCheck->setChecked(false); }
    if (gui_transparency_slider) gui_transparency_slider->setValue(0);
    updateHueDialTint(0);
    updateSpreadDialTint(SonicPiTheme::kHueSpreadDefault);
    emit themeChanged();
}

// The three iconic Sonic Pi Greek glyphs (lambda, delta, pi), drawn in a centred
// row and recoloured to `tint` (masked by each glyph's own alpha) so they read on
// any card background. A member (not a build-time lambda) so refreshThemeCards()
// can regenerate it when the global colour filters change.
QPixmap SettingsWidget::makeThemeCardGlyphs(const QColor& tint) const {
    const QSize iconSize(ScaleWidthForDPI(80), ScaleHeightForDPI(26));
    const qreal dpr = devicePixelRatioF();
    static const char* const paths[] = {
        ":/images/toolbar/pro/info.png",    // lambda
        ":/images/toolbar/pro/help.png",    // delta
        ":/images/toolbar/pro/prefs.png",   // pi
    };
    QPixmap pm(iconSize * dpr);
    pm.setDevicePixelRatio(dpr);
    pm.fill(Qt::transparent);
    QPainter p(&pm);
    p.setRenderHint(QPainter::Antialiasing, true);
    p.setRenderHint(QPainter::SmoothPixmapTransform, true);
    const qreal w = iconSize.width(), h = iconSize.height();
    const qreal ih = h * 0.82;   // glyph height; width follows aspect ratio
    auto tinted = [&](const QString& path) -> QPixmap {
        QPixmap src(path);
        if (src.isNull()) return src;
        QPixmap t(src.size());
        t.setDevicePixelRatio(src.devicePixelRatio());
        t.fill(Qt::transparent);
        QPainter tp(&t);
        tp.drawPixmap(0, 0, src);
        tp.setCompositionMode(QPainter::CompositionMode_SourceIn);
        tp.fillRect(t.rect(), tint);
        tp.end();
        return t;
    };
    QList<QPixmap> glyphs;
    qreal totalW = 0;
    const qreal gap = ScaleWidthForDPI(7);
    for (const char* path : paths) {
        const QPixmap t = tinted(QString::fromLatin1(path));
        glyphs.append(t);
        if (!t.isNull())
            totalW += ih * (qreal(t.width()) / qMax(1, t.height()));
    }
    if (!glyphs.isEmpty())
        totalW += gap * (glyphs.size() - 1);
    qreal x = (w - totalW) / 2.0;
    for (const QPixmap& t : glyphs) {
        if (t.isNull()) continue;
        const qreal iw = ih * (qreal(t.width()) / qMax(1, t.height()));
        p.drawPixmap(QRectF(x, (h - ih) / 2.0, iw, ih), t, QRectF(t.rect()));
        x += iw + gap;
    }
    p.end();
    return pm;
}

void SettingsWidget::refreshThemeCards(SonicPiTheme* theme) {
    m_cardTheme = theme;
    if (!theme) return;
    // Preview each card's palette as the interface would render it under the
    // current global filters, so the cards track hue rotation / monochrome /
    // invert along with everything else.
    for (const ThemeCardInfo& c : m_themeCards) {
        const QColor bg     = theme->applyGlobalTransforms(c.bg);
        const QColor fg     = theme->applyGlobalTransforms(c.fg);
        const QColor accent = theme->applyGlobalTransforms(c.accent);
        const QColor border = theme->applyGlobalTransforms(c.border);
        static_cast<ThemeCard*>(c.card)->setCardColors(bg, border);
        // The selected/hover ring uses the current theme's accent (not a fixed pink).
        static_cast<ThemeCard*>(c.card)->setHighlight(theme->color("HighlightedBackground"));
        // Recolour the name label directly — re-setting the button's own
        // stylesheet here would re-polish its child layout and make the card
        // creep taller on every rotate.
        if (c.name) c.name->setStyleSheet(QString("background:transparent; color:%1;").arg(fg.name()));
        if (c.icon) c.icon->setPixmap(makeThemeCardGlyphs(accent));
    }

    // Recording-mode segmented icons bake their off/on colours in, so regenerate
    // them on theme change: resting = window foreground, selected = auto-contrast
    // against the accent fill (black on neon green, white on dark, …).
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    if (recording_type_audio_radio && recording_type_av_radio) {
        const int px = ScaleHeightForDPI(32);
        const QColor off = theme->color("WindowForeground");
        const QColor on = theme->contrastingText(theme->color("HighlightedBackground"));
        recording_type_audio_radio->setIcon(makeSvgToggleIcon(kWaveformSvg, off, on, px));
        recording_type_av_radio->setIcon(makeSvgToggleIcon(kVideoSvg, off, on, px));
    }
#endif
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // The window-publishing pill's icons bake their colours in the same way.
    if (publish_off_radio && publish_send_radio) {
        const int px = ScaleHeightForDPI(32);
        const QColor off = theme->color("WindowForeground");
        const QColor on = theme->contrastingText(theme->color("HighlightedBackground"));
        publish_off_radio->setIcon(makeSvgToggleIcon(kCastOffSvg, off, on, px));
        publish_send_radio->setIcon(makeSvgToggleIcon(kCastSvg, off, on, px));
        equalizePublishSegments();
    }
#endif
}


void SettingsWidget::toggleScope() {
    emit scopeChanged();
}

void SettingsWidget::toggleScopeLabels() {
    emit scopeLabelsChanged();
}

void SettingsWidget::toggleTitles() {
    emit titlesChanged();
}

void SettingsWidget::toggleHideMenuBarInFullscreen() {
    emit hideMenuBarInFullscreenChanged();
}

void SettingsWidget::updateTransparency(int t) {
    emit transparencyChanged(t);
}

void SettingsWidget::toggleCheckUpdates() {
    emit checkUpdatesChanged();
}

void SettingsWidget::checkForUpdatesNow() {
    emit forceCheckUpdates();
}

void SettingsWidget::checkArgs() {
  emit checkArgsChanged();
}

void SettingsWidget::synthTriggerTimingGuarantees() {
  emit synthTriggerTimingGuaranteesChanged();
}

void SettingsWidget::enableExternalSynths() {
  emit enableExternalSynthsChanged();
}

void SettingsWidget::midiDefaultChannel() {
  emit midiDefaultChannelChanged();
}

void SettingsWidget::logCues() {
  emit logCuesChanged();
}

void SettingsWidget::logSynths() {
  emit logSynthsChanged();
}

void SettingsWidget::clearOutputOnRun() {
  emit clearOutputOnRunChanged();
}


void SettingsWidget::autoIndentOnRun() {
  emit autoIndentOnRunChanged();
}

void SettingsWidget::openSonicPiNet() {
  QDesktopServices::openUrl(QUrl("https://sonic-pi.net", QUrl::TolerantMode));
}

void SettingsWidget::updateVersionInfo( QString info_string, QString visit, bool sonic_pi_net_visible, bool check_now_visible) {
    update_info->setText( info_string );
    visit_sonic_pi_net->setText( visit );
    visit_sonic_pi_net->setVisible(sonic_pi_net_visible);
    check_updates_now->setVisible(check_now_visible);
}

void SettingsWidget::updateSettings() {

    std::cout << "[GUI] - update settings" << std::endl;
    piSettings->language = available_languages[language_combo->currentIndex()];
    piSettings->mixer_invert_stereo = mixer_invert_stereo->isChecked();
    piSettings->enable_scsynth_inputs = enable_scsynth_inputs->isChecked();
    piSettings->mixer_force_mono = mixer_force_mono->isChecked();
    piSettings->check_args = check_args->isChecked();
    piSettings->synth_trigger_timing_guarantees = synth_trigger_timing_guarantees_cb->isChecked();
    piSettings->enable_external_synths = enable_external_synths_cb->isChecked();
    piSettings->main_volume = system_vol_slider->value();
    piSettings->main_drive = system_drive_slider->value();

    piSettings->osc_server_enabled = osc_server_enabled_check->isChecked();
    piSettings->osc_public = osc_server_enabled_check->isChecked() && osc_public_check->isChecked();
    if(piSettings->osc_server_enabled){
      osc_public_check->show();
    } else {
      osc_public_check->hide();
    }
    if(!osc_server_enabled_check->isChecked()) {
      osc_public_check->setChecked(false);
    }

    QString channel_pat_str = midi_default_channel_combo->currentText();
    if(channel_pat_str.startsWith("*")) {
      channel_pat_str = QString("*");
    }

    piSettings->midi_default_channel = midi_default_channel_combo->currentIndex();
    piSettings->midi_default_channel_str = channel_pat_str;
    piSettings->midi_enabled = midi_enable_check->isChecked();
    piSettings->gamepad_enabled = gamepad_enable_check->isChecked();

    piSettings->auto_indent_on_run = auto_indent_on_run->isChecked();
    piSettings->show_line_numbers = show_line_numbers->isChecked();
    piSettings->show_autocompletion = show_autocompletion->isChecked();
    piSettings->show_completion_help = show_completion_help->isChecked();
    piSettings->show_context = show_context->isChecked();
    piSettings->flash_code = flash_code->isChecked();
    piSettings->flash_brightness = flash_brightness_slider->value();
    piSettings->flash_gutter = flash_gutter->isChecked();
    piSettings->show_loop_scopes = show_loop_scopes->isChecked();
    piSettings->loop_scope_scroll = loop_scope_scroll->isChecked();
    piSettings->speak_transport = speak_transport->isChecked();
    piSettings->reduce_motion = reduce_motion->isChecked();
    // Widgets consult prefersReducedMotion() directly (no settings pointer
    // there), so push the preference into the shared flag as it changes.
    SonicPi::setReduceMotionPreference(piSettings->reduce_motion);
    piSettings->show_log = show_log->isChecked();
    piSettings->show_cues = show_cues->isChecked();
    piSettings->show_metro = show_metro->isChecked();
    piSettings->show_buttons = show_buttons->isChecked();
    piSettings->show_editor_toolbar = show_editor_toolbar->isChecked();
    piSettings->show_tabs = show_tabs->isChecked();
    piSettings->full_screen = full_screen->isChecked();
    piSettings->log_synths = log_synths->isChecked();
    piSettings->clear_output_on_run = clear_output_on_run->isChecked();
    piSettings->log_cues = log_cues->isChecked();
    piSettings->log_auto_scroll = log_auto_scroll->isChecked();
    piSettings->gui_transparency = gui_transparency_slider->value();
    SonicPiTheme::ColourScheme scheme = SonicPiTheme::LightScheme;
    if (darkModeCheck->isChecked())         { scheme = SonicPiTheme::DarkScheme; }
    if (highContrastModeCheck->isChecked()) { scheme = SonicPiTheme::HighContrastScheme; }
    if (mildModeCheck->isChecked())         { scheme = SonicPiTheme::MildDarkScheme; }
    if (phosphorModeCheck->isChecked())     { scheme = SonicPiTheme::PhosphorScheme; }
    if (signalModeCheck->isChecked())       { scheme = SonicPiTheme::SignalScheme; }
    piSettings->colourScheme = scheme;
    piSettings->proIcons = proIconsCheck->isChecked();
    if (m_hueDial) piSettings->hue_rotation = m_hueDial->value();
    if (m_spreadDial) piSettings->hue_spread = m_spreadDial->value();
    if (monochromeCheck) piSettings->monochrome = monochromeCheck->isChecked();
    if (invertCheck) piSettings->invert_colours = invertCheck->isChecked();

    piSettings->show_scopes = show_scopes->isChecked();
    piSettings->show_scope_labels = show_scope_labels->isChecked();
    piSettings->show_titles = show_titles->isChecked();
    piSettings->hide_menubar_in_fullscreen = hide_menubar_in_fullscreen->isChecked();

    piSettings->check_updates = check_updates->isChecked();
}

void SettingsWidget::settingsChanged() {
    language_combo->setCurrentIndex(available_languages.indexOf(piSettings->language));
    QString language_detail_text = "";
    if (!i18n) {
      language_detail_text += "<b>Failed to load language translation. Using English (UK).</b>";
    }
    if (piSettings->language == "system_language") {
      language_detail_text += tr("System language: %1\n").arg(sonicPii18n->getNativeLanguageName(sonicPii18n->currentlyLoadedLanguage()));
    }
    language_details_label->setText(language_detail_text);

    mixer_invert_stereo->setChecked(piSettings->mixer_invert_stereo);
    mixer_force_mono->setChecked(piSettings->mixer_force_mono);
    enable_scsynth_inputs->setChecked(piSettings->enable_scsynth_inputs);
    check_args->setChecked(piSettings->check_args);
    synth_trigger_timing_guarantees_cb->setChecked( piSettings->synth_trigger_timing_guarantees);
    enable_external_synths_cb->setChecked(piSettings->enable_external_synths);
    system_vol_slider->setValue(piSettings->main_volume);
    system_drive_slider->setValue(piSettings->main_drive);

    osc_server_enabled_check->setChecked(piSettings->osc_server_enabled);
    if(piSettings->osc_server_enabled){
      osc_public_check->show();
    } else {
      osc_public_check->hide();
    }
    osc_public_check->setChecked(piSettings->osc_server_enabled && piSettings->osc_public);
    midi_default_channel_combo->setCurrentIndex(piSettings->midi_default_channel);
    piSettings->midi_default_channel_str = midi_default_channel_combo->currentText(); // TODO find a more elegant solution
    midi_enable_check->setChecked(piSettings->midi_enabled);
    gamepad_enable_check->setChecked(piSettings->gamepad_enabled);

    auto_indent_on_run->setChecked(piSettings->auto_indent_on_run);

    show_line_numbers->setChecked(piSettings->show_line_numbers);
    show_log->setChecked(piSettings->show_log);
    show_cues->setChecked(piSettings->show_cues);
    show_metro->setChecked(piSettings->show_metro);
    show_buttons->setChecked(piSettings->show_buttons);
    show_editor_toolbar->setChecked(piSettings->show_editor_toolbar);
    show_tabs->setChecked(piSettings->show_tabs);
    full_screen->setChecked(piSettings->full_screen);
    log_synths->setChecked(piSettings->log_synths);
    clear_output_on_run->setChecked(piSettings->clear_output_on_run);
    log_cues->setChecked(piSettings->log_cues);
    log_auto_scroll->setChecked(piSettings->log_auto_scroll);
    gui_transparency_slider->setValue(piSettings->gui_transparency);
    const SonicPiTheme::ColourScheme scheme = piSettings->colourScheme;
    lightModeCheck->setChecked( scheme == SonicPiTheme::LightScheme );
    darkModeCheck->setChecked( scheme == SonicPiTheme::DarkScheme );
    highContrastModeCheck->setChecked( scheme == SonicPiTheme::HighContrastScheme );
    mildModeCheck->setChecked( scheme == SonicPiTheme::MildDarkScheme );
    phosphorModeCheck->setChecked( scheme == SonicPiTheme::PhosphorScheme );
    signalModeCheck->setChecked( scheme == SonicPiTheme::SignalScheme );
    proIconsCheck->setChecked( piSettings->proIcons );
    if (m_hueDial) { QSignalBlocker hb(m_hueDial); m_hueDial->setValue(piSettings->hue_rotation); }
    if (m_spreadDial) { QSignalBlocker sb(m_spreadDial); m_spreadDial->setValue(piSettings->hue_spread); }
    if (monochromeCheck) monochromeCheck->setChecked(piSettings->monochrome);
    if (invertCheck) invertCheck->setChecked(piSettings->invert_colours);

    show_scopes->setChecked(piSettings->show_scopes);
    show_scope_labels->setChecked(piSettings->show_scope_labels);
    show_titles->setChecked(piSettings->show_titles);
    hide_menubar_in_fullscreen->setChecked(piSettings->hide_menubar_in_fullscreen);

    check_updates->setChecked(piSettings->check_updates);
    show_autocompletion->setChecked(piSettings->show_autocompletion);
    show_completion_help->setChecked(piSettings->show_completion_help);
    show_context->setChecked(piSettings->show_context);
    flash_code->setChecked(piSettings->flash_code);
    { QSignalBlocker fb(flash_brightness_slider); flash_brightness_slider->setValue(piSettings->flash_brightness); }
    flash_gutter->setChecked(piSettings->flash_gutter);
    show_loop_scopes->setChecked(piSettings->show_loop_scopes);
    loop_scope_scroll->setChecked(piSettings->loop_scope_scroll);
    speak_transport->setChecked(piSettings->speak_transport);
    reduce_motion->setChecked(piSettings->reduce_motion);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // setChecked emits toggled, not idClicked, so this doesn't echo
    // back to recordingTypeChanged.
    if (piSettings->recording_type == SonicPiSettings::AudioAndVideo) {
        recording_type_av_radio->setChecked(true);
    } else {
        recording_type_audio_radio->setChecked(true);
    }
#endif
    updateScopeKindVisibility();
}

void SettingsWidget::connectAll() {
    //connect(language_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(language_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateUILanguage(int)));
    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));
    connect(system_drive_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));
    connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
    connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));
    connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(changeMainVolume(int)));
    connect(system_drive_slider, SIGNAL(valueChanged(int)), this, SLOT(changeMainDrive(int)));
    connect(enable_scsynth_inputs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(enable_scsynth_inputs, SIGNAL(clicked()), this, SLOT(updateEnableScsynthInputs()));

    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(toggleMidi()));
    connect(gamepad_enable_check, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(gamepad_enable_check, SIGNAL(clicked()), this, SLOT(toggleGamepad()));
    connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));
    connect(osc_public_check, SIGNAL(clicked()), this, SLOT(toggleOscServer()));

    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_metro, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_editor_toolbar, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(mildModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(phosphorModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(signalModeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(proIconsCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(monochromeCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(invertCheck, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_completion_help, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_code, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_code, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(flash_gutter, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(flash_gutter, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(show_loop_scopes, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_loop_scopes, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(loop_scope_scroll, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(loop_scope_scroll, SIGNAL(clicked()), this, SLOT(flashOnPlay()));
    connect(flash_brightness_slider, SIGNAL(valueChanged(int)), this, SLOT(updateSettings()));
    connect(flash_brightness_slider, SIGNAL(valueChanged(int)), this, SLOT(flashOnPlay()));
    connect(speak_transport, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(speak_transport, SIGNAL(clicked()), this, SLOT(speakTransport()));
    connect(reduce_motion, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(reduce_motion, SIGNAL(clicked()), this, SLOT(reduceMotion()));

    connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(toggleLineNumbers()));
    connect(show_log, SIGNAL(clicked()), this, SLOT(toggleLog()));
    connect(show_cues, SIGNAL(clicked()), this, SLOT(toggleCuesLog()));
    connect(show_metro, SIGNAL(clicked()), this, SLOT(toggleMetro()));
    connect(show_buttons, SIGNAL(clicked()), this, SLOT(toggleButtons()));
    connect(show_editor_toolbar, SIGNAL(clicked()), this, SLOT(toggleEditorToolbar()));
    connect(full_screen, SIGNAL(clicked()), this, SLOT(toggleFullScreen()));
    connect(show_tabs, SIGNAL(clicked()), this, SLOT(toggleTabs()));
    connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(toggleLogAutoScroll()));
    connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(mildModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(phosphorModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(signalModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(proIconsCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(monochromeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(invertCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
    connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(updateTransparency(int)));

    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_scope_labels, SIGNAL(clicked()), this, SLOT(toggleScopeLabels()));
    connect(show_titles, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(show_titles, SIGNAL(clicked()), this, SLOT(toggleTitles()));
    connect(hide_menubar_in_fullscreen, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(hide_menubar_in_fullscreen, SIGNAL(clicked()), this, SLOT(toggleHideMenuBarInFullscreen()));
    connect(show_scopes, SIGNAL(clicked()), this, SLOT(toggleScope()));

    connect(check_updates, SIGNAL(clicked()), this, SLOT(updateSettings()));
    connect(check_updates, SIGNAL(clicked()), this, SLOT(toggleCheckUpdates()));
    connect(visit_sonic_pi_net, SIGNAL(clicked()), this, SLOT(openSonicPiNet()));
    connect(check_updates_now, SIGNAL(clicked()), this, SLOT(checkForUpdatesNow()));

    connect(show_autocompletion, SIGNAL(clicked()), this, SLOT(showAutoCompletion()));
    connect(show_completion_help, SIGNAL(clicked()), this, SLOT(showCompletionHelp()));
    connect(show_context, SIGNAL(clicked()), this, SLOT(showContext()));
    connect(check_args, SIGNAL(clicked()), this, SLOT(checkArgs()));
    connect(synth_trigger_timing_guarantees_cb, SIGNAL(clicked()), this, SLOT(synthTriggerTimingGuarantees()));
    connect(enable_external_synths_cb, SIGNAL(clicked()), this, SLOT(enableExternalSynths()));
    connect(midi_default_channel_combo, SIGNAL(currentIndexChanged(int)), this, SLOT(midiDefaultChannel()));
    connect(log_cues, SIGNAL(clicked()), this, SLOT(logCues()));
    connect(log_synths, SIGNAL(clicked()), this, SLOT(logSynths()));
    connect(clear_output_on_run, SIGNAL(clicked()), this, SLOT(clearOutputOnRun()));
    connect(auto_indent_on_run, SIGNAL(clicked()), this, SLOT(autoIndentOnRun()));

    // Prefs checkboxes: focus only via keyboard (Tab), not a mouse click. A click
    // still toggles the box, but no longer grabs focus and draws the blue focus
    // ring around the whole row — which read as a pointless "selection". Keyboard
    // users keep the focus ring (and Space to toggle) for accessibility.
    for (QCheckBox* cb : findChildren<QCheckBox*>()) {
        cb->setFocusPolicy(Qt::TabFocus);
    }
    // Watch focus application-wide so that checkboxes created after startup
    // (scope kinds, MIDI/OSC device rows) get the keyboard focus ring too
    // (see eventFilter). Installed once: connectAll() runs only from the
    // constructor, and SettingsWidget is constructed once.
    qApp->installEventFilter(this);
}

bool SettingsWidget::eventFilter(QObject* obj, QEvent* event)
{
    // Re-elide the audio status message whenever the column resizes.
    if (obj == audio_status_label && event->type() == QEvent::Resize) {
        // Re-eliding must not demote a sticky notice to a clearable one.
        setAudioStatus(m_audioStatusText, m_audioStatusSticky);
        return false;
    }

    // The checkbox focus ring (QCheckBox[kbFocus="true"] in app.qss) reads as a
    // pointless "selection box" when a mouse click draws it. Show it ONLY for
    // keyboard (Tab) focus: flip the kbFocus property by focus reason and repolish.
    // The toggle-on-click is unaffected. This filter is installed on qApp so it
    // covers every checkbox, including ones created after startup.
    QCheckBox* cb = qobject_cast<QCheckBox*>(obj);
    if (cb && (event->type() == QEvent::FocusIn || event->type() == QEvent::FocusOut)) {
        const bool kb = event->type() == QEvent::FocusIn
            && [event]() { const Qt::FocusReason r = static_cast<QFocusEvent*>(event)->reason();
                           return r == Qt::TabFocusReason || r == Qt::BacktabFocusReason; }();
        if (cb->property("kbFocus").toBool() != kb) {
            cb->setProperty("kbFocus", kb);
            cb->style()->unpolish(cb);
            cb->style()->polish(cb);
            cb->update();
        }
    }
    return QWidget::eventFilter(obj, event);
}

void SettingsWidget::add_language_combo_box_entries(QComboBox* combo) {
  // Add language combo entries
  std::cout << "[Debug] Adding language combo box entries..." << std::endl;
  std::cout << (std::to_string(static_cast<int>(available_languages.size()))) << std::endl;

  for (auto const &language : available_languages) {
    std::cout << "[Debug] Adding language " << language.toUtf8().data() << " to the combo box" << std::endl;
    if (language != "system_language") {
      // Add the language's name to the combo box
      combo->addItem(sonicPii18n->getNativeLanguageName(language));
    } else {
      combo->addItem(tr("Use system language"));
    }
  }
}
