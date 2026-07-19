//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

// Standard stuff
#include <fstream>
#include <iostream>
#include <sstream>

// Qt stuff
#include <QAction>
#include <QActionGroup>
#include <QApplication>
#include <QClipboard>
#include <QAccessible>
#include <QBoxLayout>
#include <QDesktopServices>
#include <QDialogButtonBox>
#include <QDockWidget>
#include <QDateTime>
#include <QDir>
#include <QRegularExpression>
#include <QFile>
#include <QFileDialog>
#include <QStandardPaths>
#include <QUuid>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMenu>
#include <QMenuBar>
#include <QPropertyAnimation>
#include <QMessageBox>
#include <QNetworkInterface>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QSet>
#include <QShortcut>
#include <QTimer>
#include <QSplitter>
#include <QStackedWidget>
#include <QStatusBar>
#include <QStyle>
#include <QStyledItemDelegate>
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
#include <QAccessibilityHints>
#endif
#include <QTextBrowser>
#include <QTextDocument>
#include <QTextEdit>
#include <QTextStream>
#include <QThread>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>
#include <QtGlobal>

#include "mainwindow.h"

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>

#include "model/sonicpitheme.h"
#include "widgets/sonicpitooltip.h"
#include <QFileOpenEvent>

#include "utils/reducedmotion.h"
#include "utils/gui_settings.h"
#include "utils/scintilla_api.h"
#include "utils/setbundle.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"
#include "widgets/sonicpierrorcard.h"
#include "utils/chrome_metrics.h"
#include "widgets/icontabbar.h"
#include "widgets/quickstartpane.h"
#include "utils/tablericons.h"
#include "widgets/tutorialpane.h"
#include "widgets/welcomewidget.h"
#include "widgets/splashwidget.h"

#include "utils/sonicpi_i18n.h"

#include "utils/borderlesslinksproxystyle.h"
#include "visualizer/scope_window.h"

#include "qt_api_client.h"
using namespace oscpkt; // OSC specific stuff

#include "model/settings.h"
#include "widgets/infowidget.h"
#include "widgets/settingswidget.h"
#include "widgets/sonicpicontext.h"
#include "widgets/sonicpieditor.h"
#include "widgets/sonicpilog.h"
#include "widgets/sonicpimetro.h"
#include "widgets/linkaudiostreamswidget.h"
#include "widgets/logpanel.h"
#include "widgets/metricspanel.h"
#include "widgets/zoombar.h"
#include "widgets/thinsplitter.h"
#include "utils/dividerproxystyle.h"
#include "utils/tablericons.h"

#include <QMouseEvent>

#include "utils/ruby_help.h"

#include "dpi.h"

// Operating System Specific includes
#if defined(Q_OS_WIN)
#include <QtConcurrent/QtConcurrentRun>
#elif defined(Q_OS_MAC)
#include <QtConcurrent/QtConcurrentRun>
#else
// assuming Raspberry Pi
#include <QtConcurrentRun>
#include <cmath>
#endif

#if QT_VERSION >= 0x050400
// Requires Qt5
#include <QWindow>
#endif

#ifdef Q_OS_MAC
#include "platform/macos.h"
#endif

#ifdef Q_OS_WIN
#include "platform/windows.h"
#endif

using namespace std::chrono;

using namespace SonicPi;

MainWindow::MainWindow(QApplication& app, SplashWidget* splash)
{
    app.installEventFilter(this);
    app.processEvents();
    connect(&app, &QApplication::aboutToQuit, this, &MainWindow::onExitCleanup);

    printAsciiArtLogo();

    QApplication::instance()->setAttribute(Qt::AA_DontShowIconsInMenus, true);

    this->splash = splash;

    // API and Client
    m_spClient = std::make_shared<QtAPIClient>(this);
    m_spAPI = std::make_shared<SonicPiAPI>(m_spClient.get(), APIProtocol::UDP, LogOption::File);

    this->piSettings = new SonicPiSettings();

    startup_error_reported = new QCheckBox(this);
    startup_error_reported->setChecked(false);

    hash_salt = "Secret Hash ;-)";

    updated_dark_mode_for_help = false;
    updated_dark_mode_for_prefs = false;
    loaded_workspaces = false;
    is_recording = false;
    show_rec_icon_a = false;
    restoreDocPane = false;
    focusMode = false;
    version = SONIC_PI_VERSION;
    latest_version = "";
    version_num = 0;
    latest_version_num = 0;

    APIInitResult init_success = m_spAPI->Init(rootPath().toStdString());

    if (init_success == APIInitResult::HomePathNotWritableError)
    {
        std::cout << "[GUI] - API HomePath Not Writable" << std::endl;
        homeDirWriteError();
    }
    else if (init_success != APIInitResult::Successful)
    {
        std::cout << "[GUI] - API Init failed" << std::endl;
    }

    initPaths();
    readSettings();
    bool noScsynthInputs = !piSettings->enable_scsynth_inputs;
    APIBootResult boot_success = m_spAPI->Boot(noScsynthInputs);

    if (boot_success == APIBootResult::Successful)
    {
        std::cout << "[GUI] - API Boot successful" << std::endl;
    }
    else
    {
        std::cout << "[GUI] - API Boot failed" << std::endl;
    }

    const QRect rect = this->geometry();
    m_appWindowSizeRect = std::make_shared<QRect>(rect);

    guiID = m_spAPI->GetGuid();

    this->sonicPii18n = new SonicPii18n(rootPath());
    std::cout << "[GUI] - Language setting: " << piSettings->language.toUtf8().constData() << std::endl;
    std::cout << "[GUI] - System language: " << QLocale::system().name().toStdString() << std::endl;
    this->ui_language = sonicPii18n->determineUILanguage(piSettings->language);
    std::cout << "[GUI] - Using language: " << ui_language.toUtf8().constData() << std::endl;
    this->i18n = sonicPii18n->loadTranslations(ui_language);

    // The splash was built in main() before the translator was installed, so
    // its screen-reader announcement resolved to English; re-resolve it now.
    if (splash)
        splash->retranslate();

    if (i18n)
    {
        std::cout << "[GUI] - translations available " << std::endl;
    }
    else
    {
        std::cout << "[GUI] - translations unavailable (using EN)" << std::endl;
    }

    std::cout << "[GUI] - hiding main window" << std::endl;
    hide();

    setupTheme();

    lexer = new SonicPiLexer(theme);
    QPalette p = theme->createPalette();
    QApplication::setPalette(p);

    setupWindowStructure();

    loadUserShortcuts();
    createStatusBar();
    createInfoPane();
    setWindowTitle(tr("Sonic Pi"));

    createToolBar();
    updateShortcuts();
    updateTabsVisibility();
    updateButtonVisibility();
    // After BOTH setupWindowStructure (the workspaces) and createToolBar
    // (the action) — it touches each.
    updateEditorToolbarVisibility();
    updateLogVisibility();
    updateCuesVisibility();
    createDebugAndLogTabs();
    applySouthTabIcons();

    // The implementation of this method is dynamically generated and can
    // be found in ruby_help.h:
    std::cout << "[GUI] - initialising documentation window" << std::endl;
    initDocsWindow();
    updateDocsNavMinWidth();


    // setup autocompletion
    autocomplete->loadSamples(QString::fromStdString(m_spAPI->GetPath(SonicPiPath::SamplePath)));

    QThreadPool::globalInstance()->setMaxThreadCount(3);

    mirrorToolTipsToAccessibleDescriptions();

    // Win/Linux file associations pass the path via argv; macOS double-clicks
    // arrive as QFileOpenEvent in eventFilter.
    for (const QString& arg : app.arguments().mid(1))
    {
        if (arg.endsWith(".sonicpi", Qt::CaseInsensitive) && QFileInfo(arg).isFile())
        {
            openSetPath(QFileInfo(arg).absoluteFilePath());
            break;
        }
    }

    // Defer the blocking server wait to the live event loop.
    QTimer::singleShot(0, this, &MainWindow::completeBoot);
}

void MainWindow::completeBoot()
{
    // The event loop is live now, so start the strapline animation.
    if (splash)
        splash->startAnimation(piSettings->reduce_motion);

    // Defer the ready poll until the intro has played: onServerReady's heavy
    // synchronous finalisation would freeze the event loop mid-intro and bunch
    // the word reveals. The daemon boots in parallel throughout.
    const int introMs = (splash && !piSettings->reduce_motion) ? splash->introDurationMs() : 0;
    QTimer::singleShot(introMs, this, &MainWindow::beginServerReadyPoll);
}

void MainWindow::beginServerReadyPoll()
{
    // Poll the daemon's readiness without blocking the event loop (the state
    // flips on the API's pinger thread during boot). Same budget as
    // WaitUntilReady: 600 * 100ms = 60s.
    boot_poll_tries = 600;
    boot_poll_timer = new QTimer(this);
    connect(boot_poll_timer, &QTimer::timeout, this, &MainWindow::pollServerReady);
    boot_poll_timer->start(100);
}

void MainWindow::pollServerReady()
{
    if (m_spAPI->IsServerReady())
    {
        boot_poll_timer->stop();
        onServerReady();
        return;
    }

    if (m_spAPI->HasServerErrored() || --boot_poll_tries <= 0)
    {
        boot_poll_timer->stop();
        std::cout << "[GUI] - Critical Error. Unable to connect to server.." << std::endl;
        startupError("GUI was unable to connect to the Ruby server.");
        toggleOSCServer(1);
        editorTabWidget->currentWidget()->activateWindow();
    }
}

void MainWindow::onServerReady()
{
    // We have a connection! Finish up loading app...

    scopeWindow->Booted();
    std::cout << "[GUI] - restore windows" << std::endl;
    restoreWindows();
    std::cout << "[GUI] - honour prefs" << std::endl;
    honourPrefs();
    std::cout << "[GUI] - update prefs icon" << std::endl;
    updatePrefsIcon();
    std::cout << "[GUI] - toggle icons" << std::endl;
    toggleIcons();
    std::cout << "[GUI] - full screen" << std::endl;

    updateFullScreenMode();

    // May swap the colour scheme to high contrast before the first theme
    // application below; also follows live OS contrast toggles from here
    // on (the call creates the QAccessibilityHints instance).
    applyOSContrastPreference();
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
    connect(accessibilityHints, &QAccessibilityHints::contrastPreferenceChanged,
            this, &MainWindow::applyOSContrastPreference);
#endif

    updateColourTheme();
    std::cout << "[GUI] - load workspaces" << std::endl;
    loadWorkspaces();
    std::cout << "[GUI] - load request Version" << std::endl;
    requestVersion();
    changeSystemPreAmp(piSettings->main_volume, 1);

    // Register GUI with SuperSonic for push notifications and get device info
    m_spAPI->RequestAudioDevices();

    QTimer* timer = new QTimer(this);
    connect(timer, &QTimer::timeout, this, &MainWindow::heartbeatOSC);
    timer->start(1000);
    emit settingsChanged();
    splashClose();
    focusEditor();
    showWindow();
    statusBar()->showMessage(tr("Sonic Pi is ready"));
    announce(tr("Sonic Pi is ready"));
    std::cout << "[GUI] - boot sequence completed." << std::endl;

    toggleOSCServer(1);

    editorTabWidget->currentWidget()->activateWindow();

    showWelcomeScreen();

    bootAnnouncementsReady = true;

    // Second pass for widgets (and tooltips) created during boot completion.
    mirrorToolTipsToAccessibleDescriptions();

    std::cout << "[GUI] - MainWindow initialisation completed." << std::endl;
}

void MainWindow::initPaths()
{

    QString settings_path = sonicPiConfigPath() + QDir::separator() + "v5-gui-settings.ini";
    SonicPi::setGuiSettingsPath(settings_path);
    gui_settings = new QSettings(settings_path, QSettings::IniFormat);

    QString root_path = rootPath();

    qt_app_theme_path = QDir::toNativeSeparators(root_path + "/app/gui/theme/app.qss");
    qt_browser_dark_css = QDir::toNativeSeparators(root_path + "/app/gui/theme/dark/doc-styles.css");
    qt_browser_light_css = QDir::toNativeSeparators(root_path + "/app/gui/theme/light/doc-styles.css");
    qt_browser_hc_css = QDir::toNativeSeparators(root_path + "/app/gui/theme/high_contrast/doc-styles.css");
}

void MainWindow::checkForStudioMode()
{
    // Studio mode should always be enabled on linux
#if defined(Q_OS_LINUX)
    studio_mode->setChecked(true);
    return;
#else
    // other operating systems need to support the project
    // to enable studio mode
    studio_mode->setChecked(false);
#endif

    QString queryStr;
    queryStr = QString("%1")
                   .arg(QString(QCryptographicHash::hash(QString(user_token->text() + hash_salt).toUtf8(), QCryptographicHash::Sha256).toHex()));

    QStringList studioHashList = QStringList();

    std::cout << "[GUI] - Fetching Studio hashes" << std::endl;
    QProcess fetchStudioHashes;
    QStringList fetch_studio_hashes_send_args;
    fetch_studio_hashes_send_args << QString::fromStdString(m_spAPI->GetPath(SonicPiPath::FetchUrlPath)) << "http://sonic-pi.net/static/info/studio-hashes.txt";
    fetchStudioHashes.start(QString::fromStdString(m_spAPI->GetPath(SonicPiPath::RubyPath)), fetch_studio_hashes_send_args);
    // Bounded wait so a slow/unreachable network can't freeze the GUI at startup.
    if (!fetchStudioHashes.waitForFinished(5000))
    {
        fetchStudioHashes.kill();
        fetchStudioHashes.waitForFinished(1000);
    }
    QTextStream stream(fetchStudioHashes.readAllStandardOutput().trimmed());
    QString line = stream.readLine();
    while (!line.isNull())
    {
        studioHashList << line;
        line = stream.readLine();
    };

    if (studioHashList.contains(queryStr))
    {
        std::cout << "[GUI] - Found Studio Hash Match" << std::endl;
        std::cout << "[GUI] - Enabling Studio Mode..." << std::endl;
        std::cout << "[GUI] - Thank-you for supporting Sonic Pi's continued development :-)" << std::endl;
        showStatusAndAnnounce(tr("Studio Mode Enabled. Thank-you for supporting Sonic Pi."), 5000);
        studio_mode->setChecked(true);
    }
    else
    {
        std::cout << "[GUI] - No Studio Hash Match Found" << std::endl;
        showStatusAndAnnounce(tr("No Matching Studio Hash Found..."), 1000);
        studio_mode->setChecked(false);
    }
}

void MainWindow::showWelcomeScreen()
{
    if (gui_settings->value("first_time", 1).toInt() == 1)
    {
        WelcomeWidget* welcome = new WelcomeWidget(theme, piSettings->reduce_motion, this);
        connect(welcome, &WelcomeWidget::dismissRequested, this, [this, welcome]() {
            welcome->close();
            focusEditor();
        });
        docWidget->show();
        southTabs->setCurrentWidget(quickstartPane);
        // Size the dock so a fresh install sees two full rows of cards.
        resizeDocks({ docWidget }, { quickstartPane->preferredDockHeight() }, Qt::Vertical);
        docsNavTabs->setCurrentIndex((int)DocTab::Tutorial);
        helpLists[(int)DocTab::Tutorial]->setCurrentRow(0);
        welcome->show();
        welcome->raise();
        welcome->activateWindow();
        outputPane->verticalScrollBar()->setValue(0);
    }
}

void MainWindow::setupTheme()
{
    // Syntax highlighting

    QString themeFilename = sonicPiConfigPath() + QDir::separator() + "v5-colour-theme.properties";

    this->theme = new SonicPiTheme(this, themeFilename, rootPath());

    // Route every tooltip in the app through the themed, anchored popup
    // (installs itself as an application-wide event filter).
    this->toolTipManager = new SonicPiToolTipManager(theme, this);
}

void MainWindow::setupWindowStructure()
{
    std::cout << "[GUI] - setting up window structure" << std::endl;

    setUnifiedTitleAndToolBarOnMac(true);
    setWindowIcon(QIcon(":images/icon-smaller.png"));

    rec_flash_timer = new QTimer(this);
    connect(rec_flash_timer, SIGNAL(timeout()), this, SLOT(toggleRecordingOnIcon()));

    // Setup output and error panes

    outputPane = new SonicPiLog;
    outputPane->setAccessibleName(tr("Log"));
    incomingPane = new SonicPiLog;
    incomingPane->setAccessibleName(tr("Cues"));
    errorPane = new QTextBrowser;
    errorPane->document()->setDocumentMargin(ScaleWidthForDPI(20));  // text inset; keeps the scrollbar flush
    errorPane->setAccessibleName(tr("Errors"));
    metroPane = new SonicPiMetro(m_spClient, m_spAPI, theme, this);

    connect(metroPane, SIGNAL(linkEnabled()), this, SLOT(checkEnableLinkMenu()));
    connect(metroPane, SIGNAL(linkDisabled()), this, SLOT(uncheckEnableLinkMenu()));
    // Metro-row actions (tap tempo, Link on/off, network visibility) report
    // through the status bar and screen reader like every other GUI action.
    connect(metroPane, &SonicPiMetro::statusMessage, this,
            [this](const QString& msg) { showStatusAndAnnounce(msg, 2000); });

    // Handle links ourselves so web links open in the system browser rather
    // than navigating the pane.
    errorPane->setOpenLinks(false);
    connect(errorPane, &QTextBrowser::anchorClicked, this, &MainWindow::onErrorAnchorClicked);

    // Window layout
    editorTabWidget = new QTabWidget();
    editorTabWidget->setTabsClosable(false);
    editorTabWidget->setMovable(false);
    editorTabWidget->setTabPosition(QTabWidget::South);
    // Buffer selector tabs share the chrome unit with the side tab strip.
    editorTabWidget->tabBar()->setFixedHeight(ScaleHeightForDPI(SonicPi::kChromeControlDp));

    lexer->setAutoIndentStyle(SonicPiScintilla::AiMaintain);

    // create workspaces and add them to the tabs
    // workspace shortcuts
    QVBoxLayout* prefsLayout = new QVBoxLayout;
    prefsWidget = new QWidget;
    prefsWidget->setParent(this);
    prefsWidget->hide();

    settingsWidget = new SettingsWidget(m_spAPI->GetPort(SonicPiPortId::tau_osc_cues), i18n, piSettings, sonicPii18n, shortcutsConfigPath(), this);
    settingsWidget->setAccessibleName(tr("Preferences"));
    settingsWidget->setObjectName("settings");
    settingsWidget->setAttribute(Qt::WA_StyledBackground, true);
    connect(settingsWidget, SIGNAL(restartApp()), this, SLOT(restartApp()));
    connect(settingsWidget, &SettingsWidget::shortcutsApplyRequested, this, &MainWindow::applyUserShortcuts);
    connect(settingsWidget, &SettingsWidget::shortcutSchemeChanged, this, &MainWindow::shortcutModeMenuChanged);
    connect(settingsWidget, SIGNAL(volumeChanged(int)), this, SLOT(changeSystemPreAmp(int)));
    connect(settingsWidget, SIGNAL(mixerSettingsChanged()), this, SLOT(mixerSettingsChanged()));
    connect(settingsWidget, SIGNAL(enableScsynthInputsChanged()), this, SLOT(changeEnableScsynthInputs()));
    connect(settingsWidget, SIGNAL(midiSettingsChanged()), this, SLOT(toggleMidi()));
    connect(settingsWidget, SIGNAL(gamepadSettingsChanged()), this, SLOT(toggleGamepad()));
    connect(settingsWidget, &SettingsWidget::midiPortEnabledChanged, this, &MainWindow::setMidiPortEnabled);
    connect(settingsWidget, &SettingsWidget::gamepadDeviceEnabledChanged, this, &MainWindow::setGamepadDeviceEnabled);
    connect(settingsWidget, SIGNAL(oscSettingsChanged()), this, SLOT(toggleOSCServer()));
    // Slider drives only the Link mesh reach; the OSC bind scope stays
    // on the prefs IO checkboxes.
    if (auto* lasw = metroPane->findChild<LinkAudioStreamsWidget*>()) {
        connect(lasw, &LinkAudioStreamsWidget::requestNetworkVisibilityChange,
                this, [this](int mode) {
                    if (mode != 1 && mode != 2) return;
                    gui_settings->setValue("supersonic/networkVisibility", mode);
                    metroPane->onSupersonicNetworkVisibilityChanged(mode);
                });
        connect(lasw, &LinkAudioStreamsWidget::linkAudioStreamsChanged,
                this, [this](const QStringList& peers, const QStringList& channels) {
                    if (autocomplete) autocomplete->updateLinkAudioStreams(peers, channels);
                });
    }
    connect(settingsWidget, SIGNAL(showLineNumbersChanged()), this, SLOT(changeShowLineNumbers()));
    connect(settingsWidget, SIGNAL(showAutoCompletionChanged()), this, SLOT(changeShowAutoCompletion()));
    connect(settingsWidget, SIGNAL(showCompletionHelpChanged()), this, SLOT(changeShowCompletionHelp()));
    connect(settingsWidget, SIGNAL(showLogChanged()), this, SLOT(updateLogVisibility()));
    connect(settingsWidget, SIGNAL(showCuesChanged()), this, SLOT(updateCuesVisibility()));
    connect(settingsWidget, SIGNAL(showMetroChanged()), this, SLOT(updateMetroVisibility()));
    connect(settingsWidget, SIGNAL(showButtonsChanged()), this, SLOT(updateButtonVisibility()));
    connect(settingsWidget, SIGNAL(showEditorToolbarChanged()), this, SLOT(updateEditorToolbarVisibility()));
    connect(settingsWidget, SIGNAL(showFullscreenChanged()), this, SLOT(updateFullScreenMode()));
    connect(settingsWidget, SIGNAL(showTabsChanged()), this, SLOT(updateTabsVisibility()));
    connect(settingsWidget, SIGNAL(logAutoScrollChanged()), this, SLOT(updateLogAutoScroll()));
    // The full re-theme (stylesheet regen + repolish of every widget) is heavy
    // and must not run while an input is being dragged (e.g. the hue dial), or
    // the UI thread stutters. This timer debounces it off the input path: each
    // change restarts it, so during a continuous drag nothing re-themes; the
    // single apply fires once the dial settles (and reads the latest values).
    themeApplyTimer = new QTimer(this);
    themeApplyTimer->setSingleShot(true);
    themeApplyTimer->setInterval(60);
    connect(themeApplyTimer, &QTimer::timeout, this, [this]() { updateColourTheme(); });
    connect(settingsWidget, &SettingsWidget::themeChanged, this, [this]() {
        // The theme buttons emit clicked() (and thus this signal) even when
        // the already-active theme is clicked; updateSettings() has run first
        // and written the scheme choice, so compare against the last applied
        // scheme to treat only real changes as an explicit pick. The icon set
        // is an independent axis (see applyOSContrastPreference): toggling it
        // still re-themes below but is not a theme pick, so it must not
        // disturb the OS-contrast restore state.
        if (!themeEverApplied
            || static_cast<int>(piSettings->colourScheme) != appliedColourScheme)
            noteExplicitThemeChoice();
        themeApplyTimer->start();   // debounce: only re-theme once the input settles
    });
    connect(settingsWidget, SIGNAL(scopeChanged()), this, SLOT(scope()));
    connect(settingsWidget, SIGNAL(scopeChanged(QString)), this, SLOT(changeScopeKindVisibility(QString)));
    connect(settingsWidget, SIGNAL(scopeLabelsChanged()), this, SLOT(changeScopeLabels()));
    connect(settingsWidget, SIGNAL(titlesChanged()), this, SLOT(changeTitleVisibility()));
    connect(settingsWidget, SIGNAL(hideMenuBarInFullscreenChanged()), this, SLOT(changeMenuBarInFullscreenVisibility()));
    connect(settingsWidget, SIGNAL(transparencyChanged(int)), this, SLOT(changeGUITransparency(int)));

    connect(settingsWidget, SIGNAL(checkUpdatesChanged()), this, SLOT(update_check_updates()));
    connect(settingsWidget, SIGNAL(forceCheckUpdates()), this, SLOT(check_for_updates_now()));
    connect(settingsWidget, SIGNAL(showContextChanged()), this, SLOT(changeShowContext()));
    connect(settingsWidget, SIGNAL(flashSettingsChanged()), this, SLOT(changeFlashSettings()));
    connect(settingsWidget, SIGNAL(speakTransportChanged()), this, SLOT(changeSpeakTransport()));
    connect(settingsWidget, SIGNAL(reduceMotionChanged()), this, SLOT(changeReduceMotion()));
    connect(settingsWidget, SIGNAL(checkArgsChanged()), this, SLOT(changeAudioSafeMode()));
    connect(settingsWidget, SIGNAL(synthTriggerTimingGuaranteesChanged()), this, SLOT(changeAudioTimingGuarantees()));
    connect(settingsWidget, SIGNAL(enableExternalSynthsChanged()), this, SLOT(changeEnableExternalSynths()));
    connect(settingsWidget, SIGNAL(midiDefaultChannelChanged()), this, SLOT(changeMidiDefaultChannel()));
    connect(settingsWidget, SIGNAL(logCuesChanged()), this, SLOT(changeLogCues()));
    connect(settingsWidget, SIGNAL(logSynthsChanged()), this, SLOT(changeLogSynths()));
    connect(settingsWidget, SIGNAL(clearOutputOnRunChanged()), this, SLOT(changeClearOutputOnRun()));
    connect(settingsWidget, SIGNAL(autoIndentOnRunChanged()), this, SLOT(changeAutoIndentOnRun()));

    connect(settingsWidget, SIGNAL(driverChanged(QString)), this, SLOT(switchAudioDriver(QString)));
    connect(settingsWidget, SIGNAL(audioOutputDeviceChanged(QString)), this, SLOT(switchAudioDevice(QString)));
    connect(settingsWidget, SIGNAL(audioInputDeviceChangedSignal(QString)), this, SLOT(switchAudioInputDevice(QString)));
    connect(settingsWidget, SIGNAL(sampleRateChanged(int)), this, SLOT(changeSampleRate(int)));
    connect(settingsWidget, SIGNAL(bufferSizeChanged(int)), this, SLOT(changeBufferSize(int)));
    connect(this, SIGNAL(settingsChanged()), settingsWidget, SLOT(settingsChanged()));
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    connect(settingsWidget, SIGNAL(recordingModeChangedFromPrefs(int)),
            this, SLOT(setRecordingMode(int)));
#endif

    scopeWindow = new ScopeWindow(m_spClient, m_spAPI, this);

    connect(m_spClient.get(), &SonicPi::QtAPIClient::FlashReceived, this,
            [this](const QString& workspace, int line) {
                if (!piSettings->flash_code && !piSettings->flash_gutter)
                    return;
                // Only flash real editor buffers — tutorial/jukebox runs use
                // other workspace names and filenameToWorkspace would otherwise
                // fall back to buffer 0 and flash the wrong tab.
                if (!workspace.startsWith("workspace_"))
                    return;
                // The spider sends flashes at scheduled-render time; delay by
                // the device output latency so the pulse lands with the sound.
                auto apply = [this, workspace, line]() {
                    // Runtime lines are 1-based; Scintilla lines are 0-based.
                    // flashRunLine maps the run-time line through edit-tracking
                    // handles so it lands correctly after live edits.
                    SonicPiScintilla* ws = filenameToWorkspace(workspace.toStdString());
                    if (ws)
                        ws->flashRunLine(line - 1, piSettings->flash_code, piSettings->flash_gutter);
                };
                if (m_visualLatencyMs > 0)
                    QTimer::singleShot(m_visualLatencyMs, this, apply);
                else
                    apply();
            });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::LiveLoopScopeReceived, this,
            [this](int, const QString& name, const QString& workspace, int line, int scopeNum) {
                // Same guard as the flash: only pin scopes to real editor buffers.
                if (!piSettings->show_loop_scopes || !workspace.startsWith("workspace_") || !m_spAPI)
                    return;
                SonicPiScintilla* ws = filenameToWorkspace(workspace.toStdString());
                if (ws)
                    ws->setLiveLoopScope(name, line - 1,
                                         m_spAPI->AudioProcessor_GetScopeReader((unsigned int)scopeNum));
            });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::LiveLoopScopeEndedReceived, this,
            [this](int, const QString& name) {
                // Loop names are global, so clear the widget wherever it lives.
                for (int i = 0; i < editorTabWidget->count(); i++)
                    ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace()->endLiveLoopScope(name);
            });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::AudioDevicesReceived,
            this, &MainWindow::updateAudioDevices);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::AudioInputDevicesReceived,
            this, &MainWindow::updateAudioInputDevices);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::AudioDeviceConfigReceived,
            this, &MainWindow::updateAudioDeviceConfig);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::SupersonicSetupReceived,
            this, &MainWindow::onSupersonicSetup);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::SpiderReadyReceived,
            this, &MainWindow::onSpiderReady);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::AudioSwitchDoneReceived,
            this, &MainWindow::onAudioSwitchDone);

    scopeWindow->Pause();
    scopeWindow->setObjectName("scopes");

    restoreScopeState(scopeWindow->GetScopeCategories());
    settingsWidget->updateScopeNames(scopeWindow->GetScopeCategories());

    QHBoxLayout* prefsLabelLayout = new QHBoxLayout;
    QLabel* prefsLabel = new QLabel(tr("Preferences"));
    prefsLabelLayout->addStretch(1);
    prefsLabelLayout->addWidget(prefsLabel);
    prefsLabelLayout->addStretch(1);
    prefsLayout->addLayout(prefsLabelLayout);
    prefsLayout->addWidget(settingsWidget, 2);
    QHBoxLayout* prefsButtonLayout = new QHBoxLayout;
    QPushButton* prefsHidePushButton = new QPushButton(tr("Close"));
    prefsHidePushButton->setToolTip(tr("Close the preferences panel."));
    prefsHidePushButton->setObjectName("prefsHideButton"); // padding in app.qss
    prefsButtonLayout->setContentsMargins(0, ScaleHeightForDPI(6), ScaleWidthForDPI(10), ScaleHeightForDPI(8));
    prefsButtonLayout->addStretch(1);
    prefsButtonLayout->addWidget(prefsHidePushButton);
    prefsLayout->addLayout(prefsButtonLayout);
    prefsWidget->setObjectName("prefs");
    prefsWidget->setLayout(prefsLayout);
    prefsWidget->setMinimumHeight(qMax(settingsWidget->height(), settingsWidget->sizeHint().height()) + ScaleHeightForDPI(240));
    prefsWidget->setMinimumWidth(qMax(settingsWidget->width(), settingsWidget->sizeHint().width()) + ScaleWidthForDPI(200));
    QSizePolicy prefsSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
    prefsWidget->setSizePolicy(prefsSizePolicy);

    connect(prefsHidePushButton, &QPushButton::clicked, this, [=]() {
        togglePrefs();
    });

    bool auto_indent = piSettings->auto_indent_on_run;
    // Shared across all ten editor ctors so the bindings ini is parsed once.
    QSettings keyBindings(QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net",
                          "v5-scintilla-key-bindings");
    for (int ws = 0; ws < workspace_max; ws++)
    {
        std::string s;
        QString fileName = QString("workspace_") + QString::fromStdString(number_name(ws));

        // TODO: this is only here to ensure auto_indent_on_run is
        //       initialised before using it to construct the
        //       workspaces. Strongly consider how to clean this up in a way
        //       that nicely scales for more properties such as this.  This
        //       should only be considered an interim solution necessary to
        //       fix the return issue on Japanese keyboards.

        SonicPiScintilla* workspace = new SonicPiScintilla(lexer, theme, fileName, auto_indent, &keyBindings);
        workspace->setAudioApi(m_spAPI.get());
        connect(workspace,
            &SonicPiScintilla::bufferNewlineAndIndent,
            this,
            [this](int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName) {
                m_spAPI->BufferNewLineAndIndent(point_line, point_index, first_line, code, fileName);
            });

        workspace->setObjectName(QString("Buffer %1").arg(ws));
        workspace->setPlaceholderText(tr(
            "# Welcome to Sonic Pi\n"
            "#\n"
            "# Type a line and press Run to hear it. Try:  play 60"));

        // tab completion when in list
        auto indentLine = new QShortcut(QKeySequence(Qt::Key_Tab), workspace);
        // WidgetShortcut: a window-context Tab would swallow focus traversal app-wide
        indentLine->setContext(Qt::WidgetShortcut);

        connect(indentLine, &QShortcut::activated, this, [this, workspace]() {
            completeSnippetListOrIndentLine(workspace);
        });

        // escape

        QString w = QString(tr("| %1 |")).arg(QString::number(ws));
        workspaces[ws] = workspace;
        workspace->setAccessibleName(tr("Code Editor Buffer %1").arg(ws));
        SonicPiEditor* editor = new SonicPiEditor(workspace, theme, this);
        editor->getContext()->setAccessibleName(tr("Run Context"));
        editorTabWidget->setTabToolTip(editorTabWidget->addTab(editor, w),
                                       tr("Code buffer %1. All buffers are saved automatically.").arg(ws));

        connect(workspace, SIGNAL(cursorPositionChanged(int, int)), this, SLOT(updateContext(int, int)));
        connect(workspace, &SonicPiScintilla::zoomLevelChanged, this, &MainWindow::updateErrorCardZoom);
        connect(workspace, &SonicPiScintilla::docsRequested, this,
                [this](const QString& name) { showHelpForKeyword(name); });
        connect(workspace, &SonicPiScintilla::announceRequested, this,
                [this](const QString& msg) { announce(msg, true, SonicPi::Announcement::Navigation); });
        connect(workspace, &SonicPiScintilla::auditionRequested, this,
                [this](const QString& code) {
                    m_spAPI->RunCode(prefWrappedCode(code).toStdString(),
                                     "sonic-pi-autocomplete-preview", true);
                });
        // Append the idiomatic code actions to the editor's right-click menu. The
        // lambda runs when the menu is shown, so the actions already exist by then.
        connect(workspace, &SonicPiScintilla::extendContextMenu, this, [this](QMenu* menu) {
            if (!contextHelpAct) return;
            menu->addSeparator();
            menu->addAction(contextHelpAct);   // Show Docs for Current Word
            menu->addAction(textCommentAct);   // Comment / Uncomment
            menu->addAction(textAlignAct);     // Auto-align / indent
        });
    }

    connect(editorTabWidget, &QTabWidget::currentChanged, this, [this](int index) {
        focusEditor();
        // Errors and transport events are announced explicitly; do the same
        // for buffer switches rather than relying on every screen reader
        // noticing the programmatic focus move into the newly-current editor.
        // (Polite, so it queues behind any focus speech instead of cutting in.)
        if (bootAnnouncementsReady && index >= 0)
            announce(tr("Buffer %1").arg(index), false, SonicPi::Announcement::Navigation);
    });


    QFont font("Hack", 10);
    font.setStyleHint(QFont::Monospace);
#ifdef Q_OS_WIN
    font.setStyleStrategy(QFont::PreferAntialias);
    font.setHintingPreference(QFont::PreferFullHinting);
#endif
    lexer->setDefaultFont(font);

    autocomplete = new ScintillaAPI(lexer);
    // Let `play` completion show only the active synth's opts by resolving the
    // in-effect use_synth from the focused buffer at completion time.
    autocomplete->setSynthResolver([this]() { return currentSynthForCompletion(); });
    // adding universal shortcuts to outputpane seems to
    // steal events from doc system!?
    // addUniversalCopyShortcuts(outputPane);

    addUniversalCopyShortcuts(errorPane);
    outputPane->setReadOnly(true);
    outputPane->setLineWrapMode(QPlainTextEdit::NoWrap);
    outputPane->setFontFamily("Hack");

    incomingPane->setReadOnly(true);
    incomingPane->setLineWrapMode(QPlainTextEdit::NoWrap);
    incomingPane->setFontFamily("Hack");

    errorPane->setReadOnly(true);

    if (!theme->font("LogFace").isEmpty())
    {
        outputPane->setFontFamily(theme->font("LogFace"));
        incomingPane->setFontFamily(theme->font("LogFace"));
    }

    outputPane->document()->setMaximumBlockCount(1000);
    incomingPane->document()->setMaximumBlockCount(1000);
    errorPane->document()->setMaximumBlockCount(1000);

    outputPane->setTextColorKey(theme, "LogForeground");
    outputPane->appendPlainText("\n");

    incomingPane->setTextColor(QColor(theme->color("LogForeground")));
    incomingPane->appendPlainText("\n");

    errorPane->zoomIn(1);
    errorPane->setFixedHeight(ScaleHeightForDPI(200));
    // hudPane = new QTextBrowser;
    // hudPane->setMinimumHeight(130);
    // hudPane->setHtml("<center><img src=\":/images/logo.png\" height=\"113\" width=\"138\"></center>");
    // hudWidget = new QDockWidget(this);
    // hudWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
    // hudWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    // hudWidget->setTitleBarWidget(new QWidget());
    // addDockWidget(Qt::RightDockWidgetArea, hudWidget);
    // hudWidget->setWidget(hudPane);
    // hudWidget->setObjectName("hud");

    scopeWidget = new QDockWidget(tr("Scope"), this);
    scopeWidget->setFocusPolicy(Qt::NoFocus);
    scopeWidget->setAllowedAreas(Qt::RightDockWidgetArea | Qt::BottomDockWidgetArea | Qt::TopDockWidgetArea);
    scopeWidget->setFeatures(QDockWidget::DockWidgetClosable | QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);
    scopeWidget->setWidget(scopeWindow);
    scopeWidget->setObjectName("Scope");
    scopeWidget->setMinimumHeight(ScaleHeightForDPI(100));
    addDockWidget(Qt::RightDockWidgetArea, scopeWidget);

    // Scope dock title row: SCOPE + the pause/resume toggle, so the control sits
    // beside the title rather than floating over the trace.
    QWidget* scopePauseButton = scopeWindow->PauseButton();
    scopePauseButton->setFixedSize(ScaleForDPI(26, 20));
    scopeWidget->setTitleBarWidget(
        makeControlTitleBar(scopeWidget->windowTitle(), titleBarScope, { scopePauseButton }));

    connect(scopeWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(scopeVisibilityChanged()));

    outputWidget = new QDockWidget(tr("Log"), this);
    outputWidget->setFocusPolicy(Qt::NoFocus);
    outputWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
    outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    outputWidget->setWidget(outputPane);

    incomingWidget = new QDockWidget(tr("Cues"), this);
    incomingWidget->setFocusPolicy(Qt::NoFocus);
    incomingWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
    incomingWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    incomingWidget->setWidget(incomingPane);

    metroWidget = new QDockWidget(tr("Link Metronome & Global Time Warp"), this);
    metroWidget->setFocusPolicy(Qt::NoFocus);
    metroWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
    metroWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    metroWidget->setMaximumHeight(ScaleHeightForDPI(110));
    metroWidget->setWidget(metroPane);
    // Let the dock grow when the streams panel expands, shrink on collapse.
    connect(metroPane, &SonicPiMetro::linkAudioStreamsExpandedChanged, this,
            [this](bool expanded) {
                metroWidget->setMaximumHeight(
                    expanded ? QWIDGETSIZE_MAX : ScaleHeightForDPI(110));
            });

    addDockWidget(Qt::RightDockWidgetArea, outputWidget);
    addDockWidget(Qt::RightDockWidgetArea, incomingWidget);
    addDockWidget(Qt::RightDockWidgetArea, metroWidget);

    outputWidget->setObjectName("output");
    incomingWidget->setObjectName("input");
    metroWidget->setObjectName("metro");

    blankWidgetOutput = new QWidget();
    blankWidgetIncoming = new QWidget();
    blankWidgetMetro = new QWidget();

    docsNavTabs = new QTabWidget;
    docsNavTabs->setObjectName("docsNavTabs");   // chip-style tabs, see app.qss
    docsNavTabs->setFocusPolicy(Qt::NoFocus);
    // Landing on a docs tab should always show something: select the first
    // entry if the tab has never had a selection (switching back keeps the
    // previous selection). NB the initial tab's signal fires before its list
    // exists, so every show-the-docs path also calls ensureDocsSelection().
    connect(docsNavTabs, &QTabWidget::currentChanged, this,
            [this](int) { ensureDocsSelection(); });
    docsNavTabs->setTabsClosable(false);
    docsNavTabs->setMovable(false);
    // North: the section chips head the column; at the bottom they read as
    // disconnected from the list they drive.
    docsNavTabs->setTabPosition(QTabWidget::North);
    QShortcut* left = new QShortcut(Qt::Key_Left, docsNavTabs);
    left->setContext(Qt::WidgetWithChildrenShortcut);
    connect(left, SIGNAL(activated()), this, SLOT(docPrevTab()));
    QShortcut* right = new QShortcut(Qt::Key_Right, docsNavTabs);
    right->setContext(Qt::WidgetWithChildrenShortcut);
    connect(right, SIGNAL(activated()), this, SLOT(docNextTab()));

    tutorialPane = new TutorialPane(lexer, theme);
    tutorialPane->setAudioApi(m_spAPI);
    tutorialPane->setUserZoom(gui_settings->value("prefs/docs-zoom", 0).toInt());

    // Stack rather than a third splitter pane: QSplitter restores persisted
    // sizes, which can leave a late-added widget at zero width.
    QSizePolicy policy = tutorialPane->sizePolicy();
    policy.setHorizontalStretch(QSizePolicy::Maximum);
    tutorialPane->setSizePolicy(policy);
    tutorialPane->setMinimumHeight(100);

    docsplit = new ThinSplitter;
    docsplit->setHandleWidth(7);
    docsplit->addWidget(docsNavTabs);
    docsplit->addWidget(tutorialPane);

    // Chapter JSON is generated per language by qt-doc.rb with the same
    // filenames as the markdown sources (so rows align with the generated
    // titles); fall back to English.
    QDir tutorialDir(rootPath() + "/etc/doc/generated/native/en/tutorial");
    for (const QString& lang : { ui_language, ui_language.section('_', 0, 0) })
    {
        if (lang.isEmpty() || lang.startsWith("en"))
            break;
        QDir translated(rootPath() + "/etc/doc/generated/native/" + lang + "/tutorial");
        if (translated.exists() && !translated.entryList(QStringList() << "*.json", QDir::Files).isEmpty())
        {
            tutorialDir = translated;
            break;
        }
    }
    const QStringList tutorialFiles = tutorialDir.entryList(QStringList() << "*.json", QDir::Files, QDir::Name);
    for (const QString& fname : tutorialFiles)
        tutorialJsonPaths << tutorialDir.filePath(fname);

    connect(tutorialPane, &TutorialPane::runRequested, this,
            [this](const QString& code, const QString& workspace, bool silent, bool scopeTap) {
                if (!piSettings->reduce_motion)
                    scopeWindow->Resume();
                // Jukebox runs are wrapped in an fx_scope_out tap (scope_num 1,
                // matching kJukeboxScopeSlot) so the Examples scope shows only
                // this run's audio while the main scope keeps showing the mix.
                QString toRun = scopeTap
                    ? QString("with_fx :scope_out, scope_num: 1 do\n%1\nend").arg(code)
                    : code;
                m_spAPI->RunCode(prefWrappedCode(toRun).toStdString(), workspace.toStdString(), silent);
            });
    connect(tutorialPane, &TutorialPane::stopJobRequested, this,
            [this](int jobId) { m_spAPI->StopJob(jobId); });
    connect(tutorialPane, &TutorialPane::loadRequested, this, [this](const QString& code) {
        SonicPiScintilla* ws = getCurrentWorkspace();
        if (!ws)
            return;
        ws->replaceBuffer(code, 0, 0, 0); // single undo step, so it can be undone
        ws->setFocus();
        showStatusAndAnnounce(tr("Loaded example into the current buffer."), 3000);
    });
    connect(tutorialPane, &TutorialPane::announceRequested, this,
            [this](const QString& msg) { announce(msg); });
    connect(tutorialPane, &TutorialPane::linkClicked, this, &MainWindow::docLinkClicked);
    connect(tutorialPane, &TutorialPane::navigateRequested, this, [this](int delta) {
        QListWidget* list = helpLists.value((int)DocTab::Tutorial);
        if (!list)
            return;
        int row = list->currentRow() + delta;
        if (row >= 0 && row < list->count())
            list->setCurrentRow(row);
    });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunStartedReceived,
            tutorialPane, &TutorialPane::runStarted);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunEndedReceived,
            tutorialPane, &TutorialPane::runEnded);
    // Job -> workspace, so errors can tell an editor run from a help-system
    // one (help errors must never scribble markers on the editor).
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunStartedReceived, this,
            [this](int jobId, const QString& workspace) {
                m_jobWorkspaces[jobId] = workspace;
            });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunEndedReceived, this,
            [this](int jobId) { m_jobWorkspaces.remove(jobId); });

    southTabs = new IconTabWidget;
    southTabs->setObjectName("southTabs");
    southTabs->setTabPosition(QTabWidget::West);
    southTabs->setTabsClosable(false);
    southTabs->setMovable(false);
    quickstartPane = new QuickstartPane(theme);
    quickstartPane->setAudioApi(m_spAPI);
    quickstartPane->setUserZoom(gui_settings->value("prefs/quickstart-zoom", 2).toInt());
    // Cards come from an editable text file (see cardsFileToLoad).
    quickstartPane->setCardsFile(cardsFileToLoad());
    connect(quickstartPane, &QuickstartPane::runRequested, this,
            [this](const QString& title, const QString& code, const QString& workspace,
                   int scopeSlot) {
                if (!piSettings->reduce_motion)
                    scopeWindow->Resume();
                // Only live_loop cards are beat-synced: `link 1` quantises their
                // start to the next Link beat so loops drop in together and in
                // time. One-shot cards fire immediately (no wait). The `link 1;`
                // rides the wrapper's single leading line either way, so the
                // card's flash line-offset is unchanged. Wrapped in an fx
                // :scope_out tap on the card's own slot so its rings show only
                // this card's audio, isolated from the other playing cards.
                const bool sync = code.contains(QStringLiteral("live_loop"));
                const QString lead = sync ? QStringLiteral("link 1; ") : QString();
                QString toRun = QString("%1with_fx :scope_out, scope_num: %2 do\n%3\nend")
                                    .arg(lead)
                                    .arg(scopeSlot)
                                    .arg(code);
                m_spAPI->RunCode(prefWrappedCode(toRun).toStdString(), workspace.toStdString(), false);
                showStatusAndAnnounce(sync ? tr("Playing %1 on the next beat.").arg(title)
                                           : tr("Playing %1.").arg(title),
                                      3000);
            });
    connect(quickstartPane, &QuickstartPane::stopJobRequested, this,
            [this](int jobId) { m_spAPI->StopJob(jobId); });
    connect(quickstartPane, &QuickstartPane::dragEnded, this, [this] {
        // A drop on the editor already committed (dropEvent). Anything still
        // previewing here means the card was released off the editor: cancel it,
        // don't turn a fumbled drag into real code.
        for (int i = 0; i < workspace_max; i++)
            workspaces[i]->cancelInsertPreview();
    });
    connect(quickstartPane, &QuickstartPane::insertPreviewRequested, this,
            [this](const QString& title, const QString& code) {
                if (SonicPiScintilla* ws = getCurrentWorkspace())
                    ws->previewInsertAtCursor(code, title);
            });
    connect(quickstartPane, &QuickstartPane::insertPreviewCleared, this, [this] {
        // All workspaces, not just the current one: the preview went into
        // whichever buffer was current at hover time, and the user may have
        // switched buffers since (cancel is a no-op where nothing previews).
        for (int i = 0; i < workspace_max; i++)
            workspaces[i]->cancelInsertPreview();
    });
    connect(quickstartPane, &QuickstartPane::insertRequested, this,
            [this](const QString& title, const QString& code) {
                SonicPiScintilla* ws = getCurrentWorkspace();
                if (!ws)
                    return;
                // Drop any stale hover preview everywhere (it may live in
                // another buffer after a switch), then place + commit fresh in
                // the current one.
                for (int i = 0; i < workspace_max; i++)
                    workspaces[i]->cancelInsertPreview();
                ws->previewInsertAtCursor(code, title);
                ws->finaliseDropPreview();
                ws->setFocus();
                showStatusAndAnnounce(
                    title.isEmpty()
                        ? tr("Inserted the card's code at the cursor. Press Run to hear it.")
                        : tr("Inserted %1 at the cursor. Press Run to hear it.").arg(title),
                    5000);
            });
    connect(quickstartPane, &QuickstartPane::copyRequested, this,
            [this](const QString& title, const QString& code) {
                QApplication::clipboard()->setText(code);
                showStatusAndAnnounce(
                    title.isEmpty() ? tr("Copied the card's code to the clipboard.")
                                    : tr("Copied %1 to the clipboard.").arg(title),
                    5000);
            });
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunStartedReceived,
            quickstartPane, &QuickstartPane::runStarted);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::RunEndedReceived,
            quickstartPane, &QuickstartPane::runEnded);
    connect(m_spClient.get(), &SonicPi::QtAPIClient::FlashReceived, quickstartPane,
            [this](const QString& workspace, int line) {
                // Same pref gate and output-latency delay as the editor flash.
                if (!piSettings->flash_code)
                    return;
                if (m_visualLatencyMs > 0)
                    QTimer::singleShot(m_visualLatencyMs, quickstartPane,
                                       [this, workspace, line]() { quickstartPane->flashLine(workspace, line); });
                else
                    quickstartPane->flashLine(workspace, line);
            });
    southTabs->setTabToolTip(southTabs->addTab(quickstartPane, tr("Cards")),
                             tr("Quickstart cards: small runnable snippets to get going."));
    southTabs->setTabToolTip(southTabs->addTab(docsplit, "Docs"),
                             tr("Tutorial, examples and reference documentation."));
    southTabs->setAttribute(Qt::WA_StyledBackground, true);
    // Explicit minimum so the dock can always be shrunk (content scrolls):
    // the Debug tab's natural minimum is tall enough to defeat
    // stealHelpHeightForError's resizeDocks, squeezing error cards out of
    // the central area entirely.
    southTabs->setMinimumHeight(ScaleHeightForDPI(60));

    // A persistent close ✕ for the help pane, placed in the dock title row
    // (see makeControlTitleBar below) so it stays put across tabs and floats.
    // Its #helpCloseButton chip stays legible on any background; eventFilter()
    // swaps its tint on hover. (Tooltip gains the shortcut once helpAct exists.)
    helpCloseButton = new QPushButton(southTabs);
    helpCloseButton->setObjectName("helpCloseButton");
    helpCloseButton->setCursor(Qt::PointingHandCursor);
    helpCloseButton->setFocusPolicy(Qt::NoFocus);
    helpCloseButton->setAccessibleName(tr("Close the help pane"));
    connect(helpCloseButton, &QPushButton::clicked, this, &MainWindow::toggleDocPane);
    updateHelpCloseIcon();

    docWidget = new QDockWidget(tr("Help"), this);
    docWidget->setFocusPolicy(Qt::NoFocus);
    docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
    docWidget->setWidget(southTabs);
    docWidget->setObjectName("help");

    // Help dock title row: HELP + docs text-size (A-/A+) + the persistent close
    // ✕, so all three sit on the same row as the title. The row stays put
    // whether or not pane titles are shown (only the HELP label toggles).
    // Every help tab has A-/A+ text-size controls; they share the title row so
    // they line up beside the always-present close ✕. Only the current tab's
    // pair is shown. (The Logs/Debug panels are built further down, so their
    // bars are wired to the panels once those exist.)
    QWidget* docZoomControls = tutorialPane->zoomControls();
    QWidget* cardsZoomControls = quickstartPane->zoomControls();
    logsZoom = new ZoomBar(theme, tr("logs"), this);
    debugZoom = new ZoomBar(theme, tr("metrics"), this);
    docWidget->setTitleBarWidget(
        makeControlTitleBar(docWidget->windowTitle(), titleBarDoc,
                            { docZoomControls, cardsZoomControls, logsZoom, debugZoom,
                              helpCloseButton }));
    auto syncDocZoomVisible = [this, docZoomControls, cardsZoomControls]() {
        docZoomControls->setVisible(southTabs->currentWidget() == docsplit);
        cardsZoomControls->setVisible(southTabs->currentWidget() == quickstartPane);
        logsZoom->setVisible(southTabs->currentWidget() == debugLogPanel);
        debugZoom->setVisible(southTabs->currentWidget() == metricsPanel);
    };
    connect(southTabs, &QTabWidget::currentChanged, this,
            [syncDocZoomVisible](int) { syncDocZoomVisible(); });
    syncDocZoomVisible();

    addDockWidget(Qt::BottomDockWidgetArea, docWidget);
    docWidget->hide();

    // Currently causes a segfault when dragging doc pane out of main
    // window:
    connect(docWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(toggleHelpIcon()));
    // An error card must stay visible even when Help opens after the error:
    // re-steal the height once the dock's layout settles.
    connect(docWidget, &QDockWidget::visibilityChanged, this, [this](bool visible) {
        if (!visible || !errorCard || !errorCard->isVisible())
            return;
        QTimer::singleShot(0, this, [this]() {
            if (errorCard && errorCard->isVisible())
                stealHelpHeightForError(errorCard->height() > 0 ? errorCard->height()
                                                                : errorCard->sizeHint().height());
        });
    });

    mainWidgetLayout = new QVBoxLayout;
    // Fill the central area: the style's default layout margins would inset the
    // editor tab widget (and so its scrollbars) ~10px from the window edge,
    // while the dock panes (log / info) sit flush. Zero margins + spacing so
    // every pane's scrollbar shares the same edge offset.
    mainWidgetLayout->setContentsMargins(0, 0, 0, 0);
    mainWidgetLayout->setSpacing(0);
    errorCard = new SonicPiErrorCard(theme);
    connect(errorCard, &SonicPiErrorCard::jumpRequested, this, &MainWindow::jumpToError);
    connect(errorCard, &SonicPiErrorCard::closeRequested, this, &MainWindow::dismissErrorCard);
    connect(errorCard, &SonicPiErrorCard::docsRequested, this,
            [this](const QString& name) { showHelpForKeyword(name); });
    // Buffers can have different zoom levels; keep the card tracking the
    // current one.
    connect(editorTabWidget, &QTabWidget::currentChanged, this, &MainWindow::updateErrorCardZoom);

    mainWidgetLayout->addWidget(editorTabWidget);
    mainWidgetLayout->addWidget(errorPane);
    mainWidgetLayout->addWidget(errorCard);
    mainWidget = new QWidget;
    mainWidget->setFocusPolicy(Qt::NoFocus);
    errorPane->hide();
    errorCard->hide();
    mainWidget->setLayout(mainWidgetLayout);
    mainWidget->setObjectName("mainWidget");

    setCentralWidget(mainWidget);

    incomingPane->setZoomLevel(gui_settings->value("prefs/cue-zoom", 0).toInt());
    outputPane->setZoomLevel(gui_settings->value("prefs/log-zoom", 0).toInt());
}

void MainWindow::toggleDocPane()
{
    if (!docWidget)
        return;
    if (docWidget->isVisible())
    {
        m_savedDockH = docWidget->height();   // remember for re-open
        docWidget->hide();
    }
    else
    {
        docWidget->show();
        const int h = (m_savedDockH > 0) ? m_savedDockH : (height() / 3);
        resizeDocks({ docWidget }, { h }, Qt::Vertical);
        ensureDocsSelection();   // never land on a blank pane
    }
}

void MainWindow::docLinkClicked(const QUrl& url)
{
    QString link = url.toDisplayString();
    std::cout << "[GUI] Link clicked: " << link.toStdString() << std::endl;

    if (url.scheme() == "sonicpi")
    {
        handleCustomUrl(url);
    }
    else if (url.isRelative() || url.isLocalFile() || url.scheme() == "qrc")
    {
        showHelpPageForUrl(url);
    }
    else
    {
        QDesktopServices::openUrl(url);
    }
}

void MainWindow::handleCustomUrl(const QUrl& url)
{
    if (url.host() == "play-sample")
    {
        QString sample = url.path();
        sample.remove(QRegularExpression("^/"));
        QString code = "use_debug false\n"
                       "use_real_time\n"
                       "sample :"
            + sample;
        oscpkt::Message msg("/run-code");
        msg.pushInt32(guiID);
        msg.pushStr(code.toStdString());
        if (sendOSC(msg))
        {
            statusBar()->showMessage(tr("Playing Sample..."), 1000);
        }
    }
}

void MainWindow::escapeWorkspaces()
{
    errorPane->hide();
    errorCard->hide();
    returnStolenHelpHeight();

    for (int w = 0; w < workspace_max; w++)
    {
        workspaces[w]->closeFindPopup();
        workspaces[w]->escapeAndCancelSelection();
        workspaces[w]->clearLineMarkers();
    }

    getCurrentWorkspace()->setFocus();
}

void MainWindow::toggleFullScreenMode()
{
    piSettings->full_screen = !piSettings->full_screen;
    emit settingsChanged();
    updateFullScreenMode();
}

void MainWindow::fullScreenMenuChanged()
{
    piSettings->full_screen = fullScreenAct->isChecked();
    emit settingsChanged();
    updateFullScreenMode();
}

void MainWindow::shortcutModeMenuChanged(int modeID)
{

    if (modeID == 2)
    {
        piSettings->shortcut_mode = 2;
    }
    else if (modeID == 3)
    {
        piSettings->shortcut_mode = 3;
    }
    else if (modeID == 4)
    {
        piSettings->shortcut_mode = 4;
    }
    else
    {
        // default
        piSettings->shortcut_mode = 1;
    }

    emit settingsChanged();
    updateShortcuts();
}

void MainWindow::blankTitleBars()
{
    showStatusAndAnnounce(tr("Hiding pane titles..."), 2000);
    outputWidget->setTitleBarWidget(blankWidgetOutput);
    incomingWidget->setTitleBarWidget(blankWidgetIncoming);
    metroWidget->setTitleBarWidget(blankWidgetMetro);
    // Scope & Help keep their persistent control rows; hide just the label so
    // the pause / A-/A+ / close controls stay reachable.
    if (titleBarScope) titleBarScope->hide();
    if (titleBarDoc)   titleBarDoc->hide();
    if (metricsPanel) metricsPanel->setTitlesVisible(false);
}

void MainWindow::namedTitleBars()
{
    showStatusAndAnnounce(tr("Showing pane titles..."), 2000);

    // Custom title-bar labels styled like the SuperSonic debug pane titles
    // (small/muted/left, uppercase). QDockWidget::title's QSS colour isn't
    // honoured for the title text, so we supply our own #paneTitle labels.
    // Created lazily here (all docks exist by now).
    auto makeDockTitle = [](QDockWidget* dock) {
        auto* l = new QLabel(dock->windowTitle().toUpper());
        l->setObjectName("paneTitle");
        l->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        return l;
    };
    if (!titleBarOutput)   titleBarOutput   = makeDockTitle(outputWidget);
    if (!titleBarIncoming) titleBarIncoming = makeDockTitle(incomingWidget);
    if (!titleBarMetro)    titleBarMetro    = makeDockTitle(metroWidget);

    outputWidget->setTitleBarWidget(titleBarOutput);
    incomingWidget->setTitleBarWidget(titleBarIncoming);
    metroWidget->setTitleBarWidget(titleBarMetro);
    // Scope & Help keep their persistent control rows; just reveal the label.
    if (titleBarScope) titleBarScope->show();
    if (titleBarDoc)   titleBarDoc->show();
    if (metricsPanel) metricsPanel->setTitlesVisible(true);
}

QWidget* MainWindow::makeControlTitleBar(const QString& title, QLabel*& outLabel,
                                         const QVector<QWidget*>& controls)
{
    QWidget* bar = new QWidget();
    bar->setObjectName("dockTitleBar");
    bar->setAttribute(Qt::WA_StyledBackground, true);
    QHBoxLayout* layout = new QHBoxLayout(bar);
    layout->setContentsMargins(ScaleWidthForDPI(6), 0, ScaleWidthForDPI(6), 0);
    layout->setSpacing(ScaleWidthForDPI(4));
    outLabel = new QLabel(title.toUpper(), bar);
    outLabel->setObjectName("paneTitle");
    outLabel->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    layout->addWidget(outLabel);
    layout->addStretch(1);
    for (QWidget* c : controls)
        layout->addWidget(c, 0, Qt::AlignVCenter);
    return bar;
}

void MainWindow::updateFullScreenMode()
{
    QSignalBlocker blocker(fullScreenAct);
    fullScreenAct->setChecked(piSettings->full_screen);

    if (piSettings->full_screen && !fullScreenMode)
    {
        // switch to full screen mode
        std::cout << "[GUI] - switch into full screen mode." << std::endl;

#if defined(Q_OS_WIN)

        QRect rect = this->geometry();
        m_appWindowSizeRect.reset(new QRect(rect));
        QRect screenRect = this->screen()->availableGeometry();
        this->setGeometry(screenRect.x() - 1, screenRect.y() - 1, screenRect.width() + 2, screenRect.height() + 2);
        this->setWindowFlags(Qt::FramelessWindowHint);
#else
        this->showFullScreen();
#endif
        if (!quietFullScreenChange)
        {
            showStatusAndAnnounce(tr("Full screen mode on."));
        }
        fullScreenMode = true;
    }
    else if (!piSettings->full_screen && fullScreenMode)
    {
        // switch out of full screen mode
        std::cout << "[GUI] - switch out of full screen mode." << std::endl;
        menuBar()->show();
#ifdef Q_OS_WIN
        this->setWindowFlags(Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowMinimizeButtonHint | Qt::WindowMaximizeButtonHint | Qt::WindowCloseButtonHint);
        this->setWindowFlags(windowFlags() & ~Qt::FramelessWindowHint);
        this->setGeometry(*m_appWindowSizeRect.get());
        this->setWindowState((this->windowState() & ~Qt::WindowMinimized) | Qt::WindowActive);

#else
        this->showNormal();
#endif

        if (!quietFullScreenChange)
        {
            showStatusAndAnnounce(tr("Full screen mode off."), 2000);
        }
        fullScreenMode = false;
    }
    changeMenuBarInFullscreenVisibility();
    this->show();
}

void MainWindow::toggleFocusMode()
{
    focusMode = !focusMode;
    updateFocusMode();
}

void MainWindow::updateFocusMode()
{
    if (focusMode)
    {
        preFocus.fullScreen = piSettings->full_screen;
        preFocus.tabs = piSettings->show_tabs;
        preFocus.buttons = piSettings->show_buttons;
        preFocus.log = piSettings->show_log;
        preFocus.cues = piSettings->show_cues;
        preFocus.metro = piSettings->show_metro;
        preFocus.scopes = piSettings->show_scopes;
        preFocus.docs = docWidget->isVisible();

        piSettings->full_screen = true;
        piSettings->show_tabs = false;
        piSettings->show_buttons = false;
        piSettings->show_log = false;
        piSettings->show_cues = false;
        piSettings->show_metro = false;
        piSettings->show_scopes = false;
        docWidget->hide();
    }
    else
    {
        piSettings->full_screen = preFocus.fullScreen;
        piSettings->show_tabs = preFocus.tabs;
        piSettings->show_buttons = preFocus.buttons;
        piSettings->show_log = preFocus.log;
        piSettings->show_cues = preFocus.cues;
        piSettings->show_metro = preFocus.metro;
        piSettings->show_scopes = preFocus.scopes;
        if (preFocus.docs)
        {
            docWidget->show();
        }
    }
    focusModeAct->setChecked(focusMode);
    // Quiet the fullscreen transition below: its message would clobber the
    // exit hint and stack a second announcement.
    quietFullScreenChange = true;
    emit settingsChanged();
    updateFullScreenMode();
    quietFullScreenChange = false;
    updateTabsVisibility();
    updateButtonVisibility();
    updateLogVisibility();
    updateCuesVisibility();
    updateMetroVisibility();
    scope();
    if (focusMode)
    {
        showStatusAndAnnounce(tr("Focus mode on. Press %1 to exit.")
                                  .arg(focusModeAct->shortcut().toString(QKeySequence::NativeText)),
                              5000);
    }
    else
    {
        showStatusAndAnnounce(tr("Focus mode off."));
    }
}

void MainWindow::toggleScopePaused()
{
    scopeWindow->TogglePause();
}

void MainWindow::allJobsCompleted()
{
    // Deferred: the scope keeps drawing until tails ring out and the
    // spectrum decays, then pauses itself (no idle CPU while silent).
    scopeWindow->PauseWhenSilent();

    // re-enable log text selection
    // Keyboard-selectable too, so a screen reader can move a caret through the
    // log and read it (mouse-only selection isn't navigable by keyboard).
    incomingPane->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
    outputPane->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
}

void MainWindow::toggleLogVisibility()
{
    piSettings->show_log = !piSettings->show_log;
    emit settingsChanged();
    updateLogVisibility();
}

void MainWindow::toggleCuesVisibility()
{
    piSettings->show_cues = !piSettings->show_cues;
    emit settingsChanged();
    updateCuesVisibility();
}

void MainWindow::updateLogVisibility()
{
    QSignalBlocker blocker(showLogAct);
    showLogAct->setChecked(piSettings->show_log);

    if (piSettings->show_log)
    {
        outputWidget->show();
    }
    else
    {
        outputWidget->hide();
    }
}

void MainWindow::showCuesMenuChanged()
{
    piSettings->show_cues = showCuesAct->isChecked();
    emit settingsChanged();
    updateCuesVisibility();
}

void MainWindow::showMetroChanged()
{
    piSettings->show_metro = showMetroAct->isChecked();
    emit settingsChanged();
    updateMetroVisibility();
}

#ifdef Q_OS_MAC
void MainWindow::syphonPublishMenuChanged()
{
    const bool wantOn = syphonPublishAct->isChecked();
    if (wantOn) {
        // winId() is an NSView* on macOS; the publisher walks to NSWindow.
        WId wid = this->winId();
        bool started = SonicPi::startWindowSyphonPublishing(
            reinterpret_cast<void*>(wid), "Sonic Pi",
            piSettings->syphon_show_cursor);
        if (!started) {
            QSignalBlocker blocker(syphonPublishAct);
            syphonPublishAct->setChecked(false);
        }
        // Menu stays optimistically checked while the async setup
        // negotiates permission; live state is in isSyphonPublishing().
    } else {
        SonicPi::stopWindowSyphonPublishing();
    }
}

void MainWindow::syphonShowCursorMenuChanged()
{
    piSettings->syphon_show_cursor = syphonShowCursorAct->isChecked();
    emit settingsChanged();
    SonicPi::setSyphonShowCursor(piSettings->syphon_show_cursor);
}
#endif

#ifdef Q_OS_WIN
void MainWindow::spoutPublishMenuChanged()
{
    const bool wantOn = spoutPublishAct->isChecked();
    if (wantOn) {
        // winId() is the HWND on Windows.
        WId wid = this->winId();
        bool started = SonicPi::startWindowSpoutPublishing(
            reinterpret_cast<void*>(wid), "Sonic Pi",
            piSettings->spout_show_cursor);
        if (!started) {
            QSignalBlocker blocker(spoutPublishAct);
            spoutPublishAct->setChecked(false);
        }
    } else {
        SonicPi::stopWindowSpoutPublishing();
    }
}

void MainWindow::spoutShowCursorMenuChanged()
{
    piSettings->spout_show_cursor = spoutShowCursorAct->isChecked();
    emit settingsChanged();
    SonicPi::setSpoutShowCursor(piSettings->spout_show_cursor);
}
#endif

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
// Fixed scsynth node id for the supersonic-audio-out synth that feeds
// the screen recorder's audio track. Chosen high enough to be outside
// the range Sonic Pi normally allocates for user code.
static constexpr int32_t kRecordAudioOutNodeId = 999100;

// /s_new + /n_free for the supersonic-audio-out synth that feeds the
// session recorder's audio track. Called from the start/rollback paths
// in toggleRecording, the stop path, and onExitCleanup.
void MainWindow::spawnRecordAudioOutSynth()
{
    if (!m_spAPI) return;
    oscpkt::Message snew("/s_new");
    snew.pushStr("supersonic-audio-out");
    snew.pushInt32(kRecordAudioOutNodeId);
    snew.pushInt32(1);  // addAction = TAIL
    snew.pushInt32(0);  // targetGroup = root — reads bus 0 after the
                        // mixer has written this cycle's mix.
    m_spAPI->SupersonicSendOSC(snew);
}

void MainWindow::freeRecordAudioOutSynth()
{
    if (!m_spAPI) return;
    oscpkt::Message nfree("/n_free");
    nfree.pushInt32(kRecordAudioOutNodeId);
    m_spAPI->SupersonicSendOSC(nfree);
}

// Pops the IO → Recording Mode actions as a context menu, reusing the
// same QActions (and QActionGroup) so the right-click and the menubar
// stay in lock-step.
void MainWindow::showRecordingModeMenu(const QPoint& pos)
{
    QWidget* anchor = qobject_cast<QWidget*>(sender());
    if (!anchor || !recAudioModeAct || !recAudioVideoModeAct) return;
    QMenu menu(this);
    menu.addAction(recAudioModeAct);
    menu.addAction(recAudioVideoModeAct);
    menu.exec(anchor->mapToGlobal(pos));
}

// Single funnel for mode changes from the menubar, right-click, or
// Preferences. An in-flight recording is unaffected — toggleRecording
// uses m_videoTempPath, not the live setting, on the stop side.
void MainWindow::setRecordingMode(int mode)
{
    const auto newMode = static_cast<SonicPiSettings::RecordingType>(mode);
    if (newMode == piSettings->recording_type) return;
    piSettings->recording_type = newMode;
    if (recAudioModeAct && recAudioVideoModeAct) {
        if (newMode == SonicPiSettings::AudioAndVideo) {
            recAudioVideoModeAct->setChecked(true);
        } else {
            recAudioModeAct->setChecked(true);
        }
    }
    emit settingsChanged();
}

void MainWindow::recordFlashIconMenuChanged()
{
    piSettings->record_flash_icon = recordFlashIconAct->isChecked();
    emit settingsChanged();
    // Snap an in-flight indicator to the appropriate state — stopping
    // the timer mid-blink could leave it on the "off" frame.
    if (is_recording) {
        if (piSettings->record_flash_icon) {
            rec_flash_timer->start(500);
        } else {
            rec_flash_timer->stop();
            recAct->setIcon(theme->getRecIcon(true, true));
        }
    }
}

void MainWindow::recordShowCursorMenuChanged()
{
    piSettings->record_show_cursor = recordShowCursorAct->isChecked();
    emit settingsChanged();
    SonicPi::setRecordShowCursor(piSettings->record_show_cursor);
}
#endif

void MainWindow::showLogMenuChanged()
{
    piSettings->show_log = showLogAct->isChecked();
    emit settingsChanged();
    updateLogVisibility();
}

void MainWindow::updateCuesVisibility()
{
    QSignalBlocker blocker(showCuesAct);
    showCuesAct->setChecked(piSettings->show_cues);

    if (piSettings->show_cues)
    {
        incomingWidget->show();
    }
    else
    {
        incomingWidget->hide();
    }
}

void MainWindow::createDebugAndLogTabs()
{
    if (debugLogPanel)
        return;   // already created — these tabs are always present

    QVector<LogPanel::Source> sources;
    for (const auto& src : m_spAPI->GetLogSources())
    {
        sources.append({ QString::fromStdString(src.name),
                         QString::fromStdString(src.path.string()) });
    }
    debugLogPanel = new LogPanel(sources, this);
    debugLogPanel->applyTheme(theme->color("LogForeground"),
                              theme->color("LogBackground"));

    // Live SuperSonic panel (metrics + OSC in/out + debug + node tree), read
    // from the engine's shared segment. A top-level tab, sibling of Logs and
    // Docs (it starts/stops polling on show/hide).
    metricsPanel = new MetricsPanel(m_spAPI, this);
    metricsPanel->applyTheme(theme);

    // Top-level south tabs in the order Docs, Logs, Debug (Docs was added at
    // construction, so append these after it).
    southTabs->setTabToolTip(southTabs->addTab(debugLogPanel, tr("Logs")),
                             tr("A live view of Sonic Pi's log files."));
    southTabs->setTabToolTip(southTabs->addTab(metricsPanel, tr("Debug")),
                             tr("Live metrics, node tree and message logs for the SuperSonic audio engine."));
    southTabs->setCurrentWidget(metricsPanel);

    // Wire the title-row A-/A+ bars (built earlier) to the Logs/Debug panels,
    // restoring the saved level and persisting each step.
    debugLogPanel->setFontZoom(gui_settings->value("prefs/logs-text-zoom", 0).toInt());
    connect(logsZoom, &ZoomBar::zoomStep, this, [this](int delta) {
        const int lvl = qBound(-3, gui_settings->value("prefs/logs-text-zoom", 0).toInt() + delta, 12);
        gui_settings->setValue("prefs/logs-text-zoom", lvl);
        debugLogPanel->setFontZoom(lvl);
    });
    metricsPanel->setFontZoom(gui_settings->value("prefs/debug-text-zoom", 0).toInt());
    connect(debugZoom, &ZoomBar::zoomStep, this, [this](int delta) {
        const int lvl = qBound(-4, gui_settings->value("prefs/debug-text-zoom", 0).toInt() + delta, 20);
        gui_settings->setValue("prefs/debug-text-zoom", lvl);
        metricsPanel->setFontZoom(lvl);
    });
}

void MainWindow::updateMetroVisibility()
{
    QSignalBlocker blocker(showMetroAct);
    showMetroAct->setChecked(piSettings->show_metro);

    if (piSettings->show_metro)
    {
        metroWidget->show();
    }
    else
    {
        metroWidget->hide();
    }
}

void MainWindow::toggleTabsVisibility()
{
    piSettings->show_tabs = !piSettings->show_tabs;
    emit settingsChanged();
    updateTabsVisibility();
}

void MainWindow::showTabsMenuChanged()
{
    piSettings->show_tabs = showTabsAct->isChecked();
    emit settingsChanged();
    updateTabsVisibility();
}

void MainWindow::updateTabsVisibility()
{
    QSignalBlocker blocker(showTabsAct);
    showTabsAct->setChecked(piSettings->show_tabs);

    QTabBar* tabBar = editorTabWidget->findChild<QTabBar*>();
    if (!tabBar)
        return;

    if (piSettings->show_tabs)
    {
        tabBar->show();
    }
    else
    {
        tabBar->hide();
    }
}

void MainWindow::toggleButtonVisibility()
{
    piSettings->show_buttons = !piSettings->show_buttons;
    emit settingsChanged();
    updateButtonVisibility();
}

void MainWindow::showButtonsMenuChanged()
{
    piSettings->show_buttons = showButtonsAct->isChecked();
    emit settingsChanged();
    updateButtonVisibility();
}

void MainWindow::updateButtonVisibility()
{
    QSignalBlocker blocker(showButtonsAct);
    showButtonsAct->setChecked(piSettings->show_buttons);

    if (piSettings->show_buttons)
    {
        toolBar->show();
    }
    else
    {
        toolBar->close();
    }
}

void MainWindow::showEditorToolbarMenuChanged()
{
    piSettings->show_editor_toolbar = showEditorToolbarAct->isChecked();
    emit settingsChanged();
    updateEditorToolbarVisibility();
}

void MainWindow::updateEditorToolbarVisibility()
{
    QSignalBlocker blocker(showEditorToolbarAct);
    showEditorToolbarAct->setChecked(piSettings->show_editor_toolbar);
    for (int w = 0; w < workspace_max; w++)
    {
        workspaces[w]->setEditorToolbarEnabled(piSettings->show_editor_toolbar);
    }
}

void MainWindow::completeSnippetListOrIndentLine(QObject* ws)
{
    SonicPiScintilla* spws = ((SonicPiScintilla*)ws);
    if (spws->completionActive())
    {
        spws->acceptCompletionPopup();
    }
    else if (spws->isListActive())
    {
        spws->tabCompleteifList();
    }
    else
    {
        completeSnippetOrIndentCurrentLineOrSelection(spws);
    }
}

void MainWindow::completeSnippetOrIndentCurrentLineOrSelection(SonicPiScintilla* ws)
{
    int start_line, finish_line, point_line, point_index;
    ws->getCursorPosition(&point_line, &point_index);
    if (ws->hasSelectedText())
    {
        statusBar()->showMessage(tr("Indenting selection..."), 2000);
        int unused_a, unused_b;
        ws->getSelection(&start_line, &unused_a, &finish_line, &unused_b);
    }
    else
    {
        statusBar()->showMessage(tr("Indenting line..."), 2000);
        start_line = point_line;
        finish_line = point_line;
    }

    std::string code = ws->text().toStdString();

    oscpkt::Message msg("/buffer-section-complete-snippet-or-indent-selection");
    msg.pushInt32(guiID);
    std::string filename = ws->fileName.toStdString();
    msg.pushStr(filename);
    msg.pushStr(code);
    msg.pushInt32(start_line);
    msg.pushInt32(finish_line);
    msg.pushInt32(point_line);
    msg.pushInt32(point_index);
    sendOSC(msg);
}

void MainWindow::setMarkInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->setMark();
}

void MainWindow::triggerAutocompleteInCurrentWorkspace()
{
    if (SonicPiScintilla* ws = getCurrentWorkspace())
        ws->triggerCompletion();
}

void MainWindow::readCompletionDetailsInCurrentWorkspace()
{
    if (SonicPiScintilla* ws = getCurrentWorkspace())
        ws->showCompletionDocs();
}

void MainWindow::toggleCommentInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    toggleComment(ws);
}

void MainWindow::transposeCharsInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->transposeChars();
}

void MainWindow::moveLineOrSelectionUpInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->moveLineOrSelectionUp();
}

void MainWindow::moveLineOrSelectionDownInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->moveLineOrSelectionDown();
}

void MainWindow::forwardOneLineInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->forwardOneLine();
}

void MainWindow::backOneLineInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->backOneLine();
}

void MainWindow::forwardTenLinesInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->forwardTenLines();
}

void MainWindow::backTenLinesInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->backTenLines();
}

void MainWindow::cutLineFromPointInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->cutLineFromPoint();
}

void MainWindow::copyInCurrentWorkspace()
{
    QWidget* focus = QApplication::focusWidget();
    if (focus && tutorialPane && tutorialPane->isAncestorOf(focus))
    {
        tutorialPane->copySelection();
        return;
    }
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->copyClear();
}

void MainWindow::cutInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->sp_cut();
}

void MainWindow::pasteInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->sp_paste();
}

void MainWindow::rightInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->charRight();
}

void MainWindow::showFindInCurrentWorkspace()
{
    getCurrentWorkspace()->showFind();
}

void MainWindow::findNextInCurrentWorkspace()
{
    getCurrentWorkspace()->findNextMatch();
}

void MainWindow::findPrevInCurrentWorkspace()
{
    getCurrentWorkspace()->findPrevMatch();
}

void MainWindow::leftInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->charLeft();
}

void MainWindow::deleteForwardInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->deleteForward();
}

void MainWindow::deleteBackwardInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->deleteBack();
}

void MainWindow::lineStartInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->lineStart();
}

void MainWindow::lineEndInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->lineEnd();
}

void MainWindow::documentStartInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->documentStart();
}

void MainWindow::documentEndInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->documentEnd();
}

void MainWindow::wordRightInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->wordRight();
}

void MainWindow::wordLeftInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->wordLeft();
}

void MainWindow::selectLineStartInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectLineStart();
}

void MainWindow::selectLineEndInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectLineEnd();
}

void MainWindow::selectWordRightInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectWordRight();
}

void MainWindow::selectWordLeftInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectWordLeft();
}

void MainWindow::selectDocStartInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectDocStart();
}

void MainWindow::selectDocEndInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectDocEnd();
}

void MainWindow::centerCaretInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->centerCaret();
}

void MainWindow::undoInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->undo();
}

void MainWindow::redoInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->redo();
}

void MainWindow::selectAllInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->selectAll();
}

void MainWindow::deleteWordRightInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->deleteWordRight();
}

void MainWindow::deleteWordLeftInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->deleteWordLeft();
}

void MainWindow::upcaseWordOrSelectionInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->upcaseWordOrSelection();
}

void MainWindow::downcaseWordOrSelectionInCurrentWorkspace()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->downcaseWordOrSelection();
}

void MainWindow::toggleComment(SonicPiScintilla* ws)
{
    int start_line, finish_line, point_line, point_index;
    ws->getCursorPosition(&point_line, &point_index);
    if (ws->hasSelectedText())
    {
        statusBar()->showMessage(tr("Toggle selection comment..."), 2000);
        int unused_a, unused_b;
        ws->getSelection(&start_line, &unused_a, &finish_line, &unused_b);
    }
    else
    {
        statusBar()->showMessage(tr("Toggle line comment..."), 2000);
        start_line = point_line;
        finish_line = point_line;
    }

    std::string code = ws->text().toStdString();

    oscpkt::Message msg("/buffer-section-toggle-comment");
    msg.pushInt32(guiID);
    std::string filename = ws->fileName.toStdString();
    msg.pushStr(filename);
    msg.pushStr(code);
    msg.pushInt32(start_line);
    msg.pushInt32(finish_line);
    msg.pushInt32(point_line);
    msg.pushInt32(point_index);
    sendOSC(msg);
}

QString MainWindow::rootPath()
{
    QByteArray envRoot = qgetenv("SONIC_PI_ROOT");
    if (!envRoot.isEmpty())
        return QString::fromLocal8Bit(envRoot);

    // diversity is the spice of life
#if defined(Q_OS_MAC)
    return QCoreApplication::applicationDirPath() + "/../Resources";
#elif defined(Q_OS_WIN)
    // CMake builds, the exe is in build/debug/sonic-pi, etc.
    // We should pass this to the build instead of wiring it up this way!
    return QCoreApplication::applicationDirPath() + "/../../../..";
#else
    // On linux, CMake builds app into the build folder
    return QCoreApplication::applicationDirPath() + "/../../..";
#endif
}

void MainWindow::splashClose()
{
    if (!splash) return;
    // Minimum visible duration so the animation always settles; a slower boot
    // keeps it up until it finishes.
    constexpr qint64 kMinSplashMs = 4500;
    const qint64 shownAt = splash->property("shownAtMs").toLongLong();
    const qint64 elapsed = QDateTime::currentMSecsSinceEpoch() - shownAt;
    if (shownAt > 0 && elapsed < kMinSplashMs) {
        QTimer::singleShot(kMinSplashMs - elapsed, this, [this]() { splashClose(); });
        return;
    }
    // finishAndClose() fades out and self-deletes (WA_DeleteOnClose); drop our
    // reference first so any later splashClose() is a no-op.
    SplashWidget* s = splash;
    splash = nullptr;
    s->finishAndClose();
}

void MainWindow::showWindow()
{
    if (gui_settings->value("first_time", 1).toInt() == 1)
    {
        showMaximized();
    }
    else
    {
        showNormal();
    }
    changeShowLineNumbers();
    changeFlashSettings();
}

void MainWindow::enableScsynthInputsMenuChanged()
{
    piSettings->enable_scsynth_inputs = enableScsynthInputsAct->isChecked();
    emit settingsChanged();
    changeEnableScsynthInputs();
}

void MainWindow::toggleLinkMenu()
{
    metroPane->toggleLink();
}

void MainWindow::uncheckEnableLinkMenu()
{
    enableLinkAct->setChecked(false);
}

void MainWindow::checkEnableLinkMenu()
{
    enableLinkAct->setChecked(true);
}

void MainWindow::mixerForceMonoMenuChanged()
{
    piSettings->mixer_force_mono = mixerForceMonoAct->isChecked();
    emit settingsChanged();
    mixerSettingsChanged();
}

void MainWindow::midiEnabledMenuChanged()
{
    piSettings->midi_enabled = midiEnabledAct->isChecked();
    emit settingsChanged();
    toggleMidi();
}

void MainWindow::gamepadEnabledMenuChanged()
{
    piSettings->gamepad_enabled = gamepadEnabledAct->isChecked();
    emit settingsChanged();
    toggleGamepad();
}

void MainWindow::oscServerEnabledMenuChanged()
{
    piSettings->osc_server_enabled = enableOSCServerAct->isChecked();
    piSettings->osc_public = enableOSCServerAct->isChecked() && allowRemoteOSCAct->isChecked();
    if (!enableOSCServerAct->isChecked())
    {
        allowRemoteOSCAct->setChecked(false);
        piSettings->osc_public = false;
    }
    emit settingsChanged();
    toggleOSCServer();
}

void MainWindow::allowRemoteOSCMenuChanged()
{
    piSettings->osc_public = allowRemoteOSCAct->isChecked();
    emit settingsChanged();
    toggleOSCServer();
}

void MainWindow::mixerInvertStereoMenuChanged()
{
    piSettings->mixer_invert_stereo = mixerInvertStereoAct->isChecked();
    emit settingsChanged();
    mixerSettingsChanged();
}

void MainWindow::changeEnableScsynthInputs()
{
    QSignalBlocker blocker(enableScsynthInputsAct);
    enableScsynthInputsAct->setChecked(piSettings->enable_scsynth_inputs);

    // Send live input channel change to SuperSonic (triggers cold swap)
    // -1 = enable (SuperSonic resolves to boot value or default), 0 = disable.
    int inputChannels = piSettings->enable_scsynth_inputs ? -1 : 0;
    oscpkt::Message msg("/supersonic/inputs/enable");
    msg.pushInt32(inputChannels);
    m_spAPI->SupersonicSendOSC(msg);

    if (piSettings->enable_scsynth_inputs)
    {
        showStatusAndAnnounce(tr("Enabling Audio Inputs..."), 2000);
    }
    else
    {
        showStatusAndAnnounce(tr("Disabling Audio Inputs..."), 2000);
    }
}

void MainWindow::mixerSettingsChanged()
{
    QSignalBlocker blocker(mixerInvertStereoAct);
    mixerInvertStereoAct->setChecked(piSettings->mixer_invert_stereo);
    if (piSettings->mixer_invert_stereo)
    {
        mixerInvertStereo();
    }
    else
    {
        mixerStandardStereo();
    }

    QSignalBlocker blocker2(mixerForceMonoAct);
    mixerForceMonoAct->setChecked(piSettings->mixer_force_mono);
    if (piSettings->mixer_force_mono)
    {
        mixerMonoMode();
    }
    else
    {
        mixerStereoMode();
    }

    // Both axes are always re-applied above; only report the one that changed.
    if (mixerStateKnown)
    {
        if (lastMixerInvertStereo != piSettings->mixer_invert_stereo)
        {
            showStatusAndAnnounce(piSettings->mixer_invert_stereo ? tr("Enabling Inverted Stereo...")
                                                                  : tr("Enabling Standard Stereo..."), 2000);
        }
        if (lastMixerForceMono != piSettings->mixer_force_mono)
        {
            showStatusAndAnnounce(piSettings->mixer_force_mono ? tr("Mono Mode...")
                                                               : tr("Stereo Mode..."), 2000);
        }
    }
    lastMixerInvertStereo = piSettings->mixer_invert_stereo;
    lastMixerForceMono = piSettings->mixer_force_mono;
    mixerStateKnown = true;
}

void MainWindow::update_check_updates()
{
    QSignalBlocker blocker(checkUpdatesAct);
    checkUpdatesAct->setChecked(piSettings->check_updates);
    if (piSettings->check_updates)
    {
        enableCheckUpdates();
    }
    else
    {
        disableCheckUpdates();
    }
}

bool isScopeEnabledByDefault(const QString& name)
{
    if (name == "mono")
        return true;
    return false;
}

bool isScopeEnabled(const QSettings& settings, const QString& name)
{
    QString lname = name.toLower();
    return settings.value("prefs/scope/show-" + lname, isScopeEnabledByDefault(lname)).toBool();
}

void MainWindow::honourPrefs()
{
    update_check_updates();
    updateLogAutoScroll();
    changeGUITransparency(piSettings->gui_transparency);
    changeScopeLabels();
    changeTitleVisibility();
    toggleMidi(1);
    toggleGamepad(1);
    toggleOSCServer(1);
    toggleIcons();
    scope();
    changeShowAutoCompletion();
    changeShowCompletionHelp();
    changeShowContext();
    changeAudioSafeMode();
    changeEnableExternalSynths();
    mixerSettingsChanged();
    changeMidiDefaultChannel();
    changeLogSynths();
    changeLogCues();
    changeClearOutputOnRun();
    changeAutoIndentOnRun();
}

void MainWindow::setMessageBoxStyle()
{
    // Set text color to black and background colors to white for the error message display
    QPalette p = QApplication::palette();
    p.setColor(QPalette::WindowText, "#000");
    p.setColor(QPalette::ButtonText, "#000");
    p.setColor(QPalette::Text, "#000");
    p.setColor(QPalette::Base, "#FFF");
    QApplication::setPalette(p);
}

void MainWindow::startupError(QString msg)
{
    splashClose();
    setMessageBoxStyle();

    QDialog* pDialog = new QDialog(this, Qt::Window | Qt::WindowTitleHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);

    QVBoxLayout* pLayout = new QVBoxLayout(pDialog);

    pDialog->setWindowTitle(tr("Sonic Pi Boot Error"));

    QString text;
    QTextStream str(&text);
    str << tr("Apologies, unable to start...\n")
        << "==========================\n"
        << tr("Sorry, Sonic Pi is having issues booting:") << "\n\n"
        << "```\n"
        << msg
        << "\n```\n\n"
        << tr("Please consider reporting a bug at")
        << "\n\n[http://github.com/samaaron/sonic-pi/issues](http://github.com/samaaron/sonic-pi/issues)\n\n"

        << "**System Information**"
        << "\n\n"
        << "Sonic Pi version: `" << version << "`\n\n"
        << "OS: `" << osDescription() << "`\n\n"
        << "CPU: `" << cpuDescription() << "`\n\n"
        << QString::fromStdString(m_spAPI->GetLogs());

    // The text area for the message.  Allows the user to scroll/view it.
    auto pTextArea = new QTextEdit();
    pTextArea->setMarkdown(text);
    pTextArea->setReadOnly(true);
    pTextArea->setAccessibleName(tr("Boot error details"));
    pLayout->addWidget(pTextArea);
    // Focus the details so a screen reader lands on the message, not the button
    pTextArea->setFocus();

    QDialogButtonBox* pButtons = new QDialogButtonBox(QDialogButtonBox::Ok, this);
    // Accepting the dialog quits the app, so say so
    pButtons->button(QDialogButtonBox::Ok)->setText(tr("Quit"));
    pLayout->addWidget(pButtons);

    auto finished = [&]() {
        std::cout << "[GUI] - Aborting. Sorry about this." << std::endl;
        QApplication::exit(-1);
        exit(EXIT_FAILURE);
    };

    // When the user hits OK, quit
    connect(pButtons, &QDialogButtonBox::accepted, this, [=]() {
        finished();
    });

    // When the dialog is done, quit
    connect(pDialog, &QDialog::finished, this, [=]() {
        finished();
    });

    // Make a sensible size, but then allow resizing
    pDialog->setFixedSize(QSize(ScaleHeightForDPI(750), ScaleHeightForDPI(800)));
    pDialog->setMaximumSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX);
    pDialog->exec();
}

void MainWindow::replaceBuffer(QString id, QString content, int line, int index, int first_line)
{
    SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
    ws->replaceBuffer(content, line, index, first_line);

    // Exit-time saveWorkspaces() and a launch-time set open must both wait
    // until all ten boot loads have landed.
    if (!loaded_workspaces && id.startsWith("workspace_"))
    {
        initialWorkspaceLoads.insert(id);
        if (initialWorkspaceLoads.size() >= workspace_max)
        {
            loaded_workspaces = true;
            initialWorkspaceLoads.clear();
            if (!pendingSetPath.isEmpty())
            {
                const QString path = pendingSetPath;
                pendingSetPath.clear();
                QTimer::singleShot(0, this, [this, path]() { loadSetFromFile(path); });
            }
        }
    }
}

void MainWindow::openSetPath(const QString& path)
{
    if (!path.endsWith(".sonicpi", Qt::CaseInsensitive))
    {
        return;
    }
    if (!loaded_workspaces)
    {
        pendingSetPath = path;
        return;
    }
    loadSetFromFile(path);
}

void MainWindow::replaceBufferIdx(int buf_idx, QString content, int line, int index, int first_line)
{
    //  statusBar()->showMessage(tr("Replacing Buffer..."), 1000);
    SonicPiScintilla* ws = workspaces[buf_idx];
    ws->replaceBuffer(content, line, index, first_line);
}

void MainWindow::replaceLines(QString id, QString content, int start_line, int finish_line, int point_line, int point_index)
{
    SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
    ws->replaceLines(start_line, finish_line, content);
    ws->setCursorPosition(point_line, point_index);
}

QString MainWindow::osDescription()
{
#if QT_VERSION >= 0x050400
    return QSysInfo::prettyProductName();
#else
    // prettyProductName requires QT 5.4
    //
    return QString("Unknown OS");
#endif
}

QString MainWindow::cpuDescription()
{
#if QT_VERSION >= 0x050400
    return QSysInfo::currentCpuArchitecture();

#else
    // currentCpuArchitecture requires QT 5.4
    //
    return QString("Unknown CPU architecture");
#endif
}

std::string MainWindow::number_name(int i)
{
    switch (i)
    {
    case 0:
        return "zero";
    case 1:
        return "one";
    case 2:
        return "two";
    case 3:
        return "three";
    case 4:
        return "four";
    case 5:
        return "five";
    case 6:
        return "six";
    case 7:
        return "seven";
    case 8:
        return "eight";
    case 9:
        return "nine";
    default:
        assert(false);
        return "";
    }
}

void MainWindow::loadWorkspaces()
{
    std::cout << "[GUI] - loading workspaces" << std::endl;

    for (int i = 0; i < workspace_max; i++)
    {
        oscpkt::Message msg("/load-buffer");
        msg.pushInt32(guiID);
        std::string s = "workspace_" + number_name(i);
        msg.pushStr(s);
        sendOSC(msg);
    }
}

void MainWindow::saveWorkspaces()
{
    std::cout << "[GUI] - saving workspaces" << std::endl;

    for (int i = 0; i < workspace_max; i++)
    {
        // Never persist an un-committed card hover projection.
        workspaces[i]->cancelInsertPreview();
        std::string code = workspaces[i]->text().toStdString();
        oscpkt::Message msg("/save-buffer");
        msg.pushInt32(guiID);
        std::string s = "workspace_" + number_name(i);
        msg.pushStr(s);
        msg.pushStr(code);
        sendOSC(msg);
    }
}

void MainWindow::closeEvent(QCloseEvent* event)
{
    writeSettings();
    event->accept();
}

QString MainWindow::currentTabLabel()
{
    return editorTabWidget->tabText(editorTabWidget->currentIndex());
}

bool MainWindow::loadFile()
{
    QString selfilter = QString("%1 (*.rb *.txt)").arg(tr("Buffer files"));
    QString lastBufferDir = gui_settings->value("lastBufferDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getOpenFileName(this, tr("Load Sonic Pi Buffer"), lastBufferDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter);
    if (!fileName.isEmpty())
    {
        gui_settings->setValue("lastBufferDir", QDir(fileName).absolutePath());
        SonicPiScintilla* p = getCurrentWorkspace();
        loadFile(fileName, p);
        return true;
    }
    else
    {
        return false;
    }
}

bool MainWindow::saveAs()
{
    QString selfilter = QString("%1 (*.rb *.txt)").arg(tr("Buffer files"));
    QString lastBufferDir = gui_settings->value("lastBufferDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Buffer"), lastBufferDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter);

    if (!fileName.isEmpty())
    {
        gui_settings->setValue("lastBufferDir", QDir(fileName).absolutePath());
        if (!fileName.contains(QRegularExpression("\\.[a-z]+$")))
        {
            fileName = fileName + ".txt";
        }
        return saveFile(fileName, getCurrentWorkspace());
    }
    else
    {
        return false;
    }
}

bool MainWindow::confirmAction(const QString& text, const QString& informativeText, const QString& confirmLabel)
{
    // QMessageBox renders half-native under the app QSS on macOS.
    QDialog dlg(this);
    dlg.setObjectName("confirmDialog"); // styled in app.qss
    dlg.setWindowTitle(tr("Sonic Pi"));
    dlg.setModal(true);

    QLabel* headline = new QLabel(text);
    headline->setObjectName("confirmHeadline");
    headline->setWordWrap(true);
    headline->setAccessibleName(text);

    QLabel* body = new QLabel(informativeText);
    body->setObjectName("confirmBody");
    body->setWordWrap(true);

    QPushButton* cancelBtn = new QPushButton(tr("Cancel"));
    cancelBtn->setObjectName("confirmCancel");
    QPushButton* confirmBtn = new QPushButton(confirmLabel);
    confirmBtn->setObjectName("confirmPrimary");
    confirmBtn->setAccessibleDescription(informativeText);

    QHBoxLayout* buttons = new QHBoxLayout();
    buttons->addStretch(1);
    buttons->addWidget(cancelBtn);
    buttons->addWidget(confirmBtn);
    buttons->setSpacing(12);

    QVBoxLayout* layout = new QVBoxLayout(&dlg);
    layout->setContentsMargins(28, 24, 28, 20);
    layout->setSpacing(10);
    layout->addWidget(headline);
    layout->addWidget(body);
    layout->addSpacing(14);
    layout->addLayout(buttons);

    dlg.setMinimumWidth(420);
    cancelBtn->setDefault(true);
    cancelBtn->setFocus();
    connect(cancelBtn, &QPushButton::clicked, &dlg, &QDialog::reject);
    connect(confirmBtn, &QPushButton::clicked, &dlg, &QDialog::accept);

    return dlg.exec() == QDialog::Accepted;
}

bool MainWindow::loadSet()
{
    QString lastSetDir = gui_settings->value("lastSetDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getOpenFileName(this, tr("Load Sonic Pi Set"), lastSetDir,
        QString("%1 (*.sonicpi)").arg(tr("Sonic Pi Sets")));
    if (fileName.isEmpty())
    {
        return false;
    }
    loadSetFromFile(fileName);
    return true;
}

void MainWindow::loadSetFromFile(const QString& path)
{
    const SonicPi::SetBundle::Load set = SonicPi::SetBundle::read(path);
    if (!set.ok)
    {
        QMessageBox::warning(this, tr("Sonic Pi"),
            tr("Cannot load set:\n%1.").arg(set.error));
        updateColourTheme();
        return;
    }

    bool anyContent = false;
    for (int i = 0; i < workspace_max; i++)
    {
        if (!workspaces[i]->text().trimmed().isEmpty())
        {
            anyContent = true;
            break;
        }
    }
    if (anyContent)
    {
        if (!confirmAction(
                tr("Load the set %1?").arg(QFileInfo(path).completeBaseName()),
                tr("The current contents of all buffers will be replaced with the buffers stored in this set."),
                tr("Replace")))
        {
            return;
        }
    }

    for (int i = 0; i < workspace_max && i < set.buffers.size(); i++)
    {
        workspaces[i]->setText(set.buffers[i]);
        const int zoom = (i < set.zooms.size()) ? set.zooms[i] : SonicPiScintilla::kDefaultZoom;
        workspaces[i]->setProperty("zoom", QVariant(zoom));
        workspaces[i]->zoomTo(zoom);
        workspaces[i]->updatePlaceholder();
    }
    editorTabWidget->setCurrentIndex(set.currentBuffer);
    saveWorkspaces();

    currentSetPath = QFileInfo(path).absoluteFilePath();
    rememberRecentSet(path);
    showStatusAndAnnounce(tr("Set %1 loaded...").arg(QFileInfo(path).completeBaseName()), 2000);
}

void MainWindow::rememberRecentSet(const QString& path)
{
    const QString abs = QFileInfo(path).absoluteFilePath();
    gui_settings->setValue("lastSetDir", QFileInfo(abs).absolutePath());
    QStringList recents = gui_settings->value("recentSets").toStringList();
    recents.removeAll(abs);
    recents.prepend(abs);
    while (recents.size() > 8)
    {
        recents.removeLast();
    }
    gui_settings->setValue("recentSets", recents);
    updateRecentSetsMenu();
}

bool MainWindow::saveSetAs()
{
    QString lastSetDir = gui_settings->value("lastSetDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Set"),
        lastSetDir + "/" + tr("My Set") + ".sonicpi",
        QString("%1 (*.sonicpi)").arg(tr("Sonic Pi Sets")));
    if (fileName.isEmpty())
    {
        return false;
    }
    if (!fileName.endsWith(".sonicpi", Qt::CaseInsensitive))
    {
        fileName += ".sonicpi";
    }
    return saveSetToPath(fileName);
}

bool MainWindow::saveSet()
{
    if (currentSetPath.isEmpty() || !QFileInfo(currentSetPath).isFile())
    {
        return saveSetAs();
    }
    return saveSetToPath(currentSetPath);
}

bool MainWindow::saveSetToPath(const QString& path)
{
    QVector<QString> buffers(workspace_max);
    QVector<int> zooms(workspace_max);
    for (int i = 0; i < workspace_max; i++)
    {
        buffers[i] = workspaces[i]->text();
        zooms[i] = workspaces[i]->currentZoom();
    }
    const QString err = SonicPi::SetBundle::write(path, buffers, editorTabWidget->currentIndex(), zooms);
    if (!err.isEmpty())
    {
        QMessageBox::warning(this, tr("Sonic Pi"),
            tr("Cannot save set:\n%1.").arg(err));
        updateColourTheme();
        return false;
    }

    currentSetPath = QFileInfo(path).absoluteFilePath();
    rememberRecentSet(path);
    showStatusAndAnnounce(tr("Set saved as %1...").arg(QFileInfo(path).completeBaseName()), 2000);
    return true;
}

void MainWindow::clearAllBuffers()
{
    bool anyContent = false;
    for (int i = 0; i < workspace_max; i++)
    {
        if (!workspaces[i]->text().trimmed().isEmpty())
        {
            anyContent = true;
            break;
        }
    }
    if (anyContent)
    {
        if (!confirmAction(
                tr("Clear all buffers?"),
                tr("The contents of all buffers will be emptied and their text size reset."),
                tr("Clear")))
        {
            return;
        }
    }

    for (int i = 0; i < workspace_max; i++)
    {
        workspaces[i]->setText("");
        workspaces[i]->setProperty("zoom", QVariant(SonicPiScintilla::kDefaultZoom));
        workspaces[i]->zoomTo(SonicPiScintilla::kDefaultZoom);
        workspaces[i]->updatePlaceholder();
    }
    saveWorkspaces();
    // Detach so a later Save Set can't overwrite the old set with new material.
    currentSetPath.clear();
    showStatusAndAnnounce(tr("All buffers cleared..."), 2000);
}

void MainWindow::updateRecentSetsMenu()
{
    recentSetsMenu->clear();
    const QStringList recents = gui_settings->value("recentSets").toStringList();
    for (const QString& path : recents)
    {
        if (!QFileInfo(path).isFile())
        {
            continue;
        }
        QAction* act = recentSetsMenu->addAction(QFileInfo(path).completeBaseName());
        act->setToolTip(path);
        connect(act, &QAction::triggered, this, [this, path]() { loadSetFromFile(path); });
    }
    recentSetsMenu->setEnabled(!recentSetsMenu->isEmpty());
}

void MainWindow::resetErrorPane()
{
    errorPane->hide();
    errorCard->hide();
    returnStolenHelpHeight();
    focusEditor();
}

void MainWindow::runBufferIdx(int idx)
{
    QMetaObject::invokeMethod(editorTabWidget, "setCurrentIndex", Q_ARG(int, idx));
    runCode();
}

void MainWindow::showError(QString msg)
{
    errorCard->hide();
    errorPane->clear();
    errorPane->setHtml("<html><head></head><body class=\"error\">" + msg + "</body></html>");
    errorPane->show();
    // Grow the pane to fit the card so the primary "Jump to error" action is
    // never clipped; cap it so an expanded backtrace scrolls instead.
    QTextDocument* doc = errorPane->document();
    doc->setTextWidth(errorPane->viewport()->width());
    int wanted = qRound(doc->size().height()) + ScaleHeightForDPI(4);
    errorPane->setFixedHeight(qBound(ScaleHeightForDPI(110), wanted, ScaleHeightForDPI(420)));
    stealHelpHeightForError(errorPane->height());
    focusErrors();
    // Errors are the most important feedback event — announce assertively so
    // screen-reader users hear them (parallels the Run started / Stopped cues).
    announce(tr("Error: %1").arg(errorPane->toPlainText().simplified()), true,
             SonicPi::Announcement::Error);
}

void MainWindow::showErrorCard(bool isSyntax, const QString& header, const QString& location,
                               const QString& reason, const QString& codeLine, int lineNumber,
                               int colStart, int colEnd, const QString& backtrace, bool canJump)
{
    errorPane->hide();
    updateErrorCardZoom();
    errorCard->showError(isSyntax, header, location, reason, codeLine, lineNumber, colStart, colEnd, backtrace, canJump);
    stealHelpHeightForError(errorCard->sizeHint().height());
    focusErrors();
    announce(tr("Error: %1").arg(errorCard->plainText().simplified()), true,
             SonicPi::Announcement::Error);
}

void MainWindow::dismissErrorCard()
{
    errorCard->hide();
    returnStolenHelpHeight();
    for (int w = 0; w < workspace_max; w++)
        workspaces[w]->clearLineMarkers();
    focusEditor();
}

void MainWindow::updateHelpCloseIcon()
{
    if (!helpCloseButton)
        return;
    const int px = ScaleWidthForDPI(26);
    const qreal dpr = devicePixelRatioF();
    // Flat button (no chip): muted at rest, accent on hover — matching the
    // A-/A+ zoom glyphs sharing the title row.
    const QColor rest = SonicPiTheme::blend(theme->color("LogForeground"),
                                            theme->color("LogBackground"), 0.55);
    const QColor hover = theme->color("HighlightedBackground");
    helpCloseButton->setIconSize(QSize(px, px));
    m_helpCloseIcon = TablerIcons::icon(TablerIcons::Glyph::SquareX, rest, px, dpr);
    m_helpCloseIconHover = TablerIcons::icon(TablerIcons::Glyph::SquareX, hover, px, dpr);
    helpCloseButton->setIcon(helpCloseButton->underMouse() ? m_helpCloseIconHover
                                                           : m_helpCloseIcon);
}

void MainWindow::updateDocsFilterIcons()
{
    if (docsFilterSearchActions.isEmpty())
        return;
    const int px = ScaleWidthForDPI(14);
    const qreal dpr = devicePixelRatioF();
    // Same muted tint as placeholder-style chrome: legible but quiet.
    const QColor tint = SonicPiTheme::blend(theme->color("Foreground"),
                                            theme->color("PaneBackground"), 0.45);
    const QIcon icon = TablerIcons::icon(TablerIcons::Glyph::Search, tint, px, dpr);
    for (QAction* action : docsFilterSearchActions)
        action->setIcon(icon);
}

void MainWindow::updateDocsNavMinWidth()
{
    // The Tutorial / Examples / … chips must never collapse into scroll
    // arrows: hold the nav pane at the tab bar's natural width so the
    // splitter stretches to fit all chips whenever the window has room.
    // (Recomputed on theme changes — chip metrics come from the stylesheet.)
    // Clamped to 40% of the window so verbose locales (German chip labels
    // run ~530px+) can't turn the chip row into a hard window minimum on
    // small screens — beyond the clamp the bar falls back to scroll arrows.
    if (!docsNavTabs || docsNavTabs->count() == 0)
        return;
    docsNavTabs->tabBar()->ensurePolished();
    const int natural = docsNavTabs->tabBar()->sizeHint().width() + ScaleWidthForDPI(12);
    const int cap = qMax(ScaleWidthForDPI(200), (width() * 2) / 5);
    docsNavTabs->setMinimumWidth(qMin(natural, cap));
}

void MainWindow::ensureDocsSelection()
{
    // The docs pane should never show a blank page: whichever tab is
    // current, make sure it has a selected entry (selecting builds the page).
    QListWidget* list = helpLists.value(docsNavTabs ? docsNavTabs->currentIndex() : -1);
    if (list && list->currentRow() < 0 && list->count() > 0)
        list->setCurrentRow(0);
}

void MainWindow::stealHelpHeightForError(int errorH)
{
    if (!docWidget || !docWidget->isVisible())
        return;
    // Keep a useful strip of code above the error; the help pane is the
    // flexible neighbour, so it gives way first (down to a floor that keeps
    // it usable — beyond that there's nothing more to steal).
    const int wantEditor = ScaleHeightForDPI(220);
    const int need = errorH + wantEditor - mainWidget->height();
    if (need <= 0)
        return;
    const int floor = ScaleHeightForDPI(120);
    const int newDockH = qMax(floor, docWidget->height() - need);
    if (newDockH < docWidget->height())
    {
        // Remember the original height only for the first steal of this error
        // episode — a follow-up error while one is showing keeps the original
        // restore target.
        if (m_dockHBeforeSteal < 0)
            m_dockHBeforeSteal = docWidget->height();
        resizeDocks({ docWidget }, { newDockH }, Qt::Vertical);
    }
}

void MainWindow::returnStolenHelpHeight()
{
    if (m_dockHBeforeSteal < 0)
        return;
    // Unconditional: resizeDocks doesn't always honour the exact height asked
    // for (the dock content's minimums win), so comparing against the
    // requested value to detect a manual re-size is unreliable and quietly
    // skipped the restore.
    if (docWidget && docWidget->isVisible())
        resizeDocks({ docWidget }, { m_dockHBeforeSteal }, Qt::Vertical);
    m_dockHBeforeSteal = -1;
}

void MainWindow::jumpToError()
{
    if (m_errorJumpLine < 0)
        return;
    if (m_errorJumpTab >= 0 && m_errorJumpTab < editorTabWidget->count())
        editorTabWidget->setCurrentIndex(m_errorJumpTab);
    if (SonicPiScintilla* ws = getCurrentWorkspace())
    {
        ws->setCursorPosition(m_errorJumpLine, m_errorJumpCol);
        ws->ensureLineVisible(m_errorJumpLine);
        ws->setFocus();
    }
}

void MainWindow::onErrorAnchorClicked(const QUrl& link)
{
    if (link.scheme() == "http" || link.scheme() == "https")
    {
        QDesktopServices::openUrl(link);
    }
}

void MainWindow::showBufferCapacityError()
{
    showError("<h2 class=\"syntax_error_description\"><pre>GUI Error: Buffer Full</pre></h2><pre class=\"error_msg\"> Your code buffer has reached capacity. <br/> Please remove some code before continuing. <br/><span class=\"error_line\"> For working with very large buffers use: <br/> run_file \"/path/to/buffer.rb\"</span></pre>");
}

void MainWindow::runCode()
{
    // The scope is on screen by default, so with "Reduce animations" set it
    // must not start moving on its own; the user resumes it deliberately via
    // F12, the Visuals menu, or the scope button. This gates on the in-app
    // preference alone, not prefersReducedMotion(): that also folds in the OS
    // animation setting, which is force-disabled under RDP (and by users who
    // turned off window animations for unrelated reasons), so gating on it
    // would freeze the scope for them with no in-app setting to explain it.
    if (!piSettings->reduce_motion)
        scopeWindow->Resume();
    announce(tr("Run started"), false, SonicPi::Announcement::Transport);

    // move log cursors to the end of the logs. Keep them read-only but
    // keyboard-selectable so a screen reader can still navigate/read the output
    // (NoTextInteraction would make the logs unreadable after a run).
    const Qt::TextInteractionFlags logFlags = Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard;
    incomingPane->setTextInteractionFlags(logFlags);
    QTextCursor newIncomingCursor = incomingPane->textCursor();
    newIncomingCursor.movePosition(QTextCursor::End);
    incomingPane->setTextCursor(newIncomingCursor);

    outputPane->setTextInteractionFlags(logFlags);
    QTextCursor newOutputCursor = outputPane->textCursor();
    newOutputCursor.movePosition(QTextCursor::End);
    outputPane->setTextCursor(newOutputCursor);

    update();
    SonicPiScintilla* ws = getCurrentWorkspace();
    // A card hover projection is un-committed: it must never run or be saved.
    ws->cancelInsertPreview();
    // Anchor line-tracking handles to the code being run, so trigger flashes
    // stay on the right line as it's edited live afterwards.
    ws->snapshotRunLines();

    QString code = prefWrappedCode(ws->text());

    if (piSettings->auto_indent_on_run)
    {
        beautifyCode();
    }

    ws->highlightCurrentLine();
    lexer->highlightAll();
    QTimer::singleShot(500, lexer, SLOT(unhighlightAll()));
    QTimer::singleShot(500, ws, SLOT(unhighlightCurrentLine()));
    ws->clearLineMarkers();
    resetErrorPane();

    // std::string code = ws->text().toStdString();
    oscpkt::Message msg("/save-and-run-buffer");
    msg.pushInt32(guiID);

    std::string filename = getCurrentWorkspace()->fileName.toStdString();
    msg.pushStr(filename);

    if (piSettings->clear_output_on_run)
    {
        outputPane->clear();
    }

    msg.pushStr(code.toStdString());
    msg.pushStr(filename);
    bool res = sendOSC(msg);

    if (!res)
    {
        showBufferCapacityError();
        return;
    }

    statusBar()->showMessage(tr("Running Code..."), 1000);
}

QString MainWindow::prefWrappedCode(QString code)
{
    if (!piSettings->log_synths)
    {
        code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code;
    }

    if (!piSettings->log_cues)
    {
        code = "use_cue_logging false #__nosave__ set by Qt GUI user preferences.\n" + code;
    }

    if (piSettings->check_args)
    {
        code = "use_arg_checks true #__nosave__ set by Qt GUI user preferences.\n" + code;
    }

    if (piSettings->enable_external_synths)
    {
        code = "use_external_synths true #__nosave__ set by Qt GUI user preferences.\n" + code;
    }

    if (piSettings->synth_trigger_timing_guarantees)
    {
        code = "use_timing_guarantees true #__nosave__ set by Qt GUI user preferences.\n" + code;
    }

    code = "use_midi_defaults channel: \"" + piSettings->midi_default_channel_str + "\" #__nosave__ set by Qt GUI user preferences.\n" + code;

    return code;
}

QString MainWindow::cardsFileToLoad()
{
    const QString custom = gui_settings->value("prefs/quickstart-cards-file").toString();
    if (!custom.isEmpty() && QFile::exists(custom))
        return custom;
    const QString userCards =
        sonicPiConfigPath() + QDir::separator() + "v5-quickstart-cards.txt";
    if (QFile::exists(userCards))
        return userCards;
    return rootPath() + "/etc/quickstart/cards.txt";
}

void MainWindow::createExamplesMenu()
{
    examplesMenu = menuBar()->addMenu(tr("Examples"));

    QAction* quickstartAct = new QAction(tr("Quickstart Cards..."), this);
    connect(quickstartAct, &QAction::triggered, this, &MainWindow::showQuickstartCards);
    examplesMenu->addAction(quickstartAct);

    QAction* loadCardsAct = new QAction(tr("Load Card Set..."), this);
    connect(loadCardsAct, &QAction::triggered, this, [this]() {
        const QString start = gui_settings->value("lastCardsDir", QDir::homePath()).toString();
        const QString path = QFileDialog::getOpenFileName(
            this, tr("Load Card Set"), start, tr("Card sets (*.txt);;All files (*)"));
        if (path.isEmpty())
            return;
        gui_settings->setValue("lastCardsDir", QFileInfo(path).absolutePath());
        QString err;
        if (!QuickstartPane::validateCardsFile(path, &err))
        {
            QMessageBox::warning(this, tr("Load Card Set"),
                                 tr("\"%1\" is not a valid card set.\n\n%2")
                                     .arg(QFileInfo(path).fileName(), err));
            return; // keep the current cards
        }
        gui_settings->setValue("prefs/quickstart-cards-file", path);
        quickstartPane->setCardsFile(path);
        showQuickstartCards();
        showStatusAndAnnounce(tr("Loaded card set: %1").arg(QFileInfo(path).fileName()), 5000);
    });
    examplesMenu->addAction(loadCardsAct);

    QAction* resetCardsAct = new QAction(tr("Reset to Default Cards"), this);
    connect(resetCardsAct, &QAction::triggered, this, [this]() {
        gui_settings->remove("prefs/quickstart-cards-file");
        quickstartPane->setCardsFile(cardsFileToLoad());
        showQuickstartCards();
        showStatusAndAnnounce(tr("Reset to the default card set."), 5000);
    });
    examplesMenu->addAction(resetCardsAct);

    examplesMenu->addSeparator();

    const struct
    {
        QString dir;
        QString title;
    } categories[] = {
        { "apprentice", tr("Apprentice") },
        { "illusionist", tr("Illusionist") },
        { "magician", tr("Magician") },
        { "sorcerer", tr("Sorcerer") },
        { "wizard", tr("Wizard") },
        { "algomancer", tr("Algomancer") },
    };

    // Same glob + sort as qt-doc.rb, so the running row index lines up with
    // the entries in the help pane's Examples tab.
    int helpRow = 0;
    for (const auto& category : categories)
    {
        QMenu* categoryMenu = examplesMenu->addMenu(category.title);
        QDir dir(rootPath() + "/etc/examples/" + category.dir);
        const QStringList files = dir.entryList(QStringList() << "*.rb", QDir::Files, QDir::Name);
        for (const QString& fname : files)
        {
            QString base = fname;
            base.chop(3);
            QStringList words = base.split('_');
            for (QString& word : words)
            {
                if (!word.isEmpty())
                    word[0] = word[0].toUpper();
            }
            QString title = words.join(' ');
            QString path = dir.filePath(fname);
            examplePaths << path;
            exampleTitles << title;
            int row = helpRow++;
            QAction* act = categoryMenu->addAction(title);
            connect(act, &QAction::triggered, this, [this, path, title, row]() {
                openExample(path, title, row);
            });
        }
        categoryMenu->setEnabled(!files.isEmpty());
    }

    examplesMenu->addSeparator();

    examplesPlayOnOpenAct = new QAction(tr("Play When Opened"), this);
    examplesPlayOnOpenAct->setCheckable(true);
    examplesPlayOnOpenAct->setChecked(piSettings->example_play_on_open);
    connect(examplesPlayOnOpenAct, &QAction::toggled, this, [this](bool on) {
        piSettings->example_play_on_open = on;
    });
    examplesMenu->addAction(examplesPlayOnOpenAct);

    examplesMenu->addSeparator();

    QAction* browseExamplesAct = new QAction(tr("Browse Examples in Help..."), this);
    connect(browseExamplesAct, &QAction::triggered, this, [this]() {
        showExamplesHelpTab(-1);
    });
    examplesMenu->addAction(browseExamplesAct);

    QAction* browseFxAct = new QAction(tr("Browse FX in Help..."), this);
    connect(browseFxAct, &QAction::triggered, this, [this]() {
        showHelpListTab((int)DocTab::Fx, -1);
    });
    examplesMenu->addAction(browseFxAct);
}

void MainWindow::showExamplesHelpTab(int row)
{
    showHelpListTab((int)DocTab::Examples, row);
}

void MainWindow::showHelpListTab(int tabIdx, int row)
{
    southTabs->setCurrentWidget(docsplit);
    docsNavTabs->setCurrentIndex(tabIdx);
    if (tabIdx < helpLists.size())
    {
        QListWidget* list = helpLists[tabIdx];
        if (row >= 0 && row < list->count())
            list->setCurrentRow(row);
        else if (list->currentRow() < 0 && list->count() > 0)
            list->setCurrentRow(0);
    }
    if (!docWidget->isVisible())
        toggleDocPane();
}

void MainWindow::openExample(const QString& path, const QString& title, int helpRow)
{
    // Examples live in the help pane: show the example there and (optionally)
    // play it from the pane. Buffers are never touched.
    showExamplesHelpTab(helpRow);

    if (!examplesPlayOnOpenAct->isChecked())
    {
        showStatusAndAnnounce(tr("Opened %1 in the Help panel.").arg(title), 3000);
        return;
    }

    if (!piSettings->reduce_motion)
        scopeWindow->Resume();
    if (!(tutorialPane && tutorialPane->isVisible() && tutorialPane->playFirstSnippet()))
    {
        QFile file(path);
        if (!file.open(QFile::ReadOnly | QFile::Text))
        {
            showStatusAndAnnounce(tr("Unable to load example: %1").arg(title), 3000);
            return;
        }
        QTextStream in(&file);
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        in.setEncoding(QStringConverter::Utf8);
#else
        in.setCodec("UTF-8");
#endif
        oscpkt::Message msg("/run-code");
        msg.pushInt32(guiID);
        msg.pushStr(prefWrappedCode(in.readAll()).toStdString());
        sendOSC(msg);
    }
    showStatusAndAnnounce(tr("Playing %1 from the Help panel.").arg(title), 3000);
}

void MainWindow::applySouthTabIcons()
{
    const QColor fg = theme->color("TabText");
    // The selected tab is accent-filled, so its glyph (QIcon::On) is drawn
    // entirely in the contrasting colour.
    const QColor on = theme->contrastingText(theme->color("TabSelected"));
    // Sidebar thickness comes from the shared chrome unit so the side tabs,
    // buffer selector and transport all line up. Icon fills ~65%.
    const int side = ScaleHeightForDPI(SonicPi::kChromeControlDp);
    const int px = qRound(side * 0.65);
    const qreal dpr = devicePixelRatioF();
    // Set on the bar itself: setIconSize on the QTabWidget doesn't reach a
    // custom QTabBar, leaving it at the style's 16px default.
    southTabs->setIconSize(QSize(px, px));
    southTabs->tabBar()->setIconSize(QSize(px, px));
    // southTabs is an IconTabWidget, so its bar is always an IconTabBar.
    static_cast<IconTabBar*>(southTabs->tabBar())->setSquareSide(side);
    // Icon-only tabs, painted dead-centre and upright by IconTabBar; the
    // label lives on in the accessible name and tooltip.
    auto set = [&](QWidget* page, const QPixmap& normal, const QPixmap& selected,
                   const QString& name) {
        int idx = southTabs->indexOf(page);
        if (idx < 0)
            return;
        QIcon icon;
        icon.addPixmap(normal, QIcon::Normal, QIcon::Off);
        icon.addPixmap(selected, QIcon::Normal, QIcon::On);
        southTabs->setTabIcon(idx, icon);
        southTabs->setTabText(idx, "");
        southTabs->tabBar()->setAccessibleTabName(idx, name);
    };
    set(quickstartPane, TablerIcons::pixmap(TablerIcons::Glyph::GridDots, fg, px, dpr), TablerIcons::pixmap(TablerIcons::Glyph::GridDots, on, px, dpr), tr("Cards"));
    set(docsplit, TablerIcons::pixmap(TablerIcons::Glyph::Book, fg, px, dpr), TablerIcons::pixmap(TablerIcons::Glyph::Book, on, px, dpr), tr("Docs"));
    set(debugLogPanel, TablerIcons::pixmap(TablerIcons::Glyph::Radioactive, fg, px, dpr), TablerIcons::pixmap(TablerIcons::Glyph::Radioactive, on, px, dpr), tr("Logs"));
    set(metricsPanel, TablerIcons::pixmap(TablerIcons::Glyph::BinaryTree, fg, px, dpr), TablerIcons::pixmap(TablerIcons::Glyph::BinaryTree, on, px, dpr), tr("Debug"));
}

void MainWindow::showQuickstartCards()
{
    southTabs->setCurrentWidget(quickstartPane);
    if (!docWidget->isVisible())
        docWidget->show();
}

void MainWindow::zoomCurrentWorkspaceIn()
{
    statusBar()->showMessage(tr("Zooming In..."), 2000);
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->zoomFontIn();
}

void MainWindow::zoomCurrentWorkspaceOut()
{
    statusBar()->showMessage(tr("Zooming Out..."), 2000);
    SonicPiScintilla* ws = getCurrentWorkspace();
    ws->zoomFontOut();
}

void MainWindow::updateErrorCardZoom()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    if (!ws || !errorCard)
        return;
    // Editor text is the lexer's base size plus the Scintilla zoom; the card's
    // design sizes assume the default zoom of 2. Boosted a touch so the card
    // reads slightly larger than the code.
    double base = lexer->defaultFont(0).pointSizeF();
    errorCard->setFontScale(1.15 * (base + ws->currentZoom()) / (base + SonicPiScintilla::kDefaultZoom));
}

void MainWindow::beautifyCode()
{
    statusBar()->showMessage(tr("Beautifying..."), 2000);
    SonicPiScintilla* ws = getCurrentWorkspace();
    std::string code = ws->text().toStdString();
    int line = 0;
    int index = 0;
    ws->getCursorPosition(&line, &index);
    int first_line = ws->firstVisibleLine();
    oscpkt::Message msg("/buffer-beautify");
    msg.pushInt32(guiID);
    std::string filename = getCurrentWorkspace()->fileName.toStdString();
    msg.pushStr(filename);
    msg.pushStr(code);
    msg.pushInt32(line);
    msg.pushInt32(index);
    msg.pushInt32(first_line);
    sendOSC(msg);
}

bool MainWindow::sendOSC(oscpkt::Message m)
{
    return m_spAPI->SendOSC(m);
}

void MainWindow::check_for_updates_now()
{
    showStatusAndAnnounce(tr("Checking for updates..."), 2000);
    oscpkt::Message msg("/check-for-updates-now");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::enableCheckUpdates()
{
    showStatusAndAnnounce(tr("Enabling update checking..."), 2000);
    oscpkt::Message msg("/enable-update-checking");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::disableCheckUpdates()
{
    showStatusAndAnnounce(tr("Disabling update checking..."), 2000);
    oscpkt::Message msg("/disable-update-checking");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerHpfEnable(float freq)
{
    showStatusAndAnnounce(tr("Enabling Mixer HPF..."), 2000);
    oscpkt::Message msg("/mixer-hpf-enable");
    msg.pushInt32(guiID);
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerHpfDisable()
{
    showStatusAndAnnounce(tr("Disabling Mixer HPF..."), 2000);
    oscpkt::Message msg("/mixer-hpf-disable");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerLpfEnable(float freq)
{
    showStatusAndAnnounce(tr("Enabling Mixer LPF..."), 2000);
    oscpkt::Message msg("/mixer-lpf-enable");
    msg.pushInt32(guiID);
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerLpfDisable()
{
    showStatusAndAnnounce(tr("Disabling Mixer LPF..."), 2000);
    oscpkt::Message msg("/mixer-lpf-disable");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

// The four mixer senders below are re-applied for both axes on every settings
// change, so user feedback (status bar + speech) lives in mixerSettingsChanged(),
// which knows which axis actually changed.
void MainWindow::mixerInvertStereo()
{
    oscpkt::Message msg("/mixer-invert-stereo");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerStandardStereo()
{
    oscpkt::Message msg("/mixer-standard-stereo");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerMonoMode()
{
    oscpkt::Message msg("/mixer-mono-mode");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerStereoMode()
{
    oscpkt::Message msg("/mixer-stereo-mode");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::stopCode()
{
    stopRunningSynths();
    statusBar()->showMessage(tr("Stopping..."), 2000);
    announce(tr("Stopped"), false, SonicPi::Announcement::Transport);
}

void MainWindow::scopeVisibilityChanged()
{
    piSettings->show_scopes = scopeWidget->isVisible();
    // A hidden dock must suspend the audio processor: the deferred
    // pause-when-silent cannot resolve inside OnConsumeAudioData's
    // isVisible() gate.
    scopeWindow->SetSuspended(!piSettings->show_scopes);
    scopeAct->setIcon(theme->getScopeIcon(piSettings->show_scopes));
    emit settingsChanged();
}

void MainWindow::toggleScope()
{
    piSettings->show_scopes = !piSettings->show_scopes;
    emit settingsChanged();
    scope();
}

void MainWindow::scope()
{
    scopeAct->setIcon(theme->getScopeIcon(piSettings->show_scopes));
    if (piSettings->show_scopes)
    {
        for (auto name : scopeWindow->GetScopeCategories())
        {
            scopeWindow->EnableScope(name, piSettings->isScopeActive(name));
        }
        scopeWidget->show();
    }
    else
    {
        scopeWidget->hide();
    }
    // Explicit sync: a dock hidden before its first show emits no
    // visibilityChanged, so the boot path (honourPrefs → scope()) must set
    // the suspend state directly or Resume() at run start re-enables the
    // audio processor behind a closed dock.
    scopeWindow->SetSuspended(!piSettings->show_scopes);

    QSignalBlocker blocker(scopeAct);
    scopeAct->setChecked(piSettings->show_scopes);
}

void MainWindow::about()
{
    // todo: this is returning true even after the window disappears
    // Qt::Tool windows get closed automatically when app loses focus
    QSignalBlocker blocker(infoAct);

    if (infoWidg->isVisible())
    {
        showStatusAndAnnounce(tr("Hiding about window..."), 2000);
        infoWidg->hide();
        infoAct->setChecked(false);
    }
    else
    {
        showStatusAndAnnounce(tr("Showing about window..."), 2000);
        loadInfoPaneContent();     // deferred from startup to first open
        if (infoPanesDirty)
            rerenderInfoPanes();   // styles changed while hidden
        infoWidg->raise();
        infoWidg->show();
        infoAct->setChecked(true);
    }
    infoAct->setIcon(theme->getInfoIcon(infoWidg->isVisible()));
}

void MainWindow::toggleHelpIcon()
{
    helpAct->setIcon(theme->getHelpIcon(docWidget->isVisible()));
}
void MainWindow::help()
{

    QSignalBlocker blocker(helpAct);

    if (docWidget->isVisible())
    {
        showStatusAndAnnounce(tr("Hiding help..."), 2000);
        docWidget->hide();
        helpAct->setChecked(false);
    }
    else
    {
        showStatusAndAnnounce(tr("Showing help..."), 2000);
        docWidget->show();
        ensureDocsSelection();   // never land on a blank page
        helpAct->setChecked(true);
    }
    helpAct->setIcon(theme->getHelpIcon(docWidget->isVisible()));
}

void MainWindow::helpContext()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    QString selection = ws->selectedText();
    if (selection == "")
    { // get current word instead
        int line, pos;
        ws->getCursorPosition(&line, &pos);
        QString text = ws->text(line);
        selection = ws->wordAtLineIndex(line, pos);
    }
    showHelpForKeyword(selection);
}

void MainWindow::showHelpForKeyword(QString selection)
{
    // Reveal the docs (the user may be on the Debug tab) — a docs lookup from
    // C-i, the completion popup's Docs button, etc. should surface the docs.
    revealDocsTab();
    selection = selection.toLower();
    if (!selection.isEmpty() && selection[0] == ':')
        selection = selection.mid(1);

    if (helpKeywords.contains(selection))
    {
        struct help_entry entry = helpKeywords[selection];
        QListWidget* list = helpLists[entry.pageIndex];

        // force current row to be changed
        // by setting it to a different value to
        // entry.entryIndex and then setting it
        // back. That way it always gets displayed
        // in the GUI :-)
        if (entry.entryIndex == 0)
        {
            list->setCurrentRow(1);
        }
        else
        {
            list->setCurrentRow(0);
        }
        docsNavTabs->setCurrentIndex(entry.pageIndex);
        list->setCurrentRow(entry.entryIndex);
    }
}

void MainWindow::changeGUITransparency(int val)
{
    // scale it linearly from 0 -> 100 to 0.3 -> 1
    setWindowOpacity((0.7 * ((100 - (float)val) / 100.0)) + 0.3);
}

void MainWindow::changeSystemPreAmp(int val, int silent)
{
    std::cout << "[GUI] - Change Volume to " << val << std::endl;
    float v = (float)val;
    v = (v / 100.0) * 2.0;
    oscpkt::Message msg("/mixer-amp");
    msg.pushInt32(guiID);
    msg.pushFloat(v);
    msg.pushInt32(silent);
    sendOSC(msg);
    statusBar()->showMessage(tr("Updating System Volume..."), 2000);
}

void MainWindow::changeScopeKindVisibility(QString name)
{
    foreach (QAction* action, scopeKindVisibilityMenu->actions())
    {
        if (action->text() == name)
        {
            QSignalBlocker blocker(action);
            action->setChecked(piSettings->isScopeActive(name));
        }
    }

    scopeWindow->EnableScope(name, piSettings->isScopeActive(name));
}

void MainWindow::scopeKindVisibilityMenuChanged()
{
    foreach (QAction* action, scopeKindVisibilityMenu->actions())
    {
        piSettings->setScopeState(action->text(), action->isChecked());
        changeScopeKindVisibility(action->text());
    }

    emit settingsChanged();
}

void MainWindow::toggleLeftScope()
{
    // scopeInterface->enableScope("Left",show_left_scope->isChecked());
}

void MainWindow::toggleRightScope()
{
    // scopeInterface->enableScope("Right",show_right_scope->isChecked());
}

void MainWindow::showScopeLabelsMenuChanged()
{
    piSettings->show_scope_labels = showScopeLabelsAct->isChecked();
    emit settingsChanged();
    changeScopeLabels();
}

void MainWindow::titleVisibilityChanged()
{

    piSettings->show_titles = showTitlesAct->isChecked();
    emit settingsChanged();
    changeTitleVisibility();
}

void MainWindow::menuBarInFullscreenVisibilityChanged()
{

    piSettings->hide_menubar_in_fullscreen = hideMenuBarInFullscreenAct->isChecked();
    emit settingsChanged();
    changeMenuBarInFullscreenVisibility();
}

void MainWindow::changeScopeLabels()
{
    QSignalBlocker blocker(showScopeLabelsAct);
    showScopeLabelsAct->setChecked(piSettings->show_scope_labels);
    scopeWindow->SetScopeLabels(piSettings->show_scope_labels);
}

void MainWindow::changeTitleVisibility()
{
    QSignalBlocker blocker(showTitlesAct);
    if (piSettings->show_titles)
    {
        namedTitleBars();
        showTitlesAct->setChecked(true);
    }
    else
    {
        blankTitleBars();
        showTitlesAct->setChecked(false);
    }
}

void MainWindow::changeMenuBarInFullscreenVisibility()
{
    QSignalBlocker blocker(hideMenuBarInFullscreenAct);
    if (piSettings->hide_menubar_in_fullscreen)
    {
        if (piSettings->full_screen)
        {
            menuBar()->hide();
        }
        hideMenuBarInFullscreenAct->setChecked(true);
    }
    else
    {
        if (piSettings->full_screen)
        {
            menuBar()->show();
        }
        hideMenuBarInFullscreenAct->setChecked(false);
    }
}

// The user made a deliberate theme choice: stop treating the current theme as
// something we imposed on the OS's behalf, so a later "contrast off" event, a
// re-fired contrast signal, or the next boot doesn't yank their pick away.
// Callers must only invoke this for an ACTUAL theme change — re-selecting the
// current theme is not a choice and must not disturb the restore state.
void MainWindow::noteExplicitThemeChoice()
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
    if (!accessibilityHints)
        accessibilityHints = new QAccessibilityHints(this);
    if (accessibilityHints->contrastPreference() == Qt::ContrastPreference::HighContrast)
    {
        // Picked while the OS still asks for contrast: persist the opt-out so
        // neither a re-fired signal nor the next boot re-imposes high
        // contrast. Keep the saved pre-contrast theme — if the user is back
        // on high contrast when the episode ends, it's still what to restore.
        gui_settings->setValue("prefs/theme-os-contrast-opt-out", true);
        return;
    }
#endif
    gui_settings->remove("prefs/theme-before-os-contrast");
}

void MainWindow::applyOSContrastPreference()
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 10, 0)
    // QAccessibilityHints (Qt 6.10+) maps this to Windows Contrast Themes and
    // macOS Increase Contrast. On older Qt the whole feature compiles out.
    if (!accessibilityHints)
        accessibilityHints = new QAccessibilityHints(this);
    const bool osWantsContrast =
        accessibilityHints->contrastPreference() == Qt::ContrastPreference::HighContrast;

    // "prefs/theme-before-os-contrast" remembers the theme we auto-replaced,
    // persisted so quit-while-contrast-on still restores it next boot. It is
    // only ever set here and cleared by an explicit user pick.
    const QString savedTheme = gui_settings->value("prefs/theme-before-os-contrast", "").toString();

    if (osWantsContrast)
    {
        // The user explicitly picked a theme during this contrast episode
        // (see noteExplicitThemeChoice) — never re-impose high contrast over
        // that, across signal re-fires and restarts alike.
        if (gui_settings->value("prefs/theme-os-contrast-opt-out", false).toBool())
            return;
        if (piSettings->colourScheme != SonicPiTheme::HighContrastScheme)
        {
            if (savedTheme.isEmpty())
                gui_settings->setValue("prefs/theme-before-os-contrast",
                                       SonicPiTheme::colourSchemeToName(piSettings->colourScheme));
            // Force High Contrast but keep the user's current icon-set choice.
            piSettings->colourScheme = SonicPiTheme::HighContrastScheme;
            emit settingsChanged();
            updateColourTheme();
        }
    }
    else
    {
        // Episode over: a future contrast episode starts fresh.
        gui_settings->remove("prefs/theme-os-contrast-opt-out");
        if (!savedTheme.isEmpty())
        {
            // The current theme is still ours to undo: hand back what the
            // user had before. (If they picked something else meanwhile,
            // just drop the stale marker.)
            gui_settings->remove("prefs/theme-before-os-contrast");
            if (piSettings->colourScheme == SonicPiTheme::HighContrastScheme)
            {
                piSettings->colourScheme = theme->colourSchemeFromName(savedTheme);
                emit settingsChanged();
                updateColourTheme();
            }
        }
    }
#endif
}

void MainWindow::cycleThemes()
{
    noteExplicitThemeChoice();
    // Cycle the colour scheme; the icon set is an independent choice, preserved.
    SonicPiTheme::ColourScheme next;
    switch (piSettings->colourScheme) {
        case SonicPiTheme::LightScheme:        next = SonicPiTheme::DarkScheme; break;
        case SonicPiTheme::DarkScheme:         next = SonicPiTheme::MildDarkScheme; break;
        case SonicPiTheme::MildDarkScheme:     next = SonicPiTheme::PhosphorScheme; break;
        case SonicPiTheme::PhosphorScheme:     next = SonicPiTheme::HighContrastScheme; break;
        case SonicPiTheme::HighContrastScheme: next = SonicPiTheme::SignalScheme; break;
        case SonicPiTheme::SignalScheme:
        default:                               next = SonicPiTheme::LightScheme; break;
    }
    piSettings->colourScheme = next;
    emit settingsChanged();
    updateColourTheme();
}

void MainWindow::colourThemeMenuChanged(int themeID)
{
    SonicPiTheme::ColourScheme scheme = SonicPiTheme::LightScheme;
    if (themeID == 2)      scheme = SonicPiTheme::DarkScheme;
    else if (themeID == 3) scheme = SonicPiTheme::HighContrastScheme;
    else if (themeID == 4) scheme = SonicPiTheme::MildDarkScheme;
    else if (themeID == 5) scheme = SonicPiTheme::PhosphorScheme;
    else if (themeID == 6) scheme = SonicPiTheme::SignalScheme;

    // Re-selecting the current scheme is not a choice. (The icon set is a
    // separate control and is left untouched here.)
    if (scheme != piSettings->colourScheme)
        noteExplicitThemeChoice();
    piSettings->colourScheme = scheme;

    emit settingsChanged();
    updateColourTheme();
}

void MainWindow::logAutoScrollMenuChanged()
{
    piSettings->log_auto_scroll = logAutoScrollAct->isChecked();
    emit settingsChanged();
    updateLogAutoScroll();
}

void MainWindow::updateLogAutoScroll()
{
    QSignalBlocker blocker(logAutoScrollAct);
    logAutoScrollAct->setChecked(piSettings->log_auto_scroll);
    bool val = piSettings->log_auto_scroll;

    outputPane->forceScrollDown(val);
    if (val)
    {
        showStatusAndAnnounce(tr("Log Auto Scroll on..."), 2000);
    }
    else
    {
        showStatusAndAnnounce(tr("Log Auto Scroll off..."), 2000);
    }
}

void MainWindow::toggleIcons()
{
    runAct->setIcon(theme->getRunIcon());
    stopAct->setIcon(theme->getStopIcon());
    saveAsAct->setIcon(theme->getSaveAsIcon());
    loadFileAct->setIcon(theme->getLoadIcon());
    textIncAct->setIcon(theme->getTextIncIcon());
    textDecAct->setIcon(theme->getTextDecIcon());

    helpAct->setIcon(theme->getHelpIcon(docWidget->isVisible()));
    recAct->setIcon(theme->getRecIcon(is_recording, is_recording));
    prefsAct->setIcon(theme->getPrefsIcon(prefsWidget->isVisible()));
    infoAct->setIcon(theme->getInfoIcon(infoWidg->isVisible()));
    scopeAct->setIcon(theme->getScopeIcon(scopeWidget->isVisible()));

    // Pro glyphs are square; the classic set is wide (icon + baked label), so it
    // needs a wider icon slot or it renders tiny squashed into a square.
    if (piSettings->proIcons)
        toolBar->setIconSize(ScaleForDPI(38, 38));
    else
        toolBar->setIconSize(ScaleForDPI(107, 38));
    toolBar->setMinimumHeight(ScaleHeightForDPI(45));
}

void MainWindow::updateColourTheme()
{
    const SonicPiTheme::ColourScheme scheme = piSettings->colourScheme;

    QSignalBlocker lightBlocker(lightThemeAct);
    lightThemeAct->setChecked(scheme == SonicPiTheme::LightScheme);
    QSignalBlocker darkBlocker(darkThemeAct);
    darkThemeAct->setChecked(scheme == SonicPiTheme::DarkScheme);
    QSignalBlocker highContrastBlocker(highContrastThemeAct);
    highContrastThemeAct->setChecked(scheme == SonicPiTheme::HighContrastScheme);
    QSignalBlocker mildBlocker(mildThemeAct);
    mildThemeAct->setChecked(scheme == SonicPiTheme::MildDarkScheme);
    QSignalBlocker phosphorBlocker(phosphorThemeAct);
    phosphorThemeAct->setChecked(scheme == SonicPiTheme::PhosphorScheme);
    QSignalBlocker signalBlocker(signalThemeAct);
    signalThemeAct->setChecked(scheme == SonicPiTheme::SignalScheme);

    // Icon set toggle is independent of the colour scheme; every scheme,
    // High Contrast included, supports both the Classic and Pro icon sets.
    QSignalBlocker proIconsBlocker(proIconsAct);
    proIconsAct->setChecked(piSettings->proIcons);

    theme->applyTheme(piSettings->colourScheme, piSettings->proIcons);
    theme->setHueRotation(piSettings->hue_rotation);
    theme->setMonochrome(piSettings->monochrome);
    theme->setInvert(piSettings->invert_colours);
    appliedColourScheme = static_cast<int>(piSettings->colourScheme);
    appliedProIcons = piSettings->proIcons;
    themeEverApplied = true;
    showStatusAndAnnounce(tr("Colour Theme: ") + theme->getName(), 2000);

    QString css = theme->getCss();
    toggleIcons();


    // Info window typography, mirroring the docs pane: accent headings,
    // coloured links, monospace code. (QTextDocument's CSS subset — no
    // borders/hover, so it's colour, size and weight doing the work.)
    QString infoCss = QString(
        "body { color: %1; font-size: 12pt; }"
        "h1 { color: %2; font-size: 20pt; font-weight: bold; }"
        "h2 { color: %3; font-size: 15pt; font-weight: bold; }"
        "h3 { color: %1; font-size: 13pt; font-weight: bold; }"
        "a { color: %2; }"
        "code, pre { font-family: 'Hack'; color: %2; }")
        .arg(theme->color("WindowForeground").name(),
             theme->color("HighlightedBackground").name(),
             theme->color("NumberForeground").name());
    foreach (QTextBrowser* pane, infoPanes)
        pane->document()->setDefaultStyleSheet(infoCss);
    // Re-rendering resets scroll/selection and re-parses the big changelog,
    // so hidden panes just go dirty and re-render on next show (about()).
    if (infoWidg && infoWidg->isVisible())
        rerenderInfoPanes();
    else
        infoPanesDirty = true;

    errorPane->document()->setDefaultStyleSheet(css);
    errorCard->applyTheme();
    if (tutorialPane)
        tutorialPane->applyTheme();
    if (quickstartPane)
        quickstartPane->applyTheme();
    if (southTabs)
        applySouthTabIcons();

    // clear stylesheets
    this->setStyleSheet("");
    infoWidg->setStyleSheet("");
    mainWidget->setStyleSheet("");
    statusBar()->setStyleSheet("");
    outputPane->setStyleSheet("");
    outputWidget->setStyleSheet("");
    prefsWidget->setStyleSheet("");
    editorTabWidget->setStyleSheet("");
    // TODO inject to settings Widget
    // prefTabs->setStyleSheet("");
    docsNavTabs->setStyleSheet("");
    docWidget->setStyleSheet("");
    toolBar->setStyleSheet("");
    scopeWidget->setStyleSheet("");

    QPalette p = theme->createPalette();
    QApplication::setPalette(p);
    theme->reloadStylesheet();
    QString appStyling = theme->getAppStylesheet();

    this->setStyleSheet(appStyling);
    infoWidg->setStyleSheet(appStyling);
    settingsWidget->setStyleSheet(appStyling);

    // Re-tint the log history already on screen so it tracks the new theme
    // (scheme / hue / monochrome) instead of keeping the colours it was
    // written with — each fragment remembers its theme role.
    outputPane->recolour(theme);
    incomingPane->recolour(theme);

    // The hue dial previews the highlight colour; give the settings pane the
    // base (un-rotated) accent so its live tint matches the real accent.
    settingsWidget->setHuePreviewBase(theme->rawColor("HighlightedBackground"));
    settingsWidget->refreshThemeCards(theme);

    scopeWindow->Refresh();
    scopeWidget->update();

    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        ((SonicPiEditor*)editorTabWidget->widget(i))->updateColourTheme(appStyling, piSettings->colourScheme);
    }

    // The Docs nav/content divider paints itself (ThinSplitter): a thin centre
    // line at rest, revealed full-width on hover.
    docsplit->setDividerColors(theme->color("PaneBackground"),
                               theme->color("WindowBorder"),
                               theme->color("ScrollBarHover"));

    // Same reveal for the QMainWindow dock separators (painted by the proxy style).
    DividerProxyStyle::setDividerColors(theme->color("WindowBackground"),
                                        theme->color("WindowBorder"),
                                        theme->color("ScrollBarHover"));
    updateHelpCloseIcon();   // re-tint the help ✕ for the new theme
    updateDocsFilterIcons(); // re-tint the docs filter magnifiers too
    updateDocsNavMinWidth(); // chip metrics may have changed with the theme
    update();   // repaint separators with the new colours

    updateContextWithCurrentWs();
    scopeWindow->SetColor(theme->color("Scope"));
    scopeWindow->SetColor2(theme->color("Scope_2"));
    scopeWindow->SetBackgroundColor(theme->color("LogBackground"));
    scopeWindow->SetPauseButtonColor(theme->color("WindowBorder"));
    lexer->unhighlightAll();
    metroPane->updateColourTheme();

    if (debugLogPanel)
    {
        debugLogPanel->applyTheme(theme->color("LogForeground"),
                                  theme->color("LogBackground"));
    }

    if (metricsPanel)
    {
        metricsPanel->applyTheme(theme);
    }

    if (logsZoom)
        logsZoom->applyTheme();
    if (debugZoom)
        debugZoom->applyTheme();
}

void MainWindow::showLineNumbersMenuChanged()
{
    piSettings->show_line_numbers = showLineNumbersAct->isChecked();
    emit settingsChanged();
    changeShowLineNumbers();
}

void MainWindow::showAutoCompletionMenuChanged()
{
    piSettings->show_autocompletion = showAutoCompletionAct->isChecked();
    emit settingsChanged();
    changeShowAutoCompletion();
}

void MainWindow::showCompletionHelpMenuChanged()
{
    piSettings->show_completion_help = showCompletionHelpAct->isChecked();
    emit settingsChanged();
    changeShowCompletionHelp();
}

void MainWindow::showContextMenuChanged()
{
    piSettings->show_context = showContextAct->isChecked();
    emit settingsChanged();
    changeShowContext();
}

void MainWindow::flashCodeMenuChanged()
{
    piSettings->flash_code = flashCodeAct->isChecked();
    emit settingsChanged();
    changeFlashSettings();
}

void MainWindow::flashGutterMenuChanged()
{
    piSettings->flash_gutter = flashGutterAct->isChecked();
    emit settingsChanged();
    changeFlashSettings();
}

void MainWindow::showLoopScopesMenuChanged()
{
    piSettings->show_loop_scopes = showLoopScopesAct->isChecked();
    emit settingsChanged();
    changeFlashSettings();
}

void MainWindow::loopScopeScrollMenuChanged()
{
    piSettings->loop_scope_scroll = loopScopeScrollAct->isChecked();
    emit settingsChanged();
    changeFlashSettings();
}

void MainWindow::changeFlashSettings()
{
    QSignalBlocker b1(flashCodeAct);
    QSignalBlocker b2(flashGutterAct);
    QSignalBlocker b3(showLoopScopesAct);
    QSignalBlocker b4(loopScopeScrollAct);
    flashCodeAct->setChecked(piSettings->flash_code);
    flashGutterAct->setChecked(piSettings->flash_gutter);
    showLoopScopesAct->setChecked(piSettings->show_loop_scopes);
    loopScopeScrollAct->setChecked(piSettings->loop_scope_scroll);
    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        SonicPiScintilla* ws = ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace();
        ws->setFlashBrightness(piSettings->flash_brightness);
        ws->setLiveLoopScopeScroll(piSettings->loop_scope_scroll);
        // Turning loop scopes off removes the widgets right away; turning it
        // back on shows them again from the next Run (loops re-register on run).
        if (!piSettings->show_loop_scopes)
            ws->clearLiveLoopScopes();
    }
}

void MainWindow::speakTransportMenuChanged()
{
    piSettings->speak_transport = speakTransportAct->isChecked();
    emit settingsChanged();
}

void MainWindow::reduceMotionMenuChanged()
{
    piSettings->reduce_motion = reduceMotionAct->isChecked();
    SonicPi::setReduceMotionPreference(piSettings->reduce_motion);
    // Enabling should still the scope right away, not at the next silence
    // (it's on screen by default). Turning it off never auto-resumes —
    // motion only restarts on a deliberate act (Run, F12, the scope button).
    if (piSettings->reduce_motion)
        scopeWindow->Pause();
    emit settingsChanged();
    showStatusAndAnnounce(piSettings->reduce_motion ? tr("Reduce animations on")
                                                    : tr("Reduce animations off"), 2000);
}

void MainWindow::audioSafeMenuChanged()
{
    piSettings->check_args = audioSafeAct->isChecked();
    emit settingsChanged();
    changeAudioSafeMode();
}

void MainWindow::audioTimingGuaranteesMenuChanged()
{
    piSettings->synth_trigger_timing_guarantees = audioTimingGuaranteesAct->isChecked();
    emit settingsChanged();
    changeAudioTimingGuarantees();
}

void MainWindow::changeAudioTimingGuarantees()
{
    QSignalBlocker blocker(audioTimingGuaranteesAct);
    audioTimingGuaranteesAct->setChecked(piSettings->synth_trigger_timing_guarantees);
}

void MainWindow::enableExternalSynthsMenuChanged()
{
    piSettings->enable_external_synths = enableExternalSynthsAct->isChecked();
    emit settingsChanged();
    changeEnableExternalSynths();
}

void MainWindow::changeEnableExternalSynths()
{
    QSignalBlocker blocker(enableExternalSynthsAct);
    enableExternalSynthsAct->setChecked(piSettings->enable_external_synths);
}

void MainWindow::changeAudioSafeMode()
{
    QSignalBlocker blocker(audioSafeAct);
    audioSafeAct->setChecked(piSettings->check_args);
}

void MainWindow::midiDefaultChannelMenuChanged(int idx)
{
    piSettings->midi_default_channel = idx;
    emit settingsChanged();
    changeMidiDefaultChannel();
}

void MainWindow::logCuesMenuChanged()
{
    piSettings->log_cues = logCuesAct->isChecked();
    emit settingsChanged();
}

void MainWindow::changeLogCues()
{
    QSignalBlocker blocker(logCuesAct);
    logCuesAct->setChecked(piSettings->log_cues);
}

void MainWindow::logSynthsMenuChanged()
{
    piSettings->log_synths = logSynthsAct->isChecked();
    emit settingsChanged();
}

void MainWindow::changeLogSynths()
{
    QSignalBlocker blocker(logSynthsAct);
    logSynthsAct->setChecked(piSettings->log_synths);
}

void MainWindow::clearOutputOnRunMenuChanged()
{
    piSettings->clear_output_on_run = clearOutputOnRunAct->isChecked();
    emit settingsChanged();
}

void MainWindow::changeClearOutputOnRun()
{
    QSignalBlocker blocker(clearOutputOnRunAct);
    clearOutputOnRunAct->setChecked(piSettings->clear_output_on_run);
}

void MainWindow::autoIndentOnRunMenuChanged()
{
    piSettings->auto_indent_on_run = autoIndentOnRunAct->isChecked();
    emit settingsChanged();
}

void MainWindow::changeAutoIndentOnRun()
{
    QSignalBlocker blocker(autoIndentOnRunAct);
    if (piSettings->auto_indent_on_run)
    {
        showStatusAndAnnounce(tr("Auto Indent mode enabled"), 2000);
    }
    else
    {
        showStatusAndAnnounce(tr("Auto Indent mode disabled"), 2000);
    }

    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        SonicPiScintilla* ws = ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace();
        ws->setAutoIndentEnabled(piSettings->auto_indent_on_run);
    }

    autoIndentOnRunAct->setChecked(piSettings->auto_indent_on_run);
}

void MainWindow::changeMidiDefaultChannel()
{
    int idx = piSettings->midi_default_channel;

    int i = 0;
    foreach (QAction* action, ioMidiOutChannelMenu->actions())
    {
        if (i == idx)
        {
            const bool wasBlocked = action->blockSignals(true);
            action->setChecked(true);
            action->blockSignals(wasBlocked);
        }
        else
        {
            const bool wasBlocked = action->blockSignals(true);
            action->setChecked(false);
            action->blockSignals(wasBlocked);
        }

        i++;
    }
}

void MainWindow::changeShowLineNumbers()
{

    bool show = piSettings->show_line_numbers;

    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        SonicPiScintilla* ws = ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace();
        if (show)
        {
            ws->showLineNumbers();
        }
        else
        {
            ws->hideLineNumbers();
        }
    }

    QSignalBlocker blocker(showLineNumbersAct);
    showLineNumbersAct->setChecked(piSettings->show_line_numbers);
}

void MainWindow::changeShowAutoCompletion()
{
    bool show = piSettings->show_autocompletion;
    if (show)
    {
        showStatusAndAnnounce(tr("Show autocompletion on"), 2000);
    }
    else
    {
        showStatusAndAnnounce(tr("Show autocompletion off"), 2000);
    }

    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        SonicPiScintilla* ws = ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace();
        ws->showAutoCompletion(show);
    }

    QSignalBlocker blocker(showAutoCompletionAct);
    showAutoCompletionAct->setChecked(piSettings->show_autocompletion);
}

void MainWindow::changeShowCompletionHelp()
{
    bool show = piSettings->show_completion_help;
    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        SonicPiScintilla* ws = ((SonicPiEditor*)editorTabWidget->widget(i))->getWorkspace();
        ws->setCompletionHelp(show);
    }

    QSignalBlocker blocker(showCompletionHelpAct);
    showCompletionHelpAct->setChecked(piSettings->show_completion_help);
}

void MainWindow::changeShowContext()
{
    bool show = piSettings->show_context;
    if (show)
    {
        showStatusAndAnnounce(tr("Show context on"), 2000);
        for (int i = 0; i < editorTabWidget->count(); i++)
        {
            ((SonicPiEditor*)editorTabWidget->widget(i))->showContext();
        }
    }
    else
    {
        showStatusAndAnnounce(tr("Show context off"), 2000);
        for (int i = 0; i < editorTabWidget->count(); i++)
        {
            ((SonicPiEditor*)editorTabWidget->widget(i))->hideContext();
        }
    }

    QSignalBlocker blocker(showContextAct);
    showContextAct->setChecked(piSettings->show_context);
}

void MainWindow::changeSpeakTransport()
{
    QSignalBlocker blocker(speakTransportAct);
    speakTransportAct->setChecked(piSettings->speak_transport);
}

void MainWindow::changeReduceMotion()
{
    QSignalBlocker blocker(reduceMotionAct);
    reduceMotionAct->setChecked(piSettings->reduce_motion);
    // Same immediate-pause as the menu path — see reduceMotionMenuChanged().
    if (piSettings->reduce_motion)
        scopeWindow->Pause();
}

void MainWindow::togglePrefs()
{
    QSignalBlocker blocker(prefsAct);
    if (prefsWidget->isVisible())
    {
        showStatusAndAnnounce(tr("Hiding preferences..."), 2000);
        slidePrefsWidgetOut();
        prefsAct->setChecked(false);
    }
    else
    {
        showStatusAndAnnounce(tr("Showing preferences..."), 2000);

        slidePrefsWidgetIn();
        prefsAct->setChecked(true);
    }
    updatePrefsIcon();
}

void MainWindow::updatePrefsIcon()
{
    prefsAct->setIcon(theme->getPrefsIcon(prefsWidget->isVisible()));
}

void MainWindow::wheelEvent(QWheelEvent* event)
{
#if defined(Q_OS_WIN)
    if (event->modifiers() & Qt::ControlModifier)
    {
        SonicPiScintilla* ws = getCurrentWorkspace();
        if (event->angleDelta().y() > 0)
            ws->zoomFontIn();
        else
            ws->zoomFontOut();
    }
#else
    (void)event;
#endif
}

void MainWindow::stopRunningSynths()
{
    oscpkt::Message msg("/stop-all-jobs");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::clearOutputPanels()
{
    outputPane->clear();
    errorPane->clear();
}

QKeySequence MainWindow::ctrlKey(const QString& key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+%1").arg(key));
#endif
}

// Cmd on Mac, Alt everywhere else
QKeySequence MainWindow::metaKey(const QString& key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::shiftMetaKey(const QString& key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("Shift+alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::ctrlMetaKey(const QString& key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::ctrlShiftKey(const QString& key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Shift+Ctrl+%1").arg(key));
#endif
}

char MainWindow::int2char(int i)
{
    return '0' + i;
}

QString MainWindow::tooltipStrShiftMeta(const QString& key, const QString& str)
{
#ifdef Q_OS_MAC
    return QString("%1 (⇧⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (Shift-alt-%2)").arg(str).arg(key);
#endif
}

QString MainWindow::tooltipStrMeta(const QString& key, const QString& str)
{
#ifdef Q_OS_MAC
    return QString("%1 (⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (alt-%2)").arg(str).arg(key);
#endif
}

void MainWindow::updateAction(QAction* action, const QString& desc)
{
    QString shortcutDesc = action->shortcut().toString(QKeySequence::NativeText);
    // Tooltip is the description alone: the tooltip popup renders the
    // action's shortcut as a separate key-cap chip (see sonicpitooltip.h).
    action->setToolTip(desc);
    action->setText(action->iconText());
    action->setStatusTip(desc + " (" + shortcutDesc + ")");
}

QKeySequence MainWindow::resolveShortcut(QString keySequence)
{
    keySequence = keySequence.toLower().trimmed();

    if (keySequence.startsWith("shiftmeta+"))
    {
        return shiftMetaKey(keySequence.mid(10));
    }
    else if (keySequence.startsWith("metashift+"))
    {
        return shiftMetaKey(keySequence.mid(10));
    }
    else if (keySequence.startsWith("ctrlmeta+"))
    {
        return ctrlMetaKey(keySequence.mid(9));
    }
    else if (keySequence.startsWith("metactrl+"))
    {
        return ctrlMetaKey(keySequence.mid(9));
    }
    else if (keySequence.startsWith("ctrlshift+"))
    {
        return ctrlShiftKey(keySequence.mid(10));
    }
    else if (keySequence.startsWith("shiftctrl+"))
    {
        return ctrlShiftKey(keySequence.mid(10));
    }
    else if (keySequence.startsWith("meta+"))
    {
        return metaKey(keySequence.mid(5));
        // } else if (keySequence.startsWith("alt")) {
        //     QChar key = keySequence.mid(5, 1).at(0);
        //     return altKey(key.toLatin1());
    }
    else if (keySequence.startsWith("ctrl+"))
    {
        return ctrlKey(keySequence.mid(5));
    }
    else
    {
        return QKeySequence(keySequence); // Default case: if it’s a standard sequence like "Alt+Space"
    }
}

void MainWindow::updateShortcut(const QString& id, QAction* action, const QString& desc)
{
    action->setShortcut(shortcutMap[id]);
    updateAction(action, desc);
}

void MainWindow::resetShortcuts()
{
    shortcutMap.clear();
}

void MainWindow::loadUserShortcut(const QString& id, QSettings& shortcut_settings)
{
    // Custom mode is a base preset plus diffs: only overlay the ids the user
    // actually overrode, leaving the base value for everything else.
    if (shortcut_settings.contains(id))
    {
        shortcutMap[id] = resolveShortcut(shortcut_settings.value(id).toString());
    }
}

const QList<ShortcutDef>& MainWindow::shortcutDefs()
{
    static const QList<ShortcutDef> defs = {
    { "Run", QT_TR_NOOP("Run the code in the current buffer"), "Meta+Return", "Meta+Return", "Meta+Return", "Live", &MainWindow::runAct, "Meta+R" },
    { "Stop", QT_TR_NOOP("Stop all running code"), "Meta+.", "Meta+.", "Meta+.", "Live", &MainWindow::stopAct, "Meta+S" },
    { "Record", QT_TR_NOOP("Start recording to a WAV audio file"), "ShiftMeta+R", "ShiftMeta+R", "ShiftMeta+R", "Live", &MainWindow::recAct },
    // Deliberately NOT plain Ctrl+S on win: the action is Save-As (a modal
    // file dialog every press — hostile to muscle memory mid-performance),
    // and buffers autosave so a quick-save has no job to do.
    { "Save", QT_TR_NOOP("Save current buffer as an external file"), "ShiftMeta+S", "CtrlShift+S", "ShiftMeta+S", "Live", &MainWindow::saveAsAct },
    { "Load", QT_TR_NOOP("Load an external file in the current buffer"), "Ctrl+O", "Ctrl+O", "ShiftMeta+O", "Live", &MainWindow::loadFileAct },
    { "Align", QT_TR_NOOP("Align code to improve readability"), "Meta+M", "Meta+M", "Meta+M", "Code", &MainWindow::textAlignAct },
    { "Comment", QT_TR_NOOP("Comment/Uncomment code"), "Meta+/", "Meta+/", "Meta+/", "Code", &MainWindow::textCommentAct },
    // Emacs column keeps Ctrl+f as forward-char and gets classic isearch on
    // Ctrl+s / Ctrl+r instead; the win column's Ctrl+f is reclaimed from
    // "Right" below. While the find bar is focused, Ctrl+s / Ctrl+r repeat
    // the search in either direction (see FindPopup::eventFilter).
    { "Find", QT_TR_NOOP("Find text in the current buffer"), "Meta+f", "Ctrl+f", "Ctrl+s", "Code", &MainWindow::findAct },
    { "FindNext", QT_TR_NOOP("Jump to the next match"), "Meta+g", "F3", "", "Code", &MainWindow::findNextAct },
    { "FindPrev", QT_TR_NOOP("Jump to the previous match"), "ShiftMeta+g", "Shift+F3", "Ctrl+r", "Code", &MainWindow::findPrevAct },
    { "Transpose", QT_TR_NOOP("Transpose Characters"), "Ctrl+T", "Ctrl+T", "Ctrl+T", "Code", &MainWindow::textTransposeAct },
    // Win column avoids Ctrl+Alt combos: Windows delivers AltGr as Ctrl+Alt,
    // so e.g. Ctrl+Alt+N would swallow "ń" on a Polish layout.
    { "ShiftUp", QT_TR_NOOP("Shift Line or Selection Up"), "Alt+Up", "Alt+Up", "CtrlMeta+P", "Code", &MainWindow::textShiftLineUpAct },
    { "ShiftDown", QT_TR_NOOP("Shift Line or Selection Down"), "Alt+Down", "Alt+Down", "CtrlMeta+N", "Code", &MainWindow::textShiftLineDownAct },
    { "ContextualDocs", QT_TR_NOOP("Look up documentation for the current word"), "CtrlMeta+i", "Shift+F1", "CtrlMeta+i", "Focus", &MainWindow::contextHelpAct, "Shift+F1" },
    { "TextZoomIn", QT_TR_NOOP("Increase Text Size"), "Meta+=", "Ctrl++", "Meta+=", "View", &MainWindow::textIncAct },
    // Win text-zoom mirrors zoom-in's shifted symbol (Ctrl++), so out is Ctrl+_
    // (Ctrl+Shift+-). Plain Ctrl+- is the log zoom-out; sharing it here made both
    // an ambiguous shortcut that fired neither.
    { "TextZoomOut", QT_TR_NOOP("Decrease Text Size"), "Meta+-", "Ctrl+_", "Meta+-", "View", &MainWindow::textDecAct },
    { "Scope", QT_TR_NOOP("Toggle visibility of audio oscilloscope"), "Meta+O", "Meta+O", "Meta+O", "Visuals", &MainWindow::scopeAct },
    { "CycleThemes", QT_TR_NOOP("Cycle through the available colour themes"), "ShiftMeta+M", "ShiftMeta+M", "ShiftMeta+M", "Visuals", &MainWindow::cycleThemesAct },
    { "Info", QT_TR_NOOP("Toggle information about Sonic Pi"), "Meta+n", "Meta+1", "Meta+1", "View", &MainWindow::infoAct },
    { "Help", QT_TR_NOOP("Toggle the visibility of the help pane"), "Meta+?", "F1", "Meta+i", "View", &MainWindow::helpAct, "F1" },
    { "Prefs", QT_TR_NOOP("Toggle the visibility of the preferences pane"), "Meta+p", "Meta+p", "Meta+p", "View", &MainWindow::prefsAct },
    { "TabPrev", QT_TR_NOOP("Switch to the previous tab"), "ShiftMeta+[", "ShiftMeta+[", "ShiftMeta+[", "Focus", &MainWindow::tabPrevAct },
    { "TabNext", QT_TR_NOOP("Switch to the next tab"), "ShiftMeta+]", "ShiftMeta+]", "ShiftMeta+]", "Focus", &MainWindow::tabNextAct },
    { "Tab1", QT_TR_NOOP("Switch to tab 1"), "Meta+1", "ShiftMeta+1", "ShiftMeta+1", "Focus", &MainWindow::tab1Act },
    { "Tab2", QT_TR_NOOP("Switch to tab 2"), "Meta+2", "ShiftMeta+2", "ShiftMeta+2", "Focus", &MainWindow::tab2Act },
    { "Tab3", QT_TR_NOOP("Switch to tab 3"), "Meta+3", "ShiftMeta+3", "ShiftMeta+3", "Focus", &MainWindow::tab3Act },
    { "Tab4", QT_TR_NOOP("Switch to tab 4"), "Meta+4", "ShiftMeta+4", "ShiftMeta+4", "Focus", &MainWindow::tab4Act },
    { "Tab5", QT_TR_NOOP("Switch to tab 5"), "Meta+5", "ShiftMeta+5", "ShiftMeta+5", "Focus", &MainWindow::tab5Act },
    { "Tab6", QT_TR_NOOP("Switch to tab 6"), "Meta+6", "ShiftMeta+6", "ShiftMeta+6", "Focus", &MainWindow::tab6Act },
    { "Tab7", QT_TR_NOOP("Switch to tab 7"), "Meta+7", "ShiftMeta+7", "ShiftMeta+7", "Focus", &MainWindow::tab7Act },
    { "Tab8", QT_TR_NOOP("Switch to tab 8"), "Meta+8", "ShiftMeta+8", "ShiftMeta+8", "Focus", &MainWindow::tab8Act },
    { "Tab9", QT_TR_NOOP("Switch to tab 9"), "Meta+9", "ShiftMeta+9", "ShiftMeta+9", "Focus", &MainWindow::tab9Act },
    { "Tab0", QT_TR_NOOP("Switch to tab 0"), "Meta+0", "ShiftMeta+0", "ShiftMeta+0", "Focus", &MainWindow::tab0Act },
    { "Link", QT_TR_NOOP("Connect or disconnect the Link Metronome from the network"), "Meta+t", "Meta+t", "Meta+t", "Audio", &MainWindow::enableLinkAct },
    { "TapTempo", QT_TR_NOOP("Click Link Tap Tempo"), "Shift+Return", "Shift+Return", "Shift+Return", "Audio", &MainWindow::linkTapTempoAct },
    { "CycleFocusForward", QT_TR_NOOP("Move focus to the next visible pane"), "F6", "F6", "F6", "Focus", &MainWindow::cycleFocusForwardAct },
    { "CycleFocusBack", QT_TR_NOOP("Move focus to the previous visible pane"), "Shift+F6", "Shift+F6", "Shift+F6", "Focus", &MainWindow::cycleFocusBackAct },
    { "FocusEditor", QT_TR_NOOP("Place focus on the code editor"), "CtrlShift+e", "CtrlShift+e", "CtrlShift+e", "Focus", &MainWindow::focusEditorAct },
    { "FocusLogs", QT_TR_NOOP("Place focus on the logs"), "CtrlShift+l", "CtrlShift+l", "CtrlShift+l", "Focus", &MainWindow::focusLogsAct },
    { "FocusContext", QT_TR_NOOP("Place focus on the context pane"), "CtrlShift+t", "CtrlShift+t", "CtrlShift+t", "Focus", &MainWindow::focusContextAct },
    { "FocusCues", QT_TR_NOOP("Place focus on the cue event pane"), "CtrlShift+c", "CtrlShift+c", "CtrlShift+c", "Focus", &MainWindow::focusCuesAct },
    { "FocusPrefs", QT_TR_NOOP("Place focus on preferences"), "Meta+,", "Meta+,", "CtrlShift+p", "Focus", &MainWindow::focusPreferencesAct },
    { "FocusHelpListing", QT_TR_NOOP("Place focus on help listing"), "CtrlShift+h", "CtrlShift+h", "CtrlShift+h", "Focus", &MainWindow::focusHelpListingAct },
    { "FocusHelpDetails", QT_TR_NOOP("Place focus on help details"), "CtrlShift+d", "CtrlShift+d", "CtrlShift+d", "Focus", &MainWindow::focusHelpDetailsAct },
    { "FocusErrors", QT_TR_NOOP("Place focus on errors"), "CtrlShift+R", "CtrlShift+R", "CtrlShift+R", "Focus", &MainWindow::focusErrorsAct },
    { "FocusBPMScrubber", QT_TR_NOOP("Place focus on BPM Scrubber"), "CtrlShift+b", "CtrlShift+b", "CtrlShift+b", "Focus", &MainWindow::focusBPMScrubberAct },
    { "FocusTimeWarpScrubber", QT_TR_NOOP("Place focus on TimeWarp Scrubber"), "CtrlShift+w", "CtrlShift+w", "CtrlShift+w", "Focus", &MainWindow::focusTimeWarpScrubberAct },
    { "ShowButtons", QT_TR_NOOP("Show or hide the buttons"), "ShiftMeta+b", "ShiftMeta+b", "ShiftMeta+b", "View", &MainWindow::showButtonsAct },
    { "ShowCueLog", QT_TR_NOOP("Show or hide the cue log"), "ShiftMeta+c", "ShiftMeta+c", "ShiftMeta+c", "View", &MainWindow::showCuesAct },
    { "ShowLog", QT_TR_NOOP("Show or hide the log"), "ShiftMeta+l", "ShiftMeta+l", "ShiftMeta+l", "View", &MainWindow::showLogAct },
    { "SetMark", QT_TR_NOOP("Set a mark in the text"), "CtrlShift+Space", "CtrlShift+Space", "Ctrl+Space", "Code", &MainWindow::textSetMarkAct },
    { "TriggerAutocomplete", QT_TR_NOOP("Trigger code completion"), "Ctrl+Space", "Ctrl+Space", "CtrlMeta+Space", "Code", &MainWindow::triggerAutocompleteAct },
    { "ReadCompletionDetails", QT_TR_NOOP("Show autocomplete documentation for the current context"), "Ctrl+i", "Ctrl+i", "Ctrl+i", "Accessibility", &MainWindow::readCompletionDetailsAct },
    { "LogZoomIn", QT_TR_NOOP("Zoom in the log"), "Ctrl+=", "Ctrl+=", "Ctrl+=", "View", &MainWindow::logZoomInAct },
    { "LogZoomOut", QT_TR_NOOP("Zoom out the log"), "Ctrl+-", "Ctrl+-", "Ctrl+-", "View", &MainWindow::logZoomOutAct },
    { "Down", QT_TR_NOOP("Move Cursor Down"), "Ctrl+n", "Ctrl+n", "Ctrl+n", "Code", &MainWindow::textDownAct },
    { "Up", QT_TR_NOOP("Move Cursor Up"), "Ctrl+p", "Ctrl+p", "Ctrl+p", "Code", &MainWindow::textUpAct },
    { "UpTen", QT_TR_NOOP("Move Cursor Up 10 Lines"), "Meta+up", "PgUp", "ShiftMeta+u", "Code", &MainWindow::textUpTenAct },
    { "DownTen", QT_TR_NOOP("Move Cursor Down 10 Lines"), "Meta+down", "PgDown", "ShiftMeta+d", "Code", &MainWindow::textDownTenAct },
    { "CutToEnd", QT_TR_NOOP("Cut to the end of the line"), "Ctrl+k", "Ctrl+k", "Ctrl+k", "Code", &MainWindow::textCutToEndOfLineAct },
    { "Copy", QT_TR_NOOP("Copy the current selection"), "Meta+c", "Ctrl+c", "Meta+]", "Code", &MainWindow::textCopyAct },
    { "Cut", QT_TR_NOOP("Cut the current selection"), "Meta+x", "Ctrl+x", "Ctrl+]", "Code", &MainWindow::textCutAct },
    { "Paste", QT_TR_NOOP("Paste the current selection"), "Meta+v", "Ctrl+v", "Ctrl+y", "Code", &MainWindow::textPasteAct },
    // Win column: Ctrl+f belongs to Find (the platform convention); arrow keys
    // cover the motion. Mac's Ctrl+f is the real Control key (Cmd+F is
    // "Meta+f"), i.e. macOS's native forward-char — no clash with Find.
    { "Right", QT_TR_NOOP("Move Cursor Right"), "Ctrl+f", "", "Ctrl+f", "Code", &MainWindow::textRightAct },
    { "Left", QT_TR_NOOP("Move Cursor Left"), "Ctrl+b", "Ctrl+b", "Ctrl+b", "Code", &MainWindow::textLeftAct },
    { "DeleteForward", QT_TR_NOOP("Delete Right"), "Ctrl+d", "Ctrl+d", "Ctrl+d", "Code", &MainWindow::textDeleteForwardAct },
    { "DeleteBackward", QT_TR_NOOP("Delete Left"), "Ctrl+h", "Ctrl+h", "Ctrl+h", "Code", &MainWindow::textDeleteBackAct },
    { "LineStart", QT_TR_NOOP("Move Cursor to Start of Line"), "Meta+Left", "Home", "Ctrl+a", "Code", &MainWindow::textLineStartAct },
    { "LineEnd", QT_TR_NOOP("Move Cursor to End of Line"), "Meta+Right", "End", "Ctrl+e", "Code", &MainWindow::textLineEndAct },
    { "DocStart", QT_TR_NOOP("Move Cursor to Start of Document"), "MetaShift+,", "Ctrl+Home", "MetaShift+,", "Code", &MainWindow::textDocStartAct },
    { "DocEnd", QT_TR_NOOP("Move Cursor to End of Document"), "MetaShift+.", "Ctrl+End", "MetaShift+.", "Code", &MainWindow::textDocEndAct },
    { "WordRight", QT_TR_NOOP("Move Cursor Right by Word"), "Alt+Right", "Ctrl+Right", "Meta+f", "Code", &MainWindow::textWordRightAct },
    { "WordLeft", QT_TR_NOOP("Move Cursor Left by Word"), "Alt+Left", "Ctrl+Left", "Meta+b", "Code", &MainWindow::textWordLeftAct },
    { "SelectLineStart", QT_TR_NOOP("Select to Start of Line"), "ShiftMeta+Left", "Shift+Home", "CtrlShift+a", "Code", &MainWindow::textSelectLineStartAct },
    { "SelectLineEnd", QT_TR_NOOP("Select to End of Line"), "ShiftMeta+Right", "Shift+End", "CtrlShift+e", "Code", &MainWindow::textSelectLineEndAct },
    { "SelectWordRight", QT_TR_NOOP("Select Word Right"), "Alt+Shift+Right", "CtrlShift+Right", "CtrlShift+Right", "Code", &MainWindow::textSelectWordRightAct },
    { "SelectWordLeft", QT_TR_NOOP("Select Word Left"), "Alt+Shift+Left", "CtrlShift+Left", "CtrlShift+Left", "Code", &MainWindow::textSelectWordLeftAct },
    { "SelectDocStart", QT_TR_NOOP("Select to Start of Document"), "CtrlShift+Home", "CtrlShift+Home", "CtrlShift+Home", "Code", &MainWindow::textSelectDocStartAct },
    { "SelectDocEnd", QT_TR_NOOP("Select to End of Document"), "CtrlShift+End", "CtrlShift+End", "CtrlShift+End", "Code", &MainWindow::textSelectDocEndAct },
    { "CenterVertically", QT_TR_NOOP("Vertically center the caret in the editor"), "Ctrl+l", "Ctrl+l", "Ctrl+l", "Code", &MainWindow::textCenterCaretAct },
    { "Undo", QT_TR_NOOP("Undo the last action"), "Meta+z", "Ctrl+z", "Meta+z", "Code", &MainWindow::textUndoAct },
    { "Redo", QT_TR_NOOP("Redo the last undo"), "ShiftMeta+z", "ShiftCtrl+z", "ShiftMeta+z", "Code", &MainWindow::textRedoAct },
    { "SelectAll", QT_TR_NOOP("Select all text"), "Meta+a", "Ctrl+a", "Meta+a", "Code", &MainWindow::textSelectAllAct },
    // Win column follows the native Windows editing conventions
    // (Ctrl+Backspace/Delete for word deletes, Visual Studio's Ctrl(+Shift)+U
    // for case) — this also frees Alt+letter combos for menu mnemonics.
    { "DeleteWordRight", QT_TR_NOOP("Delete word to the right"), "Alt+Shift+Backspace", "Ctrl+Delete", "Meta+d", "Code", &MainWindow::textDeleteWordRightAct },
    { "DeleteWordLeft", QT_TR_NOOP("Delete word to the left"), "Alt+Backspace", "Ctrl+Backspace", "Meta+Backspace", "Code", &MainWindow::textDeleteWordLeftAct },
    { "UpcaseWord", QT_TR_NOOP("Uppercase word or selection"), "Meta+u", "CtrlShift+u", "Meta+u", "Code", &MainWindow::textUpcaseWordAct },
    { "DowncaseWord", QT_TR_NOOP("Lowercase word or selection"), "Meta+l", "Ctrl+u", "Meta+l", "Code", &MainWindow::textDowncaseWordAct },
    { "FullScreen", QT_TR_NOOP("Toggle fullscreen mode"), "ShiftMeta+f", "F11", "ShiftMeta+f", "View", &MainWindow::fullScreenAct },
    // F10 reserved for menu bar access on Windows/Linux (used by screen readers)
    { "FocusMode", QT_TR_NOOP("Toggle focus mode (fullscreen editor with all distractions hidden)"), "F10", "Ctrl+F10", "Ctrl+F10", "View", &MainWindow::focusModeAct },
    { "ScopePaused", QT_TR_NOOP("Pause or resume the audio oscilloscopes"), "F12", "F12", "F12", "Visuals", &MainWindow::scopePausedAct },
    };
    return defs;
}

void MainWindow::loadUserShortcuts()
{
    QString shortcuts_path = shortcutsConfigPath();
    QFile shortcutFile(shortcuts_path);
    QString base = "none";

    QSettings* shortcut_settings; // Pointer to QSettings

    // Check if the file exists before proceeding
    if (shortcutFile.exists())
    {
        shortcut_settings = new QSettings(shortcuts_path, QSettings::IniFormat);
        base = shortcut_settings->value("base", "none").toString().toLower();
    }
    else
    {
        // Create default QSettings in memory
        qDebug() << "Shortcut file not found, using default shortcuts.";
        shortcut_settings = new QSettings(QSettings::IniFormat, QSettings::UserScope, "defaultOrganization", "defaultApplication");
    }

    // Determine which preset to base the custom shortcuts on. An unset/unknown
    // base falls back to the platform default so Custom mode is never empty.
    if (base == "win")
    {
        loadWinShortcuts();
    }
    else if (base == "emacs")
    {
        loadEmacsShortcuts();
    }
    else if (base == "mac")
    {
        loadMacShortcuts();
    }
    else
    {
#if defined(Q_OS_WIN)
        loadWinShortcuts();
#elif defined(Q_OS_MAC)
        loadMacShortcuts();
#else
        loadEmacsShortcuts();
#endif
    }

    // Overlay the user's shortcuts from the .ini over the base
    QSet<QString> overridden;
    for (const ShortcutDef& d : shortcutDefs())
    {
        if (shortcut_settings->contains(d.id))
            overridden.insert(QString::fromLatin1(d.id));
        loadUserShortcut(d.id, *shortcut_settings);
    }

    // A user override wins over a colliding base default: base presets can
    // change between releases, and a new default landing on a key the user
    // already assigned would make both bindings ambiguous (Qt then fires
    // neither). Unbind the base-default side of any such collision.
    for (auto it = shortcutMap.begin(); it != shortcutMap.end(); ++it)
    {
        if (it.value().isEmpty() || overridden.contains(it.key()))
            continue;
        for (const QString& id : overridden)
        {
            if (shortcutMap.value(id) == it.value())
            {
                it.value() = QKeySequence();
                break;
            }
        }
    }

    // Clean up the dynamically allocated QSettings object
    delete shortcut_settings;
}

void MainWindow::applyUserShortcuts(const QString& base, const QMap<QString, QString>& keys)
{
    QString shortcuts_path = shortcutsConfigPath();
    QSettings shortcut_settings(shortcuts_path, QSettings::IniFormat);
    shortcut_settings.clear();
    shortcut_settings.setValue("base", base);
    for (auto it = keys.constBegin(); it != keys.constEnd(); ++it)
    {
        shortcut_settings.setValue(it.key(), it.value());
    }
    shortcut_settings.sync();

    piSettings->shortcut_mode = 4;
    gui_settings->setValue("prefs/shortcut-mode", piSettings->shortcut_mode);
    updateShortcuts();
}

void MainWindow::updateShortcuts()
{
    resetShortcuts();
    QSignalBlocker macShortcutBlocker(macShortcutModeAct);
    macShortcutModeAct->setChecked(false);
    QSignalBlocker winShortcutBlocker(winShortcutModeAct);
    winShortcutModeAct->setChecked(false);
    QSignalBlocker userShortcutBlocker(userShortcutModeAct);
    userShortcutModeAct->setChecked(false);
    QSignalBlocker emacsShortcutBlocker(emacsShortcutModeAct);
    emacsShortcutModeAct->setChecked(false);

    if (piSettings->shortcut_mode == 2)
    {
        winShortcutModeAct->setChecked(true);
        loadWinShortcuts();
    }
    else if (piSettings->shortcut_mode == 3)
    {
        macShortcutModeAct->setChecked(true);

        loadMacShortcuts();
    }
    else if (piSettings->shortcut_mode == 4)
    {
        userShortcutModeAct->setChecked(true);
        loadUserShortcuts();
    }
    else
    {
        piSettings->shortcut_mode = 1;
        emacsShortcutModeAct->setChecked(true);
        loadEmacsShortcuts();
    }

#ifndef Q_OS_MAC
    // F10 is the platform menu-bar key on Windows/Linux (relied on by screen
    // readers); never bind it bare, whichever keymap or custom ini is chosen.
    if (shortcutMap["FocusMode"] == QKeySequence("F10"))
    {
        shortcutMap["FocusMode"] = QKeySequence("Ctrl+F10");
    }
#endif

    // Every primary binding currently in force, so an undocumented secondary
    // never shadows a key the keymap (or the user's custom overlay) has given
    // to a different action — two actions on one key is ambiguous in Qt and
    // silently triggers neither.
    QSet<QString> primaries;
    for (auto it = shortcutMap.cbegin(); it != shortcutMap.cend(); ++it)
    {
        if (!it.value().isEmpty())
            primaries.insert(it.value().toString(QKeySequence::PortableText));
    }

    for (const ShortcutDef& d : shortcutDefs())
    {
        if (QAction* act = this->*(d.act))
        {
            updateShortcut(d.id, act, tr(d.desc));
            // Optional secondary shortcut (undocumented fallback) — both trigger
            // the same action, so setShortcuts() carries them without ambiguity.
            // Skip it when the keymap's primary is already the same key, or when
            // the key belongs to another action's primary.
            if (d.secondary && d.secondary[0])
            {
                const QKeySequence secondary = resolveShortcut(QString::fromLatin1(d.secondary));
                const QString secondaryStr = secondary.toString(QKeySequence::PortableText);
                const bool ownPrimary =
                    shortcutMap.value(d.id).toString(QKeySequence::PortableText) == secondaryStr;
                if ((ownPrimary || !primaries.contains(secondaryStr))
                    && !act->shortcuts().contains(secondary))
                    act->setShortcuts(act->shortcuts() << secondary);
            }
        }
    }
    addMenuBarMnemonics();
    // show code context
    // show metronome
}

void MainWindow::loadMacShortcuts()
{
    for (const ShortcutDef& d : shortcutDefs())
    {
        shortcutMap[d.id] = resolveShortcut(d.mac);
    }
}

void MainWindow::loadWinShortcuts()
{
    for (const ShortcutDef& d : shortcutDefs())
    {
        shortcutMap[d.id] = resolveShortcut(d.win);
    }
}

void MainWindow::loadEmacsShortcuts()
{
    for (const ShortcutDef& d : shortcutDefs())
    {
        shortcutMap[d.id] = resolveShortcut(d.emacs);
    }
}

void MainWindow::createToolBar()
{
    exitAct = new QAction(tr("Exit"), this);
    connect(exitAct, &QAction::triggered, qApp, &QApplication::closeAllWindows);

    std::cout << "[GUI] - creating tool bar" << std::endl;

    runAct = new QAction(theme->getRunIcon(), tr("Run"), this);
    connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

    // Stop
    stopAct = new QAction(theme->getStopIcon(), tr("Stop"), this);
    connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

    // Record
    recAct = new QAction(theme->getRecIcon(false, false), tr("Start Recording"), this);
    recAct->setCheckable(true);
    connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Mode-selection actions shared by the IO menubar, the rec-button
    // right-click menu, and the Preferences radio buttons.
    {
        QActionGroup* recModeGrp = new QActionGroup(this);
        recModeGrp->setExclusive(true);

        recAudioModeAct = new QAction(tr("Record Audio Only"), this);
        recAudioModeAct->setCheckable(true);
        recModeGrp->addAction(recAudioModeAct);
        connect(recAudioModeAct, &QAction::triggered, this, [this]() {
            setRecordingMode(static_cast<int>(SonicPiSettings::Audio));
        });

        recAudioVideoModeAct = new QAction(tr("Record Audio + Video"), this);
        recAudioVideoModeAct->setCheckable(true);
        recModeGrp->addAction(recAudioVideoModeAct);
        connect(recAudioVideoModeAct, &QAction::triggered, this, [this]() {
            setRecordingMode(static_cast<int>(SonicPiSettings::AudioAndVideo));
        });

        if (piSettings->recording_type == SonicPiSettings::AudioAndVideo) {
            recAudioVideoModeAct->setChecked(true);
        } else {
            recAudioModeAct->setChecked(true);
        }
    }
#endif

    // Save
    saveAsAct = new QAction(theme->getSaveAsIcon(), tr("Save Buffer As..."), this);
    saveAsAct->setIconText(tr("Save Buffer As..."));
    connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

    // Load
    loadFileAct = new QAction(theme->getLoadIcon(), tr("Load into Buffer..."), this);
    loadFileAct->setIconText(tr("Load into Buffer..."));
    connect(loadFileAct, SIGNAL(triggered()), this, SLOT(loadFile()));

    // Sets
    loadSetAct = new QAction(tr("Load Set..."), this);
    connect(loadSetAct, SIGNAL(triggered()), this, SLOT(loadSet()));

    saveSetAct = new QAction(tr("Save Set"), this);
    connect(saveSetAct, SIGNAL(triggered()), this, SLOT(saveSet()));

    saveSetAsAct = new QAction(tr("Save Set As..."), this);
    connect(saveSetAsAct, SIGNAL(triggered()), this, SLOT(saveSetAs()));

    clearAllBuffersAct = new QAction(tr("Clear All Buffers..."), this);
    connect(clearAllBuffersAct, SIGNAL(triggered()), this, SLOT(clearAllBuffers()));

    // Align
    textAlignAct = new QAction(QIcon(":/images/align.png"), tr("Align Code"), this);
    connect(textAlignAct, SIGNAL(triggered()), this, SLOT(beautifyCode()));

    // Comment
    textCommentAct = new QAction(QIcon(":/images/align.png"), tr("Comment/Uncomment code"), this);
    connect(textCommentAct, SIGNAL(triggered()), this, SLOT(toggleCommentInCurrentWorkspace()));

    // Transpose Characters
    textTransposeAct = new QAction(tr("Transpose Characters"), this);
    connect(textTransposeAct, SIGNAL(triggered()), this, SLOT(transposeCharsInCurrentWorkspace()));

    // Move Up
    textShiftLineUpAct = new QAction(tr("Move Line or Selection Up"), this);
    connect(textShiftLineUpAct, SIGNAL(triggered()), this, SLOT(moveLineOrSelectionUpInCurrentWorkspace()));

    // Move Down
    textShiftLineDownAct = new QAction(tr("Move Line or Selection Down"), this);
    connect(textShiftLineDownAct, SIGNAL(triggered()), this, SLOT(moveLineOrSelectionDownInCurrentWorkspace()));

    textDownAct = new QAction(tr("Move Down One Line"), this);
    connect(textDownAct, SIGNAL(triggered()), this, SLOT(forwardOneLineInCurrentWorkspace()));

    textUpAct = new QAction(tr("Move Up One Line"), this);
    connect(textUpAct, SIGNAL(triggered()), this, SLOT(backOneLineInCurrentWorkspace()));

    textDownTenAct = new QAction(tr("Move Down Ten Lines"), this);
    connect(textDownTenAct, SIGNAL(triggered()), this, SLOT(forwardTenLinesInCurrentWorkspace()));

    textUpTenAct = new QAction(tr("Move Up Ten Lines"), this);
    connect(textUpTenAct, SIGNAL(triggered()), this, SLOT(backTenLinesInCurrentWorkspace()));

    textCutToEndOfLineAct = new QAction(tr("Cut to End of Line"), this);
    connect(textCutToEndOfLineAct, SIGNAL(triggered()), this, SLOT(cutLineFromPointInCurrentWorkspace()));

    textCopyAct = new QAction(tr("Copy"), this);
    connect(textCopyAct, SIGNAL(triggered()), this, SLOT(copyInCurrentWorkspace()));

    textCutAct = new QAction(tr("Cut"), this);
    connect(textCutAct, SIGNAL(triggered()), this, SLOT(cutInCurrentWorkspace()));

    textPasteAct = new QAction(tr("Paste"), this);
    connect(textPasteAct, SIGNAL(triggered()), this, SLOT(pasteInCurrentWorkspace()));

    textRightAct = new QAction(tr("Move Right"), this);
    connect(textRightAct, SIGNAL(triggered()), this, SLOT(rightInCurrentWorkspace()));

    findAct = new QAction(tr("Find..."), this);
    connect(findAct, SIGNAL(triggered()), this, SLOT(showFindInCurrentWorkspace()));

    findNextAct = new QAction(tr("Find Next"), this);
    connect(findNextAct, SIGNAL(triggered()), this, SLOT(findNextInCurrentWorkspace()));

    findPrevAct = new QAction(tr("Find Previous"), this);
    connect(findPrevAct, SIGNAL(triggered()), this, SLOT(findPrevInCurrentWorkspace()));

    textLeftAct = new QAction(tr("Move Left"), this);
    connect(textLeftAct, SIGNAL(triggered()), this, SLOT(leftInCurrentWorkspace()));

    textDeleteForwardAct = new QAction(tr("Delete Forward"), this);
    connect(textDeleteForwardAct, SIGNAL(triggered()), this, SLOT(deleteForwardInCurrentWorkspace()));

    textDeleteBackAct = new QAction(tr("Delete Back"), this);
    connect(textDeleteBackAct, SIGNAL(triggered()), this, SLOT(deleteBackwardInCurrentWorkspace()));

    textLineStartAct = new QAction(tr("Move to Start of Line"), this);
    connect(textLineStartAct, SIGNAL(triggered()), this, SLOT(lineStartInCurrentWorkspace()));

    textLineEndAct = new QAction(tr("Move to End of Line"), this);
    connect(textLineEndAct, SIGNAL(triggered()), this, SLOT(lineEndInCurrentWorkspace()));

    textDocStartAct = new QAction(tr("Move to Start of Document"), this);
    connect(textDocStartAct, SIGNAL(triggered()), this, SLOT(documentStartInCurrentWorkspace()));

    textDocEndAct = new QAction(tr("Move to End of Document"), this);
    connect(textDocEndAct, SIGNAL(triggered()), this, SLOT(documentEndInCurrentWorkspace()));

    textWordRightAct = new QAction(tr("Move Right One Word"), this);
    connect(textWordRightAct, SIGNAL(triggered()), this, SLOT(wordRightInCurrentWorkspace()));

    textWordLeftAct = new QAction(tr("Move Left One Word"), this);
    connect(textWordLeftAct, SIGNAL(triggered()), this, SLOT(wordLeftInCurrentWorkspace()));

    textSelectLineStartAct = new QAction(tr("Select to Start of Line"), this);
    connect(textSelectLineStartAct, SIGNAL(triggered()), this, SLOT(selectLineStartInCurrentWorkspace()));

    textSelectLineEndAct = new QAction(tr("Select to End of Line"), this);
    connect(textSelectLineEndAct, SIGNAL(triggered()), this, SLOT(selectLineEndInCurrentWorkspace()));

    textSelectWordRightAct = new QAction(tr("Select Word Right"), this);
    connect(textSelectWordRightAct, SIGNAL(triggered()), this, SLOT(selectWordRightInCurrentWorkspace()));

    textSelectWordLeftAct = new QAction(tr("Select Word Left"), this);
    connect(textSelectWordLeftAct, SIGNAL(triggered()), this, SLOT(selectWordLeftInCurrentWorkspace()));

    textSelectDocStartAct = new QAction(tr("Select to Start of Document"), this);
    connect(textSelectDocStartAct, SIGNAL(triggered()), this, SLOT(selectDocStartInCurrentWorkspace()));

    textSelectDocEndAct = new QAction(tr("Select to End of Document"), this);
    connect(textSelectDocEndAct, SIGNAL(triggered()), this, SLOT(selectDocEndInCurrentWorkspace()));

    textCenterCaretAct = new QAction(tr("Center Cursor Vertically"), this);
    connect(textCenterCaretAct, SIGNAL(triggered()), this, SLOT(centerCaretInCurrentWorkspace()));

    textUndoAct = new QAction(tr("Undo"), this);
    connect(textUndoAct, SIGNAL(triggered()), this, SLOT(undoInCurrentWorkspace()));

    textRedoAct = new QAction(tr("Redo"), this);
    connect(textRedoAct, SIGNAL(triggered()), this, SLOT(redoInCurrentWorkspace()));

    textSelectAllAct = new QAction(tr("Select All"), this);
    connect(textSelectAllAct, SIGNAL(triggered()), this, SLOT(selectAllInCurrentWorkspace()));

    textDeleteWordLeftAct = new QAction(tr("Delete Word Left"), this);
    connect(textDeleteWordLeftAct, SIGNAL(triggered()), this, SLOT(deleteWordLeftInCurrentWorkspace()));

    textDeleteWordRightAct = new QAction(tr("Delete Word Right"), this);
    connect(textDeleteWordRightAct, SIGNAL(triggered()), this, SLOT(deleteWordRightInCurrentWorkspace()));

    textUpcaseWordAct = new QAction(tr("Upcase Word or Selection"), this);
    connect(textUpcaseWordAct, SIGNAL(triggered()), this, SLOT(upcaseWordOrSelectionInCurrentWorkspace()));

    textDowncaseWordAct = new QAction(tr("Downcase Word or Selection"), this);
    connect(textDowncaseWordAct, SIGNAL(triggered()), this, SLOT(downcaseWordOrSelectionInCurrentWorkspace()));

    // Contextual Docs
    contextHelpAct = new QAction(tr("Show Docs for Current Word"), this);
    connect(contextHelpAct, SIGNAL(triggered()), this, SLOT(helpContext()));

    // Font Size Increase
    textIncAct = new QAction(theme->getTextIncIcon(), tr("Code Size Up"), this);
    connect(textIncAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceIn()));

    // Font Size Decrease
    textDecAct = new QAction(theme->getTextDecIcon(), tr("Code Size Down"), this);
    connect(textDecAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceOut()));

    // Scope
    scopeAct = new QAction(theme->getScopeIcon(false), tr("Show Scopes"), this);
    scopeAct->setCheckable(true);
    scopeAct->setChecked(piSettings->show_scopes);
    connect(scopeAct, SIGNAL(triggered()), this, SLOT(toggleScope()));

    scopePausedAct = new QAction(tr("Pause or Resume Scopes"), this);
    connect(scopePausedAct, SIGNAL(triggered()), this, SLOT(toggleScopePaused()));

    focusModeAct = new QAction(tr("Focus Mode"), this);
    focusModeAct->setCheckable(true);
    focusModeAct->setChecked(false);
    connect(focusModeAct, SIGNAL(triggered()), this, SLOT(toggleFocusMode()));

    checkUpdatesAct = new QAction(tr("Check for Updates on Launch"), this);
    checkUpdatesAct->setCheckable(true);
    checkUpdatesAct->setChecked(piSettings->check_updates);
    connect(checkUpdatesAct, &QAction::triggered, this, [this]() {
        piSettings->check_updates = checkUpdatesAct->isChecked();
        emit settingsChanged();
        update_check_updates();
    });

    checkUpdatesNowAct = new QAction(tr("Check for Updates Now"), this);
    connect(checkUpdatesNowAct, &QAction::triggered, this, [this]() {
        check_for_updates_now();
    });

    // Cycle Themes
    cycleThemesAct = new QAction(tr("Cycle Themes"), this);
    connect(cycleThemesAct, SIGNAL(triggered()), this, SLOT(cycleThemes()));

    // Info
    infoAct = new QAction(theme->getInfoIcon(false), tr("Show Info"), this);
    infoAct->setCheckable(true);
    infoAct->setChecked(false);
    connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

    // Help
    helpAct = new QAction(theme->getHelpIcon(false), tr("Show Help"), this);
    helpAct->setCheckable(true);
    helpAct->setChecked(false);
    connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));
    if (helpCloseButton)
    {
        const QString ks = helpAct->shortcut().toString(QKeySequence::NativeText);
        helpCloseButton->setToolTip(ks.isEmpty() ? tr("Close Help")
                                                 : tr("Close Help (%1)").arg(ks));
    }

    // Preferences
    prefsAct = new QAction(theme->getPrefsIcon(false), tr("Show Preferences"), this);
    prefsAct->setCheckable(true);
    prefsAct->setChecked(false);
    connect(prefsAct, SIGNAL(triggered()), this, SLOT(togglePrefs()));

    // Tab Prev
    tabPrevAct = new QAction(tr("Previous Tab"), this);
    connect(tabPrevAct, SIGNAL(triggered()), this, SLOT(tabPrev()));

    // Tab Prev
    tabNextAct = new QAction(tr("Next Tab"), this);
    connect(tabNextAct, SIGNAL(triggered()), this, SLOT(tabNext()));

    tab1Act = new QAction(tr("Focus Tab 1"), this);
    connect(tab1Act, &QAction::triggered, [this]() { tabGoto(1); });

    tab2Act = new QAction(tr("Focus Tab 2"), this);
    connect(tab2Act, &QAction::triggered, [this]() { tabGoto(2); });

    tab3Act = new QAction(tr("Focus Tab 3"), this);
    connect(tab3Act, &QAction::triggered, [this]() { tabGoto(3); });

    tab4Act = new QAction(tr("Focus Tab 4"), this);
    connect(tab4Act, &QAction::triggered, [this]() { tabGoto(4); });

    tab5Act = new QAction(tr("Focus Tab 5"), this);
    connect(tab5Act, &QAction::triggered, [this]() { tabGoto(5); });

    tab6Act = new QAction(tr("Focus Tab 6"), this);
    connect(tab6Act, &QAction::triggered, [this]() { tabGoto(6); });

    tab7Act = new QAction(tr("Focus Tab 7"), this);
    connect(tab7Act, &QAction::triggered, [this]() { tabGoto(7); });

    tab8Act = new QAction(tr("Focus Tab 8"), this);
    connect(tab8Act, &QAction::triggered, [this]() { tabGoto(8); });

    tab9Act = new QAction(tr("Focus Tab 9"), this);
    connect(tab9Act, &QAction::triggered, [this]() { tabGoto(9); });

    tab0Act = new QAction(tr("Focus Tab 0"), this);
    connect(tab0Act, &QAction::triggered, [this]() { tabGoto(0); });

    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    toolBar = addToolBar(tr("Tools"));
    toolBar->setObjectName("toolbar");
    toolBar->setMovable(false);   // drop the drag-handle grip at the toolbar's left edge

    toolBar->addAction(runAct);
    toolBar->addAction(stopAct);
    toolBar->addAction(recAct);
#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Right-click on the rec button surfaces the mode-switch menu as
    // a shortcut to the IO menubar / Preferences setting.
    if (QWidget* recWidget = toolBar->widgetForAction(recAct)) {
        recWidget->setContextMenuPolicy(Qt::CustomContextMenu);
        connect(recWidget, &QWidget::customContextMenuRequested,
                this, &MainWindow::showRecordingModeMenu);
    }
#endif
    toolBar->addAction(loadFileAct);
    toolBar->addAction(saveAsAct);

    toolBar->addWidget(spacer);

    toolBar->addAction(textDecAct);
    toolBar->addAction(textIncAct);

    dynamic_cast<QToolButton*>(toolBar->widgetForAction(textDecAct))->setAutoRepeat(true);
    dynamic_cast<QToolButton*>(toolBar->widgetForAction(textIncAct))->setAutoRepeat(true);

    showLineNumbersAct = new QAction(tr("Show Line Numbers"), this);
    showLineNumbersAct->setCheckable(true);
    showLineNumbersAct->setChecked(piSettings->show_line_numbers);
    connect(showLineNumbersAct, SIGNAL(triggered()), this, SLOT(showLineNumbersMenuChanged()));

    showAutoCompletionAct = new QAction(tr("Show Code Completion"), this);
    showAutoCompletionAct->setCheckable(true);
    showAutoCompletionAct->setChecked(piSettings->show_autocompletion);
    connect(showAutoCompletionAct, SIGNAL(triggered()), this, SLOT(showAutoCompletionMenuChanged()));

    showCompletionHelpAct = new QAction(tr("Show Code Completion Help"), this);
    showCompletionHelpAct->setCheckable(true);
    showCompletionHelpAct->setChecked(piSettings->show_completion_help);
    connect(showCompletionHelpAct, SIGNAL(triggered()), this, SLOT(showCompletionHelpMenuChanged()));

    showContextAct = new QAction(tr("Show Code Context"), this);
    showContextAct->setCheckable(true);
    showContextAct->setChecked(piSettings->show_context);
    connect(showContextAct, SIGNAL(triggered()), this, SLOT(showContextMenuChanged()));

    flashCodeAct = new QAction(tr("Flash Code on Sound Trigger"), this);
    flashCodeAct->setCheckable(true);
    flashCodeAct->setChecked(piSettings->flash_code);
    connect(flashCodeAct, SIGNAL(triggered()), this, SLOT(flashCodeMenuChanged()));

    flashGutterAct = new QAction(tr("Flash Gutter on Sound Trigger"), this);
    flashGutterAct->setCheckable(true);
    flashGutterAct->setChecked(piSettings->flash_gutter);
    connect(flashGutterAct, SIGNAL(triggered()), this, SLOT(flashGutterMenuChanged()));

    showLoopScopesAct = new QAction(tr("Show Live Loop Scopes"), this);
    showLoopScopesAct->setCheckable(true);
    showLoopScopesAct->setChecked(piSettings->show_loop_scopes);
    connect(showLoopScopesAct, SIGNAL(triggered()), this, SLOT(showLoopScopesMenuChanged()));

    loopScopeScrollAct = new QAction(tr("Scrolling Live Loop Scopes"), this);
    loopScopeScrollAct->setCheckable(true);
    loopScopeScrollAct->setChecked(piSettings->loop_scope_scroll);
    connect(loopScopeScrollAct, SIGNAL(triggered()), this, SLOT(loopScopeScrollMenuChanged()));

    speakTransportAct = new QAction(tr("Speak Run and Stop"), this);
    speakTransportAct->setCheckable(true);
    speakTransportAct->setChecked(piSettings->speak_transport);
    connect(speakTransportAct, SIGNAL(triggered()), this, SLOT(speakTransportMenuChanged()));

    reduceMotionAct = new QAction(tr("Reduce Animations"), this);
    reduceMotionAct->setCheckable(true);
    reduceMotionAct->setChecked(piSettings->reduce_motion);
    connect(reduceMotionAct, SIGNAL(triggered()), this, SLOT(reduceMotionMenuChanged()));

    enableScsynthInputsAct = new QAction(tr("Enable Audio Inputs"), this);
    enableScsynthInputsAct->setCheckable(true);
    enableScsynthInputsAct->setChecked(piSettings->enable_scsynth_inputs);
    connect(enableScsynthInputsAct, SIGNAL(triggered()), this, SLOT(enableScsynthInputsMenuChanged()));

    enableLinkAct = new QAction(tr("Link Connect"), this);
    enableLinkAct->setCheckable(true);
    enableLinkAct->setChecked(false);
    connect(enableLinkAct, SIGNAL(triggered()), this, SLOT(toggleLinkMenu()));

    linkTapTempoAct = new QAction(tr("Tap Tempo"), this);
    connect(linkTapTempoAct, SIGNAL(triggered()), metroPane, SLOT(tapTempo()));

    audioSafeAct = new QAction(tr("Safe Audio Mode"), this);
    audioSafeAct->setCheckable(true);
    audioSafeAct->setChecked(piSettings->check_args);
    connect(audioSafeAct, SIGNAL(triggered()), this, SLOT(audioSafeMenuChanged()));

    audioTimingGuaranteesAct = new QAction(tr("Enforce Timing Guarantees"), this);
    audioTimingGuaranteesAct->setCheckable(true);
    audioTimingGuaranteesAct->setChecked(piSettings->synth_trigger_timing_guarantees);
    connect(audioTimingGuaranteesAct, SIGNAL(triggered()), this, SLOT(audioTimingGuaranteesMenuChanged()));

    enableExternalSynthsAct = new QAction(tr("Enable External Synths"), this);
    enableExternalSynthsAct->setCheckable(true);
    enableExternalSynthsAct->setChecked(piSettings->enable_external_synths);
    connect(enableExternalSynthsAct, SIGNAL(triggered()), this, SLOT(enableExternalSynthsMenuChanged()));

    mixerInvertStereoAct = new QAction(tr("Invert Stereo"), this);
    mixerInvertStereoAct->setCheckable(true);
    mixerInvertStereoAct->setChecked(piSettings->mixer_invert_stereo);
    connect(mixerInvertStereoAct, SIGNAL(triggered()), this, SLOT(mixerInvertStereoMenuChanged()));

    mixerForceMonoAct = new QAction(tr("Force Mono"), this);
    mixerForceMonoAct->setCheckable(true);
    mixerForceMonoAct->setChecked(piSettings->mixer_force_mono);
    connect(mixerForceMonoAct, SIGNAL(triggered()), this, SLOT(mixerForceMonoMenuChanged()));

    midiEnabledAct = new QAction(tr("Enable Incoming MIDI Cues"), this);
    midiEnabledAct->setCheckable(true);
    midiEnabledAct->setChecked(piSettings->midi_enabled);
    connect(midiEnabledAct, SIGNAL(triggered()), this, SLOT(midiEnabledMenuChanged()));

    gamepadEnabledAct = new QAction(tr("Enable Incoming Gamepad Cues"), this);
    gamepadEnabledAct->setCheckable(true);
    gamepadEnabledAct->setChecked(piSettings->gamepad_enabled);
    connect(gamepadEnabledAct, SIGNAL(triggered()), this, SLOT(gamepadEnabledMenuChanged()));

    enableOSCServerAct = new QAction(tr("Allow Incoming OSC"), this);
    enableOSCServerAct->setCheckable(true);
    enableOSCServerAct->setChecked(piSettings->osc_server_enabled);
    connect(enableOSCServerAct, SIGNAL(triggered()), this, SLOT(oscServerEnabledMenuChanged()));

    allowRemoteOSCAct = new QAction(tr("Allow OSC From Other Computers"), this);
    allowRemoteOSCAct->setCheckable(true);
    allowRemoteOSCAct->setChecked(piSettings->osc_public);
    connect(allowRemoteOSCAct, SIGNAL(triggered()), this, SLOT(allowRemoteOSCMenuChanged()));

    logCuesAct = new QAction(tr("Log Cues"), this);
    logCuesAct->setCheckable(true);
    logCuesAct->setChecked(piSettings->log_cues);
    connect(logCuesAct, SIGNAL(triggered()), this, SLOT(logCuesMenuChanged()));

    logSynthsAct = new QAction(tr("Log Synths"), this);
    logSynthsAct->setCheckable(true);
    logSynthsAct->setChecked(piSettings->log_synths);
    connect(logSynthsAct, SIGNAL(triggered()), this, SLOT(logSynthsMenuChanged()));

    clearOutputOnRunAct = new QAction(tr("Clear Logs on Run"), this);
    clearOutputOnRunAct->setCheckable(true);
    clearOutputOnRunAct->setChecked(piSettings->clear_output_on_run);
    connect(clearOutputOnRunAct, SIGNAL(triggered()), this, SLOT(clearOutputOnRunMenuChanged()));

    autoIndentOnRunAct = new QAction(tr("Auto Indent Code Buffer"), this);
    autoIndentOnRunAct->setCheckable(true);
    autoIndentOnRunAct->setChecked(piSettings->auto_indent_on_run);
    connect(autoIndentOnRunAct, SIGNAL(triggered()), this, SLOT(autoIndentOnRunMenuChanged()));

    logAutoScrollAct = new QAction(tr("Auto-Scroll Log"), this);
    logAutoScrollAct->setCheckable(true);
    logAutoScrollAct->setChecked(piSettings->log_auto_scroll);
    connect(logAutoScrollAct, SIGNAL(triggered()), this, SLOT(logAutoScrollMenuChanged()));

    textSetMarkAct = new QAction(tr("Set Mark"), this);
    connect(textSetMarkAct, SIGNAL(triggered()), this, SLOT(setMarkInCurrentWorkspace()));

    triggerAutocompleteAct = new QAction(tr("Trigger Autocomplete"), this);
    connect(triggerAutocompleteAct, SIGNAL(triggered()), this, SLOT(triggerAutocompleteInCurrentWorkspace()));

    readCompletionDetailsAct = new QAction(tr("Read Completion Details"), this);
    connect(readCompletionDetailsAct, SIGNAL(triggered()), this, SLOT(readCompletionDetailsInCurrentWorkspace()));

    toolBar->addAction(scopeAct);
    toolBar->addAction(infoAct);
    toolBar->addAction(helpAct);
    toolBar->addAction(prefsAct);

    liveMenu = menuBar()->addMenu(tr("Live"));
    liveMenu->addAction(runAct);
    liveMenu->addAction(stopAct);
    liveMenu->addAction(recAct);
    liveMenu->addSeparator();
    liveMenu->addAction(saveAsAct);
    liveMenu->addAction(loadFileAct);
    liveMenu->addSeparator();
    liveMenu->addAction(loadSetAct);
    liveMenu->addAction(saveSetAct);
    liveMenu->addAction(saveSetAsAct);
    recentSetsMenu = liveMenu->addMenu(tr("Load Recent Set"));
    updateRecentSetsMenu();
    liveMenu->addAction(clearAllBuffersAct);
    liveMenu->addSeparator();
    liveMenu->addAction(logSynthsAct);
    liveMenu->addAction(logCuesAct);
    liveMenu->addAction(logAutoScrollAct);
    liveMenu->addAction(clearOutputOnRunAct);
    liveMenu->addSeparator();
    liveMenu->addAction(exitAct);

    codeMenu = menuBar()->addMenu(tr("Code"));

    codeMenu->addSeparator();
    codeMenu->addAction(textCopyAct);
    codeMenu->addAction(textPasteAct);
    codeMenu->addAction(textCutAct);
    codeMenu->addAction(textCutToEndOfLineAct);
    codeMenu->addAction(textSelectAllAct);
    codeMenu->addAction(textSetMarkAct);
    codeMenu->addAction(triggerAutocompleteAct);
    codeMenu->addSeparator();
    codeMenu->addAction(findAct);
    codeMenu->addAction(findNextAct);
    codeMenu->addAction(findPrevAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textUndoAct);
    codeMenu->addAction(textRedoAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textDeleteBackAct);
    codeMenu->addAction(textDeleteForwardAct);
    codeMenu->addAction(textDeleteWordLeftAct);
    codeMenu->addAction(textDeleteWordRightAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textLeftAct);
    codeMenu->addAction(textRightAct);
    codeMenu->addAction(textUpAct);
    codeMenu->addAction(textDownAct);
    codeMenu->addAction(textWordLeftAct);
    codeMenu->addAction(textWordRightAct);
    codeMenu->addAction(textUpTenAct);
    codeMenu->addAction(textDownTenAct);
    codeMenu->addAction(textLineStartAct);
    codeMenu->addAction(textLineEndAct);
    codeMenu->addAction(textDocStartAct);
    codeMenu->addAction(textDocEndAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textSelectLineStartAct);
    codeMenu->addAction(textSelectLineEndAct);
    codeMenu->addAction(textSelectWordLeftAct);
    codeMenu->addAction(textSelectWordRightAct);
    codeMenu->addAction(textSelectDocStartAct);
    codeMenu->addAction(textSelectDocEndAct);
    codeMenu->addAction(textCenterCaretAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textTransposeAct);
    codeMenu->addAction(textShiftLineUpAct);
    codeMenu->addAction(textShiftLineDownAct);
    codeMenu->addAction(textUpcaseWordAct);
    codeMenu->addAction(textDowncaseWordAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textAlignAct);
    codeMenu->addAction(textCommentAct);

    createExamplesMenu();

    audioMenu = menuBar()->addMenu(tr("Audio"));
    audioMenu->addAction(enableExternalSynthsAct);
    audioMenu->addAction(audioSafeAct);
    audioMenu->addAction(audioTimingGuaranteesAct);
    audioMenu->addSeparator();
    audioMenu->addAction(mixerInvertStereoAct);
    audioMenu->addAction(mixerForceMonoAct);
    audioMenu->addAction(enableScsynthInputsAct);
    audioMenu->addSeparator();
    audioMenu->addAction(enableLinkAct);
    audioMenu->addAction(linkTapTempoAct);
    displayMenu = menuBar()->addMenu(tr("Visuals"));

    lightThemeAct = new QAction(tr("Light"));
    lightThemeAct->setCheckable(true);
    lightThemeAct->setChecked(false);
    connect(lightThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(1); });

    darkThemeAct = new QAction(tr("Dark"));
    darkThemeAct->setCheckable(true);
    darkThemeAct->setChecked(false);
    connect(darkThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(2); });

    highContrastThemeAct = new QAction(tr("High Contrast"));
    highContrastThemeAct->setCheckable(true);
    highContrastThemeAct->setChecked(false);
    connect(highContrastThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(3); });

    mildThemeAct = new QAction(tr("Mild Dark"));
    mildThemeAct->setCheckable(true);
    mildThemeAct->setChecked(false);
    connect(mildThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(4); });

    phosphorThemeAct = new QAction(tr("Phosphor"));
    phosphorThemeAct->setCheckable(true);
    phosphorThemeAct->setChecked(false);
    connect(phosphorThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(5); });

    signalThemeAct = new QAction(tr("Signal"));
    signalThemeAct->setCheckable(true);
    signalThemeAct->setChecked(false);
    connect(signalThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(6); });

    // Icon set is a fully independent axis; toggles Classic <-> Pro for the
    // currently-selected colour scheme.
    proIconsAct = new QAction(tr("Pro Icons"));
    proIconsAct->setCheckable(true);
    proIconsAct->setChecked(false);
    connect(proIconsAct, &QAction::triggered, [this](bool pro) {
        piSettings->proIcons = pro;
        emit settingsChanged();
        updateColourTheme();
    });

    showScopeLabelsAct = new QAction(tr("Show Scope Labels"));
    showScopeLabelsAct->setCheckable(true);
    showScopeLabelsAct->setChecked(false);
    connect(showScopeLabelsAct, SIGNAL(triggered()), this, SLOT(showScopeLabelsMenuChanged()));

    showTitlesAct = new QAction(tr("Show Titles"));
    showTitlesAct->setCheckable(true);
    showTitlesAct->setChecked(false);
    connect(showTitlesAct, SIGNAL(triggered()), this, SLOT(titleVisibilityChanged()));

    hideMenuBarInFullscreenAct = new QAction(tr("Hide Menu Bar in Fullscreen Mode"));
    hideMenuBarInFullscreenAct->setCheckable(true);
    hideMenuBarInFullscreenAct->setChecked(false);
    connect(hideMenuBarInFullscreenAct, SIGNAL(triggered()), this, SLOT(menuBarInFullscreenVisibilityChanged()));

    emacsShortcutModeAct = new QAction(tr("Emacs Live Shortcut Mode"), this);
    emacsShortcutModeAct->setCheckable(true);
    emacsShortcutModeAct->setChecked(false);
    connect(emacsShortcutModeAct, &QAction::triggered, [this]() { shortcutModeMenuChanged(1); });

    winShortcutModeAct = new QAction(tr("Windows | Linux Shortcut Mode"), this);
    winShortcutModeAct->setCheckable(true);
    winShortcutModeAct->setChecked(false);
    connect(winShortcutModeAct, &QAction::triggered, [this]() { shortcutModeMenuChanged(2); });

    macShortcutModeAct = new QAction(tr("Mac Shortcut Mode"), this);
    macShortcutModeAct->setCheckable(true);
    macShortcutModeAct->setChecked(false);
    connect(macShortcutModeAct, &QAction::triggered, [this]() { shortcutModeMenuChanged(3); });

    userShortcutModeAct = new QAction(tr("Custom Shortcut Mode"), this);
    userShortcutModeAct->setCheckable(true);
    userShortcutModeAct->setChecked(false);
    connect(userShortcutModeAct, &QAction::triggered, [this]() { shortcutModeMenuChanged(4); });

    themeMenu = displayMenu->addMenu(tr("Colour Theme"));
    themeMenu->addAction(lightThemeAct);
    themeMenu->addAction(darkThemeAct);
    themeMenu->addAction(mildThemeAct);
    themeMenu->addAction(phosphorThemeAct);
    themeMenu->addAction(highContrastThemeAct);
    themeMenu->addAction(signalThemeAct);
    themeMenu->addSeparator();
    themeMenu->addAction(proIconsAct);
    displayMenu->addAction(cycleThemesAct);
    displayMenu->addSeparator();

    displayMenu->addAction(scopeAct);
    displayMenu->addAction(scopePausedAct);
    displayMenu->addAction(showScopeLabelsAct);
    scopeKindVisibilityMenu = displayMenu->addMenu(tr("Show Scope Kinds"));

    for (auto name : scopeWindow->GetScopeCategories())
    {
        QAction* act = new QAction(name);
        act->setCheckable(true);
        act->setChecked(piSettings->isScopeActive(name));
        connect(act, SIGNAL(triggered()), this, SLOT(scopeKindVisibilityMenuChanged()));
        scopeKindVisibilityMenu->addAction(act);
    }

    // The IO menu is grouped into labelled sections (addSection) so the
    // device controls, network controls and capture controls read as
    // distinct blocks. The MIDI/gamepad device submenus are populated
    // dynamically (see updateMIDI*Ports / updateGamepadDevices) with one
    // checkable entry per device, mirroring the IO preferences pane.
    ioMenu = menuBar()->addMenu(tr("IO"));

    // Keyboard input mode — a self-contained picker, kept at the top.
    shortcutMenu = ioMenu->addMenu(tr("Shortcut Mode"));
    shortcutMenu->addAction(macShortcutModeAct);
    shortcutMenu->addAction(winShortcutModeAct);
    shortcutMenu->addAction(emacsShortcutModeAct);
    shortcutMenu->addAction(userShortcutModeAct);

    ioMenu->addSection(tr("MIDI"));
    ioMenu->addAction(midiEnabledAct);
    ioMidiInMenu = ioMenu->addMenu(tr("MIDI Inputs"));
    ioMidiInMenu->addAction(tr("No Connected Inputs"))->setEnabled(false);
    ioMidiOutMenu = ioMenu->addMenu(tr("MIDI Outputs"));
    ioMidiOutMenu->addAction(tr("No Connected Outputs"))->setEnabled(false);

    ioMidiOutChannelMenu = ioMenu->addMenu(tr("Default MIDI Out Channel"));

    QAction* midiOutChanMenuAll = ioMidiOutChannelMenu->addAction(tr("All Channels"));
    midiOutChanMenuAll->setCheckable(true);
    midiOutChanMenuAll->setChecked(true);
    connect(midiOutChanMenuAll, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(0); });

    QAction* midiOutChanMenu1 = ioMidiOutChannelMenu->addAction(tr("1"));
    midiOutChanMenu1->setCheckable(true);
    midiOutChanMenu1->setChecked(false);
    connect(midiOutChanMenu1, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(1); });

    QAction* midiOutChanMenu2 = ioMidiOutChannelMenu->addAction(tr("2"));
    midiOutChanMenu2->setCheckable(true);
    midiOutChanMenu2->setChecked(false);
    connect(midiOutChanMenu2, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(2); });

    QAction* midiOutChanMenu3 = ioMidiOutChannelMenu->addAction(tr("3"));
    midiOutChanMenu3->setCheckable(true);
    midiOutChanMenu3->setChecked(false);
    connect(midiOutChanMenu3, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(3); });

    QAction* midiOutChanMenu4 = ioMidiOutChannelMenu->addAction(tr("4"));
    midiOutChanMenu4->setCheckable(true);
    midiOutChanMenu4->setChecked(false);
    connect(midiOutChanMenu4, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(4); });

    QAction* midiOutChanMenu5 = ioMidiOutChannelMenu->addAction(tr("5"));
    midiOutChanMenu5->setCheckable(true);
    midiOutChanMenu5->setChecked(false);
    connect(midiOutChanMenu5, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(5); });

    QAction* midiOutChanMenu6 = ioMidiOutChannelMenu->addAction(tr("6"));
    midiOutChanMenu6->setCheckable(true);
    midiOutChanMenu6->setChecked(false);
    connect(midiOutChanMenu6, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(6); });

    QAction* midiOutChanMenu7 = ioMidiOutChannelMenu->addAction(tr("7"));
    midiOutChanMenu7->setCheckable(true);
    midiOutChanMenu7->setChecked(false);
    connect(midiOutChanMenu7, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(7); });

    QAction* midiOutChanMenu8 = ioMidiOutChannelMenu->addAction(tr("8"));
    midiOutChanMenu8->setCheckable(true);
    midiOutChanMenu8->setChecked(false);
    connect(midiOutChanMenu8, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(8); });

    QAction* midiOutChanMenu9 = ioMidiOutChannelMenu->addAction(tr("9"));
    midiOutChanMenu9->setCheckable(true);
    midiOutChanMenu9->setChecked(false);
    connect(midiOutChanMenu9, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(9); });

    QAction* midiOutChanMenu10 = ioMidiOutChannelMenu->addAction(tr("10"));
    midiOutChanMenu10->setCheckable(true);
    midiOutChanMenu10->setChecked(false);
    connect(midiOutChanMenu10, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(10); });

    QAction* midiOutChanMenu11 = ioMidiOutChannelMenu->addAction(tr("11"));
    midiOutChanMenu11->setCheckable(true);
    midiOutChanMenu11->setChecked(false);
    connect(midiOutChanMenu11, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(11); });

    QAction* midiOutChanMenu12 = ioMidiOutChannelMenu->addAction(tr("12"));
    midiOutChanMenu12->setCheckable(true);
    midiOutChanMenu12->setChecked(false);
    connect(midiOutChanMenu12, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(12); });

    QAction* midiOutChanMenu13 = ioMidiOutChannelMenu->addAction(tr("13"));
    midiOutChanMenu13->setCheckable(true);
    midiOutChanMenu13->setChecked(false);
    connect(midiOutChanMenu13, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(13); });

    QAction* midiOutChanMenu14 = ioMidiOutChannelMenu->addAction(tr("14"));
    midiOutChanMenu14->setCheckable(true);
    midiOutChanMenu14->setChecked(false);
    connect(midiOutChanMenu14, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(14); });

    QAction* midiOutChanMenu15 = ioMidiOutChannelMenu->addAction(tr("15"));
    midiOutChanMenu15->setCheckable(true);
    midiOutChanMenu15->setChecked(false);
    connect(midiOutChanMenu15, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(15); });

    QAction* midiOutChanMenu16 = ioMidiOutChannelMenu->addAction(tr("16"));
    midiOutChanMenu16->setCheckable(true);
    midiOutChanMenu16->setChecked(false);
    connect(midiOutChanMenu16, &QAction::triggered, [this]() { midiDefaultChannelMenuChanged(16); });

    ioMenu->addSection(tr("Game Controllers"));
    ioMenu->addAction(gamepadEnabledAct);
    ioGamepadMenu = ioMenu->addMenu(tr("Connected Controllers"));
    ioGamepadMenu->addAction(tr("No Connected Controllers"))->setEnabled(false);

    ioMenu->addSection(tr("OSC"));
    ioMenu->addAction(enableOSCServerAct);
    ioMenu->addAction(allowRemoteOSCAct);
    localIpAddressesMenu = ioMenu->addMenu(tr("Local IP Addresses"));
    QList<QHostAddress> list = QNetworkInterface::allAddresses();

    for (int nIter = 0; nIter < list.count(); nIter++)
    {
        if (!list[nIter].isLoopback())
        {
            if (list[nIter].protocol() == QAbstractSocket::IPv4Protocol)
            {
                localIpAddressesMenu->addAction(list[nIter].toString());
            }
        }
    }

    QMenu* incomingOSCPortMenu = ioMenu->addMenu(tr("Incoming OSC Port"));
    incomingOSCPortMenu->addAction(QString::number(m_spAPI->GetPort(SonicPiPortId::tau_osc_cues)));
    // Recording + publishing entries are appended further down, once
    // the syphon/spout/record QActions exist.

    viewMenu = menuBar()->addMenu(tr("View"));

    // Accessibility shortcuts

    // Focus Editor
    focusEditorAct = new QAction(tr("Focus Editor"), this);
    connect(focusEditorAct, SIGNAL(triggered()), this, SLOT(focusEditor()));

    // Focus Logs
    focusLogsAct = new QAction(tr("Focus Logs"), this);
    connect(focusLogsAct, SIGNAL(triggered()), this, SLOT(focusLogs()));

    // Focus Context
    focusContextAct = new QAction(tr("Focus Context"), this);
    connect(focusContextAct, SIGNAL(triggered()), this, SLOT(focusContext()));

    // Focus Cues
    focusCuesAct = new QAction(tr("Focus Cues"), this);
    connect(focusCuesAct, SIGNAL(triggered()), this, SLOT(focusCues()));

    // Focus Preferences
    focusPreferencesAct = new QAction(tr("Focus Preferences"), this);
    connect(focusPreferencesAct, SIGNAL(triggered()), this, SLOT(focusPreferences()));

    // Focus HelpListing
    focusHelpListingAct = new QAction(tr("Focus Help Listing"), this);
    connect(focusHelpListingAct, SIGNAL(triggered()), this, SLOT(focusHelpListing()));

    // Focus HelpDetails
    focusHelpDetailsAct = new QAction(tr("Focus Help Details"), this);
    connect(focusHelpDetailsAct, SIGNAL(triggered()), this, SLOT(focusHelpDetails()));

    // Focus Errors
    focusErrorsAct = new QAction(tr("Focus Errors"), this);
    connect(focusErrorsAct, SIGNAL(triggered()), this, SLOT(focusErrors()));

    // Cycle focus through visible panes (F6 — the standard pane-cycling key
    // on Windows, harmless elsewhere)
    cycleFocusForwardAct = new QAction(tr("Focus Next Pane"), this);
    connect(cycleFocusForwardAct, SIGNAL(triggered()), this, SLOT(cycleFocusForward()));

    cycleFocusBackAct = new QAction(tr("Focus Previous Pane"), this);
    connect(cycleFocusBackAct, SIGNAL(triggered()), this, SLOT(cycleFocusBack()));

    // Focus BPM SCrubber
    focusBPMScrubberAct = new QAction(tr("Focus BPM Scrubber"), this);
    connect(focusBPMScrubberAct, SIGNAL(triggered()), this, SLOT(focusBPMScrubber()));

    // Focus Time Warp Scrubber
    focusTimeWarpScrubberAct = new QAction(tr("Focus TimeWarp Scrubber"), this);
    connect(focusTimeWarpScrubberAct, SIGNAL(triggered()), this, SLOT(focusTimeWarpScrubber()));

    showLogAct = new QAction(tr("Show Log"), this);
    showLogAct->setCheckable(true);
    showLogAct->setChecked(piSettings->show_log);
    connect(showLogAct, SIGNAL(triggered()), this, SLOT(showLogMenuChanged()));

    showCuesAct = new QAction(tr("Show Cue Log"), this);
    showCuesAct->setCheckable(true);
    showCuesAct->setChecked(piSettings->show_cues);
    connect(showCuesAct, SIGNAL(triggered()), this, SLOT(showCuesMenuChanged()));

    showMetroAct = new QAction(tr("Show Metronome"), this);
    showMetroAct->setCheckable(true);
    showMetroAct->setChecked(piSettings->show_metro);
    connect(showMetroAct, SIGNAL(triggered()), this, SLOT(showMetroChanged()));

#ifdef Q_OS_MAC
    syphonPublishAct = new QAction(tr("Publish Window via Syphon"), this);
    syphonPublishAct->setCheckable(true);
    syphonPublishAct->setChecked(false);
    connect(syphonPublishAct, SIGNAL(triggered()), this, SLOT(syphonPublishMenuChanged()));

    syphonShowCursorAct = new QAction(tr("Include Mouse Cursor in Syphon Feed"), this);
    syphonShowCursorAct->setCheckable(true);
    syphonShowCursorAct->setChecked(piSettings->syphon_show_cursor);
    connect(syphonShowCursorAct, SIGNAL(triggered()), this, SLOT(syphonShowCursorMenuChanged()));
#endif

#ifdef Q_OS_WIN
    spoutPublishAct = new QAction(tr("Publish Window via Spout"), this);
    spoutPublishAct->setCheckable(true);
    spoutPublishAct->setChecked(false);
    connect(spoutPublishAct, SIGNAL(triggered()), this, SLOT(spoutPublishMenuChanged()));

    spoutShowCursorAct = new QAction(tr("Include Mouse Cursor in Spout Feed"), this);
    spoutShowCursorAct->setCheckable(true);
    spoutShowCursorAct->setChecked(piSettings->spout_show_cursor);
    connect(spoutShowCursorAct, SIGNAL(triggered()), this, SLOT(spoutShowCursorMenuChanged()));
#endif

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    recordShowCursorAct = new QAction(tr("Include Mouse Cursor in Session Recording"), this);
    recordShowCursorAct->setCheckable(true);
    recordShowCursorAct->setChecked(piSettings->record_show_cursor);
    connect(recordShowCursorAct, SIGNAL(triggered()), this, SLOT(recordShowCursorMenuChanged()));

    recordFlashIconAct = new QAction(tr("Flash Recording Icon"), this);
    recordFlashIconAct->setCheckable(true);
    recordFlashIconAct->setChecked(piSettings->record_flash_icon);
    connect(recordFlashIconAct, SIGNAL(triggered()), this, SLOT(recordFlashIconMenuChanged()));

    // IO menu recording / publishing tail — appended here so the
    // QActions exist.
    ioMenu->addSection(tr("Recording"));
    QMenu* recModeSubmenu = ioMenu->addMenu(tr("Recording Mode"));
    recModeSubmenu->addAction(recAudioModeAct);
    recModeSubmenu->addAction(recAudioVideoModeAct);
    ioMenu->addAction(recordShowCursorAct);
    ioMenu->addAction(recordFlashIconAct);
#endif
#ifdef Q_OS_MAC
    ioMenu->addSection(tr("Window Publishing"));
    ioMenu->addAction(syphonPublishAct);
    ioMenu->addAction(syphonShowCursorAct);
#endif
#ifdef Q_OS_WIN
    ioMenu->addSection(tr("Window Publishing"));
    ioMenu->addAction(spoutPublishAct);
    ioMenu->addAction(spoutShowCursorAct);
#endif

    showButtonsAct = new QAction(tr("Show Buttons"), this);
    showButtonsAct->setCheckable(true);
    showButtonsAct->setChecked(piSettings->show_buttons);
    connect(showButtonsAct, SIGNAL(triggered()), this, SLOT(showButtonsMenuChanged()));

    showEditorToolbarAct = new QAction(tr("Show Editor Toolbar"), this);
    showEditorToolbarAct->setCheckable(true);
    showEditorToolbarAct->setChecked(piSettings->show_editor_toolbar);
    connect(showEditorToolbarAct, SIGNAL(triggered()), this, SLOT(showEditorToolbarMenuChanged()));

    showTabsAct = new QAction(tr("Show Tabs"), this);
    showTabsAct->setCheckable(true);
    showTabsAct->setChecked(piSettings->show_tabs);
    connect(showTabsAct, SIGNAL(triggered()), this, SLOT(showTabsMenuChanged()));

    fullScreenAct = new QAction(tr("Full Screen Mode"), this);
    fullScreenAct->setCheckable(true);
    fullScreenAct->setChecked(piSettings->full_screen);
    connect(fullScreenAct, SIGNAL(triggered()), this, SLOT(fullScreenMenuChanged()));

    logZoomInAct = new QAction(tr("Zoom In Logs"), this);
    connect(logZoomInAct, SIGNAL(triggered()), this, SLOT(zoomInLogs()));

    logZoomOutAct = new QAction(tr("Zoom Out Logs"), this);
    connect(logZoomOutAct, SIGNAL(triggered()), this, SLOT(zoomOutLogs()));

    viewMenu->addAction(fullScreenAct);
    viewMenu->addAction(focusModeAct);
    viewMenu->addSeparator();
    viewMenu->addAction(textIncAct);
    viewMenu->addAction(textDecAct);
    viewMenu->addSeparator();
    viewMenu->addAction(logZoomInAct);
    viewMenu->addAction(logZoomOutAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showLogAct);
    viewMenu->addAction(showCuesAct);
    viewMenu->addAction(showContextAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showButtonsAct);
    viewMenu->addAction(showEditorToolbarAct);
    viewMenu->addAction(showTabsAct);
    viewMenu->addAction(showTitlesAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showLineNumbersAct);
    viewMenu->addAction(showAutoCompletionAct);
    viewMenu->addAction(showCompletionHelpAct);
    viewMenu->addAction(autoIndentOnRunAct);
    viewMenu->addAction(flashCodeAct);
    viewMenu->addAction(flashGutterAct);
    viewMenu->addAction(showLoopScopesAct);
    viewMenu->addAction(loopScopeScrollAct);
#ifndef Q_OS_MAC
    // Don't enable this on Mac as macOS autohides the menubar on
    // fullscreen anyway
    viewMenu->addAction(hideMenuBarInFullscreenAct);
#endif

    viewMenu->addSeparator();
    viewMenu->addAction(infoAct);
    viewMenu->addAction(helpAct);
    viewMenu->addAction(prefsAct);
    viewMenu->addAction(showMetroAct);
    viewMenu->addSeparator();
    viewMenu->addAction(checkUpdatesAct);
    viewMenu->addAction(checkUpdatesNowAct);
    viewMenu->addSeparator();

    accessibilityMenu = viewMenu->addMenu(tr("Accessibility"));
    accessibilityMenu->addAction(speakTransportAct);
    accessibilityMenu->addAction(reduceMotionAct);
    accessibilityMenu->addAction(readCompletionDetailsAct);

    focusMenu = menuBar()->addMenu(tr("Focus"));
    focusMenu->addAction(contextHelpAct);
    focusMenu->addSeparator();
    focusMenu->addAction(tabPrevAct);
    focusMenu->addAction(tabNextAct);
    focusMenu->addSeparator();
    focusMenu->addAction(tab0Act);
    focusMenu->addAction(tab1Act);
    focusMenu->addAction(tab2Act);
    focusMenu->addAction(tab3Act);
    focusMenu->addAction(tab4Act);
    focusMenu->addAction(tab5Act);
    focusMenu->addAction(tab6Act);
    focusMenu->addAction(tab7Act);
    focusMenu->addAction(tab8Act);
    focusMenu->addAction(tab9Act);
    focusMenu->addSeparator();
    focusMenu->addAction(cycleFocusForwardAct);
    focusMenu->addAction(cycleFocusBackAct);
    focusMenu->addAction(focusEditorAct);
    focusMenu->addAction(focusLogsAct);
    focusMenu->addAction(focusCuesAct);
    focusMenu->addAction(focusContextAct);
    focusMenu->addAction(focusPreferencesAct);
    focusMenu->addAction(focusHelpListingAct);
    focusMenu->addAction(focusHelpDetailsAct);
    focusMenu->addAction(focusErrorsAct);
    focusMenu->addAction(focusTimeWarpScrubberAct);
    focusMenu->addAction(focusBPMScrubberAct);

    languageMenu = menuBar()->addMenu(tr("Language"));
    QStringList available_languages = sonicPii18n->getAvailableLanguages();

    langActionGroup = new QActionGroup(this);
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    langActionGroup->setExclusionPolicy(QActionGroup::ExclusionPolicy::Exclusive);
#else
    langActionGroup->setExclusive(true);
#endif

    QSignalMapper* signalMapper = new QSignalMapper(this);

    for (int i = 0; i < available_languages.length(); i += 1)
    {
        bool is_current_lang = (available_languages[i] == piSettings->language);

        QAction* langAct = new QAction(sonicPii18n->getNativeLanguageName(available_languages[i]), this);
        langAct->setCheckable(true);
        langAct->setChecked(is_current_lang);

        connect(langAct, SIGNAL(triggered()), signalMapper, SLOT(map()));
        signalMapper->setMapping(langAct, i);

        langActionGroup->addAction(langAct);
        languageMenu->addAction(langAct);

        if (i == 0)
        { // add separator after System language
            languageMenu->addSeparator();
        }
    }

    escapeSc = new QShortcut(ctrlKey("g"), this, SLOT(escapeWorkspaces()));
    escape2Sc = new QShortcut(QKeySequence("Escape"), this, SLOT(escapeWorkspaces()));

    // help tab
    //  QShortcut* up = new QShortcut(ctrlKey('p'), nameList);
    //  up->setContext(Qt::WidgetShortcut);
    //  connect(up, SIGNAL(activated()), this, SLOT(helpScrollUp()));
    //  QShortcut* down = new QShortcut(ctrlKey('n'), nameList);
    //  down->setContext(Qt::WidgetShortcut);
    //  connect(down, SIGNAL(activated()), this, SLOT(helpScrollDown()));

    // doc?
    // QShortcut* up = new QShortcut(ctrlKey('p'), docPane);
    // up->setContext(Qt::WidgetShortcut);
    // connect(up, SIGNAL(activated()), this, SLOT(docScrollUp()));
    // QShortcut* down = new QShortcut(ctrlKey('n'), docPane);
    // down->setContext(Qt::WidgetShortcut);
    // connect(down, SIGNAL(activated()), this, SLOT(docScrollDown()));

    connect(signalMapper, SIGNAL(mappedInt(int)), settingsWidget, SLOT(updateUILanguage(int)));
    connect(settingsWidget, SIGNAL(uiLanguageChanged(QString)), this, SLOT(updateSelectedUILanguageAction(QString)));
}

// Assign Alt+letter mnemonics to the top-level menus at runtime so translated
// titles keep working without translators managing '&' placement. Letters
// already bound as Alt+<letter> shortcuts are skipped — Meta+x resolves to
// Alt+x on Windows/Linux, so e.g. '&Live' would make Alt+L (Lowercase word)
// ambiguous. Re-run on every shortcut-mode change; existing '&'s are stripped
// first so assignment stays idempotent. macOS ignores mnemonics entirely.
void MainWindow::addMenuBarMnemonics()
{
#ifndef Q_OS_MAC
    QSet<QChar> used;
    QList<QKeySequence> bindings;
    const QList<QAction*> allActions = findChildren<QAction*>();
    for (QAction* a : allActions)
    {
        bindings << a->shortcuts();
    }
    const QList<QShortcut*> allShortcuts = findChildren<QShortcut*>();
    for (QShortcut* sc : allShortcuts)
    {
        bindings << sc->key();
    }
    for (const QKeySequence& ks : bindings)
    {
        for (int i = 0; i < ks.count(); i++)
        {
            const QKeyCombination kc = ks[i];
            if (kc.keyboardModifiers() == Qt::AltModifier
                && kc.key() >= Qt::Key_A && kc.key() <= Qt::Key_Z)
            {
                used.insert(QChar::fromLatin1('a' + (kc.key() - Qt::Key_A)));
            }
        }
    }

    const QList<QAction*> topLevel = menuBar()->actions();
    for (QAction* a : topLevel)
    {
        QString title = a->text();
        title.remove('&');
        for (int i = 0; i < title.size(); i++)
        {
            QChar c = title[i].toLower();
            if (c.isLetter() && !used.contains(c))
            {
                used.insert(c);
                title = title.left(i) + "&" + title.mid(i);
                break;
            }
        }
        a->setText(title);
    }
#endif
}

void MainWindow::updateSelectedUILanguageAction(QString lang)
{
    langActionGroup->actions()[sonicPii18n->getAvailableLanguages().indexOf(lang)]->setChecked(true);
}

QString MainWindow::readFile(QString name)
{
    QFile file(name);
    if (!file.open(QFile::ReadOnly | QFile::Text))
    {
        std::cerr << "[GUI] - could not open file " << name.toStdString() << "\n";
        return "";
    }

    QTextStream st(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    st.setEncoding(QStringConverter::Utf8);
#else
    st.setCodec("UTF-8");
#endif

    return st.readAll();
}

void MainWindow::loadInfoPaneContent()
{
    if (infoPanesLoaded)
        return;
    infoPanesLoaded = true;
    foreach (QTextBrowser* pane, infoPanes)
    {
        QFile file(pane->property("infoSrc").toString());
        file.open(QFile::ReadOnly | QFile::Text);

        QTextStream st(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        st.setEncoding(QStringConverter::Utf8);
#else
        st.setCodec("UTF-8");
#endif

        QString source = st.readAll();
        source = source.replace("100dx", QString("%1").arg(ScaleHeightForDPI(100)));
        source = source.replace("254dx", QString("%1").arg(ScaleHeightForDPI(254)));
        source = source.replace("413dx", QString("%1").arg(ScaleHeightForDPI(413)));
        source = source.replace("268dx", QString("%1").arg(ScaleHeightForDPI(268)));
        source = source.replace("328dx", QString("%1").arg(ScaleHeightForDPI(328)));
        source = source.replace("__SONIC_PI_VERSION__", SONIC_PI_VERSION);
        // Stashed for re-render on theme changes: setDefaultStyleSheet only
        // affects subsequently-set html, and reload() is a no-op for setHtml
        // content (no source URL).
        pane->setProperty("infoHtml", source);
        pane->setHtml(source);
    }
    // Just rendered with the current default stylesheets, so clear any
    // dirtiness accumulated while unloaded.
    infoPanesDirty = false;
}

void MainWindow::rerenderInfoPanes()
{
    // setDefaultStyleSheet only affects subsequently-set html, so restyling
    // means re-setting each pane's stashed source; keep the reader's place.
    foreach (QTextBrowser* pane, infoPanes)
    {
        const int scrollPos = pane->verticalScrollBar()->value();
        pane->setHtml(pane->property("infoHtml").toString());
        pane->verticalScrollBar()->setValue(scrollPos);
    }
    infoPanesDirty = false;
}

void MainWindow::createInfoPane()
{
    std::cout << "[GUI] - creating info panel" << std::endl;
    QTabWidget* infoTabs = new QTabWidget(this);
    infoTabs->setObjectName("infoNavTabs");   // chip-style tabs, see app.qss

    QStringList urls, tabs;

    urls << ":/html/info.html"
         << ":/info/COMMUNITY.html"
         << ":/info/CORETEAM.html"
         << ":/info/CONTRIBUTORS.html"
         << ":/info/LICENSE.html"
         << ":/info/CHANGELOG.html";

    tabs << tr("About")
         << tr("Community")
         << tr("Core Team")
         << tr("Contributors")
         << tr("License")
         << tr("History");

    for (int t = 0; t < urls.size(); t++)
    {
        QTextBrowser* pane = new QTextBrowser;
        pane->document()->setDocumentMargin(ScaleWidthForDPI(20));  // text inset; keeps the scrollbar flush
        infoPanes.append(pane);
        addUniversalCopyShortcuts(pane);
        pane->setOpenExternalLinks(true);
        // Content is read and parsed lazily on first open (the changelog
        // alone is ~160KB of HTML) — see loadInfoPaneContent().
        pane->setProperty("infoSrc", urls[t]);
        infoTabs->addTab(pane, tabs[t]);
    }

    infoTabs->setTabPosition(QTabWidget::South);

    QHBoxLayout* infoLayout = new QHBoxLayout;
    infoLayout->addWidget(infoTabs);

    infoWidg = new InfoWidget;
    infoWidg->setWindowIcon(QIcon(":images/icon-smaller.png"));
    infoWidg->setLayout(infoLayout);
    infoWidg->setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
    infoWidg->setWindowTitle(tr("Sonic Pi - Info"));
    infoWidg->setMinimumSize(ScaleForDPI(320, 240));
    infoWidg->resize(ScaleForDPI(800, 800).boundedTo(screen()->availableGeometry().size()));

    connect(infoWidg, SIGNAL(closed()), this, SLOT(about()));

    QAction* closeInfoAct = new QAction(this);
    closeInfoAct->setShortcuts({ QKeySequence(Qt::CTRL | Qt::Key_W), QKeySequence(Qt::Key_Escape) });
    connect(closeInfoAct, SIGNAL(triggered()), this, SLOT(about()));
    infoWidg->addAction(closeInfoAct);
}

// Keep the record action's label and checked state in sync with
// is_recording so the toggle is visible to screen readers.
void MainWindow::updateRecordingUI()
{
    recAct->setText(is_recording ? tr("Stop Recording") : tr("Start Recording"));
    recAct->setChecked(is_recording);
}

/**
 * Toggle record Icon while recording is active (triggert by rec_flash_timer)
 */
void MainWindow::toggleRecordingOnIcon()
{
    show_rec_icon_a = !show_rec_icon_a;
    recAct->setIcon(theme->getRecIcon(true, show_rec_icon_a));
}

/**
 * Start or Stop recording
 */
void MainWindow::toggleRecording()
{
    is_recording = !is_recording;
    updateRecordingUI();
    showStatusAndAnnounce(is_recording ? tr("Recording started") : tr("Recording stopped"), 2000);

    // Mode is read on start only; m_videoTempPath discriminates the
    // stop path so flipping mode mid-recording is safe.
    if (is_recording)
    {
        if (piSettings->record_flash_icon) {
            rec_flash_timer->start(500);
        } else {
            // Static "lit" frame — same icon the flash animation
            // toggles through, so still distinct from idle.
            recAct->setIcon(theme->getRecIcon(true, true));
        }

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
        if (piSettings->recording_type == SonicPiSettings::AudioAndVideo) {
            startSessionRecordingFlow();
            return;
        }
#endif
        oscpkt::Message msg("/start-recording");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
    else
    {
        rec_flash_timer->stop();
        recAct->setIcon(theme->getRecIcon(false, false));

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
        if (!m_videoTempPath.isEmpty()) {
            stopSessionRecordingFlow();
            return;
        }
#endif
        oscpkt::Message msg("/stop-recording");
        msg.pushInt32(guiID);
        sendOSC(msg);
        QString lastAudioDir = gui_settings->value("lastAudioDir", QDir::homePath() + "/Desktop").toString();
        QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), lastAudioDir, tr("Wavefile (*.wav)"));
        if (!fileName.isEmpty())
        {
            gui_settings->setValue("lastAudioDir", QDir(fileName).absolutePath());
            oscpkt::Message msg("/save-recording");
            msg.pushInt32(guiID);
            msg.pushStr(fileName.toStdString());
            sendOSC(msg);
        }
        else
        {
            oscpkt::Message msg("/delete-recording");
            msg.pushInt32(guiID);
            sendOSC(msg);
        }
    }
}

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
// Session-recording (audio + video) flow. Mirrors the audio path's
// "record now, prompt on stop" UX: write to a temp file, then rename
// (or delete) it once the user picks a save location.
void MainWindow::startSessionRecordingFlow()
{
#if defined(Q_OS_MAC)
    const QString ext = "mov";
#else
    const QString ext = "mp4";
#endif
    const QString tempDir = QStandardPaths::writableLocation(QStandardPaths::TempLocation);
    m_videoTempPath = QString("%1/sonic-pi-session-%2.%3")
        .arg(tempDir,
             QUuid::createUuid().toString(QUuid::WithoutBraces),
             ext);

    shm_audio_buffer* audioSlot = m_spAPI
        ? m_spAPI->AudioProcessor_GetAudioBufferSlot(SHM_AUDIO_MASTER_SLOT)
        : nullptr;
    if (audioSlot) {
        spawnRecordAudioOutSynth();
    } else {
        std::cout << "[GUI] - Session recording: no audio slot available — recording video-only" << std::endl;
    }

    WId wid = this->winId();
    const bool started = SonicPi::startSessionRecording(
        reinterpret_cast<void*>(wid),
        m_videoTempPath.toStdString(),
        piSettings->record_show_cursor,
        audioSlot);
    if (!started) {
        if (audioSlot) freeRecordAudioOutSynth();
        is_recording = false;
        updateRecordingUI();
        statusBar()->showMessage(tr("Recording failed to start"), 2000);
        announce(tr("Recording failed to start"), true);
        rec_flash_timer->stop();
        recAct->setIcon(theme->getRecIcon(false, false));
        m_videoTempPath.clear();
    }
}

void MainWindow::stopSessionRecordingFlow()
{
    SonicPi::stopSessionRecording();
    freeRecordAudioOutSynth();

#if defined(Q_OS_MAC)
    const QString ext    = "mov";
    const QString filter = tr("QuickTime Movie (*.mov)");
#else
    const QString ext    = "mp4";
    const QString filter = tr("MP4 Video (*.mp4)");
#endif
    // MoviesLocation → ~/Movies on macOS, ~/Videos on Windows.
    const QString moviesDir = QStandardPaths::writableLocation(QStandardPaths::MoviesLocation);
    QDir(moviesDir).mkpath("Sonic Pi");
    const QString defaultDir  = moviesDir + "/Sonic Pi";
    const QString defaultName = "Sonic Pi " + QDateTime::currentDateTime().toString("yyyy-MM-dd HHmmss") + "." + ext;
    const QString fileName = QFileDialog::getSaveFileName(this,
        tr("Save Session Recording"),
        defaultDir + "/" + defaultName,
        filter);

    if (!fileName.isEmpty()) {
        if (QFile::exists(fileName)) QFile::remove(fileName);
        if (!QFile::rename(m_videoTempPath, fileName)) {
            std::cout << "[GUI] - Session recording: rename to "
                      << fileName.toStdString() << " failed; temp file left at "
                      << m_videoTempPath.toStdString() << std::endl;
        }
    } else {
        QFile::remove(m_videoTempPath);
    }
    m_videoTempPath.clear();
}
#endif

void MainWindow::createStatusBar()
{
    std::cout << "[GUI] - creating status bar" << std::endl;
    versionLabel = new QLabel(this);
    versionLabel->setText("Sonic Pi");
    statusBar()->showMessage(tr("Ready..."));
    statusBar()->addPermanentWidget(versionLabel);
}

/**
 * restores the last size and position of the mainwindow
 * restores the zoomlevels of the editor tabs
 */
void MainWindow::restoreWindows()
{
    QRect rec = QGuiApplication::primaryScreen()->geometry();
    QPoint pos = gui_settings->value("pos", QPoint(0, 0)).toPoint();
    QSize size = gui_settings->value("size", QSize(rec.width(), rec.height())).toSize();

    int index = gui_settings->value("workspace", 0).toInt();
    if (index < editorTabWidget->count())
        editorTabWidget->setCurrentIndex(index);

    for (int w = 0; w < workspace_max; w++)
    {
        // default zoom is 13
        int zoom = gui_settings->value(QString("workspace%1zoom").arg(w), 2)
                       .toInt();
        if (zoom < -5)
            zoom = -5;
        if (zoom > 20)
            zoom = 20;

        workspaces[w]->setProperty("zoom", QVariant(zoom));
        workspaces[w]->zoomTo(zoom);
        workspaces[w]->updatePlaceholder();
    }

    restoreState(gui_settings->value("windowState").toByteArray());
    docsplit->restoreState(gui_settings->value("docsplitState").toByteArray());
    restoreGeometry(gui_settings->value("windowGeom").toByteArray());

    auto current_state = saveState();

    resize(size);
    move(pos);

    // Clamp a restored Help/Debug dock that takes too much of the window height.
    if (docWidget && docWidget->isVisible())
    {
        const int winH = size.height();
        if (winH > 0 && docWidget->height() > (winH * 2) / 5)
            resizeDocks({ docWidget }, { winH / 3 }, Qt::Vertical);
        ensureDocsSelection();   // restored-visible dock: don't land on a blank page
    }
}

/**
 * read the preferences
 *
 */
void MainWindow::readSettings()
{
    // Read in preferences from previous session
    piSettings->language = gui_settings->value("prefs/language", "system_language").toString();
    piSettings->show_buttons = gui_settings->value("prefs/show-buttons", true).toBool();
    piSettings->show_editor_toolbar = gui_settings->value("prefs/show-editor-toolbar", true).toBool();
    piSettings->show_tabs = gui_settings->value("prefs/show-tabs", true).toBool();
    piSettings->show_log = gui_settings->value("prefs/show-log", true).toBool();
    piSettings->osc_public = gui_settings->value("prefs/osc-public", false).toBool();
    piSettings->osc_server_enabled = gui_settings->value("prefs/osc-enabled", true).toBool();
    piSettings->midi_enabled = gui_settings->value("prefs/midi-enable", true).toBool();
    piSettings->gamepad_enabled = gui_settings->value("prefs/gamepad-enable", true).toBool();
    piSettings->midi_default_channel = gui_settings->value("prefs/midi-default-channel", 0).toInt();
    piSettings->check_args = gui_settings->value("prefs/check-args", true).toBool();
    piSettings->log_synths = gui_settings->value("prefs/log-synths", true).toBool();
    piSettings->clear_output_on_run = gui_settings->value("prefs/clear-output-on-run", true).toBool();
    piSettings->log_cues = gui_settings->value("prefs/log-cues", false).toBool();
    piSettings->log_auto_scroll = gui_settings->value("prefs/log-auto-scroll", true).toBool();
    piSettings->show_line_numbers = gui_settings->value("prefs/show-line-numbers", true).toBool();
    piSettings->enable_external_synths = gui_settings->value("prefs/enable-external-synths", false).toBool();
    piSettings->synth_trigger_timing_guarantees = gui_settings->value("prefs/synth-trigger-timing-guarantees", false).toBool();

    piSettings->main_volume = gui_settings->value("prefs/system-vol", 80).toInt();
    piSettings->mixer_force_mono = gui_settings->value("prefs/mixer-force-mono", false).toBool();
    piSettings->mixer_invert_stereo = gui_settings->value("prefs/mixer-invert-stereo", false).toBool();
    piSettings->enable_scsynth_inputs = gui_settings->value("prefs/enable-scsynth-inputs", false).toBool();
    piSettings->audio_driver        = gui_settings->value("prefs/audio-driver", "").toString();
    piSettings->audio_output_device = gui_settings->value("prefs/audio-output-device", "").toString();
    piSettings->audio_input_device  = gui_settings->value("prefs/audio-input-device", "").toString();
    piSettings->audio_sample_rate   = gui_settings->value("prefs/audio-sample-rate", 0).toInt();
    piSettings->audio_buffer_size   = gui_settings->value("prefs/audio-buffer-size", 0).toInt();
    piSettings->check_updates = gui_settings->value("prefs/rp/check-updates", true).toBool();
    piSettings->auto_indent_on_run = gui_settings->value("prefs/auto-indent-on-run", true).toBool();
    piSettings->gui_transparency = gui_settings->value("prefs/gui_transparency", 0).toInt();
    piSettings->show_scopes = gui_settings->value("prefs/scope/show-scopes", true).toBool();
    piSettings->show_scope_labels = gui_settings->value("prefs/scope/show-labels", false).toBool();
    piSettings->show_cues = gui_settings->value("prefs/show_cues", true).toBool();
    piSettings->show_metro = gui_settings->value("prefs/show_metro", true).toBool();
    piSettings->syphon_show_cursor = gui_settings->value("prefs/syphon_show_cursor", false).toBool();
    piSettings->record_show_cursor = gui_settings->value("prefs/record_show_cursor", true).toBool();
    piSettings->record_flash_icon  = gui_settings->value("prefs/record_flash_icon", true).toBool();
    piSettings->recording_type = static_cast<SonicPiSettings::RecordingType>(
        gui_settings->value("prefs/recording_type",
                            static_cast<int>(SonicPiSettings::Audio)).toInt());
    piSettings->spout_show_cursor = gui_settings->value("prefs/spout_show_cursor", false).toBool();
    piSettings->show_titles = gui_settings->value("prefs/show-titles", true).toBool();
    piSettings->hide_menubar_in_fullscreen = gui_settings->value("prefs/hide-menubar-in-fullscreen", false).toBool();
    QString styleName = gui_settings->value("prefs/theme", "").toString();
    piSettings->colourScheme = theme->colourSchemeFromName(styleName);
    // Icon set: migrated from legacy combined names ("... Pro") when the
    // dedicated pref is absent.
    piSettings->proIcons = gui_settings->value("prefs/pro-icons",
                               styleName.trimmed().endsWith(" Pro")).toBool();
    piSettings->hue_rotation = gui_settings->value("prefs/hue-rotation", 0).toInt();
    piSettings->monochrome = gui_settings->value("prefs/monochrome", false).toBool();
    piSettings->invert_colours = gui_settings->value("prefs/invert-colours", false).toBool();
    piSettings->show_autocompletion = gui_settings->value("prefs/show-autocompletion", true).toBool();
    piSettings->show_completion_help = gui_settings->value("prefs/show-completion-help", true).toBool();
    piSettings->show_context = gui_settings->value("prefs/show-context", true).toBool();
    piSettings->flash_code = gui_settings->value("prefs/flash-code", true).toBool();
    piSettings->flash_brightness = gui_settings->value("prefs/flash-brightness", 35).toInt();
    piSettings->flash_gutter = gui_settings->value("prefs/flash-gutter", false).toBool();
    piSettings->show_loop_scopes = gui_settings->value("prefs/show-loop-scopes", true).toBool();
    piSettings->loop_scope_scroll = gui_settings->value("prefs/loop-scope-scroll", false).toBool();
    piSettings->speak_transport = gui_settings->value("prefs/speak-transport", true).toBool();
    piSettings->example_play_on_open = gui_settings->value("prefs/example-play-on-open", true).toBool();
    piSettings->reduce_motion = gui_settings->value("prefs/reduce-motion", false).toBool();
    SonicPi::setReduceMotionPreference(piSettings->reduce_motion);
#if defined(Q_OS_MAC)
    int os_shortcut_mode = 3;
#else
    int os_shortcut_mode = 2;
#endif
    piSettings->shortcut_mode = gui_settings->value("prefs/shortcut-mode", os_shortcut_mode).toInt();

    emit settingsChanged();
}

void MainWindow::restoreScopeState(std::vector<QString> names)
{
    std::cout << "[GUI] - restoring scope states " << std::endl;

    for (auto name : names)
    {
        bool def = (name.toLower() == "spectrum");
        piSettings->setScopeState(name, gui_settings->value("prefs/scope/show-" + name.toLower(), def).toBool());
    }
}

void MainWindow::writeSettings()
{
    std::cout << "[GUI] - writing settings" << std::endl;

    gui_settings->setValue("pos", pos());
    gui_settings->setValue("size", size());
    gui_settings->setValue("first_time", 0);

    gui_settings->setValue("prefs/language", piSettings->language);

    gui_settings->setValue("prefs/midi-default-channel", piSettings->midi_default_channel);
    gui_settings->setValue("prefs/midi-enable", piSettings->midi_enabled);
    gui_settings->setValue("prefs/gamepad-enable", piSettings->gamepad_enabled);
    gui_settings->setValue("prefs/osc-public", piSettings->osc_public);
    gui_settings->setValue("prefs/osc-enabled", piSettings->osc_server_enabled);

    gui_settings->setValue("prefs/check-args", piSettings->check_args);
    gui_settings->setValue("prefs/log-synths", piSettings->log_synths);
    gui_settings->setValue("prefs/clear-output-on-run", piSettings->clear_output_on_run);
    gui_settings->setValue("prefs/log-cues", piSettings->log_cues);
    gui_settings->setValue("prefs/log-auto-scroll", piSettings->log_auto_scroll);
    gui_settings->setValue("prefs/show-line-numbers", piSettings->show_line_numbers);
    gui_settings->setValue("prefs/enable-external-synths", piSettings->enable_external_synths);
    gui_settings->setValue("prefs/synth-trigger-timing-guarantees", piSettings->synth_trigger_timing_guarantees);
    gui_settings->setValue("prefs/mixer-force-mono", piSettings->mixer_force_mono);
    gui_settings->setValue("prefs/mixer-invert-stereo", piSettings->mixer_invert_stereo);
    gui_settings->setValue("prefs/enable-scsynth-inputs", piSettings->enable_scsynth_inputs);
    gui_settings->setValue("prefs/audio-driver",        piSettings->audio_driver);
    gui_settings->setValue("prefs/audio-output-device", piSettings->audio_output_device);
    gui_settings->setValue("prefs/audio-input-device",  piSettings->audio_input_device);
    gui_settings->setValue("prefs/audio-sample-rate",   piSettings->audio_sample_rate);
    gui_settings->setValue("prefs/audio-buffer-size",   piSettings->audio_buffer_size);
    gui_settings->setValue("prefs/system-vol", piSettings->main_volume);
    gui_settings->setValue("prefs/rp/check-updates", piSettings->check_updates);
    gui_settings->setValue("prefs/auto-indent-on-run", piSettings->auto_indent_on_run);
    gui_settings->setValue("prefs/gui_transparency", piSettings->gui_transparency);
    gui_settings->setValue("prefs/scope/show-labels", piSettings->show_scope_labels);
    gui_settings->setValue("prefs/scope/show-scopes", piSettings->show_scopes);
    gui_settings->setValue("prefs/show-titles", piSettings->show_titles);
    gui_settings->setValue("prefs/hide-menubar-in-fullscreen", piSettings->hide_menubar_in_fullscreen);
    gui_settings->setValue("prefs/show_cues", piSettings->show_cues);
    gui_settings->setValue("prefs/show_metro", piSettings->show_metro);
    gui_settings->setValue("prefs/syphon_show_cursor", piSettings->syphon_show_cursor);
    gui_settings->setValue("prefs/record_show_cursor", piSettings->record_show_cursor);
    gui_settings->setValue("prefs/record_flash_icon", piSettings->record_flash_icon);
    gui_settings->setValue("prefs/recording_type", static_cast<int>(piSettings->recording_type));
    gui_settings->setValue("prefs/spout_show_cursor", piSettings->spout_show_cursor);
    gui_settings->setValue("prefs/theme", SonicPiTheme::colourSchemeToName(piSettings->colourScheme));
    gui_settings->setValue("prefs/pro-icons", piSettings->proIcons);
    gui_settings->setValue("prefs/hue-rotation", piSettings->hue_rotation);
    gui_settings->setValue("prefs/monochrome", piSettings->monochrome);
    gui_settings->setValue("prefs/invert-colours", piSettings->invert_colours);

    gui_settings->setValue("prefs/show-autocompletion", piSettings->show_autocompletion);
    gui_settings->setValue("prefs/show-completion-help", piSettings->show_completion_help);

    gui_settings->setValue("prefs/show-buttons", piSettings->show_buttons);
    gui_settings->setValue("prefs/show-editor-toolbar", piSettings->show_editor_toolbar);
    gui_settings->setValue("prefs/show-tabs", piSettings->show_tabs);
    gui_settings->setValue("prefs/show-log", piSettings->show_log);
    gui_settings->setValue("prefs/show-context", piSettings->show_context);
    gui_settings->setValue("prefs/flash-code", piSettings->flash_code);
    gui_settings->setValue("prefs/flash-brightness", piSettings->flash_brightness);
    gui_settings->setValue("prefs/flash-gutter", piSettings->flash_gutter);
    gui_settings->setValue("prefs/show-loop-scopes", piSettings->show_loop_scopes);
    gui_settings->setValue("prefs/loop-scope-scroll", piSettings->loop_scope_scroll);
    gui_settings->setValue("prefs/speak-transport", piSettings->speak_transport);
    gui_settings->setValue("prefs/example-play-on-open", piSettings->example_play_on_open);
    if (tutorialPane)
        gui_settings->setValue("prefs/docs-zoom", tutorialPane->userZoom());
    if (quickstartPane)
        gui_settings->setValue("prefs/quickstart-zoom", quickstartPane->userZoom());
    gui_settings->setValue("prefs/reduce-motion", piSettings->reduce_motion);
    gui_settings->setValue("prefs/shortcut-mode", piSettings->shortcut_mode);
    gui_settings->setValue("prefs/log-zoom", outputPane->currentZoomLevel());
    gui_settings->setValue("prefs/cue-zoom", incomingPane->currentZoomLevel());
    std::cout << "[GUI] - writing zoom values " << outputPane->currentZoomLevel() << " " << incomingPane->currentZoomLevel() << std::endl;
    for (auto name : piSettings->scope_names)
    {
        gui_settings->setValue("prefs/scope/show-" + name.toLower(), piSettings->isScopeActive(name));
    }

    gui_settings->setValue("workspace", editorTabWidget->currentIndex());

    for (int w = 0; w < workspace_max; w++)
    {
        gui_settings->setValue(QString("workspace%1zoom").arg(w),
            workspaces[w]->property("zoom"));
    }

    gui_settings->setValue("docsplitState", docsplit->saveState());
    gui_settings->setValue("windowState", saveState());
    gui_settings->setValue("windowGeom", saveGeometry());

    // Force Qt to write the settings to the ini file
    gui_settings->sync();
}

void MainWindow::loadFile(const QString& fileName, SonicPiScintilla*& text)
{
    QFile file(fileName);
    if (!file.open(QFile::ReadOnly))
    {
        QMessageBox::warning(this, tr("Sonic Pi"),
            tr("Cannot read file %1:\n%2.")
                .arg(fileName)
                .arg(file.errorString()));
        updateColourTheme();
        return;
    }

    QTextStream in(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    in.setEncoding(QStringConverter::Utf8);
#else
    in.setCodec("UTF-8");
#endif

    // No wait cursor: the read is instant, and building the cursor image can
    // crash in Qt's Cocoa colorspace path (CGImageCreate PAC trap, 2026-07-02)
    text->setText(in.readAll());
    file.close();
    showStatusAndAnnounce(tr("File loaded..."), 2000);
}

bool MainWindow::saveFile(const QString& fileName, SonicPiScintilla* text)
{
    QFile file(fileName);
    if (!file.open(QFile::WriteOnly))
    {
        QMessageBox::warning(this, tr("Sonic Pi"),
            tr("Cannot write file %1:\n%2.")
                .arg(fileName)
                .arg(file.errorString()));
        updateColourTheme();
        return false;
    }

    QTextStream out(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    out.setEncoding(QStringConverter::Utf8);
#else
    out.setCodec("UTF-8");
#endif

    QString code = text->text();
#if defined(Q_OS_WIN)
    code.replace("\n", "\r\n"); // CRLF for Windows users
    code.replace("\r\r\n", "\r\n"); // don't double-replace if already encoded
#endif
    out << code;
    out.flush();
    file.close();

    showStatusAndAnnounce(tr("File saved..."), 2000);
    return true;
}

SonicPiScintilla* MainWindow::filenameToWorkspace(std::string filename)
{
    std::string s;

    for (int i = 0; i < workspace_max; i++)
    {
        s = "workspace_" + number_name(i);
        if (filename == s)
        {
            return workspaces[i];
        }
    }
    return workspaces[0];
}

void MainWindow::onExitCleanup()
{
    hide();
    std::cout << "[GUI] - initiating Shutdown..." << std::endl;

#if defined(Q_OS_MAC) || defined(Q_OS_WIN)
    // Finalise any in-progress session recording before supersonic
    // shuts down — the audio shm slot disappears with supersonic, and
    // the file's moov atom isn't written until stopSessionRecording
    // returns. Synchronous on purpose.
    if (SonicPi::isSessionRecording()) {
        std::cout << "[GUI] - finalising in-progress session recording..." << std::endl;
        SonicPi::stopSessionRecording();
        freeRecordAudioOutSynth();
    }
#endif

    if (scopeWindow)
    {
        std::cout << "[GUI] - shutting down scope..." << std::endl;
        scopeWindow->ShutDown();
    }

    if (m_spClient)
    {
        if (loaded_workspaces)
        {
            // this should be a synchorous call to avoid the following sleep
            saveWorkspaces();
        }

        std::this_thread::sleep_for(1s);

        // Do this before closing the client, so the io redirect happens after
        std::cout << "[GUI] - exiting. Cheerio :-)" << std::endl;

        // Shuts down the client/server connection
        m_spAPI->Shutdown();
    }
}

void MainWindow::restartApp()
{
    showStatusAndAnnounce(tr("Restarting Sonic Pi..."), 10000);

    qputenv("SONIC_PI_RESTART", "1");
    // Save settings and perform some cleanup
    writeSettings();
    onExitCleanup();

    std::cout << "[GUI] - performing application restart..." << std::endl;

    // Create new process
    QStringList args = qApp->arguments();
    args.removeFirst();
    QProcess process;
    bool restart_success = process.startDetached(qApp->arguments()[0], args);
    if (restart_success)
    {
        std::cout << "[GUI] - successfully restarted sonic-pi" << std::endl;
    }
    else
    {
        std::cout << "[GUI] - failed to restart sonic-pi" << std::endl;
    }

    // Quit
    qApp->exit(0);
    exit(0);
}

void MainWindow::heartbeatOSC()
{
    // Message msg("/gui-heartbeat");
    // msg.pushInt32(guiID);
    // sendOSC(msg);
}

void MainWindow::updateDocPane(QListWidgetItem* cur)
{
    if (!cur)
        return;
    QListWidget* list = cur->listWidget();
    showInTutorialPane(helpLists.indexOf(list), list ? list->row(cur) : -1);
}

bool MainWindow::showInTutorialPane(int tabIdx, int row)
{
    if (!tutorialPane || tabIdx < 0 || row < 0)
        return false;

    // itemPressed + currentItemChanged both fire per click - don't build twice
    // (also keeps a playing snippet's state on a re-click)
    if (tabIdx == lastTutorialDocTab && row == lastTutorialDocRow)
        return true;

    QListWidget* list = helpLists.value(tabIdx);
    QListWidgetItem* item = list ? list->item(row) : nullptr;
    if (!item)
        return false;

    loadNativeDocs();
    const QString keyword = helpTabKeywords.value(tabIdx).value(row);

    auto instrumentByKey = [](const QVector<SonicPi::InstrumentPage>& pages,
                              const QString& key) -> const SonicPi::InstrumentPage* {
        for (const SonicPi::InstrumentPage& page : pages)
            if (page.key == key)
                return &page;
        return nullptr;
    };

    bool built = false;
    if (tabIdx == (int)DocTab::Tutorial && row < tutorialJsonPaths.size())
    {
        QString json = readFile(tutorialJsonPaths[row]);
        if (!json.isEmpty())
        {
            SonicPi::TutorialChapter chapter = SonicPi::TutorialDocs::chapterFromJson(json.toUtf8());
            QString prevTitle = row > 0 ? list->item(row - 1)->text() : QString();
            QString nextTitle = row + 1 < list->count() ? list->item(row + 1)->text() : QString();
            tutorialPane->loadChapter(chapter, rootPath() + "/etc/doc/images", prevTitle, nextTitle);
            built = true;
        }
    }
    else if (tabIdx == (int)DocTab::Examples && row < examplePaths.size())
    {
        QString code = readFile(examplePaths[row]);
        if (!code.isEmpty())
        {
            tutorialPane->showCodePage(exampleTitles.value(row), code);
            built = true;
        }
    }
    else if (tabIdx == (int)DocTab::Synths || tabIdx == (int)DocTab::Fx)
    {
        const bool isFx = tabIdx == (int)DocTab::Fx;
        if (const SonicPi::InstrumentPage* page =
                instrumentByKey(isFx ? fxDocPages : synthDocPages, keyword))
        {
            tutorialPane->showInstrumentPage(isFx, *page);
            built = true;
        }
    }
    else if (tabIdx == (int)DocTab::Samples)
    {
        for (const SonicPi::SampleGroup& group : sampleDocGroups)
            if (group.title == item->text())
            {
                tutorialPane->showSampleGroupPage(group);
                built = true;
                break;
            }
    }
    else if (tabIdx == (int)DocTab::Lang)
    {
        for (const SonicPi::LangPage& page : langDocPages)
            if (page.key == keyword)
            {
                tutorialPane->showLangPage(page);
                built = true;
                break;
            }
    }

    if (!built)
    {
        // Docs data missing for this row (e.g. generated docs out of date):
        // show a title-only page rather than nothing, and say so in the log
        SonicPi::LangPage stub;
        stub.key = item->text();
        tutorialPane->showLangPage(stub);
        std::cout << "[GUI] - no native doc data for help row: "
                  << item->text().toStdString() << std::endl;
    }

    lastTutorialDocTab = tabIdx;
    lastTutorialDocRow = row;
    return true;
}

void MainWindow::loadNativeDocs()
{
    if (nativeDocsLoaded)
        return;
    nativeDocsLoaded = true;
    const QString base = rootPath() + "/etc/doc/generated/native/reference/";
    synthDocPages = SonicPi::TutorialDocs::instrumentsFromJson(readFile(base + "synths.json").toUtf8());
    fxDocPages = SonicPi::TutorialDocs::instrumentsFromJson(readFile(base + "fx.json").toUtf8());
    sampleDocGroups = SonicPi::TutorialDocs::sampleGroupsFromJson(readFile(base + "samples.json").toUtf8());
    langDocPages = SonicPi::TutorialDocs::langPagesFromJson(readFile(base + "lang.json").toUtf8());
}

// A qrc/relative link inside a doc page: select the help row whose page it is
void MainWindow::showHelpPageForUrl(const QUrl& url)
{
    QString target = url.toString();
    QString fileName = target.section('/', -1);
    for (int tab = 0; tab < helpLists.size(); tab++)
    {
        QListWidget* list = helpLists[tab];
        for (int row = 0; row < list->count(); row++)
        {
            QString pageUrl = list->item(row)->data(32).toString();
            if (pageUrl == target || (!fileName.isEmpty() && pageUrl.endsWith("/" + fileName)))
            {
                showHelpListTab(tab, row);
                return;
            }
        }
    }
    std::cout << "[GUI] - no help page for link: " << target.toStdString() << std::endl;
}

void MainWindow::updateDocPane2(QListWidgetItem* cur, QListWidgetItem* prev)
{
    (void)prev;
    updateDocPane(cur);
}

void MainWindow::addHelpPage(QListWidget* nameList,
    struct help_page* helpPages, int len)
{
    int i;
    struct help_entry entry;
    entry.pageIndex = docsNavTabs->count() - 1;

    for (i = 0; i < len; i++)
    {
        QListWidgetItem* item = new QListWidgetItem(helpPages[i].title);
        item->setData(32, QVariant(helpPages[i].url));
        // Searchable symbol (bass_foundation) alongside the display title
        // ("Bass Foundation") so the filter matches text pasted from code.
        item->setData(33, QVariant(QString(helpPages[i].keyword)));
        nameList->addItem(item);
        entry.entryIndex = nameList->count() - 1;
        helpTabKeywords[entry.pageIndex] << helpPages[i].keyword;

        if (helpPages[i].keyword != "")
        {
            helpKeywords.insert(helpPages[i].keyword, entry);
            switch ((DocTab)entry.pageIndex)
            {
            case DocTab::Synths:
                autocomplete->addSymbol(ScintillaAPI::Synth, helpPages[i].keyword);
                break;
            case DocTab::Fx:
                autocomplete->addSymbol(ScintillaAPI::FX, helpPages[i].keyword);
                break;
            case DocTab::Lang:
                autocomplete->addKeyword(ScintillaAPI::Func, helpPages[i].keyword);
                break;
            default:
                break;
            }
        }
    }
}

// Two-tone painting for the tutorial chapter list: chapter numbers in a muted
// tint, titles in the theme foreground, top-level chapters bold. Painting
// only — the item's text (and so its screen-reader announcement) is unchanged.
class DocsNavDelegate : public QStyledItemDelegate
{
public:
    DocsNavDelegate(SonicPiTheme* theme, QObject* parent)
        : QStyledItemDelegate(parent)
        , m_theme(theme)
    {
    }

    void paint(QPainter* painter, const QStyleOptionViewItem& option,
               const QModelIndex& index) const override
    {
        QStyleOptionViewItem opt = option;
        initStyleOption(&opt, index);

        static const QRegularExpression numbered(
            QStringLiteral("^(\\s*)(\\d+(?:\\.\\d+)*) (.*)$"));
        QRegularExpressionMatch match = numbered.match(opt.text);
        if (!match.hasMatch())
        {
            QStyledItemDelegate::paint(painter, option, index);
            return;
        }

        const QString indent = match.captured(1);
        const QString number = match.captured(2);
        const QString title = match.captured(3);
        const bool topLevel = !number.contains('.');

        // Pill background (selection / hover / resting) comes from the
        // stylesheet; draw it text-free, then match the text to its fill.
        opt.text.clear();
        QStyle* style = opt.widget ? opt.widget->style() : QApplication::style();
        style->drawControl(QStyle::CE_ItemViewItem, &opt, painter, opt.widget);

        QColor fill = m_theme->color("WindowBackground");
        QColor fg = m_theme->color("WindowForeground");
        if (opt.state & QStyle::State_Selected)
            fill = m_theme->color("MenuSelected");
        else if (opt.state & QStyle::State_MouseOver)
            fill = m_theme->color("ScrollBarHover");
        if (opt.state & (QStyle::State_Selected | QStyle::State_MouseOver))
            fg = m_theme->contrastingText(fill);
        const QColor numColour = SonicPiTheme::blend(fg, fill, 0.45);

        QFont titleFont = opt.font;
        titleFont.setBold(topLevel);
        const QFontMetrics numMetrics(opt.font);
        const QFontMetrics titleMetrics(titleFont);

        const QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &opt, opt.widget);
        painter->save();
        int x = textRect.left() + numMetrics.horizontalAdvance(indent);
        painter->setFont(opt.font);
        painter->setPen(numColour);
        painter->drawText(QRect(x, textRect.top(), qMax(0, textRect.right() - x), textRect.height()),
                          Qt::AlignVCenter | Qt::TextSingleLine, number);
        x += numMetrics.horizontalAdvance(number + QStringLiteral("  "));
        const int titleWidth = qMax(0, textRect.right() - x);
        painter->setFont(titleFont);
        painter->setPen(fg);
        painter->drawText(QRect(x, textRect.top(), titleWidth, textRect.height()),
                          Qt::AlignVCenter | Qt::TextSingleLine,
                          titleMetrics.elidedText(title, Qt::ElideRight, titleWidth));
        painter->restore();
    }

private:
    SonicPiTheme* m_theme;
};

QListWidget* MainWindow::createHelpTab(QString name)
{
    QListWidget* nameList = new QListWidget;
    nameList->setObjectName("docsNavList");   // borderless list with pill rows, see app.qss
    nameList->setFrameShape(QFrame::NoFrame);
    nameList->setAccessibleName(tr("Help Topics"));
    nameList->setSpacing(ScaleHeightForDPI(1));
    // Titles elide rather than growing a horizontal scrollbar under the list.
    nameList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    nameList->setItemDelegate(new DocsNavDelegate(theme, nameList));
    // Both signals on purpose: currentItemChanged covers keyboard navigation,
    // itemPressed covers re-clicking the already-current row (to return after
    // following links away in the browser pane). The native pane dedupes via
    // lastTutorialDocTab/Row so a click never builds a page twice.
    connect(nameList,
        SIGNAL(itemPressed(QListWidgetItem*)),
        this, SLOT(updateDocPane(QListWidgetItem*)));
    connect(nameList,
        SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
        this, SLOT(updateDocPane2(QListWidgetItem*, QListWidgetItem*)));

    QLineEdit* filter = new QLineEdit;
    filter->setObjectName("docsFilter");   // pill-shaped search field, see app.qss
    filter->setPlaceholderText(tr("Filter %1...").arg(name));
    filter->setAccessibleName(tr("Filter %1 help topics").arg(name));
    filter->setClearButtonEnabled(true);
    // Leading magnifier so the field reads as search at a glance; decorative
    // only (the accessible name above carries the semantics). Tinted for the
    // current theme here and re-tinted on theme changes (updateDocsFilterIcons).
    docsFilterSearchActions.append(filter->addAction(QIcon(), QLineEdit::LeadingPosition));
    updateDocsFilterIcons();
    connect(filter, &QLineEdit::textChanged, nameList, [nameList](const QString& q) {
        for (int i = 0; i < nameList->count(); i++)
        {
            QListWidgetItem* item = nameList->item(i);
            // Match the display title OR the raw symbol (data 33), so
            // underscored names pasted from code still filter.
            const bool match = q.isEmpty()
                               || item->text().contains(q, Qt::CaseInsensitive)
                               || item->data(33).toString().contains(q, Qt::CaseInsensitive);
            item->setHidden(!match);
        }
    });

    QBoxLayout* layout = new QBoxLayout(QBoxLayout::TopToBottom);
    // Air around the nav column so the list and filter don't press against the
    // splitter and window edges; the bottom stays tight to the tab chips.
    layout->setContentsMargins(ScaleWidthForDPI(8), ScaleHeightForDPI(8),
                               ScaleWidthForDPI(4), ScaleHeightForDPI(2));
    layout->setSpacing(ScaleHeightForDPI(6));
    layout->addWidget(filter);
    layout->addWidget(nameList, 1);
    QWidget* tabWidget = new QWidget;
    // Tinted sidebar (see #docsNavPage in app.qss) so the nav column reads as
    // its own surface against the white/dark content pane.
    tabWidget->setObjectName("docsNavPage");
    tabWidget->setAttribute(Qt::WA_StyledBackground, true);
    tabWidget->setLayout(layout);
    docsNavTabs->addTab(tabWidget, name);
    helpLists.append(nameList);
    return nameList;
}

void MainWindow::helpScrollUp()
{
    int section = docsNavTabs->currentIndex();
    int entry = helpLists[section]->currentRow();

    if (entry > 0)
        entry--;
    helpLists[section]->setCurrentRow(entry);
}

void MainWindow::helpScrollDown()
{
    int section = docsNavTabs->currentIndex();
    int entry = helpLists[section]->currentRow();

    if (entry < helpLists[section]->count() - 1)
        entry++;
    helpLists[section]->setCurrentRow(entry);
}

void MainWindow::docPrevTab()
{
    int section = docsNavTabs->currentIndex();
    if (section > 0)
        docsNavTabs->setCurrentIndex(section - 1);
}

void MainWindow::docNextTab()
{
    int section = docsNavTabs->currentIndex();
    if (section < docsNavTabs->count() - 1)
        docsNavTabs->setCurrentIndex(section + 1);
}

void MainWindow::docScrollUp()
{
    tutorialPane->scrollStep(-1);
}

void MainWindow::docScrollDown()
{
    tutorialPane->scrollStep(1);
}

void MainWindow::tabNext()
{
    int index = editorTabWidget->currentIndex();
    if (index == editorTabWidget->count() - 1)
        index = 0;
    else
        index++;
    QMetaObject::invokeMethod(editorTabWidget, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::tabPrev()
{
    int index = editorTabWidget->currentIndex();
    if (index == 0)
        index = editorTabWidget->count() - 1;
    else
        index--;
    QMetaObject::invokeMethod(editorTabWidget, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::tabGoto(int index)
{
    if (index < editorTabWidget->count())
        QMetaObject::invokeMethod(editorTabWidget, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::setLineMarkerinCurrentWorkspace(int num, bool isSyntaxError, const QString& errorToken, int colStart, int colEnd)
{
    if (num > 0)
    {
        SonicPiScintilla* ws = getCurrentWorkspace();
        ws->setLineErrorMarker(num - 1, isSyntaxError, errorToken, colStart, colEnd);
        m_errorJumpTab = editorTabWidget->currentIndex();
        m_errorJumpLine = num - 1;
        m_errorJumpCol = colStart >= 0 ? colStart : 0;
    }
}
// TODO remove
void MainWindow::setUpdateInfoText(QString t)
{
    //  update_info->setText(t);
}

void MainWindow::addUniversalCopyShortcuts(QTextEdit* te)
{
    QShortcut* copyShortcutCtrl = new QShortcut(ctrlKey("c"), te, SLOT(copy()));
    copyShortcutCtrl->setContext(Qt::WidgetShortcut);

    QShortcut* selectAllShortcutCtrl = new QShortcut(ctrlKey("a"), te, SLOT(selectAll()));
    selectAllShortcutCtrl->setContext(Qt::WidgetShortcut);

    QShortcut* copyShortcutMeta = new QShortcut(metaKey("c"), te, SLOT(copy()));
    copyShortcutMeta->setContext(Qt::WidgetShortcut);

    QShortcut* selectAllShortcutMeta = new QShortcut(metaKey("a"), te, SLOT(selectAll()));
    selectAllShortcutMeta->setContext(Qt::WidgetShortcut);
}

QString MainWindow::asciiArtLogo()
{
    return readFile(":/images/logo.txt");
}

void MainWindow::printAsciiArtLogo()
{
    QString s = asciiArtLogo();
#if QT_VERSION >= 0x050400
    qDebug().noquote() << s;
#else
    // noquote requires QT 5.4
    qDebug() << s;
#endif
}

void MainWindow::requestVersion()
{
    oscpkt::Message msg("/version");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::updateVersionNumber(QString v, int v_num, QString latest_v, int latest_v_num, QDate last_checked, QString platform)
{
    version = v;
    version_num = v_num;
    latest_version = latest_v;
    latest_version_num = latest_v_num;

    // update status bar
    versionLabel->setText(QString("Sonic Pi " + v + " on " + platform + " "));

    // update preferences
    QString last_update_check = tr("Last checked %1").arg(last_checked.toString());

    QString preamble = tr("Sonic Pi checks for updates\nevery two weeks.");

    QString print_version = tr("This is Sonic Pi %1");
    QString new_version = tr("Version %2 is now available!");

    if (v_num < latest_v_num)
    {
        QString info = QString(preamble + "\n\n" + print_version + "\n\n" + new_version).arg(version, latest_version);
        QString visit = tr("New version available!\nGet Sonic Pi %1").arg(latest_version);
        settingsWidget->updateVersionInfo(info, visit, true, false);
    }
    else
    {
        QString info = (preamble + "\n\n" + print_version + "\n\n" + last_update_check).arg(version);
        QString visit = tr("Visit http://sonic-pi.net to download new version");
        settingsWidget->updateVersionInfo(info, visit, false, true);
    }
}

void MainWindow::addCuePath(QString path, QString val)
{
    Q_UNUSED(val);

    if (!path.startsWith(":"))
    {
        path = "\"" + path + "\"";
    }

    if (!cuePaths.contains(path))
    {
        autocomplete->addCuePath(path);
        cuePaths << path;
    }
}

void MainWindow::toggleMidi(int silent)
{
    QSignalBlocker blocker(midiEnabledAct);
    midiEnabledAct->setChecked(piSettings->midi_enabled);

    if (piSettings->midi_enabled)
    {
        showStatusAndAnnounce(tr("Enabling MIDI <input>..."), 2000);
        oscpkt::Message msg("/midi-start");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
    else
    {
        showStatusAndAnnounce(tr("Disabling MIDI input..."), 2000);
        oscpkt::Message msg("/midi-stop");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
}

void MainWindow::toggleGamepad(int silent)
{
    QSignalBlocker blocker(gamepadEnabledAct);
    gamepadEnabledAct->setChecked(piSettings->gamepad_enabled);

    if (piSettings->gamepad_enabled)
    {
        showStatusAndAnnounce(tr("Enabling gamepad input..."), 2000);
        oscpkt::Message msg("/gamepad-start");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
    else
    {
        showStatusAndAnnounce(tr("Disabling gamepad input..."), 2000);
        oscpkt::Message msg("/gamepad-stop");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
}

// Per-device mute persistence lives server-side (the spider's settings
// store) — the GUI just forwards the toggle and displays whatever the
// device-list broadcasts report.
void MainWindow::setMidiPortEnabled(QString direction, QString name, bool enabled)
{
    oscpkt::Message msg("/midi-port-enable");
    msg.pushInt32(guiID);
    msg.pushStr(direction.toStdString());
    msg.pushStr(name.toStdString());
    msg.pushInt32(enabled ? 1 : 0);
    sendOSC(msg);
}

void MainWindow::setGamepadDeviceEnabled(QString name, bool enabled)
{
    oscpkt::Message msg("/gamepad-enable");
    msg.pushInt32(guiID);
    msg.pushStr(name.toStdString());
    msg.pushInt32(enabled ? 1 : 0);
    sendOSC(msg);
}

void MainWindow::toggleOSCServer(int silent)
{
    QSignalBlocker blocker(enableOSCServerAct);
    allowRemoteOSCAct->setEnabled(piSettings->osc_server_enabled);
    if (piSettings->osc_server_enabled)
    {
        enableOSCServerAct->setChecked(true);
        std::cout << "[GUI] - asking OSC server to start" << std::endl;
        oscpkt::Message msg("/cue-port-start");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
    else
    {

        enableOSCServerAct->setChecked(false);
        showStatusAndAnnounce(tr("Disabling OSC cue port..."), 2000);
        std::cout << "[GUI] - asking OSC server to stop" << std::endl;
        oscpkt::Message msg("/cue-port-stop");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }

    QSignalBlocker blocker2(allowRemoteOSCAct);
    if (piSettings->osc_public)
    {
        allowRemoteOSCAct->setChecked(true);

        if (piSettings->osc_server_enabled)
        {
            showStatusAndAnnounce(tr("Enabling external OSC cue port..."), 2000);
        }

        std::cout << "[GUI] - cue port in external mode" << std::endl;
        oscpkt::Message msg("/cue-port-external");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
    else
    {
        allowRemoteOSCAct->setChecked(false);

        if (piSettings->osc_server_enabled)
        {
            showStatusAndAnnounce(tr("Enabling internal OSC cue port..."), 2000);
        }
        std::cout << "[GUI] - cue port in internal mode" << std::endl;
        oscpkt::Message msg("/cue-port-internal");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
}

bool MainWindow::eventFilter(QObject* obj, QEvent* event)
{
    if (obj == qApp && (event->type() == QEvent::ApplicationActivate))
    {
        statusBar()->showMessage(tr("Welcome back. Now get your live code on..."), 2000);
        update();
    }

    // Swap the help ✕ glyph to its accent hover tint (flat button, no chip).
    if (helpCloseButton && obj == helpCloseButton
        && (event->type() == QEvent::Enter || event->type() == QEvent::Leave))
    {
        helpCloseButton->setIcon(event->type() == QEvent::Enter ? m_helpCloseIconHover
                                                                : m_helpCloseIcon);
    }

    if (event->type() == QEvent::FileOpen)
    {
        const QString file = static_cast<QFileOpenEvent*>(event)->file();
        if (file.endsWith(".sonicpi", Qt::CaseInsensitive))
        {
            openSetPath(file);
            return true;
        }
    }

    if (event->type() == QEvent::Shortcut)
    {
        QShortcutEvent* sc = static_cast<QShortcutEvent*>(event);
        const QKeySequence& ks = sc->key();
        // Ambiguous-only: escape2Sc already handles the unambiguous case
        if (ks == QKeySequence("Escape") && sc->isAmbiguous())
        {
            escapeWorkspaces();
        }
    }

    // Double-clicking anywhere on the editor / docks divider bar toggles the
    // dock open/closed.
    if (event->type() == QEvent::MouseButtonDblClick && docWidget && mainWidget && docWidget->isVisible())
    {
        const QPoint g = static_cast<QMouseEvent*>(event)->globalPosition().toPoint();
        const int sepTop = mainWidget->mapToGlobal(QPoint(0, mainWidget->height())).y();
        const int sepBot = docWidget->mapToGlobal(QPoint(0, 0)).y();
        const int dockL = docWidget->mapToGlobal(QPoint(0, 0)).x();
        const int dockR = dockL + docWidget->width();
        if (g.y() >= qMin(sepTop, sepBot) - 4 && g.y() <= qMax(sepTop, sepBot) + 4 &&
            g.x() >= dockL && g.x() <= dockR)
        {
            toggleDocPane();
            return true;
        }
    }

    return QMainWindow::eventFilter(obj, event);
}

QString MainWindow::sonicPiHomePath()
{
    return QString::fromStdString(m_spAPI->GetPath(SonicPiPath::HomePath));
}

QString MainWindow::sonicPiConfigPath()
{
    return QString::fromStdString(m_spAPI->GetPath(SonicPiPath::ConfigPath));
}

QString MainWindow::shortcutsConfigPath()
{
    return sonicPiConfigPath() + QDir::separator() + "v5-keyboard-shortcuts.ini";
}

void MainWindow::zoomInLogs()
{
    outputPane->zoomIn();
    incomingPane->zoomIn();
}

void MainWindow::zoomOutLogs()
{
    outputPane->zoomOut();
    incomingPane->zoomOut();
}

// Parse "enabled<TAB>name" device lines (bare names = enabled).
static QList<QPair<QString, bool>> parseDeviceLines(const QString& info)
{
    QList<QPair<QString, bool>> out;
    for (const QString& rawLine : info.split('\n', Qt::SkipEmptyParts))
    {
        const QString line = rawLine.trimmed();
        if (line.isEmpty()) continue;
        const int tab = line.indexOf('\t');
        const QString name = (tab >= 0) ? line.mid(tab + 1).trimmed() : line;
        const bool enabled = (tab < 0) || (line.left(tab).trimmed() != "0");
        if (!name.isEmpty()) out.append({ name, enabled });
    }
    return out;
}

// Rebuild an IO device submenu: one checkable entry per device reflecting the
// engine's enabled flag, each forwarding its toggle to `onToggle`. Falls back
// to a single disabled placeholder when nothing is connected. The menu mirrors
// the per-device checkboxes in the IO preferences — a user toggle round-trips
// through the spider, which rebroadcasts the device list and re-enters here.
template <typename Toggle>
static void populateDeviceMenu(QMenu* menu, const QList<QPair<QString, bool>>& devices,
                               const QString& emptyText, Toggle onToggle)
{
    menu->clear();
    if (devices.isEmpty())
    {
        menu->addAction(emptyText)->setEnabled(false);
        return;
    }
    for (const auto& d : devices)
    {
        const QString name = d.first;
        QAction* act = menu->addAction(name);
        act->setCheckable(true);
        act->setChecked(d.second);
        QObject::connect(act, &QAction::triggered, menu,
                         [onToggle, name](bool checked) { onToggle(name, checked); });
    }
}

void MainWindow::updateMIDIInPorts(QString port_info)
{
    settingsWidget->updateMidiInPorts(port_info);
    populateDeviceMenu(ioMidiInMenu, parseDeviceLines(port_info), tr("No Connected Inputs"),
                       [this](const QString& name, bool enabled) { setMidiPortEnabled("in", name, enabled); });
}

void MainWindow::updateMIDIOutPorts(QString port_info)
{
    settingsWidget->updateMidiOutPorts(port_info);
    const auto devices = parseDeviceLines(port_info);
    QStringList names;
    for (const auto& d : devices) names << d.first;
    autocomplete->updateMidiOuts(names.join("\n"));
    populateDeviceMenu(ioMidiOutMenu, devices, tr("No Connected Outputs"),
                       [this](const QString& name, bool enabled) { setMidiPortEnabled("out", name, enabled); });
}

void MainWindow::updateGamepadDevices(QString devices)
{
    settingsWidget->updateGamepadDevices(devices);
    populateDeviceMenu(ioGamepadMenu, parseDeviceLines(devices), tr("No Connected Controllers"),
                       [this](const QString& name, bool enabled) { setGamepadDeviceEnabled(name, enabled); });
}

void MainWindow::focusPane(QWidget* pane)
{
    if (!pane) return;
    pane->setFocusPolicy(Qt::StrongFocus);
    pane->setVisible(true);
    pane->raise();
    pane->setFocus(Qt::OtherFocusReason);
    pane->activateWindow();
}

void MainWindow::revealDocsTab()
{
    docWidget->show();
    southTabs->setCurrentWidget(docsplit);   // may currently be on Debug or another tab
    updatePrefsIcon();
}

void MainWindow::announce(const QString& message, bool assertive,
                          SonicPi::Announcement category)
{
    // No-op unless a screen reader is connected.
    if (message.isEmpty() || !QAccessible::isActive())
        return;
    SonicPi::AnnouncementPolicy policy;
    policy.speakTransport = piSettings->speak_transport;
    if (!policy.shouldSpeak(category))
        return;
#if QT_VERSION >= QT_VERSION_CHECK(6, 8, 0)
    // QAccessibleAnnouncementEvent arrived in Qt 6.8; on older Qt this is a no-op.
    QAccessibleAnnouncementEvent ev(this, message);
    ev.setPoliteness(assertive ? QAccessible::AnnouncementPoliteness::Assertive
                               : QAccessible::AnnouncementPoliteness::Polite);
    QAccessible::updateAccessibility(&ev);
#else
    Q_UNUSED(assertive);
#endif
}

void MainWindow::showStatusAndAnnounce(const QString& message, int timeoutMs)
{
    statusBar()->showMessage(message, timeoutMs);
    if (bootAnnouncementsReady)
    {
        announce(message);
    }
}

void MainWindow::mirrorToolTipsToAccessibleDescriptions()
{
    const QList<QWidget*> widgets = findChildren<QWidget*>();
    for (QWidget* w : widgets)
    {
        QString tip = w->toolTip();
        if (tip.isEmpty() || !w->accessibleDescription().isEmpty())
            continue;
        // Some tooltips carry markup for the custom tooltip renderer; the
        // description wants plain prose.
        if (Qt::mightBeRichText(tip))
        {
            QTextDocument doc;
            doc.setHtml(tip);
            tip = doc.toPlainText().simplified();
        }
        // Skip when the tooltip merely repeats the accessible name — a
        // duplicate description is noise for screen-reader users.
        if (tip.isEmpty() || tip == w->accessibleName())
            continue;
        w->setAccessibleDescription(tip);
    }
}

void MainWindow::focusContext()
{
    focusPane(getCurrentEditor()->getContext());
}

void MainWindow::focusLogs()
{
    outputWidget->show();
    focusPane(outputPane);
}

void MainWindow::focusEditor()
{
    focusPane(getCurrentWorkspace());
}

void MainWindow::focusCues()
{
    incomingWidget->show();
    focusPane(incomingPane);
}

void MainWindow::focusPreferences()
{
    prefsWidget->show();
    prefsWidget->raise();
    updatePrefsIcon();
    focusPane(settingsWidget);
}

void MainWindow::focusHelpListing()
{
    revealDocsTab();
    const int i = docsNavTabs->currentIndex();
    focusPane((i >= 0 && i < helpLists.size()) ? (QWidget*)helpLists[i] : (QWidget*)docsNavTabs);
}

void MainWindow::focusHelpDetails()
{
    revealDocsTab();
    focusPane(tutorialPane);
}

void MainWindow::focusErrors()
{
    focusPane(errorCard->isVisible() ? static_cast<QWidget*>(errorCard) : static_cast<QWidget*>(errorPane));
}

void MainWindow::cycleFocusForward()
{
    cycleFocus(1);
}

void MainWindow::cycleFocusBack()
{
    cycleFocus(-1);
}

// F6/Shift+F6 walk the panes that are currently on screen, in rough layout
// order, wrapping at the ends. Hidden panes are skipped — revealing a pane
// stays the job of the Focus menu / Ctrl+Shift jumps.
void MainWindow::cycleFocus(int direction)
{
    QList<QWidget*> panes;
    auto add = [&panes](QWidget* w) {
        // isVisible() alone is not enough: a splitter section dragged (or
        // clamped) to zero size still reports visible, and focusing it would
        // strand the keyboard in a pane with no on-screen presence.
        if (w && w->isVisible() && w->width() > 0 && w->height() > 0)
            panes << w;
    };
    add(getCurrentWorkspace());
    add(getCurrentEditor()->getContext());
    add(outputPane);
    add(incomingPane);
    add(errorPane);
    const int helpIdx = docsNavTabs->currentIndex();
    add((helpIdx >= 0 && helpIdx < helpLists.size()) ? (QWidget*)helpLists[helpIdx]
                                                     : (QWidget*)docsNavTabs);
    add(tutorialPane);
    add(settingsWidget);
    if (panes.isEmpty())
        return;

    QWidget* focused = QApplication::focusWidget();
    int current = -1;
    for (int i = 0; focused && i < panes.size(); ++i)
    {
        if (panes[i] == focused || panes[i]->isAncestorOf(focused))
        {
            current = i;
            break;
        }
    }
    // Focus outside any pane (toolbar, menus…): enter the ring at whichever
    // end matches the travel direction.
    const int next = (current < 0)
        ? (direction > 0 ? 0 : panes.size() - 1)
        : (current + direction + panes.size()) % panes.size();
    focusPane(panes[next]);
}

void MainWindow::focusBPMScrubber()
{
    metroWidget->show();        // the metronome dock — not the Help dock
    metroPane->setVisible(true);
    updatePrefsIcon();
    metroPane->setFocusBPMScrubber();
}

void MainWindow::focusTimeWarpScrubber()
{
    metroWidget->show();
    metroPane->setVisible(true);
    updatePrefsIcon();
    metroPane->setFocusTimeWarpScrubber();
}

void MainWindow::updateContextWithCurrentWs()
{

    SonicPiScintilla* ws = getCurrentWorkspace();
    int line, index;
    ws->getCursorPosition(&line, &index);
    updateContext(line, index);
}

void MainWindow::updateContext(int line, int index)
{
    getCurrentEditor()->setContextContent(tr("Line: %1,  Position: %2").arg(line + 1).arg(index + 1));
}

SonicPiLog* MainWindow::GetOutputPane() const
{
    return outputPane;
}

SonicPiLog* MainWindow::GetIncomingPane() const
{
    return incomingPane;
}

SonicPiTheme* MainWindow::GetTheme() const
{
    return theme;
}

void MainWindow::movePrefsWidget()
{
    int h = toolBar->size().height() + 20;
    int full_width = this->size().width();
    int w = full_width - prefsWidget->size().width();
    prefsWidget->move(w, h);
}

// Stop any in-flight prefs slide so rapid toggling doesn't fight itself.
static void cancelPrefsSlide(QWidget* prefsWidget)
{
    for (auto* a : prefsWidget->findChildren<QPropertyAnimation*>()) {
        a->stop();
        a->deleteLater();
    }
}

void MainWindow::slidePrefsWidgetIn()
{
    int h = toolBar->size().height() + 20;
    int full_width = this->size().width();
    int w = full_width - prefsWidget->size().width();

    cancelPrefsSlide(prefsWidget);

    // With reduce motion preferred (in-app setting or OS accessibility
    // setting), place the pane directly instead of sliding it in.
    if (SonicPi::prefersReducedMotion()) {
        prefsWidget->move(w, h);
        prefsWidget->show();
        prefsWidget->raise();
        return;
    }

    prefsWidget->move(full_width, h);
    prefsWidget->show();
    prefsWidget->raise();

    QPropertyAnimation* anim = new QPropertyAnimation(prefsWidget, "pos", prefsWidget);
    anim->setDuration(220);
    anim->setEasingCurve(QEasingCurve::OutCubic);
    anim->setStartValue(QPoint(full_width, h));
    anim->setEndValue(QPoint(w, h));
    connect(anim, &QPropertyAnimation::finished, this, [this]() { movePrefsWidget(); });
    anim->start(QAbstractAnimation::DeleteWhenStopped);
}

void MainWindow::slidePrefsWidgetOut()
{
    int h = toolBar->size().height() + 20;
    int full_width = this->size().width();

    cancelPrefsSlide(prefsWidget);

    if (SonicPi::prefersReducedMotion()) {
        prefsWidget->hide();
        return;
    }

    QPropertyAnimation* anim = new QPropertyAnimation(prefsWidget, "pos", prefsWidget);
    anim->setDuration(180);
    anim->setEasingCurve(QEasingCurve::InCubic);
    anim->setStartValue(prefsWidget->pos());
    anim->setEndValue(QPoint(full_width, h));
    // Refresh the toolbar icon once actually hidden: togglePrefs calls
    // updatePrefsIcon while the pane is still sliding (isVisible()==true),
    // which otherwise leaves the "open" icon showing after the pane is gone.
    connect(anim, &QPropertyAnimation::finished, this, [this]() {
        prefsWidget->hide();
        updatePrefsIcon();
    });
    anim->start(QAbstractAnimation::DeleteWhenStopped);
}

void MainWindow::resizeEvent(QResizeEvent* e)
{
    movePrefsWidget();
    QMainWindow::resizeEvent(e);
}

SonicPiScintilla* MainWindow::getCurrentWorkspace()
{
    return getCurrentEditor()->getWorkspace();
}

SonicPiEditor* MainWindow::getCurrentEditor()
{
    return (SonicPiEditor*)editorTabWidget->currentWidget();
}

QString MainWindow::currentSynthForCompletion()
{
    SonicPiScintilla* ws = getCurrentWorkspace();
    if (!ws) return "beep";
    int line = 0, index = 0;
    ws->getCursorPosition(&line, &index);
    // Text from the start of the buffer up to (and including) the cursor line.
    QString preceding;
    for (int i = 0; i <= line; ++i)
        preceding += ws->text(i);
    // The last literal use_synth / with_synth before the cursor wins. Dynamic
    // forms (variables, expressions) can't be resolved statically, so we fall
    // back to the default synth, :beep.
    static const QRegularExpression re(
        QStringLiteral("(?:use_synth|with_synth)\\s+:([A-Za-z0-9_]+)"));
    QString synth = QStringLiteral("beep");
    auto it = re.globalMatch(preceding);
    while (it.hasNext())
        synth = it.next().captured(1);
    return synth;
}

void MainWindow::updateScsynthInfo(QString description)
{
    settingsWidget->updateScsynthInfo(description);
}

void MainWindow::updateAudioDevices(const SonicPi::AudioDevicesInfo& devicesInfo)
{
    m_lastAudioDevices = devicesInfo;
    m_audioDevicesSeen = true;
    settingsWidget->updateAudioDevices(devicesInfo);
    maybeRestoreAudioIntent();
}

void MainWindow::updateAudioInputDevices(const SonicPi::AudioInputDevicesInfo& devicesInfo)
{
    m_lastAudioInputDevices = devicesInfo;
    m_audioInputDevicesSeen = true;
    settingsWidget->updateAudioInputDevices(devicesInfo);
    maybeRestoreAudioIntent();
}

void MainWindow::updateAudioDeviceConfig(const SonicPi::AudioDeviceConfigInfo& configInfo)
{
    m_lastAudioDeviceConfig = configInfo;
    m_audioDeviceConfigSeen = true;
    settingsWidget->updateAudioDeviceConfig(configInfo);
    // Spectrum bucket frequencies depend on the engine sample rate
    m_spAPI->AudioProcessor_SetSampleRate(configInfo.sampleRate);

    // Code flashes lag by the device output latency so the pulse lands with
    // the audible sound; scopes align via the engine's sample clock and
    // don't read this. Re-derived on every config broadcast so
    // device/buffer switches stay in sync.
    m_visualLatencyMs = configInfo.sampleRate > 0
        ? (int)std::lround(1000.0 * configInfo.outputLatencySamples / configInfo.sampleRate)
        : 0;

    // Don't re-push mixer settings here — Spider's cold_swap_reinit!
    // does that in Phase 4 once the new mixer node exists
    maybeRestoreAudioIntent();
}

void MainWindow::maybeRestoreAudioIntent()
{
    // Wait for SuperSonic's full initial state before diffing against intent
    if (m_audioIntentRestored) return;
    if (!m_audioDevicesSeen || !m_audioInputDevicesSeen || !m_audioDeviceConfigSeen) return;

    // Driver first — a switch cascades a device re-open, so leave
    // device/rate/buffer for the next broadcast to settle. For a driver
    // with no engine-remembered device (ASIO on a fresh boot) the switch
    // only records a pending intent; the device pass below then commits
    // it by naming the saved output, which the engine resolves under the
    // intended driver.
    const QString currentDriver = QString::fromStdString(m_lastAudioDeviceConfig.currentDriver);
    if (!m_audioDriverRestoreSent
        && !piSettings->audio_driver.isEmpty()
        && piSettings->audio_driver != currentDriver) {
        m_audioDriverRestoreSent = true;
        std::cout << "[gui-audio] restore: driver '"
                  << currentDriver.toUtf8().constData() << "' -> '"
                  << piSettings->audio_driver.toUtf8().constData() << "'" << std::endl;
        switchAudioDriver(piSettings->audio_driver);
        return;
    }
    if (m_audioDriverRestoreSent && piSettings->audio_driver != currentDriver) {
        // Driver ask dispatched but the engine hasn't reflected it yet —
        // either the swap is still in flight (device-list churn fires
        // broadcasts mid-swap; the atomic switch below would bounce off
        // the engine's swap mutex) or the engine recorded a pending
        // intent instead of opening a device. Proceed only once the
        // report shows one or the other.
        const bool intentRecorded =
            m_lastAudioDeviceConfig.hasIntendedDriver
            && piSettings->audio_driver
               == QString::fromStdString(m_lastAudioDeviceConfig.intendedDriver);
        if (!intentRecorded) return;
    }
    m_audioIntentRestored = true;

    // One atomic switch for output/input/rate/buffer — separate calls
    // race inside SuperSonic's 500ms debounce buffer
    QString currentOutput;
    if (m_lastAudioDevices.mode.empty() || m_lastAudioDevices.mode == "system") {
        currentOutput = QString("__system__");
    } else {
        currentOutput = QString::fromStdString(m_lastAudioDevices.currentDevice);
    }
    const QString currentInput = QString::fromStdString(m_lastAudioInputDevices.currentDevice);

    const bool needOutput = !piSettings->audio_output_device.isEmpty()
                         && piSettings->audio_output_device != currentOutput;
    const bool needInput  = !piSettings->audio_input_device.isEmpty()
                         && piSettings->audio_input_device != "__disabled__"
                         && piSettings->audio_input_device != "__none__"
                         && piSettings->audio_input_device != currentInput;
    const bool needRate   = piSettings->audio_sample_rate > 0
                         && piSettings->audio_sample_rate != m_lastAudioDeviceConfig.sampleRate;
    const bool needBuffer = piSettings->audio_buffer_size > 0
                         && piSettings->audio_buffer_size != m_lastAudioDeviceConfig.bufferSize;

    if (needOutput || needInput || needRate || needBuffer) {
        const QString device = needOutput ? piSettings->audio_output_device : QString();
        const QString input  = needInput  ? piSettings->audio_input_device  : QString();
        const int     rate   = needRate   ? piSettings->audio_sample_rate   : 0;
        const int     buffer = needBuffer ? piSettings->audio_buffer_size   : 0;
        std::cout << "[gui-audio] restore: atomic switch device='"
                  << device.toUtf8().constData()
                  << "' input='" << input.toUtf8().constData()
                  << "' rate=" << rate << " buffer=" << buffer << std::endl;
        sendDeviceSwitch(device, rate, buffer, input);
    }
}

void MainWindow::sendDeviceSwitch(QString device, int sampleRate, int bufferSize,
                                  QString inputDevice)
{
    // Normalise dropdown display strings to SuperSonic's sentinel form.
    // Display strings get persisted in v5-gui-settings.ini and read back by
    // the restore-from-settings path; if we forward them raw, JUCE rejects
    // the swap with "No such device: -- None --" and the rollback can
    // leave the engine in a half-broken state. Keep the mapping right
    // next to the wire format so any caller of sendDeviceSwitch is safe.
    if (inputDevice == tr("-- None --")
        || inputDevice == tr("-- DISABLED --")
        || inputDevice == "__disabled__") {
        inputDevice = "__none__";
    }
    std::cout << "[gui-audio] OSC sendDeviceSwitch: device='"
              << device.toUtf8().constData()
              << "' sr=" << sampleRate << " buf=" << bufferSize
              << " input='" << inputDevice.toUtf8().constData() << "'" << std::endl;
    oscpkt::Message msg("/daemon/audio/switch-device");
    msg.pushInt32(m_spAPI->GetToken());
    msg.pushStr(device.toStdString());
    msg.pushFloat(static_cast<float>(sampleRate));
    msg.pushInt32(bufferSize);
    // Fifth arg: input device (empty = leave unchanged)
    msg.pushStr(inputDevice.toStdString());
    m_spAPI->SendDaemonOSC(msg);
}

void MainWindow::switchAudioDriver(QString driver)
{
    piSettings->audio_driver = driver;
    gui_settings->setValue("prefs/audio-driver", driver);
    oscpkt::Message msg("/daemon/audio/switch-driver");
    msg.pushInt32(m_spAPI->GetToken());
    msg.pushStr(driver.toStdString());
    m_spAPI->SendDaemonOSC(msg);
}

void MainWindow::switchAudioDevice(QString device)
{
    m_pendingAudioPrefs.output = device;
    sendDeviceSwitch(device, 0, 0);
}

void MainWindow::switchAudioInputDevice(QString device)
{
    // "-- DISABLED --" carries __disabled__ as item data (from the greyed-
    // out dropdown when the Enable Inputs checkbox is off).
    if (device == "__disabled__" || device == tr("-- DISABLED --")) {
        m_pendingAudioPrefs.input = "__disabled__";
        // Disable audio inputs
        oscpkt::Message msg("/daemon/audio/switch-device");
        msg.pushInt32(m_spAPI->GetToken());
        msg.pushStr("");           // keep current output device
        msg.pushFloat(0);          // keep current sample rate
        msg.pushInt32(0);          // keep current buffer size
        msg.pushStr("__none__");   // sentinel: disable inputs
        m_spAPI->SendDaemonOSC(msg);
        return;
    }

    // "-- None --" means "no input device connected" — SuperSonic runs
    // without input but the dropdown stays active so the user can pick
    // a device later. Same command as disable, different GUI state.
    if (device == tr("-- None --")) {
        m_pendingAudioPrefs.input = "__none__";
        oscpkt::Message msg("/daemon/audio/switch-device");
        msg.pushInt32(m_spAPI->GetToken());
        msg.pushStr("");           // keep current output device
        msg.pushFloat(0);          // keep current sample rate
        msg.pushInt32(0);          // keep current buffer size
        msg.pushStr("__none__");   // sentinel: no input
        m_spAPI->SendDaemonOSC(msg);
        return;
    }

    m_pendingAudioPrefs.input = device;
    oscpkt::Message msg("/daemon/audio/switch-device");
    msg.pushInt32(m_spAPI->GetToken());
    msg.pushStr("");           // keep current output device
    msg.pushFloat(0);          // keep current sample rate
    msg.pushInt32(0);          // keep current buffer size
    msg.pushStr(device.toStdString());
    m_spAPI->SendDaemonOSC(msg);
}

void MainWindow::changeSampleRate(int rate)
{
    m_pendingAudioPrefs.sampleRate = rate;
    sendDeviceSwitch("", rate, 0);
}

void MainWindow::changeBufferSize(int size)
{
    m_pendingAudioPrefs.bufferSize = size;
    sendDeviceSwitch("", 0, size);
}

void MainWindow::onSupersonicSetup(int sampleRate, int bufferSize)
{
    m_spAPI->RequestAudioDevices();
    // Cold-swap re-attach. /supersonic/setup fires on cold swaps but
    // not initial boot (SupersonicEngine gates the emit on mWorldRebuilt);
    // first-boot attach is handled by onSpiderReady.
    m_spAPI->AudioProcessor_ResetConnection();
}

void MainWindow::onSpiderReady()
{
    honourPrefs();
    changeSystemPreAmp(piSettings->main_volume, 1);
    // First-boot scope-reader attach. /supersonic/setup covers
    // subsequent cold-swap re-attaches; the two handlers are disjoint.
    m_spAPI->AudioProcessor_ResetConnection();
}

void MainWindow::onAudioSwitchDone(const SonicPi::AudioSwitchOutcome& outcome)
{
    // Prefs are committed here — once the engine reports the switch
    // actually happened — not at request time. Persisting intent meant a
    // failed switch (e.g. a device that no longer exists) was saved
    // anyway, replayed by maybeRestoreAudioIntent on the next boot, and
    // failed identically forever.
    const PendingAudioPrefs pending = m_pendingAudioPrefs;
    m_pendingAudioPrefs = PendingAudioPrefs();

    if (outcome.success) {
        if (!pending.output.isEmpty()) {
            piSettings->audio_output_device = pending.output;
            gui_settings->setValue("prefs/audio-output-device", pending.output);
        }
        if (!pending.input.isEmpty() && !outcome.inputUnavailable) {
            piSettings->audio_input_device = pending.input;
            gui_settings->setValue("prefs/audio-input-device", pending.input);
        }
        if (pending.sampleRate > 0) {
            piSettings->audio_sample_rate = pending.sampleRate;
            gui_settings->setValue("prefs/audio-sample-rate", pending.sampleRate);
        }
        if (pending.bufferSize > 0) {
            piSettings->audio_buffer_size = pending.bufferSize;
            gui_settings->setValue("prefs/audio-buffer-size", pending.bufferSize);
        }
    }

    // Two failure shapes from the engine. Surface both as a modal
    // carrying the verbatim engine/JUCE error — no diagnosis, no
    // enrichment. Revert the affected dropdown.
    if (outcome.success && !outcome.inputUnavailable) return;  // nothing to surface

    if (!outcome.success) {
        // Drop the saved pref for whatever was requested. This also
        // self-heals stale prefs replayed by maybeRestoreAudioIntent
        // (requested* echoes the request even when nothing is pending).
        if (!outcome.requestedOutput.empty()) {
            piSettings->audio_output_device = "";
            gui_settings->setValue("prefs/audio-output-device", "");
        }
        if (!outcome.requestedInput.empty()) {
            piSettings->audio_input_device = "";
            gui_settings->setValue("prefs/audio-input-device", "");
        }

        QString device = QString::fromStdString(
            outcome.requestedOutput.empty()
                ? outcome.requestedInput
                : outcome.requestedOutput);
        QString error  = QString::fromStdString(outcome.error);
        QMessageBox::warning(
            this,
            tr("Audio device switch failed"),
            tr("Could not switch to:\n\n  %1\n\n%2").arg(device, error));
        // Engine has rolled back to whatever it was on; the next
        // /supersonic/devices push refreshes the dropdowns to match.
        return;
    }

    // success == true && inputUnavailable: output opened, input fell back.
    // Don't keep a pref that asks for the unavailable input on every boot.
    piSettings->audio_input_device = "";
    gui_settings->setValue("prefs/audio-input-device", "");

    QString inputName = QString::fromStdString(outcome.requestedInput);
    QString reason    = QString::fromStdString(outcome.inputUnavailableReason);
    QMessageBox::warning(
        this,
        tr("Audio input device unavailable"),
        tr("Could not open the audio input device:\n\n  %1\n\n%2").arg(inputName, reason));
    // Settings widget reverts the input dropdown when the next
    // /supersonic/input-devices push arrives carrying currentInput="".
}

void MainWindow::homeDirWriteError()
{
    splashClose();
    setMessageBoxStyle();

    QDialog* pDialog = new QDialog(this, Qt::Window | Qt::WindowTitleHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);

    QVBoxLayout* pLayout = new QVBoxLayout(pDialog);

    pDialog->setWindowTitle(tr("Sonic Pi - Unable to Write to Home Directory"));

    QString text;
    QTextStream str(&text);
    if (QProcessEnvironment::systemEnvironment().value("SONIC_PI_HOME") == "")
    {
        str << "<html><body>"
            << "<h1>" << tr("Boot Error - Home Dir not writable:") << "</h1>\n\n"
            << "<h2>" << sonicPiHomePath() << "</h2>\n\n"
            << "<h3><i>" << tr("Quick Fix: set the environment variable SONIC_PI_HOME to a directory you have permission to write to.") << "</i></h3>\n\n"
            << "<small><i>"
            << "<br/>"
            << "<br/>"
            << "<p>" << tr("For the curious among you, Sonic Pi automatically stores the contents of the code buffers, configuration files and logs in a folder called .sonic-pi which typically resides in your home directory.") << "</p>"
            << "<p>" << tr("Unfortunately you don't appear to have permission to write to your home directory:") << "</p><p style=\"color: dodgerblue;\">" << sonicPiHomePath() << "</p>"
            << "<p style=\"color: deeppink;\">" << tr("To fix this you can set the environment variable SONIC_PI_HOME to any directory you have write access to and Sonic Pi will place its .sonic-pi directory within that.") << "</p>"
            << "</body></html>";
    }
    else
    {
        str << "<html><body>"
            << "<h1>" << tr("Boot Error - SONIC_PI_HOME not writable:") << "</h1>\n\n"
            << "<h2>" << sonicPiHomePath() << "</h2>\n\n"
            << "<h3><i>" << tr("Quick Fix: set the environment variable SONIC_PI_HOME to a directory you have permission to write to.") << "</i></h3>\n\n"
            << "<small><i>"
            << "<br/>"
            << "<br/>"
            << "<p>" << tr("For the curious among you, Sonic Pi automatically stores the contents of the code buffers, configuration files and logs in a folder called .sonic-pi which typically resides in your home directory.") << "</p>"
            << "<p>" << tr("Unfortunately it appears you have set the SONIC_PI_HOME environment variable to a directory you don't have permission to write to:") << "</p><p style=\"color: dodgerblue;\">" << sonicPiHomePath() << "</p>"
            << "<p style=\"color: deeppink;\">" << tr("To fix this you can set the environment variable SONIC_PI_HOME to any directory you have write access to and Sonic Pi will place its .sonic-pi directory within that.") << "</p>"
            << "</body></html>";
    }

    // The text area for the message.  Allows the user to scroll/view it.
    auto pTextArea = new QTextEdit();

    auto text_hsv_value = palette().color(QPalette::WindowText).value();
    auto bg_hsv_value = palette().color(QPalette::Window).value();
    bool dark_theme_found = text_hsv_value > bg_hsv_value;
    QString styles;

    if (dark_theme_found)
    {
        styles = ScalePxInStyleSheet(readFile(":/theme/dark/doc-styles.css"));
    }
    else
    {
        styles = ScalePxInStyleSheet(readFile(":/theme/light/doc-styles.css"));
    }

    pTextArea->document()->setDefaultStyleSheet(styles);
    pTextArea->setHtml(text);
    pTextArea->setReadOnly(true);
    pTextArea->setAccessibleName(tr("Boot error details"));
    pLayout->addWidget(pTextArea);
    pTextArea->setFocus();

    QDialogButtonBox* pButtons = new QDialogButtonBox(QDialogButtonBox::Ok, this);
    // Accepting the dialog quits the app, so say so
    pButtons->button(QDialogButtonBox::Ok)->setText(tr("Quit"));
    pLayout->addWidget(pButtons);

    auto finished = [&]() {
        std::cout << "[GUI] - Aborting. Sorry about this." << std::endl;
        QApplication::exit(-1);
        exit(EXIT_FAILURE);
    };

    // When the user hits OK, quit
    connect(pButtons, &QDialogButtonBox::accepted, this, [=]() {
        finished();
    });

    // When the dialog is done, quit
    connect(pDialog, &QDialog::finished, this, [=]() {
        finished();
    });

    // Make a sensible size, but then allow resizing
    pDialog->setFixedSize(QSize(ScaleHeightForDPI(750), ScaleHeightForDPI(800)));
    pDialog->setMaximumSize(QWIDGETSIZE_MAX, QWIDGETSIZE_MAX);
    pDialog->exec();
}
