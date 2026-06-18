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
#include <QPlainTextEdit>
#include <QPushButton>
#include <QScrollBar>
#include <QShortcut>
#include <QSplashScreen>
#include <QTimer>
#include <QSplitter>
#include <QStatusBar>
#include <QStyle>
#include <QTextBrowser>
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
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"

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

MainWindow::MainWindow(QApplication& app, QSplashScreen* splash)
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
    updateLogVisibility();
    updateCuesVisibility();
    updateDebugLogPanelVisibility();

    // The implementation of this method is dynamically generated and can
    // be found in ruby_help.h:
    std::cout << "[GUI] - initialising documentation window" << std::endl;
    initDocsWindow();

    // setup autocompletion
    autocomplete->loadSamples(QString::fromStdString(m_spAPI->GetPath(SonicPiPath::SamplePath)));

    QThreadPool::globalInstance()->setMaxThreadCount(3);

    // Defer the blocking server wait to the live event loop.
    QTimer::singleShot(0, this, &MainWindow::completeBoot);
}

void MainWindow::completeBoot()
{
    bool startupOK = m_spAPI->WaitUntilReady();

    if (startupOK)
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
        std::cout << "[GUI] - boot sequence completed." << std::endl;
    }
    else
    {
        std::cout << "[GUI] - Critical Error. Unable to connect to server.." << std::endl;
        startupError("GUI was unable to connect to the Ruby server.");
    }

    toggleOSCServer(1);

    editorTabWidget->currentWidget()->activateWindow();

    showWelcomeScreen();

    std::cout << "[GUI] - MainWindow initialisation completed." << std::endl;
}

void MainWindow::initPaths()
{

    QString settings_path = sonicPiConfigPath() + QDir::separator() + "gui-settings.ini";
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
        statusBar()->showMessage(tr("Studio Mode Enabled. Thank-you for supporting Sonic Pi."), 5000);
        studio_mode->setChecked(true);
    }
    else
    {
        std::cout << "[GUI] - No Studio Hash Match Found" << std::endl;
        statusBar()->showMessage(tr("No Matching Studio Hash Found..."), 1000);
        studio_mode->setChecked(false);
    }
}

void MainWindow::showWelcomeScreen()
{
    if (gui_settings->value("first_time", 1).toInt() == 1)
    {
        QTextBrowser* startupPane = new QTextBrowser;
        startupPane->setFixedSize(ScaleHeightForDPI(600), ScaleHeightForDPI(650));
        startupPane->setWindowIcon(QIcon(":images/icon-smaller.png"));
        startupPane->setWindowTitle(tr("Welcome to Sonic Pi"));
        addUniversalCopyShortcuts(startupPane);
        QString styles = ScalePxInStyleSheet(readFile(":/theme/light/doc-styles.css"));
        startupPane->document()->setDefaultStyleSheet(styles);
        QFile file(":/html/startup.html");
        file.open(QFile::ReadOnly | QFile::Text);
        QTextStream st(&file);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        st.setEncoding(QStringConverter::Utf8);
#else
        st.setCodec("UTF-8");
#endif

        QString source = st.readAll();
        source = source.replace("214dx", QString("%1").arg(ScaleHeightForDPI(214)));
        source = source.replace("262dx", QString("%1").arg(ScaleHeightForDPI(262)));
        source = source.replace("50dx", QString("%1px").arg(ScaleHeightForDPI(32)));
        startupPane->setHtml(source);
        docWidget->show();
        docsNavTabs->setCurrentIndex(0);
        helpLists[0]->setCurrentRow(0);
        startupPane->show();
        startupPane->raise();
        startupPane->activateWindow();
        incomingPane->setFixedWidth(ScaleWidthForDPI(600));
        incomingPane->setFixedHeight(ScaleHeightForDPI(50));
        outputPane->verticalScrollBar()->setValue(0);
    }
}

void MainWindow::setupTheme()
{
    // Syntax highlighting

    QString themeFilename = sonicPiConfigPath() + QDir::separator() + "colour-theme.properties";

    this->theme = new SonicPiTheme(this, themeFilename, rootPath());
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
    errorPane->setAccessibleName(tr("Errors"));
    metroPane = new SonicPiMetro(m_spClient, m_spAPI, theme, this);

    connect(metroPane, SIGNAL(linkEnabled()), this, SLOT(checkEnableLinkMenu()));
    connect(metroPane, SIGNAL(linkDisabled()), this, SLOT(uncheckEnableLinkMenu()));

    errorPane->setOpenExternalLinks(true);

    // Window layout
    editorTabWidget = new QTabWidget();
    editorTabWidget->setTabsClosable(false);
    editorTabWidget->setMovable(false);
    editorTabWidget->setTabPosition(QTabWidget::South);

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
    connect(settingsWidget, SIGNAL(showFullscreenChanged()), this, SLOT(updateFullScreenMode()));
    connect(settingsWidget, SIGNAL(showTabsChanged()), this, SLOT(updateTabsVisibility()));
    connect(settingsWidget, SIGNAL(logAutoScrollChanged()), this, SLOT(updateLogAutoScroll()));
    connect(settingsWidget, SIGNAL(themeChanged()), this, SLOT(updateColourTheme()));
    connect(settingsWidget, SIGNAL(scopeChanged()), this, SLOT(scope()));
    connect(settingsWidget, SIGNAL(scopeChanged(QString)), this, SLOT(changeScopeKindVisibility(QString)));
    connect(settingsWidget, SIGNAL(scopeLabelsChanged()), this, SLOT(changeScopeLabels()));
    connect(settingsWidget, SIGNAL(titlesChanged()), this, SLOT(changeTitleVisibility()));
    connect(settingsWidget, SIGNAL(hideMenuBarInFullscreenChanged()), this, SLOT(changeMenuBarInFullscreenVisibility()));
    connect(settingsWidget, SIGNAL(transparencyChanged(int)), this, SLOT(changeGUITransparency(int)));

    connect(settingsWidget, SIGNAL(checkUpdatesChanged()), this, SLOT(update_check_updates()));
    connect(settingsWidget, SIGNAL(forceCheckUpdates()), this, SLOT(check_for_updates_now()));
    connect(settingsWidget, SIGNAL(showContextChanged()), this, SLOT(changeShowContext()));
    connect(settingsWidget, SIGNAL(checkArgsChanged()), this, SLOT(changeAudioSafeMode()));
    connect(settingsWidget, SIGNAL(synthTriggerTimingGuaranteesChanged()), this, SLOT(changeAudioTimingGuarantees()));
    connect(settingsWidget, SIGNAL(enableExternalSynthsChanged()), this, SLOT(changeEnableExternalSynths()));
    connect(settingsWidget, SIGNAL(midiDefaultChannelChanged()), this, SLOT(changeMidiDefaultChannel()));
    connect(settingsWidget, SIGNAL(logCuesChanged()), this, SLOT(changeLogCues()));
    connect(settingsWidget, SIGNAL(logSynthsChanged()), this, SLOT(changeLogSynths()));
    connect(settingsWidget, SIGNAL(clearOutputOnRunChanged()), this, SLOT(changeClearOutputOnRun()));
    connect(settingsWidget, SIGNAL(autoIndentOnRunChanged()), this, SLOT(changeAutoIndentOnRun()));
    connect(settingsWidget, SIGNAL(showDebugLogPanelChanged()), this, SLOT(updateDebugLogPanelVisibility()));

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
    prefsHidePushButton->setObjectName("prefsHideButton");
    prefsHidePushButton->setStyleSheet("#prefsHideButton { padding: 5px 18px; }");
    prefsButtonLayout->setContentsMargins(0, ScaleHeightForDPI(6), ScaleWidthForDPI(10), ScaleHeightForDPI(8));
    prefsButtonLayout->addStretch(1);
    prefsButtonLayout->addWidget(prefsHidePushButton);
    prefsLayout->addLayout(prefsButtonLayout);
    prefsWidget->setObjectName("prefs");
    prefsWidget->setLayout(prefsLayout);
    prefsWidget->setMinimumHeight(settingsWidget->height() + ScaleHeightForDPI(240));
    prefsWidget->setMinimumWidth(settingsWidget->width() + ScaleWidthForDPI(200));
    QSizePolicy prefsSizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
    prefsWidget->setSizePolicy(prefsSizePolicy);

    connect(prefsHidePushButton, &QPushButton::clicked, this, [=]() {
        togglePrefs();
    });

    bool auto_indent = piSettings->auto_indent_on_run;
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

        SonicPiScintilla* workspace = new SonicPiScintilla(lexer, theme, fileName, auto_indent);
        connect(workspace,
            &SonicPiScintilla::bufferNewlineAndIndent,
            this,
            [this](int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName) {
                m_spAPI->BufferNewLineAndIndent(point_line, point_index, first_line, code, fileName);
            });

        workspace->setObjectName(QString("Buffer %1").arg(ws));

        // tab completion when in list
        auto indentLine = new QShortcut(QKeySequence(Qt::Key_Tab), workspace);

        connect(indentLine, &QShortcut::activated, this, [this, workspace]() {
            completeSnippetListOrIndentLine(workspace);
        });

        // escape

        QString w = QString(tr("| %1 |")).arg(QString::number(ws));
        workspaces[ws] = workspace;
        workspace->setAccessibleName(tr("Code Editor Buffer %1").arg(ws));
        SonicPiEditor* editor = new SonicPiEditor(workspace, theme, this);
        editor->getContext()->setAccessibleName(tr("Run Context"));
        editorTabWidget->addTab(editor, w);

        connect(workspace, SIGNAL(cursorPositionChanged(int, int)), this, SLOT(updateContext(int, int)));
        connect(workspace, &SonicPiScintilla::docsRequested, this,
                [this](const QString& name) { showHelpForKeyword(name); });
    }

    connect(editorTabWidget, SIGNAL(currentChanged(int)), this, SLOT(focusEditor()));

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

    outputPane->setTextColor(QColor(theme->color("LogForeground")));
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
    blankWidgetScope = new QWidget();
    blankWidgetDoc = new QWidget();
    blankWidgetMetro = new QWidget();

    docsNavTabs = new QTabWidget;
    docsNavTabs->setFocusPolicy(Qt::NoFocus);
    docsNavTabs->setTabsClosable(false);
    docsNavTabs->setMovable(false);
    docsNavTabs->setTabPosition(QTabWidget::South);
    QShortcut* left = new QShortcut(Qt::Key_Left, docsNavTabs);
    left->setContext(Qt::WidgetWithChildrenShortcut);
    connect(left, SIGNAL(activated()), this, SLOT(docPrevTab()));
    QShortcut* right = new QShortcut(Qt::Key_Right, docsNavTabs);
    right->setContext(Qt::WidgetWithChildrenShortcut);
    connect(right, SIGNAL(activated()), this, SLOT(docNextTab()));
    docPane = new QTextBrowser;
    docPane->setAccessibleName(tr("Documentation"));
    QSizePolicy policy = docPane->sizePolicy();
    policy.setHorizontalStretch(QSizePolicy::Maximum);
    docPane->setSizePolicy(policy);
    docPane->setMinimumHeight(100);
    docPane->setOpenLinks(false);
    docPane->setOpenExternalLinks(true);
    docPane->setStyle(new BorderlessLinksProxyStyle);
    connect(docPane, SIGNAL(anchorClicked(const QUrl&)), this, SLOT(docLinkClicked(const QUrl&)));

    {
        // Load via QFile + setHtml (not setSource) so we can substitute the
        // version placeholder. doc.html only references absolute :/images
        // resources, so no baseUrl is needed.
        QFile doc_file(":/html/doc.html");
        doc_file.open(QFile::ReadOnly | QFile::Text);
        QString doc_src = QTextStream(&doc_file).readAll();
        doc_src = doc_src.replace("__SONIC_PI_VERSION__", SONIC_PI_VERSION);
        docPane->setHtml(doc_src);
    }

    addUniversalCopyShortcuts(docPane);

    docsplit = new QSplitter;
    docsplit->addWidget(docsNavTabs);
    docsplit->addWidget(docPane);

    southTabs = new QTabWidget;
    southTabs->setTabPosition(QTabWidget::West);
    southTabs->setTabsClosable(false);
    southTabs->setMovable(false);
    southTabs->addTab(docsplit, "Docs");
    southTabs->setAttribute(Qt::WA_StyledBackground, true);

    docWidget = new QDockWidget(tr("Help"), this);
    docWidget->setFocusPolicy(Qt::NoFocus);
    docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
    docWidget->setWidget(southTabs);
    docWidget->setObjectName("help");

    addDockWidget(Qt::BottomDockWidgetArea, docWidget);
    docWidget->hide();

    // Currently causes a segfault when dragging doc pane out of main
    // window:
    connect(docWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(toggleHelpIcon()));

    mainWidgetLayout = new QVBoxLayout;
    mainWidgetLayout->addWidget(editorTabWidget);
    mainWidgetLayout->addWidget(errorPane);
    mainWidget = new QWidget;
    mainWidget->setFocusPolicy(Qt::NoFocus);
    errorPane->hide();
    mainWidget->setLayout(mainWidgetLayout);
    mainWidget->setObjectName("mainWidget");

    setCentralWidget(mainWidget);

    incomingPane->setZoomLevel(gui_settings->value("prefs/cue-zoom", 0).toInt());
    outputPane->setZoomLevel(gui_settings->value("prefs/log-zoom", 0).toInt());
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
        docPane->setSource(url);
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
        Message msg("/run-code");
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

    for (int w = 0; w < workspace_max; w++)
    {
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
    statusBar()->showMessage(tr("Hiding pane titles..."), 2000);
    outputWidget->setTitleBarWidget(blankWidgetOutput);
    incomingWidget->setTitleBarWidget(blankWidgetIncoming);
    scopeWidget->setTitleBarWidget(blankWidgetScope);
    docWidget->setTitleBarWidget(blankWidgetDoc);
    metroWidget->setTitleBarWidget(blankWidgetMetro);
}

void MainWindow::namedTitleBars()
{
    statusBar()->showMessage(tr("Showing pane titles..."), 2000);
    outputWidget->setTitleBarWidget(0);
    incomingWidget->setTitleBarWidget(0);
    scopeWidget->setTitleBarWidget(0);
    docWidget->setTitleBarWidget(0);
    metroWidget->setTitleBarWidget(0);
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

        statusBar()->showMessage(tr("Full screen mode off."), 2000);
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
        piSettings->full_screen = true;
        piSettings->show_tabs = false;
        piSettings->show_buttons = false;
        piSettings->show_log = false;
        piSettings->show_cues = false;
    }
    else
    {
        piSettings->full_screen = false;
        piSettings->show_tabs = true;
        piSettings->show_buttons = true;
        piSettings->show_log = true;
        piSettings->show_cues = true;
    }
    emit settingsChanged();
    updateFullScreenMode();
    updateTabsVisibility();
    updateButtonVisibility();
    updateLogVisibility();
    updateCuesVisibility();
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
    Message snew("/s_new");
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
    Message nfree("/n_free");
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

void MainWindow::updateDebugLogPanelVisibility()
{
    if (piSettings->show_debug_log_panel)
    {
        if (!debugLogPanel)
        {
            QVector<LogPanel::Source> sources;
            for (const auto& src : m_spAPI->GetLogSources())
            {
                sources.append({ QString::fromStdString(src.name),
                                 QString::fromStdString(src.path.string()) });
            }
            debugLogPanel = new LogPanel(sources, this);
            debugLogPanel->applyTheme(theme->color("LogForeground"),
                                      theme->color("LogBackground"));

            // Live SuperSonic panel (metrics + OSC in/out + debug + node tree),
            // read from the engine's shared segment. Reparented into
            // debugLogPanel by addExtraTab, so it is torn down with it.
            metricsPanel = new MetricsPanel(m_spAPI, this);
            metricsPanel->applyTheme(theme);
            debugLogPanel->addExtraTab(metricsPanel, tr("SuperSonic"));

            southTabs->addTab(debugLogPanel, tr("Debug"));
            int idx = southTabs->indexOf(debugLogPanel);
            if (idx >= 0) southTabs->setCurrentIndex(idx);
        }
    }
    else if (debugLogPanel)
    {
        int idx = southTabs->indexOf(debugLogPanel);
        if (idx >= 0) southTabs->removeTab(idx);
        debugLogPanel->deleteLater();
        debugLogPanel = nullptr;
        metricsPanel = nullptr; // child of debugLogPanel — deleted with it
    }
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

    Message msg("/buffer-section-complete-snippet-or-indent-selection");
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

    Message msg("/buffer-section-toggle-comment");
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
    // Minimum visible duration so the splash doesn't flash by on fast boots.
    constexpr qint64 kMinSplashMs = 1500;
    const qint64 shownAt = splash->property("shownAtMs").toLongLong();
    const qint64 elapsed = QDateTime::currentMSecsSinceEpoch() - shownAt;
    if (shownAt > 0 && elapsed < kMinSplashMs) {
        QTimer::singleShot(kMinSplashMs - elapsed, this, [this]() {
            if (splash) splash->finish(this);
        });
        return;
    }
    splash->finish(this);
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
    Message msg("/supersonic/inputs/enable");
    msg.pushInt32(inputChannels);
    m_spAPI->SupersonicSendOSC(msg);

    if (piSettings->enable_scsynth_inputs)
    {
        statusBar()->showMessage(tr("Enabling Audio Inputs..."), 2000);
    }
    else
    {
        statusBar()->showMessage(tr("Disabling Audio Inputs..."), 2000);
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
}

void MainWindow::update_check_updates()
{
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
    pLayout->addWidget(pTextArea);

    // Add a dialog style OK button
    QDialogButtonBox* pButtons = new QDialogButtonBox(QDialogButtonBox::Ok, this);
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
        Message msg("/load-buffer");
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
        std::string code = workspaces[i]->text().toStdString();
        Message msg("/save-buffer");
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

void MainWindow::resetErrorPane()
{
    errorPane->hide();
    focusEditor();
}

void MainWindow::runBufferIdx(int idx)
{
    QMetaObject::invokeMethod(editorTabWidget, "setCurrentIndex", Q_ARG(int, idx));
    runCode();
}

void MainWindow::showError(QString msg)
{
    errorPane->clear();
    errorPane->setHtml("<html><head></head><body>" + msg + "</body></html>");
    errorPane->show();
    focusErrors();
    // Errors are the most important feedback event — announce assertively so
    // screen-reader users hear them (parallels the Run started / Stopped cues).
    announce(tr("Error: %1").arg(errorPane->toPlainText().simplified()), true);
}

void MainWindow::showBufferCapacityError()
{
    showError("<h2 class=\"syntax_error_description\"><pre>GUI Error: Buffer Full</pre></h2><pre class=\"error_msg\"> Your code buffer has reached capacity. <br/> Please remove some code before continuing. <br/><span class=\"error_line\"> For working with very large buffers use: <br/> run_file \"/path/to/buffer.rb\"</span></pre>");
}

void MainWindow::runCode()
{
    scopeWindow->Resume();
    announce(tr("Run started"));

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

    QString code = ws->text();

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
    Message msg("/save-and-run-buffer");
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

void MainWindow::beautifyCode()
{
    statusBar()->showMessage(tr("Beautifying..."), 2000);
    SonicPiScintilla* ws = getCurrentWorkspace();
    std::string code = ws->text().toStdString();
    int line = 0;
    int index = 0;
    ws->getCursorPosition(&line, &index);
    int first_line = ws->firstVisibleLine();
    Message msg("/buffer-beautify");
    msg.pushInt32(guiID);
    std::string filename = getCurrentWorkspace()->fileName.toStdString();
    msg.pushStr(filename);
    msg.pushStr(code);
    msg.pushInt32(line);
    msg.pushInt32(index);
    msg.pushInt32(first_line);
    sendOSC(msg);
}

bool MainWindow::sendOSC(Message m)
{
    return m_spAPI->SendOSC(m);
}

void MainWindow::reloadServerCode()
{
    statusBar()->showMessage(tr("Reloading..."), 2000);
    Message msg("/reload");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::check_for_updates_now()
{
    statusBar()->showMessage(tr("Checking for updates..."), 2000);
    Message msg("/check-for-updates-now");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::enableCheckUpdates()
{
    statusBar()->showMessage(tr("Enabling update checking..."), 2000);
    Message msg("/enable-update-checking");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::disableCheckUpdates()
{
    statusBar()->showMessage(tr("Disabling update checking..."), 2000);
    Message msg("/disable-update-checking");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerHpfEnable(float freq)
{
    statusBar()->showMessage(tr("Enabling Mixer HPF..."), 2000);
    Message msg("/mixer-hpf-enable");
    msg.pushInt32(guiID);
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerHpfDisable()
{
    statusBar()->showMessage(tr("Disabling Mixer HPF..."), 2000);
    Message msg("/mixer-hpf-disable");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerLpfEnable(float freq)
{
    statusBar()->showMessage(tr("Enabling Mixer LPF..."), 2000);
    Message msg("/mixer-lpf-enable");
    msg.pushInt32(guiID);
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerLpfDisable()
{
    statusBar()->showMessage(tr("Disabling Mixer LPF..."), 2000);
    Message msg("/mixer-lpf-disable");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerInvertStereo()
{
    statusBar()->showMessage(tr("Enabling Inverted Stereo..."), 2000);
    Message msg("/mixer-invert-stereo");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerStandardStereo()
{
    statusBar()->showMessage(tr("Enabling Standard Stereo..."), 2000);
    Message msg("/mixer-standard-stereo");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerMonoMode()
{
    statusBar()->showMessage(tr("Mono Mode..."), 2000);
    Message msg("/mixer-mono-mode");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::mixerStereoMode()
{
    statusBar()->showMessage(tr("Stereo Mode..."), 2000);
    Message msg("/mixer-stereo-mode");
    msg.pushInt32(guiID);
    sendOSC(msg);
}

void MainWindow::stopCode()
{
    stopRunningSynths();
    statusBar()->showMessage(tr("Stopping..."), 2000);
    announce(tr("Stopped"));
}

void MainWindow::scopeVisibilityChanged()
{
    piSettings->show_scopes = scopeWidget->isVisible();
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
        statusBar()->showMessage(tr("Hiding about window..."), 2000);
        infoWidg->hide();
        infoAct->setChecked(false);
    }
    else
    {
        statusBar()->showMessage(tr("Showing about window..."), 2000);
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
        statusBar()->showMessage(tr("Hiding help..."), 2000);
        docWidget->hide();
        helpAct->setChecked(false);
    }
    else
    {
        statusBar()->showMessage(tr("Showing help..."), 2000);
        docWidget->show();
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
    Message msg("/mixer-amp");
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

void MainWindow::cycleThemes()
{
    if (piSettings->themeStyle == SonicPiTheme::LightMode)
    {
        piSettings->themeStyle = SonicPiTheme::DarkMode;
    }
    else if (piSettings->themeStyle == SonicPiTheme::DarkMode)
    {
        piSettings->themeStyle = SonicPiTheme::LightProMode;
    }
    else if (piSettings->themeStyle == SonicPiTheme::LightProMode)
    {
        piSettings->themeStyle = SonicPiTheme::DarkProMode;
    }
    else if (piSettings->themeStyle == SonicPiTheme::DarkProMode)
    {
        piSettings->themeStyle = SonicPiTheme::HighContrastMode;
    }
    else if (piSettings->themeStyle == SonicPiTheme::HighContrastMode)
    {
        piSettings->themeStyle = SonicPiTheme::LightMode;
    }
    emit settingsChanged();
    updateColourTheme();
}

void MainWindow::colourThemeMenuChanged(int themeID)
{
    if (themeID == 2)
    {
        piSettings->themeStyle = SonicPiTheme::DarkMode;
    }
    else if (themeID == 3)
    {
        piSettings->themeStyle = SonicPiTheme::LightProMode;
    }
    else if (themeID == 4)
    {
        piSettings->themeStyle = SonicPiTheme::DarkProMode;
    }
    else if (themeID == 5)
    {
        piSettings->themeStyle = SonicPiTheme::HighContrastMode;
    }
    else
    {
        piSettings->themeStyle = SonicPiTheme::LightMode;
    }

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
        statusBar()->showMessage(tr("Log Auto Scroll on..."), 2000);
    }
    else
    {
        statusBar()->showMessage(tr("Log Auto Scroll off..."), 2000);
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
    recAct->setIcon(theme->getRecIcon(false, false));
    prefsAct->setIcon(theme->getPrefsIcon(prefsWidget->isVisible()));
    infoAct->setIcon(theme->getInfoIcon(infoWidg->isVisible()));
    scopeAct->setIcon(theme->getScopeIcon(scopeWidget->isVisible()));

    if (piSettings->themeStyle == SonicPiTheme::DarkProMode || piSettings->themeStyle == SonicPiTheme::LightProMode)
    {
        toolBar->setIconSize(ScaleForDPI(38, 38));
    }
    else
    {
        toolBar->setIconSize(ScaleForDPI(107, 38));
    }
    toolBar->setMinimumHeight(ScaleHeightForDPI(45));
}

void MainWindow::updateColourTheme()
{
    QSignalBlocker lightBlocker(lightThemeAct);
    lightThemeAct->setChecked(false);
    QSignalBlocker darkBlocker(darkThemeAct);
    darkThemeAct->setChecked(false);
    QSignalBlocker proLightBlocker(proLightThemeAct);
    proLightThemeAct->setChecked(false);
    QSignalBlocker proDarkBlocker(proDarkThemeAct);
    proDarkThemeAct->setChecked(false);
    QSignalBlocker highContrastBlocker(highContrastThemeAct);
    highContrastThemeAct->setChecked(false);

    if (piSettings->themeStyle == SonicPiTheme::LightMode)
    {
        lightThemeAct->setChecked(true);
    }
    else if (piSettings->themeStyle == SonicPiTheme::DarkMode)
    {
        darkThemeAct->setChecked(true);
    }
    else if (piSettings->themeStyle == SonicPiTheme::LightProMode)
    {
        proLightThemeAct->setChecked(true);
    }
    else if (piSettings->themeStyle == SonicPiTheme::DarkProMode)
    {
        proDarkThemeAct->setChecked(true);
    }
    else if (piSettings->themeStyle == SonicPiTheme::HighContrastMode)
    {
        highContrastThemeAct->setChecked(true);
    }

    theme->switchStyle(piSettings->themeStyle);
    statusBar()->showMessage(tr("Colour Theme: ") + theme->getName(), 2000);

    QString css = theme->getCss();
    toggleIcons();

    docPane->document()->setDefaultStyleSheet(css);
    docPane->reload();

    foreach (QTextBrowser* pane, infoPanes)
    {
        pane->document()->setDefaultStyleSheet(css);
        pane->reload();
    }

    errorPane->document()->setDefaultStyleSheet(css);

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

    scopeWindow->Refresh();
    scopeWidget->update();

    for (int i = 0; i < editorTabWidget->count(); i++)
    {
        ((SonicPiEditor*)editorTabWidget->widget(i))->updateColourTheme(appStyling, piSettings->themeStyle);
    }

    updateContextWithCurrentWs();
    scopeWindow->SetColor(theme->color("Scope"));
    scopeWindow->SetColor2(theme->color("Scope_2"));
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
        statusBar()->showMessage(tr("Auto Indent mode enabled"), 2000);
    }
    else
    {
        statusBar()->showMessage(tr("Auto Indent mode disabled"), 2000);
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
        statusBar()->showMessage(tr("Show autocompletion on"), 2000);
    }
    else
    {
        statusBar()->showMessage(tr("Show autocompletion off"), 2000);
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
        statusBar()->showMessage(tr("Show context on"), 2000);
        for (int i = 0; i < editorTabWidget->count(); i++)
        {
            ((SonicPiEditor*)editorTabWidget->widget(i))->showContext();
        }
    }
    else
    {
        statusBar()->showMessage(tr("Show context off"), 2000);
        for (int i = 0; i < editorTabWidget->count(); i++)
        {
            ((SonicPiEditor*)editorTabWidget->widget(i))->hideContext();
        }
    }

    QSignalBlocker blocker(showContextAct);
    showContextAct->setChecked(piSettings->show_context);
}

void MainWindow::togglePrefs()
{
    QSignalBlocker blocker(prefsAct);
    if (prefsWidget->isVisible())
    {
        statusBar()->showMessage(tr("Hiding preferences..."), 2000);
        slidePrefsWidgetOut();
        prefsAct->setChecked(false);
    }
    else
    {
        statusBar()->showMessage(tr("Showing preferences..."), 2000);

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
    Message msg("/stop-all-jobs");
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
    QString shortcutDesc = action->shortcut().toString(QKeySequence::PortableText);
    action->setToolTip(desc + " (" + shortcutDesc + ")");
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
    { "Run", QT_TR_NOOP("Run the code in the current buffer"), "Meta+R", "Meta+R", "Meta+R", "Live", &MainWindow::runAct },
    { "Stop", QT_TR_NOOP("Stop all running code"), "Meta+S", "Meta+S", "Meta+S", "Live", &MainWindow::stopAct },
    { "Record", QT_TR_NOOP("Start recording to a WAV audio file"), "ShiftMeta+R", "ShiftMeta+R", "ShiftMeta+R", "Live", &MainWindow::recAct },
    { "Save", QT_TR_NOOP("Save current buffer as an external file"), "ShiftMeta+S", "CtrlShift+S", "ShiftMeta+S", "Live", &MainWindow::saveAsAct },
    { "Load", QT_TR_NOOP("Load an external file in the current buffer"), "Ctrl+O", "Ctrl+O", "ShiftMeta+O", "Live", &MainWindow::loadFileAct },
    { "Align", QT_TR_NOOP("Align code to improve readability"), "Meta+M", "Meta+M", "Meta+M", "Code", &MainWindow::textAlignAct },
    { "Comment", QT_TR_NOOP("Comment/Uncomment code"), "Meta+/", "Meta+/", "Meta+/", "Code", &MainWindow::textCommentAct },
    { "Transpose", QT_TR_NOOP("Transpose Characters"), "Ctrl+T", "Ctrl+T", "Ctrl+T", "Code", &MainWindow::textTransposeAct },
    { "ShiftUp", QT_TR_NOOP("Shift Line or Selection Up"), "Alt+Up", "CtrlMeta+P", "CtrlMeta+P", "Code", &MainWindow::textShiftLineUpAct },
    { "ShiftDown", QT_TR_NOOP("Shift Line or Selection Down"), "Alt+Down", "CtrlMeta+N", "CtrlMeta+N", "Code", &MainWindow::textShiftLineDownAct },
    { "ContextualDocs", QT_TR_NOOP("Look up documentation for the current word"), "Shift+F1", "Shift+F1", "Ctrl+I", "Focus", &MainWindow::contextHelpAct },
    { "TextZoomIn", QT_TR_NOOP("Increase Text Size"), "Meta+=", "Ctrl++", "Meta+=", "View", &MainWindow::textIncAct },
    { "TextZoomOut", QT_TR_NOOP("Decrease Text Size"), "Meta+-", "Ctrl+-", "Meta+-", "View", &MainWindow::textDecAct },
    { "Scope", QT_TR_NOOP("Toggle visibility of audio oscilloscope"), "Meta+O", "Meta+O", "Meta+O", "Visuals", &MainWindow::scopeAct },
    { "CycleThemes", QT_TR_NOOP("Cycle through the available colour themes"), "ShiftMeta+M", "ShiftMeta+M", "ShiftMeta+M", "Visuals", &MainWindow::cycleThemesAct },
    { "Info", QT_TR_NOOP("Toggle information about Sonic Pi"), "Meta+n", "Meta+1", "Meta+1", "View", &MainWindow::infoAct },
    { "Help", QT_TR_NOOP("Toggle the visibility of the help pane"), "F1", "F1", "Meta+i", "View", &MainWindow::helpAct },
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
    { "SetMark", QT_TR_NOOP("Set a mark in the text"), "Ctrl+Space", "Ctrl+Space", "Ctrl+Space", "Code", &MainWindow::textSetMarkAct },
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
    { "Right", QT_TR_NOOP("Move Cursor Right"), "Ctrl+f", "Ctrl+f", "Ctrl+f", "Code", &MainWindow::textRightAct },
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
    { "DeleteWordRight", QT_TR_NOOP("Delete word to the right"), "Alt+Shift+Backspace", "Meta+d", "Meta+d", "Code", &MainWindow::textDeleteWordRightAct },
    { "DeleteWordLeft", QT_TR_NOOP("Delete word to the left"), "Alt+Backspace", "Meta+Backspace", "Meta+Backspace", "Code", &MainWindow::textDeleteWordLeftAct },
    { "UpcaseWord", QT_TR_NOOP("Uppercase word or selection"), "Meta+u", "Meta+u", "Meta+u", "Code", &MainWindow::textUpcaseWordAct },
    { "DowncaseWord", QT_TR_NOOP("Lowercase word or selection"), "Meta+l", "Meta+l", "Meta+l", "Code", &MainWindow::textDowncaseWordAct },
    { "FullScreen", QT_TR_NOOP("Toggle fullscreen mode"), "ShiftMeta+f", "F11", "ShiftMeta+f", "View", &MainWindow::fullScreenAct },
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
    for (const ShortcutDef& d : shortcutDefs())
    {
        loadUserShortcut(d.id, *shortcut_settings);
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

    for (const ShortcutDef& d : shortcutDefs())
    {
        if (QAction* act = this->*(d.act))
        {
            updateShortcut(d.id, act, tr(d.desc));
        }
    }
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
    saveAsAct = new QAction(theme->getSaveAsIcon(), tr("Save"), this);
    connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

    // Load
    loadFileAct = new QAction(theme->getLoadIcon(), tr("Load"), this);
    connect(loadFileAct, SIGNAL(triggered()), this, SLOT(loadFile()));

    // Align
    textAlignAct = new QAction(QIcon(":/images/align.png"), tr("Indent Code Buffer"), this);
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
    toolBar->addAction(saveAsAct);
    toolBar->addAction(loadFileAct);

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

    proLightThemeAct = new QAction(tr("Pro Light"));
    proLightThemeAct->setCheckable(true);
    proLightThemeAct->setChecked(false);
    connect(proLightThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(3); });

    proDarkThemeAct = new QAction(tr("Pro Dark"));
    proDarkThemeAct->setCheckable(true);
    proDarkThemeAct->setChecked(false);
    connect(proDarkThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(4); });

    highContrastThemeAct = new QAction(tr("High Contrast"));
    highContrastThemeAct->setCheckable(true);
    highContrastThemeAct->setChecked(false);
    connect(highContrastThemeAct, &QAction::triggered, [this]() { colourThemeMenuChanged(5); });

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
    themeMenu->addAction(proLightThemeAct);
    themeMenu->addAction(proDarkThemeAct);
    themeMenu->addAction(highContrastThemeAct);
    displayMenu->addAction(cycleThemesAct);
    displayMenu->addSeparator();

    displayMenu->addAction(scopeAct);
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
    viewMenu->addAction(showTabsAct);
    viewMenu->addAction(showTitlesAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showLineNumbersAct);
    viewMenu->addAction(showAutoCompletionAct);
    viewMenu->addAction(showCompletionHelpAct);
    viewMenu->addAction(autoIndentOnRunAct);
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

    // for debugging purposes
    reloadServerCodeSc = new QShortcut(QKeySequence("F8"), this, SLOT(reloadServerCode()));
    toggleFocusModeSc = new QShortcut(QKeySequence("F10"), this, SLOT(toggleFocusMode()));
    toggleScopePausedSc = new QShortcut(QKeySequence("F12"), this, SLOT(toggleScopePaused()));

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

void MainWindow::createInfoPane()
{
    std::cout << "[GUI] - creating info panel" << std::endl;
    QTabWidget* infoTabs = new QTabWidget(this);

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
        infoPanes.append(pane);
        addUniversalCopyShortcuts(pane);
        pane->setOpenExternalLinks(true);

        QFile file(urls[t]);
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
        pane->setHtml(source);
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
    infoWidg->setFixedSize(ScaleForDPI(800, 800));

    connect(infoWidg, SIGNAL(closed()), this, SLOT(about()));

    QAction* closeInfoAct = new QAction(this);
    closeInfoAct->setShortcut(QKeySequence(Qt::CTRL | Qt::Key_W));
    connect(closeInfoAct, SIGNAL(triggered()), this, SLOT(about()));
    infoWidg->addAction(closeInfoAct);
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
        Message msg("/start-recording");
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
        Message msg("/stop-recording");
        msg.pushInt32(guiID);
        sendOSC(msg);
        QString lastAudioDir = gui_settings->value("lastAudioDir", QDir::homePath() + "/Desktop").toString();
        QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), lastAudioDir, tr("Wavefile (*.wav)"));
        if (!fileName.isEmpty())
        {
            gui_settings->setValue("lastAudioDir", QDir(fileName).absolutePath());
            Message msg("/save-recording");
            msg.pushInt32(guiID);
            msg.pushStr(fileName.toStdString());
            sendOSC(msg);
        }
        else
        {
            Message msg("/delete-recording");
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
    piSettings->show_debug_log_panel = gui_settings->value("prefs/show-debug-log-panel", false).toBool();
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

    piSettings->themeStyle = theme->themeNameToStyle(styleName);
    piSettings->show_autocompletion = gui_settings->value("prefs/show-autocompletion", true).toBool();
    piSettings->show_completion_help = gui_settings->value("prefs/show-completion-help", true).toBool();
    piSettings->show_context = gui_settings->value("prefs/show-context", true).toBool();
#if defined(Q_OS_WIN)
    int os_shortcut_mode = 2;
#elif defined(Q_OS_MAC)
    int os_shortcut_mode = 3;
#else
    int os_shortcut_mode = 1;
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
    gui_settings->setValue("prefs/show-debug-log-panel", piSettings->show_debug_log_panel);
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
    gui_settings->setValue("prefs/theme", theme->themeStyleToName(piSettings->themeStyle));

    gui_settings->setValue("prefs/show-autocompletion", piSettings->show_autocompletion);
    gui_settings->setValue("prefs/show-completion-help", piSettings->show_completion_help);

    gui_settings->setValue("prefs/show-buttons", piSettings->show_buttons);
    gui_settings->setValue("prefs/show-tabs", piSettings->show_tabs);
    gui_settings->setValue("prefs/show-log", piSettings->show_log);
    gui_settings->setValue("prefs/show-context", piSettings->show_context);
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

    QApplication::setOverrideCursor(Qt::WaitCursor);
    text->setText(in.readAll());
    file.close();
    QApplication::restoreOverrideCursor();
    statusBar()->showMessage(tr("File loaded..."), 2000);
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

    QApplication::setOverrideCursor(Qt::WaitCursor);
    QString code = text->text();
#if defined(Q_OS_WIN)
    code.replace("\n", "\r\n"); // CRLF for Windows users
    code.replace("\r\r\n", "\r\n"); // don't double-replace if already encoded
#endif
    out << code;
    out.flush();
    file.close();
    QApplication::restoreOverrideCursor();

    statusBar()->showMessage(tr("File saved..."), 2000);
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
    statusBar()->showMessage(tr("Restarting Sonic Pi..."), 10000);

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
    QString url = cur->data(32).toString();
    docPane->setSource(QUrl(url));
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
        nameList->addItem(item);
        entry.entryIndex = nameList->count() - 1;

        if (helpPages[i].keyword != "")
        {
            helpKeywords.insert(helpPages[i].keyword, entry);
            // magic numbers ahoy
            // to be revamped along with the help system
            switch (entry.pageIndex)
            {
            case 2:
                autocomplete->addSymbol(ScintillaAPI::Synth, helpPages[i].keyword);
                break;
            case 3:
                autocomplete->addSymbol(ScintillaAPI::FX, helpPages[i].keyword);
                break;
            case 5:
                autocomplete->addKeyword(ScintillaAPI::Func, helpPages[i].keyword);
                break;
            }
        }
    }
}

QListWidget* MainWindow::createHelpTab(QString name)
{
    QListWidget* nameList = new QListWidget;
    nameList->setAccessibleName(tr("Help Topics"));
    connect(nameList,
        SIGNAL(itemPressed(QListWidgetItem*)),
        this, SLOT(updateDocPane(QListWidgetItem*)));
    connect(nameList,
        SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
        this, SLOT(updateDocPane2(QListWidgetItem*, QListWidgetItem*)));

    QBoxLayout* layout = new QBoxLayout(QBoxLayout::LeftToRight);
    layout->addWidget(nameList);
    layout->setStretch(1, 1);
    QWidget* tabWidget = new QWidget;
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
    docPane->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepSub);
}

void MainWindow::docScrollDown()
{
    docPane->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepAdd);
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

void MainWindow::setLineMarkerinCurrentWorkspace(int num)
{
    if (num > 0)
    {
        SonicPiScintilla* ws = getCurrentWorkspace();
        ws->setLineErrorMarker(num - 1);
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
    Message msg("/version");
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
        statusBar()->showMessage(tr("Enabling MIDI <input>..."), 2000);
        Message msg("/midi-start");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
    else
    {
        statusBar()->showMessage(tr("Disabling MIDI input..."), 2000);
        Message msg("/midi-stop");
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
        statusBar()->showMessage(tr("Enabling gamepad input..."), 2000);
        Message msg("/gamepad-start");
        msg.pushInt32(guiID);
        msg.pushInt32(silent);
        sendOSC(msg);
    }
    else
    {
        statusBar()->showMessage(tr("Disabling gamepad input..."), 2000);
        Message msg("/gamepad-stop");
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
    Message msg("/midi-port-enable");
    msg.pushInt32(guiID);
    msg.pushStr(direction.toStdString());
    msg.pushStr(name.toStdString());
    msg.pushInt32(enabled ? 1 : 0);
    sendOSC(msg);
}

void MainWindow::setGamepadDeviceEnabled(QString name, bool enabled)
{
    Message msg("/gamepad-enable");
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
        Message msg("/cue-port-start");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
    else
    {

        enableOSCServerAct->setChecked(false);
        statusBar()->showMessage(tr("Disabling OSC cue port..."), 2000);
        std::cout << "[GUI] - asking OSC server to stop" << std::endl;
        Message msg("/cue-port-stop");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }

    QSignalBlocker blocker2(allowRemoteOSCAct);
    if (piSettings->osc_public)
    {
        allowRemoteOSCAct->setChecked(true);

        if (piSettings->osc_server_enabled)
        {
            statusBar()->showMessage(tr("Enabling external OSC cue port..."), 2000);
        }

        std::cout << "[GUI] - cue port in external mode" << std::endl;
        Message msg("/cue-port-external");
        msg.pushInt32(guiID);
        sendOSC(msg);
    }
    else
    {
        allowRemoteOSCAct->setChecked(false);

        if (piSettings->osc_server_enabled)
        {
            statusBar()->showMessage(tr("Enabling internal OSC cue port..."), 2000);
        }
        std::cout << "[GUI] - cue port in internal mode" << std::endl;
        Message msg("/cue-port-internal");
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

    if (event->type() == QEvent::Shortcut)
    {
        QShortcutEvent* sc = static_cast<QShortcutEvent*>(event);
        const QKeySequence& ks = sc->key();
        if (ks == QKeySequence("Escape"))
        {
            escapeWorkspaces();
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
    return sonicPiConfigPath() + QDir::separator() + "keyboard-shortcuts.ini";
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

void MainWindow::announce(const QString& message, bool assertive)
{
    // No-op unless a screen reader is connected.
    if (message.isEmpty() || !QAccessible::isActive())
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
    focusPane(docPane);
}

void MainWindow::focusErrors()
{
    focusPane(errorPane);
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

    QPropertyAnimation* anim = new QPropertyAnimation(prefsWidget, "pos", prefsWidget);
    anim->setDuration(180);
    anim->setEasingCurve(QEasingCurve::InCubic);
    anim->setStartValue(prefsWidget->pos());
    anim->setEndValue(QPoint(full_width, h));
    connect(anim, &QPropertyAnimation::finished, prefsWidget, &QWidget::hide);
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

    // Don't re-push mixer settings here — Spider's cold_swap_reinit!
    // does that in Phase 4 once the new mixer node exists
    maybeRestoreAudioIntent();
}

void MainWindow::maybeRestoreAudioIntent()
{
    // Wait for SuperSonic's full initial state before diffing against intent
    if (m_audioIntentRestored) return;
    if (!m_audioDevicesSeen || !m_audioInputDevicesSeen || !m_audioDeviceConfigSeen) return;
    m_audioIntentRestored = true;

    // Driver first — a switch cascades a device re-open, so leave
    // device/rate/buffer for the next broadcast to settle
    const QString currentDriver = QString::fromStdString(m_lastAudioDeviceConfig.currentDriver);
    if (!piSettings->audio_driver.isEmpty() && piSettings->audio_driver != currentDriver) {
        std::cout << "[gui-audio] restore: driver '"
                  << currentDriver.toUtf8().constData() << "' -> '"
                  << piSettings->audio_driver.toUtf8().constData() << "'" << std::endl;
        switchAudioDriver(piSettings->audio_driver);
        return;
    }

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
    // Display strings get persisted in gui-settings.ini and read back by
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
    Message msg("/daemon/audio/switch-device");
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
    Message msg("/daemon/audio/switch-driver");
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
        Message msg("/daemon/audio/switch-device");
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
        Message msg("/daemon/audio/switch-device");
        msg.pushInt32(m_spAPI->GetToken());
        msg.pushStr("");           // keep current output device
        msg.pushFloat(0);          // keep current sample rate
        msg.pushInt32(0);          // keep current buffer size
        msg.pushStr("__none__");   // sentinel: no input
        m_spAPI->SendDaemonOSC(msg);
        return;
    }

    m_pendingAudioPrefs.input = device;
    Message msg("/daemon/audio/switch-device");
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
    pLayout->addWidget(pTextArea);

    // Add a dialog style OK button
    QDialogButtonBox* pButtons = new QDialogButtonBox(QDialogButtonBox::Ok, this);
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
