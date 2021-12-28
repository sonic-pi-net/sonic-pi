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
#include <QApplication>
#include <QBoxLayout>
#include <QDesktopServices>
#include <QDesktopWidget>
#include <QDialogButtonBox>
#include <QDockWidget>
#include <QFileDialog>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QNetworkInterface>
#include <QPlainTextEdit>
#include <QScrollBar>
#include <QShortcut>
#include <QSplashScreen>
#include <QSplitter>
#include <QStatusBar>
#include <QStyle>
#include <QTextBrowser>
#include <QWebEngineProfile>
#include <QTextStream>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>
#include <QWebEngineView>

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
#include "widgets/sonicpilog.h"

#include "utils/ruby_help.h"

#include "dpi.h"

// Operating System Specific includes
#if defined(Q_OS_WIN)
#include <QtConcurrent/QtConcurrentRun>
#elif defined(Q_OS_MAC)
#include <QtConcurrent/QtConcurrentRun>
#else
//assuming Raspberry Pi
#include <QtConcurrentRun>
#include <cmath>
#endif

#if QT_VERSION >= 0x050400
// Requires Qt5
#include <QWindow>
#endif

using namespace std::chrono;

using namespace SonicPi;

MainWindow::MainWindow(QApplication& app, QSplashScreen* splash)
{
    app.installEventFilter(this);
    app.processEvents();
    connect(&app, SIGNAL(aboutToQuit()), this, SLOT(onExitCleanup()));

    printAsciiArtLogo();

    this->piSettings = new SonicPiSettings();

    this->splash = splash;

    // API and Client
    m_spClient = std::make_shared<QtAPIClient>(this);
    m_spAPI = std::make_shared<SonicPiAPI>(m_spClient.get(), APIProtocol::UDP, LogOption::File);

    startup_error_reported = new QCheckBox;
    startup_error_reported->setChecked(false);

    hash_salt = "Secret Hash ;-)";

    updated_dark_mode_for_help = false;
    updated_dark_mode_for_prefs = false;
    loaded_workspaces = false;
    is_recording = false;
    show_rec_icon_a = false;
    restoreDocPane = false;
    focusMode = false;
    version = "4.0-beta2";
    latest_version = "";
    version_num = 0;
    latest_version_num = 0;
    QString settings_path = sonicPiConfigPath() + QDir::separator() + "gui-settings.ini";

    gui_settings = new QSettings(settings_path, QSettings::IniFormat);

    readSettings();
    initPaths();

    bool startupOK = false;

    m_spAPI->Init(rootPath().toStdString());
    guiID = QString::fromStdString(m_spAPI->GetGuid());

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
    createStatusBar();
    createInfoPane();
    setWindowTitle(tr("Sonic Pi"));

    createShortcuts();
    createToolBar();
    updateTabsVisibility();
    updateButtonVisibility();
    updateLogVisibility();
    updateCuesVisibility();

    // The implementation of this method is dynamically generated and can
    // be found in ruby_help.h:
    std::cout << "[GUI] - initialising documentation window" << std::endl;
    initDocsWindow();

    //setup autocompletion
    autocomplete->loadSamples(QString::fromStdString(m_spAPI->GetPath(SonicPiPath::SamplePath)));

    QThreadPool::globalInstance()->setMaxThreadCount(3);

    startupOK = m_spAPI->WaitUntilReady();

    if (startupOK)
    {
        // We have a connection! Finish up loading app...
        QUrl phxUrl;
        phxUrl.setUrl("http://localhost");
        phxUrl.setPort(m_spAPI->GetPort(SonicPiPortId::phx_http));
        std::cout << "[GUI] - loading up web view with URL: " << phxUrl.toString().toStdString() << std::endl;
        // load phoenix webview
        phxView->load(phxUrl);
        scopeWindow->Booted();
        std::cout << "[GUI] - honour prefs" << std::endl;
        restoreWindows();
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

        QTimer* timer = new QTimer(this);
        connect(timer, SIGNAL(timeout()), this, SLOT(heartbeatOSC()));
        timer->start(1000);
        emit settingsChanged();
        splashClose();
        focusEditor();
        showWindow();
        app.processEvents();
        std::cout << "[GUI] - boot sequence completed." << std::endl;
    }
    else
    {
        std::cout << "[GUI] - Critical Error. Unable to connect to server.." << std::endl;
        startupError("GUI was unable to connect to the Ruby server.");
    }

    toggleOSCServer(1);

    app.setActiveWindow(tabs->currentWidget());

    if (!i18n)
    {
        showLanguageLoadingError();
    }

    showWelcomeScreen();
}

void MainWindow::initPaths()
{
    QString root_path = rootPath();

    qt_app_theme_path = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/app.qss");
    qt_browser_dark_css = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/dark/doc-styles.css");
    qt_browser_light_css = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/light/doc-styles.css");
    qt_browser_hc_css = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/high_contrast/doc-styles.css");
}

void MainWindow::checkForStudioMode()
{
    // Studio mode should always be enabled on linux
#if defined(Q_OS_LINUX)
    studio_mode->setChecked(true);
    return;
#else
    // other operating systems need to support the project
    //to enable studio mode
    studio_mode->setChecked(false);
#endif

    QString queryStr;
    queryStr = QString("%1")
                   .arg(QString(QCryptographicHash::hash(QString(user_token->text() + hash_salt).toUtf8(), QCryptographicHash::Sha256).toHex()));

    QStringList studioHashList = QStringList();

    std::cout << "[GUI] - Fetching Studio hashes" << std::endl;
    QProcess* fetchStudioHashes = new QProcess();
    QStringList fetch_studio_hashes_send_args;
    fetch_studio_hashes_send_args << QString::fromStdString(m_spAPI->GetPath(SonicPiPath::FetchUrlPath)) << "http://sonic-pi.net/static/info/studio-hashes.txt";
    fetchStudioHashes->start(QString::fromStdString(m_spAPI->GetPath(SonicPiPath::RubyPath)), fetch_studio_hashes_send_args);
    fetchStudioHashes->waitForFinished();
    QTextStream stream(fetchStudioHashes->readAllStandardOutput().trimmed());
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
        st.setCodec("UTF-8");
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
    QString themeFilename = QDir::homePath() + QDir::separator() + ".sonic-pi" + QDir::separator() + "config" + QDir::separator() + "colour-theme.properties";
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
    incomingPane = new SonicPiLog;
    contextPane = new SonicPiContext;
    errorPane = new QTextBrowser;
    errorPane->setOpenExternalLinks(true);

    // Window layout
    tabs = new QTabWidget();
    tabs->setTabsClosable(false);
    tabs->setMovable(false);
    tabs->setTabPosition(QTabWidget::South);

    lexer->setAutoIndentStyle(SonicPiScintilla::AiMaintain);

    // create workspaces and add them to the tabs
    // workspace shortcuts
    signalMapper = new QSignalMapper(this);

    prefsWidget = new QDockWidget(tr("Preferences"), this);
    prefsWidget->setFocusPolicy(Qt::NoFocus);
    prefsWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    prefsWidget->setFeatures(QDockWidget::DockWidgetClosable);

    settingsWidget = new SettingsWidget(m_spAPI->GetPort(SonicPiPortId::server_osc_cues), i18n, piSettings, sonicPii18n, this);
    connect(settingsWidget, SIGNAL(restartApp()), this, SLOT(restartApp()));
    connect(settingsWidget, SIGNAL(volumeChanged(int)), this, SLOT(changeSystemPreAmp(int)));
    connect(settingsWidget, SIGNAL(mixerSettingsChanged()), this, SLOT(mixerSettingsChanged()));
    connect(settingsWidget, SIGNAL(midiSettingsChanged()), this, SLOT(toggleMidi()));
    connect(settingsWidget, SIGNAL(resetMidi()), this, SLOT(resetMidi()));
    connect(settingsWidget, SIGNAL(oscSettingsChanged()), this, SLOT(toggleOSCServer()));
    connect(settingsWidget, SIGNAL(showLineNumbersChanged()), this, SLOT(changeShowLineNumbers()));
    connect(settingsWidget, SIGNAL(showAutoCompletionChanged()), this, SLOT(changeShowAutoCompletion()));
    connect(settingsWidget, SIGNAL(showLogChanged()), this, SLOT(updateLogVisibility()));
    connect(settingsWidget, SIGNAL(showCuesChanged()), this, SLOT(updateCuesVisibility()));
    connect(settingsWidget, SIGNAL(showButtonsChanged()), this, SLOT(updateButtonVisibility()));
    connect(settingsWidget, SIGNAL(showFullscreenChanged()), this, SLOT(updateFullScreenMode()));
    connect(settingsWidget, SIGNAL(showTabsChanged()), this, SLOT(updateTabsVisibility()));
    connect(settingsWidget, SIGNAL(logAutoScrollChanged()), this, SLOT(updateLogAutoScroll()));
    connect(settingsWidget, SIGNAL(themeChanged()), this, SLOT(updateColourTheme()));
    connect(settingsWidget, SIGNAL(scopeChanged()), this, SLOT(scope()));
    connect(settingsWidget, SIGNAL(scopeChanged(QString)), this, SLOT(changeScopeKindVisibility(QString)));
    connect(settingsWidget, SIGNAL(scopeLabelsChanged()), this, SLOT(changeScopeLabels()));
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

    connect(this, SIGNAL(settingsChanged()), settingsWidget, SLOT(settingsChanged()));

    scopeWindow = new ScopeWindow(m_spClient, m_spAPI, this);

    scopeWindow->Pause();
    scopeWindow->setObjectName("scopes");

    restoreScopeState(scopeWindow->GetScopeCategories());
    settingsWidget->updateScopeNames(scopeWindow->GetScopeCategories());
    QSizePolicy prefsSizePolicy(QSizePolicy::Maximum, QSizePolicy::Maximum);
    settingsWidget->setSizePolicy(prefsSizePolicy);
    prefsWidget->setWidget(settingsWidget);

    addDockWidget(Qt::RightDockWidgetArea, prefsWidget);
    prefsWidget->hide();
    prefsWidget->setObjectName("prefs");

    connect(prefsWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(updatePrefsIcon()));
    bool auto_indent = piSettings->auto_indent_on_run;
    for (int ws = 0; ws < workspace_max; ws++)
    {
        std::string s;
        QString fileName = QString("workspace_") + QString::fromStdString(number_name(ws));

        //TODO: this is only here to ensure auto_indent_on_run is
        //      initialised before using it to construct the
        //      workspaces. Strongly consider how to clean this up in a way
        //      that nicely scales for more properties such as this.  This
        //      should only be considered an interim solution necessary to
        //      fix the return issue on Japanese keyboards.

        SonicPiScintilla* workspace = new SonicPiScintilla(lexer, theme, fileName, auto_indent);
        connect(workspace, &SonicPiScintilla::bufferNewlineAndIndent, this, [this](int point_line, int point_index, int first_line, const std::string& code, const std::string& fileName, const std::string& id) {
            m_spAPI->BufferNewLineAndIndent(point_line, point_index, first_line, code, fileName, id);
        });

        workspace->setObjectName(QString("Buffer %1").arg(ws));

        //tab completion when in list
        QShortcut* indentLine = new QShortcut(QKeySequence("Tab"), workspace);
        connect(indentLine, SIGNAL(activated()), signalMapper, SLOT(map()));
        signalMapper->setMapping(indentLine, (QObject*)workspace);

        // save and load buffers
        QShortcut* saveBufferShortcut = new QShortcut(shiftMetaKey('s'), workspace);
        connect(saveBufferShortcut, SIGNAL(activated()), this, SLOT(saveAs()));
        QShortcut* loadBufferShortcut = new QShortcut(shiftMetaKey('o'), workspace);
        connect(loadBufferShortcut, SIGNAL(activated()), this, SLOT(loadFile()));

        //transpose chars
        QShortcut* transposeChars = new QShortcut(ctrlKey('t'), workspace);
        connect(transposeChars, SIGNAL(activated()), workspace, SLOT(transposeChars()));

        //move line or selection up and down
        QShortcut* moveLineUp = new QShortcut(ctrlMetaKey('p'), workspace);
        connect(moveLineUp, SIGNAL(activated()), workspace, SLOT(moveLineOrSelectionUp()));

        QShortcut* moveLineDown = new QShortcut(ctrlMetaKey('n'), workspace);
        connect(moveLineDown, SIGNAL(activated()), workspace, SLOT(moveLineOrSelectionDown()));

        // Contextual help
        QShortcut* contextHelp = new QShortcut(ctrlKey('i'), workspace);
        connect(contextHelp, SIGNAL(activated()), this, SLOT(helpContext()));

        QShortcut* contextHelp2 = new QShortcut(QKeySequence("F1"), workspace);
        connect(contextHelp2, SIGNAL(activated()), this, SLOT(helpContext()));

        // Font zooming
        QShortcut* fontZoom = new QShortcut(metaKey('='), workspace);
        connect(fontZoom, SIGNAL(activated()), workspace, SLOT(zoomFontIn()));

        QShortcut* fontZoom2 = new QShortcut(metaKey('+'), workspace);
        connect(fontZoom2, SIGNAL(activated()), workspace, SLOT(zoomFontIn()));

        QShortcut* fontZoomOut = new QShortcut(metaKey('-'), workspace);
        connect(fontZoomOut, SIGNAL(activated()), workspace, SLOT(zoomFontOut()));

        QShortcut* fontZoomOut2 = new QShortcut(metaKey('_'), workspace);
        connect(fontZoomOut2, SIGNAL(activated()), workspace, SLOT(zoomFontOut()));

        //set Mark
#ifdef Q_OS_MAC
        QShortcut* setMark = new QShortcut(QKeySequence("Meta+Space"), workspace);
#else
        QShortcut* setMark = new QShortcut(QKeySequence("Ctrl+Space"), workspace);
#endif
        connect(setMark, SIGNAL(activated()), workspace, SLOT(setMark()));

        //escape
        QShortcut* escape = new QShortcut(ctrlKey('g'), workspace);
        QShortcut* escape2 = new QShortcut(QKeySequence("Escape"), workspace);
        connect(escape, SIGNAL(activated()), this, SLOT(escapeWorkspaces()));
        connect(escape2, SIGNAL(activated()), this, SLOT(escapeWorkspaces()));

        //quick nav by jumping up and down 1 lines at a time
        QShortcut* forwardOneLine = new QShortcut(ctrlKey('p'), workspace);
        connect(forwardOneLine, SIGNAL(activated()), workspace, SLOT(forwardOneLine()));
        QShortcut* backOneLine = new QShortcut(ctrlKey('n'), workspace);
        connect(backOneLine, SIGNAL(activated()), workspace, SLOT(backOneLine()));

        //quick nav by jumping up and down 10 lines at a time
        QShortcut* forwardTenLines = new QShortcut(shiftMetaKey('u'), workspace);
        connect(forwardTenLines, SIGNAL(activated()), workspace, SLOT(forwardTenLines()));
        QShortcut* backTenLines = new QShortcut(shiftMetaKey('d'), workspace);
        connect(backTenLines, SIGNAL(activated()), workspace, SLOT(backTenLines()));

        //cut to end of line
        QShortcut* cutToEndOfLine = new QShortcut(ctrlKey('k'), workspace);
        connect(cutToEndOfLine, SIGNAL(activated()), workspace, SLOT(cutLineFromPoint()));

        //Emacs live copy and cut
        QShortcut* copyToBuffer = new QShortcut(metaKey(']'), workspace);
        connect(copyToBuffer, SIGNAL(activated()), workspace, SLOT(copyClear()));

        QShortcut* cutToBufferLive = new QShortcut(ctrlKey(']'), workspace);
        connect(cutToBufferLive, SIGNAL(activated()), workspace, SLOT(sp_cut()));

        // Standard cut
        QShortcut* cutToBuffer = new QShortcut(ctrlKey('x'), workspace);
        connect(cutToBuffer, SIGNAL(activated()), workspace, SLOT(sp_cut()));

        // paste
        QShortcut* pasteToBufferWin = new QShortcut(ctrlKey('v'), workspace);
        connect(pasteToBufferWin, SIGNAL(activated()), workspace, SLOT(sp_paste()));
        QShortcut* pasteToBuffer = new QShortcut(metaKey('v'), workspace);
        connect(pasteToBuffer, SIGNAL(activated()), workspace, SLOT(sp_paste()));
        QShortcut* pasteToBufferEmacs = new QShortcut(ctrlKey('y'), workspace);
        connect(pasteToBufferEmacs, SIGNAL(activated()), workspace, SLOT(sp_paste()));

        //comment line
        QShortcut* toggleLineComment = new QShortcut(metaKey('/'), workspace);
        connect(toggleLineComment, SIGNAL(activated()), this, SLOT(toggleCommentInCurrentWorkspace()));

        //upcase next word
        QShortcut* upcaseWord = new QShortcut(metaKey('u'), workspace);
        connect(upcaseWord, SIGNAL(activated()), workspace, SLOT(upcaseWordOrSelection()));

        //downcase next word
        QShortcut* downcaseWord = new QShortcut(metaKey('l'), workspace);
        connect(downcaseWord, SIGNAL(activated()), workspace, SLOT(downcaseWordOrSelection()));

        QString w = QString(tr("| %1 |")).arg(QString::number(ws));
        workspaces[ws] = workspace;
        tabs->addTab(workspace, w);

        connect(workspace, SIGNAL(cursorPositionChanged(int, int)), this, SLOT(updateContext(int, int)));
    }

    connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(changeTab(int)));
    connect(signalMapper, SIGNAL(mapped(QObject*)), this, SLOT(completeSnippetListOrIndentLine(QObject*)));

    QFont font("Monospace");
    font.setStyleHint(QFont::Monospace);
    lexer->setDefaultFont(font);

    autocomplete = new ScintillaAPI(lexer);
    // adding universal shortcuts to outputpane seems to
    // steal events from doc system!?
    // addUniversalCopyShortcuts(outputPane);
#if QT_VERSION >= 0x050400
    //requires Qt 5
    new QShortcut(ctrlKey('='), this, SLOT(zoomInLogs()));
    new QShortcut(ctrlKey('-'), this, SLOT(zoomOutLogs()));

#endif
    addUniversalCopyShortcuts(errorPane);
    outputPane->setReadOnly(true);
    outputPane->setLineWrapMode(QPlainTextEdit::NoWrap);
    outputPane->setFontFamily("Hack");

    incomingPane->setReadOnly(true);
    incomingPane->setLineWrapMode(QPlainTextEdit::NoWrap);
    incomingPane->setFontFamily("Hack");

    errorPane->setReadOnly(true);

    contextPane->setReadOnly(true);
    contextPane->setLineWrapMode(QPlainTextEdit::NoWrap);
    contextPane->setFontFamily("Hack");

    if (!theme->font("LogFace").isEmpty())
    {
        outputPane->setFontFamily(theme->font("LogFace"));
        incomingPane->setFontFamily(theme->font("LogFace"));
    }

    outputPane->document()->setMaximumBlockCount(1000);
    incomingPane->document()->setMaximumBlockCount(1000);
    errorPane->document()->setMaximumBlockCount(1000);
    contextPane->document()->setMaximumBlockCount(1000);

    outputPane->setTextColor(QColor(theme->color("LogForeground")));
    outputPane->appendPlainText("\n");

    incomingPane->setTextColor(QColor(theme->color("LogForeground")));
    incomingPane->appendPlainText("\n");

    contextPane->setTextColor(QColor(theme->color("LogForeground")));
    contextPane->appendPlainText("\n");

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

    contextWidget = new QDockWidget(tr("Context"), this);
    contextWidget->setFocusPolicy(Qt::NoFocus);
    contextWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
    contextWidget->setAllowedAreas(Qt::RightDockWidgetArea);
    contextWidget->setWidget(contextPane);

    addDockWidget(Qt::RightDockWidgetArea, outputWidget);
    addDockWidget(Qt::RightDockWidgetArea, incomingWidget);
    addDockWidget(Qt::RightDockWidgetArea, contextWidget);
    outputWidget->setObjectName("output");
    incomingWidget->setObjectName("input");
    contextWidget->setObjectName("context");

    blankWidget = new QWidget();
    outputWidgetTitle = outputWidget->titleBarWidget();

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

    phxView = new QWebEngineView(this);
    phxProfile = new QWebEngineProfile(this);
    phxPage = new QWebEnginePage(phxProfile, phxView);
    phxView->setPage(phxPage);
    phxView->setContextMenuPolicy(Qt::NoContextMenu);

    docPane = new QTextBrowser;
    QSizePolicy policy = docPane->sizePolicy();
    policy.setHorizontalStretch(QSizePolicy::Maximum);
    docPane->setSizePolicy(policy);
    docPane->setMinimumHeight(100);
    docPane->setOpenLinks(false);
    docPane->setOpenExternalLinks(true);
    docPane->setStyle(new BorderlessLinksProxyStyle);
    connect(docPane, SIGNAL(anchorClicked(const QUrl&)), this, SLOT(docLinkClicked(const QUrl&)));

    QShortcut* up = new QShortcut(ctrlKey('p'), docPane);
    up->setContext(Qt::WidgetShortcut);
    connect(up, SIGNAL(activated()), this, SLOT(docScrollUp()));
    QShortcut* down = new QShortcut(ctrlKey('n'), docPane);
    down->setContext(Qt::WidgetShortcut);
    connect(down, SIGNAL(activated()), this, SLOT(docScrollDown()));

    docPane->setSource(QUrl("qrc:///html/doc.html"));

    addUniversalCopyShortcuts(docPane);

    docsplit = new QSplitter;



    docsplit->addWidget(docsNavTabs);
    docsplit->addWidget(docPane);

    southTabs = new QTabWidget;
    southTabs->setTabPosition(QTabWidget::West);
    southTabs->setTabsClosable(false);
    southTabs->setMovable(false);
    southTabs->addTab(docsplit, "Docs");
    southTabs->addTab(phxView, "PhX");

    docWidget = new QDockWidget(tr("Help"), this);
    docWidget->setFocusPolicy(Qt::NoFocus);
    docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
    docWidget->setWidget(southTabs);
    docWidget->setObjectName("help");

    addDockWidget(Qt::BottomDockWidgetArea, docWidget);
    docWidget->hide();

    //Currently causes a segfault when dragging doc pane out of main
    //window:
    connect(docWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(toggleHelpIcon()));

    mainWidgetLayout = new QVBoxLayout;
    mainWidgetLayout->addWidget(tabs);
    mainWidgetLayout->addWidget(errorPane);
    mainWidgetLayout->setMargin(0);
    mainWidget = new QWidget;
    mainWidget->setFocusPolicy(Qt::NoFocus);
    errorPane->hide();
    mainWidget->setLayout(mainWidgetLayout);
    mainWidget->setObjectName("mainWidget");
    setCentralWidget(mainWidget);
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
        msg.pushStr(guiID.toStdString());
        msg.pushStr(code.toStdString());
        if (sendOSC(msg))
        {
            statusBar()->showMessage(tr("Playing Sample..."), 1000);
        }
    }
}

void MainWindow::escapeWorkspaces()
{
    resetErrorPane();

    for (int w = 0; w < workspace_max; w++)
    {
        workspaces[w]->escapeAndCancelSelection();
        workspaces[w]->clearLineMarkers();
    }
}

void MainWindow::changeTab(int id)
{
    tabs->setCurrentIndex(id);
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

void MainWindow::updateFullScreenMode()
{
    QSignalBlocker blocker(fullScreenAct);
    fullScreenAct->setChecked(piSettings->full_screen);

    if (piSettings->full_screen)
    {
        outputWidget->setTitleBarWidget(blankWidget);
#ifdef Q_OS_WIN
        this->setWindowFlags(Qt::FramelessWindowHint);
#endif
        int currentScreen = QApplication::desktop()->screenNumber(this);
        statusBar()->showMessage(tr("Full screen mode on."), 2000);
#if QT_VERSION >= 0x050400
        //requires Qt5
        this->windowHandle()->setScreen(qApp->screens()[currentScreen]);
#endif
        this->setWindowState(Qt::WindowFullScreen);
        this->show();
    }
    else
    {
        outputWidget->setTitleBarWidget(outputWidgetTitle);
        this->setWindowState(windowState() & ~(Qt::WindowFullScreen));
#ifdef Q_OS_WIN
        this->setWindowFlags(Qt::WindowTitleHint | Qt::WindowSystemMenuHint | Qt::WindowMinimizeButtonHint | Qt::WindowMaximizeButtonHint | Qt::WindowCloseButtonHint);
#endif
        statusBar()->showMessage(tr("Full screen mode off."), 2000);
        this->show();
    }
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
    scopeWindow->Pause();

    // re-enable log text selection
    incomingPane->setTextInteractionFlags(Qt::TextSelectableByMouse);
    outputPane->setTextInteractionFlags(Qt::TextSelectableByMouse);
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
        outputWidget->close();
    }
}

void MainWindow::showCuesMenuChanged()
{
    piSettings->show_cues = showCuesAct->isChecked();
    emit settingsChanged();
    updateCuesVisibility();
}

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
        incomingWidget->close();
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

    QTabBar* tabBar = tabs->findChild<QTabBar*>();

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
    if (spws->isListActive())
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
    msg.pushStr(guiID.toStdString());
    std::string filename = ws->fileName.toStdString();
    msg.pushStr(filename);
    msg.pushStr(code);
    msg.pushInt32(start_line);
    msg.pushInt32(finish_line);
    msg.pushInt32(point_line);
    msg.pushInt32(point_index);
    sendOSC(msg);
}

void MainWindow::toggleCommentInCurrentWorkspace()
{
    SonicPiScintilla* ws = (SonicPiScintilla*)tabs->currentWidget();
    toggleComment(ws);
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
    msg.pushStr(guiID.toStdString());
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
    return QCoreApplication::applicationDirPath() + "/../../../../..";
#else
    // On linux, CMake builds app into the build folder
    return QCoreApplication::applicationDirPath() + "/../../../..";
#endif
}

void MainWindow::splashClose()
{
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

void MainWindow::oscServerEnabledMenuChanged()
{
    piSettings->osc_server_enabled = enableOSCServerAct->isChecked();
    piSettings->osc_public = enableOSCServerAct->isChecked() && allowRemoteOSCAct->isChecked();
    if (!enableOSCServerAct->isChecked()) {
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
    toggleMidi(1);
    toggleOSCServer(1);
    toggleIcons();
    scope();
    changeShowAutoCompletion();
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

    QVBoxLayout* pLayout = new QVBoxLayout(this);
    pDialog->setLayout(pLayout);

    pDialog->setWindowTitle(tr("Sonic Pi Boot Error"));

    QString text;
    QTextStream str(&text);
    str << tr("Apologies, a critical error occurred during startup:\n")
        << msg << "\n\n"
        << tr("Please consider reporting a bug at")
        << "\nhttp://github.com/samaaron/sonic-pi/issues\n"
        << "\n"
        << "Sonic Pi Boot Error Report\n"
        << "==========================\n"
        << "\n"
        << "System Information\n"
        << "------------------\n"
        << "\n"
        << "Sonic Pi version: " << version << "\n"
        << "OS: " << osDescription() << "\n"
        << "\n"
        << "Logs:\n\n"
        << QString::fromStdString(m_spAPI->GetLogs());

    // The text area for the message.  Allows the user to scroll/view it.
    auto pTextArea = new QPlainTextEdit(text);
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

void MainWindow::showLanguageLoadingError()
{
    QMessageBox msgBox(this);
    msgBox.setIcon(QMessageBox::Warning);
    msgBox.setText(QString(tr("Failed to load translations for language: %1")).arg(sonicPii18n->getNativeLanguageName(this->ui_language)));
    msgBox.setInformativeText(tr("Falling back to English. Sorry about this.") + "\n" + tr("Please consider reporting a bug at") + "\nhttp://github.com/sonic-pi-net/sonic-pi/issues");

    QPushButton* okButton = msgBox.addButton(tr("OK"), QMessageBox::AcceptRole);
    msgBox.setDefaultButton(okButton);

    msgBox.exec();
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
        msg.pushStr(guiID.toStdString());
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
        msg.pushStr(guiID.toStdString());
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
    return tabs->tabText(tabs->currentIndex());
}

bool MainWindow::loadFile()
{
    QString selfilter = QString("%1 (*.rb *.txt)").arg(tr("Buffer files"));
    QString lastDir = gui_settings->value("lastDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getOpenFileName(this, tr("Load Sonic Pi Buffer"), lastDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter);
    if (!fileName.isEmpty())
    {
        QFileInfo fi = fileName;
        gui_settings->setValue("lastDir", fi.dir().absolutePath());
        SonicPiScintilla* p = (SonicPiScintilla*)tabs->currentWidget();
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
    QString lastDir = gui_settings->value("lastDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Buffer"), lastDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter);

    if (!fileName.isEmpty())
    {
        QFileInfo fi = fileName;
        gui_settings->setValue("lastDir", fi.dir().absolutePath());
        if (!fileName.contains(QRegularExpression("\\.[a-z]+$")))
        {
            fileName = fileName + ".txt";
        }
        return saveFile(fileName, (SonicPiScintilla*)tabs->currentWidget());
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
    QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, idx));
    runCode();
}

void MainWindow::showError(QString msg)
{
    QString style_sheet = "qrc:///html/styles.css";
    if (piSettings->themeStyle == SonicPiTheme::DarkMode || piSettings->themeStyle == SonicPiTheme::DarkProMode)
    {
        style_sheet = "qrc:///html/dark_styles.css";
    }
    errorPane->clear();
    errorPane->setHtml("<html><head></head><body>" + msg + "</body></html>");
    errorPane->show();
    focusErrors();
}

void MainWindow::showBufferCapacityError()
{
    showError("<h2 class=\"syntax_error_description\"><pre>GUI Error: Buffer Full</pre></h2><pre class=\"error_msg\"> Your code buffer has reached capacity. <br/> Please remove some code before continuing. <br/><span class=\"error_line\"> For working with very large buffers use: <br/> run_file \"/path/to/buffer.rb\"</span></pre>");
}

void MainWindow::runCode()
{
    scopeWindow->Resume();

    // move log cursors to end of log files
    // and disable user input
    incomingPane->setTextInteractionFlags(Qt::NoTextInteraction);
    QTextCursor newIncomingCursor = incomingPane->textCursor();
    newIncomingCursor.movePosition(QTextCursor::End);
    incomingPane->setTextCursor(newIncomingCursor);

    outputPane->setTextInteractionFlags(Qt::NoTextInteraction);
    QTextCursor newOutputCursor = outputPane->textCursor();
    newOutputCursor.movePosition(QTextCursor::End);
    outputPane->setTextCursor(newOutputCursor);

    update();
    SonicPiScintilla* ws = (SonicPiScintilla*)tabs->currentWidget();

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

    //std::string code = ws->text().toStdString();
    Message msg("/save-and-run-buffer");
    msg.pushStr(guiID.toStdString());

    std::string filename = ((SonicPiScintilla*)tabs->currentWidget())->fileName.toStdString();
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
    SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
    ws->zoomFontIn();
}

void MainWindow::zoomCurrentWorkspaceOut()
{
    statusBar()->showMessage(tr("Zooming Out..."), 2000);
    SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
    ws->zoomFontOut();
}

void MainWindow::beautifyCode()
{
    statusBar()->showMessage(tr("Beautifying..."), 2000);
    SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
    std::string code = ws->text().toStdString();
    int line = 0;
    int index = 0;
    ws->getCursorPosition(&line, &index);
    int first_line = ws->firstVisibleLine();
    Message msg("/buffer-beautify");
    msg.pushStr(guiID.toStdString());
    std::string filename = ((SonicPiScintilla*)tabs->currentWidget())->fileName.toStdString();
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
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::check_for_updates_now()
{
    statusBar()->showMessage(tr("Checking for updates..."), 2000);
    Message msg("/check-for-updates-now");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::enableCheckUpdates()
{
    statusBar()->showMessage(tr("Enabling update checking..."), 2000);
    Message msg("/enable-update-checking");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::disableCheckUpdates()
{
    statusBar()->showMessage(tr("Disabling update checking..."), 2000);
    Message msg("/disable-update-checking");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerHpfEnable(float freq)
{
    statusBar()->showMessage(tr("Enabling Mixer HPF..."), 2000);
    Message msg("/mixer-hpf-enable");
    msg.pushStr(guiID.toStdString());
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerHpfDisable()
{
    statusBar()->showMessage(tr("Disabling Mixer HPF..."), 2000);
    Message msg("/mixer-hpf-disable");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerLpfEnable(float freq)
{
    statusBar()->showMessage(tr("Enabling Mixer LPF..."), 2000);
    Message msg("/mixer-lpf-enable");
    msg.pushStr(guiID.toStdString());
    msg.pushFloat(freq);
    sendOSC(msg);
}

void MainWindow::mixerLpfDisable()
{
    statusBar()->showMessage(tr("Disabling Mixer LPF..."), 2000);
    Message msg("/mixer-lpf-disable");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerInvertStereo()
{
    statusBar()->showMessage(tr("Enabling Inverted Stereo..."), 2000);
    Message msg("/mixer-invert-stereo");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerStandardStereo()
{
    statusBar()->showMessage(tr("Enabling Standard Stereo..."), 2000);
    Message msg("/mixer-standard-stereo");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerMonoMode()
{
    statusBar()->showMessage(tr("Mono Mode..."), 2000);
    Message msg("/mixer-mono-mode");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::mixerStereoMode()
{
    statusBar()->showMessage(tr("Stereo Mode..."), 2000);
    Message msg("/mixer-stereo-mode");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::stopCode()
{
    stopRunningSynths();
    statusBar()->showMessage(tr("Stopping..."), 2000);
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
    if (!docWidget->isVisible())
        docWidget->show();
    SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
    QString selection = ws->selectedText();
    if (selection == "")
    { // get current word instead
        int line, pos;
        ws->getCursorPosition(&line, &pos);
        QString text = ws->text(line);
        selection = ws->wordAtLineIndex(line, pos);
    }
    selection = selection.toLower();
    if (selection[0] == ':')
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
    msg.pushStr(guiID.toStdString());
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
    //scopeInterface->enableScope("Left",show_left_scope->isChecked());
}

void MainWindow::toggleRightScope()
{
    //scopeInterface->enableScope("Right",show_right_scope->isChecked());
}

void MainWindow::showScopeLabelsMenuChanged()
{
    piSettings->show_scope_labels = showScopeLabelsAct->isChecked();
    emit settingsChanged();
    changeScopeLabels();
}

void MainWindow::changeScopeLabels()
{
    QSignalBlocker blocker(showScopeLabelsAct);
    showScopeLabelsAct->setChecked(piSettings->show_scope_labels);
    scopeWindow->SetScopeLabels(piSettings->show_scope_labels);
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

    // update context pane
    contextPane->setTextColor(QColor(theme->color("LogForeground")));
    updateContextWithCurrentWs();

    // clear stylesheets
    this->setStyleSheet("");
    infoWidg->setStyleSheet("");
    mainWidget->setStyleSheet("");
    statusBar()->setStyleSheet("");
    outputPane->setStyleSheet("");
    outputWidget->setStyleSheet("");
    prefsWidget->setStyleSheet("");
    tabs->setStyleSheet("");
    //TODO inject to settings Widget
    //prefTabs->setStyleSheet("");
    docsNavTabs->setStyleSheet("");
    docWidget->setStyleSheet("");
    toolBar->setStyleSheet("");
    scopeWidget->setStyleSheet("");

    QPalette p = theme->createPalette();
    QApplication::setPalette(p);

    QString appStyling = theme->getAppStylesheet();

    this->setStyleSheet(appStyling);
    infoWidg->setStyleSheet(appStyling);

    scopeWindow->Refresh();
    scopeWidget->update();

    for (int i = 0; i < tabs->count(); i++)
    {
        SonicPiScintilla* ws = (SonicPiScintilla*)tabs->widget(i);
        ws->setFrameShape(QFrame::NoFrame);
        ws->setStyleSheet("");
        ws->setStyleSheet(appStyling);

        if (piSettings->themeStyle == SonicPiTheme::HighContrastMode)
        {
            ws->setCaretWidth(8);
        }
        else
        {
            ws->setCaretWidth(5);
        }
        ws->redraw();
    }

    scopeWindow->SetColor(theme->color("Scope"));
    scopeWindow->SetColor2(theme->color("Scope_2"));
    lexer->unhighlightAll();
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

    for (int i = 0; i < tabs->count(); i++)
    {
        SonicPiScintilla* ws = (SonicPiScintilla*)tabs->widget(i);
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

    for (int i = 0; i < tabs->count(); i++)
    {
        SonicPiScintilla* ws = (SonicPiScintilla*)tabs->widget(i);
        ws->showAutoCompletion(show);
    }

    QSignalBlocker blocker(showAutoCompletionAct);
    showAutoCompletionAct->setChecked(piSettings->show_autocompletion);
}

void MainWindow::changeShowContext()
{
    bool show = piSettings->show_context;
    if (show)
    {
        statusBar()->showMessage(tr("Show context on"), 2000);
        contextWidget->show();
    }
    else
    {
        statusBar()->showMessage(tr("Show context off"), 2000);
        contextWidget->hide();
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
        prefsWidget->hide();
        prefsAct->setChecked(false);
    }
    else
    {
        statusBar()->showMessage(tr("Showing preferences..."), 2000);
        prefsWidget->show();
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
        SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
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
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::clearOutputPanels()
{
    outputPane->clear();
    errorPane->clear();
}

QKeySequence MainWindow::ctrlKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+%1").arg(key));
#endif
}

// Cmd on Mac, Alt everywhere else
QKeySequence MainWindow::metaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("alt+%1").arg(key));
#endif
}

Qt::Modifier MainWindow::metaKeyModifier()
{
#ifdef Q_OS_MAC
    return Qt::CTRL;
#else
    return Qt::ALT;
#endif
}

QKeySequence MainWindow::shiftMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Ctrl+%1").arg(key));
#else
    return QKeySequence(QString("Shift+alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::ctrlMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Ctrl+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Ctrl+alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::ctrlShiftMetaKey(char key)
{
#ifdef Q_OS_MAC
    return QKeySequence(QString("Shift+Ctrl+Meta+%1").arg(key));
#else
    return QKeySequence(QString("Shift+Ctrl+alt+%1").arg(key));
#endif
}

QKeySequence MainWindow::ctrlShiftKey(char key)
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

QString MainWindow::tooltipStrShiftMeta(char key, QString str)
{
#ifdef Q_OS_MAC
    return QString("%1 (⇧⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (Shift-alt-%2)").arg(str).arg(key);
#endif
}

QString MainWindow::tooltipStrMeta(char key, QString str)
{
#ifdef Q_OS_MAC
    return QString("%1 (⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (alt-%2)").arg(str).arg(key);
#endif
}

void MainWindow::updateAction(QAction* action, QShortcut* sc, QString tooltip, QString desc = "")
{
    QString shortcutDesc = sc->key().toString(QKeySequence::NativeText);
    action->setToolTip(tooltip + " (" + shortcutDesc + ")");
    if (desc == "")
    {
        action->setText(action->iconText() + "\t" + shortcutDesc);
    }
    else
    {
        action->setText(desc + "\t" + shortcutDesc);
    }
    action->setStatusTip(tooltip + " (" + shortcutDesc + ")");
}

void MainWindow::createShortcuts()
{
    std::cout << "[GUI] - creating shortcuts" << std::endl;
    new QShortcut(shiftMetaKey('['), this, SLOT(tabPrev()));
    new QShortcut(shiftMetaKey(']'), this, SLOT(tabNext()));
    connect(new QShortcut(shiftMetaKey('1'), this), &QShortcut::activated, [this]() { tabGoto(1); });
    connect(new QShortcut(shiftMetaKey('2'), this), &QShortcut::activated, [this]() { tabGoto(2); });
    connect(new QShortcut(shiftMetaKey('3'), this), &QShortcut::activated, [this]() { tabGoto(3); });
    connect(new QShortcut(shiftMetaKey('4'), this), &QShortcut::activated, [this]() { tabGoto(4); });
    connect(new QShortcut(shiftMetaKey('5'), this), &QShortcut::activated, [this]() { tabGoto(5); });
    connect(new QShortcut(shiftMetaKey('6'), this), &QShortcut::activated, [this]() { tabGoto(6); });
    connect(new QShortcut(shiftMetaKey('7'), this), &QShortcut::activated, [this]() { tabGoto(7); });
    connect(new QShortcut(shiftMetaKey('8'), this), &QShortcut::activated, [this]() { tabGoto(8); });
    connect(new QShortcut(shiftMetaKey('9'), this), &QShortcut::activated, [this]() { tabGoto(9); });
    connect(new QShortcut(shiftMetaKey('0'), this), &QShortcut::activated, [this]() { tabGoto(0); });
    new QShortcut(QKeySequence("F8"), this, SLOT(reloadServerCode()));
    new QShortcut(QKeySequence("F9"), this, SLOT(toggleButtonVisibility()));
    new QShortcut(shiftMetaKey('B'), this, SLOT(toggleButtonVisibility()));
    new QShortcut(QKeySequence("F10"), this, SLOT(toggleFocusMode()));
    new QShortcut(shiftMetaKey('F'), this, SLOT(toggleFullScreenMode()));
    new QShortcut(shiftMetaKey('M'), this, SLOT(cycleThemes()));
    new QShortcut(QKeySequence("F11"), this, SLOT(toggleLogVisibility()));
    new QShortcut(shiftMetaKey('L'), this, SLOT(toggleLogVisibility()));
    new QShortcut(shiftMetaKey('C'), this, SLOT(toggleCuesVisibility()));
    new QShortcut(QKeySequence("F12"), this, SLOT(toggleScopePaused()));
}

void MainWindow::createToolBar()
{
    exitAct = new QAction(tr("Exit"), this);
    connect(exitAct, &QAction::triggered, qApp, &QApplication::closeAllWindows);

    std::cout << "[GUI] - creating tool bar" << std::endl;
    // Run
    runAct = new QAction(theme->getRunIcon(), tr("Run"), this);
    runSc = new QShortcut(metaKey('R'), this, SLOT(runCode()));
    updateAction(runAct, runSc, tr("Run the code in the current buffer"));
    connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

    // Stop
    stopAct = new QAction(theme->getStopIcon(), tr("Stop"), this);
    stopSc = new QShortcut(metaKey('S'), this, SLOT(stopCode()));
    updateAction(stopAct, stopSc, tr("Stop all running code"));
    connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

    // Record
    recAct = new QAction(theme->getRecIcon(false, false), tr("Start Recording"), this);
    recSc = new QShortcut(shiftMetaKey('R'), this, SLOT(toggleRecording()));
    updateAction(recAct, recSc, tr("Start recording to a WAV audio file"));
    connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

    // Save
    saveAsAct = new QAction(theme->getSaveAsIcon(), tr("Save"), this);
    saveAsSc = new QShortcut(shiftMetaKey('S'), this, SLOT(saveAs()));
    updateAction(saveAsAct, saveAsSc, tr("Save current buffer as an external file"));
    connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

    // Load
    loadFileAct = new QAction(theme->getLoadIcon(), tr("Load"), this);
    loadFileSc = new QShortcut(shiftMetaKey('O'), this, SLOT(loadFile()));
    updateAction(loadFileAct, loadFileSc, tr("Load an external file in the current buffer"));
    connect(loadFileAct, SIGNAL(triggered()), this, SLOT(loadFile()));

    // Align
    textAlignAct = new QAction(QIcon(":/images/align.png"), tr("Indent Code Buffer"), this);
    textAlignSc = new QShortcut(metaKey('M'), this, SLOT(beautifyCode()));
    updateAction(textAlignAct, textAlignSc, tr("Align code to improve readability"));
    connect(textAlignAct, SIGNAL(triggered()), this, SLOT(beautifyCode()));

    // Font Size Increase
    textIncAct = new QAction(theme->getTextIncIcon(), tr("Code Size Up"), this);
    textIncSc = new QShortcut(metaKey('+'), this, SLOT(zoomCurrentWorkspaceIn()));
    updateAction(textIncAct, textIncSc, tr("Increase Text Size"));
    connect(textIncAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceIn()));

    // Font Size Decrease
    textDecAct = new QAction(theme->getTextDecIcon(), tr("Code Size Down"), this);
    textDecSc = new QShortcut(metaKey('-'), this, SLOT(zoomCurrentWorkspaceOut()));
    updateAction(textDecAct, textDecSc, tr("Decrease Text Size"));
    connect(textDecAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceOut()));

    // Scope
    scopeAct = new QAction(theme->getScopeIcon(false), tr("Show Scopes"), this);
    scopeAct->setCheckable(true);
    scopeAct->setChecked(piSettings->show_scopes);
    scopeSc = new QShortcut(metaKey('O'), this, SLOT(toggleScope()));
    updateAction(scopeAct, scopeSc, tr("Toggle visibility of audio oscilloscope"));
    connect(scopeAct, SIGNAL(triggered()), this, SLOT(toggleScope()));

    // Info
    infoAct = new QAction(theme->getInfoIcon(false), tr("Show Info"), this);
    infoAct->setCheckable(true);
    infoAct->setChecked(false);
    infoSc = new QShortcut(metaKey('1'), this, SLOT(about()));
    updateAction(infoAct, infoSc, tr("Toggle information about Sonic Pi"));
    connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

    helpAct = new QAction(theme->getHelpIcon(false), tr("Show Help"), this);
    helpAct->setCheckable(true);
    helpAct->setChecked(false);
    helpSc = new QShortcut(metaKey('I'), this, SLOT(help()));
    updateAction(helpAct, helpSc, tr("Toggle the visibility of the help pane"));
    connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));

    // Preferences
    prefsAct = new QAction(theme->getPrefsIcon(false), tr("Show Preferences"), this);
    prefsAct->setCheckable(true);
    prefsAct->setChecked(false);
    prefsSc = new QShortcut(metaKey('P'), this, SLOT(togglePrefs()));
    updateAction(prefsAct, prefsSc, tr("Toggle the visibility of the preferences pane"));
    connect(prefsAct, SIGNAL(triggered()), this, SLOT(togglePrefs()));

    QWidget* spacer = new QWidget();
    spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
    toolBar = addToolBar(tr("Tools"));
    toolBar->setObjectName("toolbar");

    toolBar->addAction(runAct);
    toolBar->addAction(stopAct);
    toolBar->addAction(recAct);

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

    showContextAct = new QAction(tr("Show Code Context"), this);
    showContextAct->setCheckable(true);
    showContextAct->setChecked(piSettings->show_context);
    connect(showContextAct, SIGNAL(triggered()), this, SLOT(showContextMenuChanged()));

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
    logSynthsAct->setChecked(piSettings->log_cues);
    connect(logSynthsAct, SIGNAL(triggered()), this, SLOT(logSynthsMenuChanged()));

    clearOutputOnRunAct = new QAction(tr("Clear Logs on Run"), this);
    clearOutputOnRunAct->setCheckable(true);
    clearOutputOnRunAct->setChecked(piSettings->log_cues);
    connect(clearOutputOnRunAct, SIGNAL(triggered()), this, SLOT(clearOutputOnRunMenuChanged()));

    autoIndentOnRunAct = new QAction(tr("Auto Indent Code Buffer on Run"), this);
    autoIndentOnRunAct->setCheckable(true);
    autoIndentOnRunAct->setChecked(piSettings->auto_indent_on_run);
    connect(autoIndentOnRunAct, SIGNAL(triggered()), this, SLOT(autoIndentOnRunMenuChanged()));

    logAutoScrollAct = new QAction(tr("Auto-Scroll Log"), this);
    logAutoScrollAct->setCheckable(true);
    logAutoScrollAct->setChecked(piSettings->log_auto_scroll);
    connect(logAutoScrollAct, SIGNAL(triggered()), this, SLOT(logAutoScrollMenuChanged()));

    toolBar->addAction(scopeAct);
    toolBar->addAction(infoAct);
    toolBar->addAction(helpAct);
    toolBar->addAction(prefsAct);

    liveMenu = menuBar()->addMenu(tr("Live"));
    liveMenu->addAction(runAct);
    liveMenu->addAction(stopAct);
    liveMenu->addAction(recAct);
    liveMenu->addSeparator();
    liveMenu->addAction(logSynthsAct);
    liveMenu->addAction(logCuesAct);
    liveMenu->addAction(logAutoScrollAct);
    liveMenu->addAction(clearOutputOnRunAct);
    liveMenu->addSeparator();
    liveMenu->addAction(exitAct);

    codeMenu = menuBar()->addMenu(tr("Code"));
    codeMenu->addAction(saveAsAct);
    codeMenu->addAction(loadFileAct);
    codeMenu->addSeparator();
    codeMenu->addAction(textIncAct);
    codeMenu->addAction(textDecAct);
    codeMenu->addAction(textAlignAct);
    codeMenu->addSeparator();
    codeMenu->addAction(showLineNumbersAct);
    codeMenu->addAction(showAutoCompletionAct);
    codeMenu->addAction(autoIndentOnRunAct);

    audioMenu = menuBar()->addMenu(tr("Audio"));
    audioMenu->addAction(enableExternalSynthsAct);
    audioMenu->addAction(audioSafeAct);
    audioMenu->addAction(audioTimingGuaranteesAct);
    audioMenu->addSeparator();
    audioMenu->addAction(mixerInvertStereoAct);
    audioMenu->addAction(mixerForceMonoAct);

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

    themeMenu = displayMenu->addMenu(tr("Colour Theme"));
    themeMenu->addAction(lightThemeAct);
    themeMenu->addAction(darkThemeAct);
    themeMenu->addAction(proLightThemeAct);
    themeMenu->addAction(proDarkThemeAct);
    themeMenu->addAction(highContrastThemeAct);
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

    ioMenu = menuBar()->addMenu(tr("IO"));
    ioMenu->addAction(midiEnabledAct);
    ioMidiInMenu = ioMenu->addMenu(tr("MIDI Inputs"));
    ioMidiInMenu->addAction(tr("No Connected Inputs"));
    ioMidiOutMenu = ioMenu->addMenu(tr("MIDI Outputs"));
    ioMidiOutMenu->addAction(tr("No Connected Outputs"));

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

    ioMenu->addSeparator();
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
    incomingOSCPortMenu->addAction(QString::number(m_spAPI->GetPort(SonicPiPortId::server_osc_cues)));

    viewMenu = menuBar()->addMenu(tr("View"));

    //Accessibility shortcuts

    //Focus Editor
    focusEditorAct = new QAction(theme->getHelpIcon(false), tr("Focus Editor"), this);
    focusEditorSc = new QShortcut(ctrlShiftKey('E'), this, SLOT(focusEditor()));
    updateAction(focusEditorAct, focusEditorSc, tr("Place focus on the code editor"));
    connect(focusEditorAct, SIGNAL(triggered()), this, SLOT(focusEditor()));

    //Focus Logs
    focusLogsAct = new QAction(theme->getHelpIcon(false), tr("Focus Logs"), this);
    focusLogsSc = new QShortcut(ctrlShiftKey('L'), this, SLOT(focusLogs()));
    updateAction(focusLogsAct, focusLogsSc, tr("Place focus on the log pane"));
    connect(focusLogsAct, SIGNAL(triggered()), this, SLOT(focusLogs()));

    focusContextAct = new QAction(theme->getHelpIcon(false), tr("Focus Context"), this);
    focusContextSc = new QShortcut(ctrlShiftKey('T'), this, SLOT(focusContext()));
    updateAction(focusContextAct, focusContextSc, tr("Place focus on the context pane"));
    connect(focusContextAct, SIGNAL(triggered()), this, SLOT(focusContext()));

    //Focus Cues
    focusCuesAct = new QAction(theme->getHelpIcon(false), tr("Focus Cues"), this);
    focusCuesSc = new QShortcut(ctrlShiftKey('C'), this, SLOT(focusCues()));
    updateAction(focusCuesAct, focusCuesSc, tr("Place focus on the cue event pane"));
    connect(focusCuesAct, SIGNAL(triggered()), this, SLOT(focusCues()));

    //Focus Preferences
    focusPreferencesAct = new QAction(theme->getHelpIcon(false), tr("Focus Preferences"), this);
    focusPreferencesSc = new QShortcut(ctrlShiftKey('P'), this, SLOT(focusPreferences()));
    updateAction(focusPreferencesAct, focusPreferencesSc, tr("Place focus on preferences"));
    connect(focusPreferencesAct, SIGNAL(triggered()), this, SLOT(focusPreferences()));

    //Focus HelpListing
    focusHelpListingAct = new QAction(theme->getHelpIcon(false), tr("Focus Help Listing"), this);
    focusHelpListingSc = new QShortcut(ctrlShiftKey('H'), this, SLOT(focusHelpListing()));
    updateAction(focusHelpListingAct, focusHelpListingSc, tr("Place focus on help listing"));
    connect(focusHelpListingAct, SIGNAL(triggered()), this, SLOT(focusHelpListing()));

    //Focus HelpDetails
    focusHelpDetailsAct = new QAction(theme->getHelpIcon(false), tr("Focus Help Details"), this);
    focusHelpDetailsSc = new QShortcut(ctrlShiftKey('D'), this, SLOT(focusHelpDetails()));
    updateAction(focusHelpDetailsAct, focusHelpDetailsSc, tr("Place focus on help details"));
    connect(focusHelpDetailsAct, SIGNAL(triggered()), this, SLOT(focusHelpDetails()));

    //Focus Errors
    focusErrorsAct = new QAction(theme->getHelpIcon(false), tr("Focus Errors"), this);
    focusErrorsSc = new QShortcut(ctrlShiftKey('W'), this, SLOT(focusErrors()));
    updateAction(focusErrorsAct, focusErrorsSc, tr("Place focus on errors"));
    connect(focusErrorsAct, SIGNAL(triggered()), this, SLOT(focusErrors()));

    showLogAct = new QAction(tr("Show Log"), this);
    showLogAct->setCheckable(true);
    showLogAct->setChecked(piSettings->show_log);
    connect(showLogAct, SIGNAL(triggered()), this, SLOT(showLogMenuChanged()));

    showCuesAct = new QAction(tr("Show Cue Log"), this);
    showCuesAct->setCheckable(true);
    showCuesAct->setChecked(piSettings->show_cues);
    connect(showCuesAct, SIGNAL(triggered()), this, SLOT(showCuesMenuChanged()));

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

    viewMenu->addAction(fullScreenAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showLogAct);
    viewMenu->addAction(showCuesAct);
    viewMenu->addAction(showContextAct);
    viewMenu->addSeparator();
    viewMenu->addAction(showButtonsAct);
    viewMenu->addAction(showTabsAct);
    viewMenu->addSeparator();
    viewMenu->addAction(infoAct);
    viewMenu->addAction(helpAct);
    viewMenu->addAction(prefsAct);
    viewMenu->addSeparator();
    viewMenu->addAction(focusEditorAct);
    viewMenu->addAction(focusLogsAct);
    viewMenu->addAction(focusCuesAct);
    viewMenu->addAction(focusContextAct);
    viewMenu->addAction(focusPreferencesAct);
    viewMenu->addAction(focusHelpListingAct);
    viewMenu->addAction(focusHelpDetailsAct);
    viewMenu->addAction(focusErrorsAct);

    languageMenu = menuBar()->addMenu(tr("Language"));
    QStringList available_languages = sonicPii18n->getAvailableLanguages();

    langActionGroup = new QActionGroup(this);
#if (QT_VERSION >= QT_VERSION_CHECK(5, 14, 0))
    langActionGroup->setExclusionPolicy(QActionGroup::ExclusionPolicy::Exclusive);
#else
    langActionGroup->setExclusive(true);
#endif

    QSignalMapper* signalMapper = new QSignalMapper(this);

    for (size_t i = 0; i < available_languages.length(); i += 1)
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
    st.setCodec("UTF-8");
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
        st.setCodec("UTF-8");
        QString source = st.readAll();
        source = source.replace("100dx", QString("%1").arg(ScaleHeightForDPI(100)));
        source = source.replace("254dx", QString("%1").arg(ScaleHeightForDPI(254)));
        source = source.replace("413dx", QString("%1").arg(ScaleHeightForDPI(413)));
        source = source.replace("268dx", QString("%1").arg(ScaleHeightForDPI(268)));
        source = source.replace("328dx", QString("%1").arg(ScaleHeightForDPI(328)));
        pane->setHtml(source);
        infoTabs->addTab(pane, tabs[t]);
    }

    infoTabs->setTabPosition(QTabWidget::South);

    QBoxLayout* infoLayout = new QBoxLayout(QBoxLayout::LeftToRight);
    infoLayout->addWidget(infoTabs);

    infoWidg = new InfoWidget;
    infoWidg->setWindowIcon(QIcon(":images/icon-smaller.png"));
    infoWidg->setLayout(infoLayout);
    infoWidg->setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
    infoWidg->setWindowTitle(tr("Sonic Pi - Info"));
    infoWidg->setFixedSize(ScaleForDPI(800, 800));

    connect(infoWidg, SIGNAL(closed()), this, SLOT(about()));

    QAction* closeInfoAct = new QAction(this);
    closeInfoAct->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_W));
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
    if (is_recording)
    {
        updateAction(recAct, recSc, tr("Stop Recording"), tr("Stop Recording"));
        // recAct->setStatusTip(tr("Stop Recording"));
        // recAct->setToolTip(tr("Stop Recording"));
        // recAct->setText(tr("Stop Recording"));
        rec_flash_timer->start(500);
        Message msg("/start-recording");
        msg.pushStr(guiID.toStdString());
        sendOSC(msg);
    }
    else
    {
        rec_flash_timer->stop();
        updateAction(recAct, recSc, tr("Start Recording"), tr("Start Recording"));
        recAct->setIcon(theme->getRecIcon(is_recording, false));

        Message msg("/stop-recording");
        msg.pushStr(guiID.toStdString());
        sendOSC(msg);
        QString lastDir = gui_settings->value("lastDir", QDir::homePath() + "/Desktop").toString();
        QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), lastDir, tr("Wavefile (*.wav)"));
        if (!fileName.isEmpty())
        {
            QFileInfo fi = fileName;
            gui_settings->setValue("lastDir", fi.dir().absolutePath());
            Message msg("/save-recording");
            msg.pushStr(guiID.toStdString());
            msg.pushStr(fileName.toStdString());
            sendOSC(msg);
        }
        else
        {
            Message msg("/delete-recording");
            msg.pushStr(guiID.toStdString());
            sendOSC(msg);
        }
    }
}

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
    if (index < tabs->count())
        tabs->setCurrentIndex(index);

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

    docsplit->restoreState(gui_settings->value("docsplitState").toByteArray());
    //bool visualizer = piSettings->show_scopes;
    restoreState(gui_settings->value("windowState").toByteArray());
    //    restoreGeometry(settings.value("windowGeom").toByteArray());

    //    if (visualizer != piSettings->show_scopes) {
    //        piSettings->show_scopes = visualizer;
    //        scope();
    //    }

    resize(size);
    move(pos);
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
    piSettings->check_updates = gui_settings->value("prefs/rp/check-updates", true).toBool();
    piSettings->auto_indent_on_run = gui_settings->value("prefs/auto-indent-on-run", true).toBool();
    piSettings->gui_transparency = gui_settings->value("prefs/gui_transparency", 0).toInt();
    piSettings->show_scopes = gui_settings->value("prefs/scope/show-scopes", true).toBool();
    piSettings->show_scope_labels = gui_settings->value("prefs/scope/show-labels", false).toBool();
    piSettings->show_cues = gui_settings->value("prefs/show_cues", true).toBool();
    QString styleName = gui_settings->value("prefs/theme", "").toString();

    piSettings->themeStyle = theme->themeNameToStyle(styleName);
    piSettings->show_autocompletion = gui_settings->value("prefs/show-autocompletion", true).toBool();
    piSettings->show_context = gui_settings->value("prefs/show-context", true).toBool();

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
    gui_settings->setValue("prefs/system-vol", piSettings->main_volume);
    gui_settings->setValue("prefs/rp/check-updates", piSettings->check_updates);
    gui_settings->setValue("prefs/auto-indent-on-run", piSettings->auto_indent_on_run);
    gui_settings->setValue("prefs/gui_transparency", piSettings->gui_transparency);
    gui_settings->setValue("prefs/scope/show-labels", piSettings->show_scope_labels);
    gui_settings->setValue("prefs/scope/show-scopes", piSettings->show_scopes);
    gui_settings->setValue("prefs/show_cues", piSettings->show_cues);
    gui_settings->setValue("prefs/theme", theme->themeStyleToName(piSettings->themeStyle));

    gui_settings->setValue("prefs/show-autocompletion", piSettings->show_autocompletion);

    gui_settings->setValue("prefs/show-buttons", piSettings->show_buttons);
    gui_settings->setValue("prefs/show-tabs", piSettings->show_tabs);
    gui_settings->setValue("prefs/show-log", piSettings->show_log);
    gui_settings->setValue("prefs/show-context", piSettings->show_context);

    for (auto name : piSettings->scope_names)
    {
        gui_settings->setValue("prefs/scope/show-" + name.toLower(), piSettings->isScopeActive(name));
    }

    gui_settings->setValue("workspace", tabs->currentIndex());

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
    in.setCodec("UTF-8");
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
    out.setCodec("UTF-8");
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
    std::cout << "[GUI] - initiating Shutdown..." << std::endl;

    if (scopeWindow)
    {
        std::cout << "[GUI] - shutting down scope..." << std::endl;
        scopeWindow->ShutDown();
    }

    if (phxView)
    {
        std::cout << "[GUI] - shutting down PhX view..." << std::endl;
        phxView->deleteLater();
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
    QApplication* app = dynamic_cast<QApplication*>(parent());
    statusBar()->showMessage(tr("Restarting Sonic Pi..."), 10000);

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
    app->exit(0);
    exit(0);
}

void MainWindow::heartbeatOSC()
{
    // Message msg("/gui-heartbeat");
    // msg.pushStr(guiID.toStdString());
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

        if (helpPages[i].keyword != NULL)
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
    connect(nameList,
        SIGNAL(itemPressed(QListWidgetItem*)),
        this, SLOT(updateDocPane(QListWidgetItem*)));
    connect(nameList,
        SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
        this, SLOT(updateDocPane2(QListWidgetItem*, QListWidgetItem*)));

    QShortcut* up = new QShortcut(ctrlKey('p'), nameList);
    up->setContext(Qt::WidgetShortcut);
    connect(up, SIGNAL(activated()), this, SLOT(helpScrollUp()));
    QShortcut* down = new QShortcut(ctrlKey('n'), nameList);
    down->setContext(Qt::WidgetShortcut);
    connect(down, SIGNAL(activated()), this, SLOT(helpScrollDown()));

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
    int index = tabs->currentIndex();
    if (index == tabs->count() - 1)
        index = 0;
    else
        index++;
    QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::tabPrev()
{
    int index = tabs->currentIndex();
    if (index == 0)
        index = tabs->count() - 1;
    else
        index--;
    QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::tabGoto(int index)
{
    if (index < tabs->count())
        QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::setLineMarkerinCurrentWorkspace(int num)
{
    if (num > 0)
    {
        SonicPiScintilla* ws = (SonicPiScintilla*)tabs->currentWidget();
        ws->setLineErrorMarker(num - 1);
    }
}
//TODO remove
void MainWindow::setUpdateInfoText(QString t)
{
    //  update_info->setText(t);
}

void MainWindow::addUniversalCopyShortcuts(QTextEdit* te)
{
    new QShortcut(ctrlKey('c'), te, SLOT(copy()));
    new QShortcut(ctrlKey('a'), te, SLOT(selectAll()));

    new QShortcut(metaKey('c'), te, SLOT(copy()));
    new QShortcut(metaKey('a'), te, SLOT(selectAll()));
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
    //noquote requires QT 5.4
    qDebug() << s;
#endif
}

void MainWindow::requestVersion()
{
    Message msg("/version");
    msg.pushStr(guiID.toStdString());
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
        statusBar()->showMessage(tr("Enabling MIDI input..."), 2000);
        Message msg("/midi-start");
        msg.pushStr(guiID.toStdString());
        msg.pushInt32(silent);
        sendOSC(msg);
    }
    else
    {
        statusBar()->showMessage(tr("Disabling MIDI input..."), 2000);
        Message msg("/midi-stop");
        msg.pushStr(guiID.toStdString());
        msg.pushInt32(silent);
        sendOSC(msg);
    }
}

void MainWindow::resetMidi()
{
    if (piSettings->midi_enabled)
    {

        ioMidiOutMenu->clear();
        ioMidiOutMenu->addAction(tr("No Connected Outputs"));
        ioMidiInMenu->clear();
        ioMidiInMenu->addAction(tr("No Connected Inputs"));

        settingsWidget->updateMidiInPorts(tr("No connected input devices"));
        settingsWidget->updateMidiOutPorts(tr("No connected output devices"));
        statusBar()->showMessage(tr("Resetting MIDI..."), 2000);
        Message msg("/midi-reset");
        msg.pushStr(guiID.toStdString());
        sendOSC(msg);
    }
    else
    {
        statusBar()->showMessage(tr("MIDI is disabled..."), 2000);
    }
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
        msg.pushStr(guiID.toStdString());
        sendOSC(msg);
    }
    else
    {

        enableOSCServerAct->setChecked(false);
        statusBar()->showMessage(tr("Disabling OSC cue port..."), 2000);
        std::cout << "[GUI] - asking OSC server to stop" << std::endl;
        Message msg("/cue-port-stop");
        msg.pushStr(guiID.toStdString());
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
        msg.pushStr(guiID.toStdString());
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
        msg.pushStr(guiID.toStdString());
        sendOSC(msg);
    }
}

bool MainWindow::eventFilter(QObject* obj, QEvent* evt)
{
    if (obj == qApp && (evt->type() == QEvent::ApplicationActivate))
    {
        statusBar()->showMessage(tr("Welcome back. Now get your live code on..."), 2000);
        update();
    }

    // if (evt->type() == QEvent::KeyPress) {
    //     QKeyEvent *keyEvent = static_cast<QKeyEvent *>(evt);
    //     qDebug() << "Key Press: " << keyEvent->text() << " " << keyEvent->key();
    // }

    // if (evt->type() == QEvent::KeyRelease) {
    //     QKeyEvent *keyEvent = static_cast<QKeyEvent *>(evt);
    //     qDebug() << "Key Release: " << keyEvent->text();
    // }

    // if(evt->type() == QEvent::Shortcut){
    //     QShortcutEvent *sc = static_cast<QShortcutEvent *>(evt);
    //     const QKeySequence &ks = sc->key();
    //     qDebug() << "Key Shortcut: " << ks.toString();
    // }

    return QMainWindow::eventFilter(obj, evt);
}

QString MainWindow::sonicPiHomePath()
{
    QString path = qgetenv("SONIC_PI_HOME").constData();
    if (path.isEmpty())
    {
        return QDir::homePath() + QDir::separator() + ".sonic-pi";
    }
    else
    {
        return path;
    }
}

QString MainWindow::sonicPiConfigPath()
{
    return sonicPiHomePath() + QDir::separator() + "config";
}

void MainWindow::zoomInLogs()
{
    outputPane->zoomIn();
    incomingPane->zoomIn();
    contextPane->zoomIn();
}

void MainWindow::zoomOutLogs()
{
    outputPane->zoomOut();
    incomingPane->zoomOut();
    contextPane->zoomOut();
}

void MainWindow::updateMIDIInPorts(QString port_info)
{
    QString input_header = tr("Connected MIDI inputs") + ":\n\n";
    settingsWidget->updateMidiInPorts(input_header + port_info);
    ioMidiInMenu->clear();
    port_info = port_info.trimmed();
    if (port_info.isEmpty())
    {
        ioMidiInMenu->addAction(tr("No Connected Inputs"));
    }
    else
    {
        QStringList input_ports = port_info.split("\n");

        for (int i = 0; i < input_ports.size(); ++i)
        {
            ioMidiInMenu->addAction(input_ports.at(i));
        }
    }
}

void MainWindow::updateMIDIOutPorts(QString port_info)
{
    QString output_header = tr("Connected MIDI outputs") + ":\n\n";
    settingsWidget->updateMidiOutPorts(output_header + port_info);
    autocomplete->updateMidiOuts(port_info);
    ioMidiOutMenu->clear();
    port_info = port_info.trimmed();
    if (port_info.isEmpty())
    {
        ioMidiOutMenu->addAction(tr("No Connected Outputs"));
    }
    else
    {
        QStringList output_ports = port_info.split("\n");

        for (int i = 0; i < output_ports.size(); ++i)
        {
            ioMidiOutMenu->addAction(output_ports.at(i));
        }
    }
}

void MainWindow::focusContext()
{
    contextPane->showNormal();
    contextPane->setFocusPolicy(Qt::StrongFocus);
    contextPane->setFocus();
    contextPane->raise();
    contextPane->setVisible(true);
    contextPane->activateWindow();
}

void MainWindow::focusLogs()
{
    outputPane->showNormal();
    outputPane->setFocusPolicy(Qt::StrongFocus);
    outputPane->setFocus();
    outputPane->raise();
    outputPane->setVisible(true);
    outputPane->activateWindow();
}

void MainWindow::focusEditor()
{
    SonicPiScintilla* ws = (SonicPiScintilla*)tabs->currentWidget();
    ws->showNormal();
    ws->setFocusPolicy(Qt::StrongFocus);
    ws->setFocus();
    ws->raise();
    ws->setVisible(true);
    ws->activateWindow();
}

void MainWindow::focusCues()
{
    incomingPane->showNormal();
    incomingPane->setFocusPolicy(Qt::StrongFocus);
    incomingPane->setFocus();
    incomingPane->raise();
    incomingPane->setVisible(true);
    incomingPane->activateWindow();
}

void MainWindow::focusPreferences()
{
    prefsWidget->show();
    updatePrefsIcon();
    settingsWidget->showNormal();
    settingsWidget->setFocusPolicy(Qt::StrongFocus);
    settingsWidget->setFocus();
    settingsWidget->raise();
    settingsWidget->setVisible(true);
    settingsWidget->activateWindow();
}

void MainWindow::focusHelpListing()
{
    docWidget->show();
    updatePrefsIcon();
    docsNavTabs->showNormal();
    docsNavTabs->currentWidget()->setFocus();
    docsNavTabs->raise();
    docsNavTabs->setVisible(true);
    docsNavTabs->activateWindow();
}

void MainWindow::focusHelpDetails()
{
    docWidget->show();
    updatePrefsIcon();
    docPane->showNormal();
    docPane->setFocusPolicy(Qt::StrongFocus);
    docPane->setFocus();
    docPane->raise();
    docPane->setVisible(true);
    docPane->activateWindow();
}

void MainWindow::focusErrors()
{
    errorPane->showNormal();
    errorPane->setFocusPolicy(Qt::StrongFocus);
    errorPane->setFocus();
    errorPane->raise();
    errorPane->setVisible(true);
    errorPane->activateWindow();
}

void MainWindow::updateContextWithCurrentWs()
{
    SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
    int line, index;
    ws->getCursorPosition(&line, &index);
    updateContext(line, index);
}

void MainWindow::updateContext(int line, int index)
{
    contextPane->setContent(tr("Line: %1,  Position: %2").arg(line + 1).arg(index + 1));
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
