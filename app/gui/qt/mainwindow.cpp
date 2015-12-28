//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++


// Standard stuff
#include <iostream>
#include <math.h>
#include <sstream>
#include <fstream>

// Qt stuff
#include <QDate>
#include <QDesktopServices>
#include <QDir>
#include <QAction>
#include <QApplication>
#include <QCloseEvent>
#include <QFile>
#include <QFileInfo>
#include <QFileDialog>
#include <QIcon>
#include <QMenu>
#include <QMenuBar>
#include <QMessageBox>
#include <QDockWidget>
#include <QPoint>
#include <QSettings>
#include <QSize>
#include <QStatusBar>
#include <QTextEdit>
#include <QTextBrowser>
#include <QToolBar>
#include <QProcess>
#include <QFont>
#include <QTabWidget>
#include <QString>
#include <QStringList>
#include <QTextStream>
#include <QSplashScreen>
#include <QPixmap>
#include <QLabel>
#include <QSlider>
#include <QPushButton>
#include <QGridLayout>
#include <QGroupBox>
#include <QRadioButton>
#include <QCheckBox>
#include <QScrollArea>
#include <QShortcut>
#include <QToolButton>
#include <QSettings>
#include <QScrollBar>
#include <QSignalMapper>
#include <QSplitter>

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>

#include "sonicpilexer.h"
#include "sonicpiapis.h"
#include "sonicpiscintilla.h"
#include "sonicpitheme.h"

#include "oschandler.h"
#include "sonicpilog.h"
#include "sonicpiudpserver.h"
#include "sonicpitcpserver.h"

// OSC stuff
#include "oscpkt.hh"
#include "udp.hh"
using namespace oscpkt;

// OS specific stuff
#if defined(Q_OS_WIN)
  #include <QtConcurrent/QtConcurrentRun>
  void sleep(int x) { Sleep((x)*1000); }
#elif defined(Q_OS_MAC)
  #include <QtConcurrent/QtConcurrentRun>
#else
//assuming Raspberry Pi
  #include <cmath>
  #include <QtConcurrentRun>
#endif

#include "mainwindow.h"

#ifdef Q_OS_MAC
MainWindow::MainWindow(QApplication &app, bool i18n, QMainWindow* splash)
#else
MainWindow::MainWindow(QApplication &app, bool i18n, QSplashScreen* splash)
#endif
{
  QThreadPool::globalInstance()->setMaxThreadCount(3);
  app.installEventFilter(this);
  app.processEvents();

  setupLogPathAndRedirectStdOut();
  std::cout << "\n\n\n";

  guiID = QUuid::createUuid().toString();
  loaded_workspaces = false;
  this->splash = splash;
  protocol = UDP;

  if(protocol == TCP){
    clientSock = new QTcpSocket(this);
  }

  this->i18n = i18n;

  printAsciiArtLogo();

  startServer();

  setUnifiedTitleAndToolBarOnMac(true);
  setWindowIcon(QIcon(":images/icon-smaller.png"));

  defaultTextBrowserStyle = "QTextBrowser { selection-color: white; selection-background-color: deeppink; padding-left:10; padding-top:10; padding-bottom:10; padding-right:10 ; background:white;}";

  is_recording = false;
  show_rec_icon_a = false;

  rec_flash_timer = new QTimer(this);
  connect(rec_flash_timer, SIGNAL(timeout()), this, SLOT(toggleRecordingOnIcon()));

  // Setup output and error panes
  version = "";
  latest_version = "";
  version_num = 0;
  latest_version_num = 0;
  outputPane = new SonicPiLog;
  errorPane = new QTextBrowser;
  errorPane->setOpenExternalLinks(true);

  update_info = new QLabel(tr("Sonic Pi update info"));
  update_info->setWordWrap(true);

  // Syntax highlighting
  QSettings settings("uk.ac.cam.cl", "Sonic Pi");
  QString themeFilename = QDir::homePath() + QDir::separator() + ".sonic-pi" + QDir::separator() + "theme.properties";
  QFile themeFile(themeFilename);
  SonicPiTheme *theme;
  if(themeFile.exists()){
    std::cout << "[GUI] - using custom editor colours" << std::endl;
    QSettings settings(themeFilename, QSettings::IniFormat);
    theme = new SonicPiTheme(this, &settings, settings.value("prefs/dark-mode").toBool());
    lexer = new SonicPiLexer(theme);
  }
  else{
    std::cout << "[GUI] - using default editor colours" << std::endl;
    theme = new SonicPiTheme(this, 0, settings.value("prefs/dark-mode").toBool());
    lexer = new SonicPiLexer(theme);
  }




  QTimer *timer = new QTimer(this);
  connect(timer, SIGNAL(timeout()), this, SLOT(heartbeatOSC()));
  timer->start(1000);

  OscHandler* handler = new OscHandler(this, outputPane, errorPane, theme);

  if(protocol == UDP){
    sonicPiServer = new SonicPiUDPServer(this, handler);
    osc_thread = QtConcurrent::run(sonicPiServer, &SonicPiServer::startServer);
  }
  else{
    sonicPiServer = new SonicPiTCPServer(this, handler);
    sonicPiServer->startServer();
  }

  // Window layout
  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);

  lexer->setAutoIndentStyle(SonicPiScintilla::AiMaintain);

  // create workspaces and add them to the tabs
  // workspace shortcuts
  signalMapper = new QSignalMapper (this) ;
  for(int ws = 0; ws < workspace_max; ws++) {
    std::string s;


    SonicPiScintilla *workspace = new SonicPiScintilla(lexer, theme);

    //tab completion when in list
    QShortcut *indentLine = new QShortcut(QKeySequence("Tab"), workspace);
    connect (indentLine, SIGNAL(activated()), signalMapper, SLOT(map())) ;
    signalMapper -> setMapping (indentLine, (QObject*)workspace);

    //transpose chars
    QShortcut *transposeChars = new QShortcut(ctrlKey('t'), workspace);
    connect (transposeChars, SIGNAL(activated()), workspace, SLOT(transposeChars())) ;

    //move line or selection up and down
    QShortcut *moveLineUp = new QShortcut(ctrlMetaKey('p'), workspace);
    connect (moveLineUp, SIGNAL(activated()), workspace, SLOT(moveLineOrSelectionUp())) ;

    QShortcut *moveLineDown = new QShortcut(ctrlMetaKey('n'), workspace);
    connect (moveLineDown, SIGNAL(activated()), workspace, SLOT(moveLineOrSelectionDown())) ;

    // Contextual help
    QShortcut *contextHelp = new QShortcut(ctrlKey('i'), workspace);
    connect (contextHelp, SIGNAL(activated()), this, SLOT(helpContext()));

    QShortcut *contextHelp2 = new QShortcut(QKeySequence("F1"), workspace);
    connect (contextHelp2, SIGNAL(activated()), this, SLOT(helpContext()));


    // Font zooming
    QShortcut *fontZoom = new QShortcut(metaKey('='), workspace);
    connect (fontZoom, SIGNAL(activated()), workspace, SLOT(zoomFontIn()));

    QShortcut *fontZoom2 = new QShortcut(metaKey('+'), workspace);
    connect (fontZoom2, SIGNAL(activated()), workspace, SLOT(zoomFontIn()));


    QShortcut *fontZoomOut = new QShortcut(metaKey('-'), workspace);
    connect (fontZoomOut, SIGNAL(activated()), workspace, SLOT(zoomFontOut()));

    QShortcut *fontZoomOut2 = new QShortcut(metaKey('_'), workspace);
    connect (fontZoomOut2, SIGNAL(activated()), workspace, SLOT(zoomFontOut()));

    //set Mark
#ifdef Q_OS_MAC
    QShortcut *setMark = new QShortcut(QKeySequence("Meta+Space"), workspace);
#else
    QShortcut *setMark = new QShortcut(QKeySequence("Ctrl+Space"), workspace);
#endif
    connect (setMark, SIGNAL(activated()), workspace, SLOT(setMark())) ;

    //escape
    QShortcut *escape = new QShortcut(ctrlKey('g'), workspace);
    QShortcut *escape2 = new QShortcut(QKeySequence("Escape"), workspace);
    connect(escape, SIGNAL(activated()), workspace, SLOT(escapeAndCancelSelection()));
    connect(escape, SIGNAL(activated()), this, SLOT(resetErrorPane()));
    connect(escape, SIGNAL(activated()), workspace, SLOT(clearLineMarkers()));
    connect(escape2, SIGNAL(activated()), workspace, SLOT(escapeAndCancelSelection()));
    connect(escape2, SIGNAL(activated()), this, SLOT(resetErrorPane()));
    connect(escape2, SIGNAL(activated()), workspace, SLOT(clearLineMarkers()));

    //quick nav by jumping up and down 10 lines at a time
    QShortcut *forwardTenLines = new QShortcut(shiftMetaKey('u'), workspace);
    connect(forwardTenLines, SIGNAL(activated()), workspace, SLOT(forwardTenLines()));
    QShortcut *backTenLines = new QShortcut(shiftMetaKey('d'), workspace);
    connect(backTenLines, SIGNAL(activated()), workspace, SLOT(backTenLines()));

    //cut to end of line
    QShortcut *cutToEndOfLine = new QShortcut(ctrlKey('k'), workspace);
    connect(cutToEndOfLine, SIGNAL(activated()), workspace, SLOT(cutLineFromPoint()));

    //Emacs live copy and cut
    QShortcut *copyToBuffer = new QShortcut(metaKey(']'), workspace);
    connect(copyToBuffer, SIGNAL(activated()), workspace, SLOT(copyClear()));
    QShortcut *cutToBuffer = new QShortcut(ctrlKey(']'), workspace);
    connect(cutToBuffer, SIGNAL(activated()), workspace, SLOT(cut()));

    QShortcut *pasteToBufferWin = new QShortcut(ctrlKey('v'), workspace);
    connect(pasteToBufferWin, SIGNAL(activated()), workspace, SLOT(paste()));

    //comment line
    QShortcut *toggleLineComment= new QShortcut(metaKey('/'), workspace);
    connect(toggleLineComment, SIGNAL(activated()), this, SLOT(toggleCommentInCurrentWorkspace()));

    //upcase next word
    QShortcut *upcaseWord= new QShortcut(metaKey('u'), workspace);
    connect(upcaseWord, SIGNAL(activated()), workspace, SLOT(upcaseWordOrSelection()));

    //downcase next word
    QShortcut *downcaseWord= new QShortcut(metaKey('l'), workspace);
    connect(downcaseWord, SIGNAL(activated()), workspace, SLOT(downcaseWordOrSelection()));

    QString w = QString(tr("Buffer %1")).arg(QString::number(ws));
    workspaces[ws] = workspace;
    tabs->addTab(workspace, w);
  }

  connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(changeTab(int)));
  connect(signalMapper, SIGNAL(mapped(QObject*)), this, SLOT(completeListOrIndentLine(QObject*)));

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

  autocomplete = new SonicPiAPIs(lexer);
  autocomplete->loadSamples(sample_path);

  // adding universal shortcuts to outputpane seems to
  // steal events from doc system!?
  // addUniversalCopyShortcuts(outputPane);
#if QT_VERSION >= 0x050400
  //requires Qt 5
  new QShortcut(ctrlKey('='), outputPane, SLOT(zoomIn()));
  new QShortcut(ctrlKey('-'), outputPane, SLOT(zoomOut()));
#endif
  addUniversalCopyShortcuts(errorPane);
  outputPane->setReadOnly(true);
  errorPane->setReadOnly(true);
  outputPane->setLineWrapMode(QPlainTextEdit::NoWrap);
#if defined(Q_OS_WIN)
  outputPane->setFontFamily("Courier New");
#elif defined(Q_OS_MAC)
  outputPane->setFontFamily("Menlo");
#else
  outputPane->setFontFamily("Bitstream Vera Sans Mono");
#endif

  if(!theme->font("LogFace").isEmpty()){
      outputPane->setFontFamily(theme->font("LogFace"));
  }

  outputPane->document()->setMaximumBlockCount(1000);
  errorPane->document()->setMaximumBlockCount(1000);

#if QT_VERSION >= 0x050400
  //zoomable QPlainTextEdit requires QT 5.4
  outputPane->zoomIn(1);
#endif
  outputPane->setTextColor(QColor(theme->color("LogInfoForeground")));
  outputPane->appendPlainText("\n");
  //outputPane->append(asciiArtLogo());

  errorPane->zoomIn(1);
  errorPane->setMaximumHeight(130);
  errorPane->setMinimumHeight(130);

  // hudPane = new QTextBrowser;
  // hudPane->setMinimumHeight(130);
  // hudPane->setHtml("<center><img src=\":/images/logo.png\" height=\"113\" width=\"138\"></center>");
  // hudPane->setStyleSheet(defaultTextBrowserStyle);
  // hudWidget = new QDockWidget(this);
  // hudWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
  // hudWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  // hudWidget->setTitleBarWidget(new QWidget());
  // addDockWidget(Qt::RightDockWidgetArea, hudWidget);
  // hudWidget->setWidget(hudPane);
  // hudWidget->setObjectName("hud");

  prefsWidget = new QDockWidget(tr("Preferences"), this);
  prefsWidget->setFocusPolicy(Qt::NoFocus);
  prefsWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  prefsWidget->setFeatures(QDockWidget::DockWidgetClosable);

  prefsCentral = new QWidget;
          prefsWidget->setWidget(prefsCentral);
  QSizePolicy prefsSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
  prefsCentral->setSizePolicy(prefsSizePolicy);
  addDockWidget(Qt::RightDockWidgetArea, prefsWidget);
  prefsWidget->hide();
  prefsWidget->setObjectName("prefs");

  outputWidget = new QDockWidget(tr("Log"), this);
  outputWidget->setFocusPolicy(Qt::NoFocus);
  outputWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);
  outputWidget->setObjectName("output");

  blankWidget = new QWidget();
  outputWidgetTitle = outputWidget->titleBarWidget();

  docsCentral = new QTabWidget;
  docsCentral->setFocusPolicy(Qt::NoFocus);
  docsCentral->setTabsClosable(false);
  docsCentral->setMovable(false);
  docsCentral->setTabPosition(QTabWidget::South);

  docPane = new QTextBrowser;
  QSizePolicy policy = docPane->sizePolicy();
  policy.setHorizontalStretch(QSizePolicy::Maximum);
  docPane->setSizePolicy(policy);
  docPane->setMinimumHeight(200);
  docPane->setOpenExternalLinks(true);
  docPane->setStyleSheet(defaultTextBrowserStyle);

  QShortcut *up = new QShortcut(ctrlKey('p'), docPane);
  up->setContext(Qt::WidgetShortcut);
  connect(up, SIGNAL(activated()), this, SLOT(docScrollUp()));
  QShortcut *down = new QShortcut(ctrlKey('n'), docPane);
  down->setContext(Qt::WidgetShortcut);
  connect(down, SIGNAL(activated()), this, SLOT(docScrollDown()));

  docPane->setSource(QUrl("qrc:///html/doc.html"));

  addUniversalCopyShortcuts(docPane);

  docsplit = new QSplitter;

  docsplit->addWidget(docsCentral);
  docsplit->addWidget(docPane);

  docWidget = new QDockWidget(tr("Help"), this);
  docWidget->setFocusPolicy(Qt::NoFocus);
  docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
  docWidget->setWidget(docsplit);
  docWidget->setObjectName("help");

  addDockWidget(Qt::BottomDockWidgetArea, docWidget);
  docWidget->hide();

  // Currently causes a segfault when dragging doc pane out of main
  // window:
  // connect(docWidget, SIGNAL(visibilityChanged(bool)), this,
  // SLOT(helpClosed(bool)));

  mainWidgetLayout = new QVBoxLayout;
  mainWidgetLayout->addWidget(tabs);
  mainWidgetLayout->addWidget(errorPane);
  mainWidget = new QWidget;
  mainWidget->setFocusPolicy(Qt::NoFocus);
  errorPane->hide();
  mainWidget->setLayout(mainWidgetLayout);
  setCentralWidget(mainWidget);

  createShortcuts();
  createToolBar();
  createStatusBar();
  createInfoPane();

  readSettings();


  setWindowTitle(tr("Sonic Pi"));

  connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );

  waitForServiceSync();

  initPrefsWindow();

  if(settings.value("first_time", 1).toInt() == 1) {
    QTextBrowser* startupPane = new QTextBrowser;
    startupPane->setFixedSize(600, 615);
    startupPane->setWindowIcon(QIcon(":images/icon-smaller.png"));
    startupPane->setWindowTitle(tr("Welcome to Sonic Pi"));
    addUniversalCopyShortcuts(startupPane);
    startupPane->document()->setDefaultStyleSheet(readFile(":/theme/light/doc-styles.css"));
    startupPane->setSource(QUrl("qrc:///html/startup.html"));
    startupPane->setStyleSheet(defaultTextBrowserStyle);
    docWidget->show();
    startupPane->show();
  }

  restoreDocPane = false;

  focusMode = false;

  updateDarkMode();
  initDocsWindow();
  updateFullScreenMode();
  updateTabsVisibility();
  updateButtonVisibility();
  updateLogVisibility();

  requestVersion();
}

void MainWindow::changeTab(int id){
  tabs->setCurrentIndex(id);
}

void MainWindow::toggleFullScreenMode() {
  full_screen->toggle();
  updateFullScreenMode();
}

void MainWindow::updateFullScreenMode(){
  if (full_screen->isChecked()) {
    mainWidgetLayout->setMargin(0);
    outputWidget->setTitleBarWidget(blankWidget);
    this->setWindowFlags(Qt::FramelessWindowHint);
    this->setWindowState(Qt::WindowFullScreen);
    this->show();
  }
  else {
    mainWidgetLayout->setMargin(9);
    outputWidget->setTitleBarWidget(outputWidgetTitle);
    this->setWindowState(windowState() & ~(Qt::WindowFullScreen));
#ifdef Q_OS_WIN
    this->setWindowFlags(Qt::WindowTitleHint | Qt::WindowSystemMenuHint |
			 Qt::WindowMinimizeButtonHint |
			 Qt::WindowMaximizeButtonHint |
			 Qt::WindowCloseButtonHint);
#else
    this->setWindowFlags(Qt::WindowTitleHint);
#endif
    this->show();
  }
}

void MainWindow::toggleFocusMode() {
  focusMode = !focusMode;
  updateFocusMode();
}

void MainWindow::updateFocusMode(){
  if (focusMode) {
    full_screen->setChecked(true);
    show_tabs->setChecked(false);
    show_buttons->setChecked(false);
    show_log->setChecked(false);
  }
  else {
    full_screen->setChecked(false);
    show_tabs->setChecked(true);
    show_buttons->setChecked(true);
    show_log->setChecked(true);
  }

  updateFullScreenMode();
  updateTabsVisibility();
  updateButtonVisibility();
  updateLogVisibility();
}

void MainWindow::toggleLogVisibility() {
  show_log->toggle();
  updateLogVisibility();
}

void MainWindow::updateLogVisibility(){
  if(show_log->isChecked()) {
    outputWidget->show();
  }
  else{
    outputWidget->close();
  }
}


void MainWindow::toggleTabsVisibility() {
  show_tabs->toggle();
  updateTabsVisibility();
}

void MainWindow::updateTabsVisibility(){
  QTabBar *tabBar = tabs->findChild<QTabBar *>();

  if(show_tabs->isChecked()) {
    tabBar->show();
  }
  else{
    tabBar->hide();
  }
}

void MainWindow::toggleButtonVisibility() {

  show_buttons->toggle();
  updateButtonVisibility();

}

void MainWindow::updateButtonVisibility(){
  if (show_buttons->isChecked()) {
    toolBar->show();
  }
  else {
    toolBar->close();
  }
}

void MainWindow::completeListOrIndentLine(QObject* ws){
  SonicPiScintilla *spws = ((SonicPiScintilla*)ws);
  if(spws->isListActive()) {
    spws->tabCompleteifList();
  }
  else {
    indentCurrentLineOrSelection(spws);
  }
}

void MainWindow::indentCurrentLineOrSelection(SonicPiScintilla* ws) {
  int start_line, finish_line, point_line, point_index;
  ws->getCursorPosition(&point_line, &point_index);
  if(ws->hasSelectedText()) {
    statusBar()->showMessage(tr("Indenting selection..."), 2000);
    int unused_a, unused_b;
    ws->getSelection(&start_line, &unused_a, &finish_line, &unused_b);
  } else {
    statusBar()->showMessage(tr("Indenting line..."), 2000);
    start_line = point_line;
    finish_line = point_line;
  }


  std::string code = ws->text().toStdString();

  Message msg("/complete-snippet-or-indent-selection");
  msg.pushStr(guiID.toStdString());
  std::string filename = workspaceFilename(ws);
  msg.pushStr(filename);
  msg.pushStr(code);
  msg.pushInt32(start_line);
  msg.pushInt32(finish_line);
  msg.pushInt32(point_line);
  msg.pushInt32(point_index);
  sendOSC(msg);
}

void MainWindow::toggleCommentInCurrentWorkspace() {
  SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();
  toggleComment(ws);
}

void MainWindow::toggleComment(SonicPiScintilla* ws) {
  int start_line, finish_line, point_line, point_index;
  ws->getCursorPosition(&point_line, &point_index);
  if(ws->hasSelectedText()) {
    statusBar()->showMessage(tr("Commenting selection..."), 2000);
    int unused_a, unused_b;
    ws->getSelection(&start_line, &unused_a, &finish_line, &unused_b);
  } else {
    statusBar()->showMessage(tr("Commenting line..."), 2000);
    start_line = point_line;
    finish_line = point_line;
  }


  std::string code = ws->text().toStdString();

  Message msg("/toggle-comment");
  msg.pushStr(guiID.toStdString());
  std::string filename = workspaceFilename(ws);
  msg.pushStr(filename);
  msg.pushStr(code);
  msg.pushInt32(start_line);
  msg.pushInt32(finish_line);
  msg.pushInt32(point_line);
  msg.pushInt32(point_index);
  sendOSC(msg);
}

QString MainWindow::rootPath() {
  // diversity is the spice of life
#if defined(Q_OS_MAC)
  return QCoreApplication::applicationDirPath() + "/../..";
#elif defined(Q_OS_WIN)
  return QCoreApplication::applicationDirPath() + "/../../../..";
#else
  return QCoreApplication::applicationDirPath() + "/../../..";
#endif
}

void MainWindow::startServer(){

  // kill any zombie processes that may exist
  // better: test to see if UDP ports are in use, only kill/sleep if so
  // best: kill SCSynth directly if needed
  std::cout << "[GUI] - shutting down any old audio servers..." << std::endl;
  Message msg("/exit");
  msg.pushStr(guiID.toStdString());
  sendOSC(msg);
  sleep(2);


    serverProcess = new QProcess();

    QString root = rootPath();

  #if defined(Q_OS_WIN)
    QString prg_path = root + "/app/server/native/windows/ruby/bin/ruby.exe";
    QString prg_arg = root + "/app/server/bin/sonic-pi-server.rb";
    sample_path = root + "/etc/samples";
  #elif defined(Q_OS_MAC)
    QString prg_path = root + "/server/native/osx/ruby/bin/ruby";
    QString prg_arg = root + "/server/bin/sonic-pi-server.rb";
    sample_path = root + "/etc/samples";
  #else
    //assuming Raspberry Pi
    QString prg_path = root + "/app/server/native/raspberry/ruby/bin/ruby";
    QFile file(prg_path);
    if(!file.exists()) {
      // use system ruby if bundled ruby doesn't exist
      prg_path = "/usr/bin/ruby";
    }

    QString prg_arg = root + "/app/server/bin/sonic-pi-server.rb";
    sample_path = root + "/etc/samples";
  #endif

    prg_path = QDir::toNativeSeparators(prg_path);
    prg_arg = QDir::toNativeSeparators(prg_arg);


    QStringList args;
    args << prg_arg;

    if(protocol == TCP){
        args << "-t";
    }



    //    std::cout << "[GUI] - exec "<< prg_path.toStdString() << " " << prg_arg.toStdString() << std::endl;

    std::cout << "[GUI] - booting live coding server" << std::endl;
    QString sp_error_log_path = log_path + QDir::separator() + "server-errors.log";
    QString sp_output_log_path = log_path + QDir::separator() + "server-output.log";
    serverProcess->setStandardErrorFile(sp_error_log_path);
    serverProcess->setStandardOutputFile(sp_output_log_path);
    serverProcess->start(prg_path, args);
    if (!serverProcess->waitForStarted()) {
      invokeStartupError(tr("The Sonic Pi server could not be started!"));
      return;
    }
}

void MainWindow::waitForServiceSync() {
  int timeout = 30;
  std::cout << "[GUI] - waiting for server to connect..." << std::endl;
  while (sonicPiServer->waitForServer() && timeout-- > 0) {
    sleep(1);
    if(sonicPiServer->isIncomingPortOpen()) {
      Message msg("/ping");
      msg.pushStr(guiID.toStdString());
      msg.pushStr("QtClient/1/hello");
      sendOSC(msg);
    }
  }
  if (!sonicPiServer->isServerStarted()) {

    if (!startup_error_reported) {
      std::cout << "[GUI] - critical error!" << std::endl;
      invokeStartupError("Critical server error!");
    }
    return;
  }

  std::cout << "[GUI] - server connection established" << std::endl;

}

void MainWindow::splashClose() {
#if defined(Q_OS_MAC)
  splash->close();
#else
  splash->finish(this);
#endif
}

void MainWindow::serverStarted() {
  splashClose();
  loadWorkspaces();

  QSettings settings("uk.ac.cam.cl", "Sonic Pi");

  if(settings.value("first_time", 1).toInt() == 1) {
    showMaximized();
  } else {
    showNormal();

  }
  changeShowLineNumbers();
}

void MainWindow::update_mixer_invert_stereo() {
  if (mixer_invert_stereo->isChecked()) {
    mixerInvertStereo();
  } else {
    mixerStandardStereo();
  }
}

void MainWindow::update_mixer_force_mono() {
  if (mixer_force_mono->isChecked()) {
    mixerMonoMode();
  } else {
    mixerStereoMode();
  }
}

void MainWindow::update_check_updates() {
  if (check_updates->isChecked()) {
    enableCheckUpdates();
  } else {
    disableCheckUpdates();
  }
}

void MainWindow::initPrefsWindow() {

  prefTabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);

  QGridLayout *grid = new QGridLayout;

  QGroupBox *volBox = new QGroupBox(tr("Raspberry Pi System Volume"));
  volBox->setToolTip(tr("Use this slider to change the system volume of your Raspberry Pi."));

  QGroupBox *advancedAudioBox = new QGroupBox(tr("Advanced Audio"));
  advancedAudioBox->setToolTip(tr("Advanced audio settings for working with\nexternal PA systems when performing with Sonic Pi."));
  mixer_invert_stereo = new QCheckBox(tr("Invert Stereo"));
  mixer_invert_stereo->setToolTip(tr("Toggle stereo inversion.\nIf enabled, audio sent to the left speaker will\nbe routed to the right speaker and visa versa."));
  connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
  mixer_force_mono = new QCheckBox(tr("Force Mono"));
  mixer_force_mono->setToolTip(tr("Toggle mono mode.\nIf enabled both right and left audio is mixed and\nthe same signal is sent to both speakers.\nUseful when working with external systems that\ncan only handle mono."));
  connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));

  check_args = new QCheckBox(tr("Safe mode"));
  check_args->setToolTip(tr("Toggle synth argument checking functions.\nIf disabled, certain synth opt values may\ncreate unexpectedly loud or uncomfortable sounds."));

  QVBoxLayout *advanced_audio_box_layout = new QVBoxLayout;
  advanced_audio_box_layout->addWidget(mixer_invert_stereo);
  advanced_audio_box_layout->addWidget(mixer_force_mono);
  advanced_audio_box_layout->addWidget(check_args);
  // audio_box->addWidget(radio2);
  // audio_box->addWidget(radio3);
  // audio_box->addStretch(1);
  advancedAudioBox->setLayout(advanced_audio_box_layout);


  QGroupBox *audioOutputBox = new QGroupBox(tr("Raspberry Pi Audio Output"));
  audioOutputBox->setToolTip(tr("Your Raspberry Pi has two forms of audio output.\nFirstly, there is the headphone jack of the Raspberry Pi itself.\nSecondly, some HDMI monitors/TVs support audio through the HDMI port.\nUse these buttons to force the output to the one you want."));
  rp_force_audio_default = new QRadioButton(tr("&Default"));
  rp_force_audio_headphones = new QRadioButton(tr("&Headphones"));
  rp_force_audio_hdmi = new QRadioButton(tr("&HDMI"));


  connect(rp_force_audio_default, SIGNAL(clicked()), this, SLOT(setRPSystemAudioAuto()));
  connect(rp_force_audio_headphones, SIGNAL(clicked()), this, SLOT(setRPSystemAudioHeadphones()));
  connect(rp_force_audio_hdmi, SIGNAL(clicked()), this, SLOT(setRPSystemAudioHDMI()));

  QVBoxLayout *audio_box = new QVBoxLayout;
  audio_box->addWidget(rp_force_audio_default);
  audio_box->addWidget(rp_force_audio_headphones);
  audio_box->addWidget(rp_force_audio_hdmi);
  audio_box->addStretch(1);
  audioOutputBox->setLayout(audio_box);

  QHBoxLayout *vol_box = new QHBoxLayout;
  rp_system_vol = new QSlider(this);
  connect(rp_system_vol, SIGNAL(valueChanged(int)), this, SLOT(changeRPSystemVol(int)));
  vol_box->addWidget(rp_system_vol);
  volBox->setLayout(vol_box);

  QGroupBox *debug_box = new QGroupBox(tr("Logging"));
  debug_box->setToolTip(tr("Configure debug behaviour"));

  print_output = new QCheckBox(tr("Log synths"));
  print_output->setToolTip(tr("Toggle log messages.\nIf disabled, activity such as synth and sample\ntriggering will not be printed to the log by default."));

  clear_output_on_run = new QCheckBox(tr("Clear log on run"));
  clear_output_on_run->setToolTip(tr("Toggle log clearing on run.\nIf enabled, the log is cleared each\ntime the run button is pressed."));

  log_cues = new QCheckBox(tr("Log cues"));
  log_cues->setToolTip(tr("Enable or disable logging of cues.\nIf disabled, cues will still trigger.\nHowever, they will not be visible in the logs."));

  QVBoxLayout *debug_box_layout = new QVBoxLayout;
  debug_box_layout->addWidget(print_output);
  debug_box_layout->addWidget(log_cues);
  debug_box_layout->addWidget(clear_output_on_run);
  debug_box->setLayout(debug_box_layout);


  QGroupBox *transparency_box = new QGroupBox(tr("Transparency"));
  QGridLayout *transparency_box_layout = new QGridLayout;
  gui_transparency_slider = new QSlider(this);
  connect(gui_transparency_slider, SIGNAL(valueChanged(int)), this, SLOT(changeGUITransparency(int)));
  transparency_box_layout->addWidget(gui_transparency_slider);
  transparency_box->setLayout(transparency_box_layout);





  QGroupBox *update_box = new QGroupBox(tr("Updates"));
  QSizePolicy updatesPrefSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
  check_updates = new QCheckBox(tr("Check for updates"));
  update_box->setSizePolicy(updatesPrefSizePolicy);
  check_updates->setToolTip(tr("Toggle automatic update checking.\nThis check involves sending anonymous information about your platform and version."));
  check_updates_now = new QPushButton(tr("Check now"));
  check_updates_now->setToolTip(tr("Force a check for updates now.\nThis check involves sending anonymous information about your platform and version."));
  visit_sonic_pi_net = new QPushButton(tr("Get update"));
  visit_sonic_pi_net->setToolTip(tr("Visit http://sonic-pi.net to download new version"));
  visit_sonic_pi_net->setVisible(false);
  check_updates_now->setMaximumWidth(100);
  visit_sonic_pi_net->setMaximumWidth(150);

  QGroupBox *update_info_box = new QGroupBox(tr("Update Info"));
  update_info_box->setMaximumWidth(350);
  QVBoxLayout *update_info_box_layout = new QVBoxLayout;
  update_info_box_layout->addWidget(update_info);
  update_info_box->setLayout(update_info_box_layout);


  connect(check_updates, SIGNAL(clicked()), this, SLOT(update_check_updates()));
  connect(visit_sonic_pi_net, SIGNAL(clicked()), this, SLOT(open_sonic_pi_net()));
  connect(check_updates_now, SIGNAL(clicked()), this, SLOT(check_for_updates_now()));

  QVBoxLayout *update_box_layout = new QVBoxLayout;
  update_box_layout->addWidget(check_updates);

  update_box_layout->addWidget(check_updates_now);
  update_box_layout->addWidget(visit_sonic_pi_net);
  update_box->setLayout(update_box_layout);

  QGroupBox *editor_box = new QGroupBox();
  QGroupBox *editor_display_box = new QGroupBox(tr("Show and Hide"));
  editor_display_box->setToolTip(tr("Configure editor display options."));
  QGroupBox *editor_look_feel_box = new QGroupBox(tr("Look and Feel"));
  editor_look_feel_box->setToolTip(tr("Configure editor look and feel."));
  QGroupBox *automation_box = new QGroupBox(tr("Automation"));
  automation_box->setToolTip(tr("Configure automation features."));
  auto_indent_on_run = new QCheckBox(tr("Auto-align"));
  auto_indent_on_run->setToolTip(tr("Automatically align code on Run"));

  show_line_numbers = new QCheckBox(tr("Show line numbers"));
  show_line_numbers->setToolTip(tr("Toggle line number visibility."));
  show_log = new QCheckBox(tr("Show log"));
  show_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of the log.")));
  show_log->setChecked(true);
  show_buttons = new QCheckBox(tr("Show buttons"));
  show_buttons->setToolTip(tooltipStrShiftMeta('B', tr("Toggle visibility of the control buttons.")));
  show_buttons->setChecked(true);
  show_tabs = new QCheckBox(tr("Show tabs"));
  show_tabs->setChecked(true);
  show_tabs->setToolTip(tr("Toggle visibility of the buffer selection tabs."));
  full_screen = new QCheckBox(tr("Full screen"));
  full_screen->setToolTip(tooltipStrShiftMeta('F', tr("Toggle full screen mode.")));
  dark_mode = new QCheckBox(tr("Dark mode"));
  dark_mode->setToolTip(tooltipStrShiftMeta('M', tr("Toggle dark mode.")) + QString(tr("\nDark mode is perfect for live coding in night clubs.")));
  connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(changeShowLineNumbers()));
  connect(show_log, SIGNAL(clicked()), this, SLOT(updateLogVisibility()));
  connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateButtonVisibility()));
  connect(full_screen, SIGNAL(clicked()), this, SLOT(updateFullScreenMode()));
  connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateTabsVisibility()));
  connect(dark_mode, SIGNAL(clicked()), this, SLOT(updateDarkMode()));

  QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
  QVBoxLayout *editor_box_look_feel_layout = new QVBoxLayout;
  QVBoxLayout *automation_box_layout = new QVBoxLayout;
  QGridLayout *gridEditorPrefs = new QGridLayout;
  editor_display_box_layout->addWidget(show_line_numbers);
  editor_display_box_layout->addWidget(show_log);
  editor_display_box_layout->addWidget(show_buttons);
  editor_display_box_layout->addWidget(show_tabs);
  editor_box_look_feel_layout->addWidget(dark_mode);
  editor_box_look_feel_layout->addWidget(full_screen);
  editor_display_box->setLayout(editor_display_box_layout);
  editor_look_feel_box->setLayout(editor_box_look_feel_layout);

  automation_box_layout->addWidget(auto_indent_on_run);
  automation_box->setLayout(automation_box_layout);

  gridEditorPrefs->addWidget(editor_display_box, 0, 0);
  gridEditorPrefs->addWidget(editor_look_feel_box, 0, 1);
  gridEditorPrefs->addWidget(automation_box, 1, 1);


  editor_box->setLayout(gridEditorPrefs);
  grid->addWidget(prefTabs, 0, 0);

#if defined(Q_OS_LINUX)
  QGroupBox *audio_prefs_box = new QGroupBox();
  QGridLayout *audio_prefs_box_layout = new QGridLayout;

  audio_prefs_box_layout->addWidget(audioOutputBox, 0, 0);
  audio_prefs_box_layout->addWidget(volBox, 0, 1);
  audio_prefs_box->setLayout(audio_prefs_box_layout);
  prefTabs->addTab(audio_prefs_box, tr("Audio"));
#endif

    QGroupBox *studio_prefs_box = new QGroupBox();
  QGridLayout *studio_prefs_box_layout = new QGridLayout;

  studio_prefs_box_layout->addWidget(advancedAudioBox, 0, 0);

  studio_prefs_box_layout->addWidget(debug_box, 0, 1);

  studio_prefs_box->setLayout(studio_prefs_box_layout);

  prefTabs->addTab(editor_box, tr("Editor"));
  prefTabs->addTab(studio_prefs_box, tr("Studio"));

  QGroupBox *performance_box = new QGroupBox(tr("Performance"));
  performance_box->setToolTip(tr("Settings useful for performing with Sonic Pi"));


#if defined(Q_OS_WIN)
  // do nothing
#elif defined(Q_OS_MAC)
  QGridLayout *performance_box_layout = new QGridLayout;
  performance_box_layout->addWidget(transparency_box, 0, 0);
  performance_box->setLayout(performance_box_layout);
  prefTabs->addTab(performance_box, tr("Performance"));
#else
    // assuming Raspberry Pi
    // do nothing
#endif


  QGroupBox *update_prefs_box = new QGroupBox();
  QGridLayout *update_prefs_box_layout = new QGridLayout;

  update_prefs_box_layout->addWidget(update_info_box, 0, 0);
  update_prefs_box_layout->addWidget(update_box, 0, 1);
  update_prefs_box->setLayout(update_prefs_box_layout);
  prefTabs->addTab(update_prefs_box, tr("Updates"));

  if (!i18n) {
    QGroupBox *translation_box = new QGroupBox("Translation");
    QVBoxLayout *translation_box_layout = new QVBoxLayout;
    QLabel *go_translate = new QLabel;
    go_translate->setOpenExternalLinks(true);
    go_translate->setText(
      "Sonic Pi hasn't been translated to " +
      QLocale::languageToString(QLocale::system().language()) +
      " yet.<br>" +
      "You can help " +
      "<a href=\"https://github.com/samaaron/sonic-pi/blob/master/TRANSLATION.md\">" +
      "translate the Sonic Pi GUI</a> to your language."
    );
    go_translate->setTextFormat(Qt::RichText);
    translation_box_layout->addWidget(go_translate);
    translation_box->setLayout(translation_box_layout);

    grid->addWidget(translation_box, 3, 0, 1, 2);
  }

  prefsCentral->setLayout(grid);

  // Read in preferences from previous session
  QSettings settings("uk.ac.cam.cl", "Sonic Pi");
  check_args->setChecked(settings.value("prefs/check-args", true).toBool());
  print_output->setChecked(settings.value("prefs/print-output", true).toBool());
  clear_output_on_run->setChecked(settings.value("prefs/clear-output-on-run", true).toBool());
  log_cues->setChecked(settings.value("prefs/log-cues", true).toBool());
  show_line_numbers->setChecked(settings.value("prefs/show-line-numbers", true).toBool());
  dark_mode->setChecked(settings.value("prefs/dark-mode", false).toBool());
  mixer_force_mono->setChecked(settings.value("prefs/mixer-force-mono", false).toBool());
  mixer_invert_stereo->setChecked(settings.value("prefs/mixer-invert-stereo", false).toBool());

  rp_force_audio_default->setChecked(settings.value("prefs/rp/force-audio-default", true).toBool());
  rp_force_audio_headphones->setChecked(settings.value("prefs/rp/force-audio-headphones", false).toBool());
  rp_force_audio_hdmi->setChecked(settings.value("prefs/rp/force-audio-hdmi", false).toBool());

  check_updates->setChecked(settings.value("prefs/rp/check-updates", true).toBool());

  auto_indent_on_run->setChecked(settings.value("prefs/auto-indent-on-run", true).toBool());

  gui_transparency_slider->setValue(settings.value("prefs/gui_transparency", 0).toInt());

  int stored_vol = settings.value("prefs/rp/system-vol", 50).toInt();
  rp_system_vol->setValue(stored_vol);

  // Ensure prefs are honoured on boot
  update_mixer_invert_stereo();
  update_mixer_force_mono();
  changeRPSystemVol(stored_vol);
  update_check_updates();

  if(settings.value("prefs/rp/force-audio-default", true).toBool()) {
    setRPSystemAudioAuto();
  }
  if(settings.value("prefs/rp/force-audio-headphones", false).toBool()) {
    setRPSystemAudioHeadphones();
  }
  if(settings.value("prefs/rp/force-audio-hdmi", false).toBool()) {
    setRPSystemAudioHDMI();
  }


}

void MainWindow::invokeStartupError(QString msg) {
  startup_error_reported = true;
  sonicPiServer->stopServer();
  QMetaObject::invokeMethod(this, "startupError",
			    Qt::QueuedConnection,
			    Q_ARG(QString, msg));
}

void MainWindow::startupError(QString msg) {
  splashClose();

  QString gui_log = readFile(log_path + QDir::separator() + "gui.log");
  QString server_errors_log = readFile(log_path + QDir::separator() + "server-errors.log");
  QString server_output_log = readFile(log_path + QDir::separator() + "server-output.log");

  QMessageBox *box = new QMessageBox(QMessageBox::Warning,
				     tr("Server boot error..."), tr("Apologies, a critical error occurred during startup") + ":\n\n " + msg + "\n\n" + tr("Please consider reporting a bug at") + "\nhttp://github.com/samaaron/sonic-pi/issues");
  QString error_report = "Detailed Error Report:\n\nGUI log\n-------\n" + gui_log + "\n\n\nServer Errors\n-------------\n\n" + server_errors_log + "\n\n\nServer Output\n-------------\n\n" + server_output_log;
  box->setDetailedText(error_report);

  QGridLayout* layout = (QGridLayout*)box->layout();
  QSpacerItem* hSpacer = new QSpacerItem(200, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);
  layout->addItem(hSpacer, layout->rowCount(), 0, 1, layout->columnCount());
  box->exec();
  close();
}

void MainWindow::replaceBuffer(QString id, QString content, int line, int index, int first_line) {
  SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
  ws->selectAll();
  ws->replaceSelectedText(content);
  ws->setCursorPosition(line, index);
  ws->setFirstVisibleLine(first_line);
}

void MainWindow::replaceLines(QString id, QString content, int start_line, int finish_line, int point_line, int point_index) {
  SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
  ws->replaceLines(start_line, finish_line, content);
  ws->setCursorPosition(point_line, point_index);
}

std::string MainWindow::number_name(int i) {
  switch(i) {
  case 0: return "zero";
  case 1: return "one";
  case 2: return "two";
  case 3: return "three";
  case 4: return "four";
  case 5: return "five";
  case 6: return "six";
  case 7: return "seven";
  case 8: return "eight";
  case 9: return "nine";
  default: assert(false); return "";
  }
}

std::string MainWindow::workspaceFilename(SonicPiScintilla* text)
{
  for(int i = 0; i < workspace_max; i++) {
    if(text == workspaces[i]) {
      return "workspace_" + number_name(i);
    }
  }
  return "default";
}

void MainWindow::loadWorkspaces()
{
  std::cout << "[GUI] - loading workspaces" << std::endl;

  for(int i = 0; i < workspace_max; i++) {
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

  for(int i = 0; i < workspace_max; i++) {
    std::string code = workspaces[i]->text().toStdString();
    Message msg("/save-buffer");
    msg.pushStr(guiID.toStdString());
    std::string s = "workspace_" + number_name(i);
    msg.pushStr(s);
    msg.pushStr(code);
    sendOSC(msg);
  }
}

void MainWindow::closeEvent(QCloseEvent *event)
{
  writeSettings();
  std::cout.rdbuf(coutbuf); // reset to stdout before exiting
  event->accept();
}

QString MainWindow::currentTabLabel()
{
  return tabs->tabText(tabs->currentIndex());
}


bool MainWindow::saveAs()
{
  QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Buffer"), QDir::homePath() + "/Desktop");
  if(!fileName.isEmpty()){
    if (!fileName.contains(QRegExp("\\.[a-z]+$"))) {
        fileName = fileName + ".txt";
      }
    return saveFile(fileName, (SonicPiScintilla*)tabs->currentWidget());
  } else {
    return false;
  }
}

void MainWindow::sendOSC(Message m)
{
  int TIMEOUT = 30000;
  int PORT_NUM = 4557;

  if(protocol == UDP){
    UdpSocket sock;
    sock.connectTo("localhost", PORT_NUM);
    if (!sock.isOk()) {
        std::cerr << "[GUI] - Error connection to port " << PORT_NUM << ": " << sock.errorMessage() << "\n";
    } else {
        PacketWriter pw;
        pw.addMessage(m);
        sock.sendPacket(pw.packetData(), pw.packetSize());
    }
  }
  else{
    if (clientSock->state() != QAbstractSocket::ConnectedState){
      clientSock->connectToHost("localhost", PORT_NUM,  QIODevice::ReadWrite);
    }

    if(!clientSock->waitForConnected(TIMEOUT)){
      std::cerr <<  "[GUI] - Timeout, could not connect" << "\n";
      clientSock->abort();
      return;
    }

    if(clientSock->state() == QAbstractSocket::ConnectedState){
      PacketWriter pw;
      pw.addMessage(m);

      int bytesWritten = clientSock->write(pw.packetDataForStream(), pw.packetSize()+sizeof(uint32_t));
      clientSock->waitForBytesWritten();

      if (bytesWritten < 0){
        std::cerr <<  "[GUI] - Failed to send bytes" << "\n";
      }

    } else {
      std::cerr << "[GUI] - Client gone away: " << "\n";
    }
  }
}

void MainWindow::resetErrorPane() {
  errorPane->clear();
  errorPane->hide();
}


void MainWindow::runCode()
{
  if(auto_indent_on_run->isChecked()) {
    beautifyCode();
  }
  SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();
  ws->highlightAll();
  lexer->highlightAll();
  ws->clearLineMarkers();
  resetErrorPane();
  statusBar()->showMessage(tr("Running Code..."), 1000);
  std::string code = ws->text().toStdString();
  Message msg("/save-and-run-buffer");
  msg.pushStr(guiID.toStdString());
  std::string filename = workspaceFilename( (SonicPiScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  if(!print_output->isChecked()) {
    code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  if(!log_cues->isChecked()) {
    code = "use_cue_logging false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  else{
    code = "use_debug true #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  if(!check_args->isChecked()) {
    code = "use_arg_checks false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  else {
    code = "use_arg_checks true #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  if(clear_output_on_run->isChecked()){
    outputPane->clear();
  }

  msg.pushStr(code);
  msg.pushStr(QString(tr("Workspace %1")).arg(tabs->currentIndex()).toStdString());
  sendOSC(msg);

  QTimer::singleShot(500, this, SLOT(unhighlightCode()));
}

void MainWindow::unhighlightCode()
{
  SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();
  ws->unhighlightAll();

  lexer->unhighlightAll();
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
  Message msg("/beautify-buffer");
  msg.pushStr(guiID.toStdString());
  std::string filename = workspaceFilename( (SonicPiScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  msg.pushStr(code);
  msg.pushInt32(line);
  msg.pushInt32(index);
  msg.pushInt32(first_line);
  sendOSC(msg);
}

void MainWindow::reloadServerCode()
{
  statusBar()->showMessage(tr("Reloading..."), 2000);
  Message msg("/reload");
  msg.pushStr(guiID.toStdString());
  sendOSC(msg);
}

void MainWindow::check_for_updates_now() {
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

void MainWindow::about()
{
  // todo: this is returning true even after the window disappears
  // Qt::Tool windows get closed automatically when app loses focus
  if(infoWidg->isVisible()) {
    infoWidg->hide();
  } else {
    infoWidg->raise();
    infoWidg->show();
  }
}


void MainWindow::help()
{
  if(docWidget->isVisible()) {
    hidingDocPane = true;
    docWidget->hide();
  } else {
    docWidget->show();
  }
}

void MainWindow::helpContext()
{
  if (!docWidget->isVisible())
    docWidget->show();
  SonicPiScintilla *ws = ((SonicPiScintilla*)tabs->currentWidget());
  QString selection = ws->selectedText();
  if (selection == "") { // get current word instead
    int line, pos;
    ws->getCursorPosition(&line, &pos);
    QString text = ws->text(line);
    int start, end;
    for (start = pos; start > 0; start--) {
      if (!text[start-1].isLetter() && text[start-1] != '_') break;
    }
    QString identifierEndChars = QString("?!_=");
    for (end = pos; end < text.length(); end++) {
      if (!text[end].isLetter() && !identifierEndChars.contains(text[end])) break;
    }
    selection = text.mid(start, end-start);
  }
  selection = selection.toLower();
  if (selection[0] == ':')
    selection = selection.mid(1);

  if (helpKeywords.contains(selection)) {
    struct help_entry entry = helpKeywords[selection];
    QMetaObject::invokeMethod(docsCentral, "setCurrentIndex",
			      Q_ARG(int, entry.pageIndex));
    QListWidget *list = helpLists[entry.pageIndex];
    list->setCurrentRow(entry.entryIndex);
  }
}



#if defined(Q_OS_MAC)
void MainWindow::changeGUITransparency(int val)
#else
void MainWindow::changeGUITransparency(int)
#endif
{
#if defined(Q_OS_MAC)
  // scale it linearly from 0 -> 100 to 0.3 -> 1
  setWindowOpacity((0.7 * ((100 - (float)val) / 100.0))  + 0.3);
#else
    // do nothing
#endif


}


#if defined(Q_OS_LINUX)
void MainWindow::changeRPSystemVol(int val)
#else
void MainWindow::changeRPSystemVol(int)
#endif
{
#if defined(Q_OS_WIN)
  // do nothing
#elif defined(Q_OS_MAC)
  // do nothing
#else
  //assuming Raspberry Pi
  QProcess *p = new QProcess();
  float v = (float) val;
  // handle the fact that the amixer percentage range isn't linear
  float vol_float = std::pow(v/100.0, (float)1./3.) * 100.0;
  std::ostringstream ss;
  ss << vol_float;
  statusBar()->showMessage(tr("Updating System Volume..."), 2000);
  QString prog = "amixer cset numid=1 " + QString::fromStdString(ss.str()) + '%';
  p->start(prog);
#endif

}

void MainWindow::toggleDarkMode() {
  dark_mode->toggle();
  updateDarkMode();
}

void MainWindow::updateDarkMode(){
  SonicPiTheme *currentTheme = lexer->theme;

  QString css = readFile(QString(":/theme/%1/doc-styles.css").arg(dark_mode->isChecked() ? "dark" : "light"));
  docPane->document()->setDefaultStyleSheet(css);
  docPane->reload();
  foreach(QTextBrowser* pane, infoPanes) {
    pane->document()->setDefaultStyleSheet(css);
    pane->reload();
  }
  errorPane->document()->setDefaultStyleSheet(css);

  if(dark_mode->isChecked()){
    currentTheme->darkMode();

    // clear stylesheets
    this->setStyleSheet("");
    infoWidg->setStyleSheet("");
    mainWidget->setStyleSheet("");
    statusBar()->setStyleSheet("");
    outputPane->setStyleSheet("");
    outputWidget->setStyleSheet("");
    prefsWidget->setStyleSheet("");
    tabs->setStyleSheet("");
    prefTabs->setStyleSheet("");
    docsCentral->setStyleSheet("");
    docWidget->setStyleSheet("");
    toolBar->setStyleSheet("");

    docPane->setStyleSheet(defaultTextBrowserStyle);

    QPalette p = QApplication::palette();
    p.setColor(QPalette::WindowText,      currentTheme->color("WindowForeground"));
    p.setColor(QPalette::Window,          currentTheme->color("WindowBackground"));
    p.setColor(QPalette::Base,            QColor("#a3a3a3"));
    p.setColor(QPalette::AlternateBase,   QColor("#a2a2a2"));
    p.setColor(QPalette::Text,            QColor("#000"));
    p.setColor(QPalette::HighlightedText, currentTheme->color("HighlightedForeground"));
    p.setColor(QPalette::Highlight,       currentTheme->color("HighlightedBackground"));
    p.setColor(QPalette::ToolTipBase,   QColor("#929292"));
    p.setColor(QPalette::ToolTipText,   QColor("#fff"));
    p.setColor(QPalette::Button,        QColor("#000"));
    p.setColor(QPalette::ButtonText,    QColor("#fff"));
    p.setColor(QPalette::Shadow,        QColor("#333"));
    p.setColor(QPalette::Mid, QColor("#222"));
    p.setColor(QPalette::Dark, QColor("#333"));
    p.setColor(QPalette::Midlight, QColor("#222"));
    p.setColor(QPalette::Light, QColor("#111"));

    QApplication::setPalette(p);

    QString windowColor = currentTheme->color("WindowBackground").name();
    QString windowForegroundColor = currentTheme->color("WindowForeground").name();
    QString logInfoForegroundColor = currentTheme->color("LogInfoForeground").name();
    QString paneColor = currentTheme->color("PaneBackground").name();
    QString windowBorder = currentTheme->color("WindowBorder").name();
    QString selectedTab = "deeppink";

    QString buttonStyling = QString("QPushButton{padding: 5px; background-color: deeppink; border-radius: 3px; border-color: #808080; border-width: 2px;} QPushButton::pressed{background-color: white; color: #808080; }");

    QString splitterStyling =    QString("QSplitter::handle:vertical{height: 6px; image: url(images/vsplitter.png);} QSplitter::handle:horizontal {width:  6px; image: url(images/hsplitter.png);}");
    QString scrollStyling =      QString("QScrollBar::add-line:horizontal, QScrollBar::add-line:vertical {border: 0px;} QScrollBar::sub-line:horizontal,QScrollBar::sub-line:vertical{border:0px;} QScrollBar:horizontal, QScrollBar:vertical{background-color: #222; border-right: 1px solid #000; border-bottom: 1px solid #000;} QScrollBar::handle:horizontal,QScrollBar::handle:vertical { background: %1;  border-radius: 5px; min-width: 80%;} QScrollBar::add-page:horizontal, QScrollBar::sub-page:horizontal,  QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical{background: none;}").arg(windowColor);

    QString tabStyling =         QString("QTabBar::tab{background: #1c2529; color: %1;} QTabBar::tab:selected{background: %2;} QTabWidget::tab-bar{alignment: center;} QTabWidget::pane{border: 0px;}").arg(windowForegroundColor, selectedTab);
    QString widgetTitleStyling = QString("QDockWidget::title{color: %3; border-bottom: 1px solid %2; text-align: center; background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 %1, stop: 1.0 #1c2529); font-size 10px;} QDockWidget{font-size:10px;} ").arg(windowColor, windowBorder, windowForegroundColor);
    QString toolTipStyling =     QString("QToolTip {color: #ffffff; background-color: #929292; border: 0px;} ");

    this->setStyleSheet(QString(buttonStyling + splitterStyling+ toolTipStyling+scrollStyling + "QToolButton:hover{background: transparent;} QSlider::groove:vertical{margin: 2px 0; background: dodgerblue; border-radius: 3px;} QSlider::handle:vertical {border: 1px solid #222; border-radius: 3px; height: 30px; background: #333;} QMenu{background: #929292; color: #000; } QMenu:selected{background: deeppink;} QMainWindow::separator{border: 1px solid %1;} QMainWindow{background-color: %1; color: white;}").arg(windowColor));
    statusBar()->setStyleSheet( QString("QWidget{background-color: %1; color: #808080;} QStatusBar{background-color: %1; border-top: 1px solid %2;}").arg(windowColor, windowBorder));
    // Colour log messages as info by default
    outputPane->setStyleSheet(  QString("QPlainTextEdit{background-color: %1; color: %2; border: 0px;}").arg(paneColor, logInfoForegroundColor));
    outputWidget->setStyleSheet(widgetTitleStyling);
    prefsWidget->setStyleSheet( QString(widgetTitleStyling + "QGroupBox:title{subcontrol-origin: margin; top:0px; padding: 0px 0 20px 5px; font-size: 11px; color: %1; background-color: transparent;} QGroupBox{padding: 0 0 0 0; subcontrol-origin: margin; margin-top: 15px; margin-bottom: 0px; font-size: 11px; background-color:#1c2325; border: 1px solid #1c2529; color: %1;} QWidget{background-color: %2;}" + buttonStyling).arg(windowForegroundColor, windowColor));
    tabs->setStyleSheet(tabStyling);
    prefTabs->setStyleSheet(tabStyling);
    docsCentral->setStyleSheet(tabStyling);
    docWidget->setStyleSheet(   QString(widgetTitleStyling + "QDockWidget QListView {color: %2; background: %1; selection-background-color: deeppink;}").arg(paneColor, windowForegroundColor));
    docPane->setStyleSheet(     QString("QTextBrowser { selection-color: white; selection-background-color: deeppink; padding-left:10; padding-top:10; padding-bottom:10; padding-right:10 ; background: %1}").arg(paneColor));
    infoWidg->setStyleSheet(    QString(scrollStyling + tabStyling + " QTextEdit{background-color: %1;}").arg(paneColor));
    toolBar->setStyleSheet(     QString("QToolBar{background-color: %1; border-bottom: 1px solid %2;}").arg(windowColor,windowBorder));
    errorPane->setStyleSheet(   QString("QTextEdit{background-color: %1;} .error-background{background-color: %2} ").arg(paneColor, currentTheme->color("ErrorBackground").name()));
    for(int i=0; i < tabs->count(); i++){
      SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
      ws->setFrameShape(QFrame::NoFrame);
    }

    foreach(QTextBrowser* pane, infoPanes) {
      pane->setStyleSheet(QString(scrollStyling + "QTextBrowser{ padding-left:10; padding-top:10; padding-bottom:10; padding-right:10;}"));
    }

  }else{

    currentTheme->lightMode();
    // clear stylesheets
    this->setStyleSheet("");
    infoWidg->setStyleSheet("");
    mainWidget->setStyleSheet("");
    statusBar()->setStyleSheet("");
    outputPane->setStyleSheet("");
    outputWidget->setStyleSheet("");
    prefsWidget->setStyleSheet("");
    tabs->setStyleSheet("");
    prefTabs->setStyleSheet("");
    docsCentral->setStyleSheet("");
    docWidget->setStyleSheet("");
    toolBar->setStyleSheet("");

    docPane->setStyleSheet(defaultTextBrowserStyle);

    QString l_buttonStyling = QString("QPushButton{background-color: deeppink; border-radius: 3px; padding: 5px; color: white; border-color: white; border-width: 2px;} QPushButton::pressed{background-color: white; color: #808080; }");

    QString l_windowColor = currentTheme->color("WindowBackground").name();
    QString l_windowForegroundColor = currentTheme->color("WindowForeground").name();
    QString l_foregroundColor = currentTheme->color("Foreground").name();
    QString l_logInfoForegroundColor = currentTheme->color("LogInfoForeground").name();
    QString l_paneColor = currentTheme->color("PaneBackground").name();
    QString l_windowBorder = currentTheme->color("WindowBorder").name();
    QString l_selectedTab = "deeppink";
    QString l_toolTipStyling =     QString("QToolTip {color: #ffffff; background-color: #929292; border: 0px;} ");

    QString l_tabStyling =         QString("QTabBar::tab{background: #808080; color: white;} QTabBar::tab:selected{background: %1;} QTabWidget::tab-bar{alignment: center;} QTabWidget::pane{border: 0px;}").arg(l_selectedTab);

    QString l_splitterStyling =    QString("QSplitter::handle:vertical{height: 6px; image: url(images/vsplitter.png);} QSplitter::handle:horizontal {width:  6px; image: url(images/hsplitter.png);}");

    QString l_scrollStyling =      QString("QScrollBar::add-line:horizontal, QScrollBar::add-line:vertical {border: 0px;} QScrollBar::sub-line:horizontal,QScrollBar::sub-line:vertical{border:0px;} QScrollBar:horizontal, QScrollBar:vertical{background-color: lightgray; border-right: 1px solid lightgray; border-bottom: 1px solid lightgray;} QScrollBar::handle:horizontal,QScrollBar::handle:vertical { background: %1;  border-radius: 5px; min-width: 80%;} QScrollBar::add-page:horizontal, QScrollBar::sub-page:horizontal,  QScrollBar::add-page:vertical, QScrollBar::sub-page:vertical{background: none;}").arg(l_windowColor);

    QString l_widgetTitleStyling = QString("QDockWidget::title{color: %3; border-bottom: 1px solid %2; text-align: center; background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 %1, stop: 1.0 lightgray); font-size 10px;} QDockWidget{font-size:10px;} ").arg(l_windowColor, l_windowBorder, l_windowForegroundColor);


    for(int i=0; i < tabs->count(); i++){
      SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
      ws->setFrameShape(QFrame::StyledPanel);
      ws->setStyleSheet("");
    }

    QPalette p = QApplication::palette();
    p.setColor(QPalette::WindowText,      currentTheme->color("WindowForeground"));
    p.setColor(QPalette::Window,          currentTheme->color("WindowBackground"));
    p.setColor(QPalette::Base,            QColor("#fff"));
    p.setColor(QPalette::Text,            currentTheme->color("WindowForeground"));
    p.setColor(QPalette::HighlightedText, currentTheme->color("HighlightedForeground"));
    p.setColor(QPalette::Highlight,       currentTheme->color("HighlightedBackground"));

    p.setColor(QPalette::AlternateBase,   QColor("#fff"));
    p.setColor(QPalette::ToolTipBase,   QColor("#fff"));
    p.setColor(QPalette::ToolTipText,   QColor("#000"));
    p.setColor(QPalette::Button,        QColor("#ddd"));
    p.setColor(QPalette::ButtonText,    QColor("#000"));
    p.setColor(QPalette::Shadow,        QColor("#fff"));
    p.setColor(QPalette::Dark, QColor("#000"));
    p.setColor(QPalette::Midlight, QColor("grey"));
    p.setColor(QPalette::Light, QColor("#fff"));

    QApplication::setPalette(p);

    this->setStyleSheet(QString(l_buttonStyling + l_splitterStyling+ l_toolTipStyling+l_scrollStyling + "QSlider::groove:vertical{margin: 2px 0; background: dodgerblue; border-radius: 3px;} QSlider::handle:vertical {border: 1px solid #222; border-radius: 3px; height: 30px; background: #333;} QMenu{background: #929292; color: #000; } QMenu:selected{background: deeppink;} QMainWindow::separator{border: 1px solid %1;} QMainWindow{background-color: %1; color: white;}").arg(l_windowColor));

    statusBar()->setStyleSheet( QString("QStatusBar{background-color: %1; border-top: 1px solid %2;}").arg(l_windowColor, l_windowBorder));
    // Colour log messages as info by default
    outputPane->setStyleSheet(  QString("QPlainTextEdit{background-color: %1; color: %2; border: 0px;}").arg(l_paneColor, l_logInfoForegroundColor));
    outputWidget->setStyleSheet(l_widgetTitleStyling);
    prefsWidget->setStyleSheet( QString(l_buttonStyling + l_widgetTitleStyling + "QGroupBox:title{subcontrol-origin: margin; top:0px; padding: 0px 0 20px 5px; font-size: 11px; color: %1; background-color: transparent;} QGroupBox{padding: 0 0 0 0; subcontrol-origin: margin; margin-top: 15px; margin-bottom: 0px; font-size: 11px; background-color: %2; border: 1px solid lightgray; color: %1;}").arg(l_windowForegroundColor, l_windowColor));
    tabs->setStyleSheet(        l_tabStyling);
    prefTabs->setStyleSheet(l_tabStyling);
    docsCentral->setStyleSheet( l_tabStyling);
    docWidget->setStyleSheet(   QString(l_widgetTitleStyling + "QDockWidget QListView {color: %2; background: %1; selection-color: white; selection-background-color: deeppink;}").arg(l_paneColor,  l_foregroundColor));
    docPane->setStyleSheet(     QString("QTextBrowser { selection-color: white; selection-background-color: deeppink; padding-left:10; padding-top:10; padding-bottom:10; padding-right:10 ; background: %1}").arg(l_paneColor));
    infoWidg->setStyleSheet(    QString(l_scrollStyling + l_tabStyling + " QTextEdit{background-color: %1;}").arg(l_paneColor));
    toolBar->setStyleSheet(     QString("QToolBar{background-color: %1; border-bottom: 1px solid %2;}").arg(l_windowColor,l_windowBorder));
    errorPane->setStyleSheet(   QString("QTextEdit{background-color: %1;} .error-background{background-color: %2} ").arg(l_paneColor, currentTheme->color("ErrorBackground").name()));


    for(int i=0; i < tabs->count(); i++){
      SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
      ws->setFrameShape(QFrame::NoFrame);
    }

    foreach(QTextBrowser* pane, infoPanes) {
      pane->setStyleSheet(defaultTextBrowserStyle);
    }
  }

  for(int i=0; i < tabs->count(); i++){
    SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
    ws->redraw();
  }

  lexer->unhighlightAll();
}

void MainWindow::changeShowLineNumbers(){
  for(int i=0; i < tabs->count(); i++){
    SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
    if (show_line_numbers->isChecked()){

      ws->showLineNumbers();
    } else {
      ws->hideLineNumbers();
    }
  }
}

void MainWindow::setRPSystemAudioHeadphones()
{
#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing
#else
  //assuming Raspberry Pi
  statusBar()->showMessage(tr("Switching To Headphone Audio Output..."), 2000);
  QProcess *p = new QProcess();
  QString prog = "amixer cset numid=3 1";
  p->start(prog);
#endif
}

void MainWindow::setRPSystemAudioHDMI()
{

#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing
#else
  //assuming Raspberry Pi
  statusBar()->showMessage(tr("Switching To HDMI Audio Output..."), 2000);
  QProcess *p = new QProcess();
  QString prog = "amixer cset numid=3 2";
  p->start(prog);
#endif
}

void MainWindow::setRPSystemAudioAuto()
{
#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing
#else
  //assuming Raspberry Pi
  statusBar()->showMessage(tr("Switching To Default Audio Output..."), 2000);
  QProcess *p = new QProcess();
  QString prog = "amixer cset numid=3 0";
  p->start(prog);
#endif
}

void MainWindow::showPrefsPane()
{
  if(prefsWidget->isVisible()) {
    prefsWidget->hide();
  } else {
    prefsWidget->show();
  }
}

void MainWindow::wheelEvent(QWheelEvent *event)
{
#if defined(Q_OS_WIN)
  if (event->modifiers() & Qt::ControlModifier) {
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

char MainWindow::int2char(int i){
  return '0' + i;
}

 QString MainWindow::tooltipStrShiftMeta(char key, QString str) {
#ifdef Q_OS_MAC
    return QString("%1 (⇧⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (Shift-alt-%2)").arg(str).arg(key);
#endif
}

 QString MainWindow::tooltipStrMeta(char key, QString str) {
#ifdef Q_OS_MAC
    return QString("%1 (⌘%2)").arg(str).arg(key);
#else
    return QString("%1 (alt-%2)").arg(str).arg(key);
#endif
}


// set tooltips, connect event handlers, and add shortcut if applicable
void MainWindow::setupAction(QAction *action, char key, QString tooltip,
			     const char *slot)
{
  QString shortcut, tooltipKey;
  tooltipKey = tooltip;
  if (key != 0) {
    tooltipKey = tooltipStrMeta(key, tooltip);
  }

  action->setToolTip(tooltipKey);
  action->setStatusTip(tooltip);
  connect(action, SIGNAL(triggered()), this, slot);

  if (key != 0) {
    // create a QShortcut instead of setting the QAction's shortcut
    // so it will still be active with the toolbar hidden
    new QShortcut(metaKey(key), this, slot);
  }
}

void MainWindow::createShortcuts()
{

  new QShortcut(metaKey('{'), this, SLOT(tabPrev()));
  new QShortcut(metaKey('}'), this, SLOT(tabNext()));
  //new QShortcut(metaKey('U'), this, SLOT(reloadServerCode()));

  new QShortcut(QKeySequence("F9"), this, SLOT(toggleButtonVisibility()));
  new QShortcut(shiftMetaKey('B'), this, SLOT(toggleButtonVisibility()));
  new QShortcut(QKeySequence("F10"), this, SLOT(toggleFocusMode()));
  new QShortcut(shiftMetaKey('F'), this, SLOT(toggleFullScreenMode()));
  new QShortcut(shiftMetaKey('M'), this, SLOT(toggleDarkMode()));
  new QShortcut(QKeySequence("F11"), this, SLOT(toggleLogVisibility()));
  new QShortcut(shiftMetaKey('L'), this, SLOT(toggleLogVisibility()));
}

void MainWindow::createToolBar()
{
  // Run
  QAction *runAct = new QAction(QIcon(":/images/run.png"), tr("Run"), this);
  setupAction(runAct, 'R', tr("Run the code in the current buffer"),
	      SLOT(runCode()));
  new QShortcut(QKeySequence(metaKeyModifier() + Qt::Key_Return), this, SLOT(runCode()));

  // Stop
  QAction *stopAct = new QAction(QIcon(":/images/stop.png"), tr("Stop"), this);
  setupAction(stopAct, 'S', tr("Stop all running code"), SLOT(stopCode()));

  // Save
  QAction *saveAsAct = new QAction(QIcon(":/images/save.png"), tr("Save As..."), this);
  setupAction(saveAsAct, 0, tr("Save current buffer as an external file"), SLOT(saveAs()));

  // Info
  QAction *infoAct = new QAction(QIcon(":/images/info.png"), tr("Info"), this);
  setupAction(infoAct, 0, tr("See information about Sonic Pi"),
	      SLOT(about()));

  // Help
  QAction *helpAct = new QAction(QIcon(":/images/help.png"), tr("Help"), this);
  setupAction(helpAct, 'I', tr("Toggle help pane"), SLOT(help()));

  // Preferences
  QAction *prefsAct = new QAction(QIcon(":/images/prefs.png"), tr("Prefs"), this);
  setupAction(prefsAct, 'P', tr("Toggle preferences pane"),
	      SLOT(showPrefsPane()));

  // Record
  recAct = new QAction(QIcon(":/images/rec.png"), tr("Start Recording"), this);
  setupAction(recAct, 0, tr("Start recording to WAV audio file"), SLOT(toggleRecording()));

  // Align
  QAction *textAlignAct = new QAction(QIcon(":/images/align.png"),
			     tr("Auto-Align Text"), this);
  setupAction(textAlignAct, 'M', tr("Improve readability of code"), SLOT(beautifyCode()));

  // Font Size Increase
  QAction *textIncAct = new QAction(QIcon(":/images/size_up.png"),
			    tr("Increase Text Size"), this);
  setupAction(textIncAct, 0, tr("Increase Text Size"), SLOT(zoomCurrentWorkspaceIn()));

  // Font Size Decrease
  QAction *textDecAct = new QAction(QIcon(":/images/size_down.png"),
			    tr("Decrease Text Size"), this);
  setupAction(textDecAct, 0, tr("Decrease Text Size"), SLOT(zoomCurrentWorkspaceOut()));

  QWidget *spacer = new QWidget();
  spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

  toolBar = addToolBar(tr("Tools"));
  toolBar->setObjectName("toolbar");
  toolBar->setIconSize(QSize(270/3, 111/3));
  toolBar->addAction(runAct);
  toolBar->addAction(stopAct);

  toolBar->addAction(saveAsAct);
  toolBar->addAction(recAct);
  toolBar->addWidget(spacer);

  toolBar->addAction(textDecAct);
  toolBar->addAction(textIncAct);
  dynamic_cast<QToolButton*>(toolBar->widgetForAction(textDecAct))->setAutoRepeat(true);
  dynamic_cast<QToolButton*>(toolBar->widgetForAction(textIncAct))->setAutoRepeat(true);

  toolBar->addAction(textAlignAct);

  toolBar->addAction(infoAct);
  toolBar->addAction(helpAct);
  toolBar->addAction(prefsAct);
}

QString MainWindow::readFile(QString name)
{
  QFile file(name);
  if (!file.open(QFile::ReadOnly | QFile::Text)) {
    std::cerr << "[GUI] - could not open file " << name.toStdString() << "\n";
    return "";
  }

  QTextStream st(&file);
  st.setCodec("UTF-8");
  return st.readAll();
}

void MainWindow::createInfoPane() {
  QTabWidget* infoTabs = new QTabWidget(this);

  QStringList urls, tabs;

  urls << "qrc:///html/info.html"
       << "qrc:///info/CORETEAM.html"
       << "qrc:///info/CONTRIBUTORS.html"
       << "qrc:///info/COMMUNITY.html"
       << "qrc:///info/LICENSE.html"
       << "qrc:///info/CHANGELOG.html";

  tabs << tr("About")
       << tr("Core Team")
       << tr("Contributors")
       << tr("Community")
       << tr("License")
       << tr("History");

  for (int t=0; t < urls.size(); t++) {
    QTextBrowser *pane = new QTextBrowser;
    infoPanes.append(pane);
    addUniversalCopyShortcuts(pane);
    pane->setOpenExternalLinks(true);
    pane->setFixedSize(600, 615);
    pane->setSource(QUrl(urls[t]));
    infoTabs->addTab(pane, tabs[t]);
  }

  infoTabs->setTabPosition(QTabWidget::South);

  QBoxLayout *infoLayout = new QBoxLayout(QBoxLayout::LeftToRight);
  infoLayout->addWidget(infoTabs);

  infoWidg = new QWidget;
  infoWidg->setWindowIcon(QIcon(":images/icon-smaller.png"));
  infoWidg->setLayout(infoLayout);
  infoWidg->setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
  infoWidg->setWindowTitle(tr("Sonic Pi - Info"));

  QAction *closeInfoAct = new QAction(this);
  closeInfoAct->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_W));
  connect(closeInfoAct, SIGNAL(triggered()), this, SLOT(about()));
  infoWidg->addAction(closeInfoAct);
}

void MainWindow::toggleRecordingOnIcon() {
  show_rec_icon_a = !show_rec_icon_a;
  if(show_rec_icon_a) {
    recAct->setIcon(QIcon(":/images/recording_a.png"));
  } else {
    recAct->setIcon(QIcon(":/images/recording_b.png"));
  }
}

void MainWindow::toggleRecording() {
  is_recording = !is_recording;
  if(is_recording) {
    recAct->setStatusTip(tr("Stop Recording"));
    recAct->setToolTip(tr("Stop Recording"));
    rec_flash_timer->start(500);
    Message msg("/start-recording");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
  } else {
    rec_flash_timer->stop();
    recAct->setStatusTip(tr("Start Recording"));
    recAct->setToolTip(tr("Start Recording"));
    recAct->setIcon(QIcon(":/images/rec.png"));
    Message msg("/stop-recording");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), QDir::homePath() + "/Desktop/my-recording.wav");
    if (!fileName.isEmpty()) {
      Message msg("/save-recording");
      msg.pushStr(guiID.toStdString());
      msg.pushStr(fileName.toStdString());
      sendOSC(msg);
    } else {
      Message msg("/delete-recording");
      msg.pushStr(guiID.toStdString());
      sendOSC(msg);
    }
  }
}


void MainWindow::createStatusBar()
{
  versionLabel = new QLabel(this);
  versionLabel->setText("Sonic Pi");
  statusBar()->showMessage(tr("Ready..."));
  statusBar()->addPermanentWidget(versionLabel);
}

void MainWindow::readSettings() {
  // Pref settings are read in MainWindow::initPrefsWindow()

  QSettings settings("uk.ac.cam.cl", "Sonic Pi");
  QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
  QSize size = settings.value("size", QSize(400, 400)).toSize();
  resize(size);
  move(pos);

  int index = settings.value("workspace", 0).toInt();
  if (index < tabs->count())
    tabs->setCurrentIndex(index);

  for (int w=0; w < workspace_max; w++) {
    // default zoom is 13
    int zoom = settings.value(QString("workspace%1zoom").arg(w), 13)
      .toInt();
    if (zoom < -5) zoom = -5;
    if (zoom > 20) zoom = 20;

    workspaces[w]->setProperty("zoom", QVariant(zoom));
    workspaces[w]->zoomTo(zoom);
  }

  docsplit->restoreState(settings.value("docsplitState").toByteArray());

  restoreState(settings.value("windowState").toByteArray());
  restoreGeometry(settings.value("windowGeom").toByteArray());

}

void MainWindow::writeSettings()
{
  QSettings settings("uk.ac.cam.cl", "Sonic Pi");
  settings.setValue("pos", pos());
  settings.setValue("size", size());
  settings.setValue("first_time", 0);


  settings.setValue("prefs/check-args", check_args->isChecked());
  settings.setValue("prefs/print-output", print_output->isChecked());
  settings.setValue("prefs/clear-output-on-run", clear_output_on_run->isChecked());
  settings.setValue("prefs/log-cues", log_cues->isChecked());
  settings.setValue("prefs/show-line-numbers", show_line_numbers->isChecked());
  settings.setValue("prefs/dark-mode", dark_mode->isChecked());
  settings.setValue("prefs/mixer-force-mono", mixer_force_mono->isChecked());
  settings.setValue("prefs/mixer-invert-stereo", mixer_invert_stereo->isChecked());

  settings.setValue("prefs/rp/force-audio-default", rp_force_audio_default->isChecked());
  settings.setValue("prefs/rp/force-audio-headphones", rp_force_audio_headphones->isChecked());
  settings.setValue("prefs/rp/force-audio-hdmi", rp_force_audio_hdmi->isChecked());
  settings.setValue("prefs/rp/system-vol", rp_system_vol->value());

  settings.setValue("prefs/rp/check-updates", check_updates->isChecked());
  settings.setValue("prefs/auto-indent-on-run", auto_indent_on_run->isChecked());
  settings.setValue("prefs/gui_transparency", gui_transparency_slider->value());
  settings.setValue("workspace", tabs->currentIndex());

  for (int w=0; w < workspace_max; w++) {
    settings.setValue(QString("workspace%1zoom").arg(w),
		      workspaces[w]->property("zoom"));
  }

  settings.setValue("docsplitState", docsplit->saveState());
  settings.setValue("windowState", saveState());
  settings.setValue("windowGeom", saveGeometry());
}

void MainWindow::loadFile(const QString &fileName, SonicPiScintilla* &text)
{
  QFile file(fileName);
  if (!file.open(QFile::ReadOnly)) {
    QMessageBox::warning(this, tr("Sonic Pi"),
			 tr("Cannot read file %1:\n%2.")
			 .arg(fileName)
			 .arg(file.errorString()));
    return;
  }

  QTextStream in(&file);
  QApplication::setOverrideCursor(Qt::WaitCursor);
  text->setText(in.readAll());
  QApplication::restoreOverrideCursor();
  statusBar()->showMessage(tr("File loaded..."), 2000);
}

bool MainWindow::saveFile(const QString &fileName, SonicPiScintilla* text)
{
  QFile file(fileName);
  if (!file.open(QFile::WriteOnly)) {
    QMessageBox::warning(this, tr("Sonic Pi"),
			 tr("Cannot write file %1:\n%2.")
			 .arg(fileName)
			 .arg(file.errorString()));
    return false;
  }

  QTextStream out(&file);

  QApplication::setOverrideCursor(Qt::WaitCursor);
  QString code = text->text();
#if defined(Q_OS_WIN)
  code.replace("\n", "\r\n"); // CRLF for Windows users
  code.replace("\r\r\n", "\r\n"); // don't double-replace if already encoded
#endif
  out << code;
  QApplication::restoreOverrideCursor();

  statusBar()->showMessage(tr("File saved..."), 2000);
  return true;
}

SonicPiScintilla* MainWindow::filenameToWorkspace(std::string filename)
{
  std::string s;

  for(int i = 0; i < workspace_max; i++) {
    s = "workspace_" + number_name(i);
    if(filename == s) {
      return workspaces[i];
    }
  }
  return workspaces[0];
}

void MainWindow::onExitCleanup()
{
  setupLogPathAndRedirectStdOut();
  if(serverProcess->state() == QProcess::NotRunning) {
    std::cout << "[GUI] - warning, server process is not running." << std::endl;
    sonicPiServer->stopServer();
    if(protocol == TCP){
      clientSock->close();
    }
  } else {
    if (loaded_workspaces)
      saveWorkspaces();
    sleep(1);
    std::cout << "[GUI] - asking server process to exit..." << std::endl;
    Message msg("/exit");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
  }
  if(protocol == UDP){
    osc_thread.waitForFinished();
  }
  std::cout << "[GUI] - exiting. Cheerio :-)" << std::endl;
  std::cout.rdbuf(coutbuf); // reset to stdout before exiting
}

void MainWindow::heartbeatOSC() {
  Message msg("/gui-heartbeat");
  msg.pushStr(guiID.toStdString());
  sendOSC(msg);
}


void MainWindow::updateDocPane(QListWidgetItem *cur) {
  QString url = cur->data(32).toString();
  docPane->setSource(QUrl(url));
}

void MainWindow::updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev) {
  (void)prev;
  updateDocPane(cur);
}

void MainWindow::addHelpPage(QListWidget *nameList,
                             struct help_page *helpPages, int len) {
  int i;
  struct help_entry entry;
  entry.pageIndex = docsCentral->count()-1;

  for(i = 0; i < len; i++) {
    QListWidgetItem *item = new QListWidgetItem(helpPages[i].title);
    item->setData(32, QVariant(helpPages[i].url));
    item->setSizeHint(QSize(item->sizeHint().width(), 25));
    nameList->addItem(item);
    entry.entryIndex = nameList->count()-1;

    if (helpPages[i].keyword != NULL) {
      helpKeywords.insert(helpPages[i].keyword, entry);
      // magic numbers ahoy
      // to be revamped along with the help system
      switch (entry.pageIndex) {
      case 2:
        autocomplete->addSymbol(SonicPiAPIs::Synth, helpPages[i].keyword);
        break;
      case 3:
        autocomplete->addSymbol(SonicPiAPIs::FX, helpPages[i].keyword);
        break;
      case 5:
        autocomplete->addKeyword(SonicPiAPIs::Func, helpPages[i].keyword);
        break;
      }
    }
  }
}

QListWidget *MainWindow::createHelpTab(QString name) {
  QListWidget *nameList = new QListWidget;
  connect(nameList,
	  SIGNAL(itemPressed(QListWidgetItem*)),
	  this, SLOT(updateDocPane(QListWidgetItem*)));
  connect(nameList,
	  SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
	  this, SLOT(updateDocPane2(QListWidgetItem*, QListWidgetItem*)));

  QShortcut *up = new QShortcut(ctrlKey('p'), nameList);
  up->setContext(Qt::WidgetShortcut);
  connect(up, SIGNAL(activated()), this, SLOT(helpScrollUp()));
  QShortcut *down = new QShortcut(ctrlKey('n'), nameList);
  down->setContext(Qt::WidgetShortcut);
  connect(down, SIGNAL(activated()), this, SLOT(helpScrollDown()));

  QBoxLayout *layout = new QBoxLayout(QBoxLayout::LeftToRight);
  layout->addWidget(nameList);
  layout->setStretch(1, 1);
  QWidget *tabWidget = new QWidget;
  tabWidget->setLayout(layout);
  docsCentral->addTab(tabWidget, name);
  helpLists.append(nameList);
  return nameList;
}

void MainWindow::helpScrollUp() {
  int section = docsCentral->currentIndex();
  int entry = helpLists[section]->currentRow();

  if (entry > 0)
    entry--;
  helpLists[section]->setCurrentRow(entry);
}

void MainWindow::helpScrollDown() {
  int section = docsCentral->currentIndex();
  int entry = helpLists[section]->currentRow();

  if (entry < helpLists[section]->count()-1)
    entry++;
  helpLists[section]->setCurrentRow(entry);
}

void MainWindow::docScrollUp() {
  docPane->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepSub);
}

void MainWindow::docScrollDown() {
  docPane->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepAdd);
}

void MainWindow::helpClosed(bool visible) {
  if (visible) return;
  // redock on close
  if (!hidingDocPane)
    docWidget->setFloating(false);
  hidingDocPane = false;
}

void MainWindow::tabNext() {
  int index = tabs->currentIndex();
  if (index == tabs->count()-1)
    index = 0;
  else
    index++;
  QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::tabPrev() {
  int index = tabs->currentIndex();
  if (index == 0)
    index = tabs->count() - 1;
  else
    index--;
  QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, index));
}

void MainWindow::setLineMarkerinCurrentWorkspace(int num) {
  if(num > 0) {
    SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();
    ws->setLineErrorMarker(num - 1);
  }
}

void MainWindow::setUpdateInfoText(QString t) {
  update_info->setText(t);
}

void MainWindow::addUniversalCopyShortcuts(QTextEdit *te){
  new QShortcut(ctrlKey('c'), te, SLOT(copy()));
  new QShortcut(ctrlKey('a'), te, SLOT(selectAll()));

  new QShortcut(metaKey('c'), te, SLOT(copy()));
  new QShortcut(metaKey('a'), te, SLOT(selectAll()));
}

QString MainWindow::asciiArtLogo(){
  return readFile(":/images/logo.txt");
}

void MainWindow::printAsciiArtLogo(){
  QString s = asciiArtLogo();
  std::cout << std::endl << std::endl << std::endl;
#if QT_VERSION >= 0x050400
  qDebug().noquote() << s;
  std::cout << std::endl << std::endl;
#else
  //noquote requires QT 5.4
  qDebug() << s;
  std::cout << std::endl;
#endif
}

void MainWindow::requestVersion() {
    Message msg("/version");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
}

void MainWindow::open_sonic_pi_net() {
  QDesktopServices::openUrl(QUrl("http://sonic-pi.net", QUrl::TolerantMode));
}

void MainWindow::updateVersionNumber(QString v, int v_num,QString latest_v, int latest_v_num, QDate last_checked, QString platform) {
  version = v;
  version_num = v_num;
  latest_version = latest_v;
  latest_version_num = latest_v_num;

  // update status bar
  versionLabel->setText(QString("Sonic Pi " + v + " on " + platform));

  // update preferences
  QString last_update_check = tr("Last checked %1").arg(last_checked.toString());

  QString preamble = tr("Sonic Pi checks for updates\nevery two weeks.");

  QString print_version = tr("This is Sonic Pi %1");
  QString new_version = tr("Version %2 is now available!");

  if(v_num < latest_v_num) {
    setUpdateInfoText(QString(preamble + "\n\n" + print_version + "\n\n" + new_version).arg(version, latest_version));
    visit_sonic_pi_net->setText(tr("New version available!\nGet Sonic Pi %1").arg(latest_version));
    check_updates_now->setVisible(false);
    visit_sonic_pi_net->setVisible(true);
  }
  else {
    setUpdateInfoText(QString(preamble + "\n\n" + print_version + "\n\n" + last_update_check).arg(version));
  }
}


void MainWindow::setupLogPathAndRedirectStdOut() {
  QString sp_user_path = QDir::homePath() + QDir::separator() + ".sonic-pi";
  log_path =  sp_user_path + QDir::separator() + "log";
  QDir().mkdir(sp_user_path);
  QDir().mkdir(log_path);

  coutbuf = std::cout.rdbuf();
  stdlog.open(QString(log_path + "/gui.log").toStdString().c_str());
  std::cout.rdbuf(stdlog.rdbuf());
}


bool MainWindow::eventFilter(QObject *obj, QEvent *evt)
{
    if(obj==qApp && ( evt->type() == QEvent::ApplicationActivate ))
    {
      statusBar()->showMessage(tr("Welcome back. Now get your live code on..."), 2000);
      update();
    }
    return QMainWindow::eventFilter(obj, evt);
}
#include "ruby_help.h"
