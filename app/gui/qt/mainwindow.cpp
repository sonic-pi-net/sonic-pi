//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
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

// Qt 5 only

// Qt stuff
#include <QSysInfo>
#include <QPainter>
#include <QDate>
#include <QDesktopServices>
#include <QDesktopWidget>


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
#include <QScrollBar>
#include <QSignalMapper>
#include <QSplitter>
#include <QComboBox>
#include <QButtonGroup>

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>

#include "sonicpilexer.h"
#include "sonicpiapis.h"
#include "sonicpiscintilla.h"
#include "sonicpitheme.h"

#include "oschandler.h"
#include "oscsender.h"
#include "sonicpilog.h"
#include "sonic_pi_udp_osc_server.h"
#include "sonic_pi_tcp_osc_server.h"
#include "infowidget.h"

// OSC stuff
#include "oscpkt.hh"
#include "udp.hh"
using namespace oscpkt;// OSC specific stuff

// Operating System Specific includes
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


#if QT_VERSION >= 0x050400
// Requires Qt5
  #include <QWindow>
#endif

#include "mainwindow.h"

#ifdef Q_OS_MAC
MainWindow::MainWindow(QApplication &app, bool i18n, QMainWindow* splash)
#else
MainWindow::MainWindow(QApplication &app, bool i18n, QSplashScreen* splash)
#endif
{



  QString root_path = rootPath();

#if defined(Q_OS_WIN)
  ruby_path = QDir::toNativeSeparators(root_path + "/app/server/native/ruby/bin/ruby.exe");
#elif defined(Q_OS_MAC)
  ruby_path = root_path + "/app/server/native/ruby/bin/ruby";
#else
  ruby_path = root_path + "/app/server/native/ruby/bin/ruby";
#endif

  QFile file(ruby_path);
  if(!file.exists()) {
    // fallback to user's locally installed ruby
    ruby_path = "ruby";
  }

  ruby_server_path = QDir::toNativeSeparators(root_path + "/app/server/ruby/bin/sonic-pi-server.rb");
  port_discovery_path = QDir::toNativeSeparators(root_path + "/app/server/ruby/bin/port-discovery.rb");
  sample_path = QDir::toNativeSeparators(root_path + "/etc/samples");

  sp_user_path           = QDir::toNativeSeparators(sonicPiHomePath() + "/.sonic-pi");
  sp_user_tmp_path       = QDir::toNativeSeparators(sp_user_path + "/.writableTesterPath");
  log_path               = QDir::toNativeSeparators(sp_user_path + "/log");
  server_error_log_path  = QDir::toNativeSeparators(log_path + "/server-errors.log");
  server_output_log_path = QDir::toNativeSeparators(log_path + "/server-output.log");
  gui_log_path           = QDir::toNativeSeparators(log_path + QDir::separator() + "gui.log");
  process_log_path       = QDir::toNativeSeparators(log_path + "/processes.log");
  scsynth_log_path       = QDir::toNativeSeparators(log_path + QDir::separator() + "scsynth.log");

  init_script_path        = QDir::toNativeSeparators(root_path + "/app/server/ruby/bin/init-script.rb");
  exit_script_path        = QDir::toNativeSeparators(root_path + "/app/server/ruby/bin/exit-script.rb");

  qt_app_theme_path     = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/app.qss");

  qt_browser_dark_css   = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/dark/doc-styles.css");
  qt_browser_light_css   = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/light/doc-styles.css");
  qt_browser_hc_css   = QDir::toNativeSeparators(root_path + "/app/gui/qt/theme/high_contrast/doc-styles.css");

  QDir logDir(log_path);
  logDir.mkpath(logDir.absolutePath());

  QFile tmpFile(sp_user_tmp_path);
  if (!tmpFile.open(QIODevice::WriteOnly)) {
    homeDirWritable = false;
  }
  else {
    homeDirWritable = true;
    tmpFile.close();
  }
  updated_dark_mode_for_help = false;
  updated_dark_mode_for_prefs = false;
  loaded_workspaces = false;
  is_recording = false;
  show_rec_icon_a = false;
  restoreDocPane = false;
  focusMode = false;
  version = "3.1.0";
  latest_version = "";
  version_num = 0;
  latest_version_num = 0;
  this->splash = splash;
  this->i18n = i18n;
  guiID = QUuid::createUuid().toString();
  QSettings settings("sonic-pi.net", "gui-settings");

  QThreadPool::globalInstance()->setMaxThreadCount(3);
  app.installEventFilter(this);
  app.processEvents();

  protocol = UDP;
  if(protocol == TCP){
    clientSock = new QTcpSocket(this);
  }

  std::cout << "Discovering port numbers..." << std::endl;
  QProcess* determinePortNumbers = new QProcess();
  QStringList send_args;
  send_args << port_discovery_path;
  determinePortNumbers->start(ruby_path, send_args);
  determinePortNumbers->waitForFinished();
  QTextStream stream(determinePortNumbers->readAllStandardOutput().trimmed());
  QString line = stream.readLine();
  while (!line.isNull()) {
    auto parts = line.split(": ");
    std::cout << "[GUI] - Port entry " << parts[0].trimmed().toStdString() << " : " << parts[1].trimmed().toStdString() << QString(" : %1").arg(parts[1].trimmed().toInt()).toStdString() << std::endl;
    port_map[parts[0].trimmed()] = parts[1].trimmed().toInt();
    line = stream.readLine();
  };

  gui_send_to_server_port   = port_map["gui-send-to-server"];
  gui_listen_to_server_port = port_map["gui-listen-to-server"];
  server_listen_to_gui_port = port_map["server-listen-to-gui"];
  server_osc_cues_port      = port_map["server-osc-cues"];
  server_send_to_gui_port   = port_map["server-send-to-gui"];
  scsynth_port              = port_map["scsynth"];
  scsynth_send_port         = port_map["scsynth-send"];
  erlang_router_port        = port_map["erlang-router"];
  osc_midi_out_port         = port_map["osc-midi-out"];
  osc_midi_in_port          = port_map["osc-midi-in"];
  websocket_port            = port_map["websocket"];

  oscSender = new OscSender(gui_send_to_server_port);
  printAsciiArtLogo();

  // Clear out old tasks from previous sessions if they still exist
  QProcess *initProcess = new QProcess();
  initProcess->start(ruby_path, QStringList(init_script_path));
  initProcess->waitForFinished();

  // Throw all stdout into ~/.sonic-pi/log/gui.log
  setupLogPathAndRedirectStdOut();


  std::cout << "[GUI] - Detecting port numbers..." << std::endl;


  std::cout << "[GUI] - GUI listen to server port "<< gui_listen_to_server_port << std::endl;
  checkPort(gui_listen_to_server_port);

  std::cout << "[GUI] - Server listen to gui port " << server_listen_to_gui_port << std::endl;
  checkPort(server_listen_to_gui_port);

  std::cout << "[GUI] - Server incoming OSC cues port " << server_osc_cues_port << std::endl;
  checkPort(server_osc_cues_port);

  std::cout << "[GUI] - Scsynth port " << scsynth_port << std::endl;
  checkPort(scsynth_port);

  std::cout << "[GUI] - Server send to GUI port " << server_send_to_gui_port << std::endl;
  checkPort(server_send_to_gui_port);

  std::cout << "[GUI] - GUI send to server port " << gui_send_to_server_port<< std::endl;
  checkPort(gui_send_to_server_port);

  std::cout << "[GUI] - Scsynth send port " << scsynth_send_port << std::endl;
  checkPort(scsynth_send_port);

  std::cout << "[GUI] - Erlang router port " << erlang_router_port << std::endl;
  checkPort(erlang_router_port);

  std::cout << "[GUI] - OSC MIDI out port " << osc_midi_out_port << std::endl;
  checkPort(osc_midi_out_port);

  std::cout << "[GUI] - OSC MIDI in port " << osc_midi_in_port << std::endl;
  checkPort(osc_midi_in_port);

  std::cout << "[GUI] - Websocket port " << websocket_port << std::endl;
  checkPort(websocket_port);

  std::cout << "[GUI] - Init script completed" << std::endl;

  setupTheme();
  lexer = new SonicPiLexer(theme);

  // load up pro icons into memory
  pro_run_icon = QIcon(":/images/toolbar/pro/run.png");
  pro_stop_icon = QIcon(":/images/toolbar/pro/stop.png");
  pro_save_icon = QIcon(":/images/toolbar/pro/save.png");
  pro_load_icon = QIcon(":/images/toolbar/pro/load.png");
  pro_rec_icon = QIcon(":/images/toolbar/pro/rec.png");
  pro_size_up_icon = QIcon(":/images/toolbar/pro/size-up.png");
  pro_size_down_icon = QIcon(":/images/toolbar/pro/size-down.png");
  pro_scope_bordered_icon = QIcon(":/images/toolbar/pro/scope-bordered.png");
  pro_scope_icon = QIcon(":/images/toolbar/pro/scope.png");
  pro_info_icon = QIcon(":/images/toolbar/pro/info.png");
  pro_info_bordered_icon = QIcon(":/images/toolbar/pro/info-bordered.png");
  pro_help_bordered_icon = QIcon(":/images/toolbar/pro/help-bordered.png");
  pro_help_icon = QIcon(":/images/toolbar/pro/help.png");
  pro_prefs_icon = QIcon(":/images/toolbar/pro/prefs.png");
  pro_prefs_bordered_icon = QIcon(":/images/toolbar/pro/prefs-bordered.png");
  pro_info_dark_bordered_icon = QIcon(":/images/toolbar/pro/info-dark-bordered.png");
  pro_info_dark_icon = QIcon(":/images/toolbar/pro/info-dark.png");
  pro_help_dark_bordered_icon = QIcon(":/images/toolbar/pro/help-dark-bordered.png");
  pro_help_dark_icon = QIcon(":/images/toolbar/pro/help-dark.png");
  pro_prefs_dark_bordered_icon = QIcon(":/images/toolbar/pro/prefs-dark-bordered.png");
  pro_prefs_dark_icon = QIcon(":/images/toolbar/pro/prefs-dark.png");
  pro_rec_b_icon = QIcon(":/images/toolbar/pro/recording-b.png");
  pro_rec_b_dark_icon = QIcon(":/images/toolbar/pro/recording-b-dark.png");
  pro_load_dark_icon = QIcon(":/images/toolbar/pro/load-dark.png");
  pro_save_dark_icon = QIcon(":/images/toolbar/pro/save-dark.png");

  default_light_run_icon = QIcon(":/images/toolbar/default/light-run.png");
  default_light_stop_icon = QIcon(":/images/toolbar/default/light-stop.png");
  default_light_save_icon = QIcon(":/images/toolbar/default/light-save.png");
  default_light_load_icon = QIcon(":/images/toolbar/default/light-load.png");
  default_light_rec_icon = QIcon(":/images/toolbar/default/light-rec.png");
  default_light_rec_a_icon = QIcon(":/images/toolbar/default/light-rec-a.png");
  default_light_rec_b_icon = QIcon(":/images/toolbar/default/light-rec-b.png");
  default_light_size_up_icon = QIcon(":/images/toolbar/default/light-size-up.png");
  default_light_size_down_icon = QIcon(":/images/toolbar/default/light-size-down.png");
  default_light_scope_icon = QIcon(":/images/toolbar/default/light-scope.png");
  default_light_scope_toggled_icon = QIcon(":/images/toolbar/default/light-scope-toggled.png");
  default_light_info_icon = QIcon(":/images/toolbar/default/light-info.png");
  default_light_info_toggled_icon = QIcon(":/images/toolbar/default/light-info-toggled.png");
  default_light_help_icon = QIcon(":/images/toolbar/default/light-help.png");
  default_light_help_toggled_icon = QIcon(":/images/toolbar/default/light-help-toggled.png");
  default_light_prefs_icon = QIcon(":/images/toolbar/default/light-prefs.png");
  default_light_prefs_toggled_icon = QIcon(":/images/toolbar/default/light-prefs-toggled.png");

    default_dark_run_icon = QIcon(":/images/toolbar/default/dark-run.png");
  default_dark_stop_icon = QIcon(":/images/toolbar/default/dark-stop.png");
  default_dark_save_icon = QIcon(":/images/toolbar/default/dark-save.png");
  default_dark_load_icon = QIcon(":/images/toolbar/default/dark-load.png");
  default_dark_rec_icon = QIcon(":/images/toolbar/default/dark-rec.png");
  default_dark_rec_a_icon = QIcon(":/images/toolbar/default/dark-rec-a.png");
  default_dark_rec_b_icon = QIcon(":/images/toolbar/default/dark-rec-b.png");
  default_dark_size_up_icon = QIcon(":/images/toolbar/default/dark-size-up.png");
  default_dark_size_down_icon = QIcon(":/images/toolbar/default/dark-size-down.png");
  default_dark_scope_icon = QIcon(":/images/toolbar/default/dark-scope.png");
  default_dark_scope_toggled_icon = QIcon(":/images/toolbar/default/dark-scope-toggled.png");
  default_dark_info_icon = QIcon(":/images/toolbar/default/dark-info.png");
  default_dark_info_toggled_icon = QIcon(":/images/toolbar/default/dark-info-toggled.png");
  default_dark_help_icon = QIcon(":/images/toolbar/default/dark-help.png");
  default_dark_help_toggled_icon = QIcon(":/images/toolbar/default/dark-help-toggled.png");
  default_dark_prefs_icon = QIcon(":/images/toolbar/default/dark-prefs.png");
  default_dark_prefs_toggled_icon = QIcon(":/images/toolbar/default/dark-prefs-toggled.png");

      default_hc_run_icon = QIcon(":/images/toolbar/default/hc-run.png");
  default_hc_stop_icon = QIcon(":/images/toolbar/default/hc-stop.png");
  default_hc_save_icon = QIcon(":/images/toolbar/default/hc-save.png");
  default_hc_load_icon = QIcon(":/images/toolbar/default/hc-load.png");
  default_hc_rec_icon = QIcon(":/images/toolbar/default/hc-rec.png");
  default_hc_rec_a_icon = QIcon(":/images/toolbar/default/hc-rec-a.png");
  default_hc_rec_b_icon = QIcon(":/images/toolbar/default/hc-rec-b.png");
  default_hc_size_up_icon = QIcon(":/images/toolbar/default/hc-size-up.png");
  default_hc_size_down_icon = QIcon(":/images/toolbar/default/hc-size-down.png");
  default_hc_scope_icon = QIcon(":/images/toolbar/default/hc-scope.png");
  default_hc_scope_toggled_icon = QIcon(":/images/toolbar/default/hc-scope-toggled.png");
  default_hc_info_icon = QIcon(":/images/toolbar/default/hc-info.png");
  default_hc_info_toggled_icon = QIcon(":/images/toolbar/default/hc-info-toggled.png");
  default_hc_help_icon = QIcon(":/images/toolbar/default/hc-help.png");
  default_hc_help_toggled_icon = QIcon(":/images/toolbar/default/hc-help-toggled.png");
  default_hc_prefs_icon = QIcon(":/images/toolbar/default/hc-prefs.png");
  default_hc_prefs_toggled_icon = QIcon(":/images/toolbar/default/hc-prefs-toggled.png");


  setupWindowStructure();
  createShortcuts();
  createToolBar();
  createStatusBar();
  createInfoPane();
  setWindowTitle(tr("Sonic Pi"));
  initPrefsWindow();
  readSettings();
  updateTabsVisibility();
  updateButtonVisibility();
  updateLogVisibility();
  updateIncomingOscLogVisibility();

  // The implementation of this method is dynamically generated and can
  // be found in ruby_help.h:
  initDocsWindow();

  //setup autocompletion
  autocomplete->loadSamples(sample_path);

  OscHandler* handler = new OscHandler(this, outputPane, incomingPane, theme);

  if(protocol == UDP){
    sonicPiOSCServer = new SonicPiUDPOSCServer(this, handler, gui_listen_to_server_port);
    osc_thread = QtConcurrent::run(sonicPiOSCServer, &SonicPiOSCServer::start);
  }
  else{
    sonicPiOSCServer = new SonicPiTCPOSCServer(this, handler);
    sonicPiOSCServer->start();
  }

  // Wait to hear back from the server before continuing
  startRubyServer();
  if (waitForServiceSync()){
    // We have a connection! Finish up loading app...
    honourPrefs();
    loadWorkspaces();
    requestVersion();
    toggleIcons();
    toggleScope();
    updatePrefsIcon();
    updateColourTheme();
    updateFullScreenMode();

    changeSystemPreAmp(system_vol_slider->value(), 1);
    connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );
    QTimer *timer = new QTimer(this);
    connect(timer, SIGNAL(timeout()), this, SLOT(heartbeatOSC()));
    timer->start(1000);

    splashClose();
    showWindow();
    showWelcomeScreen();
  }
}

void MainWindow::checkPort(int port) {
  oscpkt::UdpSocket sock;
  sock.bindTo(port);
  if ((port < 1024) || (!sock.isOk())) {
    std::cout << "[GUI] -    port: " << port << " [Not Available]" << std::endl;
  } else {
    std::cout << "[GUI] -    port: " << port << " [OK]" << std::endl;
  }
  sock.close();
}

void MainWindow::showWelcomeScreen() {
  QSettings settings("sonic-pi.net", "gui-settings");
if(settings.value("first_time", 1).toInt() == 1) {
    QTextBrowser* startupPane = new QTextBrowser;
    startupPane->setFixedSize(600, 615);
    startupPane->setWindowIcon(QIcon(":images/icon-smaller.png"));
    startupPane->setWindowTitle(tr("Welcome to Sonic Pi"));
    addUniversalCopyShortcuts(startupPane);
    startupPane->document()->setDefaultStyleSheet(readFile(":/theme/light/doc-styles.css"));
    startupPane->setSource(QUrl("qrc:///html/startup.html"));
    docWidget->show();
    startupPane->show();
  }
}

void MainWindow::setupTheme() {
  // Syntax highlighting
  QString themeFilename = QDir::homePath() + QDir::separator() + ".sonic-pi" + QDir::separator() + "theme.properties";
  this->theme = new SonicPiTheme(this, themeFilename);
}

void MainWindow::setupWindowStructure() {

  setUnifiedTitleAndToolBarOnMac(true);
  setWindowIcon(QIcon(":images/icon-smaller.png"));

  rec_flash_timer = new QTimer(this);
  connect(rec_flash_timer, SIGNAL(timeout()), this, SLOT(toggleRecordingOnIcon()));

  // Setup output and error panes

  outputPane = new SonicPiLog;
  incomingPane = new SonicPiLog;
  errorPane = new QTextBrowser;
  errorPane->setOpenExternalLinks(true);
  update_info = new QLabel(tr("Sonic Pi update info"));
  update_info->setWordWrap(true);

  // Window layout
  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);

  lexer->setAutoIndentStyle(SonicPiScintilla::AiMaintain);

  // create workspaces and add them to the tabs
  // workspace shortcuts
  signalMapper = new QSignalMapper (this) ;
  auto_indent_on_run = new QCheckBox(tr("Auto-align"));

  for(int ws = 0; ws < workspace_max; ws++) {
    std::string s;
    QString fileName = QString("workspace_" ) + QString::fromStdString(number_name(ws));

    //TODO: this is only here to ensure auto_indent_on_run is
    //      initialised before using it to construct the
    //      workspaces. Strongly consider how to clean this up in a way
    //      that nicely scales for more properties such as this.  This
    //      should only be considered an interim solution necessary to
    //      fix the return issue on Japanese keyboards.

    SonicPiScintilla *workspace = new SonicPiScintilla(lexer, theme, fileName, oscSender, auto_indent_on_run);

    workspace->setObjectName(QString("Buffer %1").arg(ws));

    //tab completion when in list
    QShortcut *indentLine = new QShortcut(QKeySequence("Tab"), workspace);
    connect (indentLine, SIGNAL(activated()), signalMapper, SLOT(map())) ;
    signalMapper -> setMapping (indentLine, (QObject*)workspace);

    // save and load buffers
    QShortcut *saveBufferShortcut = new QShortcut(shiftMetaKey('s'), workspace);
    connect (saveBufferShortcut, SIGNAL(activated()), this, SLOT(saveAs())) ;
    QShortcut *loadBufferShortcut = new QShortcut(shiftMetaKey('o'), workspace);
    connect (loadBufferShortcut, SIGNAL(activated()), this, SLOT(loadFile())) ;


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
    connect(escape, SIGNAL(activated()), this, SLOT(escapeWorkspaces()));
    connect(escape2, SIGNAL(activated()), this, SLOT(escapeWorkspaces()));

    //quick nav by jumping up and down 1 lines at a time
    QShortcut *forwardOneLine = new QShortcut(ctrlKey('p'), workspace);
    connect(forwardOneLine, SIGNAL(activated()), workspace, SLOT(forwardOneLine()));
    QShortcut *backOneLine = new QShortcut(ctrlKey('n'), workspace);
    connect(backOneLine, SIGNAL(activated()), workspace, SLOT(backOneLine()));

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

    QShortcut *cutToBufferLive = new QShortcut(ctrlKey(']'), workspace);
    connect(cutToBufferLive, SIGNAL(activated()), workspace, SLOT(sp_cut()));

    // Standard cut
    QShortcut *cutToBuffer = new QShortcut(ctrlKey('x'), workspace);
    connect(cutToBuffer, SIGNAL(activated()), workspace, SLOT(sp_cut()));

    // paste
    QShortcut *pasteToBufferWin = new QShortcut(ctrlKey('v'), workspace);
    connect(pasteToBufferWin, SIGNAL(activated()), workspace, SLOT(sp_paste()));
    QShortcut *pasteToBuffer = new QShortcut(metaKey('v'), workspace);
    connect(pasteToBuffer, SIGNAL(activated()), workspace, SLOT(sp_paste()));
    QShortcut *pasteToBufferEmacs = new QShortcut(ctrlKey('y'), workspace);
    connect(pasteToBufferEmacs, SIGNAL(activated()), workspace, SLOT(sp_paste()));

    //comment line
    QShortcut *toggleLineComment= new QShortcut(metaKey('/'), workspace);
    connect(toggleLineComment, SIGNAL(activated()), this, SLOT(toggleCommentInCurrentWorkspace()));

    //upcase next word
    QShortcut *upcaseWord= new QShortcut(metaKey('u'), workspace);
    connect(upcaseWord, SIGNAL(activated()), workspace, SLOT(upcaseWordOrSelection()));

    //downcase next word
    QShortcut *downcaseWord= new QShortcut(metaKey('l'), workspace);
    connect(downcaseWord, SIGNAL(activated()), workspace, SLOT(downcaseWordOrSelection()));

    QString w = QString(tr("| %1 |")).arg(QString::number(ws));
    workspaces[ws] = workspace;
    tabs->addTab(workspace, w);
  }

  connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(changeTab(int)));
  connect(signalMapper, SIGNAL(mapped(QObject*)), this, SLOT(completeSnippetListOrIndentLine(QObject*)));

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

  autocomplete = new SonicPiAPIs(lexer);
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
  incomingPane->setReadOnly(true);
  errorPane->setReadOnly(true);
  outputPane->setLineWrapMode(QPlainTextEdit::NoWrap);
  outputPane->setFontFamily("Hack");
  incomingPane->setLineWrapMode(QPlainTextEdit::NoWrap);
  incomingPane->setFontFamily("Hack");

  if(!theme->font("LogFace").isEmpty()){
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
  errorPane->setMaximumHeight(130);
  errorPane->setMinimumHeight(130);

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

  scopeWidget = new QDockWidget("",this);
  scopeWidget->setFocusPolicy(Qt::NoFocus);
  scopeWidget->setAllowedAreas(Qt::RightDockWidgetArea | Qt::BottomDockWidgetArea | Qt::TopDockWidgetArea);
  scopeWidget->setFeatures(QDockWidget::DockWidgetClosable | QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);
  scopeInterface = new Scope(scsynth_port);
  scopeInterface->pause();
  scopeWidget->setWidget(scopeInterface);
  scopeWidget->setObjectName("scope");
  addDockWidget(Qt::RightDockWidgetArea, scopeWidget);

  connect(scopeWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(scopeVisibilityChanged()));

  prefsWidget = new QDockWidget(tr("Preferences"), this);
  prefsWidget->setFocusPolicy(Qt::NoFocus);
  prefsWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  prefsWidget->setFeatures(QDockWidget::DockWidgetClosable);

  prefsCentral = new QWidget;
  prefsCentral->setObjectName("prefsCentral");
  prefsWidget->setWidget(prefsCentral);
  QSizePolicy prefsSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
  prefsCentral->setSizePolicy(prefsSizePolicy);
  addDockWidget(Qt::RightDockWidgetArea, prefsWidget);
  prefsWidget->hide();
  prefsWidget->setObjectName("prefs");

  connect(prefsWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(updatePrefsIcon()));

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

  addDockWidget(Qt::RightDockWidgetArea, outputWidget);
  addDockWidget(Qt::RightDockWidgetArea, incomingWidget);
  outputWidget->setObjectName("output");
  incomingWidget->setObjectName("input");

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

  //Currently causes a segfault when dragging doc pane out of main
  //window:
  connect(docWidget, SIGNAL(visibilityChanged(bool)), this, SLOT(helpVisibilityChanged()));


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

void MainWindow::escapeWorkspaces() {
  resetErrorPane();

  for (int w=0; w < workspace_max; w++) {
    workspaces[w]->escapeAndCancelSelection();
    workspaces[w]->clearLineMarkers();
  }
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
  else {
    outputWidget->setTitleBarWidget(outputWidgetTitle);
    this->setWindowState(windowState() & ~(Qt::WindowFullScreen));

#ifdef Q_OS_WIN
    this->setWindowFlags(Qt::WindowTitleHint | Qt::WindowSystemMenuHint |
			 Qt::WindowMinimizeButtonHint |
			 Qt::WindowMaximizeButtonHint |
			 Qt::WindowCloseButtonHint);
#endif

    statusBar()->showMessage(tr("Full screen mode off."), 2000);
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
    show_incoming_osc_log->setChecked(false);
  }
  else {
    full_screen->setChecked(false);
    show_tabs->setChecked(true);
    show_buttons->setChecked(true);
    show_incoming_osc_log->setChecked(true);
  }

  updateFullScreenMode();
  updateTabsVisibility();
  updateButtonVisibility();
  updateLogVisibility();
  updateIncomingOscLogVisibility();
}

void MainWindow::toggleScopePaused() {
  scopeInterface->togglePause();
}

void MainWindow::allJobsCompleted() {
  scopeInterface->pause();

  // re-enable log text selection
  incomingPane->setTextInteractionFlags(Qt::TextSelectableByMouse);
  outputPane->setTextInteractionFlags(Qt::TextSelectableByMouse);

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

void MainWindow::updateIncomingOscLogVisibility(){
  if(show_incoming_osc_log->isChecked()) {
    incomingWidget->show();
  }
  else{
    incomingWidget->close();
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

void MainWindow::completeSnippetListOrIndentLine(QObject* ws){
  SonicPiScintilla *spws = ((SonicPiScintilla*)ws);
  if(spws->isListActive()) {
    spws->tabCompleteifList();
  }
  else {
    completeSnippetOrIndentCurrentLineOrSelection(spws);
  }
}

void MainWindow::completeSnippetOrIndentCurrentLineOrSelection(SonicPiScintilla* ws) {
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

void MainWindow::toggleCommentInCurrentWorkspace() {
  SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();
  toggleComment(ws);
}

void MainWindow::toggleComment(SonicPiScintilla* ws) {
  int start_line, finish_line, point_line, point_index;
  ws->getCursorPosition(&point_line, &point_index);
  if(ws->hasSelectedText()) {
    statusBar()->showMessage(tr("Toggle selection comment..."), 2000);
    int unused_a, unused_b;
    ws->getSelection(&start_line, &unused_a, &finish_line, &unused_b);
  } else {
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

void MainWindow::startRubyServer(){

  // kill any zombie processes that may exist
  // better: test to see if UDP ports are in use, only kill/sleep if so
  // best: kill SCSynth directly if needed
  serverProcess = new QProcess();

  QStringList args;
#if defined(Q_OS_MAC)
  args << "--enable-frozen-string-literal";
#elif defined(Q_OS_WIN)
  args << "--enable-frozen-string-literal";
#endif

  args << "-E" << "utf-8";
  args << ruby_server_path;


  if(protocol == TCP){
    args << "-t";
  }else {
    args << "-u";
  }


  args <<
    QString("%1").arg(server_listen_to_gui_port) <<
    QString("%1").arg(server_send_to_gui_port) <<
    QString("%1").arg(scsynth_port) <<
    QString("%1").arg(scsynth_send_port) <<
    QString("%1").arg(server_osc_cues_port) <<
    QString("%1").arg(erlang_router_port) <<
    QString("%1").arg(osc_midi_out_port) <<
    QString("%1").arg(osc_midi_in_port) <<
    QString("%1").arg(websocket_port);;
  std::cout << "[GUI] - launching Sonic Pi Server:" << std::endl;
  if(homeDirWritable) {
    serverProcess->setStandardErrorFile(server_error_log_path);
    serverProcess->setStandardOutputFile(server_output_log_path);
  }
  serverProcess->start(ruby_path, args);
  // Register server pid for potential zombie clearing
  QStringList regServerArgs;
#if QT_VERSION >= QT_VERSION_CHECK(5, 3, 0)
  regServerArgs << QDir::toNativeSeparators(rootPath() + "/app/server/ruby/bin/task-register.rb")<< QString::number(serverProcess->processId());
#endif
  QProcess *regServerProcess = new QProcess();
  regServerProcess->start(ruby_path, regServerArgs);
  regServerProcess->waitForFinished();
#if QT_VERSION >= QT_VERSION_CHECK(5, 3, 0)
  std::cout << "[GUI] - Ruby server pid registered: "<< serverProcess->processId() << std::endl;
#endif

  if (!serverProcess->waitForStarted()) {
    invokeStartupError(tr("The Sonic Pi Server could not be started!"));
    return;
  }
}

bool MainWindow::waitForServiceSync() {
  QString contents;
  std::cout << "[GUI] - waiting for Sonic Pi Server to boot..." << std::endl;
  bool server_booted = false;
  if (!homeDirWritable) {
    // we can't monitor the logs so hope for the best!
    sleep(15);
    server_booted = true;
  } else {
    for(int i = 0; i < 60; i ++) {
      qApp->processEvents();
      contents = readFile(server_output_log_path);
      if (contents.contains("Sonic Pi Server successfully booted.")) {
        std::cout << std::endl << "[GUI] - Sonic Pi Server successfully booted." << std::endl;
        server_booted = true;
        break;
      } else {
        std::cout << ".";
        sleep(1);
      }
    }
  }

  if (!server_booted) {
      std::cout << std::endl << "[GUI] - Critical error! Could not boot Sonic Pi Server." << std::endl;
      invokeStartupError("Critical error! - Could not boot Sonic Pi Server.");
      return false;
  }

  int timeout = 60;
  std::cout << "[GUI] - waiting for Sonic Pi Server to respond..." << std::endl;
  while (sonicPiOSCServer->waitForServer() && timeout-- > 0) {
    sleep(1);
    std::cout << ".";
    if(sonicPiOSCServer->isIncomingPortOpen()) {
      Message msg("/ping");
      msg.pushStr(guiID.toStdString());
      msg.pushStr("QtClient/1/hello");
      sendOSC(msg);
    }
  }
  if (!sonicPiOSCServer->isServerStarted()) {
      std::cout << std::endl <<  "[GUI] - Critical error! Could not connect to Sonic Pi Server." << std::endl;
      invokeStartupError("Critical server error - could not connect to Sonic Pi Server!");
      return false;
  } else {
    std::cout << std::endl << "[GUI] - Sonic Pi Server connection established" << std::endl;
    return true;
  }

}

void MainWindow::splashClose() {
#if defined(Q_OS_MAC)
  splash->close();
#else
  splash->finish(this);
#endif
}

void MainWindow::showWindow() {
  QSettings settings("sonic-pi.net", "gui-settings");
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

bool isScopeEnabledByDefault( const QString& name )
{
  if( name == "mono" ) return true;
  return false;
}

bool isScopeEnabled( const QSettings& settings, const QString& name )
{
  QString lname = name.toLower();
  return settings.value("prefs/scope/show-"+lname, isScopeEnabledByDefault(lname) ).toBool();
}

void MainWindow::honourPrefs() {
  QSettings settings("sonic-pi.net", "gui-settings");
  int stored_vol = settings.value("prefs/system-vol", 50).toInt();
  update_mixer_invert_stereo();
  update_mixer_force_mono();
  changeSystemPreAmp(stored_vol, 1);
  update_check_updates();
  updateLogAutoScroll();
  toggleScopeAxes();
  toggleMidi(1);
  toggleOSCServer(1);
  toggleIcons();
}

void MainWindow::initPrefsWindow() {

  prefTabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);

  QGridLayout *grid = new QGridLayout;

  QGroupBox *ioTab = new QGroupBox();

  QGroupBox *network_box = new QGroupBox(tr("Networked OSC"));
  network_box->setToolTip(tr("Sonic Pi can send and receive Open Sound Control messages\nto and from other programs or computers\n via the currently connected network."));

  QLabel *network_ip_label = new QLabel();
  QString ip_address_trans = tr("Local IP address");
  QString port_num_trans = tr("Listening for OSC messages on port");
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
  network_ip_label->setText(ip_address_trans + ": " + ip_address + "\n" + port_num_trans + + ": " + QString::number(server_osc_cues_port));
  network_ip_label->setToolTip(all_ip_addresses);

  osc_public_check = new QCheckBox(tr("Receive remote OSC messages"));
  osc_public_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages from remote machines.\n When unchecked, only messages from the local machine will be received."));
  connect(osc_public_check, SIGNAL(clicked()), this, SLOT(toggleOSCServer()));

  osc_server_enabled_check = new QCheckBox(tr("Enable OSC server"));
  osc_server_enabled_check->setToolTip(tr("When checked, Sonic Pi will listen for OSC messages.\n When unchecked no OSC messages will be received."));
  connect(osc_server_enabled_check, SIGNAL(clicked()), this, SLOT(toggleOSCServer()));


  QVBoxLayout *network_box_layout = new QVBoxLayout;
  network_box_layout->addWidget(osc_server_enabled_check);
  network_box_layout->addWidget(osc_public_check);
  network_box_layout->addWidget(network_ip_label);
  network_box->setLayout(network_box_layout);

  QGroupBox *midi_config_box = new QGroupBox(tr("MIDI Configuration"));
  midi_config_box->setToolTip(tr("Configure MIDI behaviour"));

  QGroupBox *midi_ports_box = new QGroupBox(tr("MIDI Ports"));
  midi_ports_box->setToolTip(tr("List all connected MIDI Ports"));

  midi_enable_check = new QCheckBox(tr("Enable MIDI subsystems"));
  midi_enable_check->setToolTip(tr("Enable or disable incoming and outgoing MIDI communication"));
  connect(midi_enable_check, SIGNAL(clicked()), this, SLOT(toggleMidi()));

  QPushButton *midi_reset_button = new QPushButton(tr("Reset MIDI"));
  midi_reset_button->setToolTip(tr("Reset MIDI subsystems \n(Required to detect device changes on macOS)" ));
  connect(midi_reset_button, SIGNAL(clicked()), this, SLOT(resetMidi()));

  midi_default_channel_combo = new QComboBox();
  midi_default_channel_combo->addItem("*");
  midi_default_channel_combo->addItem("1");
  midi_default_channel_combo->addItem("2");
  midi_default_channel_combo->addItem("3");
  midi_default_channel_combo->addItem("4");
  midi_default_channel_combo->addItem("5");
  midi_default_channel_combo->addItem("6");
  midi_default_channel_combo->addItem("7");
  midi_default_channel_combo->addItem("8");
  midi_default_channel_combo->addItem("9");
  midi_default_channel_combo->addItem("10");
  midi_default_channel_combo->addItem("11");
  midi_default_channel_combo->addItem("12");
  midi_default_channel_combo->addItem("13");
  midi_default_channel_combo->addItem("14");
  midi_default_channel_combo->addItem("15");
  midi_default_channel_combo->addItem("16");
  midi_default_channel_combo->setMaxVisibleItems(17);
  midi_default_channel_combo->setMinimumContentsLength(2);
  midi_default_channel_combo->setSizeAdjustPolicy(QComboBox::AdjustToMinimumContentsLength) ;


  QLabel *midi_default_channel_label = new QLabel;
  midi_default_channel_label->setText(tr("Default MIDI channel (* means all)"));
  midi_default_channel_label->setToolTip(tr("Default MIDI Channel to send messages to"));

  QGridLayout *midi_default_channel_layout = new QGridLayout();

  midi_default_channel_combo->setToolTip(tr("Default MIDI Channel to send messages to"));

  midi_default_channel_layout->addWidget(midi_default_channel_combo, 0, 0);
  midi_default_channel_layout->addWidget(midi_default_channel_label, 0, 1);

  midi_in_ports_label = new QLabel;
  midi_out_ports_label = new QLabel;
  midi_in_ports_label->setFont(QFont("Hack"));
  midi_out_ports_label->setFont(QFont("Hack"));
  midi_in_ports_label->setAccessibleName("midi-in-ports-label");
  midi_out_ports_label->setAccessibleName("midi-out-ports-label");
  midi_in_ports_label->setText(tr("No connected input devices"));
  midi_out_ports_label->setText(tr("No connected output devices"));
  midi_in_ports_label->setToolTip(tr("MIDI input devices send MIDI messages directly to\nSonic Pi and are received as cue events\n(similar to incoming OSC messages and internal cues)"));
  midi_out_ports_label->setToolTip(tr("MIDI output devices receive MIDI messages directly from\nSonic Pi which can be sent via the midi_* fns"));


  QVBoxLayout *midi_ports_box_layout = new QVBoxLayout;
  QVBoxLayout *midi_config_box_layout = new QVBoxLayout;
  midi_config_box_layout->addWidget(midi_enable_check);
  midi_config_box_layout->addLayout(midi_default_channel_layout);

  midi_ports_box_layout->addWidget(midi_in_ports_label);
  midi_ports_box_layout->addWidget(midi_out_ports_label);
  midi_ports_box_layout->addWidget(midi_reset_button);

  midi_ports_box->setLayout(midi_ports_box_layout);
  midi_config_box->setLayout(midi_config_box_layout);
  QGridLayout *io_tab_layout = new QGridLayout();
  io_tab_layout->addWidget(midi_ports_box, 0, 0, 0, 1);
  io_tab_layout->addWidget(midi_config_box, 0, 1);
  io_tab_layout->addWidget(network_box, 1, 1);

  ioTab->setLayout(io_tab_layout);



  QGroupBox *volBox = new QGroupBox(tr("Master Volume"));
  volBox->setToolTip(tr("Use this slider to change the system volume."));

  QGroupBox *advancedAudioBox = new QGroupBox(tr("Audio Output"));
  advancedAudioBox->setToolTip(tr("Advanced audio settings for working with\nexternal PA systems when performing with Sonic Pi."));
  mixer_invert_stereo = new QCheckBox(tr("Invert stereo"));
  mixer_invert_stereo->setToolTip(tr("Toggle stereo inversion.\nIf enabled, audio sent to the left speaker will\nbe routed to the right speaker and visa versa."));
  connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
  mixer_force_mono = new QCheckBox(tr("Force mono"));
  mixer_force_mono->setToolTip(tr("Toggle mono mode.\nIf enabled both right and left audio is mixed and\nthe same signal is sent to both speakers.\nUseful when working with external systems that\ncan only handle mono."));
  connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));

  QVBoxLayout *advanced_audio_box_layout = new QVBoxLayout;
  advanced_audio_box_layout->addWidget(mixer_invert_stereo);
  advanced_audio_box_layout->addWidget(mixer_force_mono);
  advancedAudioBox->setLayout(advanced_audio_box_layout);

  QHBoxLayout *vol_box = new QHBoxLayout;
  system_vol_slider = new QSlider(this);
  QSettings settings("sonic-pi.net", "gui-settings");
  int stored_vol = settings.value("prefs/system-vol", 50).toInt();
  system_vol_slider->setValue(stored_vol);
  connect(system_vol_slider, SIGNAL(valueChanged(int)), this, SLOT(changeSystemPreAmp(int)));
  vol_box->addWidget(system_vol_slider);
  volBox->setLayout(vol_box);

  QGroupBox *debug_box = new QGroupBox(tr("Logging"));
  debug_box->setToolTip(tr("Configure debug behaviour"));

  QGroupBox *synths_box = new QGroupBox(tr("Synths and FX"));
  synths_box->setToolTip(tr("Modify behaviour of synths and FX"));

  print_output = new QCheckBox(tr("Log synths"));
  print_output->setToolTip(tr("Toggle log messages.\nIf disabled, activity such as synth and sample\ntriggering will not be printed to the log by default."));

  clear_output_on_run = new QCheckBox(tr("Clear log on run"));
  clear_output_on_run->setToolTip(tr("Toggle log clearing on run.\nIf enabled, the log is cleared each\ntime the run button is pressed."));

  log_cues = new QCheckBox(tr("Log cues"));
  log_cues->setToolTip(tr("Enable or disable logging of cues.\nIf disabled, cues will still trigger.\nHowever, they will not be visible in the logs."));

  log_auto_scroll = new QCheckBox(tr("Auto-scroll log"));
  log_auto_scroll->setToolTip(tr("Toggle log auto scrolling.\nIf enabled the log is scrolled to the bottom after every new message is displayed."));
  connect(log_auto_scroll, SIGNAL(clicked()), this, SLOT(updateLogAutoScroll()));

  check_args = new QCheckBox(tr("Safe mode"));
  check_args->setToolTip(tr("Toggle synth argument checking functions.\nIf disabled, certain synth opt values may\ncreate unexpectedly loud or uncomfortable sounds."));


  enable_external_synths_cb = new QCheckBox(tr("Enable external synths and FX"));
  enable_external_synths_cb->setToolTip(tr("When enabled, Sonic Pi will allow\nsynths and FX loaded via load_synthdefs\nto be triggered.\n\nWhen disabled, Sonic Pi will complain\nwhen you attempt to use a synth or FX\nwhich isn't recognised."));

  synth_trigger_timing_guarantees_cb = new QCheckBox(tr("Enforce timing guarantees"));
  synth_trigger_timing_guarantees_cb->setToolTip(tr("When enabled, Sonic Pi will refuse\nto trigger synths and FX if\nit is too late to do so\n\nWhen disabled, Sonic Pi will always\nattempt to trigger synths and FX\neven when a little late."));

  QVBoxLayout *debug_box_layout = new QVBoxLayout;
  debug_box_layout->addWidget(print_output);
  debug_box_layout->addWidget(log_cues);
  debug_box_layout->addWidget(log_auto_scroll);
  debug_box_layout->addWidget(clear_output_on_run);
  debug_box->setLayout(debug_box_layout);

  QVBoxLayout *synths_box_layout = new QVBoxLayout;
  synths_box_layout->addWidget(check_args);
  synths_box_layout->addWidget(synth_trigger_timing_guarantees_cb);
  synths_box_layout->addWidget(enable_external_synths_cb);


  synths_box->setLayout(synths_box_layout);

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

  auto_indent_on_run->setToolTip(tr("Automatically align code on Run"));

  show_line_numbers = new QCheckBox(tr("Show line numbers"));
  show_line_numbers->setToolTip(tr("Toggle line number visibility."));
  show_log = new QCheckBox(tr("Show log"));
  show_incoming_osc_log = new QCheckBox(tr("Show cue log"));
  show_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of the log.")));
  show_incoming_osc_log->setToolTip(tooltipStrShiftMeta('L', tr("Toggle visibility of cue log which displays internal cues & incoming OSC/MIDI messages.")));
  show_log->setChecked(true);
  show_incoming_osc_log->setChecked(true);
  show_buttons = new QCheckBox(tr("Show buttons"));
  show_buttons->setToolTip(tooltipStrShiftMeta('B', tr("Toggle visibility of the control buttons.")));
  show_buttons->setChecked(true);
  show_tabs = new QCheckBox(tr("Show tabs"));
  show_tabs->setChecked(true);
  show_tabs->setToolTip(tr("Toggle visibility of the buffer selection tabs."));
  full_screen = new QCheckBox(tr("Full screen"));
  full_screen->setToolTip(tooltipStrShiftMeta('F', tr("Toggle full screen mode.")));
  connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(changeShowLineNumbers()));
  colourModeButtonGroup = new QButtonGroup(this);

  lightModeCheck = new QCheckBox(tr("Light"));
  darkModeCheck = new QCheckBox(tr("Dark"));
  lightProModeCheck = new QCheckBox(tr("Pro Light"));
  darkProModeCheck = new QCheckBox(tr("Pro Dark"));
  highContrastModeCheck = new QCheckBox(tr("High Contrast"));
  connect(lightModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
  connect(darkModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
  connect(lightProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
  connect(darkProModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
  connect(highContrastModeCheck, SIGNAL(clicked()), this, SLOT(updateColourTheme()));
  colourModeButtonGroup->addButton(lightModeCheck, 0);
  colourModeButtonGroup->addButton(darkModeCheck, 1);
  colourModeButtonGroup->addButton(lightProModeCheck, 2);
  colourModeButtonGroup->addButton(darkProModeCheck, 3);
  colourModeButtonGroup->addButton(highContrastModeCheck, 4);

  lightModeCheck->setChecked(true);
  connect(show_line_numbers, SIGNAL(clicked()), this, SLOT(changeShowLineNumbers()));
  connect(show_log, SIGNAL(clicked()), this, SLOT(updateLogVisibility()));
  connect(show_incoming_osc_log, SIGNAL(clicked()), this, SLOT(updateIncomingOscLogVisibility()));
  connect(show_buttons, SIGNAL(clicked()), this, SLOT(updateButtonVisibility()));
  connect(full_screen, SIGNAL(clicked()), this, SLOT(updateFullScreenMode()));
  connect(show_tabs, SIGNAL(clicked()), this, SLOT(updateTabsVisibility()));


  QVBoxLayout *editor_display_box_layout = new QVBoxLayout;
  QVBoxLayout *editor_box_look_feel_layout = new QVBoxLayout;
  QVBoxLayout *automation_box_layout = new QVBoxLayout;
  QGridLayout *gridEditorPrefs = new QGridLayout;

  editor_display_box_layout->addWidget(show_line_numbers);
  editor_display_box_layout->addWidget(show_log);
  editor_display_box_layout->addWidget(show_incoming_osc_log);
  editor_display_box_layout->addWidget(show_buttons);
  editor_display_box_layout->addWidget(show_tabs);
  editor_box_look_feel_layout->addWidget(lightModeCheck);
  editor_box_look_feel_layout->addWidget(darkModeCheck);
  editor_box_look_feel_layout->addWidget(lightProModeCheck);
  editor_box_look_feel_layout->addWidget(darkProModeCheck);
  editor_box_look_feel_layout->addWidget(highContrastModeCheck);



  editor_display_box->setLayout(editor_display_box_layout);
  editor_look_feel_box->setLayout(editor_box_look_feel_layout);

  automation_box_layout->addWidget(auto_indent_on_run);
  automation_box_layout->addWidget(full_screen);
  automation_box->setLayout(automation_box_layout);

  gridEditorPrefs->addWidget(editor_display_box, 0, 0);
  gridEditorPrefs->addWidget(editor_look_feel_box, 0, 1);
  gridEditorPrefs->addWidget(automation_box, 1, 1);
  gridEditorPrefs->addWidget(debug_box, 1, 0);

  editor_box->setLayout(gridEditorPrefs);
  grid->addWidget(prefTabs, 0, 0);

  QGroupBox *audio_prefs_box = new QGroupBox();
  QGridLayout *audio_prefs_box_layout = new QGridLayout;

  audio_prefs_box_layout->addWidget(volBox, 0, 0, 0, 1);
  audio_prefs_box_layout->addWidget(synths_box, 0, 1);
  audio_prefs_box_layout->addWidget(advancedAudioBox, 1, 1);



  prefTabs->addTab(audio_prefs_box, tr("Audio"));
  audio_prefs_box->setLayout(audio_prefs_box_layout);

  prefTabs->addTab(ioTab, tr("IO"));

  prefTabs->addTab(editor_box, tr("Editor"));


  QGroupBox *viz_box = new QGroupBox();
  viz_box->setToolTip(tr("Settings useful for performing with Sonic Pi"));

  QGridLayout* viz_tab_layout = new QGridLayout();

  QGroupBox *scope_box = new QGroupBox(tr("Show and Hide Scope"));
  QGroupBox *scope_box_kinds = new QGroupBox(tr("Scope Kinds"));

  QVBoxLayout *scope_box_kinds_layout = new QVBoxLayout;
  QVBoxLayout *scope_box_layout = new QVBoxLayout;

  scopeSignalMap = new QSignalMapper(this);
  for( auto name : scopeInterface->getScopeNames() )
  {
    QCheckBox* cb = new QCheckBox( tr(name.toLocal8Bit().data()) );
    cb->setChecked( scopeInterface->enableScope( name, isScopeEnabled(settings,name) ) );
    scopeSignalMap->setMapping( cb, cb );
    scope_box_kinds_layout->addWidget(cb);
    connect(cb, SIGNAL(clicked()), scopeSignalMap, SLOT(map()));
  }
  connect( scopeSignalMap, SIGNAL(mapped(QWidget*)), this, SLOT(toggleScope(QWidget*)));
  show_scopes = new QCheckBox(tr("Show Scopes"));
  show_scopes->setToolTip(tr("Toggle the visibility of the audio oscilloscopes."));
  show_scope_axes = new QCheckBox(tr("Show Axes"));
  show_scope_axes->setToolTip(tr("Toggle the visibility of the axes for the audio oscilloscopes"));
  show_scope_axes->setChecked(true);
  scope_box_kinds->setLayout(scope_box_kinds_layout);
  scope_box_kinds->setToolTip(tr("The audio oscilloscope comes in three flavours which may\nbe viewed independently or all together:\n\nLissajous - illustrates the phase relationship between the left and right channels\nMono - shows a combined view of the left and right channels (using RMS)\nStereo - shows two independent scopes for left and right channels"));
  scope_box_layout->addWidget(show_scopes);
  scope_box_layout->addWidget(show_scope_axes);
  scope_box->setLayout(scope_box_layout);
  viz_tab_layout->addWidget(scope_box, 0, 0);
  viz_tab_layout->addWidget(scope_box_kinds, 1, 0);
#if defined(Q_OS_LINUX)
  // do nothing
#else
    viz_tab_layout->addWidget(transparency_box, 0, 1, 0, 1);
#endif
  connect(show_scope_axes, SIGNAL(clicked()), this, SLOT(toggleScopeAxes()));
  connect(show_scopes, SIGNAL(clicked()), this, SLOT(scope()));
  viz_box->setLayout(viz_tab_layout);
  prefTabs->addTab(viz_box, tr("Visuals"));

  //  prefTabs->addTab(performance_box, tr("Status"));


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
      " yet.<br/>" +
      "We rely on crowdsourcing to help create and maintain translations.<br/>" +
      "<a href=\"https://github.com/samaaron/sonic-pi/blob/master/TRANSLATION.md\">" +
      "Please consider helping to translate Sonic Pi to your language.</a> "
    );
    go_translate->setTextFormat(Qt::RichText);
    translation_box_layout->addWidget(go_translate);
    translation_box->setLayout(translation_box_layout);

    grid->addWidget(translation_box, 3, 0, 1, 2);
  }

  prefsCentral->setLayout(grid);

  // Read in preferences from previous session

  osc_public_check->setChecked(settings.value("prefs/osc-public", false).toBool());
  osc_server_enabled_check->setChecked(settings.value("prefs/osc-enabled", true).toBool());
  midi_enable_check->setChecked(settings.value("prefs/midi-enable", true).toBool());
  midi_default_channel_combo->setCurrentIndex(settings.value("prefs/default-midi-channel", 0).toInt());
  check_args->setChecked(settings.value("prefs/check-args", true).toBool());
  print_output->setChecked(settings.value("prefs/print-output", true).toBool());
  clear_output_on_run->setChecked(settings.value("prefs/clear-output-on-run", true).toBool());
  log_cues->setChecked(settings.value("prefs/log-cues", false).toBool());
  log_auto_scroll->setChecked(settings.value("prefs/log-auto-scroll", true).toBool());
  show_line_numbers->setChecked(settings.value("prefs/show-line-numbers", true).toBool());
  enable_external_synths_cb->setChecked(settings.value("prefs/enable-external-synths", false).toBool());
  synth_trigger_timing_guarantees_cb->setChecked(settings.value("prefs/synth-trigger-timing-guarantees", false).toBool());
  mixer_force_mono->setChecked(settings.value("prefs/mixer-force-mono", false).toBool());
  mixer_invert_stereo->setChecked(settings.value("prefs/mixer-invert-stereo", false).toBool());

  check_updates->setChecked(settings.value("prefs/rp/check-updates", true).toBool());

  auto_indent_on_run->setChecked(settings.value("prefs/auto-indent-on-run", true).toBool());

  gui_transparency_slider->setValue(settings.value("prefs/gui_transparency", 0).toInt());



  //show_left_scope->setChecked( scopeInterface->enableScope( "Left", settings.value("prefs/scope/show-left", true).toBool() ) );
  //show_right_scope->setChecked( scopeInterface->enableScope( "Right", settings.value("prefs/scope/show-right", true).toBool() ) );
  show_scope_axes->setChecked( scopeInterface->setScopeAxes( settings.value("prefs/scope/show-axes", false).toBool() ) );
  show_scopes->setChecked( scopeInterface->setScopeAxes( settings.value("prefs/scope/show-scopes", true).toBool() ) );
  show_incoming_osc_log->setChecked( settings.value("prefs/show_incoming_osc_log", true).toBool());



}

void MainWindow::setMessageBoxStyle() {
  // Set text color to black and background colors to white for the error message display
  QPalette p = QApplication::palette();
  p.setColor(QPalette::WindowText,"#000");
  p.setColor(QPalette::ButtonText,"#000");
  p.setColor(QPalette::Text,"#000");
  p.setColor(QPalette::Base,"#FFF");
  QApplication::setPalette(p);
}

void MainWindow::invokeStartupError(QString msg) {
  if(startup_error_reported) {
    return;
  }

  startup_error_reported = true;
  sonicPiOSCServer->stop();
  QMetaObject::invokeMethod(this, "startupError",
			    Qt::QueuedConnection,
			    Q_ARG(QString, msg));
}

void MainWindow::startupError(QString msg) {
  splashClose();

  setMessageBoxStyle();
  QString gui_log;
  QString scsynth_log;
  QString processes_log;
  QString server_output_log;
  QString server_error_log;
  if(homeDirWritable) {
    gui_log = readFile(gui_log_path);
    scsynth_log = readFile(scsynth_log_path);
    processes_log = readFile(process_log_path);
    server_output_log = readFile(server_output_log_path);
    server_error_log = readFile(server_error_log_path);
  }
  else {
    gui_log = "Permissions error: unable to access log";
    scsynth_log = "Permissions error: unable to access log";
    server_output_log = "Permissions error: unable to access log";
    server_error_log = "Permissions error: unable to access log";
    processes_log = "Permissions error: unable to access log";
  }

  QMessageBox *box = new QMessageBox(QMessageBox::Warning,
				     tr("Server boot error..."), tr("Sonic Pi Boot Error\n\nApologies, a critical error occurred during startup") + ":\n\n " + msg + "\n\n" + tr("Please consider reporting a bug at") + "\nhttp://github.com/samaaron/sonic-pi/issues");
  QString error_report = "Sonic Pi Boot Error Report\n==================\n\n\nSystem Information\n----------------\n\n* Sonic Pi version: " + version + "\n* OS: " + osDescription() + "\n\n\nGUI Log\n-------\n\n**`" + gui_log_path + "`**\n```\n" + gui_log + "\n```\n\n\nServer Errors\n-------------\n\n**`" + server_error_log_path + "`**\n```\n" + server_error_log + "\n```\n\n\nServer Output\n-------------\n\n**`" + server_output_log_path + "`**\n```\n" + server_output_log + "\n```\n\n\nScsynth Output\n--------------\n\n**`" + scsynth_log_path + "`**\n```\n" + scsynth_log + "\n```\n\n\nProcess Log\n--------------\n\n**`" + process_log_path + "`**\n```\n" + processes_log + "\n\n\n```\n";
  box->setDetailedText(error_report);

  QGridLayout* layout = (QGridLayout*)box->layout();
  QSpacerItem* hSpacer = new QSpacerItem(200, 0, QSizePolicy::Minimum, QSizePolicy::Expanding);
  layout->addItem(hSpacer, layout->rowCount(), 0, 1, layout->columnCount());
  box->exec();
  std::cout << "[GUI] - Aborting. Sorry about this." << std::endl;
  close();
}

void MainWindow::replaceBuffer(QString id, QString content, int line, int index, int first_line) {
  SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
  ws->replaceBuffer(content, line, index, first_line);
}

void MainWindow::replaceBufferIdx(int buf_idx, QString content, int line, int index, int first_line) {
  //  statusBar()->showMessage(tr("Replacing Buffer..."), 1000);
  SonicPiScintilla* ws = workspaces[buf_idx];
  ws->replaceBuffer(content, line, index, first_line);
}

void MainWindow::replaceLines(QString id, QString content, int start_line, int finish_line, int point_line, int point_index) {
  SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
  ws->replaceLines(start_line, finish_line, content);
  ws->setCursorPosition(point_line, point_index);
}

QString MainWindow::osDescription() {
#if QT_VERSION >= 0x050400
  return QSysInfo::prettyProductName();
#else
  // prettyProductName requires QT 5.4
  //
  return QString("Unknown OS");
#endif
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


bool MainWindow::loadFile()
{
  QString selfilter = QString("%1 (*.rb *.txt)").arg(tr("Buffer files"));
  QSettings settings("sonic-pi.net", "gui-settings");
  QString lastDir = settings.value("lastDir", QDir::homePath() + "/Desktop").toString();
  QString fileName = QFileDialog::getOpenFileName(this, tr("Load Sonic Pi Buffer"), lastDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter) ;
  if(!fileName.isEmpty()){
    QFileInfo fi=fileName;
    settings.setValue("lastDir", fi.dir().absolutePath());
    SonicPiScintilla* p = (SonicPiScintilla*)tabs->currentWidget();
    loadFile(fileName, p);
    return true;
  } else {
    return false;
  }
}

bool MainWindow::saveAs()
{
  QString selfilter = QString("%1 (*.rb *.txt)").arg(tr("Buffer files"));
  QSettings settings("sonic-pi.net", "gui-settings");
  QString lastDir = settings.value("lastDir", QDir::homePath() + "/Desktop").toString();
  QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Buffer"), lastDir, QString("%1 (*.rb *.txt);;%2 (*.txt);;%3 (*.rb);;%4 (*.*)").arg(tr("Buffer files")).arg(tr("Text files")).arg(tr("Ruby files")).arg(tr("All files")), &selfilter) ;

  if(!fileName.isEmpty()){
    QFileInfo fi=fileName;
    settings.setValue("lastDir", fi.dir().absolutePath());
    if (!fileName.contains(QRegExp("\\.[a-z]+$"))) {
        fileName = fileName + ".txt";
      }
    return saveFile(fileName, (SonicPiScintilla*)tabs->currentWidget());
  } else {
    return false;
  }
}


void MainWindow::resetErrorPane() {
  errorPane->clear();
  errorPane->hide();
}

void MainWindow::runBufferIdx(int idx)
{
  QMetaObject::invokeMethod(tabs, "setCurrentIndex", Q_ARG(int, idx));
  runCode();
}

void MainWindow::showError(QString msg) {
  QString style_sheet = "qrc:///html/styles.css";
  if(darkModeCheck->isChecked() || darkProModeCheck->isChecked()) {
    style_sheet = "qrc:///html/dark_styles.css";
  }
  errorPane->clear();
  errorPane->setHtml("<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"" + style_sheet + "\"/></head><body>"  + msg + "</body></html>");
  errorPane->show();
}

void MainWindow::showBufferCapacityError() {
  showError("<h2 class=\"syntax_error_description\"><pre>GUI Error: Buffer Full</pre></h2><pre class=\"error_msg\"> Your code buffer has reached capacity. <br/> Please remove some code before continuing. <br/><span class=\"error_line\"> For working with very large buffers use: <br/> run_file \"/path/to/buffer.rb\"</span></pre>");
}

void MainWindow::runCode()
{
  scopeInterface->resume();

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
  SonicPiScintilla *ws = (SonicPiScintilla*)tabs->currentWidget();

  QString code = ws->text();

  if(!print_output->isChecked()) {
    code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

  if(!log_cues->isChecked()) {
    code = "use_cue_logging false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

  if(check_args->isChecked()) {
    code = "use_arg_checks true #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

  if(enable_external_synths_cb->isChecked()) {
    code = "use_external_synths true #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

  if(synth_trigger_timing_guarantees_cb->isChecked()) {
    code = "use_timing_guarantees true #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

  code = "use_midi_defaults channel: \"" + midi_default_channel_combo->currentText() + "\" #__nosave__ set by Qt GUI user preferences.\n" + code ;

  if(auto_indent_on_run->isChecked()) {
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

  if(clear_output_on_run->isChecked()){
    outputPane->clear();
  }

  msg.pushStr(code.toStdString());
  msg.pushStr(filename);
  bool res = sendOSC(msg);

  if(!res){
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
  bool res = oscSender->sendOSC(m);
  if(!res) {
    std::cout << "[GUI] - Could Not Send OSC" << std::endl;
  }
  return res;
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

void MainWindow::scopeVisibilityChanged() {
  if (scopeWidget->isVisible()) {
    show_scopes->setChecked(true);
  } else {
    show_scopes->setChecked(false);
  }

  scope();
}

void MainWindow::toggleScope()
{
  if(show_scopes->isChecked()) {

    show_scopes->setChecked(false);
  } else {
    show_scopes->setChecked(true);
  }
  scope();
}

void MainWindow::scope()
{

  if (lightProModeCheck->isChecked() || darkProModeCheck->isChecked()) {
    if (show_scopes->isChecked()) {
        scopeAct->setIcon(pro_scope_bordered_icon);
      } else {
      scopeAct->setIcon(pro_scope_icon);
    }

  } else if (darkModeCheck->isChecked()) {
      if (show_scopes->isChecked()) {
        scopeAct->setIcon(default_dark_scope_toggled_icon);
      } else {
        scopeAct->setIcon(default_dark_scope_icon);
      }
  } else if (lightModeCheck->isChecked()) {
      if (show_scopes->isChecked()) {
        scopeAct->setIcon(default_light_scope_toggled_icon);
      } else {
        scopeAct->setIcon(default_light_scope_icon);
      }
  } else if (highContrastModeCheck->isChecked()) {
      if (show_scopes->isChecked()) {
        scopeAct->setIcon(default_hc_scope_toggled_icon);
      } else {
        scopeAct->setIcon(default_hc_scope_icon);
      }
  }

  if(show_scopes->isChecked()) {
    scopeWidget->show();
  } else {
    scopeWidget->hide();
  }
}

void MainWindow::about()
{
  // todo: this is returning true even after the window disappears
  // Qt::Tool windows get closed automatically when app loses focus
  if(infoWidg->isVisible()) {
    infoWidg->hide();

    if (darkProModeCheck->isChecked()) {
      infoAct->setIcon(pro_info_dark_icon);
    } else if (lightProModeCheck->isChecked()) {
      infoAct->setIcon(pro_info_icon);
    } else if (lightModeCheck->isChecked()) {
      infoAct->setIcon(default_light_info_icon);
    }  else if (darkModeCheck->isChecked()) {
      infoAct->setIcon(default_dark_info_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      infoAct->setIcon(default_hc_info_icon);
    }

  } else {
    infoWidg->raise();
    infoWidg->show();

    if (darkProModeCheck->isChecked()) {
        infoAct->setIcon(pro_info_dark_bordered_icon);
    } else if (lightProModeCheck->isChecked()) {
      infoAct->setIcon(pro_info_bordered_icon);
    } else if (lightModeCheck->isChecked()) {
        infoAct->setIcon(default_light_info_toggled_icon);
    }  else if (darkModeCheck->isChecked()) {
      infoAct->setIcon(default_dark_info_toggled_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      infoAct->setIcon(default_hc_info_toggled_icon);

    }

  }
}


void MainWindow::help()
{
  if(docWidget->isVisible()) {
    hidingDocPane = true;
    docWidget->hide();
    if (darkProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_dark_icon);
    } else if (lightProModeCheck->isChecked()) {
      helpAct->setIcon(pro_help_icon);
    } else if (lightModeCheck->isChecked()) {
        helpAct->setIcon(default_light_help_icon);
    }  else if (darkModeCheck->isChecked()) {
      helpAct->setIcon(default_dark_help_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      helpAct->setIcon(default_hc_help_icon);

    }

  } else {
    docWidget->show();
    if(!updated_dark_mode_for_help) {
      updateColourTheme();
      updated_dark_mode_for_help = true;
    }

        if (darkProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_dark_bordered_icon);
    } else if (lightProModeCheck->isChecked()) {
      helpAct->setIcon(pro_help_bordered_icon);
    } else if (lightModeCheck->isChecked()) {
        helpAct->setIcon(default_light_help_toggled_icon);
    }  else if (darkModeCheck->isChecked()) {
      helpAct->setIcon(default_dark_help_toggled_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      helpAct->setIcon(default_hc_help_toggled_icon);

    }

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
    selection = ws->wordAtLineIndex(line, pos);
  }
  selection = selection.toLower();
  if (selection[0] == ':')
    selection = selection.mid(1);

  if (helpKeywords.contains(selection)) {
    struct help_entry entry = helpKeywords[selection];
    QListWidget *list = helpLists[entry.pageIndex];

    // force current row to be changed
    // by setting it to a different value to
    // entry.entryIndex and then setting it
    // back. That way it always gets displayed
    // in the GUI :-)
    if (entry.entryIndex == 0) {
      list->setCurrentRow(1);
    } else {
      list->setCurrentRow(0);
    }
    docsCentral->setCurrentIndex(entry.pageIndex);
    list->setCurrentRow(entry.entryIndex);
  }
}



#if defined(Q_OS_LINUX)
void MainWindow::changeGUITransparency(int)
#else
void MainWindow::changeGUITransparency(int val)
#endif
{
#if defined(Q_OS_LINUX)
  // do nothing
#else
  // scale it linearly from 0 -> 100 to 0.3 -> 1
  setWindowOpacity((0.7 * ((100 - (float)val) / 100.0))  + 0.3);
#endif
}

void MainWindow::changeSystemPreAmp(int val, int silent)
{
  float v = (float) val;
  v = (v / 100.0) * 2.0;
  Message msg("/mixer-amp");
  msg.pushStr(guiID.toStdString());
  msg.pushFloat(v);
  msg.pushInt32(silent);
  sendOSC(msg);
  statusBar()->showMessage(tr("Updating System Volume..."), 2000);
}

void MainWindow::toggleScope( QWidget* qw )
{
  QCheckBox* cb = static_cast<QCheckBox*>(qw);
  QSettings settings("sonic-pi.net", "gui-settings");
  settings.setValue("prefs/scope/show-"+cb->text().toLower(), cb->isChecked() );
  scopeInterface->enableScope( cb->text(), cb->isChecked() );

}

void MainWindow::toggleLeftScope()
{
  //scopeInterface->enableScope("Left",show_left_scope->isChecked());
}

void MainWindow::toggleRightScope()
{
  //scopeInterface->enableScope("Right",show_right_scope->isChecked());
}

void MainWindow::toggleScopeAxes()
{
  scopeInterface->setScopeAxes(show_scope_axes->isChecked());
}

void MainWindow::toggleDarkMode() {
  int checkedId = colourModeButtonGroup->checkedId();
  if(checkedId == (colourModeButtonGroup->buttons().size() - 1)) {
    colourModeButtonGroup->button(0)->toggle();
    } else {
    colourModeButtonGroup->button(checkedId + 1)->toggle();
  }
  updateColourTheme();
}

void MainWindow::updateLogAutoScroll() {
  bool val = log_auto_scroll->isChecked();
  outputPane->forceScrollDown(val);
  if(val) {
    statusBar()->showMessage(tr("Log Auto Scroll on..."), 2000);
    } else {
    statusBar()->showMessage(tr("Log Auto Scroll off..."), 2000);
  }
 }

void MainWindow::toggleIcons() {
  if (darkProModeCheck->isChecked()) {
    toolBar->setIconSize(QSize(30, 30));
    runAct->setIcon(pro_run_icon);
    stopAct->setIcon(pro_stop_icon);
    saveAsAct->setIcon(pro_save_dark_icon);
    loadFileAct->setIcon(pro_load_dark_icon);
    recAct->setIcon(pro_rec_icon);
    textIncAct->setIcon(pro_size_up_icon);
    textDecAct->setIcon(pro_size_down_icon);

    if (show_scopes->isChecked()) {
      scopeAct->setIcon(pro_scope_bordered_icon);
    } else {
      scopeAct->setIcon(pro_scope_icon);
    }

    if (infoWidg->isVisible()) {
      infoAct->setIcon(pro_info_dark_bordered_icon);
    } else {
      infoAct->setIcon(pro_info_dark_icon);
    }

    if (docWidget->isVisible()) {
      helpAct->setIcon(pro_help_dark_bordered_icon);
    } else {
      helpAct->setIcon(pro_help_dark_icon);
    }

    if (prefsWidget->isVisible()) {
      prefsAct->setIcon(pro_prefs_dark_bordered_icon);
    } else {
      prefsAct->setIcon(pro_prefs_dark_icon);
    }
  } else if (lightProModeCheck->isChecked()) {
    toolBar->setIconSize(QSize(30, 30));
    runAct->setIcon(pro_run_icon);
    stopAct->setIcon(pro_stop_icon);
    saveAsAct->setIcon(pro_save_icon);
    loadFileAct->setIcon(pro_load_icon);
    recAct->setIcon(pro_rec_icon);

    textIncAct->setIcon(pro_size_up_icon);
    textDecAct->setIcon(pro_size_down_icon);

    if (show_scopes->isChecked()) {
      scopeAct->setIcon(pro_scope_bordered_icon);
    } else {
      scopeAct->setIcon(pro_scope_icon);
    }


    if (infoWidg->isVisible()) {
      infoAct->setIcon(pro_info_bordered_icon);
    } else {
      infoAct->setIcon(pro_info_icon);
    }

    if (docWidget->isVisible()) {
      helpAct->setIcon(pro_help_bordered_icon);
    } else {
      helpAct->setIcon(pro_help_icon);
    }

    if (prefsWidget->isVisible()) {
      prefsAct->setIcon(pro_prefs_bordered_icon);
    } else {
      prefsAct->setIcon(pro_prefs_icon);
    }
  } else if (darkModeCheck->isChecked()) {
    toolBar->setIconSize(QSize(84.6, 30.0));
    // dark mode
    runAct->setIcon(default_dark_run_icon);
    stopAct->setIcon(default_dark_stop_icon);
    saveAsAct->setIcon(default_dark_save_icon);
    loadFileAct->setIcon(default_dark_load_icon);
    recAct->setIcon(default_dark_rec_icon);
    textIncAct->setIcon(default_dark_size_up_icon);
    textDecAct->setIcon(default_dark_size_down_icon);

    if (show_scopes->isChecked()) {
      scopeAct->setIcon(default_dark_scope_toggled_icon);
    } else {
      scopeAct->setIcon(default_dark_scope_icon);
    }

    if (infoWidg->isVisible()) {
      infoAct->setIcon(default_dark_info_toggled_icon);
    } else {
      infoAct->setIcon(default_dark_info_icon);
    }

    if (docWidget->isVisible()) {
      helpAct->setIcon(default_dark_help_toggled_icon);
    } else {
      helpAct->setIcon(default_dark_help_icon);
    }

    if (prefsWidget->isVisible()) {
      prefsAct->setIcon(default_dark_prefs_toggled_icon);
    } else {
      prefsAct->setIcon(default_dark_prefs_icon);
    }
  } else if (lightModeCheck->isChecked())  {
    toolBar->setIconSize(QSize(84.6, 30.0));
    // light mode
    runAct->setIcon(default_light_run_icon);
    stopAct->setIcon(default_light_stop_icon);
    saveAsAct->setIcon(default_light_save_icon);
    loadFileAct->setIcon(default_light_load_icon);
    recAct->setIcon(default_light_rec_icon);

    textIncAct->setIcon(default_light_size_up_icon);
    textDecAct->setIcon(default_light_size_down_icon);

    if (show_scopes->isChecked()) {
      scopeAct->setIcon(default_light_scope_toggled_icon);
    } else {
      scopeAct->setIcon(default_light_scope_icon);
    }


    if (infoWidg->isVisible()) {
      infoAct->setIcon(default_light_info_toggled_icon);
    } else {
      infoAct->setIcon(default_light_info_icon);
    }

    if (docWidget->isVisible()) {
      helpAct->setIcon(default_light_help_toggled_icon);
    } else {
      helpAct->setIcon(default_light_help_icon);
    }

    if (prefsWidget->isVisible()) {
      prefsAct->setIcon(default_light_prefs_toggled_icon);
    } else {
      prefsAct->setIcon(default_light_prefs_icon);
    }
  } else if (highContrastModeCheck->isChecked())  {
    // hc mode
    toolBar->setIconSize(QSize(84.6, 30.0));
    runAct->setIcon(default_hc_run_icon);
    stopAct->setIcon(default_hc_stop_icon);
    saveAsAct->setIcon(default_hc_save_icon);
    loadFileAct->setIcon(default_hc_load_icon);
    recAct->setIcon(default_hc_rec_icon);

    textIncAct->setIcon(default_hc_size_up_icon);
    textDecAct->setIcon(default_hc_size_down_icon);

    if (show_scopes->isChecked()) {
      scopeAct->setIcon(default_hc_scope_toggled_icon);
    } else {
      scopeAct->setIcon(default_hc_scope_icon);
    }


    if (infoWidg->isVisible()) {
      infoAct->setIcon(default_hc_info_toggled_icon);
    } else {
      infoAct->setIcon(default_hc_info_icon);
    }

    if (docWidget->isVisible()) {
      helpAct->setIcon(default_hc_help_toggled_icon);
    } else {
      helpAct->setIcon(default_hc_help_icon);
    }

    if (prefsWidget->isVisible()) {
      prefsAct->setIcon(default_hc_prefs_toggled_icon);
    } else {
      prefsAct->setIcon(default_hc_prefs_icon);
    }
  }


}

void MainWindow::updateColourTheme(){
  SonicPiTheme *currentTheme = lexer->theme;
  //Set css stylesheet for browser-like HTML widgets
  QString css = "";
  // switch themes
  if (darkModeCheck->isChecked() || darkProModeCheck->isChecked()){
    currentTheme->darkMode();
    css = readFile(qt_browser_dark_css);
  } else if (darkProModeCheck->isChecked()){
    currentTheme->darkMode();
    css = readFile(qt_browser_dark_css);
  } else if (lightModeCheck->isChecked()){
    currentTheme->lightMode();
    css = readFile(qt_browser_light_css);
  } else if (lightProModeCheck->isChecked()){
    currentTheme->lightMode();
    css = readFile(qt_browser_light_css);
  } else if (highContrastModeCheck->isChecked()){
    currentTheme->hcMode();
    css = readFile(qt_browser_hc_css);
  }

  toggleIcons();

  docPane->document()->setDefaultStyleSheet(css);
  docPane->reload();

  foreach(QTextBrowser* pane, infoPanes) {
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
  tabs->setStyleSheet("");
  prefTabs->setStyleSheet("");
  docsCentral->setStyleSheet("");
  docWidget->setStyleSheet("");
  toolBar->setStyleSheet("");
  scopeWidget->setStyleSheet("");

  QPalette p = QApplication::palette();
  p.setColor(QPalette::WindowText,      currentTheme->color("WindowForeground"));
  p.setColor(QPalette::Window,          currentTheme->color("WindowBackground"));
  p.setColor(QPalette::Base,            currentTheme->color("Base"));
  p.setColor(QPalette::AlternateBase,   currentTheme->color("AlternateBase"));
  p.setColor(QPalette::Text,            currentTheme->color("Foreground"));
  p.setColor(QPalette::HighlightedText, currentTheme->color("HighlightedForeground"));
  p.setColor(QPalette::Highlight,       currentTheme->color("HighlightedBackground"));
  p.setColor(QPalette::ToolTipBase,     currentTheme->color("ToolTipBase"));
  p.setColor(QPalette::ToolTipText,     currentTheme->color("ToolTipText"));
  p.setColor(QPalette::Button,          currentTheme->color("Button"));
  p.setColor(QPalette::ButtonText,      currentTheme->color("ButtonText"));
  p.setColor(QPalette::Shadow,          currentTheme->color("Shadow"));
  p.setColor(QPalette::Light,           currentTheme->color("Light"));
  p.setColor(QPalette::Midlight,        currentTheme->color("Midlight"));
  p.setColor(QPalette::Mid,             currentTheme->color("Mid"));
  p.setColor(QPalette::Dark,            currentTheme->color("Dark"));
  p.setColor(QPalette::Link,            currentTheme->color("Link"));
  p.setColor(QPalette::LinkVisited,     currentTheme->color("LinkVisited"));

  QApplication::setPalette(p);

  QString windowColor = currentTheme->color("WindowBackground").name();
  QString windowForegroundColor = currentTheme->color("WindowForeground").name();
  QString paneColor = currentTheme->color("PaneBackground").name();
  QString logForegroundColor = currentTheme->color("LogForeground").name();
  QString logBackgroundColor = currentTheme->color("LogBackground").name();
  QString windowBorderColor = currentTheme->color("WindowBorder").name();
  QString windowInternalBorderColor = currentTheme->color("WindowInternalBorder").name();

  QString buttonColor = currentTheme->color("Button").name();
  QString buttonBorderColor = currentTheme->color("ButtonBorder").name();
  QString buttonTextColor = currentTheme->color("ButtonText").name();
  QString pressedButtonColor = currentTheme->color("PressedButton").name();
  QString pressedButtonTextColor = currentTheme->color("PressedButtonText").name();

  QString scrollBarColor = currentTheme->color("ScrollBar").name();
  QString scrollBarBackgroundColor = currentTheme->color("ScrollBarBackground").name();

  QString tabColor = currentTheme->color("Tab").name();
  QString tabTextColor = currentTheme->color("TabText").name();
  QString tabSelectedColor = currentTheme->color("TabSelected").name();
  QString tabSelectedTextColor = currentTheme->color("TabSelectedText").name();

  QString toolTipTextColor = currentTheme->color("ToolTipText").name();
  QString toolTipBaseColor = currentTheme->color("ToolTipBase").name();

  QString statusBarColor = currentTheme->color("StatusBar").name();
  QString statusBarTextColor = currentTheme->color("StatusBarText").name();

  QString sliderColor = currentTheme->color("Slider").name();
  QString sliderBackgroundColor = currentTheme->color("SliderBackground").name();
  QString sliderBorderColor = currentTheme->color("SliderBorder").name();

  QString menuColor = currentTheme->color("Menu").name();
  QString menuTextColor = currentTheme->color("MenuText").name();
  QString menuSelectedColor = currentTheme->color("MenuSelected").name();
  QString menuSelectedTextColor = currentTheme->color("MenuSelectedText").name();

  QString selectionForegroundColor = currentTheme->color("SelectionForeground").name();
  QString selectionBackgroundColor = currentTheme->color("SelectionBackground").name();
  QString errorBackgroundColor = currentTheme->color("ErrorBackground").name();

  QString appStyling = readFile(qt_app_theme_path);

  appStyling.replace("fixedWidthFont", "\"Hack\"");

  appStyling
    .replace("windowColor", windowColor)
    .replace("windowForegroundColor", windowForegroundColor)
    .replace("paneColor", paneColor)
    .replace("logForegroundColor", logForegroundColor)
    .replace("logBackgroundColor", logBackgroundColor)
    .replace("windowBorderColor", windowBorderColor)
    .replace("windowInternalBorderColor", windowInternalBorderColor)
    .replace("buttonColor", buttonColor)
    .replace("buttonBorderColor", buttonBorderColor)
    .replace("buttonTextColor", buttonTextColor)
    .replace("pressedButtonColor", pressedButtonColor)
    .replace("pressedButtonTextColor", pressedButtonTextColor)
    .replace("scrollBarColor", scrollBarColor)
    .replace("scrollBarBackgroundColor", scrollBarBackgroundColor)
    .replace("tabColor", tabColor)
    .replace("tabTextColor", tabTextColor)
    .replace("tabSelectedColor", tabSelectedColor)
    .replace("tabSelectedTextColor", tabSelectedTextColor)
    .replace("toolTipTextColor", toolTipTextColor)
    .replace("toolTipBaseColor", toolTipBaseColor)
    .replace("statusBarColor", statusBarColor)
    .replace("statusBarTextColor", statusBarTextColor)
    .replace("sliderColor", sliderColor)
    .replace("sliderBackgroundColor", sliderBackgroundColor)
    .replace("sliderBorderColor", sliderBorderColor)
    .replace("menuColor", menuColor)
    .replace("menuTextColor", menuTextColor)
    .replace("menuSelectedColor", menuSelectedColor)
    .replace("menuSelectedTextColor", menuSelectedTextColor)
    .replace("selectionForegroundColor", selectionForegroundColor)
    .replace("selectionBackgroundColor", selectionBackgroundColor)
    .replace("errorBackgroundColor", errorBackgroundColor);

  this->setStyleSheet(appStyling);
  infoWidg->setStyleSheet(appStyling);

  errorPane->setStyleSheet( QString(
    "QTextEdit{"
    "  background-color: %1;"
    "  padding: 5;"
    "}"
    ""
    ".error-background{"
    "  background-color: %2"
    "}").arg(paneColor, errorBackgroundColor));


  docsCentral->setStyleSheet( QString(
                                      "QListWidget{"
                                      "border: none;"
                                      "font-size: 13px;"
                                      "font-family: none;"
                                      "}"));

  scopeInterface->refresh();
  scopeWidget->update();

  for(int i=0; i < tabs->count(); i++){
    SonicPiScintilla *ws = (SonicPiScintilla *)tabs->widget(i);
    ws->setFrameShape(QFrame::NoFrame);
    ws->setStyleSheet(appStyling);

    if (highContrastModeCheck->isChecked()) {
      ws->setCaretWidth(8);
    } else {
      ws->setCaretWidth(5);
    }
    ws->redraw();
  }

  scopeInterface->setColor(currentTheme->color("scope"));
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

void MainWindow::togglePrefs() {
  if(prefsWidget->isVisible()) {
    prefsWidget->hide();
  } else {
    prefsWidget->show();
  }
  updatePrefsIcon();
}

void MainWindow::updatePrefsIcon()
{
  if(prefsWidget->isVisible()) {
    if (darkProModeCheck->isChecked()) {
      prefsAct->setIcon(pro_prefs_dark_bordered_icon);
    } else if (lightProModeCheck->isChecked()) {
      prefsAct->setIcon(pro_prefs_bordered_icon);
    } else if (lightModeCheck->isChecked()) {
      prefsAct->setIcon(default_light_prefs_toggled_icon);
    }  else if (darkModeCheck->isChecked()) {
      prefsAct->setIcon(default_dark_prefs_toggled_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      prefsAct->setIcon(default_hc_prefs_toggled_icon);

    }


  }

  else {

    if (darkProModeCheck->isChecked()) {
      prefsAct->setIcon(pro_prefs_dark_icon);
    } else if (lightProModeCheck->isChecked()) {
      prefsAct->setIcon(pro_prefs_icon);
    } else if (lightModeCheck->isChecked()) {
      prefsAct->setIcon(default_light_prefs_icon);
    }  else if (darkModeCheck->isChecked()) {
      prefsAct->setIcon(default_dark_prefs_icon);
    }  else if (highContrastModeCheck->isChecked()) {
      prefsAct->setIcon(default_hc_prefs_icon);

    }

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

QKeySequence MainWindow::ctrlShiftMetaKey(char key)
{
#ifdef Q_OS_MAC
  return QKeySequence(QString("Shift+Ctrl+Meta+%1").arg(key));
#else
  return QKeySequence(QString("Shift+Ctrl+alt+%1").arg(key));
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

void MainWindow::updateAction(QAction *action, QShortcut *sc, QString tooltip,  QString desc = "")
{
  QString shortcutDesc = sc->key().toString(QKeySequence::NativeText);
  action->setToolTip(tooltip + " (" + shortcutDesc + ")");
  if (desc == "")
    {
      action->setText(action->iconText() + "\t" + shortcutDesc);
    } else {
    action->setText(desc + "\t" + shortcutDesc);
  }
  action->setStatusTip(tooltip + " (" + shortcutDesc + ")");
}

void MainWindow::createShortcuts()
{
  new QShortcut(shiftMetaKey('['), this, SLOT(tabPrev()));
  new QShortcut(shiftMetaKey(']'), this, SLOT(tabNext()));
  new QShortcut(QKeySequence("F8"), this, SLOT(reloadServerCode()));
  new QShortcut(QKeySequence("F9"), this, SLOT(toggleButtonVisibility()));
  new QShortcut(shiftMetaKey('B'), this, SLOT(toggleButtonVisibility()));
  new QShortcut(QKeySequence("F10"), this, SLOT(toggleFocusMode()));
  new QShortcut(shiftMetaKey('F'), this, SLOT(toggleFullScreenMode()));
  new QShortcut(shiftMetaKey('M'), this, SLOT(toggleDarkMode()));
  new QShortcut(QKeySequence("F11"), this, SLOT(toggleLogVisibility()));
  new QShortcut(shiftMetaKey('L'), this, SLOT(toggleLogVisibility()));
  new QShortcut(QKeySequence("F12"),this, SLOT(toggleScopePaused()));
}

void  MainWindow::createToolBar()
{
  // Run
  runAct = new QAction(default_light_run_icon, tr("Run"), this);
  runSc = new QShortcut(metaKey('R'), this, SLOT(runCode()));
  updateAction(runAct, runSc, tr("Run the code in the current buffer"));
  connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

  // Stop
  stopAct = new QAction(default_light_stop_icon, tr("Stop"), this);
  stopSc = new QShortcut(metaKey('S'), this, SLOT(stopCode()));
  updateAction(stopAct, stopSc, tr("Stop all running code"));
  connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

  // Record
  recAct = new QAction(default_light_rec_icon, tr("Start Recording"), this);
  recSc = new QShortcut(shiftMetaKey('R'), this, SLOT(toggleRecording()));
  updateAction(recAct, recSc, tr("Start recording to a WAV audio file"));
  connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

  // Save
  saveAsAct = new QAction(default_light_save_icon, tr("Save"), this);
  saveAsSc = new QShortcut(shiftMetaKey('S'), this, SLOT(saveAs()));
  updateAction(saveAsAct, saveAsSc, tr("Save current buffer as an external file"));
  connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

  // Load
  loadFileAct = new QAction(default_light_load_icon, tr("Load"), this);
  loadFileSc = new QShortcut(shiftMetaKey('O'), this, SLOT(loadFile()));
  updateAction(loadFileAct, loadFileSc,  tr("Load an external file in the current buffer"));
  connect(loadFileAct, SIGNAL(triggered()), this, SLOT(loadFile()));

  // Align
  textAlignAct = new QAction(QIcon(":/images/align.png"),
        		     tr("Auto-Align Text"), this);
  textAlignSc = new QShortcut(metaKey('M'), this, SLOT(beautifyCode()));
  updateAction(textAlignAct, textAlignSc, tr("Align code to improve readability"));
  connect(textAlignAct, SIGNAL(triggered()), this, SLOT(beautifyCode()));

  // Font Size Increase
  textIncAct = new QAction(default_light_size_up_icon, tr("Text Size Up"), this);
  textIncSc = new QShortcut(metaKey('+'), this, SLOT(zoomCurrentWorkspaceIn()));
  updateAction(textIncAct, textIncSc, tr("Increase Text Size"));
  connect(textIncAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceIn()));

  // Font Size Decrease
  textDecAct = new QAction(default_light_size_down_icon, tr("Text Size Down"), this);
  textDecSc = new QShortcut(metaKey('-'), this, SLOT(zoomCurrentWorkspaceOut()));
  updateAction(textDecAct, textDecSc, tr("Decrease Text Size"));
  connect(textDecAct, SIGNAL(triggered()), this, SLOT(zoomCurrentWorkspaceOut()));

  // Scope
  scopeAct = new QAction(default_light_scope_icon, tr("Toggle Scope"), this);
  scopeSc = new QShortcut(metaKey('O'), this, SLOT(toggleScope()));
  updateAction(scopeAct, scopeSc, tr("Toggle visibility of audio oscilloscope"));
  connect(scopeAct, SIGNAL(triggered()), this, SLOT(toggleScope()));

  // Info
  infoAct = new QAction(default_light_info_icon, tr("Show Info"), this);
  infoSc = new QShortcut(metaKey('1'), this, SLOT(about()));
  updateAction(infoAct, infoSc, tr("See information about Sonic Pi"));
  connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

  // // Help
  helpAct = new QAction(default_light_help_icon, tr("Toggle Help"), this);
  helpSc = new QShortcut(metaKey('I'), this, SLOT(help()));
  updateAction(helpAct, helpSc, tr("Toggle the visibility of the help pane"));
  connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));

  // Preferences
  prefsAct = new QAction(default_light_prefs_icon, tr("Toggle Preferences"), this);
  prefsSc = new QShortcut(metaKey('P'), this, SLOT(togglePrefs()));
  updateAction(prefsAct, prefsSc, tr("Toggle the visibility of the preferences pane"));
  connect(prefsAct, SIGNAL(triggered()), this, SLOT(togglePrefs()));


  QWidget *spacer = new QWidget();
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

  toolBar->addAction(scopeAct);
  toolBar->addAction(infoAct);
  toolBar->addAction(helpAct);
  toolBar->addAction(prefsAct);

  fileMenu = menuBar()->addMenu(tr("&File"));
  fileMenu->addAction(runAct);
  fileMenu->addAction(stopAct);
  fileMenu->addAction(recAct);
  fileMenu->addAction(saveAsAct);
  fileMenu->addAction(loadFileAct);

  editMenu = menuBar()->addMenu(tr("&Edit"));
  editMenu->addAction(textIncAct);
  editMenu->addAction(textDecAct);

  windowMenu = menuBar()->addMenu(tr("&Window"));
  windowMenu->addAction(scopeAct);
  windowMenu->addAction(infoAct);
  windowMenu->addAction(helpAct);
  windowMenu->addAction(prefsAct);


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
       << "qrc:///info/COMMUNITY.html"
       << "qrc:///info/CORETEAM.html"
       << "qrc:///info/CONTRIBUTORS.html"
       << "qrc:///info/LICENSE.html"
       << "qrc:///info/CHANGELOG.html";

  tabs << tr("About")
       << tr("Community")
       << tr("Core Team")
       << tr("Contributors")
       << tr("License")
       << tr("History");

  for (int t=0; t < urls.size(); t++) {
    QTextBrowser *pane = new QTextBrowser;
    infoPanes.append(pane);
    addUniversalCopyShortcuts(pane);
    pane->setOpenExternalLinks(true);
    pane->setSource(QUrl(urls[t]));
    infoTabs->addTab(pane, tabs[t]);
  }

  infoTabs->setTabPosition(QTabWidget::South);

  QBoxLayout *infoLayout = new QBoxLayout(QBoxLayout::LeftToRight);
  infoLayout->addWidget(infoTabs);

  infoWidg = new InfoWidget;
  infoWidg->setWindowIcon(QIcon(":images/icon-smaller.png"));
  infoWidg->setLayout(infoLayout);
  infoWidg->setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
  infoWidg->setWindowTitle(tr("Sonic Pi - Info"));
  infoWidg->setFixedSize(660, 640);

  connect(infoWidg, SIGNAL(closed()), this, SLOT(about()));

  QAction *closeInfoAct = new QAction(this);
  closeInfoAct->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_W));
  connect(closeInfoAct, SIGNAL(triggered()), this, SLOT(about()));
  infoWidg->addAction(closeInfoAct);
}

void MainWindow::toggleRecordingOnIcon() {
  show_rec_icon_a = !show_rec_icon_a;
  if (darkProModeCheck->isChecked() || lightProModeCheck->isChecked()) {
    if(show_rec_icon_a) {
      recAct->setIcon(pro_rec_icon);
    } else {
      if (darkProModeCheck->isChecked()) {
        recAct->setIcon(pro_rec_b_dark_icon);
      } else {
        recAct->setIcon(pro_rec_b_icon);
      }
    }
  } else {
    if(show_rec_icon_a) {
      if (darkModeCheck->isChecked()) {
        recAct->setIcon(default_dark_rec_a_icon);
      } else if (lightModeCheck->isChecked()){
        recAct->setIcon(default_light_rec_a_icon);
      } else if (highContrastModeCheck->isChecked()){
        recAct->setIcon(default_hc_rec_a_icon);
      }
    } else {
      if (darkModeCheck->isChecked()) {
        recAct->setIcon(default_dark_rec_b_icon);
      } else if (lightModeCheck->isChecked()){
        recAct->setIcon(default_light_rec_b_icon);
      } else if (highContrastModeCheck->isChecked()){
        recAct->setIcon(default_hc_rec_b_icon);
      }
    }
  }
}

void MainWindow::toggleRecording() {
  is_recording = !is_recording;
  if(is_recording) {
    updateAction(recAct, recSc, tr("Stop Recording"), tr("Stop Recording"));
    // recAct->setStatusTip(tr("Stop Recording"));
    // recAct->setToolTip(tr("Stop Recording"));
    // recAct->setText(tr("Stop Recording"));
    rec_flash_timer->start(500);
    Message msg("/start-recording");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
  } else {
    rec_flash_timer->stop();
    updateAction(recAct, recSc, tr("Start Recording"), tr("Start Recording"));


    if (darkModeCheck->isChecked()) {
      recAct->setIcon(default_dark_rec_icon);
    } else if (lightModeCheck->isChecked()){
      recAct->setIcon(default_light_rec_icon);
    } else if (highContrastModeCheck->isChecked()){
      recAct->setIcon(default_hc_rec_icon);
    } else {
      //default icon
      recAct->setIcon(pro_rec_icon);
    }

    Message msg("/stop-recording");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
    QSettings settings("sonic-pi.net", "gui-settings");
    QString lastDir = settings.value("lastDir", QDir::homePath() + "/Desktop").toString();
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), lastDir, tr("Wavefile (*.wav)"));
    if (!fileName.isEmpty()) {
      QFileInfo fi=fileName;
      settings.setValue("lastDir", fi.dir().absolutePath());
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
  QSettings settings("sonic-pi.net", "gui-settings");
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
  QSettings settings("sonic-pi.net", "gui-settings");
  settings.setValue("pos", pos());
  settings.setValue("size", size());
  settings.setValue("first_time", 0);

  settings.setValue("prefs/midi-default-channel", midi_default_channel_combo->currentIndex());
  settings.setValue("prefs/midi-enable", midi_enable_check->isChecked());
  settings.setValue("prefs/osc-public", osc_public_check->isChecked());
  settings.setValue("prefs/osc-enabled", osc_server_enabled_check->isChecked());

  settings.setValue("prefs/check-args", check_args->isChecked());
  settings.setValue("prefs/print-output", print_output->isChecked());
  settings.setValue("prefs/clear-output-on-run", clear_output_on_run->isChecked());
  settings.setValue("prefs/log-cues", log_cues->isChecked());
  settings.setValue("prefs/log-auto-scroll", log_auto_scroll->isChecked());
  settings.setValue("prefs/show-line-numbers", show_line_numbers->isChecked());
  settings.setValue("prefs/enable-external-synths", enable_external_synths_cb->isChecked());
  settings.setValue("prefs/synth-trigger-timing-guarantees", synth_trigger_timing_guarantees_cb->isChecked());
  settings.setValue("prefs/mixer-force-mono", mixer_force_mono->isChecked());
  settings.setValue("prefs/mixer-invert-stereo", mixer_invert_stereo->isChecked());
  settings.setValue("prefs/system-vol", system_vol_slider->value());
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

  //settings.setValue("prefs/scope/show-left", show_left_scope->isChecked() );
  //settings.setValue("prefs/scope/show-right", show_right_scope->isChecked() );
  settings.setValue("prefs/scope/show-axes", show_scope_axes->isChecked() );
  settings.setValue("prefs/scope/show-scopes", show_scopes->isChecked() );
  settings.setValue("prefs/show_incoming_osc_log", show_incoming_osc_log->isChecked() );
}

void MainWindow::loadFile(const QString &fileName, SonicPiScintilla* &text)
{
  QFile file(fileName);
  if (!file.open(QFile::ReadOnly)) {
    setMessageBoxStyle();
    QMessageBox::warning(this, tr("Sonic Pi"),
			 tr("Cannot read file %1:\n%2.")
			 .arg(fileName)
			 .arg(file.errorString()));
    updateColourTheme();
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
    setMessageBoxStyle();
    QMessageBox::warning(this, tr("Sonic Pi"),
			 tr("Cannot write file %1:\n%2.")
			 .arg(fileName)
			 .arg(file.errorString()));
    updateColourTheme();
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
  std::cout << "[GUI] - stopping OSC server" << std::endl;
  sonicPiOSCServer->stop();
  if(protocol == TCP){
    clientSock->close();
  }
  if(serverProcess->state() == QProcess::NotRunning) {
    std::cout << "[GUI] - warning, server process is not running." << std::endl;
  } else {
    if (loaded_workspaces) {
      // this should be a synchorous call to avoid the following sleep
      saveWorkspaces();
    }
    sleep(1);
    std::cout << "[GUI] - asking server process to exit..." << std::endl;
    Message msg("/exit");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
  }
  if(protocol == UDP){
    osc_thread.waitForFinished();
  }
  sleep(2);

  // ensure all child processes are nuked if they didn't die gracefully
  std::cout << "[GUI] - executing exit script" << std::endl;
  QProcess* exitProcess = new QProcess();
  exitProcess->start(ruby_path, QStringList(exit_script_path));
  exitProcess->waitForFinished();

  std::cout << "[GUI] - exiting. Cheerio :-)" << std::endl;
  std::cout.rdbuf(coutbuf); // reset to stdout before exiting
}

void MainWindow::heartbeatOSC() {
  // Message msg("/gui-heartbeat");
  // msg.pushStr(guiID.toStdString());
  // sendOSC(msg);
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

void MainWindow::helpVisibilityChanged() {
  statusBar()->showMessage(tr("help visibility changed..."), 2000);

    if (docWidget->isVisible()) {
      if (darkProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_dark_bordered_icon);
      } else if (lightProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_bordered_icon);
      } else if (lightModeCheck->isChecked()) {
        helpAct->setIcon(default_light_help_toggled_icon);
      }  else if (darkModeCheck->isChecked()) {
        helpAct->setIcon(default_dark_help_toggled_icon);
      }  else if (highContrastModeCheck->isChecked()) {
        helpAct->setIcon(default_hc_help_toggled_icon);
      }
    } else {
      if (darkProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_dark_icon);
      } else if (lightProModeCheck->isChecked()) {
        helpAct->setIcon(pro_help_icon);
      } else if (lightModeCheck->isChecked()) {
        helpAct->setIcon(default_light_help_icon);
      }  else if (darkModeCheck->isChecked()) {
        helpAct->setIcon(default_dark_help_icon);
      }  else if (highContrastModeCheck->isChecked()) {
        helpAct->setIcon(default_hc_help_icon);

      }
    }
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
#if QT_VERSION >= 0x050400
  qDebug().noquote() << s;
#else
  //noquote requires QT 5.4
  qDebug() << s;
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
  versionLabel->setText(QString("Sonic Pi " + v + " on " + platform + " "));

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

void MainWindow::addCuePath(QString path, QString val)
{
  Q_UNUSED(val);
  
  if (!path.startsWith(":"))  {
      path =  "\"" + path + "\"";
  }

  if (!cuePaths.contains(path)) {
      autocomplete->addCuePath(path);
      cuePaths << path;
  }
}


void MainWindow::setupLogPathAndRedirectStdOut() {
  QDir().mkdir(sp_user_path);
  QDir().mkdir(log_path);

  if(homeDirWritable) {
    coutbuf = std::cout.rdbuf();
    stdlog.open(gui_log_path.toStdString().c_str());
    std::cout.rdbuf(stdlog.rdbuf());
  }
}

void MainWindow::toggleMidi(int silent) {
  if (midi_enable_check->isChecked()) {
    statusBar()->showMessage(tr("Enabling MIDI..."), 2000);
    Message msg("/midi-start");
    msg.pushStr(guiID.toStdString());
    msg.pushInt32(silent);
    sendOSC(msg);
  } else {
    midi_in_ports_label->setText(tr("No connected input devices"));
    midi_out_ports_label->setText(tr("No connected output devices"));
    statusBar()->showMessage(tr("Disabling MIDI..."), 2000);
    Message msg("/midi-stop");
    msg.pushStr(guiID.toStdString());
    msg.pushInt32(silent);
    sendOSC(msg);
  }
}

void MainWindow::toggleOSCServer(int silent) {
  if (osc_server_enabled_check->isChecked()) {
    statusBar()->showMessage(tr("Opening OSC port for remote messages..."), 2000);
    int open = osc_public_check->isChecked() ? 1 : 0;

    Message msg("/osc-port-start");
    msg.pushStr(guiID.toStdString());
    msg.pushInt32(silent);
    msg.pushInt32(open);
    sendOSC(msg);
  } else {
    statusBar()->showMessage(tr("Stopping OSC server..."), 2000);
    Message msg("/osc-port-stop");
    msg.pushStr(guiID.toStdString());
    msg.pushInt32(silent);
    sendOSC(msg);
  }
}

void MainWindow::resetMidi() {
  if (midi_enable_check->isChecked()) {
    midi_in_ports_label->setText(tr("No connected input devices"));
    midi_out_ports_label->setText(tr("No connected output devices"));
    statusBar()->showMessage(tr("Resetting MIDI..."), 2000);
    Message msg("/midi-reset");
    msg.pushStr(guiID.toStdString());
    sendOSC(msg);
  } else {
    statusBar()->showMessage(tr("MIDI is disabled..."), 2000);
  }
}

bool MainWindow::eventFilter(QObject *obj, QEvent *evt)
{
    if(obj==qApp && ( evt->type() == QEvent::ApplicationActivate ))
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





QString MainWindow::sonicPiHomePath() {
  QString path = qgetenv("SONIC_PI_HOME").constData();
  if (path.isEmpty()) {
    return QDir::homePath();
  }
  else {
    return path;
  }
}

void MainWindow::zoomInLogs() {
  outputPane->zoomIn();
  incomingPane->zoomIn();
}

void MainWindow::zoomOutLogs() {
  outputPane->zoomOut();
  incomingPane->zoomOut();
}
#include "ruby_help.h"


void MainWindow::updateMIDIInPorts(QString port_info) {

  QString input_header = tr("Connected MIDI inputs") + ":\n\n";
  midi_in_ports_label->setText(input_header + port_info);
}

void MainWindow::updateMIDIOutPorts(QString port_info) {
  QString output_header = tr("Connected MIDI outputs") + ":\n\n";
  midi_out_ports_label->setText(output_header + port_info);
}
