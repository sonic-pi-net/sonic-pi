//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
// notice is included.
//++

// Standard stuff
#include <iostream>
#include <math.h>
#include <sstream>
#include <fstream>

// Qt stuff
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

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include <Qsci/qscicommandset.h>

#include "sonicpilexer.h"
#include "sonicpiapis.h"
#include "sonicpiscintilla.h"

// OSC stuff
#include "oscpkt.hh"
#include "udp.hh"

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

using namespace oscpkt;

#ifdef Q_OS_MAC
MainWindow::MainWindow(QApplication &app, QMainWindow* splash) {
#else
MainWindow::MainWindow(QApplication &app, QSplashScreen &splash) {
#endif
  this->setUnifiedTitleAndToolBarOnMac(true);

  is_recording = false;
  server_started = false;
  show_rec_icon_a = false;
  cont_listening_for_osc = true;
  osc_incoming_port_open = false;

  rec_flash_timer = new QTimer(this);
  connect(rec_flash_timer, SIGNAL(timeout()), this, SLOT(toggleRecordingOnIcon()));

  osc_thread = QtConcurrent::run(this, &MainWindow::startOSCListener);
  serverProcess = new QProcess();

#if defined(Q_OS_WIN)
  QString prg_path = "ruby.exe";
  QString native_path = QCoreApplication::applicationDirPath() + "\\..\\..\\..\\server\\native\\windows\\bin\\ruby.exe";
  std::ifstream testfile(native_path.toUtf8().constData());
  if (testfile) {
    prg_path = native_path;
    testfile.close();
  }

  QString prg_arg = QCoreApplication::applicationDirPath() + "/../../../server/bin/sonic-pi-server.rb";
  QString sample_path = QCoreApplication::applicationDirPath() + "/../../../../etc/samples";
#elif defined(Q_OS_MAC)
  QString prg_path = QCoreApplication::applicationDirPath() + "/../../server/native/osx/ruby/bin/ruby";
  QString prg_arg = QCoreApplication::applicationDirPath() + "/../../server/bin/sonic-pi-server.rb";
  QString sample_path = QCoreApplication::applicationDirPath() + "/../../etc/samples";
#else
  //assuming Raspberry Pi
  QString prg_path = "ruby";
  QString prg_arg = QCoreApplication::applicationDirPath() + "/../../server/bin/sonic-pi-server.rb";
  QString sample_path = QCoreApplication::applicationDirPath() + "/../../../etc/samples";
#endif

  prg_arg = QDir::toNativeSeparators(prg_arg);

  QStringList args;
  args << prg_arg;

  std::cout << prg_path.toStdString() << " " << prg_arg.toStdString() << std::endl;

  QString sp_user_path = QDir::homePath() + QDir::separator() + ".sonic-pi";
  QString log_path =  sp_user_path + QDir::separator() + "log";

#if defined(Q_OS_WIN)
  stdlog.open(QString(log_path + "/stdout.log").toStdString());
  std::cout.rdbuf(stdlog.rdbuf());
#endif

  QDir().mkdir(sp_user_path);
  QDir().mkdir(log_path);
  QString sp_error_log_path = log_path + QDir::separator() + "/errors.log";
  QString sp_output_log_path = log_path + QDir::separator() + "/output.log";
  serverProcess->setStandardErrorFile(sp_error_log_path);
  serverProcess->setStandardOutputFile(sp_output_log_path);
  serverProcess->start(prg_path, args);
  if (!serverProcess->waitForStarted()) {
    QMessageBox::critical(this, tr("Where is ruby?"), tr("ruby could not be started, is it installed and in your PATH?"), QMessageBox::Abort);
    QTimer::singleShot(0, this, SLOT(close()));
    cont_listening_for_osc = false;
    return;
  }

  std::cerr << "started..." << serverProcess->state() << std::endl;

  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);
  // create workspaces and add them to the tabs
  for(int ws = 0; ws < workspace_max; ws++) {
	  std::string s;

	  workspaces[ws] = new SonicPiScintilla;
	  QString w = QString("Workspace %1").arg(QString::number(ws + 1));
	  tabs->addTab(workspaces[ws], w);
  }

  lexer = new SonicPiLexer;
  lexer->setAutoIndentStyle(SonicPiScintilla::AiMaintain);

  // Autocompletion stuff
  autocomplete = new SonicPiAPIs(lexer);
  autocomplete->loadSamples(sample_path);

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);


  // Setup output and error panes
  outputPane = new QTextEdit;
  errorPane = new QTextEdit;

  outputPane->setReadOnly(true);
  errorPane->setReadOnly(true);
  outputPane->setLineWrapMode(QTextEdit::NoWrap);
#if defined(Q_OS_WIN)
  outputPane->setFontFamily("Courier New");
#elif defined(Q_OS_MAC)
  outputPane->setFontFamily("Menlo");
#else
  outputPane->setFontFamily("Bitstream Vera Sans Mono");
#endif

  outputPane->document()->setMaximumBlockCount(1000);
  errorPane->document()->setMaximumBlockCount(1000);

  outputPane->zoomIn(1);
  errorPane->zoomIn(1);
  errorPane->setMaximumHeight(100);

  prefsWidget = new QDockWidget(tr("Preferences"), this);
  prefsWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  prefsWidget->setFeatures(QDockWidget::DockWidgetClosable);
  prefsCentral = new QWidget;
  prefsWidget->setWidget(prefsCentral);
  addDockWidget(Qt::RightDockWidgetArea, prefsWidget);
  prefsWidget->hide();
  prefsWidget->setObjectName("prefs");

  outputWidget = new QDockWidget(tr("Log"), this);
  outputWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);
  outputWidget->setObjectName("output");

  docsCentral = new QTabWidget;
  docsCentral->setTabsClosable(false);
  docsCentral->setMovable(false);
  docsCentral->setTabPosition(QTabWidget::West);

  docPane = new QTextBrowser;
  docPane->setOpenExternalLinks(true);
  QString style = "QTextBrowser { padding-left:10; padding-top:10; padding-bottom:10; padding-right:10 ; background:white;}";
  docPane->setStyleSheet(style);
  docPane->setHtml("<center><img src=\":/images/logo.png\" height=\"298\" width=\"365\"></center>");

  QHBoxLayout *docLayout = new QHBoxLayout;
  docLayout->addWidget(docsCentral);
  docLayout->addWidget(docPane, 1);
  QWidget *docW = new QWidget();
  docW->setLayout(docLayout);

  docWidget = new QDockWidget("Help", this);
  docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
  docWidget->setWidget(docW);
  docWidget->setObjectName("help");

  addDockWidget(Qt::BottomDockWidgetArea, docWidget);
  docWidget->hide();

  QVBoxLayout *mainWidgetLayout = new QVBoxLayout;
  mainWidgetLayout->addWidget(tabs);
  mainWidgetLayout->addWidget(errorPane);
  QWidget *mainWidget = new QWidget;
  errorPane->hide();
  mainWidget->setLayout(mainWidgetLayout);
  setCentralWidget(mainWidget);

  // initialise workspaces
  for(int ws = 0; ws < workspace_max; ws++) {
	  initWorkspace(workspaces[ws]);
  }

  createActions();
  createToolBar();
  createStatusBar();

  readSettings();

  setWindowTitle(tr("Sonic Pi"));

  connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );

  int timeout = 30;
  while (!server_started && cont_listening_for_osc && timeout-- > 0) {
    sleep(1);
    std::cout << "Waiting for server..." << std::endl;
    if(osc_incoming_port_open) {
      Message msg("/ping");
      msg.pushStr("QtClient/1/hello");
      sendOSC(msg);
    }
  }
  if (!server_started) {
    QMessageBox::critical(this, QString("Server didn't start"), QString("Failed to start server, please check ") + log_path, QMessageBox::Abort);
    QTimer::singleShot(0, this, SLOT(close()));
    cont_listening_for_osc = false;

    return;
  }

  loadWorkspaces();

  raspberryPiSystemVol = new QSlider(this);
  connect(raspberryPiSystemVol, SIGNAL(valueChanged(int)), this, SLOT(changeRPSystemVol(int)));
  initPrefsWindow();
  initDocsWindow();


  infoPane = new QTextBrowser;
  infoPane->setOpenExternalLinks(true);
  infoPane->setFixedSize(550, 650);
  QString html;

  infoPane->setHtml("<center><img src=\":/images/logo.png\" height=\"298\" width=\"365\"><pre><font size=\"4\"><font color=\"DeepPink\">A Sound Synthesiser<br>for Live Coding</font><br><br>Designed and developed by Sam Aaron<br>in Cambridge, England<br><br><font color=\"DeepPink\"><a href=\"http://sonic-pi.net\" style=\"text-decoration: none; color:DeepPink\">http://sonic-pi.net</a></font><br><br>For the latest updates follow<br><font color=\"DeepPink\"><a href=\"http://twitter.com/sonic_pi\" style=\"text-decoration: none; color:DeepPink;\">@sonic_pi</a><br></font></font></pre><h2><pre><font color=\"#3C3C3C\"><pre>music_as <font color=\"DeepPink\">:code</font><br>code_as <font color=\"DeepPink\">:art</font></pre></h2><pre><font size=\"4\"><br>v2.0.1</font></pre></center>");


#if defined(Q_OS_MAC)
  splash->close();
#else
  splash.finish(this);
#endif

    QSettings settings("uk.ac.cam.cl", "Sonic Pi");

  if(settings.value("first_time", 1).toInt() == 1) {
    QTextEdit* startupPane = new QTextEdit;
    startupPane->setReadOnly(true);
    startupPane->setFixedSize(550, 700);
    QString html;



    startupPane->setHtml("<center><img src=\":/images/logo.png\" height=\"298\" width=\"365\"></center><center><pre><font size=\"4\"><font color=\"DeepPink\">Welcome!</font><br><br>This is Sonic Pi<br>the live coding music environment<br><br>To get started please follow the tutorial<br> in the help system below<br>(which you can always access via the Help button)<br><br>Remember...<br>with live coding<br>there are no mistakes<br>only opportunities<font color=\"DeepPink\"><br><br>Have fun and share your code<br>for others to jam with</font></font></center>");
    docWidget->show();
    startupPane->show();
  }

  // Main widget for info
  infoWidg = new QWidget;


  // Changelog
  QFile file(":/info/CHANGELOG.html");
  if(!file.open(QFile::ReadOnly | QFile::Text)) {
  }
  QString s;
  QTextStream st(&file);
  s.append(st.readAll());
  QTextBrowser *historyT = new QTextBrowser;
  historyT->setOpenExternalLinks(true);
  historyT->setHtml(s);

  //Contributors
  QFile file2(":/info/CONTRIBUTORS.html");
  if(!file2.open(QFile::ReadOnly | QFile::Text)) {
  }
  QString s2;
  QTextStream st2(&file2);
  s2.append(st2.readAll());
  QTextBrowser *contributorsT = new QTextBrowser;
  contributorsT->setOpenExternalLinks(true);
  contributorsT->setHtml(s2);

  //Community
  QFile file3(":/info/COMMUNITY.html");
  if(!file3.open(QFile::ReadOnly | QFile::Text)) {
  }
  QString s3;
  QTextStream st3(&file3);
  s3.append(st3.readAll());
  QTextBrowser *communityT = new QTextBrowser;
  communityT->setOpenExternalLinks(true);
  communityT->setHtml(s3);

  currentLine = 0; currentIndex = 0;


  // Tabs
  QTabWidget *infoTabs = new QTabWidget(this);
  infoTabs->addTab(infoPane, "About");
  infoTabs->addTab(historyT, "History");
  infoTabs->addTab(contributorsT, "Contributors");
  infoTabs->addTab(communityT, "Community");
  infoTabs->setTabPosition(QTabWidget::South);

  QBoxLayout *infoLayout = new QBoxLayout(QBoxLayout::LeftToRight);
  infoLayout->addWidget(infoTabs);

  infoWidg->setLayout(infoLayout);

  infoWidg->setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint);

  QAction *closeInfoAct = new QAction(this);
  closeInfoAct->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_W));
  connect(closeInfoAct, SIGNAL(triggered()), this, SLOT(about()));
  infoWidg->addAction(closeInfoAct);
  this->showNormal();
}

void MainWindow::serverError(QProcess::ProcessError error) {
  cont_listening_for_osc = false;
  std::cout << "SERVER ERROR" << error <<std::endl;
  std::cout << serverProcess->readAllStandardError().data() << std::endl;
  std::cout << serverProcess->readAllStandardOutput().data() << std::endl;
}

void MainWindow::serverFinished(int exitCode, QProcess::ExitStatus exitStatus) {
  std::cout << "SERVER Finished: " << exitCode << std::endl;
  std::cout << serverProcess->readAllStandardError().data() << std::endl;
  std::cout << serverProcess->readAllStandardOutput().data() << std::endl;
}

void MainWindow::update_mixer_invert_stereo() {
  if(mixer_invert_stereo->isChecked())
    {
      mixerInvertStereo();
    } else {
    mixerStandardStereo();
  }
}

void MainWindow::update_mixer_force_mono() {
  if(mixer_force_mono->isChecked())
    {
      mixerMonoMode();
    } else {
    mixerStereoMode();
  }
}

void MainWindow::initPrefsWindow() {

  QGridLayout *grid = new QGridLayout;

  QGroupBox *volBox = new QGroupBox(tr("Raspberry Pi System Volume"));
  volBox->setToolTip("Use this slider to change the system volume of your Raspberry Pi");

  QGroupBox *advancedAudioBox = new QGroupBox(tr("Studio Settings"));
  advancedAudioBox->setToolTip("Advanced audio settings for working with external PA systems when performing with Sonic Pi");
  mixer_invert_stereo = new QCheckBox("Invert Stereo");
  connect(mixer_invert_stereo, SIGNAL(clicked()), this, SLOT(update_mixer_invert_stereo()));
  mixer_force_mono = new QCheckBox("Force Mono");
  connect(mixer_force_mono, SIGNAL(clicked()), this, SLOT(update_mixer_force_mono()));


  QVBoxLayout *advanced_audio_box_layout = new QVBoxLayout;
  advanced_audio_box_layout->addWidget(mixer_invert_stereo);
  advanced_audio_box_layout->addWidget(mixer_force_mono);
  // audio_box->addWidget(radio2);
  // audio_box->addWidget(radio3);
  // audio_box->addStretch(1);
  advancedAudioBox->setLayout(advanced_audio_box_layout);


  QGroupBox *audioOutputBox = new QGroupBox(tr("Force Audio Output"));
  audioOutputBox->setToolTip("Your Raspberry Pi has two forms of audio output. \nFirstly, there is the headphone jack of the Raspberry Pi itself. \nSecondly, some HDMI monitors/TVs support audio through the HDMI port. \nUse these buttons to force the output to the one you want. \nFor example, if you have headphones connected to your Raspberry Pi, choose 'Headphones'. ");
  QRadioButton *radio1 = new QRadioButton(tr("&Default"));
  QRadioButton *radio2 = new QRadioButton(tr("&Headphones"));
  QRadioButton *radio3 = new QRadioButton(tr("&HDMI"));
  radio1->setChecked(true);

  connect(radio1, SIGNAL(clicked()), this, SLOT(setRPSystemAudioAuto()));
  connect(radio2, SIGNAL(clicked()), this, SLOT(setRPSystemAudioHeadphones()));
  connect(radio3, SIGNAL(clicked()), this, SLOT(setRPSystemAudioHDMI()));

  QVBoxLayout *audio_box = new QVBoxLayout;
  audio_box->addWidget(radio1);
  audio_box->addWidget(radio2);
  audio_box->addWidget(radio3);
  audio_box->addStretch(1);
  audioOutputBox->setLayout(audio_box);

  QHBoxLayout *vol_box = new QHBoxLayout;
  vol_box->addWidget(raspberryPiSystemVol);
  volBox->setLayout(vol_box);

  QGroupBox *debug_box = new QGroupBox("Debug Options");
  print_output = new QCheckBox("Print output");
  check_args = new QCheckBox("Check synth args");
  clear_output_on_run = new QCheckBox("Clear output on run");
  print_output->setChecked(true);
  check_args->setChecked(true);
  clear_output_on_run->setChecked(true);

  QVBoxLayout *debug_box_layout = new QVBoxLayout;
  debug_box_layout->addWidget(print_output);
  debug_box_layout->addWidget(check_args);
  debug_box_layout->addWidget(clear_output_on_run);
  debug_box->setLayout(debug_box_layout);

#if defined(Q_OS_LINUX)
  grid->addWidget(audioOutputBox, 1, 0);
  grid->addWidget(volBox, 1, 1);
#endif
  grid->addWidget(debug_box, 0, 1);
  grid->addWidget(advancedAudioBox, 0, 0);
  prefsCentral->setLayout(grid);
}

 void MainWindow::addOtherKeyBinding(QSettings &qs, int cmd, int key)
 {
   QString skey;
   skey.sprintf("/Scintilla/keymap/c%d/alt", cmd);
   qs.setValue(skey, key);
 }

  void MainWindow::addKeyBinding(QSettings &qs, int cmd, int key)
 {
   QString skey;
   skey.sprintf("/Scintilla/keymap/c%d/key", cmd);
   qs.setValue(skey, key);
 }

 void MainWindow::initWorkspace(SonicPiScintilla* ws) {
   ws->standardCommands()->clearKeys();
   ws->standardCommands()->clearAlternateKeys();
   QString skey;
   QSettings settings("Sonic Pi", "Key bindings");

#if defined(Q_OS_MAC)
   int SPi_CTRL = Qt::META;
   int SPi_META = Qt::CTRL;
#else
   int SPi_CTRL = Qt::CTRL;
   int SPi_META = Qt::ALT;
#endif


   // basic navigation
  addKeyBinding(settings, QsciCommand::LineDown, Qt::Key_N | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::LineDown, Qt::Key_Down);
  addKeyBinding(settings, QsciCommand::LineDownExtend, Qt::Key_Down | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::LineUp, Qt::Key_P | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::LineUp, Qt::Key_Up);
  addKeyBinding(settings, QsciCommand::LineUpExtend, Qt::Key_Up | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::CharRight, Qt::Key_F | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::CharRight, Qt::Key_Right);
  addKeyBinding(settings, QsciCommand::CharRightExtend, Qt::Key_Right | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::WordRight, Qt::Key_F | SPi_CTRL | Qt::SHIFT);
  addOtherKeyBinding(settings, QsciCommand::WordRight, Qt::Key_Right | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::WordRightExtend, Qt::Key_Right | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_B | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::CharLeft, Qt::Key_Left);
  addKeyBinding(settings, QsciCommand::CharLeftExtend, Qt::Key_Left | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::WordLeft, Qt::Key_B | SPi_CTRL | Qt::SHIFT);
  addOtherKeyBinding(settings, QsciCommand::WordLeft, Qt::Key_Left | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::WordLeftExtend, Qt::Key_Left | SPi_CTRL | Qt::SHIFT);

  addKeyBinding(settings, QsciCommand::Delete, Qt::Key_D | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_H | SPi_CTRL);
  addOtherKeyBinding(settings, QsciCommand::DeleteBack, Qt::Key_Backspace);

  addKeyBinding(settings, QsciCommand::Home, Qt::Key_A | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::LineEnd, Qt::Key_E | SPi_CTRL);

  addKeyBinding(settings, QsciCommand::Delete, Qt::Key_D | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::DeleteLineRight, Qt::Key_K | SPi_CTRL);
  addKeyBinding(settings, QsciCommand::VerticalCentreCaret, Qt::Key_L | SPi_CTRL);

  addKeyBinding(settings, QsciCommand::Cancel, Qt::Key_Escape);

  // tab return
  addKeyBinding(settings, QsciCommand::Newline, Qt::Key_Return);
  addKeyBinding(settings, QsciCommand::Tab, Qt::Key_Tab);

  // copy paste
  addKeyBinding(settings, QsciCommand::SelectionCut, Qt::Key_X | SPi_META);
  addKeyBinding(settings, QsciCommand::SelectionCopy, Qt::Key_C | SPi_META);
  addKeyBinding(settings, QsciCommand::Paste, Qt::Key_V | SPi_META);
  addKeyBinding(settings, QsciCommand::Undo, Qt::Key_Z | SPi_META);
  addKeyBinding(settings, QsciCommand::Redo, Qt::Key_Z | Qt::SHIFT | SPi_META);
  addKeyBinding(settings, QsciCommand::SelectAll, Qt::Key_A | SPi_META);

  ws->standardCommands()->readSettings(settings);

  ws->setAutoIndent(true);
  ws->setIndentationsUseTabs(false);
  ws->setBackspaceUnindents(true);
  ws->setTabIndents(true);
  ws->setMatchedBraceBackgroundColor(QColor("dimgray"));
  ws->setMatchedBraceForegroundColor(QColor("white"));

  ws->setIndentationWidth(2);
  ws->setIndentationGuides(true);
  ws->setIndentationGuidesForegroundColor(QColor("deep pink"));
  ws->setBraceMatching( SonicPiScintilla::SloppyBraceMatch);
  //TODO: add preference toggle for this:
  //ws->setFolding(SonicPiScintilla::CircledTreeFoldStyle, 2);
  ws->setCaretLineVisible(true);
  ws->setCaretLineBackgroundColor(QColor("whitesmoke"));
  ws->setFoldMarginColors(QColor("whitesmoke"),QColor("whitesmoke"));
  ws->setMarginLineNumbers(0, true);
  ws->setMarginWidth(0, "1000000");
  ws->setMarginsBackgroundColor(QColor("whitesmoke"));
  ws->setMarginsForegroundColor(QColor("dark gray"));
  ws->setMarginsFont(QFont("Menlo",5, -1, true));
  ws->setUtf8(true);
  ws->setText("#loading...");
  ws->setLexer(lexer);
  ws->setAutoCompletionThreshold(1);
  ws->setAutoCompletionSource(SonicPiScintilla::AcsAPIs);
  ws->setSelectionBackgroundColor("DeepPink");
  ws->setSelectionForegroundColor("white");
  ws->setCaretWidth(5);
  ws->setCaretForegroundColor("deep pink");

}

void MainWindow::startOSCListener() {
  std::cout << "starting OSC Server" << std::endl;
  int PORT_NUM = 4558;
  UdpSocket sock;
  sock.bindTo(PORT_NUM);
  std::cout << "Listening on port 4558" << std::endl;
  if (!sock.isOk()) {
    std::cout << "Unable to listen to OSC messages on port 4558" << std::endl;
  } else {
    PacketReader pr;
    PacketWriter pw;
    osc_incoming_port_open = true;
    while (sock.isOk() && cont_listening_for_osc) {

      if (sock.receiveNextPacket(30 /* timeout, in ms */)) {
        pr.init(sock.packetData(), sock.packetSize());
        oscpkt::Message *msg;
        while (pr.isOk() && (msg = pr.popMessage()) != 0) {


          if (msg->match("/multi_message")){
            int msg_count;
            int msg_type;
            int job_id;
            std::string thread_name;
            std::string runtime;
            std::string s;
            std::ostringstream ss;

            Message::ArgReader ar = msg->arg();
            ar.popInt32(job_id);
            ar.popStr(thread_name);
            ar.popStr(runtime);
            ar.popInt32(msg_count);
            QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
            ss << "[Run " << job_id;
            ss << ", Time " << runtime;
            if(!thread_name.empty()) {
              ss << ", Thread :" << thread_name;
            }
            ss << "]";
            QMetaObject::invokeMethod( outputPane, "append", Qt::QueuedConnection,
                                       Q_ARG(QString, QString::fromStdString(ss.str())) );

            for(int i = 0 ; i < msg_count ; i++) {
              ss.str("");
              ss.clear();
              ar.popInt32(msg_type);
              ar.popStr(s);

#if defined(Q_OS_WIN)
              if(i == (msg_count - 1)) {
                ss << " └─ ";
              } else {
                ss << " ├─ ";
              }
#elif defined(Q_OS_MAC)
              if(i == (msg_count - 1)) {
                ss << " └─ ";
              } else {
                ss << " ├─ ";
              }
#else
  //assuming Raspberry Pi
              if(i == (msg_count - 1)) {
                ss << " +- ";
              } else {
                ss << " |- ";
              }
#endif


              QMetaObject::invokeMethod( outputPane, "append", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(ss.str())) );


              ss.str("");
              ss.clear();

              switch(msg_type)
                {
                case 0:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("deeppink")));
                  break;
                case 1:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("dodgerblue")));
                  break;
                case 2:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("darkorange")));
                  break;
                case 3:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("red")));
                  break;
                case 4:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
                  QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("deeppink")));
                  break;
                case 5:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
                  QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("dodgerblue")));
                  break;
                case 6:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
                  QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("darkorange")));
                  break;
                default:
                  QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("green")));
                }

              ss << s;

              QMetaObject::invokeMethod( outputPane, "insertPlainText", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(ss.str())) );

              QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
              QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));



              }
            QMetaObject::invokeMethod( outputPane, "append", Qt::QueuedConnection,
                                       Q_ARG(QString,  QString::fromStdString(" ")) );
          }
          else if (msg->match("/info")) {
            std::string s;
            if (msg->arg().popStr(s).isOkNoMoreArgs()) {
              // Evil nasties!
              // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html

              QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
              QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));

              QMetaObject::invokeMethod( outputPane, "append", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString("=> " + s + "\n")) );

              QMetaObject::invokeMethod( outputPane, "setTextColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("#5e5e5e")));
              QMetaObject::invokeMethod( outputPane, "setTextBackgroundColor", Qt::QueuedConnection, Q_ARG(QColor, QColor("white")));
            } else {
              std::cout << "Server: unhandled info message: "<< std::endl;
            }
          }
          else if (msg->match("/error")) {
            int job_id;
            std::string desc;
            std::string backtrace;
            if (msg->arg().popInt32(job_id).popStr(desc).popStr(backtrace).isOkNoMoreArgs()) {
              // Evil nasties!
              // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
              QMetaObject::invokeMethod( errorPane, "show", Qt::QueuedConnection);
              QMetaObject::invokeMethod( errorPane, "clear", Qt::QueuedConnection);
              QMetaObject::invokeMethod( errorPane, "setHtml", Qt::QueuedConnection,
                                         Q_ARG(QString, "<table width=\"100%\"> cellpadding=\"2\"><tr><td bgcolor=\"#FFE4E1\"><h3><font color=\"black\"><pre>Error: " + QString::fromStdString(desc) + "</pre></font></h3></td></tr><tr><td bgcolor=\"#E8E8E8\"><h4><font color=\"#5e5e5e\", background-color=\"black\"><pre>" + QString::fromStdString(backtrace) + "</pre></font></h4></td></tr></table>") );

            } else {
              std::cout << "Server: unhandled error: "<< std::endl;
            }
          }
          else if (msg->match("/replace-buffer")) {
            std::string id;
            std::string content;
            if (msg->arg().popStr(id).popStr(content).isOkNoMoreArgs()) {

              QMetaObject::invokeMethod( this, "replaceBuffer", Qt::QueuedConnection, Q_ARG(QString, QString::fromStdString(id)), Q_ARG(QString, QString::fromStdString(content)));
            } else {
              std::cout << "Server: unhandled replace-buffer: "<< std::endl;
            }
          }
          else if (msg->match("/exited")) {
            if (msg->arg().isOkNoMoreArgs()) {
              std::cout << "server asked us to exit" << std::endl;
              cont_listening_for_osc = false;
            } else {
              std::cout << "Server: unhandled exited: "<< std::endl;
            }
          }
          else if (msg->match("/ack")) {
            std::string id;
            if (msg->arg().popStr(id).isOkNoMoreArgs()) {
              server_started = true;
            } else
              std::cout << "Server: unhandled ack " << std::endl;
          }
          else {
            std::cout << "Unknown message" << std::endl;
          }
        }
      }
    }
  }
  std::cout << "OSC Stopped, releasing socket" << std::endl;
  sock.close();
}

void MainWindow::replaceBuffer(QString id, QString content) {
  SonicPiScintilla* ws = filenameToWorkspace(id.toStdString());
  int line;
  int index;
  QString line_content;
  int line_length;
  int new_line_length;
  ws->getCursorPosition(&line, &index);
  line_content = ws->text(line);
  line_length = line_content.length();
  ws->selectAll();
  ws->replaceSelectedText(content);
  if(ws->lineLength(line) == -1) {
    // new text is clearly different from old, just put cursor at start
    // of buffer
    ws->setCursorPosition(0, 0);
  }
  else {
    line_content = ws->text(line);
    new_line_length = line_content.length();
    int diff = new_line_length - line_length;
    ws->setCursorPosition(line, index + diff);
  }
}

std::string MainWindow::number_name(int i) {
	switch(i) {
	case 1: return "one";
	case 2: return "two";
	case 3: return "three";
	case 4: return "four";
	case 5: return "five";
	case 6: return "six";
	case 7: return "seven";
	case 8: return "eight";
	default: assert(false);
	}
}

void MainWindow::loadWorkspaces()
{
  std::cout << "loading workspaces" << std::endl;;

  for(int i = 0; i < workspace_max; i++) {
	  Message msg("/load-buffer");
	  std::string s = "workspace_" + number_name(i + 1);
	  msg.pushStr(s);
	  sendOSC(msg);
  }
}

void MainWindow::saveWorkspaces()
{
  std::cout << "saving workspaces" << std::endl;;

  for(int i = 0; i < workspace_max; i++) {
	  std::string code = workspaces[i]->text().toStdString();
	  Message msg("/save-buffer");
	  std::string s = "workspace_" + number_name(i + 1);
	  msg.pushStr(s);
	  msg.pushStr(code);
	  sendOSC(msg);
  }
}

void MainWindow::closeEvent(QCloseEvent *event)
{
  writeSettings();
  event->accept();
}

QString MainWindow::currentTabLabel()
{
  return tabs->tabText(tabs->currentIndex());
}


bool MainWindow::saveAs()
{
  QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Workspace"), QDir::homePath() + "/Desktop");
  if(!fileName.isEmpty()){
    return saveFile(fileName, (SonicPiScintilla*)tabs->currentWidget());
  } else {
    return false;
  }
}

 void MainWindow::sendOSC(Message m)
{
  UdpSocket sock;
  int PORT_NUM = 4557;
  sock.connectTo("localhost", PORT_NUM);
  if (!sock.isOk()) {
    std::cerr << "Error connection to port " << PORT_NUM << ": " << sock.errorMessage() << "\n";
  } else {

    PacketWriter pw;
    pw.addMessage(m);
    sock.sendPacket(pw.packetData(), pw.packetSize());
  }
}

void MainWindow::runCode()
{


  SonicPiScintilla *ws = ((SonicPiScintilla*)tabs->currentWidget());
  if (currentLine == 0 && currentIndex == 0) {
    // only update saved position if we're not already highlighting code
    ws->getCursorPosition(&currentLine, &currentIndex);
  }
  ws->setReadOnly(true);
  ws->selectAll();
  errorPane->clear();
  errorPane->hide();
  statusBar()->showMessage(tr("Running Code...."), 1000);
  std::string code = ((SonicPiScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/save-and-run-buffer");
  std::string filename = workspaceFilename( (SonicPiScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  if(!print_output->isChecked()) {
    code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code ;
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
  sendOSC(msg);

  QTimer::singleShot(500, this, SLOT(unhighlightCode()));


}

void MainWindow::unhighlightCode()
{
  SonicPiScintilla *ws = (SonicPiScintilla *)tabs->currentWidget();
  ws->selectAll(false);
  if (currentLine != 0 || currentIndex != 0) {
    ws->setCursorPosition(currentLine, currentIndex);
    currentLine = 0; currentIndex = 0;
  }
  ws->setReadOnly(false);
}

 void MainWindow::beautifyCode()
 {
  statusBar()->showMessage(tr("Beautifying...."), 2000);
  std::string code = ((SonicPiScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/beautify-buffer");
  std::string filename = workspaceFilename( (SonicPiScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  msg.pushStr(code);
  sendOSC(msg);
}

void MainWindow::reloadServerCode()
{
  statusBar()->showMessage(tr("reloading...."), 2000);
  Message msg("/reload");
  sendOSC(msg);
}

void MainWindow::mixerHpfEnable(float freq)
{
  statusBar()->showMessage(tr("enabling mixer HPF...."), 2000);
  Message msg("/mixer-hpf-enable");
  msg.pushFloat(freq);
  sendOSC(msg);
}

void MainWindow::mixerHpfDisable()
{
  statusBar()->showMessage(tr("disabling mixer HPF...."), 2000);
  Message msg("/mixer-hpf-disable");
  sendOSC(msg);
}

void MainWindow::mixerLpfEnable(float freq)
{
  statusBar()->showMessage(tr("enabling Mixer HPF...."), 2000);
  Message msg("/mixer-lpf-enable");
  msg.pushFloat(freq);
  sendOSC(msg);
}

void MainWindow::mixerLpfDisable()
{
  statusBar()->showMessage(tr("disabling mixer LPF...."), 2000);
  Message msg("/mixer-lpf-disable");
  sendOSC(msg);
}

void MainWindow::mixerInvertStereo()
{
  statusBar()->showMessage(tr("enabling inverted stereo...."), 2000);
  Message msg("/mixer-invert-stereo");
  sendOSC(msg);
}

void MainWindow::mixerStandardStereo()
{
  statusBar()->showMessage(tr("enabling standard stereo...."), 2000);
  Message msg("/mixer-standard-stereo");
  sendOSC(msg);
}

void MainWindow::mixerMonoMode()
{
  statusBar()->showMessage(tr("mono mode...."), 2000);
  Message msg("/mixer-mono-mode");
  sendOSC(msg);
}

void MainWindow::mixerStereoMode()
{
  statusBar()->showMessage(tr("stereo mode...."), 2000);
  Message msg("/mixer-stereo-mode");
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
    for (end = pos; end < text.length(); end++) {
      if (!text[end].isLetter() && text[end] != '_') break;
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

void MainWindow::changeRPSystemVol(int val) {
#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing, just print out what it would do on RPi
  float v = (float) val;
  float vol_float = pow(v/100.0, (float)1./3.) * 100.0;
  std::ostringstream ss;
  ss << vol_float;
  QString prog = "amixer cset numid=1 " + QString::fromStdString(ss.str()) + '%';
  std::cout << prog.toStdString() << std::endl;
#else
  //assuming Raspberry Pi
  QProcess *p = new QProcess();
  float v = (float) val;
  // handle the fact that the amixer percentage range isn't linear
  float vol_float = std::pow(v/100.0, (float)1./3.) * 100.0;
  std::ostringstream ss;
  ss << vol_float;
  QString prog = "amixer cset numid=1 " + QString::fromStdString(ss.str()) + '%';
  p->start(prog);
#endif

}


void MainWindow::setRPSystemAudioHeadphones(){

#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing, just print out what it would do on RPi
  QString prog = "amixer cset numid=3 1";
  std::cout << prog.toStdString() << std::endl;
#else
  //assuming Raspberry Pi
  QProcess *p = new QProcess();
  QString prog = "amixer cset numid=3 1";
  p->start(prog);
#endif
}

void MainWindow::setRPSystemAudioHDMI(){

#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing, just print out what it would do on RPi
  QString prog = "amixer cset numid=3 2";
  std::cout << prog.toStdString() << std::endl;
#else
  //assuming Raspberry Pi
  QProcess *p = new QProcess();
  QString prog = "amixer cset numid=3 2";
  p->start(prog);
#endif
}

void MainWindow::setRPSystemAudioAuto(){

#if defined(Q_OS_WIN)
  //do nothing
#elif defined(Q_OS_MAC)
  //do nothing, just print out what it would do on RPi
  QString prog = "amixer cset numid=3 0";
  std::cout << prog.toStdString() << std::endl;
#else
  //assuming Raspberry Pi
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

void MainWindow::zoomFontIn()
{
  SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
  int zoom = ws->property("zoom").toInt();
  zoom++;
  if (zoom > 20) zoom = 20;
  ws->setProperty("zoom", QVariant(zoom));
  ws->zoomTo(zoom);
}

void MainWindow::zoomFontOut()
{
  SonicPiScintilla* ws = ((SonicPiScintilla*)tabs->currentWidget());
  int zoom = ws->property("zoom").toInt();
  zoom--;
  if (zoom < -5) zoom = -5;
  ws->setProperty("zoom", QVariant(zoom));
  ws->zoomTo(zoom);
}

void MainWindow::wheelEvent(QWheelEvent *event) {
#if defined(Q_OS_WIN)
  if (event->modifiers() & Qt::ControlModifier) {
    if (event->angleDelta().y() > 0)
      zoomFontIn();
    else
      zoomFontOut();
  }
#endif
}



void MainWindow::documentWasModified()
{
  setWindowModified(textEdit->isModified());
}


void MainWindow::stopRunningSynths()
{
  Message msg("/stop-all-jobs");
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
QKeySequence MainWindow::cmdAltKey(char key)
{
#ifdef Q_OS_MAC
  return QKeySequence(QString("Ctrl+%1").arg(key));
#else
  return QKeySequence(QString("alt+%1").arg(key));
#endif
}

// set tooltips, connect event handlers, and add shortcut if applicable
void MainWindow::setupAction(QAction *action, char key, QString tooltip,
				 const char *slot)
{
  QString shortcut, tooltipKey;
  tooltipKey = tooltip;
  if (key != 0) {
#ifdef Q_OS_MAC
    tooltipKey = QString("%1 (⌘%2)").arg(tooltip).arg(key);
#else
    tooltipKey = QString("%1 (alt-%2)").arg(tooltip).arg(key);
#endif
  }

  action->setToolTip(tooltipKey);
  action->setStatusTip(tooltip);
  connect(action, SIGNAL(triggered()), this, slot);

  if (key != 0) {
    // create a QShortcut instead of setting the QAction's shortcut
    // so it will still be active with the toolbar hidden
    new QShortcut(cmdAltKey(key), this, slot);
  }
}

void MainWindow::createActions()
{

  // Run
  runAct = new QAction(QIcon(":/images/run.png"), tr("Run"), this);
  setupAction(runAct, 'R', tr("Run the code in the current workspace"),
		 SLOT(runCode()));

  // Stop
  stopAct = new QAction(QIcon(":/images/stop.png"), tr("Stop"), this);
  setupAction(stopAct, 'S', tr("Stop all running code"), SLOT(stopCode()));

  // Save
  saveAsAct = new QAction(QIcon(":/images/save.png"), tr("Save As..."), this);
  setupAction(saveAsAct, 0, tr("Export current workspace"), SLOT(saveAs()));

  // Info
  infoAct = new QAction(QIcon(":/images/info.png"), tr("Info"), this);
  setupAction(infoAct, 0, tr("See information about Sonic Pi"),
		 SLOT(about()));

  // Help
  helpAct = new QAction(QIcon(":/images/help.png"), tr("Help"), this);
  setupAction(helpAct, 'I', tr("Toggle help pane"), SLOT(help()));

  new QShortcut(QKeySequence("F1"), this, SLOT(helpContext()));
  new QShortcut(ctrlKey('i'), this, SLOT(helpContext()));

  new QShortcut(cmdAltKey('['), this, SLOT(tabPrev()));
  new QShortcut(cmdAltKey('{'), this, SLOT(tabPrev()));
  new QShortcut(cmdAltKey(']'), this, SLOT(tabNext()));
  new QShortcut(cmdAltKey('}'), this, SLOT(tabNext()));

  // Preferences
  prefsAct = new QAction(QIcon(":/images/prefs.png"), tr("Prefs"), this);
  setupAction(prefsAct, 'P', tr("Toggle preferences pane"),
	      SLOT(showPrefsPane()));

  // Record
  recAct = new QAction(QIcon(":/images/rec.png"), tr("Start Recording"), this);
  setupAction(recAct, 0, tr("Start Recording"), SLOT(toggleRecording()));

  // Align
  textAlignAct = new QAction(QIcon(":/images/align.png"),
			     tr("Auto Align Text"), this);
  setupAction(textAlignAct, 'M', tr("Auto-align text"), SLOT(beautifyCode()));

  // Font Size Increase
  textIncAct1 = new QAction(QIcon(":/images/size_up.png"),
			    tr("Increase Text Size"), this);
  setupAction(textIncAct1, '+', tr("Make text bigger"), SLOT(zoomFontIn()));
  textIncKey2 = new QShortcut(cmdAltKey('='), this,
			      SLOT(zoomFontIn()));

  // Font Size Decrease
  textDecAct1 = new QAction(QIcon(":/images/size_down.png"),
			    tr("Decrease Text Size"), this);
  setupAction(textDecAct1, '-', tr("Make text smaller"), SLOT(zoomFontOut()));
  textDecKey2 = new QShortcut(cmdAltKey('_'), this,
			      SLOT(zoomFontOut()));

  reloadKey = new QShortcut(cmdAltKey('U'), this, SLOT(reloadServerCode()));

}

void MainWindow::createToolBar()
{

  QWidget *spacer = new QWidget();
  spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);

  toolBar = addToolBar(tr("Tools"));

  toolBar->setIconSize(QSize(270/3, 111/3));
  toolBar->addAction(runAct);
  toolBar->addAction(stopAct);

  toolBar->addAction(saveAsAct);
  toolBar->addAction(recAct);
  toolBar->addWidget(spacer);

  toolBar->addAction(textDecAct1);
  toolBar->addAction(textIncAct1);
  dynamic_cast<QToolButton*>(toolBar->widgetForAction(textDecAct1))->setAutoRepeat(true);
  dynamic_cast<QToolButton*>(toolBar->widgetForAction(textIncAct1))->setAutoRepeat(true);

  toolBar->addAction(textAlignAct);

  toolBar->addAction(infoAct);
  toolBar->addAction(helpAct);
  toolBar->addAction(prefsAct);

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
     sendOSC(msg);
   } else {
     rec_flash_timer->stop();
     recAct->setStatusTip(tr("Start Recording"));
     recAct->setToolTip(tr("Start Recording"));
     recAct->setIcon(QIcon(":/images/rec.png"));
     Message msg("/stop-recording");
     sendOSC(msg);
     QString fileName = QFileDialog::getSaveFileName(this, tr("Save Recording"), QDir::homePath() + "/Desktop/my-recording.wav");
     if (!fileName.isEmpty()) {
         Message msg("/save-recording");
         msg.pushStr(fileName.toStdString());
         sendOSC(msg);
     } else {
       Message msg("/delete-recording");
       sendOSC(msg);
     }
   }
 }


void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}

void MainWindow::readSettings()
{
    QSettings settings("uk.ac.cam.cl", "Sonic Pi");
    QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
    QSize size = settings.value("size", QSize(400, 400)).toSize();
    resize(size);
    move(pos);

    for (int w=0; w < workspace_max; w++) {
      // default zoom is 13
      int zoom = settings.value(QString("workspace%1zoom").arg(w+1), 13)
	.toInt();
      if (zoom < -5) zoom = -5;
      if (zoom > 20) zoom = 20;

      workspaces[w]->setProperty("zoom", QVariant(zoom));
      workspaces[w]->zoomTo(zoom);
    }

    restoreState(settings.value("windowState").toByteArray());
}

void MainWindow::writeSettings()
{
    QSettings settings("uk.ac.cam.cl", "Sonic Pi");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
    settings.setValue("first_time", 0);

    for (int w=0; w < workspace_max; w++) {
      settings.setValue(QString("workspace%1zoom").arg(w+1),
			workspaces[w]->property("zoom"));
    }

    settings.setValue("windowState", saveState());
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
    statusBar()->showMessage(tr("File loaded"), 2000);
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
#endif
    out << code;
    QApplication::restoreOverrideCursor();

    statusBar()->showMessage(tr("File saved"), 2000);
    return true;
}

 std::string MainWindow::workspaceFilename(SonicPiScintilla* text)
{
	for(int i = 0; i < workspace_max; i++) {
		if(text == workspaces[i]) {
                  return "workspace_" + number_name(i + 1);
		}
	}
	return "default";
}

 SonicPiScintilla*  MainWindow::filenameToWorkspace(std::string filename)
{
	std::string s;

	for(int i = 0; i < workspace_max; i++) {
		s = "workspace_" + number_name(i + 1);
		if(filename == s) {
			return workspaces[i];
		}
	}
	return workspaces[0];
}

void MainWindow::onExitCleanup()
{
  if(serverProcess->state() == QProcess::NotRunning) {
    std::cout << "Server process is not running, something is up..." << std::endl;
    cont_listening_for_osc = false;
  } else {
    saveWorkspaces();
    sleep(1);
    std::cout << "Asking server process to exit..." << std::endl;
    Message msg("/exit");
    sendOSC(msg);
  }
  osc_thread.waitForFinished();
  std::cout << "Exiting..." << std::endl;

}

void MainWindow::updateDocPane(QListWidgetItem *cur) {
  QString content = cur->data(32).toString();
  docPane->setHtml(content);
}

void MainWindow::updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev) {
  updateDocPane(cur);
  prev = prev;
}

void MainWindow::setHelpText(QListWidgetItem *item, const QString filename) {
  QFile file(filename);

  if(!file.open(QFile::ReadOnly | QFile::Text)) {
  }

  QString s;
  QTextStream st(&file);
  s.append(st.readAll());

  item->setData(32, QVariant(s));
}

void MainWindow::addHelpPage(QListWidget *nameList,
                             struct help_page *helpPages, int len) {
  int i;
  struct help_entry entry;
  entry.pageIndex = docsCentral->count()-1;

  for(i = 0; i < len; i++) {
    QListWidgetItem *item = new QListWidgetItem(helpPages[i].title);
    setHelpText(item, QString(helpPages[i].filename));
    item->setSizeHint(QSize(item->sizeHint().width(), 25));
    nameList->addItem(item);
    entry.entryIndex = nameList->count()-1;
    helpKeywords.insert(helpPages[i].keyword.toLower(), entry);

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

QListWidget *MainWindow::createHelpTab(QString name) {
  QListWidget *nameList = new QListWidget;
  connect(nameList,
	  SIGNAL(itemPressed(QListWidgetItem*)),
	  this, SLOT(updateDocPane(QListWidgetItem*)));
  connect(nameList,
	  SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
	  this, SLOT(updateDocPane2(QListWidgetItem*, QListWidgetItem*)));
  QBoxLayout *layout = new QBoxLayout(QBoxLayout::LeftToRight);
  layout->addWidget(nameList);
  layout->setStretch(1, 1);
  QWidget *tabWidget = new QWidget;
  tabWidget->setLayout(layout);
  docsCentral->addTab(tabWidget, name);
  helpLists.append(nameList);
  return nameList;
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


#include "ruby_help.h"
