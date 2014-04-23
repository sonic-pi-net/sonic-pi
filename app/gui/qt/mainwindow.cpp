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
#include <sstream>
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
#include <QToolBar>
#include <QProcess>
#include <QFont>
#include <QTabWidget>
#include <QString>
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
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include <sonicpilexer.h>
#include <iostream>
#include "oscpkt.hh"
#include "udp.hh"

#if defined(Q_OS_WIN)
  #include <QtConcurrentRun>
#elif defined(Q_OS_MAC)
  #include "qtconcurrent/qtconcurrent"
#else
  //assuming Raspberry Pi
  #include <cmath>
  #include <QtConcurrentRun>
#endif




#include "mainwindow.h"

using namespace oscpkt;

MainWindow::MainWindow(QApplication &app, QSplashScreen &splash) {

  this->setUnifiedTitleAndToolBarOnMac(true);

  is_recording = false;
  server_started = false;
  show_rec_icon_a = false;
  cont_listening_for_osc = true;
  osc_incoming_port_open = false;

  rec_flash_timer = new QTimer(this);
  connect(rec_flash_timer, SIGNAL(timeout()), this, SLOT(toggleRecordingOnIcon()));

  QtConcurrent::run(this, &MainWindow::startOSCListener);
  serverProcess = new QProcess();

  QString serverProgram = "ruby " + QCoreApplication::applicationDirPath() + "/../../server/bin/start-server.rb";
  std::cerr << serverProgram.toStdString() << std::endl;
  serverProcess->start(serverProgram);
  serverProcess->waitForStarted();

  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);
  setCentralWidget(tabs);

  workspace1 = new QsciScintilla;
  workspace2 = new QsciScintilla;
  workspace3 = new QsciScintilla;
  workspace4 = new QsciScintilla;
  workspace5 = new QsciScintilla;
  workspace6 = new QsciScintilla;
  workspace7 = new QsciScintilla;
  workspace8 = new QsciScintilla;

  QString w1 = "Workspace 1";
  QString w2 = "Workspace 2";
  QString w3 = "Workspace 3";
  QString w4 = "Workspace 4";
  QString w5 = "Workspace 5";
  QString w6 = "Workspace 6";
  QString w7 = "Workspace 7";
  QString w8 = "Workspace 8";

  tabs->addTab(workspace1, w1);
  tabs->addTab(workspace2, w2);
  tabs->addTab(workspace3, w3);
  tabs->addTab(workspace4, w4);
  tabs->addTab(workspace5, w5);
  tabs->addTab(workspace6, w6);
  tabs->addTab(workspace7, w7);
  tabs->addTab(workspace8, w8);

  lexer = new SonicPiLexer;
  lexer->setAutoIndentStyle(QsciScintilla::AiMaintain);

  QsciAPIs* api = new QsciAPIs(lexer);
  api->add("drum_heavy_kick");
  api->add("drum_tom_mid_soft");
  api->add("drum_tom_mid_hard");
  api->add("drum_tom_lo_soft");
  api->add("drum_tom_lo_hard");
  api->add("drum_tom_hi_soft");
  api->add("drum_tom_hi_hard");
  api->add("drum_splash_soft");
  api->add("drum_splash_hard");
  api->add("drum_snare_soft");
  api->add("drum_snare_hard");
  api->add("drum_cymbal_soft");
  api->add("drum_cymbal_hard");
  api->add("drum_cymbal_open");
  api->add("drum_cymbal_closed");
  api->add("drum_cymbal_pedal");
  api->add("drum_bass_soft");
  api->add("drum_bass_hard");

  api->add("elec_triangle");
  api->add("elec_snare");
  api->add("elec_lo_snare");
  api->add("elec_mid_snare");
  api->add("elec_hi_snare");
  api->add("elec_cymbal");
  api->add("elec_soft_kick");
  api->add("elec_filt_snare");
  api->add("elec_fuzz_tom");
  api->add("elec_chime");
  api->add("elec_bong");
  api->add("elec_twang");
  api->add("elec_wood");
  api->add("elec_pop");
  api->add("elec_beep");
  api->add("elec_blip");
  api->add("elec_blip2");
  api->add("elec_ping");
  api->add("elec_bell");
  api->add("elec_flip");
  api->add("elec_tick");
  api->add("elec_hollow_kick");
  api->add("elec_twip");
  api->add("elec_plip");
  api->add("elec_blup");

  api->add("guit_harmonics");
  api->add("guit_e_fifths");
  api->add("guit_e_slide");


  api->add("misc_burp");

  api->add("perc_bell");

  api->add("ambi_soft_buzz");
  api->add("ambi_swoosh");
  api->add("ambi_drone");
  api->add("ambi_glass_hum");
  api->add("ambi_glass_rub");
  api->add("ambi_haunted_hum");
  api->add("ambi_piano");
  api->add("ambi_lunar_land");
  api->add("ambi_dark_woosh");
  api->add("ambi_choir");

  api->add("bass_hit_c");
  api->add("bass_hard_c");
  api->add("bass_thick_c");
  api->add("bass_drop_c");
  api->add("bass_woodsy_c");
  api->add("bass_voxy_c");
  api->add("bass_voxy_hit_c");
  api->add("bass_dnb_f");

  api->add("loop_industrial");
  api->add("loop_compus");
  api->add("loop_amen");
  api->add("loop_amen_full");

  api->add("with_synth");
  api->add("with_merged_synth_defaults");
  api->add("with_synth_defaults");
  api->prepare();

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

  outputPane = new QTextEdit;
  errorPane = new QTextEdit;
  docPane = new QTextEdit;
  outputPane->document()->setMaximumBlockCount(1000);

  outputPane->zoomIn(1);
  errorPane->zoomIn(1);

  prefsWidget = new QDockWidget(tr("Preferences"), this);
  prefsWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  prefsCentral = new QWidget;
  prefsWidget->setWidget(prefsCentral);
  addDockWidget(Qt::RightDockWidgetArea, prefsWidget);
  prefsWidget->hide();

  outputWidget = new QDockWidget(tr("Output"), this);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);

  errorWidget = new QDockWidget(tr("Errors"), this);
  errorWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  errorWidget->setWidget(errorPane);
  addDockWidget(Qt::RightDockWidgetArea, errorWidget);


  initWorkspace(workspace1);
  initWorkspace(workspace2);
  initWorkspace(workspace3);
  initWorkspace(workspace4);
  initWorkspace(workspace5);
  initWorkspace(workspace6);
  initWorkspace(workspace7);
  initWorkspace(workspace8);

  createActions();
  createToolBars();
  createStatusBar();

  readSettings();

  setWindowTitle(tr("Sonic Pi"));

  connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );

  while (!server_started && cont_listening_for_osc) {
    sleep(1);
    if(osc_incoming_port_open) {
      Message msg("/ping");
      msg.pushStr("QtClient/1/hello");
      sendOSC(msg);
    }
  }

  loadWorkspaces();

  systemVol = new QSlider(this);
  connect(systemVol, SIGNAL(valueChanged(int)), this, SLOT(changeSystemVol(int)));
  initPrefsWindow();
  this->show();
  splash.finish(this);
}

void MainWindow::showOutputPane() {
  outputWidget->show();
}

void MainWindow::showErrorPane() {
  errorWidget->show();
}

void MainWindow::initPrefsWindow() {

  QGridLayout *grid = new QGridLayout;

  QGroupBox *volBox = new QGroupBox(tr("System Volume"));
  volBox->setToolTip("Use this slider to change the system volume of your Raspberry Pi");
  QGroupBox *groupBox = new QGroupBox(tr("Force Audio Output"));
  groupBox->setToolTip("Your Raspberry Pi has two forms of audio output. \nFirstly, there is the headphone jack of the Raspberry Pi itself. \nSecondly, some HDMI monitors/TVs support audio through the HDMI port. \nUse these buttons to force the output to the one you want. \nFor example, if you have headphones connected to your Raspberry Pi, choose 'Headphones'. ");
  QRadioButton *radio1 = new QRadioButton(tr("&Default"));
  QRadioButton *radio2 = new QRadioButton(tr("&Headphones"));
  QRadioButton *radio3 = new QRadioButton(tr("&HDMI"));
  radio1->setChecked(true);

  connect(radio1, SIGNAL(clicked()), this, SLOT(setSystemAudioAuto()));
  connect(radio2, SIGNAL(clicked()), this, SLOT(setSystemAudioHeadphones()));
  connect(radio3, SIGNAL(clicked()), this, SLOT(setSystemAudioHDMI()));

  QVBoxLayout *audio_box = new QVBoxLayout;
  audio_box->addWidget(radio1);
  audio_box->addWidget(radio2);
  audio_box->addWidget(radio3);
  audio_box->addStretch(1);
  groupBox->setLayout(audio_box);

  QHBoxLayout *vol_box = new QHBoxLayout;
  vol_box->addWidget(systemVol);
  volBox->setLayout(vol_box);

  QGroupBox *showBox = new QGroupBox("Show Panes");
  QPushButton *show_output = new QPushButton("Show Output Pane");
  QPushButton *show_error = new QPushButton("Show Error Pane");

  connect(show_output, SIGNAL(clicked()), this, SLOT(showOutputPane()));
  connect(show_error, SIGNAL(clicked()), this, SLOT(showErrorPane()));

  QVBoxLayout *info_box_layout = new QVBoxLayout;
  info_box_layout->addWidget(show_output);
  info_box_layout->addWidget(show_error);
  showBox->setLayout(info_box_layout);

  QGroupBox *debug_box = new QGroupBox("Debug Options");
  QCheckBox *print_output = new QCheckBox("Print output");
  QCheckBox *check_args = new QCheckBox("Check synth args");
  print_output->setChecked(true);
  check_args->setChecked(true);

  QVBoxLayout *debug_box_layout = new QVBoxLayout;
  debug_box_layout->addWidget(print_output);
  debug_box_layout->addWidget(check_args);
  debug_box->setLayout(debug_box_layout);

  grid->addWidget(groupBox, 0, 0);
  grid->addWidget(volBox, 0, 1);
  grid->addWidget(showBox, 1, 0);
  grid->addWidget(debug_box, 1, 1);
  prefsCentral->setLayout(grid);
}

void MainWindow::initWorkspace(QsciScintilla* ws) {
  ws->setAutoIndent(true);
  ws->setIndentationsUseTabs(false);
  ws->setBackspaceUnindents(true);
  ws->setTabIndents(true);
  ws->setMatchedBraceBackgroundColor(QColor("dimgray"));
  ws->setMatchedBraceForegroundColor(QColor("white"));

  ws->setIndentationWidth(2);
  ws->setIndentationGuides(true);
  ws->setIndentationGuidesForegroundColor(QColor("deep pink"));
  ws->setBraceMatching( QsciScintilla::SloppyBraceMatch);
  //TODO: add preference toggle for this:
  //ws->setFolding(QsciScintilla::CircledTreeFoldStyle, 2);
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
  ws->zoomIn(13);
  ws->setAutoCompletionThreshold(5);
  ws->setAutoCompletionSource(QsciScintilla::AcsAPIs);
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

          if (msg->match("/message")) {
            std::string s;
            if (msg->arg().popStr(s).isOkNoMoreArgs()) {
              // Evil nasties!
              // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
              QMetaObject::invokeMethod( outputPane, "append", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(s)) );
            } else {
              std::cout << "Server: unhandled message: "<< std::endl;
            }
          }
          else if (msg->match("/error")) {
            std::string desc;
            std::string backtrace;
            if (msg->arg().popStr(desc).popStr(backtrace).isOkNoMoreArgs()) {
              // Evil nasties!
              // See: http://www.qtforum.org/article/26801/qt4-threads-and-widgets.html
              QMetaObject::invokeMethod( errorPane, "clear", Qt::QueuedConnection);
              QMetaObject::invokeMethod( errorPane, "setHtml", Qt::QueuedConnection,
                                         Q_ARG(QString, "<h3><pre>" + QString::fromStdString(desc) + "</pre></h3><pre>" + QString::fromStdString(backtrace) + "</pre>") );

            } else {
              std::cout << "Server: unhandled error: "<< std::endl;
            }
          }
          else if (msg->match("/replace-buffer")) {
            std::string id;
            std::string content;
            if (msg->arg().popStr(id).popStr(content).isOkNoMoreArgs()) {
              QsciScintilla* ws = filenameToWorkspace(id);
              QMetaObject::invokeMethod( ws, "setText", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(content)) );
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



void MainWindow::loadWorkspaces()
{
  std::cout << "loading workspaces" << std::endl;;

  Message msg("/load-buffer");
  msg.pushStr("workspace_one");
  sendOSC(msg);

  Message msg2("/load-buffer");
  msg2.pushStr("workspace_two");
  sendOSC(msg2);

  Message msg3("/load-buffer");
  msg3.pushStr("workspace_three");
  sendOSC(msg3);

  Message msg4("/load-buffer");
  msg4.pushStr("workspace_four");
  sendOSC(msg4);

  Message msg5("/load-buffer");
  msg5.pushStr("workspace_five");
  sendOSC(msg5);

  Message msg6("/load-buffer");
  msg6.pushStr("workspace_six");
  sendOSC(msg6);

  Message msg7("/load-buffer");
  msg7.pushStr("workspace_seven");
  sendOSC(msg7);

  Message msg8("/load-buffer");
  msg8.pushStr("workspace_eight");
  sendOSC(msg8);
}

void MainWindow::saveWorkspaces()
{
  std::cout << "saving workspaces" << std::endl;;

  std::string code = workspace1->text().toStdString();
  Message msg("/save-buffer");
  msg.pushStr("workspace_one");
  msg.pushStr(code);
  sendOSC(msg);

  std::string code2 = workspace2->text().toStdString();
  Message msg2("/save-buffer");
  msg2.pushStr("workspace_two");
  msg2.pushStr(code2);
  sendOSC(msg2);

  std::string code3 = workspace3->text().toStdString();
  Message msg3("/save-buffer");
  msg3.pushStr("workspace_three");
  msg3.pushStr(code3);
  sendOSC(msg3);

  std::string code4 = workspace4->text().toStdString();
  Message msg4("/save-buffer");
  msg4.pushStr("workspace_four");
  msg4.pushStr(code4);
  sendOSC(msg4);

  std::string code5 = workspace5->text().toStdString();
  Message msg5("/save-buffer");
  msg5.pushStr("workspace_five");
  msg5.pushStr(code5);
  sendOSC(msg5);

  std::string code6 = workspace6->text().toStdString();
  Message msg6("/save-buffer");
  msg6.pushStr("workspace_six");
  msg6.pushStr(code6);
  sendOSC(msg6);

  std::string code7 = workspace7->text().toStdString();
  Message msg7("/save-buffer");
  msg7.pushStr("workspace_seven");
  msg7.pushStr(code7);
  sendOSC(msg7);

  std::string code8 = workspace8->text().toStdString();
  Message msg8("/save-buffer");
  msg8.pushStr("workspace_eight");
  msg8.pushStr(code8);
  sendOSC(msg8);
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
    return saveFile(fileName, (QsciScintilla*)tabs->currentWidget());
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
  statusBar()->showMessage(tr("Running...."), 2000);
  std::string code = ((QsciScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/save-and-run-buffer");
  std::string filename = workspaceFilename( (QsciScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  msg.pushStr(code);
  sendOSC(msg);
}


void MainWindow::stopCode()
{
  stopRunningSynths();
  outputPane->clear();
  errorPane->clear();
  statusBar()->showMessage(tr("Stopping..."), 2000);
}

void MainWindow::about()
{
  infoWindow = new QMainWindow();
  imageLabel = new QLabel(this);
  QPixmap image(":/images/splash.png");

  imageLabel->setPixmap(image);
  infoWindow->setCentralWidget(imageLabel);
  infoWindow->setMinimumHeight(image.height());
  infoWindow->setMaximumHeight(image.height());
  infoWindow->setMinimumWidth(image.width());
  infoWindow->setMaximumWidth(image.width());
  infoWindow->show();
}


void MainWindow::help()
{
  QMessageBox about;

  about.setWindowTitle("Sonic Pi Help");
  about.setText("Sonic Pi - Making Computer Science Audible");
  about.setInformativeText("Help goes here...");
  about.setStandardButtons(QMessageBox::Ok);
  about.setDefaultButton(QMessageBox::Ok);
  about.show();
  about.exec();

}

void MainWindow::changeSystemVol(int val) {
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


void MainWindow::setSystemAudioHeadphones(){

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

void MainWindow::setSystemAudioHDMI(){

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

void MainWindow::setSystemAudioAuto(){

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
  prefsWidget->show();
}

void MainWindow::zoomFontIn()
{
  ((QsciScintilla*)tabs->currentWidget())->zoomIn(1);
}

void MainWindow::zoomFontOut()
{
  ((QsciScintilla*)tabs->currentWidget())->zoomOut(1);
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

void MainWindow::createActions()
{

  runAct = new QAction(QIcon(":/images/run.png"), tr("&Run"), this);
  runAct->setShortcut(tr("Ctrl+R"));
  runAct->setStatusTip(tr("Run code"));
  connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

  stopAct = new QAction(QIcon(":/images/stop.png"), tr("&Stop"), this);
  stopAct->setShortcut(tr("Ctrl+Q"));
  stopAct->setStatusTip(tr("Stop code"));
  connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

  saveAsAct = new QAction(QIcon(":/images/save.png"), tr("&Save &As..."), this);
  saveAsAct->setStatusTip(tr("Save the document under a new name"));
  connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

  infoAct = new QAction(QIcon(":/images/info.png"), tr("&Info"), this);
  infoAct->setStatusTip(tr("See information about Sonic Pi"));
  connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

  helpAct = new QAction(QIcon(":/images/help.png"), tr("&Help"), this);
  helpAct->setStatusTip(tr("Get help"));
  connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));

  prefsAct = new QAction(QIcon(":/images/prefs.png"), tr("&Prefs"), this);
  prefsAct->setStatusTip(tr("Preferences"));
  connect(prefsAct, SIGNAL(triggered()), this, SLOT(showPrefsPane()));

  recAct = new QAction(QIcon(":/images/rec.png"), tr("&Start &Recording"), this);
  recAct->setStatusTip(tr("Start Recording"));
  connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

}

void MainWindow::createToolBars()
{
  fileToolBar = addToolBar(tr("Run"));
  fileToolBar->setIconSize(QSize(270/3, 109/3));
  fileToolBar->addAction(runAct);
  fileToolBar->addAction(stopAct);

  QWidget *spacerWidget1 = new QWidget(this);
  spacerWidget1->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  spacerWidget1->setVisible(true);


  saveToolBar = addToolBar(tr("Save"));
  saveToolBar->addWidget(spacerWidget1);
  saveToolBar->setIconSize(QSize(270/3, 109/3));
  saveToolBar->addAction(saveAsAct);
  saveToolBar->addAction(recAct);

  QWidget *spacerWidget2 = new QWidget(this);
  spacerWidget2->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  spacerWidget2->setVisible(true);

  supportToolBar = addToolBar(tr("Support"));
  supportToolBar->addWidget(spacerWidget2);
  supportToolBar->addAction(infoAct);
  supportToolBar->addAction(helpAct);
  supportToolBar->setIconSize(QSize(270/3, 109/3));
  supportToolBar->addAction(prefsAct);
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
     rec_flash_timer->start(500);
     Message msg("/start-recording");
     sendOSC(msg);
   } else {
     rec_flash_timer->stop();
     recAct->setStatusTip(tr("Start Recording"));
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
}

void MainWindow::writeSettings()
{
    QSettings settings("uk.ac.cam.cl", "Sonic Pi");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
}

void MainWindow::loadFile(const QString &fileName, QsciScintilla* &text)
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

bool MainWindow::saveFile(const QString &fileName, QsciScintilla* text)
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
    out << text->text();
    QApplication::restoreOverrideCursor();
    statusBar()->showMessage(tr("File saved"), 2000);
    return true;
}

 std::string MainWindow::workspaceFilename(QsciScintilla* text)
{
  if(text == workspace1) {return "workspace_one";}
  else if(text == workspace2) {return "workspace_two";}
  else if(text == workspace3) {return "workspace_three";}
  else if(text == workspace4) {return "workspace_four";}
  else if(text == workspace5) {return "workspace_five";}
  else if(text == workspace6) {return "workspace_six";}
  else if(text == workspace7) {return "workspace_seven";}
  else if(text == workspace8) {return "workspace_eight";}
  else {return "default";}
}

 QsciScintilla*  MainWindow::filenameToWorkspace(std::string filename)
{
  if(filename == "workspace_one") {return workspace1;}
  else if(filename == "workspace_two") {return workspace2;}
  else if(filename == "workspace_three") {return workspace3;}
  else if(filename == "workspace_four") {return workspace4;}
  else if(filename == "workspace_five") {return workspace5;}
  else if(filename == "workspace_six") {return workspace6;}
  else if(filename == "workspace_seven") {return workspace7;}
  else if(filename == "workspace_eight") {return workspace8;}
  else {return workspace1;}
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
  std::cout << "Exiting..." << std::endl;

}
