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

  for(int ws = 0; ws < workspace_max; ws++) {
	  std::string s;

	  workspaces[ws] = new QsciScintilla;
	  QString w = QString("Workspace %1").arg(QString::number(ws + 1));
	  tabs->addTab(workspaces[ws], w);
  }

  lexer = new SonicPiLexer;
  lexer->setAutoIndentStyle(QsciScintilla::AiMaintain);

  QsciAPIs* api = new QsciAPIs(lexer);
  QStringList api_names;

  // yes, really
  #include "api_list.h"

  for (int api_iter = 0; api_iter < api_names.size(); ++api_iter) {
	  api->add(api_names.at(api_iter));
  }
  api->prepare();
  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

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


  docsCentral = new QTabWidget;
  docsCentral->setTabsClosable(false);
  docsCentral->setMovable(false);
  docsCentral->setTabPosition(QTabWidget::West);
  docWidget = new QDockWidget("Documentation", this);
  docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
  docWidget->setWidget(docsCentral);

  langDocPane = new QTextEdit;
  langDocPane->setReadOnly(true);
  synthsDocPane = new QTextEdit;
  synthsDocPane->setReadOnly(true);
  fxDocPane = new QTextEdit;
  fxDocPane->setReadOnly(true);
  samplesDocPane = new QTextEdit;
  samplesDocPane->setReadOnly(true);
  examplesDocPane = new QTextEdit;
  examplesDocPane->setReadOnly(true);

  addDockWidget(Qt::BottomDockWidgetArea, docWidget);
  docWidget->hide();

  for(int ws = 0; ws < workspace_max; ws++) {
	  initWorkspace(workspaces[ws]);
  }

  createActions();
  createToolBar();
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
  initDocsWindow();
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
  print_output = new QCheckBox("Print output");
  check_args = new QCheckBox("Check synth args");
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
  errorPane->clear();
  statusBar()->showMessage(tr("Running...."), 2000);
  std::string code = ((QsciScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/save-and-run-buffer");
  std::string filename = workspaceFilename( (QsciScintilla*)tabs->currentWidget());
  msg.pushStr(filename);
  if(!print_output->isChecked()) {
    code = "use_debug false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }
  if(!check_args->isChecked()) {
    code = "use_arg_checks false #__nosave__ set by Qt GUI user preferences.\n" + code ;
  }

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
  if(docWidget->isVisible()) {
    docWidget->hide();
  } else {
    docWidget->show();
  }
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
  if(prefsWidget->isVisible()) {
    prefsWidget->hide();
  } else {
    prefsWidget->show();
  }
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
  runAct->setStatusTip(tr("Run the code in the current workspace"));
  runAct->setToolTip(tr("Run the code in the current workspace (Ctrl-R)"));
  connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

  stopAct = new QAction(QIcon(":/images/stop.png"), tr("&Stop"), this);
  stopAct->setShortcut(tr("Ctrl+Q"));
  stopAct->setStatusTip(tr("Stop all running code"));
  stopAct->setToolTip(tr("Stop all running code (Ctrl-Q)"));
  connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

  saveAsAct = new QAction(QIcon(":/images/save.png"), tr("&Save &As..."), this);
  saveAsAct->setStatusTip(tr("Save the current workspace under a new name"));
  saveAsAct->setToolTip(tr("Save the current workspace under a new name"));
  connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

  infoAct = new QAction(QIcon(":/images/info.png"), tr("&Info"), this);
  infoAct->setStatusTip(tr("See information about Sonic Pi"));
  infoAct->setToolTip(tr("See information about Sonic Pi"));
  connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

  helpAct = new QAction(QIcon(":/images/help.png"), tr("&Help"), this);
  helpAct->setStatusTip(tr("Toggle help pane"));
  helpAct->setToolTip(tr("Toggle help pane"));
  connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));

  prefsAct = new QAction(QIcon(":/images/prefs.png"), tr("&Prefs"), this);
  prefsAct->setStatusTip(tr("Toggle preferences pane"));
  prefsAct->setToolTip(tr("Toggle preferences pane"));
  connect(prefsAct, SIGNAL(triggered()), this, SLOT(showPrefsPane()));

  recAct = new QAction(QIcon(":/images/rec.png"), tr("&Start &Recording"), this);
  recAct->setStatusTip(tr("Start Recording"));
  recAct->setToolTip(tr("Start Recording"));
  connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

  textIncAct = new QAction(QIcon(":/images/text-inc.png"), tr("&Increase &Text &Size"), this);
  textIncAct->setStatusTip(tr("Make text bigger"));
  textIncAct->setToolTip(tr("Make text bigger"));
  connect(textIncAct, SIGNAL(triggered()), this, SLOT(zoomFontIn()));

  textDecAct = new QAction(QIcon(":/images/text-dec.png"), tr("&Decrease &Text &Size"), this);
  textDecAct->setStatusTip(tr("Make text smaller"));
  textDecAct->setToolTip(tr("Make text smaller"));
  connect(textDecAct, SIGNAL(triggered()), this, SLOT(zoomFontOut()));


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

  toolBar->addAction(textIncAct);
  toolBar->addAction(textDecAct);

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
	for(int i = 0; i < workspace_max; i++) {
		if(text == workspaces[i]) {
			std::string s = "workspace_" + number_name(i + 1);
		}
	}
	return "default";
}

 QsciScintilla*  MainWindow::filenameToWorkspace(std::string filename)
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
  std::cout << "Exiting..." << std::endl;

}
void MainWindow::updateDocPane(QListWidgetItem *cur, QListWidgetItem *prev) {
  QString content = cur->data(32).toString();
  langDocPane->setHtml(content);
  synthsDocPane->setHtml(content);
  fxDocPane->setHtml(content);
  samplesDocPane->setHtml(content);
  examplesDocPane->setHtml(content);
  (void) prev; /* unused, but needed for type signature */
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

  for(i = 0; i < len; i++) {
    QListWidgetItem *item = new QListWidgetItem(helpPages[i].title);
    setHelpText(item, QString(helpPages[i].filename));
    item->setSizeHint(QSize(item->sizeHint().width(), 25));
    nameList->addItem(item);
  }
}

QListWidget *MainWindow::createHelpTab(QTextEdit *docPane, QString name) {
	QListWidget *nameList = new QListWidget;
	nameList->setSortingEnabled(true);
	connect(nameList,
			SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)),
			this, SLOT(updateDocPane(QListWidgetItem*, QListWidgetItem*)));
	QBoxLayout *layout = new QBoxLayout(QBoxLayout::LeftToRight);
	layout->addWidget(nameList);
	layout->addWidget(docPane);
	layout->setStretch(1, 1);
	QWidget *tabWidget = new QWidget;
	tabWidget->setLayout(layout);
	docsCentral->addTab(tabWidget, name);
	return nameList;
}

#include "ruby_help.h"
