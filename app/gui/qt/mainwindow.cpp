/****************************************************************************
**
** Copyright (c) 2013, 2014 Samuel Aaron (http://sam.aaron.name)
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
** copies of the Software, and to permit persons to whom the Software is
** furnished to do so, subject to the following conditions:
**
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
** IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
** AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
** OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
** THE SOFTWARE.
**
****************************************************************************/

#include <QMap>
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
#include <QToolBox>
#include <QSlider>
#include <QPushButton>
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include <sonicpilexer.h>
#include <iostream>
#include "oscpkt.hh"
#include "udp.hh"

#include <QtConcurrentRun>
//#include "qtconcurrent/qtconcurrent"

#include "mainwindow.h"

using namespace oscpkt;

MainWindow::MainWindow(QApplication &app)
{
  cont_listening_for_osc = true;

  QtConcurrent::run(this, &MainWindow::startOSCListener);

  QString serverProgram = QCoreApplication::applicationDirPath() + "/../../scripts/bin/start-server.rb";
  std::cerr << serverProgram.toStdString() << std::endl;
  serverProcess = new QProcess();
  // serverProcess->start(serverProgram);
  // serverProcess->waitForStarted();

  runProcess = NULL;

  QMap<QString, QString> map;
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

  workspace1->setAutoIndent(true);
  workspace1->setIndentationsUseTabs(false);
  workspace1->setIndentationWidth(2);

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

  QString one = "one";
  QString two = "two";
  QString three = "three";
  QString four = "four";
  QString five = "five";
  QString six = "six";
  QString seven = "seven";
  QString eight = "eight";

  map.insert(w1, one);
  map.insert(w2, two);
  map.insert(w3, three);
  map.insert(w4, four);
  map.insert(w5, five);
  map.insert(w6, six);
  map.insert(w7, seven);
  map.insert(w8, eight);

  workspace1->setMarginLineNumbers(0, true);
  workspace1->setMarginWidth(0, 30);
  workspace1->setMarginsBackgroundColor(QColor("white"));
  workspace1->setMarginsForegroundColor(QColor("lightgray"));
  workspace1->setMarginsFont(QFont("Menlo",10, -1, true));

  workspace1->setUtf8(true);
  workspace2->setUtf8(true);
  workspace3->setUtf8(true);
  workspace4->setUtf8(true);
  workspace5->setUtf8(true);
  workspace6->setUtf8(true);
  workspace7->setUtf8(true);
  workspace8->setUtf8(true);

  lexer = new SonicPiLexer;
  lexer->setAutoIndentStyle(QsciScintilla::AiMaintain);

  QsciAPIs* api = new QsciAPIs(lexer);

  api->add("ambi_drone");
  api->add("ambi_haunted_hum");
  api->add("ambi_lunar_land");
  api->add("ambi_soft_buzz");
  api->add("ambi_swoosh");
  api->add("ambi_piano");
  api->add("drum_bass_hard");
  api->add("drum_bass_soft");
  api->add("drum_cymbal_closed");
  api->add("drum_cymbal_hard");
  api->add("drum_cymbal_open");
  api->add("drum_cymbal_pedal");
  api->add("drum_cymbal_soft");
  api->add("drum_heavy_kick");
  api->add("drum_snare_hard");
  api->add("drum_snare_soft");
  api->add("drum_splash_hard");
  api->add("drum_splash_soft");
  api->add("drum_tom_hi_hard");
  api->add("drum_tom_hi_soft");
  api->add("drum_tom_lo_hard");
  api->add("drum_tom_lo_soft");
  api->add("drum_tom_mid_hard");
  api->add("drum_tom_mid_soft");
  api->add("elec_beep");
  api->add("elec_bell");
  api->add("elec_blip");
  api->add("elec_blip2");
  api->add("elec_blup");
  api->add("elec_bong");
  api->add("elec_chime");
  api->add("elec_cymbal");
  api->add("elec_filt_snare");
  api->add("elec_flip");
  api->add("elec_fuzz_tom");
  api->add("elec_hi_snare");
  api->add("elec_hollow_kick");
  api->add("elec_lo_snare");
  api->add("elec_mid_snare");
  api->add("elec_ping");
  api->add("elec_plip");
  api->add("elec_pop");
  api->add("elec_snare");
  api->add("elec_soft_kick");
  api->add("elec_tick");
  api->add("elec_triangle");
  api->add("elec_twang");
  api->add("elec_twip");
  api->add("elec_wood");
  api->add("glass_hum");
  api->add("guit_e_fifths");
  api->add("guit_e_slide");
  api->add("guit_harmonics");
  api->add("loop_breakbeat");

  api->add("with_synth");
  api->add("with_merged_synth_defaults");
  api->add("with_synth_defaults");

  api->prepare();

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

  workspace1->setLexer(lexer);
  workspace1->zoomIn(13);
  workspace1->setAutoCompletionThreshold(5);
  workspace1->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace2->setLexer(lexer);
  workspace2->zoomIn(13);
  workspace2->setAutoCompletionThreshold(5);
  workspace2->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace3->setLexer(lexer);
  workspace3->zoomIn(13);
  workspace3->setAutoCompletionThreshold(5);
  workspace3->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace4->setLexer(lexer);
  workspace4->zoomIn(13);
  workspace4->setAutoCompletionThreshold(5);
  workspace4->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace5->setLexer(lexer);
  workspace5->zoomIn(13);
  workspace5->setAutoCompletionThreshold(5);
  workspace5->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace6->setLexer(lexer);
  workspace6->zoomIn(13);
  workspace6->setAutoCompletionThreshold(5);
  workspace6->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace7->setLexer(lexer);
  workspace7->zoomIn(13);
  workspace7->setAutoCompletionThreshold(5);
  workspace7->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  workspace8->setLexer(lexer);
  workspace8->zoomIn(13);
  workspace8->setAutoCompletionThreshold(5);
  workspace8->setAutoCompletionSource(QsciScintilla::AcsAPIs);

  outputPane = new QTextEdit;
  errorPane = new QTextEdit;

  outputPane->zoomIn(1);
  errorPane->zoomIn(1);

  QDockWidget *outputWidget = new QDockWidget(tr("Output"), this);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);

  QDockWidget *dockWidget = new QDockWidget(tr("Errors"), this);
  dockWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  dockWidget->setWidget(errorPane);
  addDockWidget(Qt::RightDockWidgetArea, dockWidget);

  createActions();
  createToolBars();
  createStatusBar();

  readSettings();

  setWindowTitle(tr("Sonic Pi"));
  loadWorkspaces();

  connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );
}

void MainWindow::startOSCListener() {
  std::cout << "starting OSC Server" << std::endl;
  int PORT_NUM = 4558;
  UdpSocket sock;
  sock.bindTo(PORT_NUM);
  if (!sock.isOk()) {
    std::cout << "Unable to listen to OSC messages on port 4558" << std::endl;
  } else {
    PacketReader pr;
    PacketWriter pw;

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
              QMetaObject::invokeMethod( errorPane, "append", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(desc)) );

              QMetaObject::invokeMethod( errorPane, "append", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(backtrace)) );
            } else {
              std::cout << "Server: unhandled error: "<< std::endl;
            }
          }
          else if (msg->match("/replace_buffer")) {
            std::string id;
            std::string content;
            if (msg->arg().popStr(id).popStr(content).isOkNoMoreArgs()) {
              QMetaObject::invokeMethod( workspace1, "setText", Qt::QueuedConnection,
                                         Q_ARG(QString, QString::fromStdString(content)) );
            } else {
              std::cout << "Server: unhandled replace_buffer: "<< std::endl;
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
  msg.pushStr("main");
  sendOSC(msg);
  //  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/one/1.spi", workspace1);
}

void MainWindow::saveWorkspaces()
{
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/one/1.spi", workspace1);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/two/1.spi", workspace2);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/three/1.spi", workspace3);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/four/1.spi", workspace4);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/five/1.spi", workspace5);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/six/1.spi", workspace6);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/seven/1.spi", workspace7);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/eight/1.spi", workspace8);
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
  return saveFile(fileName, (QsciScintilla*)tabs->currentWidget());
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

  saveWorkspace( (QsciScintilla*)tabs->currentWidget());
  QString emptyText = "";
  statusBar()->showMessage(tr("Running...."), 2000);
  clearOutputPanels();
  std::string code = ((QsciScintilla*)tabs->currentWidget())->text().toStdString();

  Message msg("/run-code");
  msg.pushStr(code);
  sendOSC(msg);

}

void MainWindow::killSynths()
{
  stopRunningSynths();
}

void MainWindow::stopCode()
{
  stopRunningSynths();
  outputPane->clear();
  errorPane->clear();
  statusBar()->showMessage(tr("Stopping..."), 2000);

  QString program = QCoreApplication::applicationDirPath() + "/../../scripts/stop-code.rb";
  QProcess *p = new QProcess();
  p->start(program);
  p->waitForStarted();
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

void MainWindow::prefs()
{

  prefsWindow = new QMainWindow();
  QToolBox *tools = new QToolBox();

  prefsWindow->setCentralWidget(tools);
  QPushButton *but = new QPushButton();
  tools->addItem(but, "Font Size UP");
  connect(but, SIGNAL(clicked()), this, SLOT(zoomFontIn()));

  QPushButton *but2 = new QPushButton();
  tools->addItem(but2, "Font Size Down");
  connect(but2, SIGNAL(clicked()), this, SLOT(zoomFontOut()));
  prefsWindow->show();

}

void MainWindow::zoomFontIn()
{
  outputPane->zoomIn(1);
}

void MainWindow::zoomFontOut()
{
  outputPane->zoomOut(1);
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
  connect(prefsAct, SIGNAL(triggered()), this, SLOT(prefs()));

}

void MainWindow::createToolBars()
{
  fileToolBar = addToolBar(tr("Run"));
  fileToolBar->addAction(runAct);
  fileToolBar->addAction(stopAct);
  fileToolBar->setIconSize(QSize(270/3, 109/3));
  fileToolBar->addAction(saveAsAct);

  QWidget *spacerWidget = new QWidget(this);
  spacerWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  spacerWidget->setVisible(true);

  supportToolBar = addToolBar(tr("Support"));
  supportToolBar->addWidget(spacerWidget);
  supportToolBar->addAction(infoAct);
  supportToolBar->addAction(helpAct);
  supportToolBar->setIconSize(QSize(270/3, 109/3));
  supportToolBar->addAction(prefsAct);
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

QString MainWindow::workspaceFilename(QsciScintilla* text)
{
  if(text == workspace1) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/one/1.spi";}
  else if(text == workspace2) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/two/1.spi";}
  else if(text == workspace3) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/three/1.spi";}
  else if(text == workspace4) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/four/1.spi";}
  else if(text == workspace5) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/five/1.spi";}
  else if(text == workspace6) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/six/1.spi";}
  else if(text == workspace7) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/seven/1.spi";}
  else if(text == workspace8) {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/eight/1.spi";}
 else {return QDir::homePath() + "/.sonic-pi/workspaces/"  + "/one/1.spi";}
}

bool MainWindow::saveWorkspace(QsciScintilla* text)
{
  QString label = currentTabLabel();
  saveFile(workspaceFilename(text), text);
  return true;
}

void MainWindow::onExitCleanup()
{
  if(serverProcess->state() == QProcess::NotRunning) {
    std::cout << "Server process is not running, something is up..." << std::endl;
    cont_listening_for_osc = false;
  } else {
    std::cout << "Asking server process to exit..." << std::endl;
    Message msg("/exit");
    sendOSC(msg);
  }
  std::cout << "Exiting..." << std::endl;

}
