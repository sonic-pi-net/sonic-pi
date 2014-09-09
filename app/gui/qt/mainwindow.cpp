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

// Qt stuff
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

// QScintilla stuff
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include <sonicpilexer.h>

// OSC stuff
#include "oscpkt.hh"
#include "udp.hh"

// OS specific stuff
#if defined(Q_OS_WIN)
  #include <QtConcurrent/QtConcurrentRun>
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
  QString prg_path = QCoreApplication::applicationDirPath() + "\\..\\..\\server\\native\\osx\\ruby\\bin\\ruby";
#elif defined(Q_OS_MAC)
  QString prg_path = QCoreApplication::applicationDirPath() + "/../../server/native/osx/ruby/bin/ruby";
#else
  //assuming Raspberry Pi
  QString prg_path = "ruby";
#endif

  QString prg_arg = QCoreApplication::applicationDirPath() + "/../../server/bin/sonic-pi-server.rb";
  prg_arg = QDir::toNativeSeparators(prg_arg);

  QStringList args;
  args << prg_arg;

  std::cout << prg_path.toStdString() << " " << prg_arg.toStdString() << std::endl;
  serverProcess->setStandardErrorFile("/tmp/sonic-pi-error");
  serverProcess->setStandardOutputFile("/tmp/sonic-pi-output");
  serverProcess->start(prg_path, args);
  serverProcess->waitForStarted();

  std::cerr << "started..." << serverProcess->state() << std::endl;

  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
  tabs->setTabPosition(QTabWidget::South);
  // create workspaces and add them to the tabs
  for(int ws = 0; ws < workspace_max; ws++) {
	  std::string s;

	  workspaces[ws] = new QsciScintilla;
	  QString w = QString("Workspace %1").arg(QString::number(ws + 1));
	  tabs->addTab(workspaces[ws], w);
  }

  lexer = new SonicPiLexer;
  lexer->setAutoIndentStyle(QsciScintilla::AiMaintain);

  // Autocompletion stuff
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

  outputWidget = new QDockWidget(tr("Log"), this);
  outputWidget->setFeatures(QDockWidget::NoDockWidgetFeatures);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);

  docsCentral = new QTabWidget;
  docsCentral->setTabsClosable(false);
  docsCentral->setMovable(false);
  docsCentral->setTabPosition(QTabWidget::West);
  docWidget = new QDockWidget("Help", this);
  docWidget->setAllowedAreas(Qt::BottomDockWidgetArea);
  docWidget->setWidget(docsCentral);

  tutorialDocPane = new QTextEdit;
  tutorialDocPane->setReadOnly(true);
  QString style = "QTextEdit { padding-left:10; padding-top:10; padding-bottom:10; padding-right:10 ; background:white;}";
  tutorialDocPane->setStyleSheet(style);
  tutorialDocPane->setHtml("<center><img src=\":/images/logo.png\" height=\"298\" width=\"365\"></center>");

  langDocPane = new QTextEdit;
  langDocPane->setReadOnly(true);
  langDocPane->setStyleSheet(style);

  synthsDocPane = new QTextEdit;
  synthsDocPane->setReadOnly(true);
  synthsDocPane->setStyleSheet(style);

  fxDocPane = new QTextEdit;
  fxDocPane->setReadOnly(true);
  fxDocPane->setStyleSheet(style);

  samplesDocPane = new QTextEdit;
  samplesDocPane->setReadOnly(true);
  samplesDocPane->setStyleSheet(style);

  examplesDocPane = new QTextEdit;
  examplesDocPane->setReadOnly(true);
  examplesDocPane->setStyleSheet(style);

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

  while (!server_started && cont_listening_for_osc) {
    sleep(1);
    std::cout << "Waiting for server..." << std::endl;
    if(osc_incoming_port_open) {
      Message msg("/ping");
      msg.pushStr("QtClient/1/hello");
      sendOSC(msg);
    }
  }

  loadWorkspaces();

  raspberryPiSystemVol = new QSlider(this);
  connect(raspberryPiSystemVol, SIGNAL(valueChanged(int)), this, SLOT(changeRPSystemVol(int)));
  initPrefsWindow();
  initDocsWindow();


  infoPane = new QTextEdit;
  infoPane->setReadOnly(true);
  infoPane->setFixedSize(550, 650);
  QString html;

  infoPane->setHtml("<center><img src=\":/images/logo.png\" height=\"298\" width=\"365\"><pre><font size=\"4\"><font color=\"DeepPink\">A Sound Synthesiser<br>for Live Coding</font><br><br>Designed and developed by Sam Aaron<br>in Cambridge, England<br><br><font color=\"DeepPink\">http://sonic-pi.net</font><br><br>For the latest updates follow<br><font color=\"DeepPink\">@sonic_pi<br></font></font></pre><h2><pre><font color=\"#3C3C3C\"><pre>music_as <font color=\"DeepPink\">:code</font><br>code_as <font color=\"DeepPink\">:art</font></pre></h2><pre><font size=\"4\"><br>v2.0.1</font></pre></center>");


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
  QTextEdit *historyT = new QTextEdit;
  historyT->setReadOnly(true);
  historyT->setHtml(s);

  //Contributors
  QFile file2(":/info/CONTRIBUTORS.html");
  if(!file2.open(QFile::ReadOnly | QFile::Text)) {
  }
  QString s2;
  QTextStream st2(&file2);
  s2.append(st2.readAll());
  QTextEdit *contributorsT = new QTextEdit;
  contributorsT->setReadOnly(true);
  contributorsT->setHtml(s2);

  //Community
  QFile file3(":/info/COMMUNITY.html");
  if(!file3.open(QFile::ReadOnly | QFile::Text)) {
  }
  QString s3;
  QTextStream st3(&file3);
  s3.append(st3.readAll());
  QTextEdit *communityT = new QTextEdit;
  communityT->setReadOnly(true);
  communityT->setHtml(s3);


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

void MainWindow::initPrefsWindow() {

  QGridLayout *grid = new QGridLayout;

  QGroupBox *volBox = new QGroupBox(tr("Raspberry Pi Settings"));
  volBox->setToolTip("Use this slider to change the system volume of your Raspberry Pi");
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
  grid->addWidget(audioOutputBox, 0, 0);
  grid->addWidget(volBox, 0, 1);
#endif
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
  QsciScintilla* ws = filenameToWorkspace(id.toStdString());
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


  QsciScintilla *ws = ((QsciScintilla*)tabs->currentWidget());
  ws->getCursorPosition(&currentLine, &currentIndex);
  ws->selectAll(true);
  errorPane->clear();
  errorPane->hide();
  statusBar()->showMessage(tr("Running Code...."), 1000);
  std::string code = ((QsciScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/save-and-run-buffer");
  std::string filename = workspaceFilename( (QsciScintilla*)tabs->currentWidget());
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
  ((QsciScintilla*)tabs->currentWidget())->selectAll(false);
  ((QsciScintilla*)tabs->currentWidget())->setCursorPosition(currentLine, currentIndex);
 }

 void MainWindow::beautifyCode()
 {
  statusBar()->showMessage(tr("Beautifying...."), 2000);
  std::string code = ((QsciScintilla*)tabs->currentWidget())->text().toStdString();
  Message msg("/beautify-buffer");
  std::string filename = workspaceFilename( (QsciScintilla*)tabs->currentWidget());
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


void MainWindow::stopCode()
{
  stopRunningSynths();
  statusBar()->showMessage(tr("Stopping..."), 2000);
}

void MainWindow::about()
{
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

  // Run
  runAct = new QAction(QIcon(":/images/run.png"), tr("&Run"), this);
#ifdef Q_OS_MAC
  runAct->setShortcut(tr("ctrl+R"));
  runAct->setToolTip(tr("Run the code in the current workspace (⌘R)"));
#else
  runAct->setShortcut(tr("alt+R"));
  runAct->setToolTip(tr("Run the code in the current workspace (alt-R)"));
#endif
  runAct->setStatusTip(tr("Run the code in the current workspace"));
  connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

  // Stop
  stopAct = new QAction(QIcon(":/images/stop.png"), tr("&Stop"), this);
#ifdef Q_OS_MAC
  stopAct->setShortcut(tr("ctrl+S"));
  stopAct->setToolTip(tr("Stop all running code (⌘S)"));
#else
  stopAct->setShortcut(tr("alt+S"));
  stopAct->setToolTip(tr("Stop all running code (alt-S)"));
#endif
  stopAct->setStatusTip(tr("Stop all running code"));
  connect(stopAct, SIGNAL(triggered()), this, SLOT(stopCode()));

  // Save
  saveAsAct = new QAction(QIcon(":/images/save.png"), tr("&Save &As..."), this);
  saveAsAct->setToolTip(tr("Export current workspace"));
  saveAsAct->setStatusTip(tr("Export current workspace"));
  connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

  // Info
  infoAct = new QAction(QIcon(":/images/info.png"), tr("&Info"), this);
  infoAct->setToolTip(tr("See information about Sonic Pi"));
  infoAct->setStatusTip(tr("See information about Sonic Pi"));
  connect(infoAct, SIGNAL(triggered()), this, SLOT(about()));

  // Help
  helpAct = new QAction(QIcon(":/images/help.png"), tr("&Help"), this);
#ifdef Q_OS_MAC
  helpAct->setShortcut(tr("ctrl+I"));
  helpAct->setToolTip(tr("Toggle help pane (⌘I)"));
#else
  helpAct->setShortcut(tr("alt+I"));
  helpAct->setToolTip(tr("Toggle help pane (alt-I)"));
#endif
  helpAct->setStatusTip(tr("Toggle help pane"));
  connect(helpAct, SIGNAL(triggered()), this, SLOT(help()));

  // Preferences
  prefsAct = new QAction(QIcon(":/images/prefs.png"), tr("&Prefs"), this);
  prefsAct->setToolTip(tr("Toggle preferences pane"));
  prefsAct->setStatusTip(tr("Toggle preferences pane"));
  connect(prefsAct, SIGNAL(triggered()), this, SLOT(showPrefsPane()));

  // Record
  recAct = new QAction(QIcon(":/images/rec.png"), tr("&Start &Recording"), this);
  recAct->setToolTip(tr("Start Recording"));
  recAct->setStatusTip(tr("Start Recording"));
  connect(recAct, SIGNAL(triggered()), this, SLOT(toggleRecording()));

  // Align
  textAlignAct = new QAction(QIcon(":/images/align.png"), tr("&Auto &Align &Text"), this);
#ifdef Q_OS_MAC
  textAlignAct->setShortcut(tr("ctrl+M"));
  textAlignAct->setToolTip(tr("Auto-align text (⌘M)"));
#else
  textAlignAct->setShortcut(tr("alt+M"));
  textAlignAct->setToolTip(tr("Auto-align text (alt-M)"));
#endif
  textAlignAct->setStatusTip(tr("Auto-align text"));
  connect(textAlignAct, SIGNAL(triggered()), this, SLOT(beautifyCode()));

  // Font Size Increase
  textIncAct1 = new QAction(QIcon(":/images/size_up.png"), tr("&Increase &Text &Size"), this);
  textIncAct2 = new QAction(this);
  textIncAct1->setStatusTip(tr("Make text bigger"));
#ifdef Q_OS_MAC
  textIncAct1->setShortcut(tr("ctrl++"));
  textIncAct2->setShortcut(tr("ctrl+="));
  textIncAct1->setToolTip(tr("Make text bigger (⌘+)"));
#else
  textIncAct1->setShortcut(tr("alt++"));
  textIncAct2->setShortcut(tr("alt+="));
  textIncAct1->setToolTip(tr("Make text bigger (alt+)"));
#endif
  textIncAct1->setStatusTip(tr("Make text bigger (alt+)"));
  connect(textIncAct1, SIGNAL(triggered()), this, SLOT(zoomFontIn()));
  connect(textIncAct2, SIGNAL(triggered()), this, SLOT(zoomFontIn()));

  // Font Size Decrease
  textDecAct1 = new QAction(QIcon(":/images/size_down.png"), tr("&Decrease &Text &Size"), this);
  textDecAct2 = new QAction(this);
#ifdef Q_OS_MAC
  textDecAct1->setShortcut(tr("ctrl+-"));
  textDecAct2->setShortcut(tr("ctrl+_"));
  textDecAct1->setToolTip(tr("Make text smaller (⌘-)"));
#else
  textDecAct1->setShortcut(tr("alt+-"));
  textDecAct2->setShortcut(tr("alt+_"));
  textDecAct1->setToolTip(tr("Make text smaller (alt-)"));
#endif
  textDecAct1->setStatusTip(tr("Make text smaller (alt-)"));
  connect(textDecAct1, SIGNAL(triggered()), this, SLOT(zoomFontOut()));
  connect(textDecAct2, SIGNAL(triggered()), this, SLOT(zoomFontOut()));
  addAction(textDecAct2);

  QAction *reloadAct = new QAction(this);
  reloadAct->setShortcut(QKeySequence(Qt::CTRL + Qt::Key_U));
  connect(reloadAct, SIGNAL(triggered()), this, SLOT(reloadServerCode()));
  addAction(reloadAct);
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

  toolBar->addAction(textDecAct2);
  toolBar->addAction(textIncAct2);
  toolBar->addAction(textDecAct1);
  toolBar->addAction(textIncAct1);
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
}

void MainWindow::writeSettings()
{
    QSettings settings("uk.ac.cam.cl", "Sonic Pi");
    settings.setValue("pos", pos());
    settings.setValue("size", size());
    settings.setValue("first_time", 0);
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
                  return "workspace_" + number_name(i + 1);
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
  osc_thread.waitForFinished();
  std::cout << "Exiting..." << std::endl;

}

void MainWindow::updateDocPane(QListWidgetItem *cur, QListWidgetItem *prev) {
  QString content = cur->data(32).toString();
  tutorialDocPane->setHtml(content);
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
