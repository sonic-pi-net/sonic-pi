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
#include <QTextStream>
#include <QTextEdit>
#include <QToolBar>
#include <QProcess>
#include <QFont>
#include <QTabWidget>
#include <QString>
#include <QTextStream>
#include <QFile>
#include <QSplashScreen>
#include <QPixmap>
#include <QWindow>
#include <QLabel>
#include <QToolBox>
#include <QSlider>
#include <Qsci/qsciapis.h>
#include <Qsci/qsciscintilla.h>
#include <sonicpilexer.h>

#include "mainwindow.h"

MainWindow::MainWindow(QApplication &app)
{

  //ensureWorkspaces();
  //QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/start-server.rb";
  QString program = "/Users/sam/Development/RPi/sonic-pi/app/scripts/start-server.rb";
  serverProcess = new QProcess();
  serverProcess->start(program);
  serverProcess->waitForStarted();

  connect(serverProcess, SIGNAL(readyReadStandardOutput()),
          this, SLOT(updateOutput()));

  connect(serverProcess, SIGNAL(readyReadStandardError()),
          this, SLOT(updateError()));

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

  outputPane->zoomIn(7);
  errorPane->zoomIn(3);

  QDockWidget *outputWidget = new QDockWidget(tr("Output"), this);
  outputWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  outputWidget->setWidget(outputPane);
  addDockWidget(Qt::RightDockWidgetArea, outputWidget);

  QDockWidget *dockWidget = new QDockWidget(tr("Errors"), this);
  dockWidget->setAllowedAreas(Qt::RightDockWidgetArea);
  dockWidget->setWidget(errorPane);
  addDockWidget(Qt::RightDockWidgetArea, dockWidget);

  createActions();
  createMenus();
  createToolBars();
  createStatusBar();

  readSettings();

  // connect(textEdit, SIGNAL(textChanged()),
  //         this, SLOT(documentWasModified()));

  setWindowTitle(tr("Sonic Pi"));
  callInitScript();
  loadWorkspaces();

  // connect(runProcess, SIGNAL(readyReadStandardOutput()),
  //         this, SLOT(updateOutput()));

  connect(&app, SIGNAL( aboutToQuit() ), this, SLOT( onExitCleanup() ) );
}

void MainWindow::ensureWorkspaces()
{
  QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/ensure-workspaces.rb";
  QStringList arguments;
  QObject *parent;
  runProcess = new QProcess(parent);
  runProcess->start(program, arguments);
  runProcess->waitForFinished();
}

void MainWindow::onExitCleanup()
{
  //QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/shutdown.rb";
  QString program = "/Users/sam/Development/RPi/sonic-pi/app/scripts/kill-server.rb";
  QStringList arguments;
  QObject *parent;
  runProcess = new QProcess(parent);
  runProcess->start(program, arguments);
}

void MainWindow::loadWorkspaces()
{
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/one/1.spi", workspace1);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/two/1.spi", workspace2);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/three/1.spi", workspace3);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/four/1.spi", workspace4);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/five/1.spi", workspace5);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/six/1.spi", workspace6);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/seven/1.spi", workspace7);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/"  + "/eight/1.spi", workspace8);
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
    if (maybeSave()) {
        writeSettings();
        event->accept();
    } else {
        event->ignore();
    }
}

void MainWindow::newFile()
{

}

QString MainWindow::currentTabLabel()
{
  return tabs->tabText(tabs->currentIndex());
}


bool MainWindow::saveAs()
{
  QString fileName = QFileDialog::getSaveFileName(this, tr("Save Current Workspace"), QDir::homePath() + "/Desktop");
  saveFile(fileName, (QsciScintilla*)tabs->currentWidget());
}

void MainWindow::runCode()
{
  //  printf("running code");
  //  killSynths();
  saveWorkspace( (QsciScintilla*)tabs->currentWidget());
  saveFile("/tmp/sonic-pi-current-code.rb", (QsciScintilla*)tabs->currentWidget());
  //outputPane->clear();
  //errorPane->clear();
  lexer->setPaper(Qt::lightGray);
  QString emptyText = "";
  statusBar()->showMessage(tr("Running...."), 2000);

  //  clearOutputPanels();

  //  printf((QCoreApplication::applicationDirPath() + "/../../app/scripts/run-code.rb").toAscii().data());
  //QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/run-code.rb";
  //  QString program = "/Users/sam/Development/RPi/sonic-pi/app/scripts/start-server.rb";
  QString program = "/Users/sam/Development/RPi/sonic-pi/app/scripts/run-code.rb";
  runProcess = new QProcess();
  runProcess->startDetached(program);
  runProcess->waitForStarted();

  lexer->setPaper(Qt::white);
}

void MainWindow::killSynths()
{
  stopRunningSynths();
}

void MainWindow::stopCode()
{
  outputPane->clear();
  errorPane->clear();
  lexer->setPaper(Qt::red);
  statusBar()->showMessage(tr("Stopping..."), 2000);
  //  killSynths();
  //  clearOutputPanels();
  //QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/stop-code.rb";
  QString program = "/Users/sam/Development/RPi/sonic-pi/app/scripts/stop-code.rb";
  QProcess *p = new QProcess();
  p->startDetached(program);
  p->waitForStarted();

  // connect(runProcess, SIGNAL(readyReadStandardOutput()),
  //         this, SLOT(updateOutput()));

  // connect(runProcess, SIGNAL(readyReadStandardError()),
  //         this, SLOT(updateError()));

  // runProcess->start(program, arguments);
  //runProcess->write(currentTextArea()->text().toAscii());
  //  runProcess->waitForFinished();
  lexer->setPaper(Qt::white);
}

void MainWindow::updateError()
{
  QByteArray output = serverProcess->readAllStandardError();
  errorPane->append(output);
}

void MainWindow::updateOutput()
{
  QByteArray output = serverProcess->readAllStandardOutput();
  outputPane->append(output);
}

void MainWindow::open()
{

}

bool MainWindow::save()
{

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
   // QMessageBox::about(this, tr("About Sonic Pi"),
   //          tr("Sonic Pi \nMaking Computer Science Audible\n Copyright 2013, 2014, Sam Aaron \n Developed at the University of Cambridge Computer Laboratory \n http://www.cl.cam.ac.uk/projects/raspberrypi/sonicpi/"));

  QMessageBox about;

  about.setWindowTitle("Sonic Pi Help");
  about.setText("Sonic Pi - Making Computer Science Audible");
  about.setInformativeText("Version 2.0\nCopyright © 2013, 2014 Sam Aaron\n\nA University of Cambridge Computer Laboratory project developed in collaboration with the Raspberry Pi Foundation");
  about.setStandardButtons(QMessageBox::Ok);
  about.setDefaultButton(QMessageBox::Ok);
  about.show();
  about.exec();

}

void MainWindow::prefs()
{

  prefsWindow = new QMainWindow();
  QToolBox *tools = new QToolBox();
  QSlider *slider = new QSlider();
  prefsWindow->setCentralWidget(tools);
  tools->addItem(slider, "Volume");
  prefsWindow->show();
}

void MainWindow::documentWasModified()
{
  //    setWindowModified(textEdit->isModified());
}

// void MainWindow::textChanged()
// {
//   printf("changed!");
// }

void MainWindow::callInitScript()
{
  // QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/boot.rb";
  // QStringList arguments;
  // QObject *parent;
  // QProcess *myProcess = new QProcess(parent);
  // myProcess->start(program, arguments);
  // myProcess->waitForFinished();
}

void MainWindow::stopRunningSynths()
{
  // QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/stop-running-synths.rb";
  // QStringList arguments;
  // QObject *parent;
  // QProcess *myProcess = new QProcess(parent);
  // myProcess->start(program, arguments);
  // myProcess->waitForFinished();
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

    // newAct = new QAction(QIcon(":/images/new.png"), tr("&New"), this);
    // newAct->setShortcut(tr("Ctrl+N"));
    // newAct->setStatusTip(tr("Create a new file"));
    // connect(newAct, SIGNAL(triggered()), this, SLOT(newFile()));

    // openAct = new QAction(QIcon(":/images/open.png"), tr("&Open..."), this);
    // openAct->setShortcut(tr("Ctrl+O"));
    // openAct->setStatusTip(tr("Open an existing file"));
    // connect(openAct, SIGNAL(triggered()), this, SLOT(open()));
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

    // exitAct = new QAction(tr("E&xit"), this);
    // exitAct->setShortcut(tr("Ctrl+Q"));
    // exitAct->setStatusTip(tr("Exit the application"));
    // connect(exitAct, SIGNAL(triggered()), this, SLOT(close()));

    // cutAct = new QAction(QIcon(":/images/cut.png"), tr("Cu&t"), this);
    // cutAct->setShortcut(tr("Ctrl+X"));
    // cutAct->setStatusTip(tr("Cut the current selection's contents to the "
    //                         "clipboard"));
    // connect(cutAct, SIGNAL(triggered()), textEdit, SLOT(cut()));

    // copyAct = new QAction(QIcon(":/images/copy.png"), tr("&Copy"), this);
    // copyAct->setShortcut(tr("Ctrl+C"));
    // copyAct->setStatusTip(tr("Copy the current selection's contents to the "
    //                          "clipboard"));
    // connect(copyAct, SIGNAL(triggered()), textEdit, SLOT(copy()));

    // pasteAct = new QAction(QIcon(":/images/paste.png"), tr("&Paste"), this);
    // pasteAct->setShortcut(tr("Ctrl+V"));
    // pasteAct->setStatusTip(tr("Paste the clipboard's contents into the current "
    //                           "selection"));
    // connect(pasteAct, SIGNAL(triggered()), textEdit, SLOT(paste()));
    // cutAct->setEnabled(false);
    // copyAct->setEnabled(false);
    // connect(textEdit, SIGNAL(copyAvailable(bool)),
    //         cutAct, SLOT(setEnabled(bool)));
    // connect(textEdit, SIGNAL(copyAvailable(bool)),
    //         copyAct, SLOT(setEnabled(bool)));
}

void MainWindow::createMenus()
{
    // fileMenu = menuBar()->addMenu(tr("&File"));
    // fileMenu->addAction(newAct);
    // fileMenu->addAction(openAct);
    // fileMenu->addAction(saveAct);
    // fileMenu->addAction(saveAsAct);
    // fileMenu->addSeparator();
    // fileMenu->addAction(exitAct);

    // editMenu = menuBar()->addMenu(tr("&Edit"));
    // editMenu->addAction(cutAct);
    // editMenu->addAction(copyAct);
    // editMenu->addAction(pasteAct);

    // menuBar()->addSeparator();

    // helpMenu = menuBar()->addMenu(tr("&Help"));
    // helpMenu->addAction(aboutAct);
    // helpMenu->addAction(aboutQtAct);f
}

void MainWindow::createToolBars()
{



  fileToolBar = addToolBar(tr("Run"));
  fileToolBar->addAction(runAct);
  fileToolBar->addAction(stopAct);
  fileToolBar->setIconSize(QSize(270/2.5, 109/2.5));
  fileToolBar->addAction(saveAsAct);

  QWidget *spacerWidget = new QWidget(this);
  spacerWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  spacerWidget->setVisible(true);

  supportToolBar = addToolBar(tr("Support"));
  supportToolBar->addWidget(spacerWidget);
  supportToolBar->addAction(infoAct);
  supportToolBar->addAction(helpAct);
  supportToolBar->setIconSize(QSize(270/2.5, 109/2.5));
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

bool MainWindow::maybeSave()
{
    // if (textEdit->isModified()) {
    //     int ret = QMessageBox::warning(this, tr("Application"),
    //                  tr("The document has been modified.\n"
    //                     "Do you want to save your changes?"),
    //                  QMessageBox::Yes | QMessageBox::Default,
    //                  QMessageBox::No,
    //                  QMessageBox::Cancel | QMessageBox::Escape);
    //     if (ret == QMessageBox::Yes)
    //         return save();
    //     else if (ret == QMessageBox::Cancel)
    //         return false;
    // }
    return true;
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
    //    statusBar()->showMessage(tr("File saved"), 2000);
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

// void MainWindow::switchWorkspace(const QString &fileName)
// {
//   //curWorkspace = filename;
// }

void MainWindow::setCurrentFile(const QString &fileName)
{
    curFile = fileName;
    //    textEdit->setModified(false);
    setWindowModified(false);

}

QString MainWindow::strippedName(const QString &fullFileName)
{
    return QFileInfo(fullFileName).fileName();
}
