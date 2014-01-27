/****************************************************************************
**
** Copyright (c) 2013 Samuel Aaron
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
#include <Qsci/qsciscintilla.h>
#include <Qsci/qscilexerruby.h>

#include "mainwindow.h"

MainWindow::MainWindow(QApplication &app)
{

  ensureWorkspaces();

  runProcess =  NULL;
  groupName = "default";
  QFile file("/tmp/sonic-pi/group-name");
  bool ret = file.open(QIODevice::ReadOnly | QIODevice::Text);
  if( ret )
    {
      QTextStream stream(&file);
      groupName = stream.readAll().trimmed();
    }

  QMap<QString, QString> map;
  tabs = new QTabWidget();
  tabs->setTabsClosable(false);
  tabs->setMovable(false);
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

  workspace1->setUtf8(true);
  workspace2->setUtf8(true);
  workspace3->setUtf8(true);
  workspace4->setUtf8(true);
  workspace5->setUtf8(true);
  workspace6->setUtf8(true);
  workspace7->setUtf8(true);
  workspace8->setUtf8(true);

  lexer = new QsciLexerRuby;

  QFont font("Monospace");
  font.setStyleHint(QFont::Monospace);
  lexer->setDefaultFont(font);

  workspace1->setLexer(lexer);
  workspace1->zoomIn(13);

  workspace2->setLexer(lexer);
  workspace2->zoomIn(13);

  workspace3->setLexer(lexer);
  workspace3->zoomIn(13);

  workspace4->setLexer(lexer);
  workspace4->zoomIn(13);

  workspace5->setLexer(lexer);
  workspace5->zoomIn(13);

  workspace6->setLexer(lexer);
  workspace6->zoomIn(13);

  workspace7->setLexer(lexer);
  workspace7->zoomIn(13);

  workspace8->setLexer(lexer);
  workspace8->zoomIn(13);

  outputPane = new QTextEdit;
  errorPane = new QsciScintilla;

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

  //  setWindowTitle(tr("Sonic Pi"));
  setWindowTitle("Sonic Pi - " + groupName);
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
  QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/shutdown.rb";
  QStringList arguments;
  QObject *parent;
  runProcess = new QProcess(parent);
  runProcess->start(program, arguments);
}

void MainWindow::loadWorkspaces()
{
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/one/1.spi", workspace1);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/two/1.spi", workspace2);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/three/1.spi", workspace3);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/four/1.spi", workspace4);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/five/1.spi", workspace5);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/six/1.spi", workspace6);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/seven/1.spi", workspace7);
  loadFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/eight/1.spi", workspace8);
}

void MainWindow::saveWorkspaces()
{
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/one/1.spi", workspace1);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/two/1.spi", workspace2);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/three/1.spi", workspace3);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/four/1.spi", workspace4);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/five/1.spi", workspace5);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/six/1.spi", workspace6);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/seven/1.spi", workspace7);
  saveFile(QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/eight/1.spi", workspace8);
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
  killSynths();
  saveWorkspace( (QsciScintilla*)tabs->currentWidget());
  saveFile("/tmp/sonic-pi-current-code.rb", (QsciScintilla*)tabs->currentWidget());
  outputPane->clear();
  errorPane->clear();
  lexer->setPaper(Qt::lightGray);
  QString emptyText = "";
  statusBar()->showMessage(tr("Running..."), 2000);

  //  clearOutputPanels();

  //  printf((QCoreApplication::applicationDirPath() + "/../../app/scripts/run-code.rb").toAscii().data());
  QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/run-code.rb";
  QStringList arguments;
  arguments << "/tmp/sonic-pi-current-code.rb";
  QObject *parent;
  runProcess = new QProcess(parent);

  connect(runProcess, SIGNAL(readyReadStandardOutput()),
          this, SLOT(updateOutput()));

  connect(runProcess, SIGNAL(readyReadStandardError()),
          this, SLOT(updateError()));



  runProcess->start(program, arguments);
  //runProcess->write(tabs->currentWidget()->text().toAscii());
  //  runProcess->waitForFinished();
  lexer->setPaper(Qt::white);
}

void MainWindow::killSynths()
{
  if (runProcess)
    {
      runProcess->kill();
    }
  stopRunningSynths();
}

void MainWindow::stopCode()
{
  outputPane->clear();
  errorPane->clear();
  lexer->setPaper(Qt::red);
  statusBar()->showMessage(tr("Stopping..."), 2000);
  killSynths();
  //  clearOutputPanels();
  // QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/stop-code.rb";
  // QStringList arguments;
  // arguments << "/tmp/sonic-pi";
  // QObject *parent;
  // runProcess = new QProcess(parent);

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
  QByteArray output = runProcess->readAllStandardError();
  errorPane->append(output);
}

void MainWindow::updateOutput()
{
  QByteArray output = runProcess->readAllStandardOutput();
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
   QMessageBox::about(this, tr("About Sonic Pi"),
            tr("<b>Sonic Pi</b> is an experimental language and application"
               "for using creative processes to engage students with a"
               "computer science curriculum."));
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
  QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/boot.rb";
  QStringList arguments;
  QObject *parent;
  QProcess *myProcess = new QProcess(parent);
  myProcess->start(program, arguments);
  myProcess->waitForFinished();
}

void MainWindow::stopRunningSynths()
{
  QString program = QCoreApplication::applicationDirPath() + "/../../app/scripts/stop-running-synths.rb";
  QStringList arguments;
  QObject *parent;
  QProcess *myProcess = new QProcess(parent);
  myProcess->start(program, arguments);
  myProcess->waitForFinished();
}

void MainWindow::clearOutputPanels()
{
    outputPane->clear();
    errorPane->clear();
}

void MainWindow::createActions()
{

  runAct = new QAction(QIcon(":/images/save.png"), tr("&Run"), this);
  runAct->setShortcut(tr("Ctrl+R"));
  runAct->setStatusTip(tr("Run code"));
  connect(runAct, SIGNAL(triggered()), this, SLOT(runCode()));

  stopAct = new QAction(QIcon(":/images/new.png"), tr("&Stop"), this);
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
  saveAsAct = new QAction(QIcon(":/images/open.png"), tr("&Save &As..."), this);
  saveAsAct->setStatusTip(tr("Save the document under a new name"));
  connect(saveAsAct, SIGNAL(triggered()), this, SLOT(saveAs()));

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

    aboutAct = new QAction(tr("&About"), this);
    aboutAct->setStatusTip(tr("Show the application's About box"));
    connect(aboutAct, SIGNAL(triggered()), this, SLOT(about()));

    aboutQtAct = new QAction(tr("About &Qt"), this);
    aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
    connect(aboutQtAct, SIGNAL(triggered()), qApp, SLOT(aboutQt()));

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
    // fileToolBar->addAction(newAct);
    // fileToolBar->addAction(openAct);
    fileToolBar->addAction(saveAsAct);
}

void MainWindow::createStatusBar()
{
    statusBar()->showMessage(tr("Ready"));
}

void MainWindow::readSettings()
{
    QSettings settings("Trolltech", "Application Example");
    QPoint pos = settings.value("pos", QPoint(200, 200)).toPoint();
    QSize size = settings.value("size", QSize(400, 400)).toSize();
    resize(size);
    move(pos);
}

void MainWindow::writeSettings()
{
    QSettings settings("Trolltech", "Application Example");
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
  if(text == workspace1) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/one/1.spi";}
  else if(text == workspace2) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/two/1.spi";}
  else if(text == workspace3) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/three/1.spi";}
  else if(text == workspace4) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/four/1.spi";}
  else if(text == workspace5) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/five/1.spi";}
  else if(text == workspace6) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/six/1.spi";}
  else if(text == workspace7) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/seven/1.spi";}
  else if(text == workspace8) {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/eight/1.spi";}
 else {return QDir::homePath() + "/.sonic-pi/workspaces/" + groupName + "/one/1.spi";}
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
