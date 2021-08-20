#include <iostream>

#include <QWidget>
#include <QListWidget>
#include <QTabWidget>
#include <QTextBrowser>
#include <QString>
#include <QStringList>
#include <QSizePolicy>
#include <QShortcut>
#include <QProxyStyle>
#include <QBoxLayout>
#include <QScrollBar>
#include <QDesktopServices>

#include "docwidget.h"

#include "utils/ruby_help.h"
#include "utils/shortcuts.h"
#include "utils/borderlesslinksproxystyle.h"

DocWidget::DocWidget(QString language, QWidget *parent) {
    this->doc_language = language;

    docsNavigation = new QTabWidget;
    docsNavigation->setFocusPolicy(Qt::NoFocus);
    docsNavigation->setTabsClosable(false);
    docsNavigation->setMovable(false);
    docsNavigation->setTabPosition(QTabWidget::South);
    QShortcut* left = new QShortcut(Qt::Key_Left, docsNavigation);
    left->setContext(Qt::WidgetWithChildrenShortcut);
    connect(left, SIGNAL(activated()), this, SLOT(docPrevTab()));
    QShortcut* right = new QShortcut(Qt::Key_Right, docsNavigation);
    right->setContext(Qt::WidgetWithChildrenShortcut);
    connect(right, SIGNAL(activated()), this, SLOT(docNextTab()));

    docViewer = new QTextBrowser;
    QSizePolicy policy = docViewer->sizePolicy();
    policy.setHorizontalStretch(QSizePolicy::Maximum);
    docViewer->setSizePolicy(policy);
    docViewer->setMinimumHeight(100);
    docViewer->setOpenLinks(false);
    docViewer->setOpenExternalLinks(true);
    docViewer->setStyle(new BorderlessLinksProxyStyle);
    connect(docViewer, SIGNAL(anchorClicked(const QUrl&)), this, SLOT(docLinkClicked(const QUrl&)));
    QShortcut* up = new QShortcut(ctrlKey('p'), docViewer);
    up->setContext(Qt::WidgetShortcut);
    connect(up, SIGNAL(activated()), this, SLOT(docScrollUp()));
    QShortcut* down = new QShortcut(ctrlKey('n'), docViewer);
    down->setContext(Qt::WidgetShortcut);
    connect(down, SIGNAL(activated()), this, SLOT(docScrollDown()));

    docViewer->setSource(QUrl("qrc:///html/doc.html"));

    this->addWidget(docsNavigation);
    this->addWidget(docViewer);
}

//DocWidget::~DocWidget() {

//}

//DocWidget::closeEvent() {

//}

void DocWidget::setCurrentIndex(int index) {
    docsNavigation->setCurrentIndex(index);
}

void DocWidget::findKeyword(QString keyword) {
    if (helpKeywords.contains(keyword)) {
        struct help_entry entry = helpKeywords[keyword];
        QListWidget* list = helpLists[entry.pageIndex];

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

        docsNavigation->setCurrentIndex(entry.pageIndex);
        list->setCurrentRow(entry.entryIndex);
    }
}


void DocWidget::updateDocPane(QListWidgetItem* cur)
{
    QString url = cur->data(32).toString();
    docViewer->setSource(QUrl(url));
}

void DocWidget::updateDocPane2(QListWidgetItem* cur, QListWidgetItem* prev)
{
    (void)prev;
    updateDocPane(cur);
}

void DocWidget::addHelpPage(QString section, QListWidget* nameList, struct help_page* helpPages, int len) {
    int i;
    struct help_entry entry;
    entry.pageIndex = docsNavigation->count() - 1;

    for (i = 0; i < len; i++)
    {
        QListWidgetItem* item = new QListWidgetItem(helpPages[i].title);
        item->setData(32, QVariant(helpPages[i].url));
        nameList->addItem(item);
        entry.entryIndex = nameList->count() - 1;

        if (helpPages[i].keyword != NULL)
        {
            helpKeywords.insert(helpPages[i].keyword, entry);
            emit addAutoCompleteKeyword(section, helpPages[i].keyword);
        }
    }
}

QListWidget* DocWidget::createHelpTab(QString name) {
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
    docsNavigation->addTab(tabWidget, name);
    helpLists.append(nameList);
    return nameList;
}

void DocWidget::helpScrollUp()
{
    int section = docsNavigation->currentIndex();
    int entry = helpLists[section]->currentRow();

    if (entry > 0)
        entry--;
    helpLists[section]->setCurrentRow(entry);
}

void DocWidget::helpScrollDown()
{
    int section = docsNavigation->currentIndex();
    int entry = helpLists[section]->currentRow();

    if (entry < helpLists[section]->count() - 1)
        entry++;
    helpLists[section]->setCurrentRow(entry);
}

void DocWidget::docPrevTab()
{
    int section = docsNavigation->currentIndex();
    if (section > 0)
        docsNavigation->setCurrentIndex(section - 1);
}

void DocWidget::docNextTab()
{
    int section = docsNavigation->currentIndex();
    if (section < docsNavigation->count() - 1)
        docsNavigation->setCurrentIndex(section + 1);
}

void DocWidget::docScrollUp()
{
    docViewer->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepSub);
}

void DocWidget::docScrollDown()
{
    docViewer->verticalScrollBar()->triggerAction(QAbstractSlider::SliderSingleStepAdd);
}

void DocWidget::docLinkClicked(const QUrl& url)
{
    QString link = url.toDisplayString();
    std::cout << "[GUI] Link clicked: " << link.toStdString() << std::endl;

    if (url.scheme() == "sonicpi")
    {
        emit customUrlClicked(url);
    }
    else if (url.isRelative() || url.isLocalFile() || url.scheme() == "qrc")
    {
        docViewer->setSource(url);
    }
    else
    {
        QDesktopServices::openUrl(url);
    }
}


void DocWidget::setStyleSheet(QString css) {
    docViewer->document()->setDefaultStyleSheet(css);
    docViewer->reload();
}

void DocWidget::focusDocsNavigation() {
    docsNavigation->showNormal();
    docsNavigation->currentWidget()->setFocus();
    docsNavigation->raise();
    docsNavigation->setVisible(true);
    docsNavigation->activateWindow();
}

void DocWidget::focusDocViewer() {
    docViewer->showNormal();
    docViewer->setFocusPolicy(Qt::StrongFocus);
    docViewer->setFocus();
    docViewer->raise();
    docViewer->setVisible(true);
    docViewer->activateWindow();
}
