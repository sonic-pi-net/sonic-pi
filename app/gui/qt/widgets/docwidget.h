#ifndef DOCWIDGET_H
#define DOCWIDGET_H

#include <QWidget>
#include <QSplitter>
#include <QString>
#include <QCloseEvent>
#include <QListWidgetItem>
#include <QHash>
#include <QTextBrowser>


struct help_page {
    QString title;
    QString keyword;
    QString url;
};

struct help_entry {
    int pageIndex;
    int entryIndex;
};

class DocWidget : public QSplitter
{
    Q_OBJECT

signals:
    void closed();

public:
    explicit DocWidget(QString language, QWidget *parent = nullptr);

    void initDocsWindow();
    void refreshDocContent();
    void addHelpPage(QString section, QListWidget* nameList, struct help_page* helpPages, int len);
    QListWidget *createHelpTab(QString name);
    void focusDocsNavigation();
    void focusDocViewer();

    void setCurrentIndex(int index);
    void setStyleSheet(QString css);
    void findKeyword(QString keyword);

public slots:
    void docPrevTab();
    void docNextTab();
    void docScrollUp();
    void docScrollDown();
    void helpScrollUp();
    void helpScrollDown();

    void updateDocPane(QListWidgetItem *cur);
    void updateDocPane2(QListWidgetItem *cur, QListWidgetItem *prev);

signals:
    void addAutoCompleteKeyword(QString section, QString keyword);
    void addAutoCompleteArgs(QString type, QString name, QStringList args);
    void customUrlClicked(const QUrl& url);
 private:
    //void closeEvent(QCloseEvent *event);
    QList<QListWidget *> helpLists;
    QHash<QString, help_entry> helpKeywords;

    QTabWidget* docsNavigation;
    QTextBrowser* docViewer;

    QString doc_language;

private slots:
    void docLinkClicked(const QUrl& url);
};

#endif
