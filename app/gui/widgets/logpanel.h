#ifndef LOGPANEL_H
#define LOGPANEL_H

#include <QColor>
#include <QObject>
#include <QString>
#include <QSyntaxHighlighter>
#include <QTabWidget>
#include <QVector>

class QFileSystemWatcher;
class QLabel;
class QPlainTextEdit;
class QShowEvent;
class QHideEvent;
class QTimer;

// Mutes the leading "[HH:MM:SS.mmm]" stamp on each log line so the
// message itself stands out.
class LogTimestampHighlighter : public QSyntaxHighlighter
{
    Q_OBJECT
public:
    explicit LogTimestampHighlighter(QTextDocument* doc)
        : QSyntaxHighlighter(doc) {}

    void setColor(const QColor& c);

protected:
    void highlightBlock(const QString& text) override;

private:
    QColor m_color{ Qt::gray };
};

class LogTailer : public QObject
{
    Q_OBJECT
public:
    LogTailer(const QString& path, QPlainTextEdit* edit, QObject* parent = nullptr);

    void start();
    void stop();

private slots:
    void onFileChanged(const QString& path);
    void onDirectoryChanged(const QString& path);

private:
    void seekToTail();
    void readFromOffset();
    void rewatch();

    QString m_path;
    QPlainTextEdit* m_edit;
    QFileSystemWatcher* m_watcher = nullptr;
    QTimer* m_pollTimer = nullptr;   // reliable live updates (the watcher misses appends)
    qint64 m_offset = 0;
};

class LogPanel : public QTabWidget
{
    Q_OBJECT
public:
    struct Source { QString name; QString path; };

    // All sources share a single "Logs" tab, shown side by side in a
    // splitter and tailed concurrently while that tab is visible.
    LogPanel(const QVector<Source>& sources, QWidget* parent = nullptr);

    void applyTheme(const QColor& textColor, const QColor& bgColor);

    // Add a non-log tab (e.g. the live metrics panel). It is not backed by
    // LogTailers, so all tailers are stopped while it is shown.
    void addExtraTab(QWidget* w, const QString& name);

protected:
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;

private slots:
    void onCurrentChanged(int idx);

private:
    QWidget* m_logsTab = nullptr;
    QVector<QLabel*> m_labels;
    QVector<QPlainTextEdit*> m_edits;
    QVector<LogTailer*> m_tailers;
    QVector<LogTimestampHighlighter*> m_highlighters;
};

#endif
