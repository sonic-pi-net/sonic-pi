#ifndef LOGPANEL_H
#define LOGPANEL_H

#include <QColor>
#include <QObject>
#include <QString>
#include <QTabWidget>
#include <QVector>

class QFileSystemWatcher;
class QPlainTextEdit;
class QShowEvent;
class QHideEvent;
class QTimer;

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

    LogPanel(const QVector<Source>& sources, QWidget* parent = nullptr);

    void applyTheme(const QColor& textColor, const QColor& bgColor, const QColor& borderColor);

protected:
    void showEvent(QShowEvent* e) override;
    void hideEvent(QHideEvent* e) override;

private slots:
    void onCurrentChanged(int idx);

private:
    QVector<QPlainTextEdit*> m_edits;
    QVector<LogTailer*> m_tailers;
};

#endif
