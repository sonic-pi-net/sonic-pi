#include "logpanel.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QFileSystemWatcher>
#include <QFont>
#include <QKeySequence>
#include <QPlainTextEdit>
#include <QScrollBar>
#include <QShortcut>
#include <QTabBar>
#include <QTextStream>
#include <QTimer>

static const int kMaxBlocks = 5000;
static const qint64 kInitialTailBytes = 256 * 1024;

LogTailer::LogTailer(const QString& path, QPlainTextEdit* edit, QObject* parent)
    : QObject(parent)
    , m_path(path)
    , m_edit(edit)
{
}

void LogTailer::start()
{
    if (m_watcher) return;

    m_watcher = new QFileSystemWatcher(this);
    connect(m_watcher, &QFileSystemWatcher::fileChanged,
            this, &LogTailer::onFileChanged);
    connect(m_watcher, &QFileSystemWatcher::directoryChanged,
            this, &LogTailer::onDirectoryChanged);

    m_offset = 0;
    m_edit->clear();
    seekToTail();
    rewatch();
    readFromOffset();

    // QFileSystemWatcher misses most log appends (especially on macOS), so poll
    // for new content too — readFromOffset only reads the bytes past m_offset.
    if (!m_pollTimer) {
        m_pollTimer = new QTimer(this);
        m_pollTimer->setInterval(200);
        connect(m_pollTimer, &QTimer::timeout, this, &LogTailer::readFromOffset);
    }
    m_pollTimer->start();
}

void LogTailer::stop()
{
    if (m_pollTimer) m_pollTimer->stop();
    if (!m_watcher) return;
    m_watcher->deleteLater();
    m_watcher = nullptr;
}

void LogTailer::rewatch()
{
    if (!m_watcher) return;
    if (QFileInfo::exists(m_path)) {
        m_watcher->addPath(m_path);
    } else {
        QString dir = QFileInfo(m_path).absolutePath();
        if (QDir(dir).exists()) {
            m_watcher->addPath(dir);
        }
    }
}

void LogTailer::seekToTail()
{
    QFile f(m_path);
    if (!f.open(QIODevice::ReadOnly)) return;

    qint64 size = f.size();
    if (size <= kInitialTailBytes) return;

    f.seek(size - kInitialTailBytes);
    // Align to next newline so the first chunk doesn't start mid-line.
    char c;
    while (!f.atEnd() && f.getChar(&c) && c != '\n') {}
    m_offset = f.pos();
}

void LogTailer::readFromOffset()
{
    QFile f(m_path);
    if (!f.open(QIODevice::ReadOnly | QIODevice::Text)) return;

    qint64 size = f.size();
    if (size < m_offset) {
        m_edit->clear();
        m_offset = 0;
    }
    if (size == m_offset) return;

    f.seek(m_offset);
    QTextStream stream(&f);
    QString chunk = stream.readAll();
    m_offset = f.pos();

    if (chunk.isEmpty()) return;
    if (chunk.endsWith('\n')) chunk.chop(1);

    QScrollBar* sb = m_edit->verticalScrollBar();
    bool atBottom = (sb->value() >= sb->maximum() - 4);

    m_edit->appendPlainText(chunk);

    if (atBottom) sb->setValue(sb->maximum());
}

void LogTailer::onFileChanged(const QString& /*path*/)
{
    if (!QFileInfo::exists(m_path)) {
        // File was deleted: reset state so the offset isn't stale if it's recreated.
        m_offset = 0;
        m_edit->clear();
        if (m_watcher) m_watcher->removePath(m_path);
        rewatch();
        return;
    }
    readFromOffset();
    if (m_watcher && !m_watcher->files().contains(m_path)) {
        rewatch();
    }
}

void LogTailer::onDirectoryChanged(const QString& /*path*/)
{
    if (!m_watcher || !QFileInfo::exists(m_path)) return;
    QString dir = QFileInfo(m_path).absolutePath();
    m_watcher->removePath(dir);
    rewatch();
    readFromOffset();
}

LogPanel::LogPanel(const QVector<Source>& sources, QWidget* parent)
    : QTabWidget(parent)
{
    setFocusPolicy(Qt::NoFocus);
    setTabPosition(QTabWidget::South);
    setDocumentMode(false);
    tabBar()->setExpanding(false);
    setStyleSheet("QTabWidget::tab-bar { alignment: center; }");

    QFont mono("Hack", 9, -1, false);
    mono.setStyleHint(QFont::Monospace);
    mono.setFixedPitch(true);

    auto bindShortcut = [](QPlainTextEdit* edit, QKeySequence keys, void (QPlainTextEdit::*slot)()) {
        QShortcut* s = new QShortcut(keys, edit);
        s->setContext(Qt::WidgetShortcut);
        connect(s, &QShortcut::activated, edit, slot);
    };

    for (const Source& src : sources) {
        QPlainTextEdit* edit = new QPlainTextEdit(this);
        edit->setReadOnly(true);
        edit->setMaximumBlockCount(kMaxBlocks);
        edit->setLineWrapMode(QPlainTextEdit::NoWrap);
        edit->setFont(mono);
        edit->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);

        bindShortcut(edit, QKeySequence::Copy, &QPlainTextEdit::copy);
        bindShortcut(edit, QKeySequence::SelectAll, &QPlainTextEdit::selectAll);

        addTab(edit, src.name);
        m_edits.append(edit);
        m_tailers.append(new LogTailer(src.path, edit, this));
    }

    connect(this, &QTabWidget::currentChanged, this, &LogPanel::onCurrentChanged);
}

void LogPanel::applyTheme(const QColor& textColor, const QColor& bgColor, const QColor& borderColor)
{
    const QString css = QString(
        "QPlainTextEdit {"
        "  color: %1;"
        "  background-color: %2;"
        "  border: 1px solid %3;"
        "  padding: 6px;"
        "}").arg(textColor.name(), bgColor.name(), borderColor.name());
    for (QPlainTextEdit* edit : m_edits) edit->setStyleSheet(css);
}

void LogPanel::onCurrentChanged(int idx)
{
    if (!isVisible()) return;
    for (int i = 0; i < m_tailers.size(); ++i) {
        if (i == idx) m_tailers[i]->start();
        else m_tailers[i]->stop();
    }
}

void LogPanel::showEvent(QShowEvent* e)
{
    QTabWidget::showEvent(e);
    onCurrentChanged(currentIndex());
}

void LogPanel::hideEvent(QHideEvent* e)
{
    QTabWidget::hideEvent(e);
    for (LogTailer* t : m_tailers) t->stop();
}
