#include "logpanel.h"
#include "thinsplitter.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QFileSystemWatcher>
#include <QFont>
#include <QKeySequence>
#include <QLabel>
#include <QPlainTextEdit>
#include <QRegularExpression>
#include <QScrollBar>
#include <QShortcut>
#include <QSplitter>
#include <QTextCharFormat>
#include <QTabBar>
#include <QTextStream>
#include <QTimer>
#include <QVBoxLayout>

static const int kMaxBlocks = 5000;
static const qint64 kInitialTailBytes = 256 * 1024;

void LogTimestampHighlighter::setColor(const QColor& c)
{
    m_color = c;
    rehighlight();
}

void LogTimestampHighlighter::highlightBlock(const QString& text)
{
    static const QRegularExpression re(
        QStringLiteral("^\\[\\d{2}:\\d{2}:\\d{2}\\.\\d{3}\\]"));
    const auto m = re.match(text);
    if (m.hasMatch()) {
        QTextCharFormat f;
        f.setForeground(m_color);
        setFormat(0, static_cast<int>(m.capturedLength()), f);
    }
}

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

    QFont mono("Hack", 7, -1, false);
    mono.setStyleHint(QFont::Monospace);
    mono.setFixedPitch(true);

    auto bindShortcut = [](QPlainTextEdit* edit, QKeySequence keys, void (QPlainTextEdit::*slot)()) {
        QShortcut* s = new QShortcut(keys, edit);
        s->setContext(Qt::WidgetShortcut);
        connect(s, &QShortcut::activated, edit, slot);
    };

    m_logsTab = new QWidget(this);
    QVBoxLayout* tabLayout = new QVBoxLayout(m_logsTab);
    tabLayout->setContentsMargins(0, 0, 0, 0);
    tabLayout->setSpacing(0);

    ThinSplitter* splitter = new ThinSplitter(Qt::Horizontal, m_logsTab);
    m_splitter = splitter;
    splitter->setChildrenCollapsible(false);
    splitter->setHandleWidth(7);
    tabLayout->addWidget(splitter);

    for (const Source& src : sources) {
        QWidget* pane = new QWidget(splitter);
        QVBoxLayout* paneLayout = new QVBoxLayout(pane);
        paneLayout->setContentsMargins(0, 0, 0, 0);
        paneLayout->setSpacing(0);

        QLabel* label = new QLabel(src.name.toUpper(), pane);
        label->setObjectName("paneTitle");   // shared small/muted/left title style
        label->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
        paneLayout->addWidget(label);

        QPlainTextEdit* edit = new QPlainTextEdit(pane);
        edit->setReadOnly(true);
        edit->setMaximumBlockCount(kMaxBlocks);
        edit->setLineWrapMode(QPlainTextEdit::NoWrap);
        edit->setFont(mono);
        edit->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::TextSelectableByKeyboard);
        paneLayout->addWidget(edit);

        bindShortcut(edit, QKeySequence::Copy, &QPlainTextEdit::copy);
        bindShortcut(edit, QKeySequence::SelectAll, &QPlainTextEdit::selectAll);

        splitter->addWidget(pane);
        m_labels.append(label);
        m_edits.append(edit);
        m_tailers.append(new LogTailer(src.path, edit, this));
        m_highlighters.append(new LogTimestampHighlighter(edit->document()));
    }

    addTab(m_logsTab, tr("Logs"));
    // Only the single Logs tab by default — hide the redundant tab bar. It
    // reappears if addExtraTab() adds a sibling.
    tabBar()->setVisible(false);

    connect(this, &QTabWidget::currentChanged, this, &LogPanel::onCurrentChanged);
}

void LogPanel::setFontZoom(int level)
{
    QFont mono("Hack", qMax(5, 7 + level), -1, false);
    mono.setStyleHint(QFont::Monospace);
    mono.setFixedPitch(true);
    for (QPlainTextEdit* edit : m_edits)
        edit->setFont(mono);
}

void LogPanel::addExtraTab(QWidget* w, const QString& name)
{
    // Inserted as the first tab and made current. No LogTailers are
    // registered for it, so onCurrentChanged stops all tailers while it
    // is shown.
    insertTab(0, w, name);
    setCurrentIndex(0);
    tabBar()->setVisible(true);   // more than one tab now — show the bar
}

void LogPanel::applyTheme(const QColor& textColor, const QColor& bgColor)
{
    const QString editCss = QString(
        "QPlainTextEdit {"
        "  color: %1;"
        "  background-color: %2;"
        "  border: none;"
        "  padding: 6px;"
        "}").arg(textColor.name(), bgColor.name());
    for (QPlainTextEdit* edit : m_edits) edit->setStyleSheet(editCss);

    // Source labels use the shared #paneTitle style (small/muted/left, like the
    // SuperSonic debug pane titles) from app.qss — no per-widget override here.

    // Panes blend into the log background; the header strips are the only
    // resting separators. Per-widget styles above override this cascade.
    if (m_logsTab)
        m_logsTab->setStyleSheet(QString("QWidget { background-color: %1; }").arg(bgColor.name()));

    // Timestamps sit halfway between text and background — readable but
    // clearly secondary to the message.
    const QColor muted((textColor.red() + bgColor.red()) / 2,
                       (textColor.green() + bgColor.green()) / 2,
                       (textColor.blue() + bgColor.blue()) / 2);

    // The dividers stay invisible at rest (line == background, matching the
    // header-strip design) but reveal a subtle grab handle on hover.
    if (m_splitter)
        m_splitter->setDividerColors(bgColor, bgColor, muted);
    for (LogTimestampHighlighter* h : m_highlighters) h->setColor(muted);
}

void LogPanel::onCurrentChanged(int idx)
{
    if (!isVisible()) return;
    // All log files live on the single Logs tab and tail together while
    // it is the current tab.
    bool logsShown = (widget(idx) == m_logsTab);
    for (LogTailer* t : m_tailers) {
        if (logsShown) t->start();
        else t->stop();
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
