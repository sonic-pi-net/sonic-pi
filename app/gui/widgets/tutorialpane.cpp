//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "tutorialpane.h"
#include "tutorialwidgets.h"
#include "tutscope.h"
#include "dpi.h"
#include "model/sonicpitheme.h"
#include "sonicpiscintilla.h"
#include "utils/instrument_icons.h"
#include "utils/tablericons.h"
#include "widgets/zoombar.h"
#include "api/sonicpi_api.h"

#include <QAbstractTextDocumentLayout>
#include <QApplication>
#include <QClipboard>
#include <QCursor>
#include <QLineEdit>
#include <QPointer>
#include <QTextCursor>
#include <QTextBlock>
#include <QTextDocument>
#include <QTextDocumentFragment>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QRegularExpression>
#include <QKeyEvent>
#include <QKeySequence>
#include <QLabel>
#include <QMouseEvent>
#include <QPainter>
#include <QPainterPath>
#include <QPixmap>
#include <QPushButton>
#include <QScrollArea>
#include <QScrollBar>
#include <QSpacerItem>
#include <QStringList>
#include <QStyle>
#include <QSvgRenderer>
#include <QTimer>
#include <QVBoxLayout>
#include <QtMath>

#include <algorithm>
#include <cmath>
#include <functional>
#include <memory>

namespace
{

// Examples sit beside 13pt prose; the editor's default 12pt Hack reads
// oversized next to it, so the example editor runs two points down.
constexpr int kExampleZoom = SonicPiScintilla::kDefaultZoom - 2;

void repolish(QWidget* w)
{
    w->style()->unpolish(w);
    w->style()->polish(w);
}

// Height-for-width through scroll area + nested cards occasionally
// under-reports by a line, clipping the snippet's control row out of its
// card. Pin the code view to its known line count instead — minimum sizes
// propagate reliably where HFW doesn't. (Wrapped lines may exceed this;
// it is a floor, not the layout height.)
void pinCodeViewHeight(QWidget* view, const QString& code)
{
    view->ensurePolished();
    const int lines = code.count('\n') + 1;
    view->setMinimumHeight(qCeil(QFontMetricsF(view->font()).lineSpacing() * lines));
}

// Scope-buffer slot the jukebox taps into (slot 0 is the master scope). Must
// match the scope_num in the with_fx :scope_out wrap on the run side (mainwindow).
constexpr unsigned int kJukeboxScopeSlot = 1;

// Left-aligned flow layout (Qt's classic example, trimmed): items fill the
// available width and wrap, so the dial grid uses the whole playground card
// however wide the pane is.
class TutFlowLayout : public QLayout
{
public:
    TutFlowLayout(int hSpacing, int vSpacing)
        : m_hSpace(hSpacing)
        , m_vSpace(vSpacing)
    {
        setContentsMargins(0, 0, 0, 0);
    }

    ~TutFlowLayout() override
    {
        while (QLayoutItem* item = takeAt(0))
            delete item;
    }

    void addItem(QLayoutItem* item) override { m_items.append(item); }
    int count() const override { return m_items.size(); }
    QLayoutItem* itemAt(int i) const override { return m_items.value(i); }
    QLayoutItem* takeAt(int i) override
    {
        return (i >= 0 && i < m_items.size()) ? m_items.takeAt(i) : nullptr;
    }
    Qt::Orientations expandingDirections() const override { return {}; }
    bool hasHeightForWidth() const override { return true; }
    int heightForWidth(int w) const override { return doLayout(QRect(0, 0, w, 0), true); }
    void setGeometry(const QRect& rect) override
    {
        QLayout::setGeometry(rect);
        doLayout(rect, false);
    }
    QSize sizeHint() const override { return minimumSize(); }
    QSize minimumSize() const override
    {
        QSize size;
        for (QLayoutItem* item : m_items)
            size = size.expandedTo(item->minimumSize());
        const QMargins m = contentsMargins();
        return size + QSize(m.left() + m.right(), m.top() + m.bottom());
    }

private:
    int doLayout(const QRect& rect, bool testOnly) const
    {
        const QMargins m = contentsMargins();
        const QRect eff = rect.adjusted(m.left(), m.top(), -m.right(), -m.bottom());
        int x = eff.x();
        int y = eff.y();
        int lineH = 0;
        for (QLayoutItem* item : m_items)
        {
            const QSize hint = item->sizeHint();
            if (x + hint.width() > eff.right() + 1 && lineH > 0)
            {
                x = eff.x();
                y += lineH + m_vSpace;
                lineH = 0;
            }
            if (!testOnly)
                item->setGeometry(QRect(QPoint(x, y), hint));
            x += hint.width() + m_hSpace;
            lineH = qMax(lineH, hint.height());
        }
        return y + lineH - rect.y() + m.bottom();
    }

    QList<QLayoutItem*> m_items;
    int m_hSpace, m_vSpace;
};

} // namespace

TutorialPane::TutorialPane(SonicPiLexer* lexer, SonicPiTheme* theme, QWidget* parent)
    : QFrame(parent)
    , m_lexer(lexer)
    , m_theme(theme)
{
    registerTutorialWidgetAccessibility();
    setObjectName("tutorialPane");

    m_selGroup = std::make_shared<TutSelectionGroup>();

    QVBoxLayout* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(0);

    // Docs text-size controls (A- / A+): the shared ZoomBar, displayed in the
    // help dock's title row beside the HELP title (MainWindow places the
    // widget), like the Cards/Logs/Debug tabs' bars.
    m_zoomBar = new ZoomBar(m_theme, tr("documentation"), this);
    connect(m_zoomBar, &ZoomBar::zoomStep, this, [this](int delta) {
        m_userZoom = qBound(-4, m_userZoom + delta, 8);
        applySizing();
    });

    m_scroll = new QScrollArea(this);
    m_scroll->setWidgetResizable(true);
    m_scroll->setFrameShape(QFrame::NoFrame);
    m_scroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_selGroup->setScrollArea(m_scroll);
    // Clicking into the page focuses it, so Space/QWERTY reach keyPressEvent
    m_scroll->setFocusPolicy(Qt::ClickFocus);

    m_content = new QWidget(m_scroll);
    m_content->setObjectName("tutorialContent");
    m_content->setFocusPolicy(Qt::ClickFocus);

    // The column spans the pane; the readable-measure cap is applied to the
    // prose blocks themselves (addProse), so interactive cards like the synth
    // playground can use the full width.
    QWidget* columnWidget = new QWidget(m_content);
    m_column = new QVBoxLayout(columnWidget);
    int inset = ScaleWidthForDPI(22);
    m_column->setContentsMargins(inset, ScaleHeightForDPI(18), inset, ScaleHeightForDPI(26));
    m_column->setSpacing(ScaleHeightForDPI(12));

    QHBoxLayout* contentRow = new QHBoxLayout(m_content);
    contentRow->setContentsMargins(0, 0, 0, 0);
    // Column takes all width up to its cap; the spacer only absorbs the rest
    contentRow->addWidget(columnWidget, 1);
    contentRow->addStretch(0);

    m_scroll->setWidget(m_content);
    outer->addWidget(m_scroll, 1);

    // Examples full-file page: shell built now, the editor lazily on first use
    m_examplePage = new QWidget(this);
    QVBoxLayout* examplePage = new QVBoxLayout(m_examplePage);
    int exInset = ScaleWidthForDPI(22);
    examplePage->setContentsMargins(exInset, ScaleHeightForDPI(12), exInset, ScaleHeightForDPI(12));
    examplePage->setSpacing(ScaleHeightForDPI(10));
    m_exampleTitle = new QLabel(m_examplePage);
    m_exampleTitle->setObjectName("tutH1");
    m_exampleTitle->setTextInteractionFlags(Qt::TextSelectableByMouse);
    examplePage->addWidget(m_exampleTitle);
    m_exampleFrame = new QFrame(m_examplePage);
    m_exampleFrame->setObjectName("tutCodeFrame");
    m_exampleFrame->setProperty("playing", false);
    m_exampleFrameLayout = new QVBoxLayout(m_exampleFrame);
    int exPad = ScaleWidthForDPI(10);
    m_exampleFrameLayout->setContentsMargins(exPad, exPad, exPad, exPad);
    m_exampleFrameLayout->setSpacing(ScaleHeightForDPI(4));
    QHBoxLayout* exampleControls = new QHBoxLayout();
    exampleControls->setContentsMargins(0, 0, 0, 0);
    exampleControls->setSpacing(ScaleWidthForDPI(8));
    // Jukebox transport (sits above the code): one prominent toggle button that
    // plays, then flips to stop while the example runs (only one example ever
    // plays at a time), a Load button, and a live scope that appears while it
    // is playing so it's obvious the sound is coming from here.
    m_examplePlay = new QPushButton(tr("Play"), m_exampleFrame);
    m_examplePlay->setObjectName("exPlay");
    m_examplePlay->setToolTip(tr("Run this example"));
    m_examplePlay->setAccessibleName(tr("Run example"));
    m_exampleLoad = new QPushButton(tr("Load"), m_exampleFrame);
    m_exampleLoad->setObjectName("exLoad");
    m_exampleLoad->setToolTip(tr("Load this example into the current buffer"));
    m_exampleLoad->setAccessibleName(tr("Load example into buffer"));
    int exButtonHeight = ScaleHeightForDPI(30);
    int exScopeHeight = ScaleHeightForDPI(46);
    m_examplePlay->setIconSize(ScaleForDPI(13, 13));
    // Fixed-size buttons stay packed left (they don't stretch to fill)
    for (QPushButton* b : { m_examplePlay, m_exampleLoad })
    {
        b->setMinimumHeight(exButtonHeight);
        b->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        b->setCursor(Qt::PointingHandCursor);
    }
    // Scope fills the width right of the buttons; the stretch spacer carries a
    // much smaller factor so it only takes over when the scope is hidden (and
    // then keeps the Fixed buttons from spreading out). Taller than the buttons
    // so the trace has room; they centre in the row.
    m_exampleScope = new TutScope(m_exampleFrame);
    m_exampleScope->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
    m_exampleScope->setMinimumWidth(ScaleWidthForDPI(160));
    m_exampleScope->setFixedHeight(exScopeHeight);
    m_exampleScope->hide(); // shown only while an example is playing
    exampleControls->addWidget(m_examplePlay);
    exampleControls->addWidget(m_exampleLoad);
    exampleControls->addStretch(1);
    exampleControls->addWidget(m_exampleScope, 20);
    // Reserve the scope's height permanently (zero width) so the buttons don't
    // shift when the taller scope appears/disappears
    exampleControls->addSpacerItem(
        new QSpacerItem(0, exScopeHeight, QSizePolicy::Fixed, QSizePolicy::Fixed));
    m_exampleFrameLayout->addLayout(exampleControls);
    examplePage->addWidget(m_exampleFrame, 1);
    m_examplePage->hide();
    outer->addWidget(m_examplePage, 1);

    // The example page always occupies m_snippets[0] while visible
    connect(m_examplePlay, &QPushButton::clicked, this, [this]() {
        if (m_snippets.isEmpty() || m_snippets[0].play != m_examplePlay)
            return;
        Snippet& s = m_snippets[0];
        if (s.jobId >= 0)
            emit stopJobRequested(s.jobId);
        else
            emit runRequested(s.code, s.workspace, false, true); // scope-tapped
    });
    connect(m_exampleLoad, &QPushButton::clicked, this, [this]() {
        if (m_snippets.isEmpty() || m_snippets[0].play != m_examplePlay)
            return;
        emit loadRequested(m_snippets[0].code);
    });

    applyTheme();
}

void TutorialPane::setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api)
{
    m_spAPI = api;
}

void TutorialPane::ensureExampleEditor()
{
    if (m_exampleEditor)
        return;
    m_exampleEditor = new SonicPiScintilla(m_lexer, m_theme, "sonic-pi-example-display", false);
    m_exampleEditor->setReadOnly(true);
    m_exampleEditor->setCaretLineVisible(false);
    m_exampleEditor->setCaretWidth(0);
    m_exampleEditor->showAutoCompletion(false);
    m_exampleEditor->zoomTo(kExampleZoom + m_userZoom);
    // Below the transport controls (which were added to the frame first)
    m_exampleFrameLayout->addWidget(m_exampleEditor, 1);
}

void TutorialPane::loadChapter(const SonicPi::TutorialChapter& chapter, const QString& imagesRoot,
                               const QString& prevTitle, const QString& nextTitle)
{
    m_chapter = chapter;
    m_imagesRoot = imagesRoot;
    m_prevTitle = prevTitle;
    m_nextTitle = nextTitle;
    rebuild();
    emit announceRequested(chapter.title);
}

void TutorialPane::showCodePage(const QString& title, const QString& code)
{
    clearContent();
    m_chapter = SonicPi::TutorialChapter();
    m_prevTitle.clear();
    m_nextTitle.clear();

    ensureExampleEditor();
    m_exampleTitle->setText(title);
    m_exampleEditor->setReadOnly(false);
    m_exampleEditor->setText(code);
    m_exampleEditor->setReadOnly(true);

    Snippet snippet;
    snippet.frame = m_exampleFrame;
    snippet.play = m_examplePlay;
    snippet.code = code;
    snippet.workspace = QString("sonic-pi-tutorial-%1").arg(++m_workspaceSeq);
    m_snippets.append(snippet);
    setSnippetPlaying(m_snippets[0], false);

    m_scroll->hide();
    m_examplePage->show();
    emit announceRequested(title);
}

bool TutorialPane::playFirstSnippet()
{
    if (m_snippets.isEmpty())
        return false;
    m_snippets[0].play->click();
    return true;
}

void TutorialPane::beginPage()
{
    clearContent();
    m_chapter = SonicPi::TutorialChapter();
    m_prevTitle.clear();
    m_nextTitle.clear();
}

void TutorialPane::endPage(const QString& announceTitle)
{
    m_column->addStretch(1);
    m_scroll->verticalScrollBar()->setValue(0);
    emit announceRequested(announceTitle);
}

void TutorialPane::showSampleGroupPage(const SonicPi::SampleGroup& group)
{
    beginPage();
    addHeading(1, group.title);
    for (const QString& sample : group.samples)
        addSnippet("sample :" + sample);
    endPage(group.title);
}

void TutorialPane::showLangPage(const SonicPi::LangPage& page)
{
    beginPage();
    addHeading(1, page.key);
    if (!page.summary.isEmpty())
    {
        QLabel* summary = new QLabel(page.summary, m_content);
        summary->setObjectName("tutSig");
        summary->setTextInteractionFlags(Qt::TextSelectableByMouse);
        m_column->addWidget(summary);
    }
    if (!page.usage.isEmpty())
        addSnippet(page.usage, false);
    if (!page.docHtml.isEmpty())
        addProse(page.docHtml);
    if (!page.examples.isEmpty())
    {
        addHeading(2, tr("Examples"));
        // Reference illustrations, not demos: never runnable (some — assert,
        // stop, defonce — would raise or wedge if played standalone). Comments
        // sit in their own right-hand column, like the old doc system.
        int exampleNum = 0;
        for (const SonicPi::CodeExample& example : page.examples)
        {
            QLabel* caption = new QLabel(tr("Example %1").arg(++exampleNum), m_content);
            caption->setObjectName("tutHint");
            m_column->addWidget(caption);
            addSnippet(example.code, false);
            Snippet& snippet = m_snippets.last();
            snippet.commentsAside = true;
            snippet.codeView->setHtml(exampleTableHtml(example.code));
        }
    }
    if (!page.introduced.isEmpty())
        addProse("<i>" + tr("Introduced in %1").arg(page.introduced).toHtmlEscaped() + "</i>");
    endPage(page.key);
}

void TutorialPane::showInstrumentPage(bool isFx, const SonicPi::InstrumentPage& page)
{
    beginPage();
    m_pageIsFx = isFx;
    m_pageName = page.key;

    // Titles are generated from symbol keys (dark_ambience →
    // "Dark_ambience"); show capitalised words. The signature line below
    // still carries the real :symbol.
    QString title = page.title.isEmpty() ? page.key : page.title;
    title.replace('_', ' ');
    QStringList titleWords = title.split(' ', Qt::SkipEmptyParts);
    for (QString& word : titleWords)
        word[0] = word[0].toUpper();
    title = titleWords.join(' ');
    // One "playground" card holds the whole interactive demo (name, dials,
    // piano and the generated snippet) so the controls and the code they
    // regenerate read as a single instrument.
    QFrame* playground = new QFrame(m_content);
    playground->setObjectName("tutPlayground");
    // sizeHint is a floor: the card must never be squeezed below its content
    // (a squeeze pushed the snippet controls out under the card border).
    playground->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    QVBoxLayout* playLayout = new QVBoxLayout(playground);
    int playPad = ScaleWidthForDPI(12);
    playLayout->setContentsMargins(playPad, ScaleHeightForDPI(10), playPad, ScaleHeightForDPI(10));
    playLayout->setSpacing(ScaleHeightForDPI(8));

    // Faceplate header inside the card: the instrument's glyph and name as
    // one left-aligned badge, like a hardware synth's silkscreened logo.
    QWidget* headerRow = new QWidget(playground);
    QHBoxLayout* header = new QHBoxLayout(headerRow);
    header->setContentsMargins(0, 0, 0, 0);
    header->setSpacing(ScaleWidthForDPI(10));
    QLabel* icon = new QLabel(headerRow);
    icon->setObjectName("tutFxIcon");
    icon->setProperty("fxname", page.key);
    icon->setProperty("isfx", isFx);
    icon->setAccessibleName(tr("%1 icon").arg(title));
    renderFxIcon(icon);
    QLabel* titleLabel = new QLabel(title, headerRow);
    titleLabel->setObjectName("tutPlateName");
    titleLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    header->addWidget(icon, 0, Qt::AlignVCenter);
    header->addWidget(titleLabel, 0, Qt::AlignVCenter);
    header->addStretch(1);
    playLayout->addWidget(headerRow);

    // Reset joins Copy in the code area's corner: a flat restore glyph in the
    // transport family.
    QPushButton* reset = new QPushButton(playground);
    reset->setObjectName("tutReset");
    reset->setToolTip(tr("Reset all controls to their defaults"));
    reset->setAccessibleName(tr("Reset %1 controls").arg(title));
    reset->setIconSize(ScaleForDPI(20, 20));
    reset->setFixedSize(ScaleForDPI(28, 28));
    reset->setCursor(Qt::PointingHandCursor);
    connect(reset, &QPushButton::clicked, this, [this]() {
        for (TutDial* dial : m_dials)
            dial->reset(false);
        regenerateInstrumentCode();
    });

    // Dials: the instrument's specific ranged opts first, then mix/amp for FX.
    // Slide opts are skipped — the preview snippet is a one-shot trigger, so a
    // slide dial would be a dead control.
    const QStringList commonFx = { "amp", "mix", "pre_mix", "pre_amp" };
    QVector<SonicPi::InstrumentOpt> dialOpts;
    for (const SonicPi::InstrumentOpt& opt : page.opts)
        if (opt.numeric && opt.hasRange && !opt.name.endsWith("_slide")
            && !(isFx && commonFx.contains(opt.name)))
            dialOpts.append(opt);
    if (isFx)
        for (const QString& name : { QString("mix"), QString("amp") })
            for (const SonicPi::InstrumentOpt& opt : page.opts)
                if (opt.name == name && opt.numeric && opt.hasRange)
                    dialOpts.append(opt);

    QWidget* dialsRow = new QWidget(playground);
    dialsRow->setObjectName("tutDials");
    TutFlowLayout* dialFlow = new TutFlowLayout(ScaleWidthForDPI(4), ScaleHeightForDPI(6));
    dialsRow->setLayout(dialFlow);

    QColor dialFg = m_theme->color("Foreground");
    QColor dialBg = m_theme->color("Background");
    QColor dialAccent = m_theme->color("HighlightedBackground");
    QColor dialMuted = SonicPiTheme::blend(dialFg, dialBg, 0.38);
    // Strong enough to read as a ring even at rest (0.18 washed out).
    QColor dialTrack = SonicPiTheme::blend(dialBg, dialFg, 0.28);
    reset->setIcon(TablerIcons::icon(TablerIcons::Glyph::Restore, dialMuted,
                                     ScaleWidthForDPI(20), devicePixelRatioF()));

    // Hardware-synth layout: dials cluster into labelled sections
    // (PITCH / ENVELOPE / FILTER / MOD / CHARACTER / OUT), each its own
    // quiet panel; panels flow across the card width.
    auto groupFor = [](const QString& n) -> int {
        if (n == "note" || n.startsWith("detune") || n.startsWith("freq") || n == "divisor")
            return 0; // PITCH
        if (n.startsWith("attack") || n.startsWith("decay") || n.startsWith("sustain")
            || n.startsWith("release") || n == "env_curve")
            return 1; // ENVELOPE
        if (n.contains("cutoff") || n == "res")
            return 2; // FILTER
        if (n.startsWith("vibrato") || n.contains("phase") || n.contains("feedback")
            || n.contains("mod") || n == "depth" || n == "rate")
            return 3; // MOD
        if (n == "amp" || n == "pan" || n == "mix" || n.startsWith("pre_"))
            return 5; // OUT
        return 4;     // CHARACTER — the instrument's own flavour opts
    };
    static const char* kGroupNames[] = { "PITCH", "ENVELOPE", "FILTER", "MOD", "CHARACTER", "OUT" };
    QVector<QVector<SonicPi::InstrumentOpt>> groups(6);
    int dialCount = 0;
    const int kMaxDials = 18;
    for (const SonicPi::InstrumentOpt& opt : dialOpts)
    {
        if (dialCount >= kMaxDials)
            break;
        groups[groupFor(opt.name)].append(opt);
        dialCount++;
    }

    auto onChange = [this]() { regenerateInstrumentCode(); };
    for (int g = 0; g < groups.size(); g++)
    {
        if (groups[g].isEmpty())
            continue;
        QFrame* panel = new QFrame(dialsRow);
        panel->setObjectName("tutDialGroup");
        QVBoxLayout* panelLayout = new QVBoxLayout(panel);
        panelLayout->setContentsMargins(ScaleWidthForDPI(6), ScaleHeightForDPI(2),
                                        ScaleWidthForDPI(6), ScaleHeightForDPI(4));
        panelLayout->setSpacing(ScaleHeightForDPI(2));
        QLabel* caption = new QLabel(QLatin1String(kGroupNames[g]), panel);
        caption->setObjectName("tutSection");
        caption->setAlignment(Qt::AlignHCenter);
        panelLayout->addWidget(caption);
        // Big groups chunk into rows: a single unwrappable row of fixed-width
        // dials would exceed narrow panes and clip unreachably (the scroll
        // area has no horizontal scrollbar). Rows are balanced (6 → 3+3,
        // 7 → 4+3) so no row ends up with a lone straggler.
        const int groupSize = groups[g].size();
        const int groupRows = (groupSize + 4) / 5;   // ≤5 dials per row
        const int perRow = (groupSize + groupRows - 1) / groupRows;
        QVector<QHBoxLayout*> dialRows;
        for (int d = 0; d < groupSize; d++)
        {
            if (d % perRow == 0)
            {
                QHBoxLayout* row = new QHBoxLayout();
                row->setContentsMargins(0, 0, 0, 0);
                row->setSpacing(ScaleWidthForDPI(2));
                panelLayout->addLayout(row);
                dialRows.append(row);
            }
            const SonicPi::InstrumentOpt& opt = groups[g][d];
            TutDial* dial = new TutDial(opt.name, opt.min, opt.max, opt.defaultNum, onChange,
                                        panel, opt.minExcl, opt.maxExcl);
            dial->setColours(dialFg, dialMuted, dialAccent, dialTrack);
            dial->setDocText(opt.doc);   // hover popup: the opt's reference doc
            m_dials.append(dial);
            // Top-aligned so every arc in a row sits on the same line,
            // whatever the label line count below it.
            dialRows.last()->addWidget(dial, 0, Qt::AlignTop);
        }
        for (QHBoxLayout* row : dialRows)
            row->addStretch(1);   // short trailing rows stay left-aligned
        dialFlow->addWidget(panel);
    }
    // The QWERTY keys offset from the page's default note: the FX demo's
    // played note, or the synth's note dial. An FX page's own note dial
    // (the autotuner's tuning target) is not the played note.
    m_pianoBaseNote = isFx ? m_demoNote : 52;
    if (!isFx)
        for (TutDial* dial : m_dials)
            if (dial->optName() == "note")
                m_pianoBaseNote = qRound(dial->value());

    if (m_dials.isEmpty())
    {
        dialsRow->deleteLater();
        reset->hide();   // nothing to reset without dials
    }
    else
    {
        playLayout->addWidget(dialsRow);
    }

    // Generated snippet next (a recessed text area), then the trigger row at
    // the very bottom: transport and keyboard side by side — both are ways
    // to make sound, so they share one surface. addSnippet places the code
    // frame in playLayout and the play/stop/copy buttons into this row.
    QWidget* pianoRow = new QWidget(playground);
    QHBoxLayout* pianoLayout = new QHBoxLayout(pianoRow);
    pianoLayout->setContentsMargins(0, 0, 0, 0);
    pianoLayout->setSpacing(ScaleWidthForDPI(6));

    addSnippet(QString(), true, playLayout, pianoLayout);
    // Reset joins Copy in the snippet's corner cluster.
    Snippet& demo = m_snippets.last();
    if (demo.codeArea && demo.copy)
    {
        QWidget* corner = new QWidget(demo.frame);
        QHBoxLayout* cornerRow = new QHBoxLayout(corner);
        cornerRow->setContentsMargins(0, 0, 0, 0);
        cornerRow->setSpacing(ScaleWidthForDPI(2));
        demo.codeArea->removeWidget(demo.copy);
        cornerRow->addWidget(reset);
        cornerRow->addWidget(demo.copy);
        demo.codeArea->addWidget(corner, 0, 0, Qt::AlignTop | Qt::AlignRight);
    }
    pianoLayout->addSpacing(ScaleWidthForDPI(12));
    const int octIconPx = ScaleWidthForDPI(26);
    const qreal octDpr = devicePixelRatioF();
    QPushButton* octDown = new QPushButton(pianoRow);
    octDown->setObjectName("tutOct");
    octDown->setProperty("octDir", -1);
    octDown->setIconSize(ScaleForDPI(26, 26));
    octDown->setIcon(TablerIcons::icon(TablerIcons::Glyph::CircleMinus, dialMuted, octIconPx, octDpr));
    octDown->setToolTip(tr("Octave down (z)"));
    octDown->setAccessibleName(tr("Octave down"));
    octDown->setCursor(Qt::PointingHandCursor);
    connect(octDown, &QPushButton::clicked, this, [this]() { shiftOctave(-1); });
    QPushButton* octUp = new QPushButton(pianoRow);
    octUp->setObjectName("tutOct");
    octUp->setProperty("octDir", 1);
    octUp->setIconSize(ScaleForDPI(26, 26));
    octUp->setIcon(TablerIcons::icon(TablerIcons::Glyph::CirclePlus, dialMuted, octIconPx, octDpr));
    octUp->setToolTip(tr("Octave up (x)"));
    octUp->setAccessibleName(tr("Octave up"));
    octUp->setCursor(Qt::PointingHandCursor);
    connect(octUp, &QPushButton::clicked, this, [this]() { shiftOctave(1); });
    m_piano = new TutPiano([this](int offset) { playKeyboardNote(offset); }, pianoRow);
    m_piano->setColours(dialFg, dialBg, dialAccent, dialMuted);
    m_octaveLabel = new QLabel(pianoRow);
    m_octaveLabel->setObjectName("tutHint");
    pianoLayout->addWidget(octDown, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_piano, 1);   // keyboard grows into spare row width
    pianoLayout->addWidget(octUp, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_octaveLabel, 0, Qt::AlignVCenter);
    pianoLayout->addStretch(1);
    playLayout->addWidget(pianoRow);
    shiftOctave(0);

    m_column->addWidget(playground);
    regenerateInstrumentCode();

    // Quick index of every opt with its default — a compact table whose
    // names link down into the reference rows.
    if (!page.opts.isEmpty())
    {
        QWidget* index = new QWidget(m_content);
        index->setObjectName("tutOptIndex");
        index->setAttribute(Qt::WA_StyledBackground, true);
        index->setMaximumWidth(ScaleWidthForDPI(1200));
        QGridLayout* grid = new QGridLayout(index);
        grid->setContentsMargins(ScaleWidthForDPI(10), ScaleHeightForDPI(6),
                                 ScaleWidthForDPI(10), ScaleHeightForDPI(6));
        grid->setHorizontalSpacing(ScaleWidthForDPI(22));
        grid->setVerticalSpacing(ScaleHeightForDPI(2));
        const int kIndexCols = 4;
        int cellIndex = 0;
        for (const SonicPi::InstrumentOpt& opt : page.opts)
        {
            QWidget* cell = new QWidget(index);
            QHBoxLayout* cellRow = new QHBoxLayout(cell);
            cellRow->setContentsMargins(0, 0, 0, 0);
            cellRow->setSpacing(ScaleWidthForDPI(5));
            QPushButton* link = new QPushButton(opt.name + ":", cell);
            link->setObjectName("tutOptLink");
            link->setCursor(Qt::PointingHandCursor);
            link->setAccessibleName(tr("Jump to documentation for %1").arg(opt.name));
            const QString optName = opt.name;
            connect(link, &QPushButton::clicked, this, [this, optName]() {
                QWidget* row = m_optRows.value(optName);
                if (!row)
                    return;
                m_scroll->verticalScrollBar()->setValue(
                    qMax(0, row->mapTo(m_content, QPoint(0, 0)).y() - ScaleHeightForDPI(8)));
            });
            QLabel* def = new QLabel(opt.defaultText, cell);
            def->setObjectName("tutOptDefault");
            cellRow->addWidget(link);
            cellRow->addWidget(def);
            cellRow->addStretch(1);
            grid->addWidget(cell, cellIndex / kIndexCols, cellIndex % kIndexCols);
            cellIndex++;
        }
        grid->setColumnStretch(kIndexCols, 1);   // keep columns packed left
        m_column->addWidget(index);
    }

    if (!page.docHtml.isEmpty())
        addProse(page.docHtml);
    if (!page.opts.isEmpty())
    {
        addHeading(2, tr("Options"));
        addOptsGrid(page.opts);
    }

    endPage(title);
}

QString TutorialPane::instrumentOpts() const
{
    QString opts;
    for (TutDial* dial : m_dials)
    {
        // A synth page's note renders as the `play` argument, not an opt;
        // an FX page's note dial is a real opt (the autotuner's target).
        if (!m_pageIsFx && dial->optName() == "note")
            continue;
        if (!dial->isDefault())
            opts += ", " + dial->optName() + ": " + dial->valueText();
    }
    return opts;
}

void TutorialPane::keyPressEvent(QKeyEvent* event)
{
    if (!m_pageName.isEmpty() && !event->isAutoRepeat())
    {
        if (event->key() == Qt::Key_Space)
        {
            toggleDemo();
            event->accept();
            return;
        }
        if (event->key() == Qt::Key_Z || event->key() == Qt::Key_X)
        {
            shiftOctave(event->key() == Qt::Key_Z ? -1 : 1);
            event->accept();
            return;
        }
        static const QList<Qt::Key> piano = {
            Qt::Key_A, Qt::Key_W, Qt::Key_S, Qt::Key_E, Qt::Key_D, Qt::Key_F,
            Qt::Key_T, Qt::Key_G, Qt::Key_Y, Qt::Key_H, Qt::Key_U, Qt::Key_J,
            Qt::Key_K, Qt::Key_O, Qt::Key_L, Qt::Key_P
        };
        int offset = piano.indexOf((Qt::Key)event->key());
        if (offset >= 0)
        {
            playKeyboardNote(offset);
            event->accept();
            return;
        }
    }
    QFrame::keyPressEvent(event);
}

void TutorialPane::shiftOctave(int delta)
{
    m_octave = qBound(-3, m_octave + delta, 3);
    if (m_octaveLabel)
        m_octaveLabel->setText(m_octave == 0
                                   ? tr("z/x: octave")
                                   : tr("octave %1%2").arg(m_octave > 0 ? "+" : "").arg(m_octave));
}

void TutorialPane::toggleDemo()
{
    if (m_snippets.isEmpty())
        return;
    Snippet& snippet = m_snippets[0];
    if (snippet.jobId >= 0)
        emit stopJobRequested(snippet.jobId);
    else
        snippet.play->click();
}

void TutorialPane::playKeyboardNote(int semitoneOffset)
{
    if (m_pageName.isEmpty())
        return;
    // semitoneOffset is the key's position on the keyboard (QWERTY or click);
    // the octave shift transposes the whole keyboard under the fixed labels,
    // and offsets against the page default keep repeated presses identical.
    int played = qBound(0, m_pianoBaseNote + m_octave * 12 + semitoneOffset, 130);
    QString note = QString::number(played);
    // use_real_time bypasses the default sched-ahead so keys sound instantly
    QString code = "use_real_time\nuse_debug false\n";
    double release = 1.0;
    if (m_pageIsFx)
    {
        release = 2.0; // the demo line's release: 2
        code += "with_fx :" + m_pageName + instrumentOpts() + " do\n"
                "  synth :prophet, note: " + note + ", release: 2, cutoff: 80\n"
                "end";
    }
    else
    {
        for (TutDial* dial : m_dials)
            if (dial->optName() == "release")
                release = dial->value();
        code += "use_synth :" + m_pageName + "\n"
                "play " + note + instrumentOpts();
    }
    // Untracked workspace: key notes shouldn't drive the demo's playing state
    emit runRequested(code, "sonic-pi-tutorial-keys", true);
    if (m_piano)
        m_piano->flash(semitoneOffset, qRound(release * 1000));
    // Dial/code sync happens AFTER the note is dispatched: setValue triggers
    // a synchronous snippet regeneration (font metrics + document parse),
    // which must not sit in front of the sound on the use_real_time path.
    // instrumentOpts() skips the note dial, so the emitted code is identical.
    if (m_pageIsFx)
    {
        if (m_demoNote != played)
        {
            m_demoNote = played;
            regenerateInstrumentCode();
        }
    }
    else
        for (TutDial* dial : m_dials)
            if (dial->optName() == "note")
                dial->setValue(played);
}

void TutorialPane::regenerateInstrumentCode()
{
    if (m_pageName.isEmpty() || m_snippets.isEmpty())
        return;

    struct OptTok
    {
        QString name;
        QString value;
    };
    QVector<OptTok> opts;
    QString note = "52";
    for (TutDial* dial : m_dials)
    {
        // A synth page's note renders as the `play` argument, not an opt;
        // an FX page's note dial is a real opt (the autotuner's target).
        if (!m_pageIsFx && dial->optName() == "note")
        {
            note = dial->valueText();
            continue;
        }
        if (!dial->isDefault())
            opts.append({ dial->optName(), dial->valueText() });
    }

    auto span = [](const QString& colour, const QString& text) {
        return QStringLiteral("<span style=\"color:%1\">%2</span>")
            .arg(colour, text.toHtmlEscaped());
    };
    // Editable number: an anchor the code view turns into an inline editor
    // wired to the matching dial (see openOptEditor). No resting underline —
    // the code view underlines the hovered anchor (applyAnchorHover).
    auto editable = [this](const QString& opt, const QString& value) {
        return QStringLiteral(
                   "<a href=\"opt:%1\" style=\"color:%2;text-decoration:none;\">%3</a>")
            .arg(opt, m_codeColours.number, value);
    };

    // Runnable text and anchored HTML are built in lockstep, wrapping opts
    // the way a person would write them — trailing-comma continuation lines
    // with a small indent — rather than soft-wrapping mid-expression. The
    // wrap column follows the code view's actual width (falling back to 80
    // before the first layout pass).
    int wrapCol = 80;
    if (m_snippets[0].codeView && m_snippets[0].codeView->width() > 50)
    {
        QFont mono(QStringLiteral("Hack"));
        mono.setPointSizeF(qMax(6.0, 9.0 * m_fontScale));   // the @codeSmall size
        wrapCol = qMax(40, (int)(m_snippets[0].codeView->width()
                                 / QFontMetricsF(mono).horizontalAdvance(QLatin1Char('m')))
                               - 2);
    }

    QString text, html;
    int lineLen = 0;
    auto emitOpts = [&](const QString& indent) {
        const QString htmlIndent = QStringLiteral("&nbsp;").repeated(indent.length());
        for (const OptTok& opt : opts)
        {
            text += ",";
            html += ",";
            lineLen += 1;
            const int partLen = opt.name.length() + opt.value.length() + 2;
            if (lineLen + 1 + partLen > wrapCol)
            {
                text += "\n" + indent;
                html += "<br>" + htmlIndent;
                lineLen = indent.length();
            }
            else
            {
                text += " ";
                html += " ";
                lineLen += 1;
            }
            text += opt.name + ": " + opt.value;
            html += span(m_codeColours.symbol, opt.name + ":") + " "
                    + editable(opt.name, opt.value);
            lineLen += partLen;
        }
    };

    // Function names (with_fx / use_synth / play) stay the plain text colour,
    // matching the generic highlighter — only true keywords get colour.
    if (m_pageIsFx)
    {
        text = "with_fx :" + m_pageName;
        html = "with_fx " + span(m_codeColours.symbol, ":" + m_pageName);
        lineLen = text.length();
        emitOpts("        ");
        // The demo note tracks the piano (see keyPlayNote), like the synth
        // pages' note dial.
        const QString body = QString("  synth :prophet, note: %1, release: 2, cutoff: 80\n"
                                     "end")
                                 .arg(m_demoNote);
        text += " do\n" + body;
        html += " " + span(m_codeColours.keyword, "do") + "<br>"
                + SonicPi::TutorialDocs::highlightCode(body, m_codeColours);
    }
    else
    {
        text = "use_synth :" + m_pageName + "\nplay " + note;
        html = "use_synth " + span(m_codeColours.symbol, ":" + m_pageName) + "<br>"
               + "play " + editable("note", note);
        lineLen = 5 + note.length();
        emitOpts("     ");
    }
    // Airier leading: the smaller playground font keeps its line rhythm.
    html = "<p style=\"line-height:130%; margin:0;\">" + html + "</p>";
    setSnippetCode(0, text, html);
}

QString TutorialPane::exampleTableHtml(const QString& code) const
{
    // Index of a line's trailing comment, skipping # inside string literals
    // (covers "#{...}" interpolation too); -1 when the line has none.
    auto commentStart = [](const QString& line) {
        QChar quote;
        bool inString = false;
        for (int i = 0; i < line.size(); i++)
        {
            const QChar c = line[i];
            if (inString)
            {
                if (c == '\\')   // escaped char (\" etc.) can't close the string
                    i++;
                else if (c == quote)
                    inString = false;
                continue;
            }
            if (c == '"' || c == '\'')
            {
                inString = true;
                quote = c;
                continue;
            }
            if (c == '#')
                return i;
        }
        return -1;
    };

    QString html = QStringLiteral("<table cellspacing=\"0\" cellpadding=\"0\" width=\"100%\">");
    const QStringList lines = code.split('\n');
    for (const QString& line : lines)
    {
        const int split = commentStart(line);
        const QString codePart = split < 0 ? line : line.left(split);
        // Only TRAILING comments move to the right column. Full-line comments
        // are narrative — they stay in the code column, indentation intact.
        const bool fullLineComment = split >= 0 && codePart.trimmed().isEmpty();
        QString codeHtml, commentHtml;
        if (fullLineComment)
        {
            codeHtml = SonicPi::TutorialDocs::highlightCode(line, m_codeColours);
        }
        else
        {
            // Whitespace-only cells keep the row's height (blank lines).
            codeHtml = codePart.trimmed().isEmpty()
                           ? QStringLiteral("&nbsp;")
                           : SonicPi::TutorialDocs::highlightCode(codePart, m_codeColours);
            if (split >= 0)
                commentHtml = QStringLiteral("<i><span style=\"color:%1\">%2</span></i>")
                                  .arg(m_codeColours.comment, line.mid(split).toHtmlEscaped());
        }
        // Middle cell is a fixed gutter: wrapped comment lines stay flush
        // with the comment's own left edge (an &nbsp; prefix hung outdented).
        html += "<tr><td>" + codeHtml + "</td><td width=\"18\"></td><td>"
                + commentHtml + "</td></tr>";
    }
    html += QStringLiteral("</table>");
    return html;
}

void TutorialPane::setSnippetCode(int index, const QString& code, const QString& html)
{
    if (index >= m_snippets.size())
        return;
    Snippet& snippet = m_snippets[index];
    snippet.code = code;
    snippet.codeView->setHtml(
        html.isEmpty() ? SonicPi::TutorialDocs::highlightCode(code, m_codeColours) : html);
    pinCodeViewHeight(snippet.codeView, code);
}

void TutorialPane::openOptEditor(const QString& optName, const QPoint& globalPos)
{
    TutDial* target = nullptr;
    for (TutDial* dial : m_dials)
        if (dial->optName() == optName)
            target = dial;
    if (!target)
        return;
    QLineEdit* editor = new QLineEdit(target->valueText(), this);
    editor->setObjectName("tutOptEditor");
    editor->setAccessibleName(tr("Edit %1").arg(optName));
    editor->setAlignment(Qt::AlignCenter);
    editor->setFixedSize(ScaleWidthForDPI(70), ScaleHeightForDPI(26));
    const QPoint local = mapFromGlobal(globalPos);
    editor->move(qBound(0, local.x() - ScaleWidthForDPI(24), qMax(0, width() - editor->width())),
                 qBound(0, local.y() - ScaleHeightForDPI(30), qMax(0, height() - editor->height())));
    editor->installEventFilter(this);   // Escape cancels (see eventFilter)
    QPointer<TutDial> dial(target);
    connect(editor, &QLineEdit::editingFinished, editor, [editor, dial]() {
        if (!editor->property("cancelled").toBool() && dial)
        {
            double v = 0;
            if (parseOptValue(editor->text(), &v))
                dial->setValue(v);   // regenerates the snippet via onChange
        }
        editor->deleteLater();
    });
    editor->show();
    editor->raise();
    editor->setFocus();
    editor->selectAll();
}

void TutorialPane::rebuild()
{
    clearContent();

    for (const SonicPi::TutorialBlock& block : m_chapter.blocks)
    {
        switch (block.type)
        {
        case SonicPi::TutorialBlock::Heading:
            addHeading(block.level, block.text);
            break;
        case SonicPi::TutorialBlock::Prose:
            addProse(block.text);
            break;
        case SonicPi::TutorialBlock::Code:
            addSnippet(block.source, block.runnable);
            break;
        case SonicPi::TutorialBlock::List:
            addList(block);
            break;
        case SonicPi::TutorialBlock::Image:
            addImage(block);
            break;
        }
    }

    addNavFooter();
    m_column->addStretch(1);
    m_scroll->verticalScrollBar()->setValue(0);
}

void TutorialPane::clearContent()
{
    // Jukebox: an example only plays while its page is showing — stop it on the
    // way out so at most one example is ever running
    if (!m_snippets.isEmpty() && m_snippets[0].play == m_examplePlay && m_snippets[0].jobId >= 0)
        emit stopJobRequested(m_snippets[0].jobId);
    // The stop above is async; hide the scope now so it doesn't linger on the
    // next page (runEnded won't find the snippet once m_snippets is cleared)
    if (m_exampleScope)
        m_exampleScope->stop();
    m_snippets.clear();
    m_dials.clear();
    // Labels deregister themselves on destruction, but that happens via
    // deleteLater — drop any live selection now so the group never points
    // at widgets awaiting deletion
    m_selGroup->clearAll();
    m_proseLabels.clear();
    m_piano = nullptr;
    m_octaveLabel = nullptr;
    m_optRows.clear();
    m_pianoBaseNote = 52;
    m_demoNote = 50;
    m_pageName.clear();
    m_pageIsFx = false;
    if (m_examplePage)
        m_examplePage->hide();
    m_scroll->show();
    while (QLayoutItem* item = m_column->takeAt(0))
    {
        if (QWidget* w = item->widget())
            w->deleteLater();
        delete item;
    }
}

void TutorialPane::addHeading(int level, const QString& text)
{
    // Extra air above a heading so each section reads as its own group
    if (m_column->count() > 0)
        m_column->addSpacing(ScaleHeightForDPI(level == 1 ? 12 : 8));
    QLabel* label = new QLabel(text, m_content);
    label->setObjectName(level == 1 ? "tutH1" : level == 2 ? "tutH2" : "tutH3");
    label->setWordWrap(true);
    label->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_column->addWidget(label);
}

void TutorialPane::addProse(const QString& richText)
{
    TutProseText* label = new TutProseText(m_content);
    // Readable measure: prose wraps at a comfortable line length even when
    // the pane (and the interactive cards) run much wider.
    label->setMaximumWidth(ScaleWidthForDPI(1200));
    label->setObjectName("tutProse");
    label->setProperty("mdtext", richText);
    label->setHtml(proseColoured(richText));
    label->setLinkHandler([this](const QString& link) { emit linkClicked(QUrl(link)); });
    label->setGroup(m_selGroup);
    m_selGroup->add(label);
    m_proseLabels.append(label);
    m_column->addWidget(label);
}

void TutorialPane::addList(const SonicPi::TutorialBlock& block)
{
    QString tag = block.ordered ? "ol" : "ul";
    QString html = "<" + tag + " style=\"margin-left:12px; -qt-list-indent:1;\">";
    for (const QString& item : block.items)
        html += "<li>" + item + "</li>";
    html += "</" + tag + ">";
    addProse(html);
}

void TutorialPane::addImage(const SonicPi::TutorialBlock& block)
{
    if (m_imagesRoot.isEmpty() || block.path.isEmpty())
        return;
    QString path = m_imagesRoot + "/" + block.path;
    static QHash<QString, QPixmap> cache;
    QPixmap pix = cache.value(path);
    if (pix.isNull())
    {
        pix = QPixmap(path);
        if (pix.isNull())
            return;
        int maxW = ScaleWidthForDPI(560);
        if (pix.width() > maxW)
            pix = pix.scaledToWidth(maxW, Qt::SmoothTransformation);
        cache.insert(path, pix);
    }
    QLabel* label = new QLabel(m_content);
    label->setObjectName("tutImage");
    label->setPixmap(pix);
    if (!block.text.isEmpty())
        label->setAccessibleName(block.text);
    m_column->addWidget(label);
}

void TutorialPane::addSnippet(const QString& code, bool runnable, QVBoxLayout* into,
                              QHBoxLayout* transportInto)
{
    Snippet snippet;
    snippet.code = code;
    snippet.workspace = QString("sonic-pi-tutorial-%1").arg(++m_workspaceSeq);

    snippet.frame = new QFrame(m_content);
    snippet.frame->setObjectName("tutCodeFrame");
    snippet.frame->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
    // Standalone chapter/reference snippets share the prose measure; nested
    // playground snippets span their card.
    if (!into)
        snippet.frame->setMaximumWidth(ScaleWidthForDPI(1200));
    snippet.frame->setProperty("playing", false);
    // Nested inside another card: drop the card chrome (border/fill) and keep
    // only the playing tint, so cards don't stack inside cards.
    snippet.frame->setProperty("nested", into != nullptr);
    // Display-only snippets get a quiet card — the accent border is reserved
    // for code that can actually play.
    snippet.frame->setProperty("runnable", runnable);
    QVBoxLayout* frameLayout = new QVBoxLayout(snippet.frame);
    // Nested snippets are a recessed text-area region; slightly tighter
    // padding than the standalone cards.
    int pad = ScaleWidthForDPI(into ? 8 : 10);
    frameLayout->setContentsMargins(pad, pad, pad, pad);
    frameLayout->setSpacing(ScaleHeightForDPI(4));

    // Same zero-margin text widget as prose, so the code's left edge lines up
    // exactly with the controls below it
    TutProseText* codeView = new TutProseText(snippet.frame);
    snippet.codeView = codeView;
    codeView->setObjectName("tutCode");
    codeView->setHtml(SonicPi::TutorialDocs::highlightCode(code, m_codeColours));
    pinCodeViewHeight(codeView, code);
    codeView->setGroup(m_selGroup);
    m_selGroup->add(codeView);
    // Grid cell shared by the code and the copy glyph, so copy floats in the
    // code area's top-right corner.
    QGridLayout* codeArea = new QGridLayout();
    codeArea->setContentsMargins(0, 0, 0, 0);
    codeArea->addWidget(codeView, 0, 0);
    frameLayout->addLayout(codeArea);
    snippet.codeArea = codeArea;
    // Playground snippets: numbers are anchors — clicking one opens an
    // inline editor wired to the matching dial.
    if (into)
        codeView->setLinkHandler([this](const QString& href) {
            if (href.startsWith(QLatin1String("opt:")))
                openOptEditor(href.mid(4), QCursor::pos());
        });

    QHBoxLayout* controls = transportInto;
    if (!controls)
    {
        controls = new QHBoxLayout();
        controls->setContentsMargins(0, 0, 0, 0);
        controls->setSpacing(ScaleWidthForDPI(4));
    }

    int snippetNum = m_snippets.size() + 1;

    snippet.play = new QPushButton(snippet.frame);
    snippet.play->setObjectName("tutPlay");
    snippet.play->setToolTip(tr("Run this example"));
    snippet.play->setAccessibleName(tr("Run example %1").arg(snippetNum));
    snippet.stop = new QPushButton(snippet.frame);
    snippet.stop->setObjectName("tutStop");
    snippet.stop->setToolTip(tr("Stop this example"));
    snippet.stop->setAccessibleName(tr("Stop example %1").arg(snippetNum));
    snippet.stop->setEnabled(false);
    snippet.copy = new QPushButton(snippet.frame);
    snippet.copy->setObjectName("tutCopy");
    snippet.copy->setToolTip(tr("Copy code to clipboard"));
    snippet.copy->setAccessibleName(tr("Copy example %1").arg(snippetNum));

    // Transport: flat tabler glyph buttons (accent play, foreground stop) in
    // the same family as the title-bar controls — hero-sized in the
    // playground's trigger row, standard under chapter snippets. Copy is a
    // quiet glyph floating in the code area's corner.
    const bool bigTransport = transportInto != nullptr;
    QSize buttonSize = bigTransport ? ScaleForDPI(56, 56) : ScaleForDPI(40, 40);
    QSize glyphSize = bigTransport ? ScaleForDPI(44, 44) : ScaleForDPI(30, 30);
    snippet.play->setIcon(m_playIcon);
    snippet.stop->setIcon(m_stopIcon);
    for (QPushButton* b : { snippet.play, snippet.stop })
    {
        b->setIconSize(glyphSize);
        b->setFixedSize(buttonSize);
        b->setCursor(Qt::PointingHandCursor);
    }
    snippet.copy->setIcon(m_copyIcon);
    snippet.copy->setIconSize(ScaleForDPI(20, 20));
    snippet.copy->setFixedSize(ScaleForDPI(28, 28));
    snippet.copy->setCursor(Qt::PointingHandCursor);
    codeArea->addWidget(snippet.copy, 0, 0, Qt::AlignTop | Qt::AlignRight);
    // Syntax illustrations and output excerpts copy but don't play
    snippet.play->setVisible(runnable);
    snippet.stop->setVisible(runnable);

    controls->addWidget(snippet.play);
    controls->addWidget(snippet.stop);
    if (!transportInto)
    {
        controls->addStretch(1);
        frameLayout->addLayout(controls);
    }

    int index = m_snippets.size();
    connect(snippet.play, &QPushButton::clicked, this, [this, index]() {
        if (index >= m_snippets.size())
            return;
        Snippet& s = m_snippets[index];
        emit runRequested(s.code, s.workspace);
    });
    connect(snippet.stop, &QPushButton::clicked, this, [this, index]() {
        if (index >= m_snippets.size())
            return;
        Snippet& s = m_snippets[index];
        if (s.jobId >= 0)
            emit stopJobRequested(s.jobId);
    });
    connect(snippet.copy, &QPushButton::clicked, this, [this, index]() {
        if (index >= m_snippets.size())
            return;
        Snippet& s = m_snippets[index];
        QApplication::clipboard()->setText(s.code);
        // Visual check-mark flash; spoken confirmation for screen readers.
        s.copy->setIcon(m_copiedIcon);
        emit announceRequested(tr("Copied to clipboard"));
        QPushButton* button = s.copy;
        QTimer::singleShot(1200, button, [this, button]() { button->setIcon(m_copyIcon); });
    });

    m_snippets.append(snippet);
    (into ? into : m_column)->addWidget(snippet.frame);
}

// Native option reference: one self-contained zebra-striped row per opt
// (name / default / doc), so a tall doc can never bleed into the next row.
void TutorialPane::addOptsGrid(const QVector<SonicPi::InstrumentOpt>& opts)
{
    // Uniform name/default columns, measured across all rows with the same
    // font the labels render in (Hack at the button size, tracking zoom).
    QFont mono("Hack");
    mono.setBold(true);
    mono.setPointSizeF(qMax(6.0, 10.0 * m_fontScale));   // the @buttonSize the labels render at
    const QFontMetrics nameMetrics(mono);
    int nameW = 0, defW = 0;
    for (const SonicPi::InstrumentOpt& opt : opts)
    {
        nameW = qMax(nameW, nameMetrics.horizontalAdvance(opt.name + ":"));
        defW = qMax(defW, nameMetrics.horizontalAdvance(opt.defaultText));
    }
    nameW += ScaleWidthForDPI(10);
    defW += ScaleWidthForDPI(10);

    QWidget* table = new QWidget(m_content);
    table->setMaximumWidth(ScaleWidthForDPI(1200));   // shares the prose measure
    table->setObjectName("tutOpts");
    QVBoxLayout* rows = new QVBoxLayout(table);
    rows->setContentsMargins(0, 0, 0, 0);
    rows->setSpacing(0);

    bool alt = false;
    for (const SonicPi::InstrumentOpt& opt : opts)
    {
        QFrame* rowFrame = new QFrame(table);
        rowFrame->setObjectName("tutOptRow");
        rowFrame->setProperty("alt", alt);
        alt = !alt;
        QHBoxLayout* row = new QHBoxLayout(rowFrame);
        row->setContentsMargins(ScaleWidthForDPI(8), ScaleHeightForDPI(5),
                                ScaleWidthForDPI(8), ScaleHeightForDPI(5));
        row->setSpacing(ScaleWidthForDPI(10));

        QLabel* name = new QLabel(opt.name + ":", rowFrame);
        name->setObjectName("tutOptName");
        name->setFixedWidth(nameW);
        name->setTextInteractionFlags(Qt::TextSelectableByMouse);
        QLabel* def = new QLabel(opt.defaultText, rowFrame);
        def->setObjectName("tutOptDefault");
        def->setFixedWidth(defW);
        def->setTextInteractionFlags(Qt::TextSelectableByMouse);
        TutProseText* doc = new TutProseText(rowFrame);
        doc->setObjectName("tutProse");
        // Deterministic wrap width (its heightForWidth clamps to this), so
        // the row height always matches the rendered line count — layouts
        // sometimes query heights at the full row width otherwise.
        doc->setMaximumWidth(ScaleWidthForDPI(1200) - nameW - defW
                             - ScaleWidthForDPI(10) * 2 - ScaleWidthForDPI(8) * 2);
        // Backtick spans read as inline code (`60`, `:C2`), like the prose.
        QString docHtml = opt.doc.toHtmlEscaped();
        static const QRegularExpression ticks(QStringLiteral("`([^`]+)`"));
        docHtml.replace(ticks, QStringLiteral("<code>\\1</code>"));
        if (opt.slidable)
            docHtml += " <i>(" + tr("slidable").toHtmlEscaped() + ")</i>";
        doc->setProperty("mdtext", docHtml);
        doc->setHtml(proseColoured(docHtml));
        doc->setGroup(m_selGroup);
        m_selGroup->add(doc);
        m_proseLabels.append(doc);

        row->addWidget(name, 0, Qt::AlignTop);
        row->addWidget(def, 0, Qt::AlignTop);
        row->addWidget(doc, 1);
        row->addStretch(0);
        rows->addWidget(rowFrame);
        m_optRows.insert(opt.name, rowFrame);
    }
    m_column->addWidget(table);
}

void TutorialPane::addNavFooter()
{
    if (m_prevTitle.isEmpty() && m_nextTitle.isEmpty())
        return;

    QHBoxLayout* nav = new QHBoxLayout();
    nav->setContentsMargins(0, ScaleHeightForDPI(10), 0, 0);

    if (!m_prevTitle.isEmpty())
    {
        QPushButton* prev = new QPushButton(QString("< %1").arg(m_prevTitle), m_content);
        prev->setObjectName("tutNav");
        prev->setAccessibleName(tr("Previous chapter: %1").arg(m_prevTitle));
        connect(prev, &QPushButton::clicked, this, [this]() { emit navigateRequested(-1); });
        nav->addWidget(prev);
    }
    nav->addStretch(1);
    if (!m_nextTitle.isEmpty())
    {
        QPushButton* next = new QPushButton(QString("%1 >").arg(m_nextTitle), m_content);
        next->setObjectName("tutNav");
        next->setAccessibleName(tr("Next chapter: %1").arg(m_nextTitle));
        connect(next, &QPushButton::clicked, this, [this]() { emit navigateRequested(1); });
        nav->addWidget(next);
    }

    QWidget* navRow = new QWidget(m_content);
    navRow->setLayout(nav);
    m_column->addWidget(navRow);
}

void TutorialPane::runStarted(int jobId, const QString& workspace)
{
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.workspace == workspace)
        {
            snippet.jobId = jobId;
            setSnippetPlaying(snippet, true);
            if (m_piano && !m_pageName.isEmpty())
            {
                // Light the key for the note actually played (it may have
                // moved off the page default). An FX page's own note dial is
                // its tuning target, not the played note. Out-of-range
                // offsets simply don't light anything.
                int offset = 0;
                if (m_pageIsFx)
                    offset = m_demoNote - (m_pianoBaseNote + m_octave * 12);
                else
                    for (TutDial* dial : m_dials)
                        if (dial->optName() == "note")
                            offset = qRound(dial->value()) - (m_pianoBaseNote + m_octave * 12);
                m_piano->flash(offset);
            }
            return;
        }
    }
}

void TutorialPane::runEnded(int jobId)
{
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.jobId == jobId)
        {
            snippet.jobId = -1;
            setSnippetPlaying(snippet, false);
            return;
        }
    }
}

void TutorialPane::setSnippetPlaying(Snippet& snippet, bool playing)
{
    if (snippet.play == m_examplePlay)
    {
        // Jukebox toggle: the single transport button flips between play and stop
        snippet.play->setIcon(playing ? m_exStopIcon : m_exPlayIcon);
        snippet.play->setText(playing ? tr("Stop") : tr("Play"));
        snippet.play->setToolTip(playing ? tr("Stop this example") : tr("Run this example"));
        snippet.play->setAccessibleName(playing ? tr("Stop example") : tr("Run example"));
        if (m_exampleScope)
        {
            if (playing)
                m_exampleScope->start(m_spAPI.get(), kJukeboxScopeSlot);
            else
                m_exampleScope->stop();
        }
    }
    else if (snippet.stop)
    {
        snippet.stop->setEnabled(playing);
    }
    snippet.frame->setProperty("playing", playing);
    repolish(snippet.frame);
}

void TutorialPane::scrollStep(int direction)
{
    m_scroll->verticalScrollBar()->triggerAction(
        direction < 0 ? QAbstractSlider::SliderSingleStepSub
                      : QAbstractSlider::SliderSingleStepAdd);
}

QWidget* TutorialPane::zoomControls() const
{
    return m_zoomBar;
}

bool TutorialPane::eventFilter(QObject* obj, QEvent* event)
{
    // Escape cancels an inline opt editor without committing.
    if (event->type() == QEvent::KeyPress
        && obj->objectName() == QLatin1String("tutOptEditor")
        && static_cast<QKeyEvent*>(event)->key() == Qt::Key_Escape)
    {
        QWidget* editor = qobject_cast<QWidget*>(obj);
        editor->setProperty("cancelled", true);
        editor->deleteLater();
        return true;
    }
    return QFrame::eventFilter(obj, event);
}

void TutorialPane::setUserZoom(int zoom)
{
    zoom = qBound(-4, zoom, 8);
    if (zoom == m_userZoom)
        return;
    m_userZoom = zoom;
    applySizing();
}

void TutorialPane::applySizing()
{
    m_fontScale = qBound(0.5, std::pow(1.1, m_userZoom), 3.0);
    if (m_exampleEditor)
        m_exampleEditor->zoomTo(kExampleZoom + m_userZoom);
    applyTheme();
}

QString TutorialPane::proseColoured(const QString& richText) const
{
    QColor accent = m_theme->color("HighlightedBackground");
    QString out = richText;
    out.replace("<code>",
                QString("<span style=\"font-family:'Hack'; color:%1;\">").arg(accent.name()));
    out.replace("</code>", "</span>");
    if (!out.startsWith("<ul") && !out.startsWith("<ol"))
        out = "<p style=\"line-height:150%; margin:0;\">" + out + "</p>";
    return out;
}

void TutorialPane::applyTheme()
{
    QColor bg = m_theme->color("PaneBackground");
    QColor editorBg = m_theme->color("Background");
    QColor fg = m_theme->color("Foreground");
    QColor accent = m_theme->color("HighlightedBackground");
    QColor h2 = m_theme->color("NumberForeground");

    QColor muted = SonicPiTheme::blend(fg, editorBg, 0.38);
    QColor hoverTint = SonicPiTheme::blend(editorBg, accent, 0.25);
    QColor pressedTint = SonicPiTheme::blend(editorBg, accent, 0.45);
    QColor playingTint = SonicPiTheme::blend(editorBg, accent, 0.16);
    QColor sigColour = SonicPiTheme::blend(fg, editorBg, 0.18);

    if (m_zoomBar)
        m_zoomBar->applyTheme();

    // Design sizes at the default editor zoom, scaled to track it
    auto pt = [this](int base) {
        return QString::number(qMax(6, qRound(base * m_fontScale))) + "pt";
    };

    // Soft accent hairline under H1s: mostly-background so it reads as a
    // rule, not another banner.
    QColor h1Rule = SonicPiTheme::blend(bg, accent, 0.45);
    QColor accentHover = SonicPiTheme::blend(accent, fg, 0.14);

    QString qss = QStringLiteral(
        "#tutorialPane, #tutorialContent { background:@bg; }"
        "#tutProse { color:@fg; font-size:@proseSize; background:transparent; }"
        "#tutH1 { background:transparent; color:@accent; font-size:@h1Size; font-weight:bold;"
        " padding-bottom:6dx; border-bottom:2dx solid @h1Rule; }"
        "#tutH2 { background:transparent; color:@h2; font-size:@h2Size; font-weight:bold; }"
        "#tutH3 { color:@fg; font-size:@proseSize; font-weight:bold; }"
        "#tutCode { color:@fg; font-family:'Hack'; font-size:@codeSize; background:transparent; }"
        "#tutCodeFrame { background:@editorBg; border:1dx solid @accent; border-radius:8dx; }"
        // Display-only code: quiet neutral card — accent borders mean playable.
        "#tutCodeFrame[runnable=\"false\"] { border:1dx solid rgba(127,127,127,50);"
        " background:rgba(127,127,127,10); }"
        "#tutCodeFrame[playing=\"true\"] { border:1dx solid @accent; background:@playingTint; }"
        // Nested in the playground: a recessed text-area region, smaller code.
        "#tutCodeFrame[nested=\"true\"] { background:rgba(127,127,127,18);"
        " border:1dx solid rgba(127,127,127,60); border-radius:6dx; }"
        // Playing: the border lights accent; the background stays put so the
        // code doesn't flood with colour while it runs.
        "#tutCodeFrame[nested=\"true\"][playing=\"true\"] { border:1dx solid @accent;"
        " background:rgba(127,127,127,18); }"
        "#tutCodeFrame[nested=\"true\"] #tutCode { font-size:@codeSmall; }"
        // Quiet neutral card matching the display-code grammar; the badge
        // carries the instrument's identity, not a coloured border.
        "#tutPlayground { background:@editorBg; border:1dx solid rgba(127,127,127,60);"
        " border-radius:8dx; }"
        "#tutPlateName { background:transparent; color:@fg; font-size:@h1Size;"
        " font-weight:bold; }"
        // Synth-panel sections: each dial group is its own quiet region.
        "#tutDialGroup { background:rgba(127,127,127,22); border:none; border-radius:6dx; }"
        "#tutSection { color:@muted; font-family:'Hack'; font-size:@hintSize;"
        " background:transparent; }"
        // Transport + octave + reset: flat tabler glyph buttons.
        "#tutPlay, #tutStop, #tutCopy, #tutOct, #tutReset { background:transparent;"
        " border:none; border-radius:6dx; padding:2dx; }"
        "#tutPlay:hover:!pressed, #tutStop:hover:!pressed, #tutCopy:hover:!pressed,"
        " #tutOct:hover:!pressed, #tutReset:hover:!pressed { background:@hoverTint; }"
        "#tutPlay:pressed, #tutStop:pressed, #tutCopy:pressed, #tutOct:pressed,"
        " #tutReset:pressed { background:@pressedTint; }"
        "#tutSig { color:@sigColour; font-family:'Hack'; font-size:@buttonSize; }"
        "#tutHint { color:@muted; font-family:'Hack'; font-size:@hintSize; }"
        "#tutOptName { color:@h2; font-family:'Hack'; font-size:@buttonSize; font-weight:bold;"
        " background:transparent; }"
        "#tutOptDefault { color:@sigColour; font-family:'Hack'; font-size:@buttonSize;"
        " background:transparent; }"
        // Zebra rows keep each opt's doc visually tied to its name.
        "#tutOptRow { background:transparent; border:none; border-radius:4dx; }"
        "#tutOptRow[alt=\"true\"] { background:rgba(127,127,127,16); }"
        // Opt quick-index: a quiet panel of name-links with their defaults.
        "#tutOptIndex { background:rgba(127,127,127,16); border:none;"
        " border-radius:6dx; }"
        "#tutOptLink { background:transparent; color:@h2; border:none;"
        " font-family:'Hack'; font-size:@buttonSize; text-decoration:underline;"
        " padding-top:1dx; padding-bottom:1dx; padding-left:0dx; padding-right:0dx; }"
        "#tutOptLink:hover { color:@accent; }"
        "#tutDials { background:transparent; border:none; }"
        "#tutNav { background:transparent; color:@muted; border:none;"
        " text-decoration:underline; font-size:@navSize; padding:4dx; }"
        "#tutNav:hover { color:@accent; }");

    // Named tokens, replaced longest-first so no token is a prefix of a later
    // one (@h2Size before @h2, @codeSmall before @codeSize, …). An unused
    // token is harmless — unlike the numbered QString::arg markers this
    // replaces, where one gap silently shifted every later substitution.
    const struct { const char* token; QString value; } subs[] = {
        { "@pressedTint", pressedTint.name() },
        { "@playingTint", playingTint.name() },
        { "@editorBg", editorBg.name() },
        { "@hoverTint", hoverTint.name() },
        { "@sigColour", sigColour.name() },
        { "@proseSize", pt(13) },
        { "@buttonSize", pt(10) },
        { "@codeSmall", pt(9) },
        { "@codeSize", pt(12) },
        { "@hintSize", pt(9) },
        { "@navSize", pt(10) },
        { "@h2Size", pt(16) },
        { "@h1Size", pt(19) },
        { "@h1Rule", h1Rule.name() },
        { "@accent", accent.name() },
        { "@muted", muted.name() },
        { "@h2", h2.name() },
        { "@bg", bg.name() },
        { "@fg", fg.name() },
    };
    for (const auto& sub : subs)
        qss.replace(QLatin1String(sub.token), sub.value);
    setStyleSheet(ScalePxInStyleSheet(qss));

    // Jukebox transport buttons: a bold accent-filled Play/Stop and an outlined
    // Load, both prominent so they're easy to spot (styled on the frame so both
    // children pick it up; ScalePxInStyleSheet needs per-side padding).
    QColor loadBorder = SonicPiTheme::blend(fg, editorBg, 0.45);
    QString transportQss = QString(
        "#exPlay { background:%1; color:%2; border:none; border-radius:5dx;"
        " padding-top:4dx; padding-bottom:4dx; padding-left:14dx; padding-right:14dx;"
        " font-size:%3; font-weight:bold; }"
        "#exPlay:hover { background:%4; }"
        "#exLoad { background:transparent; color:%5; border:1dx solid %6;"
        " border-radius:5dx; padding-top:4dx; padding-bottom:4dx;"
        " padding-left:12dx; padding-right:12dx; font-size:%3; }"
        "#exLoad:hover { color:%7; border-color:%7; }")
        .arg(accent.name(), m_theme->contrastingText(accent).name(), pt(12),
             accentHover.name(), fg.name(), loadBorder.name(), accent.name());
    if (m_examplePlay)
        m_examplePlay->setStyleSheet(ScalePxInStyleSheet(transportQss));
    if (m_exampleLoad)
        m_exampleLoad->setStyleSheet(ScalePxInStyleSheet(transportQss));

    applyContentTheme();
}

// The theme-dependent bits of the page content (prose colours, icons,
// dials). Cheap relative to a stylesheet re-polish, so page loads call
// this alone — the pane-level stylesheet already styles new children.
void TutorialPane::applyContentTheme()
{
    QColor editorBg = m_theme->color("Background");
    QColor fg = m_theme->color("Foreground");
    QColor accent = m_theme->color("HighlightedBackground");
    QColor link = m_theme->color("KeywordForeground");
    QColor muted = SonicPiTheme::blend(fg, editorBg, 0.38);

    QPalette pal = palette();
    pal.setColor(QPalette::Link, link);
    setPalette(pal);

    // Re-render prose (inline code colour / doc CSS) and refresh snippet
    // editors + themed button icons
    for (TutProseText* label : m_proseLabels)
    {
        QString md = label->property("mdtext").toString();
        if (!md.isEmpty())
            label->setHtml(proseColoured(md));
        label->setPalette(pal);
    }
    m_codeColours.keyword = m_theme->color("KeywordForeground").name();
    m_codeColours.symbol = m_theme->color("SymbolForeground").name();
    m_codeColours.number = m_theme->color("NumberForeground").name();
    m_codeColours.string = m_theme->color("DoubleQuotedStringForeground").name();
    m_codeColours.comment = m_theme->color("CommentForeground").name();

    // Tabler transport glyphs, matching the title-bar controls' icon family.
    const qreal dpr = devicePixelRatioF();
    const int glyphPx = ScaleWidthForDPI(30);
    // Same transport badges as the quickstart cards: filled glyph on a disc
    // (accent for play; foreground for stop, which greys out when disabled).
    m_playIcon = QIcon(TablerIcons::discBadge(TablerIcons::Glyph::PlayFilled, accent,
                                              m_theme->contrastingText(accent), glyphPx, dpr));
    m_stopIcon = QIcon(TablerIcons::discBadge(TablerIcons::Glyph::StopFilled, fg,
                                              m_theme->contrastingText(fg), glyphPx, dpr));
    m_copyIcon = TablerIcons::icon(TablerIcons::Glyph::Copy, muted, ScaleWidthForDPI(24), dpr);
    m_copiedIcon = TablerIcons::icon(TablerIcons::Glyph::Check, accent, ScaleWidthForDPI(24), dpr);
    // The jukebox transport button is accent-filled, so its glyphs are drawn in
    // the contrasting colour rather than the accent used on the flat snippets.
    QColor exGlyph = m_theme->contrastingText(accent);
    // The jukebox button is itself accent-filled, so its glyphs stay flat
    // (a disc-on-pill would double up) but share the cards' filled shapes.
    m_exPlayIcon = TablerIcons::icon(TablerIcons::Glyph::PlayFilled, exGlyph, glyphPx, dpr);
    m_exStopIcon = TablerIcons::icon(TablerIcons::Glyph::StopFilled, exGlyph, glyphPx, dpr);
    // Re-tint the per-page glyph icons (Reset / octave −+) for the new theme.
    for (QPushButton* b : m_content->findChildren<QPushButton*>("tutReset"))
        b->setIcon(TablerIcons::icon(TablerIcons::Glyph::Restore, muted, ScaleWidthForDPI(20), dpr));
    for (QPushButton* b : m_content->findChildren<QPushButton*>("tutOct"))
        b->setIcon(TablerIcons::icon(b->property("octDir").toInt() < 0
                                         ? TablerIcons::Glyph::CircleMinus
                                         : TablerIcons::Glyph::CirclePlus,
                                     muted, ScaleWidthForDPI(26), dpr));
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.codeView)
        {
            snippet.codeView->setHtml(
                snippet.commentsAside
                    ? exampleTableHtml(snippet.code)
                    : SonicPi::TutorialDocs::highlightCode(snippet.code, m_codeColours));
            pinCodeViewHeight(snippet.codeView, snippet.code);
        }
        // The example snippet's play button is the jukebox transport, themed
        // just below; its stop pointer is null (single toggle button).
        if (snippet.play && snippet.play != m_examplePlay)
            snippet.play->setIcon(m_playIcon);
        if (snippet.stop)
            snippet.stop->setIcon(m_stopIcon);
        if (snippet.copy)
            snippet.copy->setIcon(m_copyIcon);
    }
    bool examplePlaying = !m_snippets.isEmpty() && m_snippets[0].play == m_examplePlay
                          && m_snippets[0].jobId >= 0;
    m_examplePlay->setIcon(examplePlaying ? m_exStopIcon : m_exPlayIcon);
    // Pin the transport button to the wider of its two states so it doesn't
    // change size when the label toggles between Play and Stop. Recomputed here
    // so it tracks the current font size (icons + stylesheet are already set).
    m_examplePlay->ensurePolished();
    QString exText = m_examplePlay->text();
    int exW = 0;
    for (const QString& s : { tr("Play"), tr("Stop") })
    {
        m_examplePlay->setText(s);
        exW = qMax(exW, m_examplePlay->sizeHint().width());
    }
    m_examplePlay->setText(exText);
    m_examplePlay->setFixedWidth(exW);
    if (m_exampleScope)
        m_exampleScope->setColours(accent, SonicPiTheme::blend(editorBg, fg, 0.22),
                                   SonicPiTheme::blend(editorBg, fg, 0.05),
                                   SonicPiTheme::blend(editorBg, fg, 0.22));
    if (m_exampleEditor)
        m_exampleEditor->redraw();

    QColor dialTrack = SonicPiTheme::blend(editorBg, fg, 0.28);
    for (TutDial* dial : m_dials)
        dial->setColours(fg, muted, accent, dialTrack);
    if (m_piano)
        m_piano->setColours(fg, editorBg, accent, muted);

    const QList<QLabel*> icons = m_content->findChildren<QLabel*>("tutFxIcon");
    for (QLabel* iconLabel : icons)
        renderFxIcon(iconLabel);

    // Playground snippet: rebuild so its editable-number anchors keep the new
    // theme colours (the generic re-render above used plain highlighting).
    if (!m_pageName.isEmpty())
        regenerateInstrumentCode();
}

void TutorialPane::renderFxIcon(QLabel* iconLabel)
{
    // FX icons in the reference blue, synths in the accent pink — the
    // colour split tau-state's sets were drawn in
    bool isFx = iconLabel->property("isfx").toBool();
    QColor colour = m_theme->color(isFx ? "NumberForeground" : "HighlightedBackground");
    QSvgRenderer renderer(
        instrumentIconSvg(isFx, iconLabel->property("fxname").toString(), colour).toUtf8());
    // Badge-sized: sits beside the name at roughly its cap height, not a
    // banner illustration.
    QSize size = ScaleForDPI(56, 32);
    qreal dpr = devicePixelRatioF();
    QPixmap pix(size * dpr);
    pix.fill(Qt::transparent);
    QPainter painter(&pix);
    renderer.render(&painter, QRectF(QPointF(0, 0), QSizeF(size * dpr)));
    painter.end();
    pix.setDevicePixelRatio(dpr);
    iconLabel->setPixmap(pix);
}

