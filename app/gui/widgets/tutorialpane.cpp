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
#include "carddeck.h"
#include "codecard.h"
#include "utils/code_colours.h"
#include "tutorialwidgets.h"
#include "dpi.h"
#include "utils/fontroles.h"
#include "model/sonicpitheme.h"
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
    int heightForWidth(int w) const override
    {
        // Answer at the width the flow will actually render at: box layouts
        // budget items at the full content width but apply geometry at the
        // width-capped one, and a flow wraps into more (taller) lines at
        // the cap — the difference would be stolen from a sibling.
        if (QWidget* pw = parentWidget())
            w = qMin(w, pw->maximumWidth());
        return doLayout(QRect(0, 0, w, 0), true);
    }
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

TutorialPane::TutorialPane(SonicPiTheme* theme, QWidget* parent)
    : QFrame(parent)
    , m_theme(theme)
{
    registerTutorialWidgetAccessibility();
    setObjectName("tutorialPane");

    m_selGroup = std::make_shared<TutSelectionGroup>();

    // A page's cards play one at a time, as the web's do. Each run goes
    // through a scope tap on the jukebox's slot, for the card's rings.
    m_deck = new CardDeck(this, CardDeck::Playing::OneAtATime);
    connect(m_deck, &CardDeck::runRequested, this,
            [this](const QString&, const QString& code, const QString& workspace, int) {
                emit runRequested(code, workspace, false, true);
            });
    connect(m_deck, &CardDeck::stopJobRequested, this, &TutorialPane::stopJobRequested);

    QVBoxLayout* outer = new QVBoxLayout(this);
    outer->setContentsMargins(0, 0, 0, 0);
    outer->setSpacing(0);

    // Docs text-size controls (A- / A+): the shared ZoomBar, displayed at the
    // foot of the help's tab rail (MainWindow places the widget), like the
    // Cards/Logs/Debug tabs' bars.
    m_zoomBar = new ZoomBar(m_theme, tr("documentation"), this);
    // Through setUserZoom, not around it: it owns the clamp and the
    // already-at-that-zoom guard, so pressing A+ at the ceiling is a no-op
    // instead of rebuilding the page and resetting the scroll every time.
    connect(m_zoomBar, &ZoomBar::zoomStep, this,
            [this](int delta) { setUserZoom(m_userZoom + delta); });

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
    int inset = sx(22);
    m_column->setContentsMargins(inset, sy(26), inset, sy(36));
    // The gap between every block in the page — prose, code frames, cards. It
    // is the page's base rhythm, so headings add their extra air on top of it
    // (addHeading) rather than replacing it. Generous on purpose: this is a
    // long read, and tight blocks make a chapter look like one wall of text.
    m_column->setSpacing(sy(18));

    QHBoxLayout* contentRow = new QHBoxLayout(m_content);
    contentRow->setContentsMargins(0, 0, 0, 0);
    // Column takes all width up to its cap; the spacer only absorbs the rest
    contentRow->addWidget(columnWidget, 1);
    contentRow->addStretch(0);

    m_scroll->setWidget(m_content);
    outer->addWidget(m_scroll, 1);

    applyTheme();
}

void TutorialPane::setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api)
{
    m_spAPI = api;
    m_deck->setAudioApi(m_spAPI.get());
}

void TutorialPane::loadChapter(const SonicPi::TutorialChapter& chapter, const QString& imagesRoot,
                               const QString& prevTitle, const QString& nextTitle)
{
    m_chapter = chapter;
    m_imagesRoot = imagesRoot;
    m_prevTitle = prevTitle;
    m_nextTitle = nextTitle;
    m_pageKind = PageKind::Chapter;
    rebuild();
    announcePage(chapter.title);
}

// An example, as the web's Examples page has it: one card, fitted to the
// pane so its transport always shows; a long example scrolls inside it. An
// edit is kept until Reset.
void TutorialPane::showExamplePage(const QString& key, const QString& title, const QString& code,
                                   const QString& blurb)
{
    beginPage();
    m_pageKind = PageKind::Example;
    m_exampleKey = key;
    m_exampleTitle = title;
    m_exampleCode = code;
    m_exampleBlurb = blurb;
    m_pageId = QStringLiteral("example:") + key;
    m_cardSection = title;
    QString shown = code;
    if (shown.endsWith(QLatin1Char('\n')))
        shown.chop(1);
    m_exampleCard = addCard(shown, true,
                            CodeCard::Edit | CodeCard::Reset | CodeCard::Copy | CodeCard::Open
                                | CodeCard::Drag,
                            blurb, QStringLiteral("examples/") + key);
    endPage(title);
    fitExampleCard();
}

void TutorialPane::fitExampleCard()
{
    if (m_pageKind != PageKind::Example || !m_exampleCard)
        return;
    // The room the column leaves, less what of the card isn't its code.
    const QMargins margins = m_column->contentsMargins();
    const int room = m_scroll->viewport()->height() - margins.top() - margins.bottom();
    m_exampleCard->ensurePolished();
    const int chrome = m_exampleCard->sizeHint().height() - m_exampleCard->body()->height();
    m_exampleCard->setCodeBodyHeight(qMax(sy(80), room - chrome));
}

void TutorialPane::resizeEvent(QResizeEvent* event)
{
    QFrame::resizeEvent(event);
    fitExampleCard();
}

// Page titles are announced on navigation only. A zoom step rebuilds the
// same page, and re-reading the title each time A+ is pressed would bury the
// zoom feedback the ZoomBar itself announces.
void TutorialPane::announcePage(const QString& title)
{
    if (!m_redisplaying)
        emit announceRequested(title);
}

bool TutorialPane::playFirstSnippet()
{
    for (CodeCard* card : m_deck->cards())
        if (card->playButton())
        {
            card->playButton()->click();
            return true;
        }
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
    restoreFocusAfterBuild();
    announcePage(announceTitle);
}

void TutorialPane::showSampleGroupPage(const SonicPi::SampleGroup& group)
{
    beginPage();
    m_pageKind = PageKind::SampleGroup;
    m_sampleGroupPage = group;
    addHeading(1, group.title);
    for (const QString& sample : group.samples)
        addSnippet("sample :" + sample);
    endPage(group.title);
}

void TutorialPane::showLangPage(const SonicPi::LangPage& page)
{
    beginPage();
    m_pageKind = PageKind::Lang;
    m_langPage = page;
    m_pageId = QStringLiteral("lang:") + page.key;
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
        // Cards, as the web's docs have them: each plays if the reference
        // says it can stand alone (one that raises shows its error on the
        // card), and goes into the editor by Add or a drag.
        addHeading(2, tr("Examples"));
        for (const SonicPi::CodeExample& example : page.examples)
            addCard(example.code, example.runnable,
                    CodeCard::Edit | CodeCard::Reset | CodeCard::Copy | CodeCard::Add
                        | CodeCard::Drag);
    }
    if (!page.introduced.isEmpty())
        addProse("<i>" + tr("Introduced in %1").arg(page.introduced).toHtmlEscaped() + "</i>");
    endPage(page.key);
}

void TutorialPane::showInstrumentPage(bool isFx, const SonicPi::InstrumentPage& page)
{
    beginPage();
    m_pageKind = PageKind::Instrument;
    m_instrumentPage = page;
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
    int playPad = sx(12);
    playLayout->setContentsMargins(playPad, sy(10), playPad, sy(10));
    playLayout->setSpacing(sy(8));

    // Faceplate header inside the card: the instrument's glyph and name as
    // one left-aligned badge, like a hardware synth's silkscreened logo.
    QWidget* headerRow = new QWidget(playground);
    QHBoxLayout* header = new QHBoxLayout(headerRow);
    header->setContentsMargins(0, 0, 0, 0);
    header->setSpacing(sx(10));
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
    reset->setIconSize(QSize(sx(20), sy(20)));
    reset->setFixedSize(QSize(sx(28), sy(28)));
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
    TutFlowLayout* dialFlow = new TutFlowLayout(sx(4), sy(6));
    dialsRow->setLayout(dialFlow);

    QColor dialFg = m_theme->color("Foreground");
    QColor dialBg = m_theme->color("Background");
    QColor dialAccent = m_theme->color("HighlightedBackground");
    QColor dialMuted = SonicPiTheme::blend(dialFg, dialBg, 0.38);
    // Strong enough to read as a ring even at rest (0.18 washed out).
    QColor dialTrack = SonicPiTheme::blend(dialBg, dialFg, 0.28);
    reset->setIcon(TablerIcons::icon(TablerIcons::Glyph::Restore, dialMuted,
                                     sx(20), devicePixelRatioF()));

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
        panelLayout->setContentsMargins(sx(6), sy(2),
                                        sx(6), sy(4));
        panelLayout->setSpacing(sy(2));
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
                row->setSpacing(sx(2));
                panelLayout->addLayout(row);
                dialRows.append(row);
            }
            const SonicPi::InstrumentOpt& opt = groups[g][d];
            TutDial* dial = new TutDial(opt.name, opt.min, opt.max, opt.defaultNum, onChange,
                                        panel, opt.minExcl, opt.maxExcl);
            dial->setUiScale(m_fontScale);   // painted widget: not reached by the pane qss
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
        // Roving focus: the dial cluster is ONE Tab stop, however many
        // knobs the instrument has. Tab enters at the first dial (only it
        // is TabFocus), Left/Right move between dials, Up/Down adjust the
        // focused one, and Tab moves on past the whole cluster.
        for (int i = 0; i < m_dials.size(); ++i)
        {
            // Every dial takes focus on click (an open value editor on
            // another dial must lose focus and close); only the roving one
            // is additionally tab-focusable.
            m_dials[i]->setFocusPolicy(i == 0 ? Qt::StrongFocus : Qt::ClickFocus);
            m_dials[i]->setNavigateHandler([this](TutDial* from, int direction) {
                const int at = m_dials.indexOf(from);
                const int to = at + direction;
                if (at < 0 || to < 0 || to >= m_dials.size())
                    return; // ends don't wrap: the cluster has clear edges
                // The roving tab stop follows focus, so Shift+Tab back into
                // the cluster returns to where the user left off.
                from->setFocusPolicy(Qt::ClickFocus);
                m_dials[to]->setFocusPolicy(Qt::StrongFocus);
                m_dials[to]->setFocus(Qt::OtherFocusReason);
            });
        }
    }

    // Generated snippet next (a recessed text area), then the trigger row at
    // the very bottom: transport and keyboard side by side — both are ways
    // to make sound, so they share one surface. addSnippet places the code
    // frame in playLayout and the play/stop/copy buttons into this row.
    QWidget* pianoRow = new QWidget(playground);
    QHBoxLayout* pianoLayout = new QHBoxLayout(pianoRow);
    pianoLayout->setContentsMargins(0, 0, 0, 0);
    pianoLayout->setSpacing(sx(6));

    addSnippet(QString(), true, playLayout, pianoLayout);
    // Reset joins Copy in the snippet's corner cluster.
    Snippet& demo = m_snippets.last();
    // The demo transport sounds as instantly as the piano keys do — both
    // run on the use_real_time path rather than the default sched-ahead.
    demo.realTime = true;
    if (demo.codeArea && demo.copy)
    {
        QWidget* corner = new QWidget(demo.frame);
        QHBoxLayout* cornerRow = new QHBoxLayout(corner);
        cornerRow->setContentsMargins(0, 0, 0, 0);
        cornerRow->setSpacing(sx(2));
        demo.codeArea->removeWidget(demo.copy);
        cornerRow->addWidget(reset);
        cornerRow->addWidget(demo.copy);
        demo.codeArea->addWidget(corner, 0, 0, Qt::AlignTop | Qt::AlignRight);
    }
    pianoLayout->addSpacing(sx(12));
    const int octIconPx = sx(26);
    const qreal octDpr = devicePixelRatioF();
    QPushButton* octDown = new QPushButton(pianoRow);
    octDown->setObjectName("tutOct");
    octDown->setProperty("octDir", -1);
    octDown->setIconSize(QSize(sx(26), sy(26)));
    octDown->setIcon(TablerIcons::icon(TablerIcons::Glyph::CircleMinus, dialMuted, octIconPx, octDpr));
    octDown->setToolTip(tr("Octave down (z)"));
    octDown->setAccessibleName(tr("Octave down"));
    octDown->setCursor(Qt::PointingHandCursor);
    connect(octDown, &QPushButton::clicked, this, [this]() { shiftOctave(-1); });
    QPushButton* octUp = new QPushButton(pianoRow);
    octUp->setObjectName("tutOct");
    octUp->setProperty("octDir", 1);
    octUp->setIconSize(QSize(sx(26), sy(26)));
    octUp->setIcon(TablerIcons::icon(TablerIcons::Glyph::CirclePlus, dialMuted, octIconPx, octDpr));
    octUp->setToolTip(tr("Octave up (x)"));
    octUp->setAccessibleName(tr("Octave up"));
    octUp->setCursor(Qt::PointingHandCursor);
    connect(octUp, &QPushButton::clicked, this, [this]() { shiftOctave(1); });
    m_piano = new TutPiano([this](int offset) { playKeyboardNote(offset); }, pianoRow);
    m_piano->setUiScale(m_fontScale);
    m_piano->setColours(dialFg, dialBg, dialAccent, dialMuted);
    m_octaveLabel = new QLabel(pianoRow);
    m_octaveLabel->setObjectName("tutHint");
    pianoLayout->addWidget(octDown, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_piano, 1);   // keyboard grows into spare row width
    pianoLayout->addWidget(octUp, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_octaveLabel, 0, Qt::AlignVCenter);
    // A zero-stretch tail: the keyboard (the only stretch item) wins all
    // spare width until its 30-white cap, and only then does the spacer
    // absorb the rest — so octave-up stays against the board on very wide
    // panes instead of drifting to the pane edge. (An equal-stretch tail
    // would split the spare width and halve the board at every width.)
    pianoLayout->addStretch(0);
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
        index->setMaximumWidth(sx(1200));
        // Uniform cells flowed to the pane width, not a fixed column count:
        // four hard columns of zoomed Hack exceeded the pane at high zoom and
        // clipped unreachably (the scroll area has no horizontal scrollbar).
        TutFlowLayout* flow = new TutFlowLayout(sx(22), sy(2));
        flow->setContentsMargins(sx(10), sy(6),
                                 sx(10), sy(6));
        index->setLayout(flow);
        QVector<QWidget*> cells;
        for (const SonicPi::InstrumentOpt& opt : page.opts)
        {
            QWidget* cell = new QWidget(index);
            QHBoxLayout* cellRow = new QHBoxLayout(cell);
            cellRow->setContentsMargins(0, 0, 0, 0);
            cellRow->setSpacing(sx(5));
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
                    qMax(0, row->mapTo(m_content, QPoint(0, 0)).y() - sy(8)));
            });
            QLabel* def = new QLabel(opt.defaultText, cell);
            def->setObjectName("tutOptDefault");
            cellRow->addWidget(link);
            cellRow->addWidget(def);
            cellRow->addStretch(1);
            flow->addWidget(cell);
            cells.append(cell);
        }
        int cellW = 0;
        for (QWidget* cell : cells)
            cellW = qMax(cellW, cell->sizeHint().width());
        for (QWidget* cell : cells)
            cell->setFixedWidth(cellW);
        m_column->addWidget(index);
        m_optIndex = index;
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
    editor->setFixedSize(sx(70), sy(26));
    const QPoint local = mapFromGlobal(globalPos);
    editor->move(qBound(0, local.x() - sx(24), qMax(0, width() - editor->width())),
                 qBound(0, local.y() - sy(30), qMax(0, height() - editor->height())));
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
    m_pageId = QStringLiteral("chapter:") + m_chapter.title;

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
            addCard(block.source, block.runnable,
                    CodeCard::Edit | CodeCard::Reset | CodeCard::Copy | CodeCard::Open
                        | CodeCard::Drag);
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
    restoreFocusAfterBuild();
}

void TutorialPane::clearContent()
{
    // Real navigation drops any scroll position held for a zoom rebuild, so
    // a restore still in flight can't scroll the incoming page. (A page's
    // cards' runs outlive it, as the web's do: the next card played stops
    // them, and a zoom's rebuild of the same page finds them playing.)
    if (!m_redisplaying)
        m_pendingScrollFrac = -1.0;
    // The teardown below deletes whichever widget holds focus; note that now
    // so the incoming page can take focus back instead of losing it to the
    // window (which reads as "kicked to the title bar" in a screen reader).
    QWidget* focused = QApplication::focusWidget();
    m_restoreFocus = focused && isAncestorOf(focused);
    m_snippets.clear();
    m_deck->clear();
    m_cardSection.clear();
    m_cardCounts.clear();
    m_cardIndex = 0;
    m_dials.clear();
    // Labels deregister themselves on destruction, but that happens via
    // deleteLater — drop any live selection now so the group never points
    // at widgets awaiting deletion
    m_selGroup->clearAll();
    m_proseLabels.clear();
    m_readingOrder.clear();
    m_piano = nullptr;
    m_octaveLabel = nullptr;
    m_optRows.clear();
    m_optIndex = nullptr;
    m_pianoBaseNote = 52;
    m_demoNote = 50;
    m_pageName.clear();
    m_pageIsFx = false;
    m_exampleCard = nullptr;
    while (QLayoutItem* item = m_column->takeAt(0))
    {
        if (QWidget* w = item->widget())
            w->deleteLater();
        delete item;
    }
}

void TutorialPane::addHeading(int level, const QString& text)
{
    // Extra air above a heading so each section reads as its own group. This
    // sits on top of the column's own inter-block spacing, and wants to end up
    // clearly greater than the gap *below* the heading — otherwise the title
    // floats between two bodies of text instead of belonging to the one it
    // introduces. Deeper levels get less, so the hierarchy is legible from the
    // spacing alone.
    if (m_column->count() > 0)
        m_column->addSpacing(sy(level == 1 ? 28 : level == 2 ? 20 : 12));
    // TutHeading: reads as a real heading (with its level) to screen readers
    m_cardSection = text; // the cards below are named for it
    QLabel* label = new TutHeading(text, level, m_content);
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
    label->setMaximumWidth(sx(1200));
    label->setObjectName("tutProse");
    label->setProperty("mdtext", richText);
    label->setHtml(proseColoured(richText));
    label->setLinkHandler([this](const QString& link) { emit linkClicked(QUrl(link)); });
    label->setGroup(m_selGroup);
    m_selGroup->add(label);
    m_proseLabels.append(label);
    wireProse(label);
    m_column->addWidget(label);
}

void TutorialPane::wireProse(TutProseText* label)
{
    m_readingOrder.append(label);
    label->setCaretExitHandler(
        [this](TutProseText* from, int direction) { focusAdjacentText(from, direction); });
    label->setCaretVisibleHandler(
        [this](const QRect& globalRect) { ensureGlobalRectVisible(globalRect); });
    label->setAnnounceHandler([this](const QString& msg) { emit announceRequested(msg); });
}

void TutorialPane::focusAdjacentText(TutProseText* from, int direction)
{
    const int i = m_readingOrder.indexOf(from) + (direction < 0 ? -1 : 1);
    if (i < 0 || i >= m_readingOrder.size())
        return; // first/last block: the caret simply stops
    TutProseText* next = m_readingOrder[i];
    next->setFocus(Qt::OtherFocusReason);
    // Entering forwards starts at the top, entering backwards at the end, so
    // repeated arrow presses read straight through the page.
    next->setCaretPosition(direction < 0 ? next->endPosition() : 0);
}

void TutorialPane::ensureGlobalRectVisible(const QRect& globalRect)
{
    if (!m_scroll || !m_scroll->isVisible())
        return;
    QWidget* vp = m_scroll->viewport();
    const QRect view(vp->mapToGlobal(QPoint(0, 0)), vp->size());
    const int margin = sy(24); // context line above/below the caret
    QScrollBar* bar = m_scroll->verticalScrollBar();
    if (globalRect.top() < view.top() + margin)
        bar->setValue(bar->value() - (view.top() + margin - globalRect.top()));
    else if (globalRect.bottom() > view.bottom() - margin)
        bar->setValue(bar->value() + (globalRect.bottom() - (view.bottom() - margin)));
}

void TutorialPane::restoreFocusAfterBuild()
{
    if (!m_restoreFocus)
        return;
    m_restoreFocus = false;
    focusContent();
}

void TutorialPane::focusContent()
{
    if (!m_readingOrder.isEmpty())
    {
        TutProseText* first = m_readingOrder.first();
        first->setFocus(Qt::OtherFocusReason);
        first->setCaretPosition(0);
        return;
    }
    if (m_scroll)
        m_scroll->setFocus(Qt::OtherFocusReason);
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
        int maxW = sx(560);
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
        snippet.frame->setMaximumWidth(sx(1200));
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
    int pad = sx(into ? 8 : 10);
    frameLayout->setContentsMargins(pad, pad, pad, pad);
    frameLayout->setSpacing(sy(4));

    // Same zero-margin text widget as prose, so the code's left edge lines up
    // exactly with the controls below it
    TutProseText* codeView = new TutProseText(snippet.frame);
    snippet.codeView = codeView;
    codeView->setObjectName("tutCode");
    codeView->setHtml(SonicPi::TutorialDocs::highlightCode(code, m_codeColours));
    pinCodeViewHeight(codeView, code);
    codeView->setGroup(m_selGroup);
    m_selGroup->add(codeView);
    wireProse(codeView); // code blocks join the caret reading path too
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
        controls->setSpacing(sx(4));
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
    QSize buttonSize = bigTransport ? QSize(sx(56), sy(56)) : QSize(sx(40), sy(40));
    QSize glyphSize = bigTransport ? QSize(sx(44), sy(44)) : QSize(sx(30), sy(30));
    snippet.play->setIcon(m_playIcon);
    snippet.stop->setIcon(m_stopIcon);
    for (QPushButton* b : { snippet.play, snippet.stop })
    {
        b->setIconSize(glyphSize);
        b->setFixedSize(buttonSize);
        b->setCursor(Qt::PointingHandCursor);
    }
    snippet.copy->setIcon(m_copyIcon);
    snippet.copy->setIconSize(QSize(sx(20), sy(20)));
    snippet.copy->setFixedSize(QSize(sx(28), sy(28)));
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
        emit runRequested(s.realTime ? "use_real_time\n" + s.code : s.code, s.workspace);
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

CodeCard* TutorialPane::addCard(const QString& code, bool runnable, int actions,
                                const QString& blurb, const QString& key)
{
    const QString section = m_cardSection.isEmpty() ? tr("Example") : m_cardSection;
    const int n = ++m_cardCounts[section];
    CodeCard::Spec spec;
    spec.title = n > 1 ? QStringLiteral("%1 · %2").arg(section).arg(n) : section;
    spec.code = code;
    spec.blurb = blurb.toHtmlEscaped();
    spec.actions = actions;
    spec.runnable = runnable;
    spec.readable = true;
    spec.key = key;
    spec.scopeSlot = kJukeboxScopeSlot;
    // As tall as its code; the type at the page's code size, the title at its
    // prose size.
    CodeCard::Metrics metrics;
    metrics.codeFontPx = zoomedPtPx(12);
    metrics.titlePx = zoomedPtPx(13);
    metrics.scopeSide = sy(56);
    metrics.zoomFactor = m_fontScale;
    CodeCard* card = new CodeCard(spec, metrics, m_theme, m_content);
    card->setMaximumWidth(sx(1200)); // the prose's measure

    // The code reads on from the prose above and into the prose below, and a
    // selection may run through it.
    TutProseText* text = card->codeText();
    text->setGroup(m_selGroup);
    m_selGroup->add(text);
    wireProse(text);

    connect(card, &CodeCard::openRequested, this,
            [this](const QString&, const QString& code) { emit loadRequested(code); });
    connect(card, &CodeCard::insertRequested, this, &TutorialPane::insertRequested);
    connect(card, &CodeCard::insertPreviewRequested, this, &TutorialPane::insertPreviewRequested);
    connect(card, &CodeCard::insertPreviewCleared, this, &TutorialPane::insertPreviewCleared);
    connect(card, &CodeCard::dragEnded, this, &TutorialPane::dragEnded);
    connect(card, &CodeCard::copyRequested, this, &TutorialPane::copyRequested);
    connect(card, &CodeCard::announceRequested, this, &TutorialPane::announceRequested);
    // Stable across a zoom's rebuild of the same page, so the deck finds a
    // playing card's runs again.
    m_deck->add(card, QStringLiteral("sonic-pi-tutorial-card-%1-%2")
                          .arg(qHash(m_pageId), 0, 16)
                          .arg(++m_cardIndex));
    m_column->addWidget(card);
    return card;
}

int TutorialPane::zoomedPtPx(int basePt) const
{
    QFont f;
    f.setPointSize(qMax(6, qRound(basePt * m_fontScale)));
    return QFontInfo(f).pixelSize();
}

// Native option reference: one self-contained zebra-striped row per opt
// (name / default / doc), so a tall doc can never bleed into the next row.
void TutorialPane::addOptsGrid(const QVector<SonicPi::InstrumentOpt>& opts)
{
    // Uniform name/default columns, measured across all rows with the same
    // font the labels render in (Hack at the button size, tracking zoom).
    QFont mono("Hack");
    mono.setBold(true);
    mono.setPointSizeF(qMax(6.0, 12.0 * m_fontScale));   // the @optNameSize the labels render at
    const QFontMetrics nameMetrics(mono);
    int nameW = 0, defW = 0;
    for (const SonicPi::InstrumentOpt& opt : opts)
    {
        nameW = qMax(nameW, nameMetrics.horizontalAdvance(opt.name + ":"));
        defW = qMax(defW, nameMetrics.horizontalAdvance(opt.defaultText));
    }
    nameW += sx(10);
    defW += sx(10);

    QWidget* table = new QWidget(m_content);
    table->setMaximumWidth(sx(1200));   // shares the prose measure
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
        row->setContentsMargins(sx(8), sy(9),
                                sx(8), sy(9));
        row->setSpacing(sx(10));

        // The row's name links back up to the quick-index — the return leg
        // of the index's jump-down links.
        QPushButton* name = new QPushButton(opt.name + ":", rowFrame);
        name->setObjectName("tutOptName");
        name->setFixedWidth(nameW);
        name->setCursor(Qt::PointingHandCursor);
        name->setAccessibleName(tr("Back to the options list from %1").arg(opt.name));
        connect(name, &QPushButton::clicked, this, [this]() {
            m_scroll->verticalScrollBar()->setValue(
                m_optIndex
                    ? qMax(0, m_optIndex->mapTo(m_content, QPoint(0, 0)).y() - sy(8))
                    : 0);
        });
        QLabel* def = new QLabel(opt.defaultText, rowFrame);
        def->setObjectName("tutOptDefault");
        def->setFixedWidth(defW);
        def->setTextInteractionFlags(Qt::TextSelectableByMouse);
        TutProseText* doc = new TutProseText(rowFrame);
        doc->setObjectName("tutProse");
        // Deterministic wrap width (its heightForWidth clamps to this), so
        // the row height always matches the rendered line count — layouts
        // sometimes query heights at the full row width otherwise.
        doc->setMaximumWidth(sx(1200) - nameW - defW
                             - sx(10) * 2 - sx(8) * 2);
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
        wireProse(doc);

        row->addWidget(name, 0, Qt::AlignTop);
        row->addWidget(def, 0, Qt::AlignTop);
        row->addWidget(doc, 1);
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
    nav->setContentsMargins(0, sy(10), 0, 0);

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
    m_deck->runStarted(jobId, workspace);
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
    m_deck->runEnded(jobId);
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

void TutorialPane::flashLine(const QString& workspace, int line)
{
    m_deck->flashLine(workspace, line);
}

void TutorialPane::runOutput(int jobId, const QString& text)
{
    m_deck->runOutput(jobId, text);
}

void TutorialPane::runError(int jobId, const QString& message, int line)
{
    m_deck->runError(jobId, message, line);
}

void TutorialPane::setSnippetPlaying(Snippet& snippet, bool playing)
{
    if (snippet.stop)
        snippet.stop->setEnabled(playing);
    snippet.frame->setProperty("playing", playing);
    repolish(snippet.frame);
}

void TutorialPane::scrollStep(int direction)
{
    m_scroll->verticalScrollBar()->triggerAction(
        direction < 0 ? QAbstractSlider::SliderSingleStepSub
                      : QAbstractSlider::SliderSingleStepAdd);
}

void TutorialPane::copySelection()
{
    if (m_selGroup)
        m_selGroup->copy();
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
    zoom = qBound(kFontZoomMin, zoom, kFontZoomMax);
    if (zoom == m_userZoom)
        return;
    m_userZoom = zoom;
    applySizing();
}

// Content metrics: the DPI scale the whole GUI uses, times the pane's own
// text zoom. Rounded through the DPI helper first so the 1x case is
// bit-identical to the plain ScaleWidthForDPI these replaced.
int TutorialPane::sx(int px) const
{
    return UiScale(m_fontScale).x(px);
}

int TutorialPane::sy(int px) const
{
    return UiScale(m_fontScale).y(px);
}

void TutorialPane::applySizing()
{
    m_fontScale = FontZoomFactor(m_userZoom);
    applyShellSizing();
    applyTheme();
    // Column widths, card padding and dial geometry are all computed while a
    // page is being built, so restyling alone leaves them at the old zoom's
    // measurements — rebuild the page to pick the new ones up.
    redisplayCurrentPage();
    emit zoomChanged(m_userZoom);
}

// Margins and spacing on the long-lived containers (built once in the ctor,
// so they miss the per-page rebuild).
void TutorialPane::applyShellSizing()
{
    const int inset = sx(22);
    m_column->setContentsMargins(inset, sy(18), inset, sy(26));
    m_column->setSpacing(sy(12));
}

// Rebuild the visible page from its stored source. Dial values and the
// scroll offset survive the round trip so a zoom step reads as the text
// growing, not as the page resetting under the reader.
void TutorialPane::redisplayCurrentPage()
{
    if (m_pageKind == PageKind::None)
        return;

    // Scroll as a fraction: the rebuilt page is a different height, so the
    // raw offset would land somewhere unrelated. Only sample it when no
    // restore is already outstanding — mid-flight the bar reads 0 (endPage
    // reset it), so a second zoom step would capture that and lose the place.
    QScrollBar* bar = m_scroll->verticalScrollBar();
    if (m_pendingScrollFrac < 0.0)
        m_pendingScrollFrac = bar->maximum() > 0
                                  ? double(bar->value()) / double(bar->maximum())
                                  : 0.0;
    QHash<QString, double> dialValues;
    for (TutDial* dial : m_dials)
        dialValues.insert(dial->optName(), dial->value());
    const int octave = m_octave;
    // Anything mid-run keeps running: the rebuild is deterministic for a given
    // page, so job ids reattach by snippet index and the stop buttons — and
    // runEnded — still find their snippet.
    QVector<int> jobIds;
    QVector<QString> workspaces;
    for (const Snippet& snippet : m_snippets)
    {
        jobIds.append(snippet.jobId);
        workspaces.append(snippet.workspace);
    }

    // The show* calls below re-store their argument into the very member it
    // came from, so hand them copies rather than aliases of pane state.
    m_redisplaying = true;
    switch (m_pageKind)
    {
    case PageKind::Chapter:
        rebuild();
        break;
    case PageKind::Example:
        showExamplePage(QString(m_exampleKey), QString(m_exampleTitle), QString(m_exampleCode),
                        QString(m_exampleBlurb));
        break;
    case PageKind::Instrument:
        showInstrumentPage(m_pageIsFx, SonicPi::InstrumentPage(m_instrumentPage));
        break;
    case PageKind::SampleGroup:
        showSampleGroupPage(SonicPi::SampleGroup(m_sampleGroupPage));
        break;
    case PageKind::Lang:
        showLangPage(SonicPi::LangPage(m_langPage));
        break;
    case PageKind::None:
        m_redisplaying = false;
        return;
    }
    m_redisplaying = false;

    for (int i = 0; i < m_snippets.size() && i < jobIds.size(); i++)
    {
        if (jobIds[i] < 0)
            continue;
        m_snippets[i].jobId = jobIds[i];
        // The fresh build minted a new workspace name; keep the one the
        // running job was launched under so runEnded still matches.
        m_snippets[i].workspace = workspaces[i];
        setSnippetPlaying(m_snippets[i], true);
    }

    m_octave = octave;
    bool restored = false;
    for (TutDial* dial : m_dials)
    {
        auto it = dialValues.constFind(dial->optName());
        if (it != dialValues.constEnd() && !qFuzzyCompare(*it + 1.0, dial->value() + 1.0))
        {
            dial->setValue(*it, false);
            restored = true;
        }
    }
    if (restored)
        regenerateInstrumentCode();

    // After the layout has settled, or maximum() is still the old page's. One
    // restore in flight at a time; holding A+ coalesces onto the fraction
    // sampled before the first step, which is the position the reader had.
    // `this` as the context object cancels it if the pane goes away.
    if (!m_scrollRestoreQueued)
    {
        m_scrollRestoreQueued = true;
        QTimer::singleShot(0, this, [this]() {
            m_scrollRestoreQueued = false;
            const double frac = m_pendingScrollFrac;
            m_pendingScrollFrac = -1.0;
            if (frac < 0.0)
                return;
            QScrollBar* bar = m_scroll->verticalScrollBar();
            bar->setValue(qRound(frac * bar->maximum()));
        });
    }
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
    QColor zebraTint = SonicPiTheme::blend(bg, fg, 0.10);
    QColor zebraHover = SonicPiTheme::blend(bg, fg, 0.16);

    if (m_zoomBar)
        m_zoomBar->applyTheme();

    // Design sizes at the default editor zoom, scaled to track it
    auto pt = [this](int base) {
        return QString::number(qMax(6, qRound(base * m_fontScale))) + "pt";
    };

    // Soft accent hairline under H1s: mostly-background so it reads as a
    // rule, not another banner.
    QColor h1Rule = SonicPiTheme::blend(bg, accent, 0.45);

    QString qss = QStringLiteral(
        "#tutorialPane, #tutorialContent { background:@bg; }"
        "#tutProse { color:@fg; font-size:@proseSize; background:transparent; }"
        "#tutH1 { background:transparent; color:@accent; font-size:@h1Size; font-weight:bold;"
        " padding-bottom:6dx; border-bottom:2dx solid @h1Rule; }"
        "#tutH2 { background:transparent; color:@h2; font-size:@h2Size; font-weight:bold; }"
        "#tutH3 { color:@fg; font-size:@proseSize; font-weight:bold; }"
        "#tutCode { color:@fg; font-family:'Hack'; font-size:@codeSize; background:transparent; }"
        "#tutCodeFrame { background:@editorBg; border:1dx solid @accent; border-radius:@radiusMedium; }"
        // Display-only code: quiet neutral card — accent borders mean playable.
        "#tutCodeFrame[runnable=\"false\"] { border:1dx solid rgba(127,127,127,50);"
        " background:rgba(127,127,127,10); }"
        "#tutCodeFrame[playing=\"true\"] { border:1dx solid @accent; background:@playingTint; }"
        // Nested in the playground: a recessed text-area region, smaller code.
        "#tutCodeFrame[nested=\"true\"] { background:rgba(127,127,127,18);"
        " border:1dx solid rgba(127,127,127,60); border-radius:@radiusMedium; }"
        // Playing: the border lights accent; the background stays put so the
        // code doesn't flood with colour while it runs.
        "#tutCodeFrame[nested=\"true\"][playing=\"true\"] { border:1dx solid @accent;"
        " background:rgba(127,127,127,18); }"
        "#tutCodeFrame[nested=\"true\"] #tutCode { font-size:@codeSmall; }"
        // Quiet neutral card matching the display-code grammar; the badge
        // carries the instrument's identity, not a coloured border.
        "#tutPlayground { background:@editorBg; border:1dx solid rgba(127,127,127,60);"
        " border-radius:@radiusMedium; }"
        "#tutPlateName { background:transparent; color:@fg; font-size:@h1Size;"
        " font-weight:bold; }"
        // Synth-panel sections: each dial group is its own quiet region.
        "#tutDialGroup { background:rgba(127,127,127,22); border:none; border-radius:@radiusMedium; }"
        "#tutSection { color:@muted; font-family:'Hack'; font-size:@hintSize;"
        " background:transparent; }"
        // Transport + octave + reset: flat tabler glyph buttons.
        "#tutPlay, #tutStop, #tutCopy, #tutOct, #tutReset { background:transparent;"
        " border:none; border-radius:@radiusSmall; padding:2dx; }"
        "#tutPlay:hover:!pressed, #tutStop:hover:!pressed, #tutCopy:hover:!pressed,"
        " #tutOct:hover:!pressed, #tutReset:hover:!pressed"
        " { background:@hoverTint; }"
        "#tutPlay:pressed, #tutStop:pressed, #tutCopy:pressed,"
        " #tutOct:pressed, #tutReset:pressed { background:@pressedTint; }"
        "#tutSig { color:@sigColour; font-family:'Hack'; font-size:@buttonSize; }"
        "#tutHint { color:@muted; font-family:'Hack'; font-size:@hintSize; }"
        "#tutOptName { color:@h2; font-family:'Hack'; font-size:@optNameSize; font-weight:bold;"
        " background:transparent; border:none; text-align:left; padding:0dx; }"
        "#tutOptName:hover { color:@accent; text-decoration:underline; }"
        "#tutOptDefault { color:@sigColour; font-family:'Hack'; font-size:@optNameSize;"
        " background:transparent; }"
        // Zebra rows keep each opt's doc visually tied to its name. The tint
        // is a fg/bg blend, not a fixed grey — a grey wash vanishes against
        // near-black themes. Hover adds a stronger neutral wash so the row
        // under the pointer reads as one unit while scanning.
        "#tutOptRow { background:transparent; border:none; border-radius:@radiusSmall; }"
        "#tutOptRow[alt=\"true\"] { background:@zebraTint; }"
        "#tutOptRow:hover, #tutOptRow[alt=\"true\"]:hover { background:@zebraHover; }"
        // Opt quick-index: a quiet panel of name-links with their defaults.
        "#tutOptIndex { background:rgba(127,127,127,16); border:none;"
        " border-radius:@radiusMedium; }"
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
        // The shared corner-radius scale (dpi.h), so the pane's frames and
        // buttons curve exactly like the rest of the app instead of carrying
        // their own near-miss values.
        { "@radiusMedium", QString::number(kRadiusMediumDx) + "dx" },
        { "@radiusSmall", QString::number(kRadiusSmallDx) + "dx" },
        { "@pressedTint", pressedTint.name() },
        { "@playingTint", playingTint.name() },
        { "@editorBg", editorBg.name() },
        { "@hoverTint", hoverTint.name() },
        { "@zebraTint", zebraTint.name() },
        { "@zebraHover", zebraHover.name() },
        { "@sigColour", sigColour.name() },
        { "@proseSize", pt(13) },
        { "@buttonSize", pt(10) },
        { "@optNameSize", pt(12) },
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
    // dx metrics ride the pane zoom too, so card padding and border radii keep
    // their proportion to the type rather than pinching it at large sizes.
    setStyleSheet(ScalePxInStyleSheet(qss, m_fontScale));

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
    m_codeColours = SonicPi::codeColours(m_theme);

    // Tabler transport glyphs, matching the title-bar controls' icon family.
    const qreal dpr = devicePixelRatioF();
    // Same transport badges as the quickstart cards: filled glyph on a disc
    // (accent for play; foreground for stop, which greys out when disabled).
    // The discs are displayed at two sizes (standard snippets and the
    // playground's hero trigger row), so bake a pixmap for each — letting
    // QIcon upscale the smaller render blurs on high-DPI screens.
    auto discIcon = [this, dpr](TablerIcons::Glyph glyph, const QColor& disc) {
        QIcon icon;
        for (int side : { 30, 44 })
            icon.addPixmap(TablerIcons::discBadge(glyph, disc, m_theme->contrastingText(disc),
                                                  sx(side), dpr));
        return icon;
    };
    m_playIcon = discIcon(TablerIcons::Glyph::PlayFilled, accent);
    m_stopIcon = discIcon(TablerIcons::Glyph::StopFilled, fg);
    m_copyIcon = TablerIcons::icon(TablerIcons::Glyph::Copy, muted, sx(24), dpr);
    m_copiedIcon = TablerIcons::icon(TablerIcons::Glyph::Check, accent, sx(24), dpr);
    // Re-tint the per-page glyph icons (Reset / octave −+) for the new theme.
    for (QPushButton* b : m_content->findChildren<QPushButton*>("tutReset"))
        b->setIcon(TablerIcons::icon(TablerIcons::Glyph::Restore, muted, sx(20), dpr));
    for (QPushButton* b : m_content->findChildren<QPushButton*>("tutOct"))
        b->setIcon(TablerIcons::icon(b->property("octDir").toInt() < 0
                                         ? TablerIcons::Glyph::CircleMinus
                                         : TablerIcons::Glyph::CirclePlus,
                                     muted, sx(26), dpr));
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.codeView)
        {
            snippet.codeView->setHtml(SonicPi::TutorialDocs::highlightCode(snippet.code, m_codeColours));
            pinCodeViewHeight(snippet.codeView, snippet.code);
        }
        if (snippet.play)
            snippet.play->setIcon(m_playIcon);
        if (snippet.stop)
            snippet.stop->setIcon(m_stopIcon);
        if (snippet.copy)
            snippet.copy->setIcon(m_copyIcon);
    }

    QColor dialTrack = SonicPiTheme::blend(editorBg, fg, 0.28);
    for (TutDial* dial : m_dials)
    {
        dial->setUiScale(m_fontScale);
        dial->setColours(fg, muted, accent, dialTrack);
    }
    if (m_piano)
    {
        m_piano->setUiScale(m_fontScale);
        m_piano->setColours(fg, editorBg, accent, muted);
    }

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
    QSize size = QSize(sx(56), sy(32));
    qreal dpr = devicePixelRatioF();
    QPixmap pix(size * dpr);
    pix.fill(Qt::transparent);
    QPainter painter(&pix);
    renderer.render(&painter, QRectF(QPointF(0, 0), QSizeF(size * dpr)));
    painter.end();
    pix.setDevicePixelRatio(dpr);
    iconLabel->setPixmap(pix);
}

