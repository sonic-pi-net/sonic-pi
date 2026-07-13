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
#include "dpi.h"
#include "model/sonicpitheme.h"
#include "sonicpiscintilla.h"
#include "utils/instrument_icons.h"
#include "api/sonicpi_api.h"

#include <QAbstractTextDocumentLayout>
#include <QApplication>
#include <QClipboard>
#include <QTextCursor>
#include <QTextBlock>
#include <QTextDocument>
#include <QTextDocumentFragment>
#include <QGridLayout>
#include <QHBoxLayout>
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

void repolish(QWidget* w)
{
    w->style()->unpolish(w);
    w->style()->polish(w);
}

// Scope-buffer slot the jukebox taps into (slot 0 is the master scope). Must
// match the scope_num in the with_fx :scope_out wrap on the run side (mainwindow).
constexpr unsigned int kJukeboxScopeSlot = 1;

} // namespace

// Live oscilloscope on the Examples jukebox page. Reads an isolated scope-buffer
// slot (fed by a wrapping fx_scope_out tap), so it shows only the example's own
// audio while the main Scope dock keeps showing the full mix. Decorative:
// mouse-transparent, no focus, no accessible role (the Play/Stop button conveys
// the running state).
class TutScope : public QWidget
{
public:
    explicit TutScope(QWidget* parent = nullptr)
        : QWidget(parent)
    {
        setAttribute(Qt::WA_TransparentForMouseEvents, true);
        setFocusPolicy(Qt::NoFocus);
        setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        m_timer = new QTimer(this);
        connect(m_timer, &QTimer::timeout, this, [this]() { poll(); });
    }

    void setColours(const QColor& wave, const QColor& base, const QColor& panel,
                    const QColor& border)
    {
        m_wave = wave;
        m_base = base;
        m_panel = panel;
        m_border = border;
        update();
    }

    // Begin polling scope slot `scopeNum`; the reader is fetched fresh each
    // time so it survives a device swap between plays.
    void start(SonicPi::SonicPiAPI* api, unsigned int scopeNum)
    {
        m_reader = api ? api->AudioProcessor_GetScopeReader(scopeNum)
                       : shm_scope_buffer_reader();
        m_samples.clear();
        show();
        if (!m_timer->isActive())
            m_timer->start(30);
        update();
    }

    void stop()
    {
        m_timer->stop();
        m_reader = shm_scope_buffer_reader();
        m_samples.clear();
        hide();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const qreal w = width();
        const qreal h = height();
        const qreal mid = h / 2.0;
        const qreal radius = ScaleWidthForDPI(4);

        // Rounded panel so the scope reads as a distinct little display, even
        // when the trace is quiet or flat.
        QRectF panelRect(0.5, 0.5, w - 1.0, h - 1.0);
        if (m_panel.isValid())
        {
            p.setPen(Qt::NoPen);
            p.setBrush(m_panel);
            p.drawRoundedRect(panelRect, radius, radius);
        }

        // Clip the trace to the rounded panel so it never spills past the corners
        QPainterPath clip;
        clip.addRoundedRect(panelRect, radius, radius);
        p.setClipPath(clip);

        QColor base = m_base.isValid() ? m_base : palette().mid().color();
        QPen basePen(base);
        basePen.setWidthF(1.0);
        p.setPen(basePen);
        p.drawLine(QPointF(0, mid), QPointF(w, mid));

        if (m_samples.size() >= 2 && w >= 2)
        {
            QColor wave = m_wave.isValid() ? m_wave : palette().highlight().color();
            const qreal amp = mid * 0.92;
            const size_t n = m_samples.size();
            const int cols = qMax(2, (int)w);

            // The raw waveform, traced once and reused for the fill and stroke
            QPainterPath line;
            for (int x = 0; x < cols; x++)
            {
                size_t idx = (size_t)((qreal)x / (cols - 1) * (n - 1));
                qreal v = qBound(-1.0, (double)m_samples[idx], 1.0);
                qreal y = mid - v * amp;
                qreal px = (qreal)x / (cols - 1) * w;
                if (x == 0)
                    line.moveTo(px, y);
                else
                    line.lineTo(px, y);
            }

            // Fill back along the midline for a soft body under the stroke
            QPainterPath body = line;
            body.lineTo(w, mid);
            body.lineTo(0, mid);
            body.closeSubpath();
            QColor fill = wave;
            fill.setAlpha(70);
            p.fillPath(body, fill);

            QPen wavePen(wave);
            wavePen.setWidthF(3.0);
            wavePen.setJoinStyle(Qt::RoundJoin);
            wavePen.setCapStyle(Qt::RoundCap);
            p.setPen(wavePen);
            p.drawPath(line);
        }

        // Panel border, crisp on top (outside the clip)
        p.setClipping(false);
        if (m_border.isValid())
        {
            p.setPen(QPen(m_border, 1.0));
            p.setBrush(Qt::NoBrush);
            p.drawRoundedRect(panelRect, radius, radius);
        }
    }

private:
    void poll()
    {
        unsigned int frames = 0;
        if (!m_reader.pull(frames) || frames == 0)
            return;
        float* d = m_reader.data();
        if (!d)
            return;
        unsigned int stride = m_reader.max_frames();
        unsigned int ch = m_reader.channels();
        m_samples.resize(frames);
        for (unsigned int i = 0; i < frames; i++)
            m_samples[i] = ch >= 2 ? 0.5f * (d[i] + d[stride + i]) : d[i];
        update();
    }

    QColor m_wave;
    QColor m_base;
    QColor m_panel;
    QColor m_border;
    std::vector<float> m_samples;
    shm_scope_buffer_reader m_reader;
    QTimer* m_timer = nullptr;
};

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

    QHBoxLayout* bar = new QHBoxLayout();
    bar->setContentsMargins(ScaleWidthForDPI(6), ScaleHeightForDPI(4), ScaleWidthForDPI(6), 0);
    bar->setSpacing(ScaleWidthForDPI(2));
    bar->addStretch(1);
    m_zoomOut = new QPushButton("A-", this);
    m_zoomOut->setObjectName("tutZoom");
    m_zoomOut->setToolTip(tr("Decrease documentation text size"));
    m_zoomOut->setAccessibleName(tr("Decrease documentation text size"));
    m_zoomIn = new QPushButton("A+", this);
    m_zoomIn->setObjectName("tutZoom");
    m_zoomIn->setToolTip(tr("Increase documentation text size"));
    m_zoomIn->setAccessibleName(tr("Increase documentation text size"));
    connect(m_zoomOut, &QPushButton::clicked, this, [this]() {
        m_userZoom = qMax(m_userZoom - 1, -4);
        applySizing();
    });
    connect(m_zoomIn, &QPushButton::clicked, this, [this]() {
        m_userZoom = qMin(m_userZoom + 1, 8);
        applySizing();
    });
    bar->addWidget(m_zoomOut);
    bar->addWidget(m_zoomIn);
    outer->addLayout(bar);

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

    // Readable measure: cap the column width so long lines wrap instead of
    // spanning the whole (possibly very wide) pane.
    QWidget* columnWidget = new QWidget(m_content);
    columnWidget->setMaximumWidth(ScaleWidthForDPI(1200));
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
    m_exampleEditor->zoomTo(SonicPiScintilla::kDefaultZoom + m_userZoom);
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
        for (const SonicPi::CodeExample& example : page.examples)
            addSnippet(example.code, example.runnable);
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

    QString title = page.title.isEmpty() ? page.key : page.title;
    QLabel* titleLabel = new QLabel(title, m_content);
    titleLabel->setObjectName("tutH1");
    titleLabel->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_column->addWidget(titleLabel);
    {
        QLabel* icon = new QLabel(m_content);
        icon->setObjectName("tutFxIcon");
        icon->setProperty("fxname", page.key);
        icon->setProperty("isfx", isFx);
        icon->setAccessibleName(tr("%1 icon").arg(title));
        renderFxIcon(icon);
        m_column->addWidget(icon);
    }

    QLabel* sig = new QLabel((isFx ? "with_fx :" : "use_synth :") + page.key, m_content);
    sig->setObjectName("tutSig");
    sig->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_column->addWidget(sig);

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

    QWidget* dialsRow = new QWidget(m_content);
    dialsRow->setObjectName("tutDials");
    QVBoxLayout* dialRows = new QVBoxLayout(dialsRow);
    int dialPad = ScaleWidthForDPI(10);
    dialRows->setContentsMargins(dialPad, ScaleHeightForDPI(8), dialPad, ScaleHeightForDPI(6));
    dialRows->setSpacing(ScaleHeightForDPI(2));

    QColor dialFg = m_theme->color("Foreground");
    QColor dialBg = m_theme->color("Background");
    QColor dialAccent = m_theme->color("HighlightedBackground");
    QColor dialMuted = SonicPiTheme::blend(dialFg, dialBg, 0.38);
    QColor dialTrack = SonicPiTheme::blend(dialBg, dialFg, 0.18);
    auto onChange = [this]() { regenerateInstrumentCode(); };
    // Wrap into rows so every ranged opt gets a dial: a single row overflowed
    // the pane sideways, which silently hid everything past the first few
    // opts (e.g. :saw's cutoff).
    const int kDialsPerRow = 6, kMaxDials = 18;
    QHBoxLayout* dials = nullptr;
    for (const SonicPi::InstrumentOpt& opt : dialOpts)
    {
        if (m_dials.size() >= kMaxDials)
            break;
        if (!dials || m_dials.size() % kDialsPerRow == 0)
        {
            dials = new QHBoxLayout;
            dials->setContentsMargins(0, 0, 0, 0);
            dials->setSpacing(ScaleWidthForDPI(6));
            dials->addStretch(1);
            dialRows->addLayout(dials);
        }
        TutDial* dial = new TutDial(opt.name, opt.min, opt.max, opt.defaultNum, onChange, dialsRow);
        dial->setColours(dialFg, dialMuted, dialAccent, dialTrack);
        m_dials.append(dial);
        dials->insertWidget(dials->count() - 1, dial);
    }

    QPushButton* reset = new QPushButton(tr("Reset"), dialsRow);
    reset->setObjectName("tutReset");
    reset->setAccessibleName(tr("Reset %1 controls").arg(title));
    reset->setCursor(Qt::PointingHandCursor);
    connect(reset, &QPushButton::clicked, this, [this]() {
        for (TutDial* dial : m_dials)
            dial->reset(false);
        regenerateInstrumentCode();
    });
    if (dials)
        dials->addWidget(reset, 0, Qt::AlignBottom);

    if (m_dials.isEmpty())
        dialsRow->deleteLater();
    else
        m_column->addWidget(dialsRow);

    QWidget* pianoRow = new QWidget(m_content);
    QHBoxLayout* pianoLayout = new QHBoxLayout(pianoRow);
    pianoLayout->setContentsMargins(0, 0, 0, 0);
    pianoLayout->setSpacing(ScaleWidthForDPI(6));
    QPushButton* octDown = new QPushButton("-", pianoRow);
    octDown->setObjectName("tutReset");
    octDown->setToolTip(tr("Octave down (z)"));
    octDown->setAccessibleName(tr("Octave down"));
    octDown->setCursor(Qt::PointingHandCursor);
    connect(octDown, &QPushButton::clicked, this, [this]() { shiftOctave(-1); });
    QPushButton* octUp = new QPushButton("+", pianoRow);
    octUp->setObjectName("tutReset");
    octUp->setToolTip(tr("Octave up (x)"));
    octUp->setAccessibleName(tr("Octave up"));
    octUp->setCursor(Qt::PointingHandCursor);
    connect(octUp, &QPushButton::clicked, this, [this]() { shiftOctave(1); });
    m_piano = new TutPiano([this](int offset) { playKeyboardNote(offset); }, pianoRow);
    m_piano->setColours(dialFg, dialBg, dialAccent, dialMuted);
    m_octaveLabel = new QLabel(pianoRow);
    m_octaveLabel->setObjectName("tutSig");
    pianoLayout->addWidget(octDown, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_piano);
    pianoLayout->addWidget(octUp, 0, Qt::AlignVCenter);
    pianoLayout->addWidget(m_octaveLabel, 0, Qt::AlignVCenter);
    pianoLayout->addStretch(1);
    m_column->addWidget(pianoRow);
    shiftOctave(0);

    addSnippet(QString());
    regenerateInstrumentCode();

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
        if (dial->optName() == "note")
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
    int base = 52;
    for (TutDial* dial : m_dials)
        if (dial->optName() == "note")
            base = qRound(dial->value());
    QString note = QString::number(qBound(0, base + m_octave * 12 + semitoneOffset, 130));
    // use_real_time bypasses the default sched-ahead so keys sound instantly
    QString code = "use_real_time\nuse_debug false\n";
    if (m_pageIsFx)
    {
        code += "with_fx :" + m_pageName + instrumentOpts() + " do\n"
                "  use_synth :prophet\n"
                "  play " + note + ", release: 1\n"
                "end";
    }
    else
    {
        code += "use_synth :" + m_pageName + "\n"
                "play " + note + instrumentOpts();
    }
    // Untracked workspace: key notes shouldn't drive the demo's playing state
    emit runRequested(code, "sonic-pi-tutorial-keys", true);
    if (m_piano)
        m_piano->flash(semitoneOffset);
}

void TutorialPane::regenerateInstrumentCode()
{
    if (m_pageName.isEmpty() || m_snippets.isEmpty())
        return;

    QString opts;
    QString note = "52";
    for (TutDial* dial : m_dials)
    {
        if (dial->optName() == "note")
        {
            note = dial->valueText();
            continue;
        }
        if (!dial->isDefault())
            opts += ", " + dial->optName() + ": " + dial->valueText();
    }

    QString code;
    if (m_pageIsFx)
    {
        code = "with_fx :" + m_pageName + opts + " do\n"
               "  use_synth :prophet\n"
               "  play 50, release: 2, cutoff: 80\n"
               "  sleep 0.5\n"
               "end";
    }
    else
    {
        code = "use_synth :" + m_pageName + "\n"
               "play " + note + opts;
    }
    setSnippetCode(0, code);
}

void TutorialPane::setSnippetCode(int index, const QString& code)
{
    if (index >= m_snippets.size())
        return;
    Snippet& snippet = m_snippets[index];
    snippet.code = code;
    snippet.codeView->setHtml(SonicPi::TutorialDocs::highlightCode(code, m_codeColours));
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

void TutorialPane::addSnippet(const QString& code, bool runnable)
{
    Snippet snippet;
    snippet.code = code;
    snippet.workspace = QString("sonic-pi-tutorial-%1").arg(++m_workspaceSeq);

    snippet.frame = new QFrame(m_content);
    snippet.frame->setObjectName("tutCodeFrame");
    snippet.frame->setProperty("playing", false);
    QVBoxLayout* frameLayout = new QVBoxLayout(snippet.frame);
    int pad = ScaleWidthForDPI(10);
    frameLayout->setContentsMargins(pad, pad, pad, pad);
    frameLayout->setSpacing(ScaleHeightForDPI(4));

    // Same zero-margin text widget as prose, so the code's left edge lines up
    // exactly with the controls below it
    TutProseText* codeView = new TutProseText(snippet.frame);
    snippet.codeView = codeView;
    codeView->setObjectName("tutCode");
    codeView->setHtml(SonicPi::TutorialDocs::highlightCode(code, m_codeColours));
    codeView->setGroup(m_selGroup);
    m_selGroup->add(codeView);
    frameLayout->addWidget(codeView);

    QHBoxLayout* controls = new QHBoxLayout();
    controls->setContentsMargins(0, 0, 0, 0);
    controls->setSpacing(ScaleWidthForDPI(4));

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
    snippet.copy = new QPushButton(tr("Copy"), snippet.frame);
    snippet.copy->setObjectName("tutCopy");
    snippet.copy->setToolTip(tr("Copy code to clipboard"));
    snippet.copy->setAccessibleName(tr("Copy example %1").arg(snippetNum));

    QSize iconSize = ScaleForDPI(15, 15);
    QSize buttonSize = ScaleForDPI(28, 28);
    snippet.play->setIcon(m_playIcon);
    snippet.stop->setIcon(m_stopIcon);
    for (QPushButton* b : { snippet.play, snippet.stop })
    {
        b->setIconSize(iconSize);
        b->setFixedSize(buttonSize);
        b->setFlat(true);
        b->setCursor(Qt::PointingHandCursor);
    }
    snippet.copy->setCursor(Qt::PointingHandCursor);
    snippet.copy->setFixedHeight(buttonSize.height());
    // Syntax illustrations and output excerpts copy but don't play
    snippet.play->setVisible(runnable);
    snippet.stop->setVisible(runnable);

    controls->addWidget(snippet.play);
    controls->addWidget(snippet.stop);
    controls->addWidget(snippet.copy);
    controls->addStretch(1);
    frameLayout->addLayout(controls);

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
        s.copy->setText(tr("Copied"));
        QPushButton* button = s.copy;
        QTimer::singleShot(1200, button, [button]() { button->setText(tr("Copy")); });
    });

    m_snippets.append(snippet);
    m_column->addWidget(snippet.frame);
}

// Native option reference: name and default in code style, doc as prose
void TutorialPane::addOptsGrid(const QVector<SonicPi::InstrumentOpt>& opts)
{
    QWidget* grid = new QWidget(m_content);
    grid->setObjectName("tutOpts");
    QGridLayout* layout = new QGridLayout(grid);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setHorizontalSpacing(ScaleWidthForDPI(14));
    layout->setVerticalSpacing(ScaleHeightForDPI(6));
    layout->setColumnStretch(2, 1);

    int row = 0;
    for (const SonicPi::InstrumentOpt& opt : opts)
    {
        QLabel* name = new QLabel(opt.name + ":", grid);
        name->setObjectName("tutOptName");
        name->setTextInteractionFlags(Qt::TextSelectableByMouse);
        QLabel* def = new QLabel(opt.defaultText, grid);
        def->setObjectName("tutOptDefault");
        def->setTextInteractionFlags(Qt::TextSelectableByMouse);
        TutProseText* doc = new TutProseText(grid);
        doc->setObjectName("tutProse");
        QString docHtml = opt.doc.toHtmlEscaped();
        if (opt.slidable)
            docHtml += " <i>(" + tr("slidable").toHtmlEscaped() + ")</i>";
        doc->setProperty("mdtext", docHtml);
        doc->setHtml(proseColoured(docHtml));
        doc->setGroup(m_selGroup);
        m_selGroup->add(doc);
        m_proseLabels.append(doc);
        layout->addWidget(name, row, 0, Qt::AlignTop);
        layout->addWidget(def, row, 1, Qt::AlignTop);
        layout->addWidget(doc, row, 2);
        row++;
    }
    m_column->addWidget(grid);
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
                m_piano->flash(0);
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
        m_exampleEditor->zoomTo(SonicPiScintilla::kDefaultZoom + m_userZoom);
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
        out = "<p style=\"line-height:136%; margin:0;\">" + out + "</p>";
    return out;
}

void TutorialPane::applyTheme()
{
    QColor bg = m_theme->color("PaneBackground");
    QColor editorBg = m_theme->color("Background");
    QColor fg = m_theme->color("Foreground");
    QColor accent = m_theme->color("HighlightedBackground");
    QColor h2 = m_theme->color("NumberForeground");
    QColor link = m_theme->color("KeywordForeground");

    QColor muted = SonicPiTheme::blend(fg, editorBg, 0.38);
    QColor hoverTint = SonicPiTheme::blend(editorBg, accent, 0.25);
    QColor pressedTint = SonicPiTheme::blend(editorBg, accent, 0.45);
    QColor playingTint = SonicPiTheme::blend(editorBg, accent, 0.16);
    QColor sigColour = SonicPiTheme::blend(fg, editorBg, 0.18);

    // Design sizes at the default editor zoom, scaled to track it
    auto pt = [this](int base) {
        return QString::number(qMax(6, qRound(base * m_fontScale))) + "pt";
    };

    // Every marker must stay used, in gap-free order, or QString::arg
    // shifts all later substitutions.
    // %1 bg %2 fg %3 accent %4 accentText %5 h2 %6 h2Text %7 editorBg
    // %8 muted %9 hoverTint %10 pressedTint %11 playingTint %12 sigColour
    // %13 h1Size %14 proseSize %15 buttonSize %16 navSize %17 h2Size
    QString qss = QString(
        "#tutorialPane, #tutorialContent { background:%1; }"
        "#tutProse { color:%2; font-size:%14; background:transparent; }"
        "#tutH1 { background:%3; color:%4; font-size:%13; font-weight:bold;"
        " padding-top:5dx; padding-bottom:5dx; padding-left:10dx; padding-right:10dx;"
        " border-radius:4dx; }"
        "#tutH2 { background:%5; color:%6; font-size:%17; font-weight:bold;"
        " padding-top:4dx; padding-bottom:4dx; padding-left:10dx; padding-right:10dx;"
        " border-radius:4dx; }"
        "#tutH3 { color:%2; font-size:%14; font-weight:bold; }"
        "#tutCode { color:%2; font-family:'Hack'; font-size:%14; background:transparent; }"
        "#tutCodeFrame { background:%7; border:1dx solid %3; border-radius:8dx; }"
        "#tutCodeFrame[playing=\"true\"] { border:1dx solid %3; background:%11; }"
        "#tutPlay, #tutStop { background:transparent; border:none; border-radius:5dx; }"
        "#tutPlay:hover:!pressed, #tutStop:hover:!pressed { background:%9; }"
        "#tutPlay:pressed, #tutStop:pressed { background:%10; }"
        "#tutCopy, #tutReset { background:transparent; color:%8; border:none;"
        " border-radius:4dx; padding-top:3dx; padding-bottom:3dx;"
        " padding-left:8dx; padding-right:8dx; font-size:%15; }"
        "#tutCopy:hover, #tutReset:hover { color:%3; }"
        "#tutSig { color:%12; font-family:'Hack'; font-size:%15; }"
        "#tutOptName { color:%5; font-family:'Hack'; font-size:%15; font-weight:bold; }"
        "#tutOptDefault { color:%12; font-family:'Hack'; font-size:%15; }"
        "#tutDials { background:transparent; border:none; }"
        "#tutZoom { background:transparent; color:%8; border:none;"
        " border-radius:4dx; padding-top:2dx; padding-bottom:2dx;"
        " padding-left:8dx; padding-right:8dx; font-size:%15; font-weight:bold; }"
        "#tutZoom:hover { color:%3; }"
        "#tutNav { background:transparent; color:%8; border:none;"
        " text-decoration:underline; font-size:%16; padding:4dx; }"
        "#tutNav:hover { color:%3; }")
        .arg(bg.name(), fg.name(), accent.name(),
             m_theme->contrastingText(accent).name(), h2.name(),
             m_theme->contrastingText(h2).name(), editorBg.name(), muted.name())
        .arg(hoverTint.name(), pressedTint.name(), playingTint.name(), sigColour.name())
        .arg(pt(22), pt(15), pt(11), pt(11), pt(18));
    setStyleSheet(ScalePxInStyleSheet(qss));

    // Jukebox transport buttons: a bold accent-filled Play/Stop and an outlined
    // Load, both prominent so they're easy to spot (styled on the frame so both
    // children pick it up; ScalePxInStyleSheet needs per-side padding).
    QColor accentHover = SonicPiTheme::blend(accent, fg, 0.14);
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
    // Crisp painted transport glyphs: accent triangle, foreground square
    auto glyph = [this](bool isPlay, const QColor& colour) {
        qreal dpr = devicePixelRatioF();
        int px = ScaleWidthForDPI(15);
        QPixmap pm(QSize(px, px) * dpr);
        pm.fill(Qt::transparent);
        QPainter gp(&pm);
        gp.setRenderHint(QPainter::Antialiasing);
        gp.scale(dpr, dpr);
        gp.setPen(Qt::NoPen);
        gp.setBrush(colour);
        if (isPlay)
        {
            QPainterPath path;
            path.moveTo(px * 0.24, px * 0.10);
            path.lineTo(px * 0.92, px * 0.5);
            path.lineTo(px * 0.24, px * 0.90);
            path.closeSubpath();
            gp.drawPath(path);
        }
        else
        {
            gp.drawRoundedRect(QRectF(px * 0.18, px * 0.18, px * 0.64, px * 0.64),
                               px * 0.1, px * 0.1);
        }
        gp.end();
        pm.setDevicePixelRatio(dpr);
        return QIcon(pm);
    };
    m_codeColours.keyword = m_theme->color("KeywordForeground").name();
    m_codeColours.symbol = m_theme->color("SymbolForeground").name();
    m_codeColours.number = m_theme->color("NumberForeground").name();
    m_codeColours.string = m_theme->color("DoubleQuotedStringForeground").name();
    m_codeColours.comment = m_theme->color("CommentForeground").name();

    m_playIcon = glyph(true, accent);
    m_stopIcon = glyph(false, fg);
    // The jukebox transport button is accent-filled, so its glyphs are drawn in
    // the contrasting colour rather than the accent used on the flat snippets.
    QColor exGlyph = m_theme->contrastingText(accent);
    m_exPlayIcon = glyph(true, exGlyph);
    m_exStopIcon = glyph(false, exGlyph);
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.codeView)
            snippet.codeView->setHtml(
                SonicPi::TutorialDocs::highlightCode(snippet.code, m_codeColours));
        // The example snippet's play button is the jukebox transport, themed
        // just below; its stop pointer is null (single toggle button).
        if (snippet.play && snippet.play != m_examplePlay)
            snippet.play->setIcon(m_playIcon);
        if (snippet.stop)
            snippet.stop->setIcon(m_stopIcon);
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

    QColor dialTrack = SonicPiTheme::blend(editorBg, fg, 0.18);
    for (TutDial* dial : m_dials)
        dial->setColours(fg, muted, accent, dialTrack);
    if (m_piano)
        m_piano->setColours(fg, editorBg, accent, muted);

    const QList<QLabel*> icons = m_content->findChildren<QLabel*>("tutFxIcon");
    for (QLabel* iconLabel : icons)
        renderFxIcon(iconLabel);
}

void TutorialPane::renderFxIcon(QLabel* iconLabel)
{
    // FX icons in the reference blue, synths in the accent pink — the
    // colour split tau-state's sets were drawn in
    bool isFx = iconLabel->property("isfx").toBool();
    QColor colour = m_theme->color(isFx ? "NumberForeground" : "HighlightedBackground");
    QSvgRenderer renderer(
        instrumentIconSvg(isFx, iconLabel->property("fxname").toString(), colour).toUtf8());
    QSize size = ScaleForDPI(144, 81);
    qreal dpr = devicePixelRatioF();
    QPixmap pix(size * dpr);
    pix.fill(Qt::transparent);
    QPainter painter(&pix);
    renderer.render(&painter, QRectF(QPointF(0, 0), QSizeF(size * dpr)));
    painter.end();
    pix.setDevicePixelRatio(dpr);
    iconLabel->setPixmap(pix);
}

