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
    exampleControls->setSpacing(ScaleWidthForDPI(4));
    m_examplePlay = new QPushButton(m_exampleFrame);
    m_examplePlay->setObjectName("tutPlay");
    m_examplePlay->setToolTip(tr("Run this example"));
    m_examplePlay->setAccessibleName(tr("Run example"));
    m_exampleStop = new QPushButton(m_exampleFrame);
    m_exampleStop->setObjectName("tutStop");
    m_exampleStop->setToolTip(tr("Stop this example"));
    m_exampleStop->setAccessibleName(tr("Stop example"));
    m_exampleStop->setEnabled(false);
    m_exampleCopy = new QPushButton(tr("Copy"), m_exampleFrame);
    m_exampleCopy->setObjectName("tutCopy");
    m_exampleCopy->setToolTip(tr("Copy code to clipboard"));
    m_exampleCopy->setAccessibleName(tr("Copy example"));
    QSize exIconSize = ScaleForDPI(15, 15);
    QSize exButtonSize = ScaleForDPI(28, 28);
    for (QPushButton* b : { m_examplePlay, m_exampleStop })
    {
        b->setIconSize(exIconSize);
        b->setFixedSize(exButtonSize);
        b->setFlat(true);
        b->setCursor(Qt::PointingHandCursor);
    }
    m_exampleCopy->setCursor(Qt::PointingHandCursor);
    m_exampleCopy->setFixedHeight(exButtonSize.height());
    exampleControls->addWidget(m_examplePlay);
    exampleControls->addWidget(m_exampleStop);
    exampleControls->addWidget(m_exampleCopy);
    exampleControls->addStretch(1);
    m_exampleFrameLayout->addLayout(exampleControls);
    examplePage->addWidget(m_exampleFrame, 1);
    m_examplePage->hide();
    outer->addWidget(m_examplePage, 1);

    // The example page always occupies m_snippets[0] while visible
    connect(m_examplePlay, &QPushButton::clicked, this, [this]() {
        if (!m_snippets.isEmpty() && m_snippets[0].play == m_examplePlay)
            emit runRequested(m_snippets[0].code, m_snippets[0].workspace);
    });
    connect(m_exampleStop, &QPushButton::clicked, this, [this]() {
        if (!m_snippets.isEmpty() && m_snippets[0].play == m_examplePlay && m_snippets[0].jobId >= 0)
            emit stopJobRequested(m_snippets[0].jobId);
    });
    connect(m_exampleCopy, &QPushButton::clicked, this, [this]() {
        if (m_snippets.isEmpty() || m_snippets[0].play != m_examplePlay)
            return;
        QApplication::clipboard()->setText(m_snippets[0].code);
        m_exampleCopy->setText(tr("Copied"));
        QPushButton* button = m_exampleCopy;
        QTimer::singleShot(1200, button, [button]() { button->setText(tr("Copy")); });
    });

    applyTheme();
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
    m_exampleFrameLayout->insertWidget(0, m_exampleEditor, 1);
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
    snippet.stop = m_exampleStop;
    snippet.copy = m_exampleCopy;
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

    // Dials: the instrument's specific ranged opts first, then mix/amp for FX
    const QStringList commonFx = { "amp", "mix", "pre_mix", "pre_amp" };
    QVector<SonicPi::InstrumentOpt> dialOpts;
    for (const SonicPi::InstrumentOpt& opt : page.opts)
        if (opt.numeric && opt.hasRange && !(isFx && commonFx.contains(opt.name)))
            dialOpts.append(opt);
    if (isFx)
        for (const QString& name : { QString("mix"), QString("amp") })
            for (const SonicPi::InstrumentOpt& opt : page.opts)
                if (opt.name == name && opt.numeric && opt.hasRange)
                    dialOpts.append(opt);

    QWidget* dialsRow = new QWidget(m_content);
    dialsRow->setObjectName("tutDials");
    QHBoxLayout* dials = new QHBoxLayout(dialsRow);
    int dialPad = ScaleWidthForDPI(10);
    dials->setContentsMargins(dialPad, ScaleHeightForDPI(8), dialPad, ScaleHeightForDPI(6));
    dials->setSpacing(ScaleWidthForDPI(6));

    QColor dialFg = m_theme->color("Foreground");
    QColor dialBg = m_theme->color("Background");
    QColor dialAccent = m_theme->color("HighlightedBackground");
    QColor dialMuted = SonicPiTheme::blend(dialFg, dialBg, 0.38);
    QColor dialTrack = SonicPiTheme::blend(dialBg, dialFg, 0.18);
    auto onChange = [this]() { regenerateInstrumentCode(); };
    for (const SonicPi::InstrumentOpt& opt : dialOpts)
    {
        if (m_dials.size() >= 8)
            break;
        TutDial* dial = new TutDial(opt.name, opt.min, opt.max, opt.defaultNum, onChange, dialsRow);
        dial->setColours(dialFg, dialMuted, dialAccent, dialTrack);
        m_dials.append(dial);
        dials->addWidget(dial);
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
    dials->addStretch(1);
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
    for (Snippet& snippet : m_snippets)
    {
        if (snippet.codeView)
            snippet.codeView->setHtml(
                SonicPi::TutorialDocs::highlightCode(snippet.code, m_codeColours));
        snippet.play->setIcon(m_playIcon);
        snippet.stop->setIcon(m_stopIcon);
    }
    m_examplePlay->setIcon(m_playIcon);
    m_exampleStop->setIcon(m_stopIcon);
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

