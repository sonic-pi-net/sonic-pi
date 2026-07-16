//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sonicpierrorcard.h"
#include "dpi.h"
#include "model/sonicpitheme.h"

#include <QLabel>
#include <QPushButton>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QRegularExpression>
#include <QPainter>
#include <QPainterPath>
#include <QPaintEvent>
#include <QGuiApplication>
#include <QClipboard>
#include <QTimer>

#include "sonicpierrorcodeline.h"

namespace
{
QColor blend(const QColor& a, const QColor& b, double t)
{
    return QColor(qRound(a.red() * (1 - t) + b.red() * t),
                  qRound(a.green() * (1 - t) + b.green() * t),
                  qRound(a.blue() * (1 - t) + b.blue() * t));
}
} // namespace

SonicPiErrorCard::SonicPiErrorCard(SonicPiTheme* theme, QWidget* parent)
    : QFrame(parent)
    , m_theme(theme)
{
    setObjectName("errorCard");

    setFocusPolicy(Qt::StrongFocus);

    // Outer margins inset the visible card from the window edges so it floats.
    QVBoxLayout* outer = new QVBoxLayout(this);
    outer->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(8),
                              ScaleWidthForDPI(12), ScaleHeightForDPI(12));
    outer->setSpacing(0);

    // Everything lives in one rounded frame; the accent is a soft border. Header
    // and rows share this single layout so their left edges align by construction.
    QFrame* cardFrame = new QFrame(this);
    cardFrame->setObjectName("errCardFrame");
    QVBoxLayout* bodyV = new QVBoxLayout(cardFrame);
    bodyV->setContentsMargins(ScaleWidthForDPI(18), ScaleHeightForDPI(14),
                              ScaleWidthForDPI(18), ScaleHeightForDPI(14));
    bodyV->setSpacing(ScaleHeightForDPI(4));

    m_header = new QLabel(cardFrame);
    m_header->setObjectName("errHeader");
    m_header->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    m_header->setTextFormat(Qt::PlainText);

    int closeW = ScaleWidthForDPI(24);
    m_close = new QPushButton(QString::fromUtf8("\xE2\x9C\x95"), cardFrame);  // ✕
    m_close->setObjectName("errClose");
    m_close->setFlat(true);
    m_close->setFixedWidth(closeW);
    m_close->setCursor(Qt::PointingHandCursor);
    m_close->setFocusPolicy(Qt::NoFocus);
    m_close->setToolTip(tr("Close (Esc)"));
    connect(m_close, &QPushButton::clicked, this, [this] { emit closeRequested(); });

    // Title row: the "Runtime Error" label on the left, close button top-right.
    QHBoxLayout* headerRow = new QHBoxLayout;
    headerRow->setContentsMargins(0, 0, 0, 0);
    headerRow->addWidget(m_header, 1);
    headerRow->addWidget(m_close, 0, Qt::AlignVCenter);

    m_message = new QLabel(cardFrame);
    m_message->setObjectName("errMessage");
    m_message->setWordWrap(true);
    m_message->setTextFormat(Qt::RichText);
    m_message->setTextInteractionFlags(Qt::TextSelectableByMouse);

    m_location = new QLabel(cardFrame);
    m_location->setObjectName("errLocation");
    m_location->setTextFormat(Qt::RichText);
    m_location->setToolTip(tr("The buffer (tab) and line where the error happened"));

    m_reason = new QLabel(cardFrame);
    m_reason->setObjectName("errReason");
    m_reason->setWordWrap(true);
    m_reason->setTextFormat(Qt::RichText);
    m_reason->setTextInteractionFlags(Qt::TextSelectableByMouse);

    m_codeFrame = new QFrame(cardFrame);
    m_codeFrame->setObjectName("errCodeFrame");
    QVBoxLayout* codeV = new QVBoxLayout(m_codeFrame);
    codeV->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(16),
                              ScaleWidthForDPI(12), ScaleHeightForDPI(16));
    m_code = new SonicPiErrorCodeLine(m_codeFrame);
    m_code->setObjectName("errCode");
    m_code->setToolTip(tr("The line of your code that caused the error"));
    codeV->addWidget(m_code);

    // The usage illustration as its own callout: an accent-tinted strip with
    // a quiet "For example:" lead-in and the canonical example in plain code.
    // Reads as a general how-it's-used note from Sonic Pi — not a correction
    // of the user's specific values, and never confusable with the offending
    // line above it.
    m_hintFrame = new QFrame(cardFrame);
    m_hintFrame->setObjectName("errHintFrame");
    QHBoxLayout* hintH = new QHBoxLayout(m_hintFrame);
    hintH->setContentsMargins(ScaleWidthForDPI(12), ScaleHeightForDPI(7),
                              ScaleWidthForDPI(12), ScaleHeightForDPI(7));
    hintH->setSpacing(ScaleWidthForDPI(10));
    // Both the lead-in and the example code are docs links: the whole strip
    // is one affordance, clicking anywhere meaningful opens the failing fn's
    // help page.
    auto openDocs = [this] {
        if (!m_docsFn.isEmpty())
            emit docsRequested(m_docsFn);
    };
    m_hintChip = new QPushButton(tr("Doc Example:"), m_hintFrame);
    m_hintChip->setObjectName("errHintChip");
    m_hintChip->setFlat(true);
    m_hintChip->setCursor(Qt::PointingHandCursor);
    m_hintChip->setFocusPolicy(Qt::NoFocus);
    connect(m_hintChip, &QPushButton::clicked, this, openDocs);
    m_hintCode = new QPushButton(m_hintFrame);
    m_hintCode->setObjectName("errHintCode");
    m_hintCode->setFlat(true);
    m_hintCode->setCursor(Qt::PointingHandCursor);
    m_hintCode->setFocusPolicy(Qt::NoFocus);
    connect(m_hintCode, &QPushButton::clicked, this, openDocs);
    hintH->addWidget(m_hintChip, 0, Qt::AlignVCenter);
    hintH->addWidget(m_hintCode, 0, Qt::AlignVCenter);
    hintH->addStretch(1);
    m_hintFrame->hide();

    m_backtraceScroll = new QScrollArea(cardFrame);
    m_backtraceScroll->setObjectName("errBacktraceScroll");
    m_backtraceScroll->setWidgetResizable(true);
    m_backtraceScroll->setFrameShape(QFrame::NoFrame);
    m_backtraceScroll->setMaximumHeight(ScaleHeightForDPI(150));
    m_backtrace = new QLabel(m_backtraceScroll);
    m_backtrace->setObjectName("errBacktrace");
    m_backtrace->setFont(QFont("Hack"));
    m_backtrace->setTextFormat(Qt::PlainText);
    m_backtrace->setAlignment(Qt::AlignTop | Qt::AlignLeft);
    m_backtrace->setTextInteractionFlags(Qt::TextSelectableByMouse);
    m_backtraceScroll->setWidget(m_backtrace);
    m_backtraceScroll->hide();

    m_details = new QPushButton(tr("Show details"), cardFrame);
    m_details->setObjectName("errDetails");
    m_details->setCursor(Qt::PointingHandCursor);
    m_details->setFlat(true);
    m_details->setFocusPolicy(Qt::NoFocus);
    connect(m_details, &QPushButton::clicked, this, [this] { setDetailsVisible(!m_detailsOn); });

    // "Copy" shares the details link's objectName so the #errDetails
    // stylesheet rule styles both.
    m_copy = new QPushButton(tr("Copy"), cardFrame);
    m_copy->setObjectName("errDetails");
    m_copy->setCursor(Qt::PointingHandCursor);
    m_copy->setFlat(true);
    m_copy->setFocusPolicy(Qt::NoFocus);
    m_copy->setToolTip(tr("Copy the whole error report (message, location, code and backtrace) to the clipboard"));
    m_copy->setAccessibleName(tr("Copy error report"));
    connect(m_copy, &QPushButton::clicked, this, [this] {
        QGuiApplication::clipboard()->setText(clipboardText());
        m_copy->setText(tr("Copied ✓"));
        QTimer::singleShot(1500, m_copy, [this] { m_copy->setText(tr("Copy")); });
    });

    m_jump = new QPushButton(cardFrame);
    m_jump->setObjectName("errJump");
    m_jump->setCursor(Qt::PointingHandCursor);
    m_jump->setToolTip(tr("Move the cursor to the error in your code"));
    connect(m_jump, &QPushButton::clicked, this, [this] { emit jumpRequested(); });

    // Bottom row: "Show details" + "Copy" on the left; the location sits next
    // to the primary jump button on the right.
    QHBoxLayout* actions = new QHBoxLayout;
    actions->setContentsMargins(0, ScaleHeightForDPI(6), 0, 0);
    actions->addWidget(m_details, 0, Qt::AlignLeft | Qt::AlignVCenter);
    actions->addSpacing(ScaleWidthForDPI(14));
    actions->addWidget(m_copy, 0, Qt::AlignLeft | Qt::AlignVCenter);
    actions->addStretch(1);
    actions->addWidget(m_location, 0, Qt::AlignVCenter);
    actions->addSpacing(ScaleWidthForDPI(12));
    actions->addWidget(m_jump, 0, Qt::AlignVCenter);

    // Reading order = priority order: what happened, the offending code, the
    // doc example, and only then the raw technical error (muted, lowest).
    bodyV->addLayout(headerRow);
    bodyV->addSpacing(ScaleHeightForDPI(10));   // gap between title and message
    bodyV->addWidget(m_message);
    bodyV->addSpacing(ScaleHeightForDPI(10));   // let the code box breathe
    bodyV->addWidget(m_codeFrame);
    bodyV->addSpacing(ScaleHeightForDPI(8));
    bodyV->addWidget(m_hintFrame);
    bodyV->addSpacing(ScaleHeightForDPI(4));
    bodyV->addWidget(m_reason);
    bodyV->addSpacing(ScaleHeightForDPI(8));
    bodyV->addWidget(m_backtraceScroll);
    bodyV->addLayout(actions);

    outer->addWidget(cardFrame);

    applyTheme();
}

void SonicPiErrorCard::setFontScale(double scale)
{
    scale = qBound(0.5, scale, 3.0);
    if (qFuzzyCompare(scale, m_fontScale))
        return;
    m_fontScale = scale;
    applyTheme();
}

void SonicPiErrorCard::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape)
    {
        emit closeRequested();
        event->accept();
        return;
    }
    QFrame::keyPressEvent(event);
}

void SonicPiErrorCard::applyTheme()
{
    QColor accent = m_theme->color(m_isSyntax ? "MarkerBackgroundSyntax" : "MarkerBackground");
    QColor editorBg = m_theme->color("Background");
    QColor fg = m_theme->color("Foreground");
    bool dark = editorBg.lightnessF() < 0.5;

    QColor cardBg = blend(editorBg, fg, dark ? 0.09 : 0.06);
    QColor codeBorder = blend(cardBg, fg, 0.18);
    QColor muted = m_theme->mutedForeground();
    QColor textColor = m_theme->softForeground();

    // House-style button: black fill, themed border/text, hover blue, pressed
    // pink. The fill is literal black by design, so it must go through the
    // global transforms by hand (color() isn't involved) or Invert leaves it
    // black under inverted-to-black text.
    QColor btnBg = m_theme->applyGlobalTransforms(QColor(Qt::black));
    QColor btnText = m_theme->color("ButtonText");
    QColor btnBorder = m_theme->color("ButtonBorder");
    QColor btnHover = m_theme->color("HoverButton");
    QColor btnPressed = m_theme->color("PressedButton");
    QColor btnHoverText = m_theme->contrastingText(btnHover);
    QColor btnPressedText = m_theme->contrastingText(btnPressed);

    // Font sizes are design sizes (at the default editor zoom) scaled by
    // m_fontScale so the card tracks the editor's zoom level.
    auto pt = [this](int base) {
        return QString::number(qMax(6, qRound(base * m_fontScale))) + "pt";
    };

    // %1 accent  %2 cardBg  %3 muted  %4 textColor  %5 editorBg  %6 codeBorder
    // %7 btnText %8 btnBorder %9 btnHover %10 btnPressed
    // %11 btnBg %12 btnHoverText %13 btnPressedText
    // %14-%21 scaled font sizes
    QString qss = QString(
        "#errCardFrame { background:%2; border:2px solid %1; border-radius:10px; }"
        "#errHeader { background:transparent; color:%1; font-size:%14; font-weight:bold; }"
        "#errClose { background:transparent; border:none; color:%3; font-size:%15;"
        " padding:0 2px; }"
        "#errClose:hover { color:%4; }"
        "#errMessage { color:%4; font-size:%16; font-weight:bold; }"
        "#errLocation { color:%3; font-size:%17; }"
        "#errReason { color:%3; font-size:%18; }"
        "#errCodeFrame { background:%5; border-radius:6px; border:1px solid %6; }"
        "#errBacktrace { color:%3; background:transparent; font-size:%19; }"
        "#errJump { background:%11; color:%7; border:2px solid %8;"
        " border-radius:3px; padding:5px 12px; font-size:%20; }"
        "#errJump:hover:!pressed { background:%9; color:%12; }"
        "#errJump:pressed { background:%10; color:%13; }"
        "#errDetails { background:transparent; border:none; color:%3;"
        " text-decoration:underline; font-size:%21; padding:0; text-align:left; }"
        "#errDetails:hover { color:%4; }"
        "#errHintFrame { background:%22; border:1px solid %23; border-radius:6px; }"
        "#errHintChip { background:transparent; color:%3; font-size:%24;"
        " border:none; padding:0; text-align:left; }"
        "#errHintChip:hover:enabled { color:%1; text-decoration:underline; }"
        "#errHintCode { background:transparent; color:%4; border:none; padding:0;"
        " text-align:left; font-family:'Hack','Courier New',monospace; font-size:%25; }"
        "#errHintCode:hover:enabled { color:%1; text-decoration:underline; }")
        .arg(accent.name(), cardBg.name(), muted.name(), textColor.name(), editorBg.name())
        .arg(codeBorder.name(), btnText.name(), btnBorder.name(), btnHover.name(), btnPressed.name())
        .arg(btnBg.name(), btnHoverText.name(), btnPressedText.name())
        .arg(pt(15), pt(13), pt(14), pt(10), pt(9), pt(10), pt(10), pt(9))
        .arg(blend(cardBg, accent, 0.10).name(), blend(cardBg, accent, 0.45).name())
        .arg(pt(10), pt(11));
    setStyleSheet(qss);

    QFont codeFont("Hack");
    codeFont.setPointSize(qMax(6, qRound(11 * m_fontScale)));
    m_code->setFont(codeFont);

    // The message's accent code-spans and the offending code line bake theme
    // colours into their content (not QSS), so re-render them here too.
    renderThemedContent();
}

void SonicPiErrorCard::renderThemedContent()
{
    QColor accent = m_theme->color(m_isSyntax ? "MarkerBackgroundSyntax" : "MarkerBackground");

    // Identifiers are backtick-marked (`name`); render them as coloured code
    // font instead of quotes, which read poorly next to contraction apostrophes.
    if (!m_messageText.isEmpty())
    {
        QString msgHtml = m_messageText.toHtmlEscaped();
        static const QRegularExpression reTok("`([^`]+)`");
        msgHtml.replace(reTok, "<span style=\"font-family:'Hack','Courier New',monospace; color:"
                        + accent.name() + ";\">\\1</span>");
        // Rich text swallows newlines; the server uses one for the
        // "Try: ..." hint line.
        msgHtml.replace("\n", "<br/>");
        m_message->setText(msgHtml);
    }

    // The offending span gets the full-strength foreground with the accent
    // zig-zag beneath; the rest of the line recedes into comment grey so the
    // error is the focus.
    if (!m_codeLine.isEmpty())
    {
        m_code->setContent(m_codeLine, m_colStart, m_colEnd, m_codeLineNumber,
                           m_theme->color("CommentForeground"),
                           m_theme->color("Foreground"), accent);
    }
}

void SonicPiErrorCard::showError(bool isSyntax,
                                 const QString& header,
                                 const QString& location,
                                 const QString& reason,
                                 const QString& codeLine,
                                 int lineNumber,
                                 int colStart, int colEnd,
                                 const QString& backtrace,
                                 bool canJump)
{
    m_isSyntax = isSyntax;

    // The accent-coloured "Runtime Error"/"Syntax Error" label is the centred
    // title; the friendly message that follows it becomes its own row.
    QString labelText = isSyntax ? QStringLiteral("Syntax Error") : QStringLiteral("Runtime Error");
    QString title = labelText;
    QString message;
    if (header.startsWith(labelText))
        message = header.mid(labelText.length()).trimmed();
    else
        title = header;
    m_headerPlain = header;
    m_headerPlain.remove('`');   // don't let the screen reader speak "backtick"
    m_header->setText(title);

    // Pull the server's structured trailer lines out of the message: the
    // "Try: ..." remedy and the "Docs: fn" link render as their own callout
    // below the code + reason, not as part of the headline.
    QString hint;
    m_docsFn.clear();
    {
        QStringList kept;
        const QStringList lines = message.split('\n');
        for (const QString& line : lines)
        {
            if (line.startsWith(QStringLiteral("Example: ")))
                hint = QString(line.mid(9)).remove('`').trimmed();
            else if (line.startsWith(QStringLiteral("Docs: ")))
                m_docsFn = line.mid(6).trimmed();
            else
                kept << line;
        }
        message = kept.join('\n').trimmed();
    }
    // The lead-in labels the click action, so it stays for the docs-only case
    // (e.g. a typo'd name), where the link is just the fn name.
    m_hintCode->setText(hint.isEmpty() && !m_docsFn.isEmpty() ? m_docsFn : hint);
    const QString docsTip = m_docsFn.isEmpty()
                                ? QString()
                                : tr("Open the documentation for %1").arg(m_docsFn);
    for (QPushButton* link : { m_hintChip, m_hintCode })
    {
        link->setEnabled(!m_docsFn.isEmpty());
        link->setCursor(m_docsFn.isEmpty() ? Qt::ArrowCursor : Qt::PointingHandCursor);
        link->setToolTip(docsTip);
    }
    m_hintCode->setAccessibleName(m_docsFn.isEmpty()
                                      ? tr("Usage example")
                                      : tr("Usage example - opens the %1 documentation").arg(m_docsFn));
    m_hintFrame->setVisible(!hint.isEmpty() || !m_docsFn.isEmpty());

    m_messageText = message;
    m_message->setVisible(!message.isEmpty());

    m_location->setText(location.toHtmlEscaped());
    m_location->setVisible(!location.isEmpty());
    m_reason->setText(reason.toHtmlEscaped());
    m_reason->setVisible(!reason.isEmpty());

    QString code = codeLine;
    while (code.endsWith('\n') || code.endsWith('\r'))
        code.chop(1);
    bool hasCode = !code.trimmed().isEmpty();
    m_codeFrame->setVisible(hasCode);
    m_codeLine = hasCode ? codeLine : QString();
    m_colStart = colStart;
    m_colEnd = colEnd;
    m_codeLineNumber = lineNumber;

    // Apply the themed QSS and paint the accent-coloured message spans + code
    // line from the values just stored (applyTheme re-runs renderThemedContent).
    applyTheme();

    m_jump->setText(tr("Jump to error"));
    m_jump->setVisible(canJump);

    m_backtrace->setText(backtrace);
    bool hasBt = !backtrace.trimmed().isEmpty();
    m_details->setVisible(hasBt);
    setDetailsVisible(false);

    show();
}

void SonicPiErrorCard::setDetailsVisible(bool on)
{
    m_detailsOn = on;
    m_backtraceScroll->setVisible(on);
    m_details->setText(on ? tr("Hide details") : tr("Show details"));
    m_details->setToolTip(on ? tr("Hide the full technical error and backtrace")
                             : tr("Show the full technical error and backtrace"));
}

QString SonicPiErrorCard::plainText() const
{
    QStringList parts;
    parts << m_headerPlain;
    if (m_location->isVisible())
        parts << m_location->text();
    if (m_reason->isVisible())
        parts << m_reason->text();
    return parts.join(". ");
}

QString SonicPiErrorCard::clipboardText() const
{
    QStringList parts;
    parts << m_headerPlain;
    if (m_location->isVisible())
        parts << m_location->text();
    if (m_reason->isVisible())
        parts << m_reason->text();
    if (!m_codeLine.isEmpty())
    {
        QString code = m_codeLine;
        while (code.endsWith('\n') || code.endsWith('\r'))
            code.chop(1);
        parts << (m_codeLineNumber >= 0
                      ? QStringLiteral("line %1: %2").arg(m_codeLineNumber).arg(code)
                      : code);
    }
    const QString bt = m_backtrace->text();
    if (!bt.trimmed().isEmpty())
        parts << QStringLiteral("Backtrace:\n") + bt;
    return parts.join("\n");
}
