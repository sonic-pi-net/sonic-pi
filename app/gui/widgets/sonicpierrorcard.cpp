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
    QFont codeFont("Hack");
    codeFont.setPointSize(11);
    m_code->setFont(codeFont);
    codeV->addWidget(m_code);

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

    m_jump = new QPushButton(cardFrame);
    m_jump->setObjectName("errJump");
    m_jump->setCursor(Qt::PointingHandCursor);
    m_jump->setToolTip(tr("Move the cursor to the error in your code"));
    connect(m_jump, &QPushButton::clicked, this, [this] { emit jumpRequested(); });

    // Bottom row: "Show details" on the left; the location sits next to the
    // primary jump button on the right.
    QHBoxLayout* actions = new QHBoxLayout;
    actions->setContentsMargins(0, ScaleHeightForDPI(6), 0, 0);
    actions->addWidget(m_details, 0, Qt::AlignLeft | Qt::AlignVCenter);
    actions->addStretch(1);
    actions->addWidget(m_location, 0, Qt::AlignVCenter);
    actions->addSpacing(ScaleWidthForDPI(12));
    actions->addWidget(m_jump, 0, Qt::AlignVCenter);

    bodyV->addLayout(headerRow);
    bodyV->addSpacing(ScaleHeightForDPI(10));   // gap between title and message
    bodyV->addWidget(m_message);
    bodyV->addWidget(m_reason);
    bodyV->addSpacing(ScaleHeightForDPI(10));   // let the code box breathe
    bodyV->addWidget(m_codeFrame);
    bodyV->addSpacing(ScaleHeightForDPI(8));
    bodyV->addWidget(m_backtraceScroll);
    bodyV->addLayout(actions);

    outer->addWidget(cardFrame);

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
    QColor muted = blend(fg, editorBg, 0.38);
    QColor textColor = blend(fg, editorBg, 0.08);

    // House-style button: black fill, themed border/text, hover blue, pressed pink.
    QColor btnText = m_theme->color("ButtonText");
    QColor btnBorder = m_theme->color("ButtonBorder");
    QColor btnHover = m_theme->color("HoverButton");
    QColor btnPressed = m_theme->color("PressedButton");

    // %1 accent  %2 cardBg  %3 muted  %4 textColor  %5 editorBg  %6 codeBorder
    // %7 btnText %8 btnBorder %9 btnHover %10 btnPressed
    QString qss = QString(
        "#errCardFrame { background:%2; border:2px solid %1; border-radius:10px; }"
        "#errHeader { background:transparent; color:%1; font-size:15pt; font-weight:bold; }"
        "#errClose { background:transparent; border:none; color:%3; font-size:13pt;"
        " padding:0 2px; }"
        "#errClose:hover { color:%4; }"
        "#errMessage { color:%4; font-size:14pt; font-weight:bold; }"
        "#errLocation { color:%3; font-size:10pt; }"
        "#errReason { color:%3; font-size:12pt; }"
        "#errCodeFrame { background:%5; border-radius:6px; border:1px solid %6; }"
        "#errBacktrace { color:%3; background:transparent; font-size:10pt; }"
        "#errJump { background:black; color:%7; border:2px solid %8;"
        " border-radius:3px; padding:5px 12px; font-size:10pt; }"
        "#errJump:hover:!pressed { background:%9; color:%7; }"
        "#errJump:pressed { background:%10; color:%7; }"
        "#errDetails { background:transparent; border:none; color:%3;"
        " text-decoration:underline; font-size:9pt; }"
        "#errDetails:hover { color:%4; }")
        .arg(accent.name(), cardBg.name(), muted.name(), textColor.name(), editorBg.name())
        .arg(codeBorder.name(), btnText.name(), btnBorder.name(), btnHover.name(), btnPressed.name());
    setStyleSheet(qss);
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
    applyTheme();

    QColor accent = m_theme->color(isSyntax ? "MarkerBackgroundSyntax" : "MarkerBackground");

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
    // Identifiers are backtick-marked (`name`); render them as coloured code
    // font instead of quotes, which read poorly next to contraction apostrophes.
    QString msgHtml = message.toHtmlEscaped();
    static const QRegularExpression reTok("`([^`]+)`");
    msgHtml.replace(reTok, "<span style=\"font-family:'Hack','Courier New',monospace; color:"
                    + accent.name() + ";\">\\1</span>");
    m_message->setText(msgHtml);
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
    if (hasCode)
    {
        // The offending span gets the full-strength foreground with the accent
        // zig-zag beneath; the rest of the line recedes into comment grey so
        // the error is the focus.
        m_code->setContent(codeLine, colStart, colEnd, lineNumber,
                           m_theme->color("CommentForeground"),
                           m_theme->color("Foreground"), accent);
    }

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
