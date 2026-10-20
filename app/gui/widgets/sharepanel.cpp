//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "sharepanel.h"

#include <QAbstractButton>
#include <QApplication>
#include <QClipboard>
#include <QDir>
#include <QEvent>
#include <QFileDialog>
#include <QFileInfo>
#include <QFocusEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QLineEdit>
#include <QPainter>
#include <QPainterPath>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QRegularExpression>
#include <QStackedWidget>
#include <QStyle>
#include <QTimer>
#include <QVBoxLayout>

#include <algorithm>

#include "model/sonicpitheme.h"
#include "utils/setbundle.h"
#include "utils/fontroles.h"
#include "utils/tablericons.h"

namespace ShareLink = SonicPi::ShareLink;
using TablerIcons::Glyph;

namespace {

// the web's measures (style.css: --r-m, the panel's 340px)
constexpr int Radius = 8;
constexpr int PanelWidth = 340;
constexpr int QrSide = 280;   // the code across the card, in the screen's own pixels
constexpr int ScanMin = 3;    // pixels a module needs for a phone's camera to read it off a screen: finer, Larger is lit
constexpr int Quiet = 4;      // the quiet zone around a code, in modules

enum Page { About, Code, Link };

// a rounded rect, its corners rounded only where asked
QPainterPath roundedPath(const QRectF& r, qreal radius, bool tl, bool tr, bool br, bool bl)
{
    QPainterPath p;
    p.moveTo(r.left() + (tl ? radius : 0), r.top());
    p.lineTo(r.right() - (tr ? radius : 0), r.top());
    if (tr) p.quadTo(r.right(), r.top(), r.right(), r.top() + radius);
    p.lineTo(r.right(), r.bottom() - (br ? radius : 0));
    if (br) p.quadTo(r.right(), r.bottom(), r.right() - radius, r.bottom());
    p.lineTo(r.left() + (bl ? radius : 0), r.bottom());
    if (bl) p.quadTo(r.left(), r.bottom(), r.left(), r.bottom() - radius);
    p.lineTo(r.left(), r.top() + (tl ? radius : 0));
    if (tl) p.quadTo(r.left(), r.top(), r.left() + radius, r.top());
    p.closeSubpath();
    return p;
}

// the panel's words in the house's roles (dpi.h FontRole), as the app's other panes: what is read or typed (what it
// is, the tabs' names, the fields, the four ways) at Base; its titles, uppercased and muted, and its counts and notes
// at Small
QFont sized(int weight = QFont::Normal, FontRole role = FontRole::Base)
{
    QFont f = RoleFont(role);
    f.setWeight(QFont::Weight(weight));
    return f;
}

QString plural(int n, const QString& one, const QString& many)
{
    return QStringLiteral("%1 %2").arg(n).arg(n == 1 ? one : many);
}

// A ring round what has the focus only when the keyboard put it there, as the web's :focus-visible
bool keyboardFocus(QFocusEvent* e)
{
    return e->reason() == Qt::TabFocusReason || e->reason() == Qt::BacktabFocusReason || e->reason() == Qt::ShortcutFocusReason;
}

// What to share: a tab, its glyph, its name and what it holds; the chosen one in the accent (.sm-what-opt)
class Tab : public QAbstractButton
{
public:
    Tab(Glyph glyph, const QString& name, const QString& summary, SonicPiTheme* theme)
        : m_glyph(glyph), m_summary(summary), m_theme(theme)
    {
        setText(name);
        setCheckable(true);
        setAutoExclusive(true);
        setCursor(Qt::PointingHandCursor);
        setFocusPolicy(Qt::StrongFocus);
        setAccessibleName(name);
        setAccessibleDescription(summary);
        setAttribute(Qt::WA_Hover);
    }
    QSize sizeHint() const override
    {
        return QSize(150, 18 + std::max(22, QFontMetrics(sized(QFont::Bold)).height() + QFontMetrics(sized(QFont::Normal, FontRole::Small)).height()));
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        const bool chosen = isChecked(), hover = underMouse() && !chosen;
        const QColor ground = chosen ? m_theme->color("HighlightedBackground") : hover ? m_theme->accentTintStrong() : m_theme->accentTint();
        const QColor ink = chosen ? m_theme->accentContrastText() : hover ? m_theme->color("WindowForeground") : m_theme->softForeground();
        p.fillPath(roundedPath(rect(), Radius, true, true, false, false), ground);
        p.drawPixmap(11, (height() - 22) / 2, TablerIcons::pixmap(m_glyph, ink, 22, devicePixelRatioF()));
        const int x = 11 + 22 + 9, w = width() - x - 8;
        const QFontMetrics nameM(sized(QFont::Bold)), sumM(sized(QFont::Normal, FontRole::Small));
        const int top = (height() - nameM.height() - sumM.height()) / 2;
        p.setFont(sized(QFont::Bold));
        p.setPen(ink);
        p.drawText(QRect(x, top, w, nameM.height()), Qt::AlignLeft | Qt::AlignVCenter, nameM.elidedText(text(), Qt::ElideRight, w));
        p.setFont(sized(QFont::Normal, FontRole::Small));
        p.setPen(chosen ? m_theme->accentContrastText() : m_theme->mutedForeground());
        p.drawText(QRect(x, top + nameM.height(), w, sumM.height()), Qt::AlignLeft | Qt::AlignVCenter, sumM.elidedText(m_summary, Qt::ElideRight, w));
        if (hasFocus() && m_ring)
        {
            p.setPen(QPen(m_theme->color("WindowForeground"), 2));
            p.drawPath(roundedPath(QRectF(rect()).adjusted(3, 3, -3, -3), Radius - 3, true, true, false, false));
        }
    }
    void focusInEvent(QFocusEvent* e) override { m_ring = keyboardFocus(e) || (m_ring && e->reason() == Qt::OtherFocusReason); QAbstractButton::focusInEvent(e); }
    // the arrows move along the tabs, choosing as they go: the ring follows them
    void keyPressEvent(QKeyEvent* e) override { m_ring = true; QAbstractButton::keyPressEvent(e); }

private:
    Glyph m_glyph;
    QString m_summary;
    SonicPiTheme* m_theme;
    bool m_ring = false;
};

// The card: the chosen tab's accent across its top, the corner under that tab square, and its foot, the four ways,
// in the stronger tint (.sm-card, .sm-how)
class Card : public QWidget
{
public:
    Card(SonicPiTheme* theme, QWidget* foot) : m_theme(theme), m_foot(foot) {}
    int chosen = 0;

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        p.setClipPath(roundedPath(rect(), Radius, chosen != 0, chosen != 1, true, true));
        p.fillRect(rect(), m_theme->accentTint());
        p.fillRect(QRect(0, m_foot->y(), width(), height() - m_foot->y()), m_theme->accentTintStrong());
        p.fillRect(QRect(0, 0, width(), 3), m_theme->color("HighlightedBackground"));
    }

private:
    SonicPiTheme* m_theme;
    QWidget* m_foot;
};

// A way to share: a round glyph with its word under it; lit in the accent while its view shows (a QR code, the
// link) or under the pointer; a tick in the theme's green, with the word for what it did, a moment after (.sm-act)
class Action : public QAbstractButton
{
public:
    Action(Glyph glyph, const QString& label, const QString& tip, SonicPiTheme* theme)
        : m_glyph(glyph), m_label(label), m_theme(theme)
    {
        setText(label);
        setToolTip(tip);
        setAccessibleName(label);
        setAccessibleDescription(tip);
        setCursor(Qt::PointingHandCursor);
        setFocusPolicy(Qt::StrongFocus);
        setAttribute(Qt::WA_Hover);
        m_undo.setSingleShot(true);
        QObject::connect(&m_undo, &QTimer::timeout, this, [this] { m_done = false; setText(m_label); update(); });
    }
    QSize sizeHint() const override { return QSize(70, 6 + 40 + 5 + QFontMetrics(sized()).height() + 6); }
    // done: the tick and the word for it, for a moment
    void said(const QString& word)
    {
        m_done = true;
        setText(word);
        m_undo.start(2500);
        update();
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);
        const QColor fg = m_theme->color("WindowForeground");
        QColor disc = m_theme->color("Background"), ink = m_theme->softForeground(), word = fg;
        if (m_done)
        {
            // the theme's own green, its strings', readable on light and dark alike
            disc = m_theme->color("DoubleQuotedStringForeground");
            disc.setAlphaF(0.24);
            ink = word = SonicPiTheme::blend(m_theme->color("DoubleQuotedStringForeground"), fg, 0.3);
        }
        else if (isChecked() || isDown() || underMouse())
        {
            disc = m_theme->color("HighlightedBackground");
            ink = m_theme->accentContrastText();
        }
        const QRectF circle((width() - 40) / 2.0, 6, 40, 40);
        p.setPen(Qt::NoPen);
        p.setBrush(disc);
        p.drawEllipse(circle);
        p.drawPixmap(QPointF(circle.center().x() - 11, circle.center().y() - 11),
                     TablerIcons::pixmap(m_done ? Glyph::Check : m_glyph, ink, 22, devicePixelRatioF()));
        const QFontMetrics m(sized());
        p.setFont(sized());
        p.setPen(word);
        p.drawText(QRect(0, 6 + 40 + 5, width(), m.height()), Qt::AlignHCenter | Qt::AlignVCenter, m.elidedText(text(), Qt::ElideRight, width()));
        if (hasFocus() && m_ring)
        {
            p.setPen(QPen(m_theme->color("HighlightedBackground"), 2));
            p.setBrush(Qt::NoBrush);
            p.drawRoundedRect(QRectF(rect()).adjusted(1, 1, -1, -1), Radius, Radius);
        }
    }
    void focusInEvent(QFocusEvent* e) override { m_ring = keyboardFocus(e); QAbstractButton::focusInEvent(e); }

private:
    Glyph m_glyph;
    QString m_label;
    bool m_done = false;
    bool m_ring = false;
    SonicPiTheme* m_theme;
    QTimer m_undo;
};

// The close at the top right, a circled cross (.sm-close)
class Close : public QAbstractButton
{
public:
    explicit Close(SonicPiTheme* theme) : m_theme(theme)
    {
        setToolTip(QObject::tr("Close"));
        setAccessibleName(QObject::tr("Close"));
        setCursor(Qt::PointingHandCursor);
        setFocusPolicy(Qt::StrongFocus);
        setAttribute(Qt::WA_Hover);
        setFixedSize(28, 28);
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        const QColor ink = underMouse() || (hasFocus() && m_ring) ? m_theme->color("HighlightedBackground") : m_theme->softForeground();
        p.drawPixmap(2, 2, TablerIcons::pixmap(Glyph::CircleX, ink, 24, devicePixelRatioF()));
    }
    void focusInEvent(QFocusEvent* e) override { m_ring = keyboardFocus(e); QAbstractButton::focusInEvent(e); }

private:
    SonicPiTheme* m_theme;
    bool m_ring = false;
};

} // namespace

SharePanel::SharePanel(Scope buffer, Scope set, SonicPiTheme* theme, QWidget* window)
    : SonicPi::OverlayPane(window), m_theme(theme), m_scopes{ std::move(buffer), std::move(set) }
{
    setObjectName("sharePanel");   // its words', fields' and buttons' look: app.qss, as the web's style.css .sm-*
    setAccessibleName(tr("Save and Share"));
    setAttribute(Qt::WA_DeleteOnClose);
    hide();

    // its head: the words, and a close
    QLabel* step = new QLabel(tr("Save and Share").toUpper());
    step->setObjectName("shareStep");
    step->setAccessibleName(tr("Save and Share"));
    Close* close = new Close(theme);
    close->setObjectName("shareClose");
    connect(close, &QAbstractButton::clicked, this, &QWidget::close);
    QHBoxLayout* top = new QHBoxLayout();
    top->setContentsMargins(8, 2, 2, 2);
    top->addWidget(step);
    top->addStretch(1);
    top->addWidget(close);

    // what to share: two tabs
    QHBoxLayout* what = new QHBoxLayout();
    what->setContentsMargins(4, 0, 4, 0);
    what->setSpacing(4);
    const Glyph glyphs[2] = { Glyph::FileCode, Glyph::Stack2 };
    for (int i = 0; i < 2; i++)
    {
        m_tabs[i] = new Tab(glyphs[i], m_scopes[i].kind, m_scopes[i].summary, theme);
        m_tabs[i]->setObjectName(i == 0 ? "shareBuffer" : "shareSet");
        connect(m_tabs[i], &QAbstractButton::toggled, this, [this, i](bool on) { if (on) choose(i); });
        what->addWidget(m_tabs[i], 1);
    }

    // the card's body: what it is (a line, which says what was just done too), then its name, and a set's
    // description; in their place, its QR code or its link
    m_about = new QLabel();
    m_about->setObjectName("shareAbout");
    m_about->setWordWrap(true);
    m_about->setMinimumHeight(2 * QFontMetrics(sized()).lineSpacing());
    m_about->setAlignment(Qt::AlignLeft | Qt::AlignTop);

    QWidget* about = new QWidget();
    QVBoxLayout* aboutLayout = new QVBoxLayout(about);
    aboutLayout->setContentsMargins(0, 0, 0, 0);
    aboutLayout->setSpacing(5);
    // its name, and a set's description, each labelled (.sm-fields)
    QLabel* nameLabel = new QLabel(tr("Name").toUpper());
    nameLabel->setObjectName("shareFieldLabel");
    nameLabel->setAccessibleName(tr("Name"));
    m_name = new QLineEdit();
    m_name->setObjectName("shareName");
    m_name->setAccessibleName(tr("Name"));
    m_name->setMaxLength(SonicPi::SetBundle::NameMax);
    nameLabel->setBuddy(m_name);
    // a set's: its description
    m_descriptionBox = new QWidget();
    QVBoxLayout* descriptionLayout = new QVBoxLayout(m_descriptionBox);
    descriptionLayout->setContentsMargins(0, 2, 0, 0);
    descriptionLayout->setSpacing(5);
    QHBoxLayout* descriptionHead = new QHBoxLayout();
    QLabel* descriptionLabel = new QLabel(tr("Description").toUpper());
    descriptionLabel->setObjectName("shareFieldLabel");
    descriptionLabel->setAccessibleName(tr("Description"));
    QLabel* optional = new QLabel(tr("optional"));
    optional->setObjectName("shareFieldHint");
    descriptionHead->addWidget(descriptionLabel);
    descriptionHead->addStretch(1);
    descriptionHead->addWidget(optional);
    m_description = new QPlainTextEdit();
    m_description->setObjectName("shareDescription");
    m_description->setAccessibleName(tr("Description"));
    m_description->setPlaceholderText(tr("Describe this set..."));
    m_description->setTabChangesFocus(true);
    descriptionLabel->setBuddy(m_description);
    descriptionLayout->addLayout(descriptionHead);
    descriptionLayout->addWidget(m_description, 1);
    // kept as they are typed; a name can't be emptied: it keeps the one it had (as the web's, on leaving it)
    auto keep = [this] {
        Scope& s = m_scopes[m_scope];
        if (!s.setFields)
            return;
        if (s.nameEditable && !m_name->text().trimmed().isEmpty())
            s.name = m_name->text().trimmed();
        if (s.described)
            s.description = m_description->toPlainText();
        s.setFields(s.name, s.description);
        static_cast<Tab*>(m_tabs[m_scope])->update();
    };
    connect(m_name, &QLineEdit::textEdited, this, keep);
    connect(m_name, &QLineEdit::editingFinished, this, [this] { if (m_name->text().trimmed().isEmpty()) m_name->setText(m_scopes[m_scope].name); });
    connect(m_description, &QPlainTextEdit::textChanged, this, keep);
    aboutLayout->addWidget(nameLabel);
    aboutLayout->addWidget(m_name);
    aboutLayout->addWidget(m_descriptionBox, 1);

    // its QR code: the code, and Larger (a big program's code is too fine to scan at the card's width) and Save
    QWidget* code = new QWidget();
    QVBoxLayout* codeLayout = new QVBoxLayout(code);
    codeLayout->setContentsMargins(0, 0, 0, 0);
    codeLayout->setSpacing(8);
    m_qrImage = new QLabel();
    m_qrImage->setObjectName("shareQrImage");
    m_qrImage->setAlignment(Qt::AlignCenter);
    m_qrImage->setAccessibleName(tr("QR code of the link"));
    m_qrImage->setCursor(Qt::PointingHandCursor);
    m_qrImage->installEventFilter(this);
    m_qrNote = new QLabel();
    m_qrNote->setObjectName("shareQrNote");
    m_qrNote->setWordWrap(true);
    m_qrNote->setAlignment(Qt::AlignCenter);
    m_larger = new QPushButton(tr("Larger"));
    m_larger->setObjectName("shareLarger");
    m_larger->setToolTip(tr("Show the code larger, for a phone to read off the screen"));
    m_saveImage = new QPushButton(tr("Save the image"));
    m_saveImage->setObjectName("shareSaveImage");
    m_saveImage->setToolTip(tr("Save the code as an image, for printing"));
    connect(m_larger, &QPushButton::clicked, this, [this] { m_large = !m_large; drawQr(); });
    connect(m_saveImage, &QPushButton::clicked, this, &SharePanel::saveImage);
    QHBoxLayout* codeButtons = new QHBoxLayout();
    codeButtons->setSpacing(8);
    codeButtons->addStretch(1);
    codeButtons->addWidget(m_larger);
    codeButtons->addWidget(m_saveImage);
    codeButtons->addStretch(1);
    codeLayout->addWidget(m_qrImage, 0, Qt::AlignHCenter);
    codeLayout->addWidget(m_qrNote);
    codeLayout->addLayout(codeButtons);
    codeLayout->addStretch(1);

    // its link, to copy by hand
    QWidget* linkPage = new QWidget();
    QVBoxLayout* linkLayout = new QVBoxLayout(linkPage);
    linkLayout->setContentsMargins(0, 0, 0, 0);
    linkLayout->setSpacing(8);
    m_linkText = new QPlainTextEdit();
    m_linkText->setObjectName("shareLinkText");
    m_linkText->setReadOnly(true);
    m_linkText->setAccessibleName(tr("The link"));
    m_linkNote = new QLabel();
    m_linkNote->setObjectName("shareLinkNote");
    m_linkNote->setAlignment(Qt::AlignCenter);
    linkLayout->addWidget(m_linkText, 1);
    linkLayout->addWidget(m_linkNote);

    m_pages = new QStackedWidget();
    m_pages->addWidget(about);
    m_pages->addWidget(code);
    m_pages->addWidget(linkPage);

    // the card's foot: the four ways, equal columns whatever their words
    QWidget* foot = new QWidget();
    QHBoxLayout* how = new QHBoxLayout(foot);
    how->setContentsMargins(6, 8, 6, 6);
    how->setSpacing(4);
    m_copyAct = new Action(Glyph::Copy, tr("Copy link"), tr("Copy its link to the clipboard, ready to paste"), theme);
    m_qrAct = new Action(Glyph::Qrcode, tr("QR code"), tr("Show a QR code, for a phone to scan or to save as an image"), theme);
    m_linkAct = new Action(Glyph::Link, tr("Show link"), tr("Show the link, to copy by hand"), theme);
    m_fileAct = new Action(Glyph::Download, tr("Save file"), QString(), theme);
    m_copyAct->setObjectName("shareCopy");
    m_qrAct->setObjectName("shareQr");
    m_linkAct->setObjectName("shareShowLink");
    m_fileAct->setObjectName("shareSaveFile");
    m_qrAct->setCheckable(true);
    m_linkAct->setCheckable(true);
    connect(m_copyAct, &QAbstractButton::clicked, this, &SharePanel::copyLink);
    connect(m_qrAct, &QAbstractButton::clicked, this, &SharePanel::toggleQr);
    connect(m_linkAct, &QAbstractButton::clicked, this, &SharePanel::toggleLink);
    connect(m_fileAct, &QAbstractButton::clicked, this, &SharePanel::saveFile);
    // the link's two ways side by side, then its QR code, then its file
    for (QAbstractButton* b : { m_copyAct, m_linkAct, m_qrAct, m_fileAct })
        how->addWidget(b, 1);

    Card* card = new Card(theme, foot);
    m_card = card;
    QVBoxLayout* cardLayout = new QVBoxLayout(card);
    cardLayout->setContentsMargins(0, 3, 0, 0);
    cardLayout->setSpacing(0);
    QWidget* panel = new QWidget();
    QVBoxLayout* panelLayout = new QVBoxLayout(panel);
    panelLayout->setContentsMargins(12, 10, 12, 12);
    panelLayout->setSpacing(8);
    panelLayout->addWidget(m_about);
    panelLayout->addWidget(m_pages, 1);
    cardLayout->addWidget(panel, 1);
    cardLayout->addWidget(foot);

    QVBoxLayout* layout = new QVBoxLayout(this);
    // never smaller than all of it: a code cut short doesn't scan
    layout->setSizeConstraint(QLayout::SetMinimumSize);
    layout->setContentsMargins(8, 8, 8, 8);
    layout->setSpacing(0);
    layout->addLayout(top);
    layout->addSpacing(4);
    layout->addLayout(what);   // the chosen tab joined to the card under it
    QHBoxLayout* cardRow = new QHBoxLayout();
    cardRow->setContentsMargins(4, 0, 4, 4);
    cardRow->addWidget(card);
    layout->addLayout(cardRow, 1);
    setMinimumWidth(PanelWidth);

    // its words and fields in sized()'s roles, set here rather than in app.qss, where a size would stop the zoom
    const QStringList small = { "shareStep", "shareFieldLabel", "shareFieldHint", "shareQrNote", "shareLinkNote" };
    for (QWidget* w : findChildren<QWidget*>())
        if (qobject_cast<QLabel*>(w) || qobject_cast<QLineEdit*>(w) || qobject_cast<QPlainTextEdit*>(w) || qobject_cast<QPushButton*>(w))
            ApplyFontRole(w, small.contains(w->objectName()) ? FontRole::Small : FontRole::Base);
    {
        QFont code = m_linkText->font();
        code.setFamily(QStringLiteral("Hack"));
        m_linkText->setFont(code);
    }
    // a set's description: all the room the card has, and never less than five lines, clear of its box's border and
    // padding, which are the stylesheet's
    m_description->ensurePolished();
    const QMargins chrome = m_description->contentsMargins();
    m_description->setMinimumHeight(5 * QFontMetrics(m_description->font()).lineSpacing()
                                    + 2 * int(m_description->document()->documentMargin()) + chrome.top() + chrome.bottom());
    m_description->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Ignored);

    m_tabs[0]->setChecked(true);
}

SharePanel::~SharePanel()
{
    stopWatching();
}

void SharePanel::openUnder(QWidget* button)
{
    m_button = button;
    m_focusBefore = QApplication::focusWidget();
    // the window's keys and presses, to close on as the web's (share-menu.js), and its resizes, to stay in it
    qApp->installEventFilter(this);
    show();
    raise();
    fit();
    m_tabs[m_scope]->setFocus();
}

void SharePanel::stopWatching()
{
    qApp->removeEventFilter(this);
}

// closed, the focus goes back where it was, as the web's goes back to its button
void SharePanel::closeEvent(QCloseEvent* event)
{
    stopWatching();
    if (m_focusBefore && (!QApplication::focusWidget() || isAncestorOf(QApplication::focusWidget())))
        m_focusBefore->setFocus();
    SonicPi::OverlayPane::closeEvent(event);
}

QString SharePanel::link() const
{
    return LinkBase + QString::fromStdString(ShareLink::encode(m_scopes[m_scope].code().toStdString()));
}

QString SharePanel::qrLink() const
{
    return LinkBase + QString::fromStdString(ShareLink::encodeDigits(m_scopes[m_scope].code().toStdString()));
}

QImage SharePanel::qrImage(const ShareLink::Qr& qr, int modulePx)
{
    const int side = modulePx * (qr.size + 2 * Quiet);
    QImage image(side, side, QImage::Format_RGB32);
    image.fill(Qt::white);
    QPainter p(&image);
    for (int r = 0; r < qr.size; r++)
        for (int c = 0; c < qr.size; c++)
            if (qr.isDark(r, c))
                p.fillRect((c + Quiet) * modulePx, (r + Quiet) * modulePx, modulePx, modulePx, Qt::black);
    return image;
}

// the panel: the editor's own ground inside the controls' outline, as the web's popups and native's chips
void SharePanel::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.setRenderHint(QPainter::Antialiasing);
    p.setPen(QPen(m_theme->color("Button"), 2));
    p.setBrush(m_theme->color("Background"));
    p.drawRoundedRect(QRectF(rect()).adjusted(1, 1, -1, -1), Radius + 2, Radius + 2);
}

void SharePanel::choose(int scope)
{
    m_scope = scope;
    static_cast<Card*>(m_card)->chosen = scope;
    m_card->update();
    render();
}

// the chosen one: what it is, its name, and a set's description
void SharePanel::render()
{
    const Scope& s = m_scopes[m_scope];
    m_about->setText(s.about);
    m_about->setProperty("said", false);
    m_about->style()->unpolish(m_about);
    m_about->style()->polish(m_about);
    m_descriptionBox->setVisible(s.described);
    m_name->setText(s.name);
    m_name->setReadOnly(!s.nameEditable);
    m_name->setToolTip(s.nameEditable ? tr("Its name, and its file's") : tr("A saved set is called what its file is: Save Set As gives it another name"));
    {
        const QSignalBlocker quiet(m_description);
        m_description->setPlainText(s.description);
    }
    m_fileAct->setToolTip(tr("Save it as a %1 file").arg(s.fileKind));
    m_fileAct->setAccessibleDescription(m_fileAct->toolTip());
    m_large = false;
    showPage(About);
    fit();
}

void SharePanel::showPage(int page)
{
    // the views are the size of the one showing: what it is keeps its one height, a QR code grows the card under it
    for (int i = 0; i < m_pages->count(); i++)
        m_pages->widget(i)->setSizePolicy(i == page ? QSizePolicy::Preferred : QSizePolicy::Ignored,
                                          i == page ? QSizePolicy::Preferred : QSizePolicy::Ignored);
    m_pages->setCurrentIndex(page);
    m_qrAct->setChecked(page == Code);
    m_linkAct->setChecked(page == Link);
}

// what was just done: the action a tick with the word for it, and a line saying it in the card's own line
void SharePanel::done(QAbstractButton* action, const QString& word, const QString& said)
{
    static_cast<Action*>(action)->said(word);
    showPage(About);
    fit();
    m_about->setText(said);
    m_about->setProperty("said", true);
    m_about->style()->unpolish(m_about);
    m_about->style()->polish(m_about);
}

void SharePanel::copyLink()
{
    QApplication::clipboard()->setText(link());
    done(m_copyAct, tr("Copied"), tr("Link copied"));
}

// pressed again, the code puts back what it is
void SharePanel::toggleQr()
{
    if (m_pages->currentIndex() == Code)
    {
        showPage(About);
        fit();
        return;
    }
    m_qr = ShareLink::qrCode(qrLink().toStdString());
    m_large = false;
    showPage(Code);
    drawQr();
}

void SharePanel::drawQr()
{
    const bool fits = m_qr.version > 0;
    m_qrImage->setVisible(fits);
    m_larger->setVisible(fits);
    m_saveImage->setVisible(fits);
    m_qrNote->setVisible(!fits);
    if (!fits)
    {
        m_qrNote->setText(m_scope == 1
            ? tr("This set is too big for a QR code a phone can read: copy the link or save it as a file instead.")
            : tr("This program is too big for a QR code a phone can read: copy the link or save it as a file instead."));
        fit();
        return;
    }
    // drawn at a whole number of the screen's pixels a module: scaled, a module comes out a pixel wider or narrower
    // here and there, an uneven grid a camera reads badly
    // larger: what the window leaves once the rest of the panel has its own, as the web's what the page leaves
    int side = QrSide;
    if (m_large)
    {
        const QRect room = parentWidget()->rect();
        const int aroundW = width() - m_qrImage->width(), aroundH = height() - m_qrImage->height();
        side = std::max(QrSide, (int)std::min(room.height() * 0.96 - aroundH, room.width() * 0.96 - aroundW));
    }
    const qreal dpr = devicePixelRatioF();
    const int modules = m_qr.size + 2 * Quiet;
    const QImage image = qrImage(m_qr, std::max(1, (int)(side * dpr / modules)));
    // its corners rounded, as the web's (.sm-qr)
    QImage rounded(image.size(), QImage::Format_ARGB32_Premultiplied);
    rounded.fill(Qt::transparent);
    {
        QPainter p(&rounded);
        p.setRenderHint(QPainter::Antialiasing);
        QPainterPath clip;
        clip.addRoundedRect(QRectF(rounded.rect()), Radius * dpr, Radius * dpr);
        p.setClipPath(clip);
        p.drawImage(0, 0, image);
    }
    rounded.setDevicePixelRatio(dpr);
    m_qrImage->setPixmap(QPixmap::fromImage(rounded));
    // never cut: a code missing its edge doesn't scan, so the panel grows (or shrinks) to the whole of it
    m_qrImage->setFixedSize(rounded.deviceIndependentSize().toSize());
    m_qrImage->setToolTip(m_large ? tr("Make it smaller") : tr("Make it larger"));
    m_larger->setText(m_large ? tr("Smaller") : tr("Larger"));
    m_larger->setProperty("primary", !m_large && (double)QrSide / modules < ScanMin);
    m_larger->style()->unpolish(m_larger);
    m_larger->style()->polish(m_larger);
    fit();
}

// the size it needs now, at once: left to the event loop, a new size climbs a layout a pass, and the panel is drawn
// round what it held before
void SharePanel::fit()
{
    // what Qt does a layer a pass, done now from the deepest up: each widget laid out afresh, then its new size told
    // to the layout it sits in (which keeps its old one till then)
    QList<QWidget*> widgets = findChildren<QWidget*>();
    auto depth = [this](QWidget* w) { int d = 0; for (; w && w != this; w = w->parentWidget()) d++; return d; };
    std::stable_sort(widgets.begin(), widgets.end(), [&](QWidget* a, QWidget* b) { return depth(a) > depth(b); });
    for (QWidget* w : widgets)
    {
        if (w->layout())
        {
            w->layout()->invalidate();
            w->layout()->activate();
        }
        w->updateGeometry();
    }
    layout()->invalidate();
    layout()->activate();
    adjustSize();
    place();
}

// under its button, kept 12 pixels inside the window, as the web's (share-menu.js open); a larger QR code, or with no
// button, in the middle of the window (style.css #link-menu.sm-large)
void SharePanel::place()
{
    constexpr int Edge = 12;
    const QSize room = parentWidget()->size();
    if (!m_button || m_large)
    {
        move((room.width() - width()) / 2, (room.height() - height()) / 2);
        return;
    }
    QPoint at = m_button->mapTo(parentWidget(), QPoint(0, m_button->height() + 6));
    at.setX(std::clamp(at.x(), Edge, std::max(Edge, room.width() - width() - Edge)));
    if (at.y() + height() > room.height() - Edge)   // too long for the room under its button: up, as far as the edge
        at.setY(std::max(Edge, room.height() - height() - Edge));
    move(at);
}

bool SharePanel::eventFilter(QObject* watched, QEvent* event)
{
    if (watched == m_qrImage && event->type() == QEvent::MouseButtonRelease)
    {
        m_large = !m_large;
        drawQr();
        return true;
    }
    // the rest is the window's, while it is open: as the web's, Escape or Ctrl-G anywhere in it closes the panel, ahead
    // of the window's own Escape (escapeWorkspaces), and a press anywhere in it but the panel and its button closes it,
    // going on to what it was on. Every other key is the window's, its shortcuts too.
    QWidget* w = qobject_cast<QWidget*>(watched);
    if (!w || w->window() != window() || !isVisible())
        return false;
    if (watched == parentWidget() && event->type() == QEvent::Resize)
        place();
    else if (event->type() == QEvent::ShortcutOverride || event->type() == QEvent::KeyPress)
    {
        // Qt on a Mac gives the Command key as Control and the Control key as Meta: either, as the find popup's
        auto* key = static_cast<QKeyEvent*>(event);
        const bool ctrlish = key->modifiers() & (Qt::ControlModifier | Qt::MetaModifier);
        if (key->key() == Qt::Key_Escape || (ctrlish && key->key() == Qt::Key_G))
        {
            event->accept();
            if (event->type() == QEvent::KeyPress)
                close();
            return true;
        }
    }
    else if (event->type() == QEvent::MouseButtonPress && w != this && !isAncestorOf(w)
             && !(m_button && (w == m_button || m_button->isAncestorOf(w))))
        close();
    return false;
}

void SharePanel::toggleLink()
{
    if (m_pages->currentIndex() == Link)
    {
        showPage(About);
        fit();
        return;
    }
    const QString url = link();
    m_linkText->setPlainText(url);
    m_linkNote->setText(plural(url.size(), tr("character"), tr("characters")));
    showPage(Link);
    fit();
    m_linkText->setFocus();
}

void SharePanel::saveFile()
{
    if (m_scopes[m_scope].save && m_scopes[m_scope].save())
        done(m_fileAct, tr("Saved"), tr("Saved"));
}

// for printing: 8 pixels a module, whatever the screen
void SharePanel::saveImage()
{
    const QString name = QStringLiteral("sonic-pi-%1.png").arg(QString(m_scopes[m_scope].title).replace(QRegularExpression("[^\\w-]+"), "-"));
    const QString path = QFileDialog::getSaveFileName(this, tr("Save the QR code"), QDir::homePath() + "/Desktop/" + name,
                                                      QString("%1 (*.png)").arg(tr("PNG images")));
    if (!path.isEmpty())
        qrImage(m_qr, 8).save(path, "PNG");
}
