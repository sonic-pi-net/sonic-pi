//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//++

#include "quickstartpane.h"

#include "chevronbutton.h"

#include <QAccessible>
#include <QAccessibleWidget>
#include <QBuffer>
#include <QDateTime>
#include <QFile>
#include <QFileInfo>
#include <QFontMetrics>
#include <QFontMetricsF>
#include <QFrame>
#include <QHBoxLayout>
#include <QImage>
#include <QKeyEvent>
#include <QLabel>
#include <QPainter>
#include <QPainterPath>
#include <QPair>
#include <QPen>
#include <QPushButton>
#include <QRegularExpression>
#include <QScrollArea>
#include <QScrollBar>
#include <QSettings>
#include <QStyle>
#include <QSvgRenderer>
#include <QTextDocumentFragment>
#include <QTimer>
#include <QVBoxLayout>
#include <QWheelEvent>

#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/fontroles.h"
#include "utils/gui_settings.h"
#include "utils/tablericons.h"
#include "utils/tutorialdocs.h"
#include "widgets/carddeck.h"
#include "widgets/codecard.h"
#include "widgets/tutorialwidgets.h" // TutHeading (accessible section titles)

#include "api/sonicpi_api.h"

namespace
{
// Re-evaluate a widget's stylesheet after a dynamic property change so
// property-driven rules (e.g. [cardHover="true"]) take effect.
void repolish(QWidget* w)
{
    w->style()->unpolish(w);
    w->style()->polish(w);
    w->update();
}

struct Deck
{
    QString title;
    QString description; // the deck's words, on its welcome card
    QVector<SonicPi::QuickstartCard> cards;
};
} // namespace

// Parse the quickstart cards file (etc/quickstart/cards.txt) into decks.
// Format: "# Deck: name" starts a deck; the text before its first card is the
// deck's description; "## title" starts a card, the lines after are its blurb,
// and a ``` fenced block is the code. Forgiving, so it is easy to hand-author.
static QVector<Deck> parseDecks(const QString& text)
{
    QVector<Deck> decks;
    SonicPi::QuickstartCard* card = nullptr;
    bool inCode = false;
    const QStringList lines = text.split('\n');
    for (const QString& line : lines)
    {
        const QString trimmed = line.trimmed();
        if (inCode)
        {
            if (trimmed == QLatin1String("```")) { inCode = false; continue; }
            if (card)
            {
                if (!card->code.isEmpty()) card->code += '\n';
                card->code += line; // keep indentation verbatim
            }
            continue;
        }
        if (line.startsWith(QLatin1String("# Deck:")))
        {
            Deck d;
            d.title = line.mid(7).trimmed();
            decks.append(d);
            card = nullptr;
            continue;
        }
        if (decks.isEmpty()) continue;
        if (line.startsWith(QLatin1String("## ")))
        {
            SonicPi::QuickstartCard c;
            c.title = line.mid(3).trimmed();
            decks.last().cards.append(c);
            card = &decks.last().cards.last();
            continue;
        }
        if (trimmed == QLatin1String("```")) { inCode = true; continue; }
        if (trimmed.startsWith(QLatin1Char('#')) || trimmed.isEmpty()) continue; // comment/blank
        if (card) // blurb line(s)
        {
            if (!card->blurb.isEmpty()) card->blurb += ' ';
            card->blurb += trimmed;
        }
        else // text before the first card is the deck description
        {
            if (!decks.last().description.isEmpty()) decks.last().description += ' ';
            decks.last().description += trimmed;
        }
    }
    return decks;
}

// A tiny built-in deck, used only if the cards file is missing or empty so
// the pane never comes up blank.
static QVector<Deck> fallbackDecks()
{
    Deck d;
    d.title = QStringLiteral("Play");
    d.description = QStringLiteral("A few first sounds to get you started.");
    d.cards.append({ QStringLiteral("Your First Note"),
                     QStringLiteral("One line, one note."), QStringLiteral("play 60") });
    d.cards.append({ QStringLiteral("A Beat"), QStringLiteral("A drum break in one line."),
                     QStringLiteral("sample :loop_amen") });
    return { d };
}

bool QuickstartPane::validateCardsFile(const QString& path, QString* error)
{
    QFile f(path);
    if (!f.open(QFile::ReadOnly | QFile::Text))
    {
        if (error)
            *error = tr("The file could not be opened for reading.");
        return false;
    }
    const QVector<Deck> decks = parseDecks(QString::fromUtf8(f.readAll()));
    if (decks.isEmpty())
    {
        if (error)
            *error = tr("No card decks were found. A card set needs at least one "
                        "\"# Deck: name\" line followed by \"## Card title\" cards.");
        return false;
    }
    return true;
}

// Load + cache the decks from `path`. Re-read when the path OR the file's
// modification time changes, so editing the file and reloading picks up edits.
static const QVector<Deck>& quickstartDecks(const QString& path)
{
    static QString cachedPath;
    static QDateTime cachedMtime;
    static QVector<Deck> cached;
    const QDateTime mtime = QFileInfo(path).lastModified();
    if (!cached.isEmpty() && path == cachedPath && mtime == cachedMtime)
        return cached;
    cachedPath = path;
    cachedMtime = mtime;
    cached.clear();
    // The pane is built before MainWindow names its file: no path yet is
    // nothing to open, not a file that failed to.
    if (!path.isEmpty())
    {
        QFile f(path);
        if (f.open(QFile::ReadOnly | QFile::Text))
            cached = parseDecks(QString::fromUtf8(f.readAll()));
    }
    if (cached.isEmpty()) cached = fallbackDecks();
    return cached;
}

namespace
{
using SonicPi::QuickstartProgress;

// The welcome card reads as a group named for its deck, as a card does (CodeCardAccessible): a QFrame's own role,
// Border, is pruned from the tree by the platform bridges, and its name would never be spoken.
QAccessibleInterface* quickstartAccessibleFactory(const QString&, QObject* object)
{
    QWidget* w = qobject_cast<QWidget*>(object);
    if (w && w->objectName() == QLatin1String("qsWelcome"))
        return new QAccessibleWidget(w, QAccessible::Grouping);
    return nullptr;
}

// A button that Return and Enter press, as Space does: on macOS a QPushButton outside a dialog takes Space alone.
// Left and Right are left to the pane (the next card, the previous one), not a move to the next control.
class KeyButton : public QPushButton
{
public:
    using QPushButton::QPushButton;

protected:
    void keyPressEvent(QKeyEvent* e) override
    {
        const bool plain = !(e->modifiers() & (Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier));
        if (plain && (e->key() == Qt::Key_Return || e->key() == Qt::Key_Enter))
        {
            click();
            return;
        }
        if (e->key() == Qt::Key_Left || e->key() == Qt::Key_Right)
        {
            e->ignore();
            return;
        }
        QPushButton::keyPressEvent(e);
    }
};

// Words drawn inside a button. The button is named for them, so a screen reader is not read them a second time, and
// a click on them is the button's.
QLabel* innerLabel(const QString& text, const char* name, QWidget* parent)
{
    QLabel* l = new QLabel(text, parent);
    l->setObjectName(QString::fromLatin1(name));
    hideFromScreenReaders(l);
    l->setAttribute(Qt::WA_TransparentForMouseEvents);
    return l;
}

QColor withAlpha(QColor c, qreal alpha)
{
    c.setAlphaF(alpha);
    return c;
}

// The dashed outline the welcome card and the tile share (the web's: 2px, dashed, in the accent over a wash of it),
// lit while the way in is under the pointer or has the focus.
void paintDashed(QPainter& p, const QRectF& r, const QColor& accent, bool lit, qreal width, qreal radius)
{
    p.setRenderHint(QPainter::Antialiasing, true);
    QPen pen(withAlpha(accent, lit ? 1.0 : 0.55), width);
    pen.setStyle(Qt::CustomDashLine);
    pen.setDashPattern({ 3, 2 });
    p.setPen(pen);
    p.setBrush(withAlpha(accent, lit ? 0.12 : 0.06));
    const qreal inset = width / 2;
    p.drawRoundedRect(r.adjusted(inset, inset, -inset, -inset), radius, radius);
}

// The deck's first card, drawn as the tile is: not a program, the way in to them. As big as the cards beside it.
class WelcomeCard : public QFrame
{
public:
    QColor accent;
    qreal border = 2;
    qreal radius = 8;
    QWidget* way = nullptr;   // Start: the card lights with it

    explicit WelcomeCard(QWidget* parent)
        : QFrame(parent)
    {
        setObjectName(QStringLiteral("qsWelcome"));
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        paintDashed(p, rect(), accent, way && (way->underMouse() || way->hasFocus()), border, radius);
    }
};

// The welcome card's call (Start deck, Carry on, Play through again): the accent disc in its halo, a little larger
// and with a wider halo under the pointer or the focus, and its words beside it.
class StartButton : public KeyButton
{
public:
    QColor accent;
    QColor glyph;
    int disc = 72;
    QWidget* card = nullptr;   // lights with it

    explicit StartButton(QWidget* parent)
        : KeyButton(parent)
    {
        setObjectName(QStringLiteral("qsStart"));
        setCursor(Qt::PointingHandCursor);
    }
    // room for the halo at its widest (lit: the disc grown 6%, the ring a tenth of the disc)
    int halo() const { return qRound(disc * 0.14); }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        const bool lit = underMouse() || hasFocus();
        const qreal d = disc * (lit ? 1.06 : 1.0);
        const qreal ring = disc * (lit ? 0.1 : 0.06);
        const QPointF c(halo() + disc / 2.0, height() / 2.0);
        p.setPen(Qt::NoPen);
        p.setBrush(withAlpha(accent, lit ? 0.32 : 0.22));
        p.drawEllipse(c, d / 2 + ring, d / 2 + ring);
        p.setBrush(accent);
        p.drawEllipse(c, d / 2, d / 2);
        // the triangle a little right of centre, where it looks centred
        const qreal t = d * 0.14;
        const qreal x = c.x() + d * 0.03;
        QPainterPath tri;
        tri.moveTo(x - t * 0.8, c.y() - t);
        tri.lineTo(x - t * 0.8, c.y() + t);
        tri.lineTo(x + t, c.y());
        tri.closeSubpath();
        p.setBrush(glyph);
        p.drawPath(tri);
    }
    void enterEvent(QEnterEvent* e) override
    {
        KeyButton::enterEvent(e);
        relight();
    }
    void leaveEvent(QEvent* e) override
    {
        KeyButton::leaveEvent(e);
        relight();
    }
    void focusInEvent(QFocusEvent* e) override
    {
        KeyButton::focusInEvent(e);
        relight();
    }
    void focusOutEvent(QFocusEvent* e) override
    {
        KeyButton::focusOutEvent(e);
        relight();
    }

private:
    void relight()
    {
        update();
        if (card)
            card->update();
    }
};

// The tile after the newest card: what comes next (the next card, or the deck done, the next deck) and a press to
// deal it. The disc sits over its words.
class NextTile : public KeyButton
{
public:
    QColor accent;
    QColor glyph;
    int disc = 64;
    int gap = 12;
    qreal border = 2;
    qreal radius = 8;
    QWidget* below = nullptr;   // the words the disc sits over

    explicit NextTile(QWidget* parent)
        : KeyButton(parent)
    {
        setObjectName(QStringLiteral("qsNextTile"));
        setCursor(Qt::PointingHandCursor);
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        paintDashed(p, rect(), accent, underMouse() || hasFocus(), border, radius);
        if (!below)
            return;
        const QPointF c(width() / 2.0, below->geometry().top() - gap - disc / 2.0);
        p.setPen(Qt::NoPen);
        p.setBrush(accent);
        p.drawEllipse(c, disc / 2.0, disc / 2.0);
        // tabler's chevron-right (M9 6l6 6l-6 6), at the web's 34 of the disc's 64
        const qreal s = disc * 34.0 / 64.0 / 24.0;
        QPainterPath chev;
        chev.moveTo(c.x() + (9 - 12) * s, c.y() + (6 - 12) * s);
        chev.lineTo(c.x() + (15 - 12) * s, c.y());
        chev.lineTo(c.x() + (9 - 12) * s, c.y() + (18 - 12) * s);
        QPen pen(glyph, 2.5 * s);
        pen.setCapStyle(Qt::RoundCap);
        pen.setJoinStyle(Qt::RoundJoin);
        p.setPen(pen);
        p.setBrush(Qt::NoBrush);
        p.drawPath(chev);
    }
    void enterEvent(QEnterEvent* e) override
    {
        KeyButton::enterEvent(e);
        update();
    }
    void leaveEvent(QEvent* e) override
    {
        KeyButton::leaveEvent(e);
        update();
    }
    void focusInEvent(QFocusEvent* e) override
    {
        KeyButton::focusInEvent(e);
        update();
    }
    void focusOutEvent(QFocusEvent* e) override
    {
        KeyButton::focusOutEvent(e);
        update();
    }
};

// A deck's words, read at a glance: the first sentence the lead (to a full stop, a ! or a ?, or a smiley: Basics'
// "Welcome friend :-)"), the rest under it. As the web's paintDesc.
QPair<QString, QString> splitLead(const QString& text)
{
    static const QRegularExpression lead(QStringLiteral(R"(^(.+?(?:[.!?]|[:;]-?[)D]))(?:\s+(.*))?$)"),
                                         QRegularExpression::DotMatchesEverythingOption);
    const QRegularExpressionMatch m = lead.match(text.trimmed());
    if (!m.hasMatch())
        return { text.trimmed(), QString() };
    return { m.captured(1), m.captured(2).trimmed() };
}

// Where the deck's words name a card's control (Press play, drag it, insert at point), that control's glyph beside
// the words, as the web shows it: the glyph in the accent's square. The words stay as they are, so a screen reader
// reads them and nothing for the glyph.
QString withGlyphs(const QString& text, const QColor& accent, const QColor& ink, int px, qreal dpr)
{
    using TablerIcons::Glyph;
    QString html = text.toHtmlEscaped();
    const struct { const char* words; Glyph glyph; } named[] = {
        { "Press play", Glyph::Play },
        { "drag it", Glyph::Texture },
        { "insert at point", Glyph::SquareChevronsUp },
    };
    for (const auto& n : named)
    {
        const QString words = QString::fromLatin1(n.words);
        const int at = html.indexOf(words);
        if (at < 0)
            continue;
        const int dev = qRound(px * dpr);
        QImage img(dev, dev, QImage::Format_ARGB32_Premultiplied);
        img.fill(Qt::transparent);
        QPainter p(&img);
        p.setRenderHint(QPainter::Antialiasing, true);
        p.setPen(Qt::NoPen);
        p.setBrush(accent);
        p.drawRoundedRect(QRectF(0, 0, dev, dev), dev * 0.22, dev * 0.22);
        QSvgRenderer svg(TablerIcons::svgMarkup(n.glyph, ink).toUtf8());
        const qreal g = dev * 0.66;
        svg.render(&p, QRectF((dev - g) / 2, (dev - g) / 2, g, g));
        p.end();
        QByteArray png;
        QBuffer buf(&png);
        buf.open(QIODevice::WriteOnly);
        img.save(&buf, "PNG");
        html.insert(at + words.size(),
                    QStringLiteral(" <img src=\"data:image/png;base64,%1\" width=\"%2\" height=\"%2\" "
                                   "style=\"vertical-align: middle\">")
                        .arg(QString::fromLatin1(png.toBase64()))
                        .arg(px));
    }
    return html;
}
} // namespace

QuickstartPane::QuickstartPane(SonicPiTheme* theme, QWidget* parent)
    : QWidget(parent), m_theme(theme)
{
    registerTutorialWidgetAccessibility(); // the deck's name is a TutHeading
    registerCodeCardAccessibility();       // the cards' groups, and the words inside a button left unread
    static bool installed = false;
    if (!installed)
    {
        installed = true;
        QAccessible::installFactory(quickstartAccessibleFactory);
    }
    setAttribute(Qt::WA_StyledBackground, true);
    QVBoxLayout* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    // The cards' runs: layered, as a performance deck's loops stack. Their hover too, polled only while the pane shows.
    m_deck = new CardDeck(this, CardDeck::Playing::Layered);
    connect(m_deck, &CardDeck::runRequested, this, &QuickstartPane::runRequested);
    connect(m_deck, &CardDeck::stopJobRequested, this, &QuickstartPane::stopJobRequested);

    loadProgress();
    rebuild();
}

void QuickstartPane::setCardsFile(const QString& path)
{
    m_cardsPath = path;
    loadProgress();
    rebuild();
}

// The progress kept is for the set it was kept with: another set starts afresh.
void QuickstartPane::loadProgress()
{
    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    QVector<int> sizes;
    for (const Deck& d : decks)
        sizes << d.cards.size();
    m_progress.setDecks(sizes);
    if (m_cardsPath.isEmpty())
        return;
    QSettings s = SonicPi::guiSettings();
    if (s.value(QStringLiteral("quickstart/set")).toString() != m_cardsPath)
        return;
    QVector<int> dealt;
    for (const QString& n : s.value(QStringLiteral("quickstart/dealt")).toString().split(',', Qt::SkipEmptyParts))
        dealt << n.toInt();
    m_progress.restore(s.value(QStringLiteral("quickstart/deck")).toInt(), dealt,
                       s.value(QStringLiteral("quickstart/reached")).toInt());
}

void QuickstartPane::saveProgress() const
{
    if (m_cardsPath.isEmpty())
        return;
    QStringList dealt;
    for (int n : m_progress.dealtAll())
        dealt << QString::number(n);
    QSettings s = SonicPi::guiSettings();
    s.setValue(QStringLiteral("quickstart/set"), m_cardsPath);
    s.setValue(QStringLiteral("quickstart/deck"), m_progress.deck());
    s.setValue(QStringLiteral("quickstart/dealt"), dealt.join(','));
    s.setValue(QStringLiteral("quickstart/reached"), m_progress.reached());
}

void QuickstartPane::setAudioApi(std::shared_ptr<SonicPi::SonicPiAPI> api)
{
    m_spAPI = api; // kept: the deck's rings borrow it
    m_deck->setAudioApi(m_spAPI.get());
}

void QuickstartPane::showEvent(QShowEvent* event)
{
    QWidget::showEvent(event);
    if (m_themeDirty)
    {
        m_themeDirty = false;
        rebuild();
    }
    else if (m_row && perPage() != m_laidOut)
        render();
}

void QuickstartPane::applyTheme()
{
    // The rebuild re-creates the page's cards, a heavy step of a whole-app re-theme: a hidden Cards tab takes it
    // lazily on its next show instead of on every theme or hue change.
    if (isVisible())
        rebuild();
    else
        m_themeDirty = true;
}

int QuickstartPane::preferredDockHeight() const
{
    // The bar (kept room for, so the cards don't move when dealing starts) and one row of cards.
    int h = m_cardHeight + uiScale().y(8) + uiScale().y(14);
    if (m_head)
        h += m_head->sizeHint().height();
    return h;
}

int QuickstartPane::cardWidth() const
{
    // Sized so kMaxCols monospace columns fit without shrinking the code font (grown in step with kMaxCols: 41 cols
    // needs ~441).
    return qRound(ScaleHeightForDPI(441) * m_zoomFactor);
}

void QuickstartPane::computeGlobalLayout()
{
    if (m_codeBodyH > 0 && m_layoutZoom == m_userZoom)
        return; // only the (width-independent) zoom invalidates these
    m_layoutZoom = m_userZoom;

    // The card is a fixed-size format; content is authored to fit these budgets rather than the card growing to fit
    // content.
    const int kMaxCols = 41;   // widest code line a card is designed to hold
    const int kCodeLines = 6;  // tallest snippet a card is designed to hold
    const int kBlurbLines = 2; // blurb runs full width, so it wraps in fewer lines

    const int pad = uiScale().y(14);
    // The frame's fixed width includes the 2dx #qsCard border (app.qss), so the body gets less than cardWidth() to
    // lay out in.
    const int border = ResolveDxToPx(2);
    const int codeAvail = cardWidth() - 2 * pad - 2 * border;

    // Code font: the requested (zoomed) size, capped so kMaxCols monospace columns fit the card width. An authored
    // line wider than that is still reachable (the body scrolls), but sizing to the budget is what keeps scrolling
    // the exception rather than the reading experience.
    int codePx = qMax(8, qRound(ScaleHeightForDPI(15) * m_zoomFactor));
    QFont hack(QStringLiteral("Hack"));
    hack.setPixelSize(codePx);
    const qreal colW = QFontMetricsF(hack).horizontalAdvance(QLatin1Char('m'));
    if (colW * kMaxCols > codeAvail)
        // Floor stays unscaled: it is the "fit an authored line" backstop, so letting it rise with zoom would defeat
        // the shrink-to-fit it guards.
        codePx = qMax(ScaleHeightForDPI(9), int(codePx * codeAvail / (colW * kMaxCols)));
    m_codeFontPx = codePx;

    // The code lays out through a QTextDocument, which at some font sizes rounds a pixel taller than
    // QFontMetrics::height(); a 1px/line underestimate overflows the fixed body. Measured as a card lays it out.
    m_codeBodyH = 2 * uiScale().y(10) + CodeCard::codeLinesHeight(kCodeLines, m_codeFontPx);

    // Footer: the description (two lines) beside the scope, which is the play/stop control. The scope is inset from
    // the footer's inner height so its rings never touch (get clipped by) the edges.
    QFont sans;
    sans.setPixelSize(m_codeFontPx); // the blurb renders at the code's size
    const int blurbLineH = QFontMetrics(sans).height();
    const int innerH = kBlurbLines * blurbLineH + uiScale().y(58);
    m_footerH = 2 * uiScale().y(8) + innerH;
    m_scopeSide = innerH - uiScale().y(16);
    // The blurb yields the full hit-box width (scope + its surrounding margin), not just the scope square.
    m_blurbW = codeAvail - innerH - uiScale().y(12);
}

int QuickstartPane::perPage() const
{
    const int gap = uiScale().y(14);
    const int pad = uiScale().y(14);
    return qMax(1, (width() - 2 * pad + gap) / (cardWidth() + gap));
}

void QuickstartPane::resizeEvent(QResizeEvent* event)
{
    QWidget::resizeEvent(event);
    if (m_row && perPage() != m_laidOut)
        render(); // a width that fits more or fewer cards
}

void QuickstartPane::runStarted(int jobId, const QString& workspace)
{
    m_deck->runStarted(jobId, workspace);
}

void QuickstartPane::runEnded(int jobId)
{
    m_deck->runEnded(jobId);
}

void QuickstartPane::flashLine(const QString& workspace, int line)
{
    m_deck->flashLine(workspace, line);
}

void QuickstartPane::setFlashBrightness(int percent)
{
    m_deck->setFlashBrightness(percent);
}

void QuickstartPane::runOutput(int jobId, const QString& text)
{
    m_deck->runOutput(jobId, text);
}

void QuickstartPane::runError(int jobId, const QString& message, int line)
{
    m_deck->runError(jobId, message, line);
}

void QuickstartPane::setUserZoom(int zoom)
{
    m_userZoom = qBound(kFontZoomMin, zoom, kFontZoomMax);
    m_zoomFactor = FontZoomFactor(m_userZoom);
    rebuild();
}

// Shared curve (utils/fontroles.h): multiplicative, so the card's type hierarchy holds its proportions as it zooms.
int QuickstartPane::rolePx(FontRole role) const
{
    return qMax(8, qRound(FontRolePx(role) * m_zoomFactor));
}

void QuickstartPane::rebuild()
{
    // deleteLater, not delete: rebuild runs from clicks on the widgets it replaces.
    if (QLayout* lay = layout())
    {
        while (QLayoutItem* item = lay->takeAt(0))
        {
            if (QWidget* w = item->widget())
            {
                w->setParent(nullptr);
                w->deleteLater();
            }
            delete item;
        }
    }
    m_head = nullptr;
    m_decks = nullptr;
    m_deckPills.clear();
    m_bar = nullptr;
    m_exit = nullptr;
    m_barTitle = nullptr;
    m_prevArrow = nullptr;
    m_nextArrow = nullptr;
    m_dotsHost = nullptr;
    m_dotsLayout = nullptr;
    m_count = nullptr;
    m_row = nullptr;
    m_rowLayout = nullptr;
    m_items.clear();
    m_cardFrames.clear();
    m_start = nullptr;
    m_tile = nullptr;
    m_laidOut = 0;
    m_deck->clear();

    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    if (decks.isEmpty())
        return;
    // a set edited on disk since its progress was read: read it again, within the set as it is now
    bool same = m_progress.dealtAll().size() == decks.size();
    for (int i = 0; same && i < decks.size(); ++i)
        same = m_progress.dealt(i) <= decks[i].cards.size();
    if (!same)
        loadProgress();

    computeGlobalLayout();
    // the cards' one height, a card's own at this zoom
    {
        CodeCard::Spec spec;
        spec.title = decks[0].cards.value(0).title;
        spec.code = decks[0].cards.value(0).code;
        CodeCard::Metrics metrics;
        metrics.width = cardWidth();
        metrics.codeBodyHeight = m_codeBodyH;
        metrics.footerHeight = m_footerH;
        metrics.scopeSide = m_scopeSide;
        metrics.blurbWidth = m_blurbW;
        metrics.codeFontPx = m_codeFontPx;
        metrics.titlePx = rolePx(FontRole::Large);
        metrics.zoomFactor = m_zoomFactor;
        CodeCard probe(spec, metrics, m_theme);
        m_cardHeight = probe.sizeHint().height();
    }

    const QColor accent = m_theme->color("HighlightedBackground");

    // ── the head: the deck buttons, or in a deck its bar ──
    m_head = new QWidget(this);
    QVBoxLayout* headLay = new QVBoxLayout(m_head);
    headLay->setContentsMargins(uiScale().y(14), uiScale().y(8), uiScale().y(12), uiScale().y(3));
    headLay->setSpacing(0);

    // The decks reached, to choose between: containers a screen reader walks through get names, or an anonymous
    // "group" stop tells the user nothing about where they are.
    m_decks = new QWidget(m_head);
    m_decks->setAccessibleName(tr("Card decks"));
    QHBoxLayout* decksLay = new QHBoxLayout(m_decks);
    decksLay->setContentsMargins(0, 0, 0, 0);
    decksLay->setSpacing(uiScale().y(6));
    for (int i = 0; i < decks.size(); ++i)
    {
        QPushButton* pill = new KeyButton(decks[i].title, m_decks);
        pill->setObjectName(QStringLiteral("qsDeckPill"));
        pill->setCursor(Qt::PointingHandCursor);
        pill->setAccessibleName(tr("%1 card deck").arg(decks[i].title));
        pill->setStyleSheet(QString("font-size: %1px;").arg(rolePx(FontRole::Base)));
        connect(pill, &QPushButton::clicked, this, [this, i] { chooseDeck(i); });
        decksLay->addWidget(pill);
        m_deckPills << pill;
    }
    decksLay->addStretch(1);
    headLay->addWidget(m_decks);

    // In a deck: the way back to the decks and the deck's name at the left, its pages (‹ dots ›) at the centre.
    m_bar = new QWidget(m_head);
    m_bar->setAccessibleName(tr("Card navigation"));
    QHBoxLayout* barLay = new QHBoxLayout(m_bar);
    barLay->setContentsMargins(0, 0, 0, 0);
    barLay->setSpacing(uiScale().y(12));
    QWidget* barStart = new QWidget(m_bar);
    QHBoxLayout* barStartLay = new QHBoxLayout(barStart);
    barStartLay->setContentsMargins(0, 0, 0, 0);
    barStartLay->setSpacing(uiScale().y(12));
    m_exit = new KeyButton(tr("Decks"), barStart);
    m_exit->setObjectName(QStringLiteral("qsExit"));
    m_exit->setCursor(Qt::PointingHandCursor);
    m_exit->setIcon(TablerIcons::icon(TablerIcons::Glyph::X, m_theme->color("Foreground"), uiScale().y(16),
                                      devicePixelRatioF()));
    m_exit->setIconSize(uiScale().size(16, 16));
    m_exit->setAccessibleName(tr("Back to the decks"));
    m_exit->setToolTip(tr("Back to the decks"));
    m_exit->setStyleSheet(QString("font-size: %1px;").arg(rolePx(FontRole::Small)));
    connect(m_exit, &QPushButton::clicked, this, &QuickstartPane::leave);
    m_barTitle = new QLabel(barStart);
    m_barTitle->setObjectName(QStringLiteral("qsBarTitle"));
    QFont titleFont = font();
    titleFont.setPixelSize(rolePx(FontRole::Large));
    titleFont.setWeight(QFont::Bold);
    m_barTitle->setFont(titleFont);
    barStartLay->addWidget(m_exit);
    barStartLay->addWidget(m_barTitle, 1);

    // The pages: the one chevron control (ChevronButton) each side of the dots, in one soft pill.
    const QColor arrowInk = SonicPiTheme::blend(m_theme->color("Foreground"), m_theme->color("PaneBackground"), 0.45);
    auto makeArrow = [&](ChevronButton::Dir dir, const char* name, const QString& a11y) {
        auto* b = new ChevronButton(m_bar);
        b->setObjectName(QString::fromLatin1(name));
        b->setDir(dir);
        b->setColors(Qt::transparent, Qt::transparent, arrowInk, accent);
        b->setAccessibleName(a11y);
        b->setToolTip(a11y);
        b->setFixedSize(uiScale().size(30, 30));
        return b;
    };
    m_prevArrow = makeArrow(ChevronButton::Left, "qsPrev", tr("Previous cards"));
    m_nextArrow = makeArrow(ChevronButton::Right, "qsNext", tr("Next cards"));
    connect(m_prevArrow, &QAbstractButton::clicked, this, [this] { turn(-1); });
    connect(m_nextArrow, &QAbstractButton::clicked, this, &QuickstartPane::forward);
    m_dotsHost = new QWidget(m_bar);
    m_dotsLayout = new QHBoxLayout(m_dotsHost);
    m_dotsLayout->setContentsMargins(uiScale().y(6), 0, uiScale().y(6), 0);
    m_dotsLayout->setSpacing(uiScale().y(8));
    m_count = new QLabel(m_bar);
    m_count->setObjectName(QStringLiteral("qsCount"));
    QWidget* nav = new QWidget(m_bar);
    nav->setObjectName(QStringLiteral("qsNav"));
    nav->setAccessibleName(tr("Pages"));
    QHBoxLayout* navLay = new QHBoxLayout(nav);
    navLay->setContentsMargins(uiScale().y(4), uiScale().y(2), uiScale().y(4), uiScale().y(2));
    navLay->setSpacing(uiScale().y(2));
    // The height pinned, and even, so the radius is exactly half of it: a radius Qt is given above half paints
    // artifacts, below it visible corners.
    int navH = uiScale().y(30) + 2 * uiScale().y(2);
    navH += navH % 2;
    nav->setFixedHeight(navH);
    nav->setStyleSheet(QStringLiteral("QWidget#qsNav { border-radius: %1px; }").arg(navH / 2));
    navLay->addWidget(m_prevArrow);
    navLay->addWidget(m_dotsHost);
    navLay->addWidget(m_count);
    navLay->addWidget(m_nextArrow);
    // the name's side and the other side share the room evenly, so the pages sit at the bar's centre
    barLay->addWidget(barStart, 1);
    barLay->addWidget(nav, 0, Qt::AlignVCenter);
    barLay->addStretch(1);
    headLay->addWidget(m_bar);
    layout()->addWidget(m_head);

    // ── the page: the welcome card, the cards dealt, the tile ──
    m_row = new QWidget(this);
    m_rowLayout = new QHBoxLayout(m_row);
    m_rowLayout->setContentsMargins(uiScale().y(14), uiScale().y(8), uiScale().y(14), uiScale().y(14));
    m_rowLayout->setSpacing(uiScale().y(14));
    layout()->addWidget(m_row);
    static_cast<QVBoxLayout*>(layout())->addStretch(1);

    render();
    // the pane's width is not final until it is laid out
    QTimer::singleShot(0, this, [this] {
        if (m_row && perPage() != m_laidOut)
            render();
    });
}

void QuickstartPane::render(FocusTo focus, int card)
{
    if (!m_row)
        return;
    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    const int deckIdx = m_progress.deck();
    if (deckIdx >= decks.size())
        return;
    const Deck& d = decks[deckIdx];
    const int per = perPage();
    m_laidOut = per;
    QWidget* had = window() ? window()->focusWidget() : nullptr;
    const bool hadIn = had && isAncestorOf(had);
    const bool hadOnPage = hadIn && m_row->isAncestorOf(had);

    // the head: the decks reached out of a deck, the bar in one
    for (int i = 0; i < m_deckPills.size(); ++i)
    {
        QPushButton* pill = m_deckPills[i];
        pill->setVisible(m_progress.deckShown(i));
        if (pill->property("current").toBool() != (i == deckIdx))
        {
            pill->setProperty("current", i == deckIdx);
            repolish(pill);
        }
    }
    m_decks->setVisible(m_progress.decksShown());
    m_bar->setVisible(m_progress.inDeck());
    m_head->setVisible(m_progress.decksShown() || m_progress.inDeck());
    m_barTitle->setText(d.title);
    rebuildDots(m_progress.pages(per));
    m_prevArrow->setEnabled(m_progress.canBack());
    m_nextArrow->setEnabled(m_progress.canForward(per));

    // What is on the page now comes, shown at once, and takes the focus; only then does what was there go (a playing
    // card's run carries on: CardDeck). The other way round, hiding the focused item first hands the focus to the
    // next control for a moment, and a screen reader reads that control out on the way.
    m_deck->clear();
    QList<QWidget*> old;
    while (QLayoutItem* item = m_rowLayout->takeAt(0))
    {
        if (QWidget* w = item->widget())
            old << w;
        delete item;
    }
    m_items.clear();
    m_cardFrames.clear();
    m_start = nullptr;
    m_tile = nullptr;
    const QuickstartProgress::Page shown = m_progress.shown(per);
    if (shown.welcome)
        m_items << welcomeCard();
    for (int i = shown.from; i < shown.to; ++i)
    {
        CodeCard* c = addCard(i);
        m_items << c;
        m_cardFrames << c;
    }
    if (shown.tile != QuickstartProgress::Tile::None)
        m_items << nextTile();
    for (QWidget* w : m_items)
    {
        m_rowLayout->addWidget(w, 0, Qt::AlignTop);
        w->show();
    }
    m_rowLayout->addStretch(1);
    m_row->setAccessibleName(tr("%1 cards").arg(d.title));
    const auto dropOld = [&old] {
        for (QWidget* w : old)
        {
            w->hide();
            w->deleteLater();
        }
    };

    // the pane's way in (F6, the Focus menu): the page's first item
    QWidget* entry = m_items.isEmpty() ? nullptr : focusTarget(m_items.first());
    setFocusProxy(entry);
    saveProgress();

    if (!hadIn)
    {
        dropOld();
        return;   // the focus was elsewhere: it stays there
    }
    QWidget* target = nullptr;
    switch (focus)
    {
    case FocusTo::Card:
        for (QWidget* f : m_cardFrames)
            if (f->property("cardIndex").toInt() == card)
                target = f;
        break;
    case FocusTo::Start:
        target = m_start;
        break;
    case FocusTo::Tile:
        target = m_tile;
        break;
    case FocusTo::Keep:
        // what had it went with the page, or is hidden (a deck button) or spent (an arrow at its end): the nearest
        // thing that does the same
        if (hadOnPage)
            target = entry;
        else if (!had->isVisible() || !had->isEnabled())
            target = m_bar->isVisible() && m_nextArrow->isEnabled() ? static_cast<QWidget*>(m_nextArrow) : entry;
        break;
    }
    if (!target && focus != FocusTo::Keep)
        target = entry;
    if (target)
        target->setFocus(Qt::OtherFocusReason);
    dropOld();
}

void QuickstartPane::rebuildDots(int pages)
{
    const int page = m_progress.page(m_laidOut);
    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    const int len = decks.value(m_progress.deck()).cards.size();
    // a dot a page while they fit at a glance; past that, where you are in words (the welcome card, or the cards on
    // show of the deck's), as the web's
    const bool asWords = pages > 6;
    m_dotsHost->setVisible(!asWords);
    m_count->setVisible(asWords);
    if (asWords)
    {
        const int a = m_progress.first(page, m_laidOut);
        const int b = qMin(m_progress.dealt(), a + m_laidOut - 1);
        m_count->setText(a == 0 ? tr("Intro") : a >= b ? tr("%1 of %2").arg(a).arg(len) : tr("%1–%2 of %3").arg(a).arg(b).arg(len));
        return;
    }
    // Made again only when the page count changes: a dot pressed stays, with the focus a keyboard or screen reader
    // user has on it.
    if (m_dotsLayout->count() != pages)
    {
        while (QLayoutItem* it = m_dotsLayout->takeAt(0))
        {
            if (QWidget* w = it->widget())
            {
                w->hide();
                w->deleteLater();
            }
            delete it;
        }
        // Even, so the radius below is exactly half: only an exact half renders a true circle.
        int dot = uiScale().y(10);
        dot += dot % 2;
        for (int i = 0; i < pages; ++i)
        {
            QPushButton* b = new KeyButton(m_dotsHost);
            b->setObjectName(QStringLiteral("qsDot"));
            b->setCheckable(true);
            b->setCursor(Qt::PointingHandCursor);
            b->setFixedSize(dot, dot);
            b->setStyleSheet(QStringLiteral("QPushButton#qsDot { border-radius: %1px; }").arg(dot / 2));
            b->setAccessibleName(tr("Page %1 of %2").arg(i + 1).arg(pages));
            b->setToolTip(tr("Page %1").arg(i + 1));
            connect(b, &QPushButton::clicked, this, [this, i] { goToPage(i); });
            m_dotsLayout->addWidget(b);
        }
    }
    for (int i = 0; i < m_dotsLayout->count(); ++i)
    {
        QPushButton* b = qobject_cast<QPushButton*>(m_dotsLayout->itemAt(i)->widget());
        if (!b)
            continue;
        b->setChecked(i == page);
        if (b->property("seen").toBool() != (i < page))
        {
            b->setProperty("seen", i < page);
            repolish(b);
        }
    }
}

QWidget* QuickstartPane::welcomeCard()
{
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    const QColor accent = m_theme->color("HighlightedBackground");
    WelcomeCard* card = new WelcomeCard(m_row);
    card->accent = accent;
    card->border = ResolveDxToPx(2);
    card->radius = uiScale().y(8);
    card->setFixedSize(cardWidth(), m_cardHeight);
    card->setAccessibleName(tr("%1: the deck's introduction").arg(d.title));
    QVBoxLayout* lay = new QVBoxLayout(card);
    lay->setContentsMargins(uiScale().y(18), uiScale().y(16), uiScale().y(18), uiScale().y(16));
    lay->setSpacing(0);

    // the deck's name, its heading: drawn in capitals, read as the word
    TutHeading* name = new TutHeading(d.title, 2, card);
    name->setObjectName(QStringLiteral("qsWelcomeDeck"));
    QFont nameFont = font();
    nameFont.setPixelSize(rolePx(FontRole::Small));
    nameFont.setWeight(QFont::DemiBold);
    nameFont.setCapitalization(QFont::AllUppercase);
    nameFont.setLetterSpacing(QFont::PercentageSpacing, 110);
    name->setFont(nameFont);
    lay->addWidget(name);
    lay->addSpacing(uiScale().y(8));

    const auto [lead, rest] = splitLead(d.description);
    if (!lead.isEmpty())
    {
        QLabel* leadLabel = new QLabel(lead, card);
        leadLabel->setObjectName(QStringLiteral("qsWelcomeLead"));
        leadLabel->setWordWrap(true);
        QFont leadFont = font();
        leadFont.setPixelSize(rolePx(FontRole::Large));
        leadFont.setWeight(QFont::Bold);
        leadLabel->setFont(leadFont);
        lay->addWidget(leadLabel);
        lay->addSpacing(uiScale().y(4));
    }
    if (!rest.isEmpty())
    {
        QLabel* body = new QLabel(card);
        body->setObjectName(QStringLiteral("qsWelcomeBody"));
        body->setTextFormat(Qt::RichText);
        body->setWordWrap(true);
        QFont bodyFont = font();
        bodyFont.setPixelSize(rolePx(FontRole::Base));
        body->setFont(bodyFont);
        body->setText(withGlyphs(rest, accent, m_theme->accentContrastText(),
                                 QFontMetrics(bodyFont).height() - 2, devicePixelRatioF()));   // no taller than its line
        lay->addWidget(body);
    }
    lay->addStretch(1);

    // the call to it: Start deals the first card; part-way, Carry on goes to the newest; all dealt, Play through again
    const int len = d.cards.size();
    const int n = m_progress.dealt();
    QString say, sub, tip;
    switch (m_progress.start())
    {
    case QuickstartProgress::Start::Start:
        say = tr("Start deck");
        sub = tr("%n cards", nullptr, len);
        tip = tr("Deal the first card: %1").arg(d.cards.value(0).title);
        break;
    case QuickstartProgress::Start::CarryOn:
        say = tr("Carry on");
        sub = tr("%1 of %n cards dealt", nullptr, len).arg(n);
        tip = tr("To the newest card: %1").arg(d.cards.value(n - 1).title);
        break;
    case QuickstartProgress::Start::Again:
        say = tr("Play through again");
        sub = tr("all %n cards dealt", nullptr, len);
        tip = tr("Back to the first card: %1").arg(d.cards.value(0).title);
        break;
    }
    StartButton* start = new StartButton(card);
    start->accent = accent;
    start->glyph = m_theme->accentContrastText();
    start->disc = uiScale().y(72);
    start->card = card;
    card->way = start;
    start->setAccessibleName(tr("%1, %2").arg(say, sub));
    start->setAccessibleDescription(tip);
    start->setToolTip(tip);
    QHBoxLayout* startLay = new QHBoxLayout(start);
    startLay->setContentsMargins(start->disc + 2 * start->halo() + uiScale().y(8), 0, 0, 0);
    QVBoxLayout* words = new QVBoxLayout;
    words->setSpacing(uiScale().y(2));
    QLabel* sayLabel = innerLabel(say, "qsStartSay", start);
    QFont sayFont = font();
    sayFont.setPixelSize(rolePx(FontRole::Large));
    sayFont.setWeight(QFont::Bold);
    sayLabel->setFont(sayFont);
    QLabel* subLabel = innerLabel(sub, "qsStartSub", start);
    QFont mono(QStringLiteral("Hack"));
    mono.setPixelSize(rolePx(FontRole::Small));
    subLabel->setFont(mono);
    words->addStretch(1);
    words->addWidget(sayLabel);
    words->addWidget(subLabel);
    words->addStretch(1);
    startLay->addLayout(words);
    start->setFixedHeight(start->disc + 2 * start->halo());
    start->setMinimumWidth(start->disc + 2 * start->halo() + uiScale().y(8)
                           + qMax(QFontMetrics(sayFont).horizontalAdvance(say), QFontMetrics(mono).horizontalAdvance(sub)));
    connect(start, &QPushButton::clicked, this, &QuickstartPane::startDeck);
    lay->addWidget(start, 0, Qt::AlignLeft);
    m_start = start;
    return card;
}

QWidget* QuickstartPane::nextTile()
{
    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    const Deck& d = decks[m_progress.deck()];
    const int n = m_progress.dealt();
    const bool aCard = n < d.cards.size();   // else the deck is done: the next deck
    QString label, title, count, tip;
    if (aCard)
    {
        label = tr("Next card");
        title = d.cards[n].title;
        count = tr("%1 of %2").arg(n + 1).arg(d.cards.size());
        tip = tr("Show the next card: %1").arg(title);
    }
    else
    {
        const Deck& next = decks[m_progress.deck() + 1];
        label = tr("Next deck");
        title = next.title;
        count = tr("%n cards", nullptr, next.cards.size());
        tip = tr("On to the next deck: %1").arg(title);
    }
    NextTile* tile = new NextTile(m_row);
    tile->accent = m_theme->color("HighlightedBackground");
    tile->glyph = m_theme->accentContrastText();
    tile->disc = uiScale().y(64);
    tile->gap = uiScale().y(12);
    tile->border = ResolveDxToPx(2);
    tile->radius = uiScale().y(8);
    tile->setFixedSize(cardWidth(), m_cardHeight);
    tile->setAccessibleName(tr("%1: %2, %3").arg(label, title, count));
    tile->setAccessibleDescription(tip);
    tile->setToolTip(tip);

    QVBoxLayout* lay = new QVBoxLayout(tile);
    lay->setContentsMargins(uiScale().y(16), uiScale().y(16), uiScale().y(16), uiScale().y(16));
    lay->setSpacing(uiScale().y(4));
    lay->addStretch(1);
    lay->addSpacing(tile->disc + tile->gap);
    QLabel* labelLabel = innerLabel(label, "qsTileLabel", tile);
    QFont labelFont = font();
    labelFont.setPixelSize(rolePx(FontRole::Small));
    labelFont.setWeight(QFont::DemiBold);
    labelFont.setCapitalization(QFont::AllUppercase);
    labelFont.setLetterSpacing(QFont::PercentageSpacing, 110);
    labelLabel->setFont(labelFont);
    QLabel* titleLabel = innerLabel(title, "qsTileTitle", tile);
    QFont titleFont = font();
    titleFont.setPixelSize(rolePx(FontRole::Large));
    titleFont.setWeight(QFont::Bold);
    titleLabel->setFont(titleFont);
    titleLabel->setWordWrap(true);
    QLabel* countLabel = innerLabel(count, "qsTileCount", tile);
    QFont mono(QStringLiteral("Hack"));
    mono.setPixelSize(rolePx(FontRole::Small));
    countLabel->setFont(mono);
    for (QLabel* l : { labelLabel, titleLabel, countLabel })
    {
        l->setAlignment(Qt::AlignHCenter);
        lay->addWidget(l);
    }
    lay->addStretch(1);
    tile->below = labelLabel;
    if (aCard)
        connect(tile, &QPushButton::clicked, this, &QuickstartPane::deal);
    else
        connect(tile, &QPushButton::clicked, this, &QuickstartPane::nextDeck);
    m_tile = tile;
    return tile;
}

CodeCard* QuickstartPane::addCard(int i)
{
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    const SonicPi::QuickstartCard& card = d.cards[i];
    CodeCard::Spec spec;
    spec.title = card.title;
    spec.code = card.code;
    spec.blurb = card.blurb;
    // Each card gets its own scope slot, wrapping within the cards' slot budget so a user-authored deck with more
    // cards than slots shares slots between cards rather than colliding with the live_loop range.
    spec.scopeSlot = kFirstScopeSlot + (unsigned(i) % kCardScopeSlots);
    // An edit is kept by the deck's and the card's names, so it follows the card when a set is reordered.
    spec.key = QStringLiteral("quickstart/%1/%2").arg(d.title, card.title);
    // One card size for every deck: the sizes computeGlobalLayout settled on.
    CodeCard::Metrics metrics;
    metrics.width = cardWidth();
    metrics.codeBodyHeight = m_codeBodyH;
    metrics.footerHeight = m_footerH;
    metrics.scopeSide = m_scopeSide;
    metrics.blurbWidth = m_blurbW;
    metrics.codeFontPx = m_codeFontPx;
    metrics.titlePx = rolePx(FontRole::Large);
    metrics.zoomFactor = m_zoomFactor;
    CodeCard* c = new CodeCard(spec, metrics, m_theme, m_row);
    c->setProperty("cardIndex", i);
    m_deck->add(c, QString("sonic-pi-quickstart-%1-%2").arg(m_progress.deck()).arg(i));
    // The name carries the blurb: a screen reader always speaks the name on focus, whereas the description (AXHelp)
    // is at the mercy of the user's hint-verbosity settings.
    const QString plainBlurb = QTextDocumentFragment::fromHtml(card.blurb).toPlainText().simplified();
    c->setAccessibleName(tr("%1 card, %2 of %3. %4").arg(card.title).arg(i + 1).arg(d.cards.size()).arg(plainBlurb));
    connect(c, &CodeCard::insertRequested, this, &QuickstartPane::insertRequested);
    connect(c, &CodeCard::copyRequested, this, &QuickstartPane::copyRequested);
    connect(c, &CodeCard::insertPreviewRequested, this, &QuickstartPane::insertPreviewRequested);
    connect(c, &CodeCard::insertPreviewCleared, this, &QuickstartPane::insertPreviewCleared);
    connect(c, &CodeCard::announceRequested, this, &QuickstartPane::announceRequested);
    connect(c, &CodeCard::dragEnded, this, &QuickstartPane::dragEnded);
    connect(c, &CodeCard::stepRequested, this, [this, c](int delta) { stepFrom(c, delta); });
    // The wheel over a card's code is shared with the pages (eventFilter).
    c->body()->viewport()->installEventFilter(this);
    return c;
}

QWidget* QuickstartPane::focusTarget(QWidget* item) const
{
    if (item && item->objectName() == QLatin1String("qsWelcome"))
        return m_start;
    return item;
}

bool QuickstartPane::focusOnCard() const
{
    QWidget* f = window() ? window()->focusWidget() : nullptr;
    return f && m_cardFrames.contains(f);
}

void QuickstartPane::focusCarousel()
{
    // Land on the page's first card, or the welcome card's Start, not a container: the card is a named group with
    // its keys, and says where it is; Start is told the deck it opens.
    if (m_items.isEmpty())
        return;
    if (QWidget* target = focusTarget(m_items.first()))
        target->setFocus(Qt::OtherFocusReason);
    sayPage(true);
}

// ── the controls ──────────────────────────────────────────────────────────────────────────────────────────────────

void QuickstartPane::chooseDeck(int deck)
{
    m_progress.choose(deck);
    render();
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    emit announceRequested(tr("%1 deck, %n cards", nullptr, d.cards.size()).arg(d.title));
}

void QuickstartPane::startDeck()
{
    const int n = m_progress.dealt();
    if (n == 0)
    {
        deal();
        return;
    }
    const int len = m_progress.cards();
    m_progress.startDeck(perPage());
    render(FocusTo::Card, n < len ? n - 1 : 0);   // the newest card, or all dealt the first
    sayPage(true);
}

void QuickstartPane::deal()
{
    const int n = m_progress.dealt();
    m_progress.deal(perPage());
    if (m_progress.dealt() > n)
        showDealt(n);
}

// Card n just dealt: the focus on it, which says which card it is; said, if the focus was elsewhere.
void QuickstartPane::showDealt(int n)
{
    render(FocusTo::Card, n);
    if (focusOnCard())
        return;
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    emit announceRequested(tr("Card %1 of %2: %3").arg(n + 1).arg(d.cards.size()).arg(d.cards[n].title));
}

void QuickstartPane::nextDeck()
{
    m_progress.nextDeck();
    showNewDeck();
}

// The next deck reached: its welcome card, the focus on its Start, and its name said.
void QuickstartPane::showNewDeck()
{
    render(FocusTo::Start);
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    emit announceRequested(tr("New deck: %1, %n cards", nullptr, d.cards.size()).arg(d.title));
}

void QuickstartPane::leave()
{
    m_progress.leave();
    render(FocusTo::Start);
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    emit announceRequested(tr("Decks. %1: the deck's introduction").arg(d.title));
}

void QuickstartPane::turn(int by)
{
    const bool moved = m_progress.turn(by, perPage());
    render();
    sayPage(moved);
}

void QuickstartPane::forward()
{
    const int n = m_progress.dealt();
    switch (m_progress.forward(perPage()))
    {
    case QuickstartProgress::Moved::Page:
        render();
        sayPage(true);
        break;
    case QuickstartProgress::Moved::Dealt:
        showDealt(n);
        break;
    case QuickstartProgress::Moved::NewDeck:
        showNewDeck();
        break;
    case QuickstartProgress::Moved::None:
        sayPage(false);
        break;
    }
}

void QuickstartPane::goToPage(int page)
{
    const bool moved = m_progress.goTo(page, perPage());
    render();
    sayPage(moved);
}

// Where a page landed, unless a card has the focus: the card's own name says which card it is.
void QuickstartPane::sayPage(bool moved)
{
    if (focusOnCard())
        return;
    const Deck& d = quickstartDecks(m_cardsPath)[m_progress.deck()];
    const int per = perPage();
    if (!moved)
    {
        emit announceRequested(m_progress.page(per) == 0 ? tr("First card.") : tr("Last card."));
        return;
    }
    const int first = m_progress.first(m_progress.page(per), per);
    emit announceRequested(first == 0 ? tr("%1: the deck's introduction").arg(d.title)
                                      : tr("Card %1 of %2").arg(first).arg(d.cards.size()));
}

void QuickstartPane::stepFrom(QWidget* item, int delta)
{
    const int at = m_items.indexOf(item);
    const int to = at + delta;
    if (at >= 0 && to >= 0 && to < m_items.size())
    {
        if (QWidget* target = focusTarget(m_items[to]))
            target->setFocus(Qt::OtherFocusReason);
        return;
    }
    // off the page's end: the page before, on the item before this one, or the next (or the next card dealt)
    const int per = perPage();
    const int firstItem = m_progress.first(m_progress.page(per), per);
    if (delta < 0)
    {
        if (!m_progress.turn(-1, per))
        {
            sayPage(false);
            return;
        }
        render();
        const int want = firstItem - 1;   // item 0 is the welcome card, item k card k - 1
        QWidget* target = want <= 0 ? m_start : nullptr;
        for (QWidget* f : m_cardFrames)
            if (f->property("cardIndex").toInt() == want - 1)
                target = f;
        if (target)
            target->setFocus(Qt::OtherFocusReason);
        sayPage(true);
        return;
    }
    const int lastItem = firstItem + int(m_items.size()) - 1;
    if (m_progress.page(per) < m_progress.pages(per) - 1)
    {
        m_progress.turn(1, per);
        render();
        QWidget* target = m_tile;
        for (QWidget* f : m_cardFrames)
            if (f->property("cardIndex").toInt() == lastItem)   // item lastItem + 1 is card lastItem
                target = f;
        if (target)
            target->setFocus(Qt::OtherFocusReason);
        sayPage(true);
        return;
    }
    forward();
}

void QuickstartPane::keyPressEvent(QKeyEvent* event)
{
    if (event->key() != Qt::Key_Left && event->key() != Qt::Key_Right)
    {
        QWidget::keyPressEvent(event);
        return;
    }
    const int delta = event->key() == Qt::Key_Right ? 1 : -1;
    // from an item on the page (the welcome card's Start, the tile): the item beside it; from anywhere else, a page
    QWidget* f = window() ? window()->focusWidget() : nullptr;
    for (QWidget* item : m_items)
    {
        if (f && (f == item || item->isAncestorOf(f)))
        {
            stepFrom(item, delta);
            return;
        }
    }
    if (delta > 0)
        forward();
    else
        turn(-1);
}

// The wheel turns the pages, one a gesture: the cooldown absorbs a trackpad swipe's momentum. It never deals a
// card: that is a press, of the tile or of Next.
void QuickstartPane::pageByWheel(const QWheelEvent* we)
{
    const int dx = we->angleDelta().x();
    const int dy = we->angleDelta().y();
    bool next = false;
    // angleDelta folds in the system's natural-scrolling setting, so read it as a scrollbar delta: negative x scrolls
    // the content rightward, i.e. a macOS natural swipe left goes on to the next cards.
    if (qAbs(dx) > qAbs(dy) && qAbs(dx) > 2)
        next = dx < 0;
    else if (qAbs(dy) > 2)
        next = dy < 0; // vertical wheel: down = forward
    else
        return;
    if (m_wheelCooldown)
        return;
    m_wheelCooldown = true;
    QTimer::singleShot(320, this, [this] { m_wheelCooldown = false; });
    turn(next ? 1 : -1);
}

void QuickstartPane::wheelEvent(QWheelEvent* event)
{
    pageByWheel(event);
    event->accept();
}

bool QuickstartPane::eventFilter(QObject* obj, QEvent* event)
{
    // A card's code only claims the wheel along an axis it can scroll; otherwise the gesture turns the pages, so a
    // snippet that fits (almost all of them) still turns them from anywhere on the card.
    if (event->type() == QEvent::Wheel)
    {
        if (QWidget* vp = qobject_cast<QWidget*>(obj))
        {
            QScrollArea* sa = qobject_cast<QScrollArea*>(vp->parentWidget());
            if (sa && sa->objectName() == QLatin1String("qsCardBody") && vp == sa->viewport())
            {
                QWheelEvent* we = static_cast<QWheelEvent*>(event);
                const bool horizontal = qAbs(we->angleDelta().x()) > qAbs(we->angleDelta().y());
                const QScrollBar* bar = horizontal ? sa->horizontalScrollBar() : sa->verticalScrollBar();
                if (bar && bar->minimum() != bar->maximum())
                    return false; // the code scrolls it
                pageByWheel(we);
                return true;
            }
        }
    }
    return QWidget::eventFilter(obj, event);
}
