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

#include <QApplication>
#include <QCursor>
#include <QDateTime>
#include <QFile>
#include <QFileInfo>
#include <QRegularExpression>
#include <QDrag>
#include <QFrame>
#include <QGridLayout>
#include <QIcon>
#include <QFontMetrics>
#include <QFontMetricsF>
#include <QPainter>
#include <QPainterPath>
#include <QPen>
#include <QTransform>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QMimeData>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QPropertyAnimation>
#include <QVariantAnimation>
#include <QAccessible>
#include <QAccessibleWidget>
#include <QPushButton>
#include <QTextDocumentFragment>
#include <QScrollArea>
#include <QScrollBar>
#include <QStyle>
#include <QTimer>
#include <QVBoxLayout>

#include <QPointer>

#include <cmath>

#include <QSvgRenderer>

#include "dpi.h"
#include "utils/fontroles.h"
#include "model/sonicpitheme.h"
#include "utils/tablericons.h"
#include "widgets/tutorialwidgets.h" // TutHeading (accessible section titles)
#include "widgets/carddeck.h"
#include "widgets/codecard.h"
#include "widgets/zoombar.h"
#include "utils/flash_style.h"
#include "utils/reducedmotion.h"
#include "utils/tutorialdocs.h"

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
    QString description; // one-line intro shown at the top of the deck
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

QuickstartPane::QuickstartPane(SonicPiTheme* theme, QWidget* parent)
    : QWidget(parent), m_theme(theme)
{
    registerTutorialWidgetAccessibility(); // deck titles are TutHeadings
    setAttribute(Qt::WA_StyledBackground, true);
    // Deck switcher runs down the left as a side column, so it costs no
    // vertical space and more card rows are visible.
    QHBoxLayout* layout = new QHBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    // The cards' runs: layered, as a performance deck's loops stack. Their
    // hover too, polled only while the pane shows.
    m_deck = new CardDeck(this, CardDeck::Playing::Layered);
    connect(m_deck, &CardDeck::runRequested, this, &QuickstartPane::runRequested);
    connect(m_deck, &CardDeck::stopJobRequested, this, &QuickstartPane::stopJobRequested);

    rebuild();
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
}

void QuickstartPane::applyTheme()
{
    // The deck rebuild re-creates every card widget, so it's by far the
    // heaviest single step of a whole-app re-theme. A hidden Cards tab can
    // take it lazily on next show instead of on every theme/hue change.
    if (isVisible())
        rebuild();
    else
        m_themeDirty = true;
    if (m_zoomBar)
        m_zoomBar->applyTheme();
}

QWidget* QuickstartPane::zoomControls()
{
    if (!m_zoomBar)
    {
        m_zoomBar = new ZoomBar(m_theme, tr("quickstart"), this);
        connect(m_zoomBar, &ZoomBar::zoomStep, this,
                [this](int delta) { setUserZoom(m_userZoom + delta); });
    }
    return m_zoomBar;
}

int QuickstartPane::preferredDockHeight() const
{
    // Start tall enough for the deck bar, the description and one full card.
    int chrome = uiScale().y(24); // grid margins around the card
    if (m_topBar)
        chrome += m_topBar->sizeHint().height();
    if (m_navRow)
        chrome += m_navRow->sizeHint().height();
    if (m_deckDesc)
    {
        QFont descFont;
        descFont.setPixelSize(rolePx(FontRole::Base));
        chrome += 2 * QFontMetrics(descFont).height() + uiScale().y(12); // two lines
    }
    return m_cardHeight + chrome;
}

int QuickstartPane::cardWidth() const
{
    // Sized so kMaxCols monospace columns fit without shrinking the code font
    // (grown in step with kMaxCols: 41 cols needs ~441).
    return qRound(ScaleHeightForDPI(441) * m_zoomFactor);
}

void QuickstartPane::computeGlobalLayout()
{
    if (m_codeBodyH > 0 && m_layoutZoom == m_userZoom)
        return; // only the (width-independent) zoom invalidates these
    m_layoutZoom = m_userZoom;

    // The card is a fixed-size format; content is authored to fit these
    // budgets rather than the card growing to fit content.
    const int kMaxCols = 41;   // widest code line a card is designed to hold
    const int kCodeLines = 6;  // tallest snippet a card is designed to hold
    const int kBlurbLines = 2; // blurb runs full width, so it wraps in fewer lines

    const int pad = uiScale().y(14);
    // The frame's fixed width includes the 2dx #qsCard border (app.qss), so
    // the body gets less than cardWidth() to lay out in.
    const int border = ResolveDxToPx(2);
    const int codeAvail = cardWidth() - 2 * pad - 2 * border;
    m_blurbW = codeAvail; // the description now spans the full card width

    // Code font: the requested (zoomed) size, capped so kMaxCols monospace
    // columns fit the card width. An authored line wider than that is still
    // reachable (the body scrolls), but sizing to the budget is what keeps
    // scrolling the exception rather than the reading experience.
    int codePx = qMax(8, qRound(ScaleHeightForDPI(15) * m_zoomFactor));
    QFont hack(QStringLiteral("Hack"));
    hack.setPixelSize(codePx);
    const qreal colW = QFontMetricsF(hack).horizontalAdvance(QLatin1Char('m'));
    if (colW * kMaxCols > codeAvail)
        // Floor stays unscaled: it is the "fit an authored line" backstop, so
        // letting it rise with zoom would defeat the shrink-to-fit it guards.
        codePx = qMax(ScaleHeightForDPI(9), int(codePx * codeAvail / (colW * kMaxCols)));
    m_codeFontPx = codePx;

    // The code lays out through a QTextDocument, which at some font sizes
    // rounds a pixel taller than QFontMetrics::height(); a 1px/line
    // underestimate overflows the fixed body. Measured as a card lays it out.
    m_codeBodyH = 2 * uiScale().y(10) + CodeCard::codeLinesHeight(kCodeLines, m_codeFontPx);

    // Footer: a full-height scope on the left, and to its right the
    // description (two lines) over the Run/Add buttons.
    QFont sans;
    sans.setPixelSize(m_codeFontPx); // the blurb renders at the code's size
    const int blurbLineH = QFontMetrics(sans).height();
    // The footer holds the description (two lines) and, on the right, the scope
    // which is the play/stop control. The scope is inset from the footer's
    // inner height so its rings never touch (get clipped by) the edges.
    const int innerH = kBlurbLines * blurbLineH + uiScale().y(58);
    m_footerH = 2 * uiScale().y(8) + innerH;
    m_scopeSide = innerH - uiScale().y(16);
    // The blurb yields the full hit-box width (scope + its surrounding
    // margin), not just the scope square — see the scopeHit box in addCard.
    m_blurbW = codeAvail - innerH - uiScale().y(12);
}

bool QuickstartPane::eventFilter(QObject* obj, QEvent* event)
{
    // The viewport's Resize is the only reliable "the layout gave me my real
    // size" signal: rebuild() runs while a fresh scroll area is still at its
    // 100px default, and even a singleShot(0) can lose the race with the
    // posted layout pass.
    if (event->type() == QEvent::Resize && m_scroll && obj == m_scroll->viewport())
        updateCarousel();
    // The invisible left-margin strip pages back (see updateBackEdge for its
    // enabled/cursor state).
    if (obj == m_backEdge && event->type() == QEvent::MouseButtonPress
        && m_backEdge->isEnabled()
        && static_cast<QMouseEvent*>(event)->button() == Qt::LeftButton)
    {
        goToPage(m_pageIndex - 1);
        return true;
    }
    // A card's code body only claims the wheel along an axis it can actually
    // scroll; otherwise the gesture falls through to the carousel below, so a
    // snippet that fits (almost all of them) still pages the deck from
    // anywhere on the card.
    if (event->type() == QEvent::Wheel && m_scroll)
    {
        if (QWidget* vp = qobject_cast<QWidget*>(obj))
        {
            QScrollArea* sa = qobject_cast<QScrollArea*>(vp->parentWidget());
            if (sa && sa->objectName() == QLatin1String("qsCardBody")
                && vp == sa->viewport())
            {
                QWheelEvent* we = static_cast<QWheelEvent*>(event);
                const bool horizontal
                    = qAbs(we->angleDelta().x()) > qAbs(we->angleDelta().y());
                const QScrollBar* bar
                    = horizontal ? sa->horizontalScrollBar() : sa->verticalScrollBar();
                if (bar && bar->minimum() != bar->maximum())
                    return false; // the body scrolls it
                // Nothing to scroll along that axis: re-aim at the carousel.
                obj = m_scroll;
            }
        }
    }
    // Side-scrolling pages the carousel one card at a time (never free-scrolls).
    // A cooldown absorbs a trackpad swipe's momentum so one gesture = one card.
    if (event->type() == QEvent::Wheel && m_scroll
        && (obj == m_scroll || obj == m_scroll->viewport()))
    {
        QWheelEvent* we = static_cast<QWheelEvent*>(event);
        const int dx = we->angleDelta().x();
        const int dy = we->angleDelta().y();
        bool act = true, next = false;
        // angleDelta folds in the system's natural-scrolling setting, so read it
        // as a scrollbar delta (same as dy below): negative x scrolls the content
        // rightward; i.e. a macOS natural swipe left advances to the next cards.
        if (qAbs(dx) > qAbs(dy) && qAbs(dx) > 2)
            next = dx < 0;
        else if (qAbs(dy) > 2)
            next = dy < 0; // vertical wheel: down = forward
        else
            act = false;
        if (act && !m_wheelCooldown)
        {
            goToPage(m_pageIndex + (next ? 1 : -1));
            m_wheelCooldown = true;
            QTimer::singleShot(320, this, [this] { m_wheelCooldown = false; });
        }
        return true;
    }
    // Left/Right with focus on the scroll area drop the selection onto the
    // leading visible card; from there each press moves the selection one
    // card at a time (the selected-card branch below) and the deck scrolls
    // to keep it in view — the nav bar follows the selection, not the other
    // way round.
    if (event->type() == QEvent::KeyPress && m_scroll
        && (obj == m_scroll || obj == m_scroll->viewport()))
    {
        const int key = static_cast<QKeyEvent*>(event)->key();
        if (key == Qt::Key_Left || key == Qt::Key_Right)
        {
            if (QWidget* card = firstVisibleCard())
            {
                card->setFocus(Qt::OtherFocusReason);
                return true;
            }
            goToPage(m_pageIndex + (key == Qt::Key_Right ? 1 : -1));
            return true;
        }
    }
    // Everything else about a card — a click, its keys, being dragged — is
    // the card's own (CodeCard).
    return QWidget::eventFilter(obj, event);
}

namespace
{
// The left "page back" zone: invisible at rest; entering it fades in a slim
// header-green bar with a ‹ chevron, centred in the margin. The click target
// is the whole margin (lane reaches the first card's edge, no dead gap);
// only the painted bar is slim. Clicks are handled by
// QuickstartPane::eventFilter.
class BackZone : public QWidget
{
public:
    QColor fill, chev;
    int contentTop = 0;    // the card row's top offset, so the bar aligns with the cards
    int contentHeight = 0; // the shared card height (0 = span the whole strip)

    explicit BackZone(QWidget* parent)
        : QWidget(parent)
    {
        m_fade = new QVariantAnimation(this);
        // Linear: an eased curve front-loads the opacity ramp so hard the
        // fade-in reads as a snap.
        m_fade->setEasingCurve(QEasingCurve::Linear);
        connect(m_fade, &QVariantAnimation::valueChanged, this, [this](const QVariant& v) {
            m_opacity = v.toReal();
            update();
        });
    }

protected:
    void enterEvent(QEnterEvent*) override { fadeTo(1.0); }
    void leaveEvent(QEvent*) override { fadeTo(0.0); }
    void paintEvent(QPaintEvent*) override
    {
        if (!isEnabled() || m_opacity <= 0.01)
            return; // invisible until hovered (and inert on page one)
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);
        p.setOpacity(m_opacity);
        const int top = contentTop;
        const int h = contentHeight > 0 ? qMin(contentHeight, height() - top) : height() - top;
        const qreal w = ScaleWidthForDPI(16); // slim bar; the click zone stays full-width
        // Snug against the first card's edge (its 2px transparent border sits
        // just past the lane), keeping the breathing room on the tab side.
        const QRectF r(width() - w - ScaleWidthForDPI(3), top, w, h);
        p.setPen(Qt::NoPen);
        p.setBrush(fill);
        const qreal rad = ScaleHeightForDPI(6);
        p.drawRoundedRect(r, rad, rad);
        // The one chevron design (ChevronButton), on the bar.
        ChevronButton::paintChevron(p, r.center(), ChevronButton::Left, chev);
    }

private:
    void fadeTo(qreal target)
    {
        if (SonicPi::prefersReducedMotion())
        {
            m_opacity = target;
            update();
            return;
        }
        m_fade->stop();
        if (target < m_opacity)
        {
            // Leaving: snap out.
            m_opacity = target;
            update();
            return;
        }
        m_fade->setDuration(500);
        m_fade->setStartValue(m_opacity);
        m_fade->setEndValue(target);
        m_fade->start();
    }

    QVariantAnimation* m_fade;
    qreal m_opacity = 0.0;
};
} // namespace

void QuickstartPane::resizeEvent(QResizeEvent* event)
{
    QWidget::resizeEvent(event);
    updateHeaderForHeight();
    updateCarousel(); // a width change alters how many whole cards fit per page
}

void QuickstartPane::updateHeaderForHeight()
{
    if (!m_deckDesc)
        return;
    // Keep the deck bar and a whole card visible; spend any height left over on
    // the description row, hiding it (and its accent rail) when it won't fit.
    // Reserve a fixed two lines (not the per-deck text height) so every deck's
    // description shows or hides at the same pane height; otherwise a longer,
    // two-line blurb (Basics) would hide while a one-line one (FX) stayed.
    const int deckBarH = m_topBar ? m_topBar->sizeHint().height() : 0;
    const int cardRow = m_cardHeight + uiScale().y(20);
    QFont descFont;
    descFont.setPixelSize(rolePx(FontRole::Base));
    const int descH = 2 * QFontMetrics(descFont).height() + uiScale().y(12);
    const bool showDesc = height() - deckBarH - cardRow >= descH;
    m_deckDesc->setVisible(showDesc);
    if (m_headerRail)
        m_headerRail->setVisible(showDesc);
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

void QuickstartPane::runOutput(int jobId, const QString& text)
{
    m_deck->runOutput(jobId, text);
}

void QuickstartPane::runError(int jobId, const QString& message, int line)
{
    m_deck->runError(jobId, message, line);
}

void QuickstartPane::stepFrom(QWidget* card, int delta)
{
    const int idx = m_cardFrames.indexOf(card);
    if (idx < 0)
        return;
    const int next = idx + delta;
    if (next < 0 || next >= m_cardFrames.size())
    {
        emit announceRequested(next < 0 ? tr("First card.") : tr("Last card."));
        return;
    }
    QWidget* to = m_cardFrames.value(next);
    if (!to)
        return;
    scrollCardIntoView(to);
    to->setFocus(Qt::OtherFocusReason);
}

void QuickstartPane::setUserZoom(int zoom)
{
    m_userZoom = qBound(kFontZoomMin, zoom, kFontZoomMax);
    m_zoomFactor = FontZoomFactor(m_userZoom);
    rebuild();
}

// Shared curve (utils/fontroles.h): multiplicative, so the card's type
// hierarchy holds its proportions as it zooms. The old `base + m_userZoom`
// added the same pixel to every size, which pulled a 17px heading down
// toward a 13px pill the further you zoomed in.
int QuickstartPane::rolePx(FontRole role) const
{
    return qMax(8, qRound(FontRolePx(role) * m_zoomFactor));
}

void QuickstartPane::rebuild()
{
    // Clear the whole layout: rebuild() is called from the deck pills / zoom
    // buttons, so deleteLater (not delete) avoids destroying the widget whose
    // click we are still handling. Takes the side column and the right
    // container (header + scroll) so nothing accumulates.
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
    m_topBar = nullptr;
    m_navRow = nullptr;
    m_scroll = nullptr;
    m_cardRow = nullptr;
    m_cardGrid = nullptr;
    m_cardFrames.clear();
    m_gridSpacer = nullptr;
    m_gridRows = 0;
    m_prevArrow = nullptr;
    m_nextArrow = nullptr;
    m_backEdge = nullptr;
    m_dotsHost = nullptr;
    m_dotsLayout = nullptr;
    m_headerRail = nullptr;
    m_deckTitle = nullptr;
    m_deckDesc = nullptr;
    m_deck->clear();

    const QVector<Deck>& decks = quickstartDecks(m_cardsPath);
    if (decks.isEmpty())
        return;
    if (m_deckIdx >= decks.size())
        m_deckIdx = 0;

    const QColor accent = m_theme->color("HighlightedBackground");

    // A single vertical stack: a full-width deck bar (the deck selector) over
    // the deck description over the cards.
    QWidget* rightSide = new QWidget(this);
    QVBoxLayout* rightCol = new QVBoxLayout(rightSide);
    rightCol->setContentsMargins(0, 0, 0, 0);
    rightCol->setSpacing(0);

    // Deck bar: selector pills on the left, the carousel pill centred, zoom on
    // the right. The highlighted pill is the current deck (it doubles as title).
    m_topBar = new QWidget(rightSide);
    // Containers a screen reader walks through get names — an anonymous
    // "group" stop tells the user nothing about where they are.
    m_topBar->setAccessibleName(tr("Deck selector"));
    QHBoxLayout* deckBar = new QHBoxLayout(m_topBar);
    deckBar->setContentsMargins(uiScale().y(14), uiScale().y(8),
                                uiScale().y(12), uiScale().y(3));
    deckBar->setSpacing(uiScale().y(6));
    for (int i = 0; i < decks.size(); ++i)
    {
        QPushButton* pill = new QPushButton(decks[i].title, m_topBar);
        pill->setObjectName(QStringLiteral("qsDeckPill"));
        pill->setProperty("current", i == m_deckIdx);
        pill->setCursor(Qt::PointingHandCursor);
        pill->setAccessibleName(tr("%1 card deck").arg(decks[i].title));
        // Follows this pane's zoom, as the docs pills follow theirs.
        pill->setStyleSheet(QString("font-size: %1px;").arg(rolePx(FontRole::Base)));
        connect(pill, &QPushButton::clicked, this, [this, i] {
            // Each deck keeps its own carousel position: stash where this deck
            // was, restore where the target deck last was (first card if new).
            m_deckPages[m_deckIdx] = m_pageIndex;
            m_deckIdx = i;
            m_pageIndex = m_deckPages.value(i, 0);
            rebuild();
            // Hand focus to the carousel so the Left/Right keys drive it
            // straight away after choosing a deck, and say what arrived.
            if (m_scroll)
                m_scroll->setFocus(Qt::OtherFocusReason);
            const QVector<Deck>& d = quickstartDecks(m_cardsPath);
            if (m_deckIdx < d.size())
                emit announceRequested(tr("%1 deck, %2 cards")
                                           .arg(d[m_deckIdx].title)
                                           .arg(d[m_deckIdx].cards.size()));
        });
        deckBar->addWidget(pill);
    }
    deckBar->addStretch(1);

    // Carousel controls: prev chevron, page dots and next chevron in one
    // grouped pill. The chevrons are the one chevron control (ChevronButton),
    // as every chevron is: no knob of their own inside the pill, the muted
    // ink at rest and the accent under the pointer.
    const QColor arrowInk = SonicPiTheme::blend(m_theme->color("Foreground"),
                                                m_theme->color("PaneBackground"), 0.45);
    auto makeArrow = [&](ChevronButton::Dir dir, const QString& a11y) {
        auto* b = new ChevronButton(m_topBar);
        b->setObjectName(QStringLiteral("qsArrow"));
        b->setDir(dir);
        b->setColors(Qt::transparent, Qt::transparent, arrowInk, m_theme->color("HighlightedBackground"));
        b->setAccessibleName(a11y);
        b->setFixedSize(uiScale().size(30, 30));
        return b;
    };
    m_prevArrow = makeArrow(ChevronButton::Left, tr("Previous cards"));
    m_nextArrow = makeArrow(ChevronButton::Right, tr("More cards"));
    connect(m_prevArrow, &QAbstractButton::clicked, this, [this] { goToPage(m_pageIndex - 1); });
    connect(m_nextArrow, &QAbstractButton::clicked, this, [this] { goToPage(m_pageIndex + 1); });

    m_dotsHost = new QWidget(m_topBar);
    m_dotsLayout = new QHBoxLayout(m_dotsHost);
    m_dotsLayout->setContentsMargins(uiScale().y(6), 0, uiScale().y(6), 0);
    m_dotsLayout->setSpacing(uiScale().y(8));

    QWidget* nav = new QWidget(m_topBar);
    nav->setObjectName(QStringLiteral("qsNav"));
    nav->setAccessibleName(tr("Card navigation"));
    QHBoxLayout* navLay = new QHBoxLayout(nav);
    navLay->setContentsMargins(uiScale().y(4), uiScale().y(2),
                               uiScale().y(4), uiScale().y(2));
    navLay->setSpacing(uiScale().y(2));
    // Pin the height rather than deriving it: the radius has to be exactly half
    // of what the widget ACTUALLY ends up as, and the layout was giving this
    // row 30px where the contents-plus-margins calculation said 27 — leaving
    // the pill a pixel-and-a-half short of round. Fixing the height makes the
    // two agree by construction. Even, so the halving is exact.
    int navH = uiScale().y(30) + 2 * uiScale().y(2);
    navH += navH % 2;
    nav->setFixedHeight(navH);
    nav->setStyleSheet(
        QStringLiteral("QWidget#qsNav { border-radius: %1px; }").arg(navH / 2));
    navLay->addWidget(m_prevArrow);
    navLay->addWidget(m_dotsHost);
    navLay->addWidget(m_nextArrow);
    // nav is added to its own centred row below the cards (see below).
    // A-/A+ zoom lives at the foot of the help's tab rail (zoomControls()), not here.
    rightCol->addWidget(m_topBar);

    // Description row: a small accent rail and the deck's one-line description.
    // Hidden first as the pane shrinks (updateHeaderForHeight).
    m_deckTitle = nullptr;
    m_deckDesc = nullptr;
    m_headerRail = nullptr;
    const QString desc = decks[m_deckIdx].description;
    if (!desc.isEmpty())
    {
        QWidget* descRow = new QWidget(rightSide);
        QHBoxLayout* descLay = new QHBoxLayout(descRow);
        descLay->setContentsMargins(uiScale().y(14), uiScale().y(4), uiScale().y(14),
                                    uiScale().y(9));
        descLay->setSpacing(uiScale().y(10));
        QWidget* rail = new QWidget(descRow);
        rail->setObjectName(QStringLiteral("qsHeaderRail"));
        int railW = uiScale().y(4);
        railW += railW % 2;   // even: the radius must be exactly half (see rebuildDots)
        rail->setFixedWidth(railW);
        rail->setStyleSheet(
            QStringLiteral("QWidget#qsHeaderRail { border-radius: %1px; }").arg(railW / 2));
        descLay->addWidget(rail);
        m_headerRail = rail;
        QLabel* descLabel = new QLabel(desc, descRow);
        descLabel->setObjectName(QStringLiteral("qsDeckDesc"));
        descLabel->setWordWrap(true);
        descLabel->setStyleSheet(QString("font-size: %1px;").arg(rolePx(FontRole::Base)));
        descLay->addWidget(descLabel, 1);
        m_deckDesc = descLabel;
        rightCol->addWidget(descRow);
    }

    // Cards live on a single horizontal row with no scrollbar and no wheel
    // scrolling. You move through the deck a whole page at a time (all the
    // cards that fit), snapping to card boundaries so a card is never left
    // half on screen. The header arrows and dots drive it.
    m_scroll = new QScrollArea(rightSide);
    m_scroll->setWidgetResizable(false); // content keeps its natural width so it can overflow
    m_scroll->setFrameShape(QFrame::NoFrame);
    m_scroll->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_scroll->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_scroll->setFocusPolicy(Qt::StrongFocus); // hold focus so Left/Right page it
    m_scroll->setAccessibleName(tr("%1 cards").arg(decks[m_deckIdx].title));
    m_scroll->setAccessibleDescription(
        tr("Left and Right arrow keys move through the cards."));
    // Focusing the pane (F6 cycling, focusPane) lands on the current card —
    // rebuildDots keeps the proxy pointing at the page's first card.
    setFocusProxy(m_scroll);
    QWidget* content = new QWidget(m_scroll);
    content->setAccessibleName(tr("Cards"));
    m_cardRow = content;
    m_cardGrid = new QGridLayout(content);
    const int pad = uiScale().y(14);
    // No left margin: the back zone's lane provides the left pad, so the whole
    // margin up to the first card is one contiguous click target.
    m_cardGrid->setContentsMargins(0, uiScale().y(8), pad, pad);
    m_cardGrid->setHorizontalSpacing(uiScale().y(14));
    m_cardGrid->setVerticalSpacing(uiScale().y(14));

    // One set of dimensions for every card in every deck.
    computeGlobalLayout();

    const QVector<SonicPi::QuickstartCard>& cards = decks[m_deckIdx].cards;
    m_cardCount = cards.size();
    m_cardFrames.clear();
    for (int i = 0; i < cards.size(); ++i)
    {
        // Each card gets its own scope slot, wrapping within the cards' slot
        // budget so a user-authored deck with more cards than slots shares
        // slots between cards rather than colliding with the live_loop range.
        // An edit is kept by the deck's and the card's names, so it follows
        // the card when a set is reordered.
        QWidget* f = addCard(cards[i],
                             QString("sonic-pi-quickstart-%1-%2").arg(m_deckIdx).arg(i),
                             kFirstScopeSlot + (i % kCardScopeSlots),
                             QStringLiteral("quickstart/%1/%2").arg(decks[m_deckIdx].title, cards[i].title));
        // The name carries the blurb: a screen reader always speaks the name
        // on focus, whereas the description (AXHelp) is at the mercy of the
        // user's hint-verbosity settings.
        const QString plainBlurb =
            QTextDocumentFragment::fromHtml(cards[i].blurb).toPlainText().simplified();
        f->setAccessibleName(tr("%1 card, %2 of %3. %4")
                                 .arg(cards[i].title)
                                 .arg(i + 1)
                                 .arg(cards.size())
                                 .arg(plainBlurb));
        m_cardFrames << f;
    }
    // Trailing spacer: sits in the grid column after the last card and holds
    // the right-hand pad so the final page still snaps to a card boundary
    // (no half cards) without the grid spreading the cards apart.
    m_gridSpacer = new QWidget(content);
    m_gridSpacer->setFixedWidth(0);
    m_gridRows = 0; // force the first updateCarousel to place the grid
    m_cardHeight = m_cardFrames.isEmpty() ? 0 : m_cardFrames.first()->sizeHint().height();

    m_scroll->setWidget(content);
    m_scroll->installEventFilter(this);
    m_scroll->viewport()->installEventFilter(this);

    // Left-edge "page back" affordance: the right side is easy (click the card
    // peeking in), but a full page snaps flush-left with nothing to click going
    // back. The whole left margin is the click target, right up to the first
    // card's edge (the grid's left pad lives in this lane, not the grid).
    // Invisible at rest; hovering reveals a soft wash + chevron (see
    // BackZone). Click handled in eventFilter.
    BackZone* backZone = new BackZone(rightSide);
    backZone->fill = accent; // the card headers' green
    backZone->chev = m_theme->accentContrastText();
    m_backEdge = backZone;
    m_backEdge->setFixedWidth(uiScale().x(26) + uiScale().y(14));
    m_backEdge->setFocusPolicy(Qt::NoFocus);
    // No accessible name: unnamed, it's pruned from the accessibility tree.
    // It's a mouse-only convenience that duplicates the Previous arrow — to
    // a screen reader it was just one more mystery stop.
    m_backEdge->installEventFilter(this);

    QHBoxLayout* scrollRow = new QHBoxLayout;
    scrollRow->setContentsMargins(0, 0, 0, 0);
    scrollRow->setSpacing(0);
    scrollRow->addWidget(m_backEdge);
    scrollRow->addWidget(m_scroll, 1);
    rightCol->addLayout(scrollRow, 1);

    // The row must always rest with a full card flush left. Qt scrolls the
    // area behind our back (e.g. auto-revealing a clicked button inside a
    // partly-visible card), so once the scrollbar settles anywhere off a card
    // boundary, snap to the nearest page.
    QTimer* settle = new QTimer(m_scroll);
    settle->setSingleShot(true);
    settle->setInterval(160);
    connect(m_scroll->horizontalScrollBar(), &QScrollBar::valueChanged, settle,
            [settle] { settle->start(); });
    connect(settle, &QTimer::timeout, this, [this] {
        if (!m_scroll)
            return;
        const int step = cardWidth() + uiScale().y(14);
        QScrollBar* sb = m_scroll->horizontalScrollBar();
        const int p = qBound(0, qRound(double(sb->value()) / step), pageCount() - 1);
        // Same clamp goToPage applies, so an unreachable boundary can't make
        // this re-fire forever.
        const int target = qBound(sb->minimum(), p * step, sb->maximum());
        if (sb->value() != target)
            goToPage(p);
        else if (p != m_pageIndex)
        {
            m_pageIndex = p; // keep the dots honest if something scrolled silently
            rebuildDots();
        }
    });

    // Carousel control on its own full-width row beneath the cards, centred.
    QWidget* navRow = new QWidget(rightSide);
    QHBoxLayout* navRowLay = new QHBoxLayout(navRow);
    navRowLay->setContentsMargins(0, uiScale().y(4), 0, uiScale().y(6));
    navRowLay->addStretch(1);
    navRowLay->addWidget(nav);
    navRowLay->addStretch(1);
    rightCol->addWidget(navRow);
    m_navRow = navRow;

    m_pageIndex = qBound(0, m_pageIndex, qMax(0, pageCount() - 1));
    static_cast<QHBoxLayout*>(layout())->addWidget(rightSide, 1);
    // The viewport size (and so rows/columns per page) isn't final until the
    // pane is laid out; compute it now and again once the geometry settles.
    updateCarousel();
    QTimer::singleShot(0, this, [this] { updateCarousel(); });
    updateHeaderForHeight();
}

int QuickstartPane::rowsThatFit() const
{
    if (!m_scroll || m_cardHeight <= 0)
        return 1;
    const int gap = uiScale().y(14);
    const int marginsV = uiScale().y(8) + uiScale().y(14);
    const int vh = m_scroll->viewport()->height() - marginsV;
    int rows = qMax(1, (vh + gap) / (m_cardHeight + gap));
    if (m_cardCount > 0)
        rows = qMin(rows, m_cardCount);
    return rows;
}

int QuickstartPane::cardsPerView() const
{
    if (!m_scroll)
        return 1;
    const int step = cardWidth() + uiScale().y(14);
    const int vw = m_scroll->viewport()->width();
    return qMax(1, (vw + uiScale().y(14)) / step);
}

int QuickstartPane::pageCount() const
{
    // The carousel moves one card (column) at a time, so the number of scroll
    // stops is the columns that don't fit, plus one (each stop shows a full
    // set of whole cards, shifted along by one).
    const int rows = rowsThatFit();
    const int totalCols = m_cardCount > 0 ? (m_cardCount + rows - 1) / rows : 1;
    const int cpv = cardsPerView();
    return qMax(1, totalCols - cpv + 1);
}

void QuickstartPane::rebuildDots()
{
    if (!m_dotsLayout || !m_dotsHost || !m_prevArrow || !m_nextArrow)
        return;
    const int pages = pageCount();
    m_pageIndex = qBound(0, m_pageIndex, pages - 1);
    const int rows = rowsThatFit();
    const int cpv = cardsPerView();

    // One dot per card; every card currently on screen lights accent, so the
    // lit run of dots shows how many cards are visible and where you are.
    // The dots are created once per rebuild and restyled in place on page
    // changes: destroying them would yank the very button a keyboard or
    // screen-reader user just pressed out from under their focus (which then
    // falls all the way back to the window).
    if (m_dotsLayout->count() != m_cardCount)
    {
        while (QLayoutItem* it = m_dotsLayout->takeAt(0))
        {
            if (QWidget* w = it->widget())
                w->deleteLater();
            delete it;
        }
        // Even, so the radius below is exactly half. Qt does not clamp a
        // radius to half the box — it paints artifacts above that and visible
        // corners below (see the scale in dpi.h) — so only an exact half
        // renders a true circle. An odd box makes that impossible in integer
        // pixels, which is why rounding either way alternated circle/square
        // as zoom nudged the size.
        int dot = uiScale().y(10);
        dot += dot % 2;
        for (int i = 0; i < m_cardCount; ++i)
        {
            QPushButton* d = new QPushButton(m_dotsHost);
            d->setObjectName(QStringLiteral("qsDot"));
            d->setCursor(Qt::PointingHandCursor);
            d->setFixedSize(dot, dot);
            // Exactly half the (even) box — see the size calculation above.
            d->setStyleSheet(
                QStringLiteral("QPushButton#qsDot { border-radius: %1px; }").arg(dot / 2));
            d->setAccessibleName(tr("Scroll to card %1 of %2").arg(i + 1).arg(m_cardCount));
            // Row count (and so this card's column) can change between
            // presses; resolve at click time.
            connect(d, &QPushButton::clicked, this, [this, i] {
                goToPage(qBound(0, i / qMax(1, rowsThatFit()), pageCount() - 1));
            });
            m_dotsLayout->addWidget(d);
        }
    }
    for (int i = 0; i < m_dotsLayout->count(); ++i)
    {
        QWidget* d = m_dotsLayout->itemAt(i)->widget();
        if (!d)
            continue;
        const int col = rows > 0 ? i / rows : i;
        const bool visible = col >= m_pageIndex && col < m_pageIndex + cpv;
        if (d->property("lit").toBool() != visible)
        {
            d->setProperty("lit", visible);
            repolish(d);
        }
    }
    m_dotsHost->setVisible(pages > 1);
    m_prevArrow->setVisible(pages > 1);
    m_nextArrow->setVisible(pages > 1);
    m_prevArrow->setEnabled(m_pageIndex > 0);
    m_nextArrow->setEnabled(m_pageIndex < pages - 1);
    // Keep the pane's focus entry point on the page's current card (see
    // focusCarousel for why a card, not the scroll area).
    QWidget* entry = firstVisibleCard();
    setFocusProxy(entry ? entry : static_cast<QWidget*>(m_scroll));
    updateBackEdge();
}

void QuickstartPane::updateBackEdge()
{
    if (!m_backEdge)
        return;
    // The margin keeps its slot (fixed width, no layout shift); only its
    // clickability and hover-reveal come and go with the page.
    const bool canBack = m_pageIndex > 0;
    BackZone* bz = static_cast<BackZone*>(m_backEdge);
    // Wash aligns with the cards' PAINTED extent: the frame reserves a 2px
    // transparent hover border top and bottom, so inset past it.
    const int borderW = uiScale().y(2);
    bz->contentTop = uiScale().y(8) + borderW;
    bz->contentHeight = qMax(0, m_cardHeight - 2 * borderW);
    bz->setEnabled(canBack);
    bz->setCursor(canBack ? Qt::PointingHandCursor : Qt::ArrowCursor);
    bz->setToolTip(canBack ? tr("Click to see the previous cards.") : QString());
    bz->update();
}

void QuickstartPane::relayoutGrid(int rows)
{
    if (!m_cardGrid)
        return;
    // Take every card out of the grid (the card widgets survive; only the
    // layout items are freed) and re-place them column-major, so paging left
    // and right moves through whole columns.
    while (QLayoutItem* it = m_cardGrid->takeAt(0))
        delete it;
    for (int i = 0; i < m_cardFrames.size(); ++i)
        m_cardGrid->addWidget(m_cardFrames[i], i % rows, i / rows,
                              Qt::AlignTop | Qt::AlignLeft);
    const int totalCols = m_cardFrames.isEmpty() ? 1 : (m_cardFrames.size() + rows - 1) / rows;
    if (m_gridSpacer)
        m_cardGrid->addWidget(m_gridSpacer, 0, totalCols, qMax(1, rows), 1);
}

void QuickstartPane::updateCarousel()
{
    if (!m_scroll || !m_cardGrid || !m_cardRow)
        return;
    const int gap = uiScale().y(14);
    const int pad = uiScale().y(14);
    const int step = cardWidth() + gap;

    // Use as many rows as fit the height, then page horizontally by columns.
    const int rows = rowsThatFit();
    if (rows != m_gridRows)
    {
        relayoutGrid(rows);
        m_gridRows = rows;
    }
    const int totalCols = m_cardCount > 0 ? (m_cardCount + rows - 1) / rows : 1;
    const int pages = pageCount();
    m_pageIndex = qBound(0, m_pageIndex, pages - 1);

    // Pad the trailing spacer so the last stop still lands on a card boundary
    // (no half cards). The spacer absorbs the slack so the cards themselves
    // stay tightly packed rather than the grid spreading them apart.
    const int vw = m_scroll->viewport()->width();
    // Right margin only; the left pad lives in the back zone's lane.
    const int gridW = totalCols > 0 ? totalCols * cardWidth() + (totalCols - 1) * gap + pad : 0;
    const int lastOffset = (pages - 1) * step;
    if (m_gridSpacer)
        m_gridSpacer->setFixedWidth(qMax(0, lastOffset + vw - gridW));
    // Recompute the grid immediately: adjustSize() otherwise reads a stale
    // size hint from before the spacer resize, leaving the content too narrow
    // for the last page to land on a card boundary.
    m_cardGrid->invalidate();
    m_cardGrid->activate();
    m_cardRow->adjustSize();

    rebuildDots();
    // Snap straight to the current stop (no animation; this is a relayout).
    m_scroll->horizontalScrollBar()->setValue(m_pageIndex * step);
}

QWidget* QuickstartPane::firstVisibleCard() const
{
    const int rows = qMax(1, rowsThatFit());
    return m_cardFrames.value(m_pageIndex * rows, nullptr);
}

void QuickstartPane::focusCarousel()
{
    // Land on the current card, not the scroll area: the card is a named
    // group ("Play a note card, 1 of 31") with the Space/I actions, whereas
    // the scroll area reads as an anonymous container a screen-reader user
    // has to dig into.
    if (QWidget* card = firstVisibleCard())
    {
        card->setFocus(Qt::OtherFocusReason);
        return; // the card's accessible name already carries its position
    }
    if (m_scroll)
    {
        m_scroll->setFocus(Qt::OtherFocusReason);
        announcePage();
    }
}

void QuickstartPane::goToPage(int page)
{
    if (!m_scroll)
        return;
    const int step = cardWidth() + uiScale().y(14);
    const int pages = pageCount();
    // Linear deck: the ends are hard stops.
    const int prevIndex = m_pageIndex;
    m_pageIndex = qBound(0, page, pages - 1);

    QScrollBar* sb = m_scroll->horizontalScrollBar();
    const int target = qBound(sb->minimum(), m_pageIndex * step, sb->maximum());
    // With a screen reader connected, snap instead of glide: focus moves to
    // the new page's card immediately, and an animation would leave the
    // reader's focus rectangle drawn where the card was mid-slide.
    if (SonicPi::prefersReducedMotion() || QAccessible::isActive())
    {
        sb->setValue(target);
    }
    else
    {
        QPropertyAnimation* anim = new QPropertyAnimation(sb, "value", sb);
        anim->setDuration(240);
        anim->setStartValue(sb->value());
        anim->setEndValue(target);
        anim->setEasingCurve(QEasingCurve::OutCubic);
        anim->start(QAbstractAnimation::DeleteWhenStopped);
    }
    rebuildDots(); // recolour dots + arrow state for the page we moved to
    if (m_pageIndex != prevIndex)
        announcePage();
}

// Tell a screen reader where paging landed. Always speaks per-card ("Card 6
// of 10") — a screen reader experiences one card at a time, so the visual
// page range ("cards 6 to 10") is meaningless to it.
void QuickstartPane::announcePage()
{
    if (m_cardCount <= 0)
        return;
    // When focus is riding the cards (keyboard/screen-reader paging), the
    // newly focused card announces itself.
    QWidget* focused = QApplication::focusWidget();
    if (focused && m_cardFrames.contains(focused))
        return;
    const int lead = m_pageIndex * qMax(1, rowsThatFit());
    if (lead >= m_cardCount)
        return;
    emit announceRequested(tr("Card %1 of %2").arg(lead + 1).arg(m_cardCount));
}

void QuickstartPane::scrollCardIntoView(QWidget* frame)
{
    const int idx = m_cardFrames.indexOf(frame);
    if (idx < 0)
        return;
    const int rows = qMax(1, rowsThatFit());
    const int col = idx / rows;
    const int cpv = cardsPerView();
    // The card sits in column `col`; the fully-visible columns are
    // [m_pageIndex, m_pageIndex + cpv). If it's outside (only peeking), page so
    // it becomes flush at the near edge.
    if (col < m_pageIndex)
        goToPage(col);
    else if (col >= m_pageIndex + cpv)
        goToPage(col - cpv + 1);
}

CodeCard* QuickstartPane::addCard(const SonicPi::QuickstartCard& card, const QString& workspace,
                                  int scopeSlot, const QString& key)
{
    CodeCard::Spec spec;
    spec.title = card.title;
    spec.code = card.code;
    spec.blurb = card.blurb;
    spec.scopeSlot = static_cast<unsigned int>(scopeSlot);
    spec.key = key;
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
    CodeCard* c = new CodeCard(spec, metrics, m_theme);
    m_deck->add(c, workspace);
    connect(c, &CodeCard::insertRequested, this, &QuickstartPane::insertRequested);
    connect(c, &CodeCard::copyRequested, this, &QuickstartPane::copyRequested);
    connect(c, &CodeCard::insertPreviewRequested, this, &QuickstartPane::insertPreviewRequested);
    connect(c, &CodeCard::insertPreviewCleared, this, &QuickstartPane::insertPreviewCleared);
    connect(c, &CodeCard::announceRequested, this, &QuickstartPane::announceRequested);
    connect(c, &CodeCard::dragEnded, this, &QuickstartPane::dragEnded);
    connect(c, &CodeCard::stepRequested, this, [this, c](int delta) { stepFrom(c, delta); });
    // A plain click on a partly-visible card scrolls it fully into view; the
    // same as pressing the arrow toward it.
    connect(c, &CodeCard::clicked, this, [this, c] { scrollCardIntoView(c); });
    // The wheel over a card's code is shared with the carousel (eventFilter).
    c->body()->viewport()->installEventFilter(this);

    return c;
}
