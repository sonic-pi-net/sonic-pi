#include "widgets/zoombar.h"

#include <QEvent>
#include <QHBoxLayout>
#include <QPushButton>

#include "dpi.h"
#include "model/sonicpitheme.h"
#include "utils/tablericons.h"

ZoomBar::ZoomBar(SonicPiTheme* theme, const QString& subject, QWidget* parent)
    : QWidget(parent)
    , m_theme(theme)
{
    QHBoxLayout* lay = new QHBoxLayout(this);
    lay->setContentsMargins(0, 0, 0, 0);
    // Match the title row's control spacing so the gap between -/+ equals the
    // gap on to the shared close ✕: three evenly spaced buttons in a row.
    lay->setSpacing(ScaleWidthForDPI(4));
    // Flat chrome comes from the #zoomBtn rule in app.qss, which keeps the
    // app-wide QPushButton padding/height/border from leaking in.

    const int px = ScaleWidthForDPI(26);
    auto make = [&](QPushButton*& btn, const QString& a11y, int delta) {
        btn = new QPushButton(this);
        btn->setObjectName("zoomBtn");
        btn->setCursor(Qt::PointingHandCursor);
        btn->setFocusPolicy(Qt::NoFocus);
        btn->setToolTip(a11y);
        btn->setAccessibleName(a11y);
        btn->setIconSize(QSize(px, px));
        btn->installEventFilter(this);
        connect(btn, &QPushButton::clicked, this, [this, delta] { emit zoomStep(delta); });
        lay->addWidget(btn);
    };
    make(m_out, tr("Make %1 text smaller").arg(subject), -1);
    make(m_in, tr("Make %1 text larger").arg(subject), 1);
    applyTheme();
}

void ZoomBar::applyTheme()
{
    const QColor accent = m_theme->color("HighlightedBackground");
    const QColor muted = m_theme->mutedForeground();
    const int px = ScaleWidthForDPI(26);
    const qreal dpr = devicePixelRatioF();
    m_outIcon = TablerIcons::icon(TablerIcons::Glyph::CircleMinus, muted, px, dpr);
    m_outHover = TablerIcons::icon(TablerIcons::Glyph::CircleMinus, accent, px, dpr);
    m_inIcon = TablerIcons::icon(TablerIcons::Glyph::CirclePlus, muted, px, dpr);
    m_inHover = TablerIcons::icon(TablerIcons::Glyph::CirclePlus, accent, px, dpr);
    m_out->setIcon(m_out->underMouse() ? m_outHover : m_outIcon);
    m_in->setIcon(m_in->underMouse() ? m_inHover : m_inIcon);
}

bool ZoomBar::eventFilter(QObject* obj, QEvent* event)
{
    if ((obj == m_out || obj == m_in)
        && (event->type() == QEvent::Enter || event->type() == QEvent::Leave))
    {
        const bool en = event->type() == QEvent::Enter;
        if (obj == m_out)
            m_out->setIcon(en ? m_outHover : m_outIcon);
        else
            m_in->setIcon(en ? m_inHover : m_inIcon);
    }
    return QWidget::eventFilter(obj, event);
}
