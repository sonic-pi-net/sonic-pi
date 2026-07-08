//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//++

#ifndef THEME_CARD_H
#define THEME_CARD_H

#include <QPushButton>
#include <QPainter>
#include <QPaintEvent>
#include <QEnterEvent>

// A checkable card button for the theme picker. Paints its own antialiased
// rounded rect + border (QSS rounded borders alias badly at the corners), with
// the resting / hover / checked border states. Child labels (glyphs + name)
// render on top.
class ThemeCard : public QPushButton
{
public:
    using QPushButton::QPushButton;

    void setCardColors(const QColor& bg, const QColor& border)
    {
        m_bg = bg;
        m_border = border;
        update();
    }

    // The selected/hover border colour — the current theme's accent, so the
    // highlighted card matches the scheme instead of a fixed pink.
    void setHighlight(const QColor& c)
    {
        m_highlight = c;
        update();
    }

protected:
    void enterEvent(QEnterEvent*) override { update(); }
    void leaveEvent(QEvent*) override { update(); }

    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing, true);

        const qreal rad = 7.0;
        qreal bw = 2.0;
        QColor bc = m_border;
        if (isChecked())        { bc = m_highlight;            bw = 2.5; }
        else if (underMouse())  { bc = m_highlight; bc.setAlpha(170); }

        const QRectF r = QRectF(rect()).adjusted(bw / 2.0, bw / 2.0, -bw / 2.0, -bw / 2.0);
        p.setPen(QPen(bc, bw));
        p.setBrush(m_bg);
        p.drawRoundedRect(r, rad, rad);
    }

private:
    QColor m_bg{ Qt::transparent };
    QColor m_border{ QColor(127, 127, 127, 90) };
    QColor m_highlight{ QColor(255, 20, 147) };   // deeppink fallback until themed
};

#endif // THEME_CARD_H
