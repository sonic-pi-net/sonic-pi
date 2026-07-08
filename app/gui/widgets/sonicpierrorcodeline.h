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

#ifndef SONICPIERRORCODELINE_H
#define SONICPIERRORCODELINE_H

#include <QWidget>
#include <QPainter>
#include <QPainterPath>
#include <QFontMetrics>
#include <QString>
#include <QColor>
#include <QtMath>

// A single line of monospace code with a muted line-number gutter and a zig-zag
// (squiggle) underline drawn beneath the offending [colStart,colEnd) token.
// Custom-painted because Qt rich text can't draw a wavy underline. Byte columns
// are treated as character indices, which lines up for the (ASCII) source Sonic
// Pi code normally is.
class SonicPiErrorCodeLine : public QWidget
{
public:
    explicit SonicPiErrorCodeLine(QWidget* parent = nullptr) : QWidget(parent) {}

    // lineNum (<=0 to hide) draws a gutter in contextColor. contextColor paints
    // the non-erroneous code (comment grey), tokenColor the offending span
    // (full-strength foreground), accent the zig-zag beneath it.
    void setContent(const QString& line, int colStart, int colEnd, int lineNum,
                    const QColor& contextColor, const QColor& tokenColor, const QColor& accent)
    {
        m_line = line;
        while (m_line.endsWith('\n') || m_line.endsWith('\r'))
            m_line.chop(1);
        m_cs = colStart;
        m_ce = colEnd;
        m_lineNum = lineNum;
        m_text = contextColor;
        m_tokText = tokenColor;
        m_accent = accent;
        updateGeometry();
        update();
    }

    QSize sizeHint() const override
    {
        QFontMetrics fm(font());
        // Height covers the text plus the zig-zag sitting kGap below the
        // descent (see paintEvent) with its amplitude and stroke.
        const int wave = kGap + qCeil(2 * kAmp + kStroke);
        return QSize(int(gutterWidth(fm)) + fm.horizontalAdvance(m_line) + 2, fm.height() + wave);
    }

protected:
    void paintEvent(QPaintEvent*) override
    {
        QPainter p(this);
        p.setFont(font());
        QFontMetrics fm(font());
        const int baseline = fm.ascent();

        const qreal codeX = gutterWidth(fm);
        if (m_lineNum > 0)
        {
            p.setPen(m_text);
            p.drawText(QPointF(0, baseline), QString::number(m_lineNum));
            QColor sep = m_text;
            sep.setAlpha(110);
            p.setPen(sep);
            const qreal sepX = codeX - kGutterPad;
            p.drawLine(QPointF(sepX, 1), QPointF(sepX, height() - 2));
        }

        int cs = m_cs;
        int ce = m_ce;
        const bool hasTok = (cs >= 0 && ce > cs && cs < m_line.length());
        if (hasTok)
        {
            cs = qBound(0, cs, m_line.length());
            ce = qBound(cs, ce, m_line.length());
        }
        const QString pre = hasTok ? m_line.left(cs) : m_line;
        const QString tok = hasTok ? m_line.mid(cs, ce - cs) : QString();
        const QString post = hasTok ? m_line.mid(ce) : QString();

        qreal x = codeX;
        // With no marked span the whole line is the error — keep it full
        // strength rather than receding it into the context grey.
        p.setPen(hasTok ? m_text : m_tokText);
        p.drawText(QPointF(x, baseline), pre);
        x += fm.horizontalAdvance(pre);

        const qreal tokX0 = x;
        if (hasTok)
        {
            p.setPen(m_tokText);
            p.drawText(QPointF(x, baseline), tok);
            x += fm.horizontalAdvance(tok);
        }
        const qreal tokX1 = x;

        p.setPen(m_text);
        p.drawText(QPointF(x, baseline), post);

        if (hasTok && tokX1 > tokX0)
            drawZigzag(p, tokX0, tokX1, baseline + fm.descent() + kGap + kAmp, m_accent);
    }

private:
    // Zig-zag geometry — keep in step with the editor's enlarged squiggle in
    // QScintilla_src-*/scintilla/src/Indicator.cpp so both renderings match.
    static constexpr qreal kAmp = 3.0;     // peak height above/below the centre line
    static constexpr qreal kHalf = 5.0;    // half a wavelength
    static constexpr qreal kStroke = 2.0;  // line thickness
    static constexpr int kGap = 6;         // px between the descent and the wave top
    static constexpr qreal kGutterPad = 10.0;  // padding either side of the gutter separator

    qreal gutterWidth(const QFontMetrics& fm) const
    {
        if (m_lineNum <= 0)
            return 0;
        return fm.horizontalAdvance(QString::number(m_lineNum)) + kGutterPad * 2;
    }

    static void drawZigzag(QPainter& p, qreal x0, qreal x1, qreal y, const QColor& c)
    {
        p.save();
        p.setRenderHint(QPainter::Antialiasing, true);
        QPen pen(c, kStroke);
        pen.setJoinStyle(Qt::MiterJoin);
        p.setPen(pen);
        QPainterPath path;
        bool up = true;
        path.moveTo(x0, y + kAmp);
        for (qreal x = x0; x < x1;)
        {
            const qreal nx = qMin(x + kHalf, x1);
            path.lineTo(nx, y + (up ? -kAmp : kAmp));
            up = !up;
            x = nx;
        }
        p.drawPath(path);
        p.restore();
    }

    QString m_line;
    int m_cs = -1;
    int m_ce = -1;
    int m_lineNum = -1;
    QColor m_text;
    QColor m_tokText;
    QColor m_accent;
};

#endif
