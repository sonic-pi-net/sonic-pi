#pragma once

#include <QGuiApplication>
#include <QFont>
#include <QScreen>
#include <QRegularExpression>

#include <cmath>

// House corner radius (in dx) for pill-shaped controls: the docs/info nav
// chips, the card-deck selector pills, the docs filter field and the pill
// selection rows. One value keeps every pill on the same curvature, sized so
// the smallest pill stays above twice the radius (Qt paints artifacts once a
// radius exceeds half the widget height). Stylesheets consume it via the
// `pillRadius` token substituted in SonicPiTheme::reloadStylesheet().
constexpr int kPillRadiusDx = 12;

inline QSizeF GetDisplayScale()
{
//super hacky temporary fudge to paper over the
//massive cracks that is the difference between how
//macOS and other platforms handle high DPI monitors

#if defined(Q_OS_WIN)
  float scale = 96.0f * 1.6f;
#elif defined(Q_OS_MAC)
  float scale = 96.0f;
#else
  //assuming linux
  float scale = 96.0f * 1.2f;
#endif


  QSizeF scaleDpi = QSizeF(scale, scale);
    if (const QScreen* pScreen = QGuiApplication::primaryScreen())
    {
        scaleDpi.setWidth(pScreen->logicalDotsPerInchX());
        scaleDpi.setHeight(pScreen->logicalDotsPerInchY());
    }

    return QSizeF(scaleDpi.width() / scale, scaleDpi.height() / scale);
}

inline QSize ScaleForDPI(const QSize& sz)
{
    auto scale = GetDisplayScale();
    return QSize(scale.width() * sz.width(), scale.height() * sz.height());
}

inline QSize ScaleForDPI(int x, int y)
{
    auto scale = GetDisplayScale();
    return QSize(scale.width() * x, scale.height() * y);
}

inline int ScaleHeightForDPI(int y)
{
  if (y <= 0)
  {
    return 0;
  }

  // ensure returned value is at least 1
  auto scale = GetDisplayScale();
  return (scale.height() * y) + 1;
}

inline int ScaleYDeltaForDPI(int y)
{
  auto scale = GetDisplayScale();
  return (scale.height() * y);
}

inline int ScaleWidthForDPI(int x)
{
  if (x <= 0)
  {
    return 0;
  }

  // ensure returned value is at least 1
  auto scale = GetDisplayScale();
  return (scale.width() * x) + 1;
}

// ── Type scale ───────────────────────────────────────────────────────────
// The GUI's font sizes, and the one zoom curve every A-/A+ control uses.
//
// These live here rather than in app.qss because a stylesheet `font-size`
// silently beats setFont(), which made per-pane text zoom undiscoverable: a
// widget was scalable or not depending on whether some unrelated rule
// happened to match it. With no font-size in the stylesheet, setFont() is
// always the answer and a pane's zoom composes by construction.
//
// The stylesheet's `small`/`medium`/… keywords resolve through this same
// table (see ScalePxInStyleSheet below), so there is one set of numbers.
enum class FontRole
{
    Tiny,      // 9px: the Link widget's per-control section labels
    PaneTitle, // 11px Hack: DEBUG / TO SUPERSONIC / … strip titles
    Small,     // subordinate notes, scope labels, dock chrome
    Base,      // the app default — every ordinary label, button and field
    Large,
    XLarge,
    XXLarge,
    Arrow, // the cards deck's chevron glyphs
};

// One curve for every pane that offers A-/A+ (docs, cards, …). Multiplicative
// so a type hierarchy keeps its proportions as it grows — the cards pane's
// old additive step (base + zoom) added the same pixel to every size, which
// flattened a 17px heading toward a 13px pill the further you zoomed in.
constexpr int kFontZoomMin = -4;
constexpr int kFontZoomMax = 8;

inline double FontZoomFactor(int step)
{
    if (step < kFontZoomMin)
        step = kFontZoomMin;
    if (step > kFontZoomMax)
        step = kFontZoomMax;
    const double f = std::pow(1.1, step);
    return f < 0.5 ? 0.5 : (f > 3.0 ? 3.0 : f);
}

// Design sizes at 1x zoom. Tiny/PaneTitle were literal `px` in the stylesheet
// (not `dx`), so they are deliberately not DPI-scaled — keeping the resting
// appearance identical to the rules these replaced.
inline int FontRolePx(FontRole role, double scale = 1.0)
{
    int px = 0;
    switch (role)
    {
    case FontRole::Tiny:
        px = 9;
        break;
    case FontRole::PaneTitle:
        px = 11;
        break;
    case FontRole::Arrow:
        px = ScaleHeightForDPI(26);
        break;
#ifdef __APPLE__
    case FontRole::Small:   px = ScaleHeightForDPI(13); break;
    case FontRole::Base:    px = ScaleHeightForDPI(18); break;
    case FontRole::Large:   px = ScaleHeightForDPI(21); break;
    case FontRole::XLarge:  px = ScaleHeightForDPI(25); break;
    case FontRole::XXLarge: px = ScaleHeightForDPI(31); break;
#else
    case FontRole::Small:   px = ScaleHeightForDPI(14); break;
    case FontRole::Base:    px = ScaleHeightForDPI(19); break;
    case FontRole::Large:   px = ScaleHeightForDPI(22); break;
    case FontRole::XLarge:  px = ScaleHeightForDPI(26); break;
    case FontRole::XXLarge: px = ScaleHeightForDPI(32); break;
#endif
    }
    const int scaled = int(px * scale + 0.5);
    return scaled < 6 ? 6 : scaled;
}

// A QFont carries EITHER a point size or a pixel size; the unused one reads
// back as -1. The app default is pixel-sized (FontRolePx above, matching what
// the stylesheet used to set), so `f.setPointSizeF(f.pointSizeF() * 0.8)` on
// an inherited font silently yields a negative size and a Qt warning. Derive
// relative sizes through these instead — they work in whichever unit the font
// actually uses.
inline double FontSizeValue(const QFont& f)
{
    return f.pointSizeF() > 0 ? f.pointSizeF() : double(f.pixelSize());
}

inline void SetFontSizeValue(QFont& f, double value, double minimum = 6.0)
{
    if (value < minimum)
        value = minimum;
    if (f.pointSizeF() > 0)
        f.setPointSizeF(value);
    else
        f.setPixelSize(int(value + 0.5));
}

// Convenience: the same font, resized by a factor of its current size.
inline QFont ScaledFont(const QFont& base, double factor, double minimum = 6.0)
{
    QFont f = base;
    SetFontSizeValue(f, FontSizeValue(base) * factor, minimum);
    return f;
}

// Zoom-aware pixel metrics: the display DPI scale times a pane's own text
// zoom. Lay every content size out through one of these and a page built at
// 2x text gets 2x padding and column widths to sit in — scaling the font
// alone is what left labels overlapping their neighbours.
class UiScale
{
public:
    UiScale() = default;
    explicit UiScale(double factor)
        : m_factor(factor)
    {
    }
    static UiScale fromZoom(int step) { return UiScale(FontZoomFactor(step)); }

    double factor() const { return m_factor; }

    int x(int px) const { return px <= 0 ? 0 : scaled(ScaleWidthForDPI(px)); }
    int y(int px) const { return px <= 0 ? 0 : scaled(ScaleHeightForDPI(px)); }
    QSize size(int w, int h) const { return QSize(x(w), y(h)); }
    int font(FontRole role) const { return FontRolePx(role, m_factor); }

private:
    int scaled(int px) const
    {
        const int v = int(px * m_factor + 0.5);
        return v < 1 ? 1 : v;
    }
    double m_factor = 1.0;
};

// The stylesheet's `dx` unit: a design pixel scaled for the display. Values
// above 29 are bucketed so the sheet reads in round numbers — the buckets are
// preserved exactly from the sixty-line per-value replacement chain this
// replaces (1-29 and 35 exact, then nearest 10, then the 100s stops).
inline int ResolveDxToPx(int dx)
{
    if (dx <= 0)
        return 0;
    if (dx <= 29 || dx == 35)
        return ScaleHeightForDPI(dx);
    if (dx < 100)
        return ScaleHeightForDPI((dx / 10) * 10);
    if (dx < 110)
        return ScaleHeightForDPI(100);
    if (dx < 120)
        return ScaleHeightForDPI(110);
    if (dx < 150)
        return ScaleHeightForDPI(125);
    if (dx < 200)
        return ScaleHeightForDPI(150);
    return ScaleHeightForDPI((dx / 100) * 100);
}

// Resolve a stylesheet's `dx` lengths and font-size keywords to real pixels.
//
// extraScale is an additional multiplier for panes that carry their own text
// zoom on top of the display DPI (the docs pane's A-/A+) — padding, radii and
// rule weights then grow with the type instead of pinching it. One function
// rather than two overloads so a value can't resolve differently depending on
// which one happened to see it.
inline QString ScalePxInStyleSheet(QString style, double extraScale = 1.0)
{
  static const QRegularExpression dxUnit(QStringLiteral(":\\s*(\\d+)dx"));
  QString out;
  int last = 0;
  QRegularExpressionMatchIterator it = dxUnit.globalMatch(style);
  while (it.hasNext())
  {
    const QRegularExpressionMatch match = it.next();
    out += QStringView(style).mid(last, match.capturedStart() - last);
    const int px = ResolveDxToPx(match.captured(1).toInt());
    out += QString(": %1px").arg(px <= 0 ? 0 : qMax(1, qRound(px * extraScale)));
    last = match.capturedEnd();
  }
  out += QStringView(style).mid(last);

  // Keywords resolve through FontRolePx so the stylesheet and setFont() can
  // never disagree about what `medium` means.
  const struct { const char* keyword; FontRole role; } kFontKeywords[] = {
    { "xx-large", FontRole::XXLarge },
    { "x-large",  FontRole::XLarge  },
    { "large",    FontRole::Large   },
    { "medium",   FontRole::Base    },
    { "small",    FontRole::Small   },
  };
  for (const auto& kw : kFontKeywords)
  {
    out = out.replace(
        QRegularExpression(QString("font-size:\\s*%1\\s*;").arg(kw.keyword)),
        QString("font-size: %1px; /*%2*/").arg(FontRolePx(kw.role, extraScale)).arg(kw.keyword));
  }
  return out;
}
