//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>
#include <QPalette>
#include <QIcon>
class SonicPiTheme : public QObject
{
Q_OBJECT
public:
    // The colour scheme is the only theme axis. Each scheme is just a theme map.
    // The icon set (Classic vs Pro) is a fully independent choice carried
    // separately as a bool, so every scheme pairs with either icon set. There is
    // deliberately no combined "style" type — the two axes never merge.
    enum ColourScheme {
        LightScheme, DarkScheme, HighContrastScheme,
        MildDarkScheme, PhosphorScheme, SignalScheme
    };

    static QString colourSchemeToName(ColourScheme scheme);
    static ColourScheme colourSchemeFromName(QString name);

    // Global filters layered over the active scheme. Grouped so they can be
    // reset, and later saved as a preset, as one unit.
    static constexpr int kHueSpreadDefault = 0;     // authored theme, unmodified
    static constexpr int kHueSpreadEven    = 100;   // hues spaced evenly

    // Global hue rotation (degrees) applied to every colour the theme resolves.
    void setHueRotation(int degrees);
    // How far the hues open out from the accent. 0 reproduces the theme exactly,
    // 100 spaces them evenly. Even spacing is the ceiling, so a hue cannot wrap
    // past its neighbour at any setting.
    void setHueSpread(int amount);
    // A colour as it would render at a candidate spread, for the prefs dial
    // preview. Spread leaves the primary fixed, so the dial samples a secondary.
    // Spread and rotation are both passed in rather than read from stored state,
    // which only updates on release, so the swatch tracks either dial mid-drag.
    QColor previewWithSpread(QColor c, int amount, int hueRotation) const;
    // The theme colour for a key WITHOUT the global hue/monochrome transform
    // (e.g. for a preview that wants to apply its own rotation).
    QColor rawColor(QString key);
    // Global monochrome (greyscale) toggle over every colour the theme resolves.
    void setMonochrome(bool on);
    // Global colour inversion (photo-negative) over every colour the theme resolves.
    void setInvert(bool on);

    explicit SonicPiTheme(QObject *parent = 0, QString customSettingsFilename="", QString rootPath = "");
    ~SonicPiTheme();
    QColor color(QString);
    // Black or white, whichever reads better on `bg` (by perceived brightness).
    // For text placed on an accent fill whose lightness varies per theme.
    QColor contrastingText(const QColor& bg) const;
    // Linear mix of two colours (t = 0 -> a, t = 1 -> b). The shared helper for
    // widgets deriving in-between tones from theme tokens.
    static QColor blend(const QColor& a, const QColor& b, double t)
    {
        return QColor(qRound(a.red() * (1 - t) + b.red() * t),
                      qRound(a.green() * (1 - t) + b.green() * t),
                      qRound(a.blue() * (1 - t) + b.blue() * t));
    }
    // Derived colour roles: one definition each, shared by the QSS tokens
    // (reloadStylesheet) and any C++ painting that needs the same tone.
    QColor softForeground();     // foreground -> pane emphasis ramp: 10%
    QColor mutedForeground();    // 30%
    QColor faintForeground();    // 62%
    QColor ghostForeground();    // 82%
    QColor subtleFill();         // pane nudged 7% toward the foreground
    QColor accentTint();         // pane washed 6% toward the accent
    QColor accentTintStrong();   // pane washed 14% toward the accent
    QColor accentContrastText(); // auto-contrast text on the accent
    // Applies the global invert / monochrome / hue-rotation transforms to an
    // arbitrary colour (the shared pipeline behind color(); also used for literal
    // colours and for previewing each theme card under the active toggles).
    QColor applyGlobalTransforms(QColor c) const;
    // The same pipeline with the transforms supplied explicitly, for callers
    // that need a hue other than the member state (e.g. previewing a candidate
    // rotation mid-drag). applyGlobalTransforms() delegates here.
    static QColor applyColourTransforms(QColor c, bool invert, bool monochrome, int hueRotation);
    QString font(QString);
    void darkMode();
    void lightMode();
    void hcMode();
    void mildDarkMode();
    void phosphorMode();
    void signalMode();
    void updateCustomSettings();
    QPalette createPalette();

    void reloadStylesheet();
    QString getAppStylesheet();

    QString getCss();
    void applyTheme(ColourScheme scheme, bool proIcons);
    QString getName();
    ColourScheme getColourScheme();
    bool getProIcons();

    QIcon getRunIcon();
    QIcon getStopIcon();
    QIcon getSaveAsIcon();
    QIcon getLoadIcon();
    QIcon getTextIncIcon();
    QIcon getTextDecIcon();

    QIcon getHelpIcon(bool active);
    QIcon getRecIcon(bool on, bool ab);
    QIcon getPrefsIcon(bool active);
    QIcon getInfoIcon(bool active);
    QIcon getScopeIcon(bool active);

    // The square brand mark, rendered from vector at `devicePx` wide and
    // tinted with the theme accent — the same mask treatment the boot splash
    // uses (tile filled, glyphs punched through), except the fill tracks the
    // live theme instead of the fixed brand pink. Because the accent already
    // runs through the global colour transforms, the mark follows the
    // monochrome / invert / hue settings for free.
    QImage logoMark(int devicePx);

private:
    QString name;
    ColourScheme colourScheme;
    bool proIcons;
    int m_hueRotation = 0;
    int m_hueSpread = kHueSpreadDefault;
    bool m_monochrome = false;
    bool m_invert = false;
    // Rebuilt per theme: the accent hue the spacing pivots on, plus each
    // authored hue mapped to its position under even spacing.
    int m_hueBase = -1;
    QMap<int, int> m_hueEven;
    void rebuildHueSpreadMap();
    // Interpolates one colour's hue along authored -> evenly spaced.
    QColor applyHueSpread(QColor c, int amount) const;
    QString stylesheet;
    QString m_cssTemplate;   // cached disk-read + DPI-scaled .qss (colours filled per re-theme)
    // Cached disk-read + DPI-scaled doc-styles .css per source file (the scaled
    // text depends only on the file, not the theme — colours are swapped in later
    // by getCss()).
    QHash<QString, QString> m_docCssCache;
    // getRecIcon() results per (on, ab) frame: the recording flash timer requests
    // the same frames twice a second and generating one is expensive (per-pixel
    // recolour or resource reload + tint). Cleared whenever its inputs change
    // (re-theme / icon set / global colour filters).
    QHash<int, QIcon> m_recIconCache;

    QString customSettingsFilename;
    QString rootPath;
    QString qt_app_theme_path;
    QString qt_browser_dark_css;
    QString qt_browser_light_css;
    QString qt_browser_hc_css;

    QString css;

    QMap<QString, QString> withCustomSettings(QMap<QString, QString> settings);
    QMap<QString, QString> lightTheme();
    QMap<QString, QString> darkTheme();
    QMap<QString, QString> highContrastTheme();
    // Dark, desaturated. Base for the derived schemes so an unset key renders
    // grey rather than inheriting a plausible colour. See the definition.
    QMap<QString, QString> neutralBaseTheme();
    QMap<QString, QString> mildDarkTheme();
    QMap<QString, QString> phosphorTheme();
    QMap<QString, QString> signalTheme();
    // Assigns all toolbar icon pointers for a scheme + icon-set pair (colours
    // are set separately by the *Mode() helpers).
    void applyIcons(ColourScheme scheme, bool proIcons);
    // Recolours a single-colour glyph PNG to a theme colour (masked by alpha).
    QIcon tintedIcon(const QString& resource, const QColor& colour);
    // Runs a classic (hand-designed, uniform) toolbar icon through a per-theme hue
    // offset (rotating the art's baked accent onto this theme's accent) and then
    // the global colour filters, at its native size — keeping the original art so
    // its border and uniform pill shape are preserved. When logoContrast is set,
    // the glyph-box logo is additionally recoloured black/white for best contrast
    // against its (light or dark) box background. When wordOnAccent is set (the
    // recording flash frames, whose word box is accent-filled), the light word
    // text is recoloured black/white to contrast the accent too.
    QIcon classicIcon(const QIcon* icon, bool logoContrast = false, bool wordOnAccent = false);
    // Degrees to pre-rotate the classic icon art so its baked deep-pink accent
    // lands on this theme's accent hue. Explicit via the "ClassicIconHueOffset"
    // theme key, else derived from the accent. Applied before the global filters.
    int classicIconHueOffset() const;
    // Generates a checked-checkbox indicator (box in the theme accent, white
    // tick) tinted to the current highlight colour and returns an absolute file
    // path for the QSS. Cached per accent colour under the temp dir; the QSS
    // (checkbox-checked static PNG) can't follow the theme on its own.
    QString checkboxCheckedImagePath();
    QMap<QString, QString> theme;
    QMap<QString, QString> customSettings;

    QString readFile(QString name);
    // The doc-styles .css for a source path, disk-read + DPI-scaled once and
    // cached (see m_docCssCache).
    QString scaledDocCss(const QString& path);

    void loadToolBarIcons();

    QIcon* runIcon;
    QIcon* stopIcon;
    QIcon* saveAsIcon;
    QIcon* loadIcon;
    QIcon* textIncIcon;
    QIcon* textDecIcon;

    QIcon* helpIcon;
    QIcon* helpIconActive;
    QIcon* recIcon;
    QIcon* recIconA;
    QIcon* recIconB;
    QIcon* prefsIcon;
    QIcon* prefsIconActive;
    QIcon* infoIcon;
    QIcon* infoIconActive;
    QIcon* scopeIcon;
    QIcon* scopeIconActive;

    QIcon pro_run_icon,
          pro_stop_icon,
          pro_save_icon,
          pro_load_icon,
          pro_rec_icon,
          pro_size_up_icon,
          pro_size_down_icon,
          pro_scope_bordered_icon,
          pro_scope_icon,
          pro_info_bordered_icon,
          pro_info_icon,
          pro_help_bordered_icon,
          pro_help_icon,
          pro_prefs_icon,
          pro_prefs_bordered_icon,
          pro_info_dark_bordered_icon,
          pro_info_dark_icon,
          pro_help_dark_bordered_icon,
          pro_help_dark_icon,
          pro_prefs_dark_bordered_icon,
          pro_prefs_dark_icon,
          pro_rec_b_icon,
          pro_rec_b_dark_icon,
          pro_load_dark_icon,
          pro_save_dark_icon,

          default_light_run_icon,
          default_light_stop_icon,
          default_light_save_icon,
          default_light_load_icon,
          default_light_rec_icon,
          default_light_rec_a_icon,
          default_light_rec_b_icon,
          default_light_size_up_icon,
          default_light_size_down_icon,
          default_light_scope_icon,
          default_light_scope_toggled_icon,
          default_light_info_icon,
          default_light_info_toggled_icon,
          default_light_help_icon,
          default_light_help_toggled_icon,
          default_light_prefs_icon,
          default_light_prefs_toggled_icon,

          default_dark_run_icon,
          default_dark_stop_icon,
          default_dark_save_icon,
          default_dark_load_icon,
          default_dark_rec_icon,
          default_dark_rec_a_icon,
          default_dark_rec_b_icon,
          default_dark_size_up_icon,
          default_dark_size_down_icon,
          default_dark_scope_icon,
          default_dark_scope_toggled_icon,
          default_dark_info_icon,
          default_dark_info_toggled_icon,
          default_dark_help_icon,
          default_dark_help_toggled_icon,
          default_dark_prefs_icon,
          default_dark_prefs_toggled_icon,
          default_hc_run_icon,
          default_hc_stop_icon,
          default_hc_save_icon,
          default_hc_load_icon,
          default_hc_rec_icon,
          default_hc_rec_a_icon,
          default_hc_rec_b_icon,
          default_hc_size_up_icon,
          default_hc_size_down_icon,
          default_hc_scope_icon,
          default_hc_scope_toggled_icon,
          default_hc_info_icon,
          default_hc_info_toggled_icon,
          default_hc_help_icon,
          default_hc_help_toggled_icon,
          default_hc_prefs_icon,
          default_hc_prefs_toggled_icon;

signals:

public slots:
};

#endif // SONICPITHEME_H
