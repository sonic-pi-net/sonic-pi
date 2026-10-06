#ifndef ZOOMBAR_H
#define ZOOMBAR_H

#include <QIcon>
#include <QWidget>

class SonicPiTheme;
class QPushButton;

// A flat circle -/+ text-size control, the two stacked, shown at the foot of
// the help's tab rail (IconTabWidget::setFootWidget) as the web shows its
// own. Every help tab (Cards, Docs, Logs, Debug, Tracks) has one, and only
// the current tab's is shown. Emits zoomStep(±1); the owning panel keeps its
// own zoom level and applies the font change.
class ZoomBar : public QWidget
{
    Q_OBJECT
public:
    explicit ZoomBar(SonicPiTheme* theme, const QString& subject,
                     QWidget* parent = nullptr);

    // Retint the glyphs for the current theme (muted at rest, accent on hover).
    void applyTheme();

signals:
    void zoomStep(int delta); // -1 (smaller) or +1 (larger)

protected:
    bool eventFilter(QObject* obj, QEvent* event) override;

private:
    SonicPiTheme* m_theme;
    QPushButton* m_out = nullptr;
    QPushButton* m_in = nullptr;
    QIcon m_outIcon, m_outHover, m_inIcon, m_inHover;
};

#endif // ZOOMBAR_H
