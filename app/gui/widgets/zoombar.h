#ifndef ZOOMBAR_H
#define ZOOMBAR_H

#include <QIcon>
#include <QWidget>

class SonicPiTheme;
class QPushButton;

// A flat circle -/+ text-size control shown in the help dock's title row. Every
// help tab (Cards, Docs, Logs, Debug) uses one so their controls line up in the
// same row, evenly spaced and beside the shared close ✕. Emits zoomStep(±1);
// the owning panel keeps its own zoom level and applies the font change.
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
