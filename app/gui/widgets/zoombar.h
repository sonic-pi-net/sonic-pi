#ifndef ZOOMBAR_H
#define ZOOMBAR_H

#include <QIcon>
#include <QWidget>

class SonicPiTheme;
class QPushButton;

// A flat circle +/- text-size control, larger over smaller, at the foot of
// the help's tab rail (IconTabWidget::setFootWidget) while the Docs tab is
// showing: the help's text size (MainWindow::setHelpZoom), which the View
// menu and Preferences also offer. The buttons take no focus; from the
// keyboard the View menu's items do the same. Emits zoomStep(±1).
class ZoomBar : public QWidget
{
    Q_OBJECT
public:
    // larger, smaller: each button's name and tooltip, the View menu's for it
    ZoomBar(SonicPiTheme* theme, const QString& larger, const QString& smaller,
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
