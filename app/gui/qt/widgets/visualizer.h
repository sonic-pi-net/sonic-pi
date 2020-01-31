#include <QWidget>
#include <QPoint>
#include <vector>

#include "scope.h"

QT_FORWARD_DECLARE_CLASS(QPaintEvent)

class Visualizer : public QWidget
{
    Q_OBJECT
public:
    Visualizer(Scope* pScope, QWidget* pParent = nullptr);

protected:
    virtual void paintEvent(QPaintEvent* painter) override;

private slots:
  void drawLoop();

private:
    Scope* m_pScope = nullptr;
    std::vector<QPoint> m_wavePointsLeft;
    std::vector<QPoint> m_wavePointsRight;
};