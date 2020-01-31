#include <QPainter>
#include <QTimer>

#include "visualizer.h"

Visualizer::Visualizer(Scope* pScope, QWidget* pParent)
    : QWidget(pParent),
    m_pScope(pScope)
{
  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(drawLoop()));
  scopeTimer->start(20);
}
    
void Visualizer::paintEvent(QPaintEvent* paintEvent)
{
    QPainter painter(this);
    painter.setRenderHint(QPainter::RenderHint::HighQualityAntialiasing);

    auto rc = rect();

    int y = int(rc.height() / 2.0f);
    float yScale = (float)y;

    // A crude sampling of the source data, with no bounds checking...
    // and a nasty hack to extract the data from the scope (the data processing should be in a seperate thread somewhere).
    double* pData = m_pScope->GetSamples();
    double step = 4096 / double(rc.width());

    double average = 0.0;

    // Make a list of points; it's better to gather them and submit in a batch
    // Here we are just drawing in pixel space
    m_wavePointsLeft.resize(rc.width());
    m_wavePointsRight.resize(rc.width());
    for (int x = 0; x < rc.width(); x++)
    {
        // Should really smooth the samples here, but this is just a quick demo
        auto left = pData[int(double(x) * step)];
        auto right = pData[4096 + int(double(x) * step)];
        average += std::abs(left);
        average += std::abs(right);

        m_wavePointsLeft[x] = QPoint(x, left * yScale + y);
        m_wavePointsRight[x] = QPoint(x, right * yScale + y);
    }
    
    // Draw a simple volume based on the samples, in the background
    average /= rc.width() * 2;
    painter.fillRect(0, int(double(rc.height()) - average * yScale * 4), rc.width(),int(average * yScale * 4), Qt::blue);

    // Draw the left/right stereo, different colors, overlayed

    painter.setPen(Qt::green);
    painter.drawPolyline(&m_wavePointsLeft[0], m_wavePointsLeft.size());

    painter.setPen(Qt::red);
    painter.drawPolyline(&m_wavePointsRight[0], m_wavePointsRight.size());
}

void Visualizer::drawLoop()
{
    repaint();
}