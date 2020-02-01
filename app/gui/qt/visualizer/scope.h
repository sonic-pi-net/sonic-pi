//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <QWidget>
#include <QOpenGLWidget>
#include <QPen>

#include <memory>
#include <string>
#include <visualizer/server_shm.hpp>

QT_FORWARD_DECLARE_CLASS(QPaintEvent)
QT_FORWARD_DECLARE_CLASS(QResizeEvent)

enum class ScopeType
{
    Left,
    Right,
    Mono,
    Lissajous,
    MirrorStereo
};

struct Panel
{
    QString category;
    QString name;
    ScopeType type;

    QRect rc;
    QRect rcGraph;
    QRect rcTitle;
    QPen pen;
    bool visible = true;
    bool axisVisible = false;
    bool titleVisible = true;
    
    std::vector<QPoint> wavePoints;
    QLinearGradient redBlueGradient;
};

class Scope : public QOpenGLWidget
{
    Q_OBJECT

public:
    Scope(int scsynthPort, QWidget* parent = nullptr);
    virtual ~Scope();

    std::vector<QString> GetScopeCategories() const;
    bool EnableScope(const QString& name, bool on);
    bool SetScopeAxes(bool on);
    void TogglePause();
    void Pause();
    void Resume();
    void ResetScope();
    void Refresh();
    void ScsynthBooted();
    void SetColor(QColor c);

    void DrawWave(QPainter& painter, Panel& panel);
    void DrawMirrorStereo(QPainter& painter, Panel& panel);
    void DrawLissajous(QPainter& painter, Panel& panel);

protected:
    virtual void paintEvent(QPaintEvent* pEv) override;
    virtual void resizeEvent(QResizeEvent* pSize) override;

private:
    void Layout();

private slots:
    void OnTimer();

private:
    std::vector<Panel> m_panels;

    std::unique_ptr<server_shared_memory_client> m_shmClient;
    scope_buffer_reader m_shmReader;

    std::vector<std::vector<double>> m_samples;
    std::vector<double> m_monoSamples;

    unsigned int m_emptyFrames = 0;
    int m_scsynthPort = 0;

    bool m_scsynthIsBooted = false;
    bool m_paused = false;
 
};
