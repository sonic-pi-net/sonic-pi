#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>
#include <QJsonObject>

class SonicPiTheme : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiTheme(QObject *parent = 0, const QJsonObject& settings = QJsonObject());
    ~SonicPiTheme();
    QColor color(QString);

private:
    QJsonObject theme;

signals:

public slots:
};

#endif // SONICPITHEME_H
