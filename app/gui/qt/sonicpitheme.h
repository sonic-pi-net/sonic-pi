#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>

class SonicPiTheme : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiTheme();
    QColor color(QString);
    void readTheme(QString filename);

private:
    bool isInitialized;
    QMap<QString, QString> theme;

signals:

public slots:
};

#endif // SONICPITHEME_H
