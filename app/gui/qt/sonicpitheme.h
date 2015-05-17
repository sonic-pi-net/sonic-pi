#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>

class SonicPiTheme : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiTheme(QObject *parent = 0, QSettings *settings = 0, bool dark = false);
    ~SonicPiTheme();
    QColor color(QString);
    void darkMode();
    void lightMode();

private:
    QMap<QString, QString> lightTheme();
    QMap<QString, QString> darkTheme();
    QMap<QString, QString> theme;

signals:

public slots:
};

#endif // SONICPITHEME_H
