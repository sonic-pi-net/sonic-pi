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
    QString font(QString);
    void darkMode();
    void lightMode();

private:
    QMap<QString, QString> withCustomSettings(QMap<QString, QString> settings);
    QMap<QString, QString> lightTheme();
    QMap<QString, QString> darkTheme();
    QMap<QString, QString> theme;
    QMap<QString, QString> customSettings;

signals:

public slots:
};

#endif // SONICPITHEME_H
