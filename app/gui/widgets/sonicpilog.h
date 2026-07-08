//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPILOG_H
#define SONICPILOG_H

#include <QPlainTextEdit>

class SonicPiTheme;
class QContextMenuEvent;

class SonicPiLog : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit SonicPiLog(QWidget *parent = 0);
    bool forceScroll;
    int zoomLevel;

    struct Message
    {
        int msg_type;
        std::string s;
    };
    typedef std::vector<Message> Messages;

    struct MultiMessage
    {
        SonicPiTheme *theme;
        int job_id;
        std::string thread_name;
        std::string runtime;
        Messages messages;
    };

signals:

public slots:
    void setTextColor(QColor c);
    void setTextBackgroundColor(QColor c);
    void setTextBgFgColors(QColor fg, QColor bg);
    // Role-aware variants: record the theme keys used so recolour() can
    // re-derive the colours after a theme change (scheme / hue / monochrome).
    void setTextColorKey(SonicPiTheme* theme, const QString& fgKey);
    void setTextBgFgColorKeys(SonicPiTheme* theme, const QString& bgKey, const QString& fgKey, int bgAlpha = 255);
    void setTextBackgroundColorKey(SonicPiTheme* theme, const QString& bgKey, int bgAlpha = 255);
    void recolour(SonicPiTheme* theme);
    void setFontFamily(QString font_name);
    void handleMultiMessage(SonicPiLog::MultiMessage mm);
    void forceScrollDown(bool force);
    void appendPlainText(QString text);
    void zoomIn();
    void zoomOut();
    void setZoomLevel(int zoom);
    int currentZoomLevel() const;


protected:
    void contextMenuEvent(QContextMenuEvent* event) override;
};

Q_DECLARE_METATYPE(SonicPiLog::MultiMessage)

#endif // SONICPILOG_H
