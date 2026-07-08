//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright (C) 2026 by Sam Aaron
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPIERRORCARD_H
#define SONICPIERRORCARD_H

#include <QFrame>
#include <QString>

class QLabel;
class QPushButton;
class QScrollArea;
class SonicPiTheme;
class SonicPiErrorCodeLine;

// A native error card: a rounded, themed panel with a coloured header, the
// location and parser reason, the offending line of code in an inset box, a
// compact "Jump to error" button and an expandable backtrace. Replaces the
// old QTextBrowser HTML so it can have rounded corners, a real hover button
// and proper spacing.
class SonicPiErrorCard : public QFrame
{
    Q_OBJECT
public:
    explicit SonicPiErrorCard(SonicPiTheme* theme, QWidget* parent = nullptr);

    // codeLine is the raw offending source line; [colStart,colEnd) is the byte
    // span of the token to highlight (or -1 for none). backtrace empty hides the
    // details toggle. isSyntax picks the blue accent over the runtime pink.
    void showError(bool isSyntax,
                   const QString& header,
                   const QString& location,
                   const QString& reason,
                   const QString& codeLine,
                   int lineNumber,
                   int colStart, int colEnd,
                   const QString& backtrace,
                   bool canJump);

    void applyTheme();
    QString plainText() const;  // flat text for the screen-reader announce

signals:
    void jumpRequested();
    void closeRequested();

protected:
    void keyPressEvent(QKeyEvent* event) override;

private:
    void setDetailsVisible(bool on);
    // Re-paints the theme-coloured dynamic content (accent code-spans in the
    // message + the syntax-coloured offending line) from the stored error, so a
    // theme change re-tints it without needing the error to be re-reported.
    void renderThemedContent();

    SonicPiTheme* m_theme;
    bool m_isSyntax = false;
    QString m_headerPlain;  // raw header text for the screen-reader announce

    // Stored inputs from the last showError(), so renderThemedContent() can
    // re-derive the accent-coloured content against the current theme.
    QString m_messageText;   // friendly message with `backtick` tokens intact
    QString m_codeLine;      // raw offending source line ("" when none)
    int m_colStart = -1;
    int m_colEnd = -1;
    int m_codeLineNumber = -1;

    QLabel* m_header;
    QPushButton* m_close;
    QLabel* m_message;
    QLabel* m_location;
    QLabel* m_reason;
    QFrame* m_codeFrame;
    SonicPiErrorCodeLine* m_code;
    QPushButton* m_jump;
    QPushButton* m_details;
    QScrollArea* m_backtraceScroll;
    QLabel* m_backtrace;
    bool m_detailsOn = false;
};

#endif
