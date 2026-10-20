//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2026 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#pragma once

#include <QImage>
#include <QPointer>

#include <functional>

#include "sharelink.h"
#include "widgets/overlaypane.h"

class QAbstractButton;
class QLabel;
class QLineEdit;
class QPlainTextEdit;
class QPushButton;
class QStackedWidget;
class SonicPiTheme;

// Save and Share, as the web's (app/web/app/src/share-menu.js, and its look: style.css .sm-*): what to share, the
// buffer showing or the whole set, as two tabs, the chosen one joined to the card below in the accent; then how, the
// card's foot: its link copied or shown to copy by hand, side by side, as a QR code for a phone to scan or to save as
// an image, or saved as a file, a buffer as .txt and a set as .sonicpi. The links are the web's own (app/sharelink), so
// a phone that scans the code opens the program on sonic-pi.net, and the web opens them all.
//
// A panel over its window under its button, as the web's over the page: the window's shortcuts and menus work while
// it is open, and it closes as the web's does, on its close, Escape or Ctrl-G, or a press anywhere else in the window.
class SharePanel : public SonicPi::OverlayPane
{
    Q_OBJECT

public:
    struct Scope
    {
        QString kind;                    // its tab's words: "This buffer"
        QString summary;                 // what it holds: "12 lines"
        QString title;                   // the words for it, and the saved image's name: "buffer 3"
        QString about;                   // what it is, and what its file opens in
        std::function<QString()> code;   // what its link carries (a set's: its .sonicpi text), asked as it is shared
        std::function<bool()> save;      // saves it as a file: false when put off
        QString fileKind;                // ".txt", ".sonicpi"
        // its name, and a set's its description too, given here where it is shared and kept with it
        // as they are typed (setFields); a saved set's name is its file's, not to be typed here (nameEditable)
        bool described = false;
        bool nameEditable = true;
        QString name;
        QString description;
        std::function<void(const QString& name, const QString& description)> setFields;
    };

    static constexpr const char* LinkBase = "https://sonic-pi.net/code.html#code=";

    SharePanel(Scope buffer, Scope set, SonicPiTheme* theme, QWidget* window);
    ~SharePanel() override;
    // shown under button (the toolbar's Share), or in the middle of the window without one, with the focus; closed,
    // the focus goes back where it was. The button opens and closes it itself: a press on it is not one outside.
    void openUnder(QWidget* button);

    // the chosen one's link, and its link for a QR code (the program as digits, which a code holds a third more of)
    QString link() const;
    QString qrLink() const;

    // A code as an image: modulePx pixels a module, dark on white, with its quiet zone of 4 modules
    static QImage qrImage(const SonicPi::ShareLink::Qr& qr, int modulePx);

protected:
    void paintEvent(QPaintEvent* event) override;                  // the panel: the editor's ground, the controls' outline
    // a click on the code makes it larger, or smaller; and while it is open, the window's keys and presses that close it
    bool eventFilter(QObject* watched, QEvent* event) override;
    void closeEvent(QCloseEvent* event) override;

private:
    void choose(int scope);
    void render();
    void copyLink();
    void toggleQr();
    void toggleLink();
    void saveFile();
    void saveImage();
    void drawQr();
    void done(QAbstractButton* action, const QString& word, const QString& said);
    void showPage(int page);
    void fit();
    void place();
    void stopWatching();

    SonicPiTheme* m_theme;
    Scope m_scopes[2];
    int m_scope = 0;
    bool m_large = false;
    SonicPi::ShareLink::Qr m_qr;

    QAbstractButton* m_tabs[2] = {};
    QWidget* m_card = nullptr;
    QLabel* m_about = nullptr;
    QStackedWidget* m_pages = nullptr;   // what it is (its name, and a set's description), its QR code, its link
    QWidget* m_descriptionBox = nullptr;
    QLineEdit* m_name = nullptr;
    QPlainTextEdit* m_description = nullptr;
    QLabel* m_qrImage = nullptr;
    QLabel* m_qrNote = nullptr;
    QPushButton* m_larger = nullptr;
    QPushButton* m_saveImage = nullptr;
    QPlainTextEdit* m_linkText = nullptr;
    QLabel* m_linkNote = nullptr;
    QAbstractButton* m_copyAct = nullptr;
    QAbstractButton* m_qrAct = nullptr;
    QAbstractButton* m_linkAct = nullptr;
    QAbstractButton* m_fileAct = nullptr;
    QPointer<QWidget> m_button;        // what it opened under
    QPointer<QWidget> m_focusBefore;   // where the focus goes back to
};
