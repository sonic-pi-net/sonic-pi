// Share, as the web's Share panel: the buffer or the whole set, its link copied, as a QR code (scan or save), the
// link shown, or saved as a file; a panel over the window under its button, as the web's over the page, with the
// window's shortcuts working while it is open. The links and codes are the shared codec's (app/sharelink), the one the web's page
// runs: the web's tests read its codes back with a decoder of their own (share-link.test.mjs), and both pin the same
// code for the same link. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QAction>
#include <QApplication>
#include <QClipboard>
#include <QCryptographicHash>
#include <QFontInfo>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPointer>
#include <QAbstractButton>
#include <QPushButton>
#include <QScrollBar>
#include <QShortcut>
#include <QStackedWidget>
#include <QTest>
#include <QTextBrowser>

#include "app_stylesheet.h"
#include "dpi.h"
#include "model/sonicpitheme.h"
#include "sharelink.h"
#include "widgets/sharepanel.h"

namespace ShareLink = SonicPi::ShareLink;

namespace {

const char* drums = "live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n";

// Share in a window as the main window is: the toolbar's Share button it opens under, the editor that has the focus,
// and the window's own shortcuts, Run's and Escape's and Ctrl+G's (escapeWorkspaces)
struct Share
{
    QString setName = "Tuesday", setDescription, bufferName = "drums";
    int saved = 0, ran = 0, escaped = 0;
    QString setCode = "#-- Sonic Pi Set v1\n#-- buffer 0\nplay 60\n";
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    QWidget window;
    QPushButton* button = new QPushButton("Share", &window);
    QLineEdit* editor = new QLineEdit(&window);
    QPointer<SharePanel> panel;

    explicit Share(const QString& code = drums)
    {
        window.resize(900, 700);
        button->setGeometry(200, 8, 80, 28);
        editor->setGeometry(20, 600, 400, 30);
        QAction* run = new QAction(&window);
        run->setShortcut(QKeySequence(Qt::CTRL | Qt::Key_R));
        QObject::connect(run, &QAction::triggered, [this] { ran++; });
        window.addAction(run);
        for (const QKeySequence& key : { QKeySequence(Qt::Key_Escape), QKeySequence(Qt::CTRL | Qt::Key_G) })
            QObject::connect(new QShortcut(key, &window), &QShortcut::activated, [this] { escaped++; });
        window.show();
        window.activateWindow();
        REQUIRE(QTest::qWaitForWindowActive(&window));
        editor->setFocus();

        SharePanel::Scope buffer;
        buffer.kind = "This buffer";
        buffer.summary = "4 lines";
        buffer.title = "buffer 3";
        buffer.about = "Save the current buffer as a single plain text file.";
        buffer.preview = code;
        buffer.code = [code] { return code; };
        buffer.save = [this] { saved++; return true; };
        buffer.fileKind = ".txt";
        buffer.name = bufferName;
        buffer.setFields = [this](const QString& name, const QString&) { bufferName = name; };
        SharePanel::Scope set;
        set.kind = "Whole set";
        set.summary = "10 buffers";
        set.title = "the set \"Tuesday\"";
        set.code = [this] { return setCode + "#-- " + setDescription; };
        set.fileKind = ".sonicpi";
        set.described = true;
        set.nameEditable = true;
        set.name = setName;
        set.setFields = [this](const QString& name, const QString& description) { setName = name; setDescription = description; };
        panel = new SharePanel(buffer, set, theme, &window);
        panel->openUnder(button);
        QApplication::processEvents();
    }
    ~Share()
    {
        delete panel;
        delete theme;
        QApplication::processEvents();
    }
    template <typename T> T* find(const char* name) { return panel->findChild<T*>(name); }
    void press(const char* name) { find<QAbstractButton>(name)->click(); QApplication::processEvents(); }
    int page() { return panel->findChild<QStackedWidget*>()->currentIndex(); }
    bool closed() { QApplication::processEvents(); return panel.isNull() || !panel->isVisible(); }
};

std::string modules(const ShareLink::Qr& qr)
{
    std::string rows;
    for (int r = 0; r < qr.size; r++)
    {
        for (int c = 0; c < qr.size; c++)
            rows += qr.isDark(r, c) ? '1' : '0';
        rows += '\n';
    }
    return rows;
}

} // namespace

TEST_CASE("a link's QR code is the one the web's page draws for it", "[share][qr]")
{
    const ShareLink::Qr qr = ShareLink::qrCode("https://sonic-pi.net/code.html#code=" + ShareLink::encodeDigits(drums));
    CHECK(qr.version == 4);
    CHECK(qr.level == 'M');
    CHECK(QCryptographicHash::hash(QByteArray::fromStdString(modules(qr)), QCryptographicHash::Sha256).toHex().toStdString()
          == "2bb02469f4a34d0d387b3de24ac93fb0069bd360d3abdf35230baef5fc10049d");
}

TEST_CASE("medium error correction while it fits, low when only that holds it, no code past version 25", "[share][qr]")
{
    auto random = [](int n) { std::string t; unsigned seed = 1; while ((int)t.size() < n) { seed = (seed * 1103515245u + 12345u) % 2147483648u; t += (char)(33 + seed % 90); } return t; };
    // as a program grows, its code goes M, then L, then none, never back
    std::string seen;
    for (int n = 0; n <= 3000; n += 25)
    {
        const ShareLink::Qr qr = ShareLink::qrCode("https://sonic-pi.net/code.html#code=" + ShareLink::encodeDigits(random(n)));
        const char level = qr.version ? qr.level : '-';
        if (seen.empty() || seen.back() != level)
            seen += level;
        if (qr.version)
        {
            CHECK(qr.version <= ShareLink::QrMaxVersion);
            CHECK(qr.size == 17 + 4 * qr.version);
        }
    }
    CHECK(seen == "ML-");
}

TEST_CASE("a code's image is its modules, 8 pixels each for printing, inside a quiet zone of 4", "[share][qr]")
{
    const ShareLink::Qr qr = ShareLink::qrCode("https://sonic-pi.net/code.html#code=N10066414848");
    const QImage image = SharePanel::qrImage(qr, 8);
    REQUIRE(image.width() == 8 * (qr.size + 8));
    int wrong = 0;
    for (int r = 0; r < qr.size; r++)
        for (int c = 0; c < qr.size; c++)
            wrong += (qGray(image.pixel((c + 4) * 8 + 4, (r + 4) * 8 + 4)) < 128) != qr.isDark(r, c);
    CHECK(wrong == 0);
    CHECK(image.pixel(2, 2) == qRgb(255, 255, 255));
}

// The native Share was all at the house's Small, 9 px on Windows and 10 on a
// Mac, and hard to read (#3593); all at Base, it lost its order. It is in the
// house's roles (dpi.h FontRole), as the app's other panes: what is read or
// typed at Base, its titles and notes at Small.
TEST_CASE("Share reads at the house's Base, its titles and notes at Small, as the app's other panes", "[share]")
{
    Share s;
    const QStringList small = { "shareStep", "shareFieldLabel", "shareFieldHint", "shareQrNote", "shareLinkNote" };
    int bases = 0, smalls = 0;
    for (QWidget* w : s.panel->findChildren<QWidget*>())
    {
        if (!(qobject_cast<QLabel*>(w) || qobject_cast<QLineEdit*>(w) || qobject_cast<QPlainTextEdit*>(w)
              || qobject_cast<QTextBrowser*>(w) || qobject_cast<QPushButton*>(w)))
            continue;
        INFO(w->metaObject()->className() << " " << w->objectName().toStdString());
        const bool isSmall = small.contains(w->objectName());
        CHECK(QFontInfo(w->font()).pixelSize() == FontRolePx(isSmall ? FontRole::Small : FontRole::Base));
        (isSmall ? smalls : bases)++;
    }
    CHECK(bases == 8);    // what it is, its name, preview and description, the code, Larger, Save the image, the link
    CHECK(smalls == 7);   // SHARE, the three fields' titles, "optional", and the code's and the link's notes
}

// a title reads as one by being uppercased and muted (dpi.h FontRole::Small), and
// a screen reader says its word, not its letters
TEST_CASE("Share's titles are in capitals, and heard as words", "[share]")
{
    Share s;
    QStringList heard;
    for (QLabel* l : s.panel->findChildren<QLabel*>())
        if (l->objectName() == "shareStep" || l->objectName() == "shareFieldLabel")
        {
            INFO(l->text().toStdString());
            CHECK(l->text() == l->text().toUpper());
            heard << l->accessibleName();
        }
    heard.sort();
    CHECK(heard == QStringList{ "Description", "Name", "Preview", "Share" });
}

// under the app's stylesheet, as it always is: its box's border and padding, and its scroll bar, are the sheet's
TEST_CASE("Share's preview takes all the room the card has, and never less than five lines clear of its scroll bar", "[share]")
{
    AppStylesheet sheet;
    QString wide = drums;
    wide += "sample :bd_haus, amp: 2, rate: 1, cutoff: 100, release: 0.25, attack: 0.01, pan: 0, lpf: 120, hpf: 30\n";
    Share s(wide);
    auto* preview = s.find<QTextBrowser>("sharePreview");
    REQUIRE(preview->horizontalScrollBar()->isVisible());
    const int lines = QFontMetrics(preview->font()).lineSpacing();
    CHECK(preview->viewport()->height() >= 5 * lines + 2 * int(preview->document()->documentMargin()));

    const int before = preview->height();
    s.panel->resize(s.panel->width(), s.panel->height() + 200);
    QApplication::processEvents();
    CHECK(preview->height() == before + 200);

    // the same room for a set's description
    const int grown = preview->height();
    s.press("shareSet");
    s.panel->resize(s.panel->width(), s.panel->height() + 200);
    QApplication::processEvents();
    CHECK(s.find<QPlainTextEdit>("shareDescription")->height() == grown);
}

TEST_CASE("Share's link is the web's: copied, it opens the buffer's code", "[share]")
{
    Share s;
    CHECK(s.panel->link() == QString("https://sonic-pi.net/code.html#code=") + QString::fromStdString(ShareLink::encode(drums)));
    s.press("shareCopy");
    const ShareLink::Read back = ShareLink::decode(QApplication::clipboard()->text().toStdString());
    REQUIRE(back.ok);
    CHECK(back.code == drums);
    CHECK(s.find<QLabel>("shareAbout")->text() == "Link copied");
    CHECK(s.find<QAbstractButton>("shareCopy")->text() == "Copied");   // the tick and the word for it, a moment
}

TEST_CASE("Share's QR code is the link's, whole, and pressed again puts back what it is", "[share]")
{
    Share s;
    s.press("shareQr");
    CHECK(s.page() == 1);
    CHECK(s.find<QAbstractButton>("shareQr")->isChecked());
    QLabel* code = s.find<QLabel>("shareQrImage");
    REQUIRE_FALSE(code->pixmap().isNull());
    // all of it on show: a code missing its edge doesn't scan
    const QSize whole = code->pixmap().deviceIndependentSize().toSize();
    CHECK(code->contentsRect().width() >= whole.width());
    CHECK(code->contentsRect().height() >= whole.height());
    CHECK(code->visibleRegion().boundingRect().size() == code->size());
    // and Larger makes it larger, all of it still
    s.press("shareLarger");
    CHECK(code->pixmap().deviceIndependentSize().width() > whole.width());
    CHECK(code->visibleRegion().boundingRect().size() == code->size());
    s.press("shareLarger");
    CHECK(s.find<QPushButton>("shareSaveImage")->isVisible());
    s.press("shareQr");
    CHECK(s.page() == 0);
}

TEST_CASE("Share says when a program is too big for a code a phone can read, and offers none", "[share]")
{
    std::string big;
    unsigned seed = 7;
    while (big.size() < 4000) { seed = (seed * 1103515245u + 12345u) % 2147483648u; big += (char)(33 + seed % 90); }
    Share s(QString::fromStdString(big));
    s.press("shareQr");
    CHECK(s.find<QLabel>("shareQrImage")->isHidden());
    CHECK(s.find<QPushButton>("shareSaveImage")->isHidden());
    CHECK(s.find<QLabel>("shareQrNote")->isVisible());
    CHECK(s.find<QLabel>("shareQrNote")->text().contains("too big for a QR code"));
}

TEST_CASE("Share shows the link to copy by hand, its length under it", "[share]")
{
    Share s;
    s.press("shareShowLink");
    CHECK(s.page() == 2);
    CHECK(s.find<QPlainTextEdit>("shareLinkText")->toPlainText() == s.panel->link());
}

TEST_CASE("Share saves a file as Save As does", "[share]")
{
    Share s;
    s.press("shareSaveFile");
    CHECK(s.saved == 1);
    CHECK(s.find<QLabel>("shareAbout")->text() == "Saved");
}

TEST_CASE("the whole set: its name and description given where it's shared, its link carrying them", "[share]")
{
    Share s;
    s.find<QAbstractButton>("shareSet")->click();
    QApplication::processEvents();
    CHECK(s.find<QAbstractButton>("shareSet")->isChecked());
    CHECK_FALSE(s.find<QAbstractButton>("shareBuffer")->isChecked());
    CHECK(s.find<QLineEdit>("shareName")->isVisible());
    CHECK(s.find<QLineEdit>("shareName")->text() == "Tuesday");
    CHECK_FALSE(s.find<QWidget>("sharePreview")->isVisible());
    CHECK(s.find<QWidget>("shareDescription")->isVisible());
    s.find<QPlainTextEdit>("shareDescription")->setPlainText("for the gig");
    s.find<QLineEdit>("shareName")->setText("Gig");
    emit s.find<QLineEdit>("shareName")->textEdited("Gig");
    CHECK(s.setDescription == "for the gig");
    CHECK(s.setName == "Gig");
    const ShareLink::Read back = ShareLink::decode(s.panel->link().toStdString());
    REQUIRE(back.ok);
    CHECK(back.code == "#-- Sonic Pi Set v1\n#-- buffer 0\nplay 60\n#-- for the gig");
}

TEST_CASE("the buffer's name is given where it's shared, its code previewed under it", "[share]")
{
    Share s;
    QLineEdit* name = s.find<QLineEdit>("shareName");
    CHECK(name->isVisible());
    CHECK(name->text() == "drums");
    CHECK_FALSE(s.find<QWidget>("shareDescription")->isVisible());
    name->setText("kick");
    emit name->textEdited("kick");
    CHECK(s.bufferName == "kick");
    // a name can't be emptied: it keeps the one it had
    name->setText("");
    emit name->editingFinished();
    CHECK(name->text() == "kick");
}

TEST_CASE("the buffer's preview is its code, highlighted as the editor has it", "[share]")
{
    Share s;
    CHECK(s.find<QWidget>("sharePreview")->isVisible());
    CHECK(s.panel->findChild<QTextBrowser*>("sharePreview")->toPlainText().trimmed() == QString(drums).trimmed());
}

// The web's panel is a popover over the page (share-menu.js): the page's shortcuts work while it is open. Native's was a
// modal dialog, and every shortcut and menu was dead until it closed: Quit too.
TEST_CASE("Share opens in its window, under its button, as the web's panel under its own", "[share][popover]")
{
    Share s;
    CHECK_FALSE(s.panel->isWindow());
    CHECK(s.panel->parentWidget() == &s.window);
    CHECK(s.panel->x() == s.button->x());
    CHECK(s.panel->y() == s.button->geometry().bottom() + 1 + 6);
    CHECK(s.panel->isAncestorOf(QApplication::focusWidget()));
}

TEST_CASE("with Share open, the window's shortcuts work, and Share stays open", "[share][popover]")
{
    Share s;
    QTest::keyClick(QApplication::focusWidget(), Qt::Key_R, Qt::ControlModifier);
    CHECK(s.ran == 1);
    CHECK_FALSE(s.closed());
}

TEST_CASE("Escape closes Share, ahead of the window's own Escape, and the focus goes back where it was", "[share][popover]")
{
    Share s;
    QTest::keyClick(QApplication::focusWidget(), Qt::Key_Escape);
    CHECK(s.closed());
    CHECK(s.escaped == 0);
    CHECK(QApplication::focusWidget() == s.editor);
}

// Emacs's Ctrl-G, as the web's and the find popup's: the Control key, or the Command key on a Mac (Qt's Ctrl)
TEST_CASE("Ctrl-G closes Share as Escape does", "[share][popover]")
{
    for (Qt::KeyboardModifier ctrl : { Qt::ControlModifier, Qt::MetaModifier })
    {
        Share s;
        QTest::keyClick(QApplication::focusWidget(), Qt::Key_G, ctrl);
        CHECK(s.closed());
        CHECK(s.escaped == 0);
    }
}

TEST_CASE("Escape closes Share wherever the focus is in its window, as the web's", "[share][popover]")
{
    Share s;
    s.editor->setFocus();
    QTest::keyClick(s.editor, Qt::Key_Escape);
    CHECK(s.closed());
    CHECK(s.escaped == 0);
}

// the button opens and closes it itself (MainWindow::share): a press there is not a click outside
TEST_CASE("a click anywhere else in its window closes Share; one in it, or on its button, doesn't", "[share][popover]")
{
    Share s;
    QTest::mouseClick(s.find<QLabel>("shareAbout"), Qt::LeftButton);
    CHECK_FALSE(s.closed());
    QTest::mouseClick(s.button, Qt::LeftButton);
    CHECK_FALSE(s.closed());
    QTest::mouseClick(s.editor, Qt::LeftButton);
    CHECK(s.closed());
}

// as the web's, in the middle of what it is over: there the page, here its window
TEST_CASE("Share's larger QR code is kept in its window, in the middle of it, and Smaller puts it back", "[share][popover]")
{
    Share s;
    s.press("shareQr");
    s.press("shareLarger");
    CHECK(s.window.rect().contains(s.panel->geometry()));
    CHECK(std::abs(s.panel->geometry().center().x() - s.window.rect().center().x()) <= 1);
    CHECK(std::abs(s.panel->geometry().center().y() - s.window.rect().center().y()) <= 1);
    s.press("shareLarger");
    CHECK(s.panel->y() == s.button->geometry().bottom() + 1 + 6);
}
