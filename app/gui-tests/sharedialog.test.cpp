// Share, as the web's Share panel: the buffer or the whole set, its link copied, as a QR code (scan or save), the
// link shown, or saved as a file. The links and codes are the shared codec's (app/sharelink), the one the web's page
// runs: the web's tests read its codes back with a decoder of their own (share-link.test.mjs), and both pin the same
// code for the same link. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QClipboard>
#include <QCryptographicHash>
#include <QLabel>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QAbstractButton>
#include <QPushButton>
#include <QStackedWidget>
#include <QTextBrowser>

#include "model/sonicpitheme.h"
#include "sharelink.h"
#include "widgets/sharedialog.h"

namespace ShareLink = SonicPi::ShareLink;

namespace {

const char* drums = "live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n";

struct Share
{
    QString setName = "Tuesday", setDescription, bufferName = "drums";
    int saved = 0;
    QString setCode = "#-- Sonic Pi Set v1\n#-- buffer 0\nplay 60\n";
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    ShareDialog* dialog = nullptr;

    explicit Share(const QString& code = drums)
    {
        ShareDialog::Scope buffer;
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
        ShareDialog::Scope set;
        set.kind = "Whole set";
        set.summary = "10 buffers";
        set.title = "the set \"Tuesday\"";
        set.code = [this] { return setCode + "#-- " + setDescription; };
        set.fileKind = ".sonicpi";
        set.described = true;
        set.nameEditable = true;
        set.name = setName;
        set.setFields = [this](const QString& name, const QString& description) { setName = name; setDescription = description; };
        dialog = new ShareDialog(buffer, set, theme);
        dialog->show();
        QApplication::processEvents();
    }
    ~Share()
    {
        delete dialog;
        delete theme;
        QApplication::processEvents();
    }
    template <typename T> T* find(const char* name) { return dialog->findChild<T*>(name); }
    void press(const char* name) { find<QAbstractButton>(name)->click(); QApplication::processEvents(); }
    int page() { return dialog->findChild<QStackedWidget*>()->currentIndex(); }
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
    const QImage image = ShareDialog::qrImage(qr, 8);
    REQUIRE(image.width() == 8 * (qr.size + 8));
    int wrong = 0;
    for (int r = 0; r < qr.size; r++)
        for (int c = 0; c < qr.size; c++)
            wrong += (qGray(image.pixel((c + 4) * 8 + 4, (r + 4) * 8 + 4)) < 128) != qr.isDark(r, c);
    CHECK(wrong == 0);
    CHECK(image.pixel(2, 2) == qRgb(255, 255, 255));
}

TEST_CASE("Share's link is the web's: copied, it opens the buffer's code", "[share]")
{
    Share s;
    CHECK(s.dialog->link() == QString("https://sonic-pi.net/code.html#code=") + QString::fromStdString(ShareLink::encode(drums)));
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
    CHECK(s.find<QPlainTextEdit>("shareLinkText")->toPlainText() == s.dialog->link());
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
    const ShareLink::Read back = ShareLink::decode(s.dialog->link().toStdString());
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
    CHECK(s.dialog->findChild<QTextBrowser*>("sharePreview")->toPlainText().trimmed() == QString(drums).trimmed());
}
