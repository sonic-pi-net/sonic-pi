// The editor and files: dropped on the code, as the web's editor takes them (app/web/app/src/editor.js). A set
// among them opens as a set (setDropped, which the window opens), nothing put in; a file of code goes in as its
// text where it is dropped; anything else (a sample) as its path in quotes, ready for sample. And code loaded into
// a buffer is one undoable change. Headless, offscreen.

#include <catch2/catch_test_macros.hpp>

#include <QApplication>
#include <QFile>
#include <QMimeData>
#include <QSettings>
#include <QSignalSpy>
#include <QTemporaryDir>
#include <QUrl>

#include "model/sonicpitheme.h"
#include "utils/scintilla_api.h"
#include "widgets/sonicpilexer.h"
#include "widgets/sonicpiscintilla.h"

namespace {

struct Editor
{
    SonicPiTheme* theme = new SonicPiTheme(nullptr, "", QStringLiteral(SP_ROOT));
    SonicPiLexer* lexer = new SonicPiLexer(theme);
    ScintillaAPI* api = new ScintillaAPI(lexer);
    QSettings keys{ QSettings::IniFormat, QSettings::UserScope, "sonic-pi.net", "gui-tests-keys" };
    SonicPiScintilla* ed = new SonicPiScintilla(lexer, theme, "", true, &keys);
    QTemporaryDir dir;

    Editor()
    {
        ed->resize(600, 400);
        ed->setText(QString());
        ed->show();
        QApplication::processEvents();
    }

    ~Editor()
    {
        delete ed;
        delete lexer;   // owns the api
        delete theme;
        QApplication::processEvents();
    }

    QString file(const QString& name, const QByteArray& bytes)
    {
        const QString path = dir.path() + "/" + name;
        QFile f(path);
        REQUIRE(f.open(QFile::WriteOnly));
        f.write(bytes);
        return path;
    }

    // the files dropped where the code starts, as the desktop drops them
    void drop(const QStringList& paths)
    {
        QList<QUrl> urls;
        for (const QString& p : paths)
            urls.append(QUrl::fromLocalFile(p));
        dropUrls(urls);
    }

    // what a browser drops: its links
    void dropUrls(const QList<QUrl>& urls)
    {
        QMimeData mime;
        mime.setUrls(urls);
        QDragEnterEvent enter(QPoint(2, 2), Qt::CopyAction, &mime, Qt::LeftButton, Qt::NoModifier);
        QApplication::sendEvent(ed->viewport(), &enter);
        QDropEvent ev(QPointF(2, 2), Qt::CopyAction, &mime, Qt::LeftButton, Qt::NoModifier);
        QApplication::sendEvent(ed->viewport(), &ev);
        QApplication::processEvents();
    }

    std::string code() const { return ed->text().toStdString(); }
};

} // namespace

TEST_CASE("a file of code dropped on the code goes in as its text, where it is dropped, one undo taking it out", "[editor][files]")
{
    Editor e;
    e.ed->setText("play 60\n");
    e.drop({ e.file("riff.rb", "play 72\n") });
    CHECK(e.code() == "play 72\nplay 60\n");
    e.ed->undo();
    CHECK(e.code() == "play 60\n");
}

TEST_CASE("a set dropped on the code is handed to the window to open, nothing put in", "[editor][files]")
{
    Editor e;
    e.ed->setText("play 60\n");
    QSignalSpy opened(e.ed, &SonicPiScintilla::setDropped);
    const QString set = e.file("gig.sonicpi", "#-- buffer 0\nplay 80\n");
    e.drop({ e.file("riff.rb", "play 72\n"), set });
    REQUIRE(opened.size() == 1);
    CHECK(opened.at(0).at(0).toString() == set);
    CHECK(e.code() == "play 60\n");

    // told by its header too, whatever its name
    const QString saved = e.file("gig.txt", "#-- Sonic Pi Set v1\n#-- buffer 0\nplay 80\n");
    e.drop({ saved });
    REQUIRE(opened.size() == 2);
    CHECK(opened.at(1).at(0).toString() == saved);
    CHECK(e.code() == "play 60\n");
}

TEST_CASE("a file that isn't code dropped on the code goes in as its path, in quotes", "[editor][files]")
{
    Editor e;
    e.ed->setText("play 60\n");
    const QString kick = e.file("kick.wav", QByteArray("RIFF\0\0\0\0WAVE", 12));
    e.drop({ kick });
    CHECK(e.code() == ("\"" + kick + "\"\nplay 60\n").toStdString());
}

TEST_CASE("code loaded into a buffer replaces it as one undoable change", "[editor][files]")
{
    Editor e;
    e.ed->setText("play 60\n");
    e.ed->replaceBuffer("sample :bd_haus\n", 0, 0, 0);
    CHECK(e.code() == "sample :bd_haus\n");
    e.ed->undo();
    CHECK(e.code() == "play 60\n");
}

TEST_CASE("a Sonic Pi link dropped on the code is handed to the window to open, nothing put in", "[editor][files]")
{
    Editor e;
    e.ed->setText("play 60\n");
    QSignalSpy opened(e.ed, &SonicPiScintilla::linkDropped);
    e.dropUrls(QList<QUrl>{ QUrl("https://sonic-pi.net/code.html#code=1A_VpAA") });
    REQUIRE(opened.size() == 1);
    CHECK(opened.at(0).at(0).toString() == "https://sonic-pi.net/code.html#code=1A_VpAA");
    CHECK(e.code() == "play 60\n");

    // any other link goes in as the files do, quoted
    e.dropUrls(QList<QUrl>{ QUrl("https://example.com/kick.wav") });
    CHECK(opened.size() == 1);
    CHECK(e.code() == "\"https://example.com/kick.wav\"\nplay 60\n");
}
