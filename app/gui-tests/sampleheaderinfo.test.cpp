// Tests for the sample-header parser feeding the completion popup's sample
// helper pane: duration/channels/rate read from FLAC/WAV/AIFF headers with no
// decoding. FLAC is exercised against the real built-in samples; WAV against
// a synthesised fixture; junk input must yield an empty string (no pane line).

#include <catch2/catch_test_macros.hpp>

#include <QDir>
#include <QTemporaryFile>

#include "utils/sampleheaderinfo.h"

using SonicPi::sampleHeaderInfo;

static QString repoSample(const char* name)
{
    return QStringLiteral(QT_TESTCASE_SOURCEDIR) + "/../../etc/samples/" + name;
}

TEST_CASE("built-in FLAC header parses to duration/channels/rate", "[sampleinfo]")
{
    // ambi_choir.flac: 1.5715s, stereo, 44100 Hz (ffprobe ground truth).
    const QString info = sampleHeaderInfo(repoSample("ambi_choir.flac"));
    CHECK(info == QString::fromUtf8("1.57s · stereo · 44.1 kHz"));
}

TEST_CASE("every built-in sample yields an info line", "[sampleinfo]")
{
    QDir dir(QStringLiteral(QT_TESTCASE_SOURCEDIR) + "/../../etc/samples");
    const QFileInfoList files =
        dir.entryInfoList({ "*.flac", "*.wav", "*.aiff", "*.aif" }, QDir::Files);
    REQUIRE(files.size() > 100);   // the shipped set — guards a bad dir path
    for (const QFileInfo& f : files) {
        INFO(f.fileName().toStdString());
        CHECK_FALSE(sampleHeaderInfo(f.filePath()).isEmpty());
    }
}

TEST_CASE("synthesised WAV header parses", "[sampleinfo]")
{
    // Minimal RIFF/WAVE: fmt (PCM, mono, 8000 Hz, 16-bit) + 16000 bytes of
    // data = exactly 1 second.
    QByteArray wav;
    auto le16 = [&](quint16 v) { wav.append(char(v & 0xFF)); wav.append(char(v >> 8)); };
    auto le32 = [&](quint32 v) {
        for (int i = 0; i < 4; ++i) wav.append(char((v >> (8 * i)) & 0xFF));
    };
    wav.append("RIFF"); le32(36 + 16000); wav.append("WAVE");
    wav.append("fmt "); le32(16);
    le16(1); le16(1); le32(8000); le32(16000); le16(2); le16(16);
    wav.append("data"); le32(16000);
    wav.append(QByteArray(64, '\0'));   // parser needs the header only

    QTemporaryFile f(QDir::tempPath() + "/spi_test_XXXXXX.wav");
    REQUIRE(f.open());
    f.write(wav);
    f.flush();
    CHECK(sampleHeaderInfo(f.fileName()) == QString::fromUtf8("1.00s · mono · 8 kHz"));
}

TEST_CASE("junk input yields no info line", "[sampleinfo]")
{
    QTemporaryFile f;
    REQUIRE(f.open());
    f.write("this is not audio");
    f.flush();
    CHECK(sampleHeaderInfo(f.fileName()).isEmpty());
    CHECK(sampleHeaderInfo(QStringLiteral("/nonexistent/nope.flac")).isEmpty());
}
