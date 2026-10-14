// The share link's codec (app/sharelink), as the native app links it: every link the web has written reads back,
// and native writes the very links the web's page does (app/web/app/test/share-link.test.mjs holds the wasm build
// of this same codec to the same WRITTEN links), so a program has one link whichever Sonic Pi shared it.

#include <catch2/catch_test_macros.hpp>

#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>

#include "sharelink.h"

namespace ShareLink = SonicPi::ShareLink;

namespace {

struct Vector
{
    const char* program;
    const char* read;      // as format 1's first codec (the web's, on fflate) wrote it: reads back for good
    const char* written;   // as the codec writes it now, native and web alike
    const char* digits;    // the QR code's form of `written`, where the table has one
};

// docs/share-link-format.md's test vectors, and what the codec writes for each: the first five format 1's, short
// enough that the codec still writes them in format 1 (deflate's frame is the smaller); the last format 2's, a
// program, written by its first codec (zstd), so read and written are one
const Vector vectors[] = {
    { "play 60", "1A_VpAA", "1A_VpAA", "N10066414848" },
    { "live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n", "1QypHIFdtQYsQAA", "1QypH1sGKIlARAgA", "N1189053114736665941343291904" },
    { "# h\xc3\xa9llo \xe2\x80\x94 \xc3\xbcn\xc3\xaf" "code, \xf0\x9f\x8e\xb9 and a tab\there\nplay :e3\r\nsleep 1",
      "1m5oBOj4HOLYPGqNpYJ6iAOTuyQMS60F1ho4C8wfm-cx9zDuB2Q-4gzMxiRPUrgLNh6ca84KGagA",
      "1m5oBOj4nJycfNEbTwDxFAcjdkwck1oPqDB0F5g_M85n7mHcCs18iaGccJ6hd9QB8lSgvaKgGAA", nullptr },
    { "\x01\x02\x03 x \x7f", "1Y2ZkZmJmBi7AqwcA", "1Y2ZkZmJmVqhQqAcA", "N1279786040948424540722907694848" },
    { "#-- Sonic Pi Set v1\n#-- meta {\"current\":2,\"names\":[\"x\"],\"zooms\":[5,2,2,2,2,2,2,2,2,2]}\n#-- buffer 0\nplay 1\n\n#-- buffer 2\n#-- ~#-- tricky\nplay 3\n\n",
      "1I9YdwBwOHusBWgbcHRGLsNoUr9UdoCEeZHvrQATojJvsSqCk8YMHAA", "1I9YdRhBrQZYpVSjFIqw2xWt1B2iIB9neOhABOuMmuxIoafzgAQA", nullptr },
    { "# Coded by Sam Aaron\n\nuse_sample_bpm :loop_amen\n\nwith_fx :rlpf, cutoff: 10, cutoff_slide: 4 do |c|\n  live_loop :dnb do\n    sample :bass_dnb_f, amp: 5\n    sample :loop_amen, amp: 5\n    sleep 1\n    control c, cutoff: rrand(40, 120), cutoff_slide: rrand(1, 4)\n  end\nend\n",
      "2AAB9AQAolaQx4mMM_HWRC8wZN-xykxW9zSfz-wxdNN4ZQyA5WhjCAe3k2ChNRnfecnD-FA", "2AAB9AQAolaQx4mMM_HWRC8wZN-xykxW9zSfz-wxdNN4ZQyA5WhjCAe3k2ChNRnfecnD-FA",
      "N20000053688769960546216744574712949408268428473744504140072651141631503480181366407968161431360531286766084290732935537807405076" },
};
const Vector& dnb = vectors[5];

std::string readBack(const std::string& link)
{
    const ShareLink::Read r = ShareLink::decode(link);
    return r.ok ? r.code : "error: " + r.error;
}

} // namespace

TEST_CASE("every format's links read back as they always have", "[sharelink]")
{
    for (const Vector& v : vectors)
    {
        CHECK(readBack(v.read) == v.program);
        CHECK(readBack(v.written) == v.program);
        if (v.digits)
            CHECK(readBack(v.digits) == v.program);
    }
}

TEST_CASE("native writes the links the web's page writes", "[sharelink]")
{
    for (const Vector& v : vectors)
    {
        CHECK(ShareLink::encode(v.program) == v.written);
        if (v.digits)
            CHECK(ShareLink::encodeDigits(v.program) == v.digits);
    }
}

TEST_CASE("every link each format's first codec wrote for the programs Sonic Pi ships reads back", "[sharelink]")
{
    for (const char* fixture : { SP_ROOT "/app/web/app/test/fixtures/share-links-1.json", SP_ROOT "/app/web/app/test/fixtures/share-links-2.json" })
    {
        QFile f(QString::fromUtf8(fixture));
        REQUIRE(f.open(QFile::ReadOnly));
        const QJsonArray cases = QJsonDocument::fromJson(f.readAll()).object().value("cases").toArray();
        REQUIRE(cases.size() > 500);
        int unread = 0;
        for (const auto& c : cases)
        {
            const QJsonObject o = c.toObject();
            const std::string code = o.value("code").toString().toStdString();
            if (readBack(o.value("link").toString().toStdString()) != code || readBack(o.value("digits").toString().toStdString()) != code)
            {
                unread++;
                UNSCOPED_INFO(o.value("key").toString().toStdString());
            }
        }
        CHECK(unread == 0);
    }
}

TEST_CASE("a link goes in whichever format makes it shorter: format 1 for a line, format 2 for a program", "[sharelink]")
{
    const ShareLink::Codec one(*ShareLink::table('1')), two(*ShareLink::table('2'));
    for (const std::string program : { std::string("play 60"), std::string(dnb.program), std::string(vectors[1].program) })
    {
        const std::string asOne = "1" + ShareLink::base64url(one.encode(program)), asTwo = "2" + ShareLink::base64url(two.encode(program));
        CHECK(ShareLink::encode(program) == (asTwo.size() <= asOne.size() ? asTwo : asOne));
    }
    CHECK(ShareLink::encode("play 60")[0] == '1');
    CHECK(ShareLink::encode(dnb.program)[0] == '2');
}

TEST_CASE("a program and a set go by link and come back, as text or digits", "[sharelink]")
{
    for (const std::string program : { std::string("use_synth :prophet\nplay 40, release: 8\n"), std::string(), std::string(70000, 'a'),
                                       std::string("\n\n  \n      x\n                                        y"), std::string("\xf0\x9f\x8e\xb9 \x00 \x08", 9) })
    {
        CHECK(readBack(ShareLink::encode(program)) == program);
        CHECK(readBack(ShareLink::encodeDigits(program)) == program);
    }
}

TEST_CASE("a whole link reads as its code does, wherever it points and however it was pasted", "[sharelink]")
{
    const std::string code = ShareLink::encode("play 60");
    CHECK(readBack("https://sonic-pi.net/code.html#code=" + code) == "play 60");
    CHECK(readBack("http://localhost:8460/web/#code=" + code + "\n") == "play 60");
    CHECK(readBack("  " + code + " ") == "play 60");
}

TEST_CASE("a link that can't be read says why", "[sharelink]")
{
    const std::string link = ShareLink::encode("live_loop :drums do\n  sample :bd_haus, amp: 2\n  sleep 0.5\nend\n");
    for (size_t n = 2; n < link.size(); n++)
        CHECK(readBack(link.substr(0, n)) == "error: the link's code is cut short");
    CHECK(readBack(ShareLink::encodeDigits("play 60").substr(0, 11)) == "error: the link's digits are cut short");
    CHECK(readBack("9abc") == "error: a link from a newer Sonic Pi (format \"9\")");
    CHECK(readBack("N9123") == "error: a link from a newer Sonic Pi (format \"9\")");
    CHECK(readBack("1A_V*pAA") == "error: the link's code has characters no link has");
    CHECK(readBack("https://sonic-pi.net/code.html#code=") == "error: the link has no code in it");
}

TEST_CASE("any bytes go to digits and back, every length", "[sharelink]")
{
    for (size_t n = 0; n < 60; n++)
    {
        ShareLink::Bytes bytes(n);
        for (size_t i = 0; i < n; i++)
            bytes[i] = (std::uint8_t)((i * 97 + n * 31) % 256);
        ShareLink::Bytes back;
        std::string error;
        REQUIRE(ShareLink::fromDigits(ShareLink::toDigits(bytes), back, error));
        CHECK(back == bytes);
    }
    ShareLink::Bytes back;
    std::string error;
    CHECK_FALSE(ShareLink::fromDigits("1234", back, error));               // no length of bytes is four digits
    CHECK_FALSE(ShareLink::fromDigits(std::string(17, '9'), back, error)); // more than seven bytes hold
}
