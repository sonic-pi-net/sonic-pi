//---------------------------------------------------------------------------------------
//
// Copyright (c) 2018, Steffen Schümann <s.schuemann@pobox.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
//---------------------------------------------------------------------------------------
#include <algorithm>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <random>
#include <set>
#include <sstream>
#include <thread>

#if (defined(WIN32) || defined(_WIN32)) && !defined(__GNUC__)
#define NOMINMAX 1
#endif

#ifdef USE_STD_FS
#include <filesystem>
namespace fs {
using namespace std::filesystem;
using ifstream = std::ifstream;
using ofstream = std::ofstream;
using fstream = std::fstream;
}  // namespace fs
#ifdef __GNUC__
#define GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#endif
#ifdef _MSC_VER
#define IS_WCHAR_PATH
#endif
#ifdef WIN32
#define GHC_OS_WINDOWS
#endif
#else
#ifdef GHC_FILESYSTEM_FWD_TEST
#include <ghc/fs_fwd.hpp>
#else
#include <ghc/filesystem.hpp>
#endif
namespace fs {
using namespace ghc::filesystem;
using ifstream = ghc::filesystem::ifstream;
using ofstream = ghc::filesystem::ofstream;
using fstream = ghc::filesystem::fstream;
}  // namespace fs
#endif

#if defined(WIN32) || defined(_WIN32)
#include <windows.h>
#else
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#endif

#ifndef GHC_FILESYSTEM_FWD_TEST
#define CATCH_CONFIG_MAIN
#endif
#include "catch.hpp"

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Behaviour Switches (should match the config in ghc/filesystem.hpp):
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// LWG #2682 disables the since then invalid use of the copy option create_symlinks on directories
#define TEST_LWG_2682_BEHAVIOUR
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// LWG #2395 makes crate_directory/create_directories not emit an error if there is a regular
// file with that name, it is superceded by P1164R1, so only activate if really needed
// #define TEST_LWG_2935_BEHAVIOUR
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// LWG #2937 enforces that fs::equivalent emits an error, if !fs::exists(p1)||!exists(p2)
#define TEST_LWG_2937_BEHAVIOUR
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

template <typename TP>
std::time_t to_time_t(TP tp)
{
    using namespace std::chrono;
    auto sctp = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());
    return system_clock::to_time_t(sctp);
}

template <typename TP>
TP from_time_t(std::time_t t)
{
    using namespace std::chrono;
    auto sctp = system_clock::from_time_t(t);
    auto tp = time_point_cast<typename TP::duration>(sctp - system_clock::now() + TP::clock::now());
    return tp;
}

namespace Catch {
template <>
struct StringMaker<fs::path>
{
    static std::string convert(fs::path const& value) { return '"' + value.string() + '"'; }
};

template <>
struct StringMaker<fs::perms>
{
    static std::string convert(fs::perms const& value) { return std::to_string(static_cast<unsigned int>(value)); }
};

#ifdef __cpp_lib_char8_t
template <>
struct StringMaker<char8_t>
{
    static std::string convert(char8_t const& value) { return std::to_string(static_cast<unsigned int>(value)); }
};
#endif

template <>
struct StringMaker<fs::file_time_type>
{
    static std::string convert(fs::file_time_type const& value)
    {
        std::time_t t = to_time_t(value);
        std::tm* ptm = std::localtime(&t);
        std::ostringstream os;
        if (ptm) {
            std::tm ttm = *ptm;
            os << std::put_time(&ttm, "%Y-%m-%d %H:%M:%S");
        }
        else {
            os << "(invalid-time)";
        }
        return os.str();
    }
};
}  // namespace Catch

enum class TempOpt { none, change_path };
class TemporaryDirectory
{
public:
    TemporaryDirectory(TempOpt opt = TempOpt::none)
    {
        static auto seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
        static auto rng = std::bind(std::uniform_int_distribution<int>(0, 35), std::mt19937(static_cast<unsigned int>(seed) ^ static_cast<unsigned int>(reinterpret_cast<ptrdiff_t>(&opt))));
        std::string filename;
        do {
            filename = "test_";
            for (int i = 0; i < 8; ++i) {
                filename += "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[rng()];
            }
            _path = fs::canonical(fs::temp_directory_path()) / filename;
        } while (fs::exists(_path));
        fs::create_directories(_path);
        if (opt == TempOpt::change_path) {
            _orig_dir = fs::current_path();
            fs::current_path(_path);
        }
    }

    ~TemporaryDirectory()
    {
        if (!_orig_dir.empty()) {
            fs::current_path(_orig_dir);
        }
        fs::remove_all(_path);
    }

    const fs::path& path() const { return _path; }

private:
    fs::path _path;
    fs::path _orig_dir;
};

static void generateFile(const fs::path& pathname, int withSize = -1)
{
    fs::ofstream outfile(pathname);
    if (withSize < 0) {
        outfile << "Hello world!" << std::endl;
    }
    else {
        outfile << std::string(size_t(withSize), '*');
    }
}

#ifdef GHC_OS_WINDOWS
inline bool isWow64Proc()
{
    typedef BOOL(WINAPI * IsWow64Process_t)(HANDLE, PBOOL);
    BOOL bIsWow64 = FALSE;
    auto fnIsWow64Process = (IsWow64Process_t)GetProcAddress(GetModuleHandle(TEXT("kernel32")), "IsWow64Process");
    if (NULL != fnIsWow64Process) {
        if (!fnIsWow64Process(GetCurrentProcess(), &bIsWow64)) {
            bIsWow64 = FALSE;
        }
    }
    return bIsWow64 == TRUE;
}

static bool is_symlink_creation_supported()
{
    bool result = true;
    HKEY key;
    REGSAM flags = KEY_READ;
#ifdef _WIN64
    flags |= KEY_WOW64_64KEY;
#elif defined(KEY_WOW64_64KEY)
    if (isWow64Proc()) {
        flags |= KEY_WOW64_64KEY;
    }
    else {
        flags |= KEY_WOW64_32KEY;
    }
#else
    result = false;
#endif
    if (result) {
        auto err = RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\AppModelUnlock", 0, flags, &key);
        if (err == ERROR_SUCCESS) {
            DWORD val = 0, size = sizeof(DWORD);
            err = RegQueryValueExW(key, L"AllowDevelopmentWithoutDevLicense", 0, NULL, reinterpret_cast<LPBYTE>(&val), &size);
            RegCloseKey(key);
            if (err != ERROR_SUCCESS) {
                result = false;
            }
            else {
                result = (val != 0);
            }
        }
        else {
            result = false;
        }
    }
    if (!result) {
        std::clog << "Warning: Symlink creation not supported." << std::endl;
    }
    return result;
}
#else
static bool is_symlink_creation_supported()
{
    return true;
}
#endif

static bool has_host_root_name_support()
{
    return fs::path("//host").has_root_name();
}

template <class T>
class TestAllocator
{
public:
    using value_type = T;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using difference_type = ptrdiff_t;
    using size_type = size_t;
    TestAllocator() noexcept {}
    template <class U>
    TestAllocator(TestAllocator<U> const&) noexcept
    {
    }
    value_type* allocate(std::size_t n) { return static_cast<value_type*>(::operator new(n * sizeof(value_type))); }
    void deallocate(value_type* p, std::size_t) noexcept { ::operator delete(p); }
    template<class U>
    struct rebind {
        typedef TestAllocator<U> other;
    };
};

template <class T, class U>
bool operator==(TestAllocator<T> const&, TestAllocator<U> const&) noexcept
{
    return true;
}

template <class T, class U>
bool operator!=(TestAllocator<T> const& x, TestAllocator<U> const& y) noexcept
{
    return !(x == y);
}

TEST_CASE("Temporary Directory", "[fs.test.tempdir]")
{
    fs::path tempPath;
    {
        TemporaryDirectory t;
        tempPath = t.path();
        REQUIRE(fs::exists(fs::path(t.path())));
        REQUIRE(fs::is_directory(t.path()));
    }
    REQUIRE(!fs::exists(tempPath));
}

#ifdef GHC_FILESYSTEM_VERSION
TEST_CASE("fs::detail::fromUtf8", "[filesystem][fs.detail.utf8]")
{
    CHECK(fs::detail::fromUtf8<std::wstring>("foobar").length() == 6);
    CHECK(fs::detail::fromUtf8<std::wstring>("foobar") == L"foobar");
    CHECK(fs::detail::fromUtf8<std::wstring>(u8"föobar").length() == 6);
    CHECK(fs::detail::fromUtf8<std::wstring>(u8"föobar") == L"föobar");

    CHECK(fs::detail::toUtf8(std::wstring(L"foobar")).length() == 6);
    CHECK(fs::detail::toUtf8(std::wstring(L"foobar")) == "foobar");
    CHECK(fs::detail::toUtf8(std::wstring(L"föobar")).length() == 7);
    //CHECK(fs::detail::toUtf8(std::wstring(L"föobar")) == u8"föobar");

#ifdef GHC_RAISE_UNICODE_ERRORS
    CHECK_THROWS_AS(fs::detail::fromUtf8<std::u16string>(std::string("\xed\xa0\x80")), fs::filesystem_error);
    CHECK_THROWS_AS(fs::detail::fromUtf8<std::u16string>(std::string("\xc3")), fs::filesystem_error);
#else
    CHECK(std::u16string(2,0xfffd) == fs::detail::fromUtf8<std::u16string>(std::string("\xed\xa0\x80")));
    CHECK(std::u16string(1,0xfffd) == fs::detail::fromUtf8<std::u16string>(std::string("\xc3")));
#endif
}

TEST_CASE("fs::detail::toUtf8", "[filesystem][fs.detail.utf8]")
{
    std::string t;
    CHECK(std::string("\xc3\xa4/\xe2\x82\xac\xf0\x9d\x84\x9e") == fs::detail::toUtf8(std::u16string(u"\u00E4/\u20AC\U0001D11E")));
#ifdef GHC_RAISE_UNICODE_ERRORS
    CHECK_THROWS_AS(fs::detail::toUtf8(std::u16string(1, 0xd800)), fs::filesystem_error);
    CHECK_THROWS_AS(fs::detail::appendUTF8(t, 0x200000), fs::filesystem_error);
#else
    CHECK(std::string("\xEF\xBF\xBD") == fs::detail::toUtf8(std::u16string(1, 0xd800)));
    fs::detail::appendUTF8(t, 0x200000);
    CHECK(std::string("\xEF\xBF\xBD") == t);
#endif
}
#endif

TEST_CASE("30.10.8.1 path::preferred_separator", "[filesystem][path][fs.path.generic]")
{
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path::preferred_separator == '\\');
#else
    CHECK(fs::path::preferred_separator == '/');
#endif
}

#ifndef GHC_OS_WINDOWS
TEST_CASE("30.10.8.1 path(\"//host\").has_root_name()", "[filesystem][path][fs.path.generic]")
{
    if (!has_host_root_name_support()) {
        WARN("This implementation doesn't support path(\"//host\").has_root_name() == true [C++17 30.12.8.1 par. 4] on this platform, tests based on this are skipped. (Should be okay.)");
    }
}
#endif

TEST_CASE("30.10.8.4.1 path constructors and destructor", "[filesystem][path][fs.path.construct]")
{
    CHECK("/usr/local/bin" == fs::path("/usr/local/bin").generic_string());
    std::string str = "/usr/local/bin";
    std::u16string u16str = u"/usr/local/bin";
    std::u32string u32str = U"/usr/local/bin";
    CHECK(str == fs::path(str, fs::path::format::generic_format));
    CHECK(str == fs::path(str.begin(), str.end()));
    CHECK(fs::path(std::wstring(3, 67)) == "CCC");
    CHECK(str == fs::path(u16str.begin(), u16str.end()));
    CHECK(str == fs::path(u32str.begin(), u32str.end()));
#ifdef GHC_FILESYSTEM_VERSION
    CHECK(fs::path("///foo/bar") == "/foo/bar");
    CHECK(fs::path("//foo//bar") == "//foo/bar");
#endif
#ifdef GHC_OS_WINDOWS
    CHECK("\\usr\\local\\bin" == fs::path("/usr/local/bin"));
    CHECK("C:\\usr\\local\\bin" == fs::path("C:\\usr\\local\\bin"));
#else
    CHECK("/usr/local/bin" == fs::path("/usr/local/bin"));
#endif
    if (has_host_root_name_support()) {
        CHECK("//host/foo/bar" == fs::path("//host/foo/bar"));
    }

#if !defined(GHC_OS_WINDOWS) && !(defined(GCC_VERSION) && GCC_VERSION < 80100) && !defined(USE_STD_FS)
    std::locale loc;
    bool testUTF8Locale = false;
    try {
        if (const char* lang = std::getenv("LANG")) {
            loc = std::locale(lang);
        }
        else {
            loc = std::locale("en_US.UTF-8");
        }
        std::string name = loc.name();
        if (name.length() > 5 && (name.substr(name.length() - 5) == "UTF-8" || name.substr(name.length() - 5) == "utf-8")) {
            testUTF8Locale = true;
        }
    }
    catch (std::runtime_error&) {
        WARN("Couldn't create an UTF-8 locale!");
    }
    if (testUTF8Locale) {
        CHECK("/usr/local/bin" == fs::path("/usr/local/bin", loc));
        CHECK(str == fs::path(str.begin(), str.end(), loc));
        CHECK(str == fs::path(u16str.begin(), u16str.end(), loc));
        CHECK(str == fs::path(u32str.begin(), u32str.end(), loc));
    }
#endif
}

TEST_CASE("30.10.8.4.2 path assignments", "[filesystem][path][fs.path.assign]")
{
    fs::path p1{"/foo/bar"};
    fs::path p2{"/usr/local"};
    fs::path p3;
    p3 = p1;
    REQUIRE(p1 == p3);
    p3 = fs::path{"/usr/local"};
    REQUIRE(p2 == p3);
#if defined(IS_WCHAR_PATH) || defined(GHC_USE_WCHAR_T)
    p3 = fs::path::string_type{L"/foo/bar"};
    REQUIRE(p1 == p3);
    p3.assign(fs::path::string_type{L"/usr/local"});
    REQUIRE(p2 == p3);
#else
    p3 = fs::path::string_type{"/foo/bar"};
    REQUIRE(p1 == p3);
    p3.assign(fs::path::string_type{"/usr/local"});
    REQUIRE(p2 == p3);
#endif
    p3 = std::u16string(u"/foo/bar");
    REQUIRE(p1 == p3);
    p3 = U"/usr/local";
    REQUIRE(p2 == p3);
    p3.assign(std::u16string(u"/foo/bar"));
    REQUIRE(p1 == p3);
    std::string s{"/usr/local"};
    p3.assign(s.begin(), s.end());
    REQUIRE(p2 == p3);
}

TEST_CASE("30.10.8.4.3 path appends", "[filesystem][path][fs.path.append]")
{
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("foo") / "c:/bar" == "c:/bar");
    CHECK(fs::path("foo") / "c:" == "c:");
    CHECK(fs::path("c:") / "" == "c:");
    CHECK(fs::path("c:foo") / "/bar" == "c:/bar");
    CHECK(fs::path("c:foo") / "c:bar" == "c:foo/bar");
#else
    CHECK(fs::path("foo") / "" == "foo/");
    CHECK(fs::path("foo") / "/bar" == "/bar");
    CHECK(fs::path("/foo") / "/" == "/");
    if (has_host_root_name_support()) {
        CHECK(fs::path("//host/foo") / "/bar" == "/bar");
        CHECK(fs::path("//host") / "/" == "//host/");
        CHECK(fs::path("//host/foo") / "/" == "/");
    }
#endif
    CHECK(fs::path("/foo/bar") / "some///other" == "/foo/bar/some/other");
    fs::path p1{"/tmp/test"};
    fs::path p2{"foobar.txt"};
    fs::path p3 = p1 / p2;
    CHECK("/tmp/test/foobar.txt" == p3);
    // TODO: append(first, last)
}

TEST_CASE("30.10.8.4.4 path concatenation", "[filesystem][path][fs.path.concat]")
{
    CHECK((fs::path("foo") += fs::path("bar")) == "foobar");
    CHECK((fs::path("foo") += fs::path("/bar")) == "foo/bar");

    CHECK((fs::path("foo") += std::string("bar")) == "foobar");
    CHECK((fs::path("foo") += std::string("/bar")) == "foo/bar");

    CHECK((fs::path("foo") += "bar") == "foobar");
    CHECK((fs::path("foo") += "/bar") == "foo/bar");

    CHECK((fs::path("foo") += 'b') == "foob");
    CHECK((fs::path("foo") += '/') == "foo/");

    CHECK((fs::path("foo") += std::string("bar")) == "foobar");
    CHECK((fs::path("foo") += std::string("/bar")) == "foo/bar");

    CHECK((fs::path("foo") += std::u16string(u"bar")) == "foobar");
    CHECK((fs::path("foo") += std::u16string(u"/bar")) == "foo/bar");

    CHECK((fs::path("foo") += std::u32string(U"bar")) == "foobar");
    CHECK((fs::path("foo") += std::u32string(U"/bar")) == "foo/bar");

    CHECK(fs::path("foo").concat("bar") == "foobar");
    CHECK(fs::path("foo").concat("/bar") == "foo/bar");
    std::string bar = "bar";
    CHECK(fs::path("foo").concat(bar.begin(), bar.end()) == "foobar");
#ifndef USE_STD_FS
    CHECK((fs::path("/foo/bar") += "/some///other") == "/foo/bar/some/other");
#endif
    // TODO: contat(first, last)
}

TEST_CASE("30.10.8.4.5 path modifiers", "[filesystem][path][fs.path.modifiers]")
{
    fs::path p = fs::path("/foo/bar");
    p.clear();
    CHECK(p == "");

    // make_preferred() is a no-op
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("foo\\bar") == "foo/bar");
    CHECK(fs::path("foo\\bar").make_preferred() == "foo/bar");
#else
    CHECK(fs::path("foo\\bar") == "foo\\bar");
    CHECK(fs::path("foo\\bar").make_preferred() == "foo\\bar");
#endif
    CHECK(fs::path("foo/bar").make_preferred() == "foo/bar");

    CHECK(fs::path("foo/bar").remove_filename() == "foo/");
    CHECK(fs::path("foo/").remove_filename() == "foo/");
    CHECK(fs::path("/foo").remove_filename() == "/");
    CHECK(fs::path("/").remove_filename() == "/");

    CHECK(fs::path("/foo").replace_filename("bar") == "/bar");
    CHECK(fs::path("/").replace_filename("bar") == "/bar");
    CHECK(fs::path("/foo").replace_filename("b//ar") == "/b/ar");

    CHECK(fs::path("/foo/bar.txt").replace_extension("odf") == "/foo/bar.odf");
    CHECK(fs::path("/foo/bar.txt").replace_extension() == "/foo/bar");
    CHECK(fs::path("/foo/bar").replace_extension("odf") == "/foo/bar.odf");
    CHECK(fs::path("/foo/bar").replace_extension(".odf") == "/foo/bar.odf");
    CHECK(fs::path("/foo/bar.").replace_extension(".odf") == "/foo/bar.odf");
    CHECK(fs::path("/foo/bar/").replace_extension("odf") == "/foo/bar/.odf");

    fs::path p1 = "foo";
    fs::path p2 = "bar";
    p1.swap(p2);
    CHECK(p1 == "bar");
    CHECK(p2 == "foo");
}

TEST_CASE("30.10.8.4.6 path native format observers", "[filesystem][path][fs.path.native.obs]")
{
#ifdef GHC_OS_WINDOWS
#if defined(IS_WCHAR_PATH) || defined(GHC_USE_WCHAR_T)
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").native() == fs::path::string_type(L"\u00E4\\\u20AC"));
    // CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").string() == std::string("ä\\€")); // MSVCs returns local DBCS encoding
#else
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").native() == fs::path::string_type("\xc3\xa4\\\xe2\x82\xac"));
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").string() == std::string("\xc3\xa4\\\xe2\x82\xac"));
    CHECK(!::strcmp(fs::u8path("\xc3\xa4\\\xe2\x82\xac").c_str(), "\xc3\xa4\\\xe2\x82\xac"));
    CHECK((std::string)fs::u8path("\xc3\xa4\\\xe2\x82\xac") == std::string("\xc3\xa4\\\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").wstring() == std::wstring(L"\u00E4\\\u20AC"));
#if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").u8string() == std::u8string(u8"\u00E4\\\u20AC"));
#else
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").u8string() == std::string("\xc3\xa4\\\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").u16string() == std::u16string(u"\u00E4\\\u20AC"));
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").u32string() == std::u32string(U"\U000000E4\\\U000020AC"));
#else
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").native() == fs::path::string_type("\xc3\xa4/\xe2\x82\xac"));
    CHECK(!::strcmp(fs::u8path("\xc3\xa4/\xe2\x82\xac").c_str(), "\xc3\xa4/\xe2\x82\xac"));
    CHECK((std::string)fs::u8path("\xc3\xa4/\xe2\x82\xac") == std::string("\xc3\xa4/\xe2\x82\xac"));
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").string() == std::string("\xc3\xa4/\xe2\x82\xac"));
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").wstring() == std::wstring(L"ä/€"));
#if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").u8string() == std::u8string(u8"\xc3\xa4/\xe2\x82\xac"));
#else
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").u8string() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").u16string() == std::u16string(u"\u00E4/\u20AC"));
    INFO("This check might fail on GCC8 (with \"Illegal byte sequence\") due to not detecting the valid unicode codepoint U+1D11E.");
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac\xf0\x9d\x84\x9e").u16string() == std::u16string(u"\u00E4/\u20AC\U0001D11E"));
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").u32string() == std::u32string(U"\U000000E4/\U000020AC"));
#endif
}

TEST_CASE("30.10.8.4.7 path generic format observers", "[filesystem][path][fs.path.generic.obs]")
{
#ifdef GHC_OS_WINDOWS
#ifndef IS_WCHAR_PATH
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_string() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
#ifndef USE_STD_FS
    auto t = fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_string<char, std::char_traits<char>, TestAllocator<char>>();
    CHECK(t.c_str() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_wstring() == std::wstring(L"\U000000E4/\U000020AC"));
#if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_u8string() == std::u8string(u8"\u00E4/\u20AC"));
#else
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_u8string() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_u16string() == std::u16string(u"\u00E4/\u20AC"));
    CHECK(fs::u8path("\xc3\xa4\\\xe2\x82\xac").generic_u32string() == std::u32string(U"\U000000E4/\U000020AC"));
#else
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_string() == std::string("\xc3\xa4/\xe2\x82\xac"));
#ifndef USE_STD_FS
    auto t = fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_string<char, std::char_traits<char>, TestAllocator<char>>();
    CHECK(t.c_str() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_wstring() == std::wstring(L"ä/€"));
#if defined(__cpp_lib_char8_t) && !defined(GHC_FILESYSTEM_ENFORCE_CPP17_API)
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_u8string() == std::u8string(u8"\xc3\xa4/\xe2\x82\xac"));
#else
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_u8string() == std::string("\xc3\xa4/\xe2\x82\xac"));
#endif
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_u16string() == std::u16string(u"\u00E4/\u20AC"));
    CHECK(fs::u8path("\xc3\xa4/\xe2\x82\xac").generic_u32string() == std::u32string(U"\U000000E4/\U000020AC"));
#endif
}

TEST_CASE("30.10.8.4.8 path compare", "[filesystem][path][fs.path.compare]")
{
    CHECK(fs::path("/foo/b").compare("/foo/a") > 0);
    CHECK(fs::path("/foo/b").compare("/foo/b") == 0);
    CHECK(fs::path("/foo/b").compare("/foo/c") < 0);

    CHECK(fs::path("/foo/b").compare(std::string("/foo/a")) > 0);
    CHECK(fs::path("/foo/b").compare(std::string("/foo/b")) == 0);
    CHECK(fs::path("/foo/b").compare(std::string("/foo/c")) < 0);

    CHECK(fs::path("/foo/b").compare(fs::path("/foo/a")) > 0);
    CHECK(fs::path("/foo/b").compare(fs::path("/foo/b")) == 0);
    CHECK(fs::path("/foo/b").compare(fs::path("/foo/c")) < 0);

#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("c:\\a\\b").compare("C:\\a\\b") == 0);
    CHECK(fs::path("c:\\a\\b").compare("d:\\a\\b") != 0);
    CHECK(fs::path("c:\\a\\b").compare("C:\\A\\b") != 0);
#endif

#ifdef LWG_2936_BEHAVIOUR
    CHECK(fs::path("/a/b/").compare("/a/b/c") < 0);
    CHECK(fs::path("/a/b/").compare("a/c") > 0);
#endif // LWG_2936_BEHAVIOUR
}

TEST_CASE("30.10.8.4.9 path decomposition", "[filesystem][path][fs.path.decompose]")
{
    // root_name()
    CHECK(fs::path("").root_name() == "");
    CHECK(fs::path(".").root_name() == "");
    CHECK(fs::path("..").root_name() == "");
    CHECK(fs::path("foo").root_name() == "");
    CHECK(fs::path("/").root_name() == "");
    CHECK(fs::path("/foo").root_name() == "");
    CHECK(fs::path("foo/").root_name() == "");
    CHECK(fs::path("/foo/").root_name() == "");
    CHECK(fs::path("foo/bar").root_name() == "");
    CHECK(fs::path("/foo/bar").root_name() == "");
    CHECK(fs::path("///foo/bar").root_name() == "");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").root_name() == "C:");
    CHECK(fs::path("C:\\foo").root_name() == "C:");
    CHECK(fs::path("C:foo").root_name() == "C:");
#endif

    // root_directory()
    CHECK(fs::path("").root_directory() == "");
    CHECK(fs::path(".").root_directory() == "");
    CHECK(fs::path("..").root_directory() == "");
    CHECK(fs::path("foo").root_directory() == "");
    CHECK(fs::path("/").root_directory() == "/");
    CHECK(fs::path("/foo").root_directory() == "/");
    CHECK(fs::path("foo/").root_directory() == "");
    CHECK(fs::path("/foo/").root_directory() == "/");
    CHECK(fs::path("foo/bar").root_directory() == "");
    CHECK(fs::path("/foo/bar").root_directory() == "/");
    CHECK(fs::path("///foo/bar").root_directory() == "/");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").root_directory() == "/");
    CHECK(fs::path("C:\\foo").root_directory() == "/");
    CHECK(fs::path("C:foo").root_directory() == "");
#endif

    // root_path()
    CHECK(fs::path("").root_path() == "");
    CHECK(fs::path(".").root_path() == "");
    CHECK(fs::path("..").root_path() == "");
    CHECK(fs::path("foo").root_path() == "");
    CHECK(fs::path("/").root_path() == "/");
    CHECK(fs::path("/foo").root_path() == "/");
    CHECK(fs::path("foo/").root_path() == "");
    CHECK(fs::path("/foo/").root_path() == "/");
    CHECK(fs::path("foo/bar").root_path() == "");
    CHECK(fs::path("/foo/bar").root_path() == "/");
    CHECK(fs::path("///foo/bar").root_path() == "/");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").root_path() == "C:/");
    CHECK(fs::path("C:\\foo").root_path() == "C:/");
    CHECK(fs::path("C:foo").root_path() == "C:");
#endif

    // relative_path()
    CHECK(fs::path("").relative_path() == "");
    CHECK(fs::path(".").relative_path() == ".");
    CHECK(fs::path("..").relative_path() == "..");
    CHECK(fs::path("foo").relative_path() == "foo");
    CHECK(fs::path("/").relative_path() == "");
    CHECK(fs::path("/foo").relative_path() == "foo");
    CHECK(fs::path("foo/").relative_path() == "foo/");
    CHECK(fs::path("/foo/").relative_path() == "foo/");
    CHECK(fs::path("foo/bar").relative_path() == "foo/bar");
    CHECK(fs::path("/foo/bar").relative_path() == "foo/bar");
    CHECK(fs::path("///foo/bar").relative_path() == "foo/bar");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").relative_path() == "foo");
    CHECK(fs::path("C:\\foo").relative_path() == "foo");
    CHECK(fs::path("C:foo").relative_path() == "foo");
#endif

    // parent_path()
    CHECK(fs::path("").parent_path() == "");
    CHECK(fs::path(".").parent_path() == "");
    CHECK(fs::path("..").parent_path() == "");  // unintuitive but as defined in the standard
    CHECK(fs::path("foo").parent_path() == "");
    CHECK(fs::path("/").parent_path() == "/");
    CHECK(fs::path("/foo").parent_path() == "/");
    CHECK(fs::path("foo/").parent_path() == "foo");
    CHECK(fs::path("/foo/").parent_path() == "/foo");
    CHECK(fs::path("foo/bar").parent_path() == "foo");
    CHECK(fs::path("/foo/bar").parent_path() == "/foo");
    CHECK(fs::path("///foo/bar").parent_path() == "/foo");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").parent_path() == "C:/");
    CHECK(fs::path("C:\\foo").parent_path() == "C:/");
    CHECK(fs::path("C:foo").parent_path() == "C:");
#endif

    // filename()
    CHECK(fs::path("").filename() == "");
    CHECK(fs::path(".").filename() == ".");
    CHECK(fs::path("..").filename() == "..");
    CHECK(fs::path("foo").filename() == "foo");
    CHECK(fs::path("/").filename() == "");
    CHECK(fs::path("/foo").filename() == "foo");
    CHECK(fs::path("foo/").filename() == "");
    CHECK(fs::path("/foo/").filename() == "");
    CHECK(fs::path("foo/bar").filename() == "bar");
    CHECK(fs::path("/foo/bar").filename() == "bar");
    CHECK(fs::path("///foo/bar").filename() == "bar");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:/foo").filename() == "foo");
    CHECK(fs::path("C:\\foo").filename() == "foo");
    CHECK(fs::path("C:foo").filename() == "foo");
#endif

    // stem()
    CHECK(fs::path("/foo/bar.txt").stem() == "bar");
    {
        fs::path p = "foo.bar.baz.tar";
        CHECK(p.extension() == ".tar");
        p = p.stem();
        CHECK(p.extension() == ".baz");
        p = p.stem();
        CHECK(p.extension() == ".bar");
        p = p.stem();
        CHECK(p == "foo");
    }
    CHECK(fs::path("/foo/.profile").stem() == ".profile");
    CHECK(fs::path(".bar").stem() == ".bar");
    CHECK(fs::path("..bar").stem() == ".");

    // extension()
    CHECK(fs::path("/foo/bar.txt").extension() == ".txt");
    CHECK(fs::path("/foo/bar").extension() == "");
    CHECK(fs::path("/foo/.profile").extension() == "");
    CHECK(fs::path(".bar").extension() == "");
    CHECK(fs::path("..bar").extension() == ".bar");

    if (has_host_root_name_support()) {
        // //host-based root-names
        CHECK(fs::path("//host").root_name() == "//host");
        CHECK(fs::path("//host/foo").root_name() == "//host");
        CHECK(fs::path("//host").root_directory() == "");
        CHECK(fs::path("//host/foo").root_directory() == "/");
        CHECK(fs::path("//host").root_path() == "//host");
        CHECK(fs::path("//host/foo").root_path() == "//host/");
        CHECK(fs::path("//host").relative_path() == "");
        CHECK(fs::path("//host/foo").relative_path() == "foo");
        CHECK(fs::path("//host").parent_path() == "//host");
        CHECK(fs::path("//host/foo").parent_path() == "//host/");
        CHECK(fs::path("//host").filename() == "");
        CHECK(fs::path("//host/foo").filename() == "foo");
    }
}

TEST_CASE("30.10.8.4.10 path query", "[fielsystem][path][fs.path.query]")
{
    // empty
    CHECK(fs::path("").empty());
    CHECK(!fs::path("foo").empty());

    // has_root_path()
    CHECK(!fs::path("foo").has_root_path());
    CHECK(!fs::path("foo/bar").has_root_path());
    CHECK(fs::path("/foo").has_root_path());
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:foo").has_root_path());
    CHECK(fs::path("C:/foo").has_root_path());
#endif

    // has_root_name()
    CHECK(!fs::path("foo").has_root_name());
    CHECK(!fs::path("foo/bar").has_root_name());
    CHECK(!fs::path("/foo").has_root_name());
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("C:foo").has_root_name());
    CHECK(fs::path("C:/foo").has_root_name());
#endif

    // has_root_directory()
    CHECK(!fs::path("foo").has_root_directory());
    CHECK(!fs::path("foo/bar").has_root_directory());
    CHECK(fs::path("/foo").has_root_directory());
#ifdef GHC_OS_WINDOWS
    CHECK(!fs::path("C:foo").has_root_directory());
    CHECK(fs::path("C:/foo").has_root_directory());
#endif

    // has_relative_path()
    CHECK(!fs::path("").has_relative_path());
    CHECK(!fs::path("/").has_relative_path());
    CHECK(fs::path("/foo").has_relative_path());

    // has_parent_path()
    CHECK(!fs::path("").has_parent_path());
    CHECK(!fs::path(".").has_parent_path());
    CHECK(!fs::path("..").has_parent_path());  // unintuitive but as defined in the standard
    CHECK(!fs::path("foo").has_parent_path());
    CHECK(fs::path("/").has_parent_path());
    CHECK(fs::path("/foo").has_parent_path());
    CHECK(fs::path("foo/").has_parent_path());
    CHECK(fs::path("/foo/").has_parent_path());

    // has_filename()
    CHECK(fs::path("foo").has_filename());
    CHECK(fs::path("foo/bar").has_filename());
    CHECK(!fs::path("/foo/bar/").has_filename());

    // has_stem()
    CHECK(fs::path("foo").has_stem());
    CHECK(fs::path("foo.bar").has_stem());
    CHECK(fs::path(".profile").has_stem());
    CHECK(!fs::path("/foo/").has_stem());

    // has_extension()
    CHECK(!fs::path("foo").has_extension());
    CHECK(fs::path("foo.bar").has_extension());
    CHECK(!fs::path(".profile").has_extension());

    // is_absolute()
    CHECK(!fs::path("foo/bar").is_absolute());
#ifdef GHC_OS_WINDOWS
    CHECK(!fs::path("/foo").is_absolute());
    CHECK(!fs::path("c:foo").is_absolute());
    CHECK(fs::path("c:/foo").is_absolute());
#else
    CHECK(fs::path("/foo").is_absolute());
#endif

    // is_relative()
    CHECK(fs::path("foo/bar").is_relative());
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("/foo").is_relative());
    CHECK(fs::path("c:foo").is_relative());
    CHECK(!fs::path("c:/foo").is_relative());
#else
    CHECK(!fs::path("/foo").is_relative());
#endif

    if (has_host_root_name_support()) {
        CHECK(fs::path("//host").has_root_name());
        CHECK(fs::path("//host/foo").has_root_name());
        CHECK(fs::path("//host").has_root_path());
        CHECK(fs::path("//host/foo").has_root_path());
        CHECK(!fs::path("//host").has_root_directory());
        CHECK(fs::path("//host/foo").has_root_directory());
        CHECK(!fs::path("//host").has_relative_path());
        CHECK(fs::path("//host/foo").has_relative_path());
        CHECK(fs::path("//host/foo").is_absolute());
        CHECK(!fs::path("//host/foo").is_relative());
    }
}

TEST_CASE("30.10.8.4.11 path generation", "[filesystem][path][fs.path.gen]")
{
    // lexically_normal()
    CHECK(fs::path("foo/./bar/..").lexically_normal() == "foo/");
    CHECK(fs::path("foo/.///bar/../").lexically_normal() == "foo/");
    CHECK(fs::path("/foo/../..").lexically_normal() == "/");
    CHECK(fs::path("foo/..").lexically_normal() == ".");
    CHECK(fs::path("ab/cd/ef/../../qw").lexically_normal() == "ab/qw");
    CHECK(fs::path("a/b/../../../c").lexically_normal() == "../c");
    CHECK(fs::path("../").lexically_normal() == "..");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("\\/\\///\\/").lexically_normal() == "/");
    CHECK(fs::path("a/b/..\\//..///\\/../c\\\\/").lexically_normal() == "../c/");
    CHECK(fs::path("..a/b/..\\//..///\\/../c\\\\/").lexically_normal() == "../c/");
    CHECK(fs::path("..\\").lexically_normal() == "..");
#endif

    // lexically_relative()
    CHECK(fs::path("/a/d").lexically_relative("/a/b/c") == "../../d");
    CHECK(fs::path("/a/b/c").lexically_relative("/a/d") == "../b/c");
    CHECK(fs::path("a/b/c").lexically_relative("a") == "b/c");
    CHECK(fs::path("a/b/c").lexically_relative("a/b/c/x/y") == "../..");
    CHECK(fs::path("a/b/c").lexically_relative("a/b/c") == ".");
    CHECK(fs::path("a/b").lexically_relative("c/d") == "../../a/b");
    CHECK(fs::path("a/b").lexically_relative("a/") == "b");
    if (has_host_root_name_support()) {
        CHECK(fs::path("//host1/foo").lexically_relative("//host2.bar") == "");
    }
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("c:/foo").lexically_relative("/bar") == "");
    CHECK(fs::path("c:foo").lexically_relative("c:/bar") == "");
    CHECK(fs::path("foo").lexically_relative("/bar") == "");
    CHECK(fs::path("c:/foo/bar.txt").lexically_relative("c:/foo/") == "bar.txt");
    CHECK(fs::path("c:/foo/bar.txt").lexically_relative("C:/foo/") == "bar.txt");
#else
    CHECK(fs::path("/foo").lexically_relative("bar") == "");
    CHECK(fs::path("foo").lexically_relative("/bar") == "");
#endif

    // lexically_proximate()
    CHECK(fs::path("/a/d").lexically_proximate("/a/b/c") == "../../d");
    if (has_host_root_name_support()) {
        CHECK(fs::path("//host1/a/d").lexically_proximate("//host2/a/b/c") == "//host1/a/d");
    }
    CHECK(fs::path("a/d").lexically_proximate("/a/b/c") == "a/d");
#ifdef GHC_OS_WINDOWS
    CHECK(fs::path("c:/a/d").lexically_proximate("c:/a/b/c") == "../../d");
    CHECK(fs::path("c:/a/d").lexically_proximate("d:/a/b/c") == "c:/a/d");
    CHECK(fs::path("c:/foo").lexically_proximate("/bar") == "c:/foo");
    CHECK(fs::path("c:foo").lexically_proximate("c:/bar") == "c:foo");
    CHECK(fs::path("foo").lexically_proximate("/bar") == "foo");
#else
    CHECK(fs::path("/foo").lexically_proximate("bar") == "/foo");
    CHECK(fs::path("foo").lexically_proximate("/bar") == "foo");
#endif
}

static std::string iterateResult(const fs::path& path)
{
    std::ostringstream result;
    for (fs::path::const_iterator i = path.begin(); i != path.end(); ++i) {
        if (i != path.begin()) {
            result << ",";
        }
        result << i->generic_string();
    }
    return result.str();
}

static std::string reverseIterateResult(const fs::path& path)
{
    std::ostringstream result;
    fs::path::const_iterator iter = path.end();
    bool first = true;
    if (iter != path.begin()) {
        do {
            --iter;
            if (!first) {
                result << ",";
            }
            first = false;
            result << iter->generic_string();
        } while (iter != path.begin());
    }
    return result.str();
}

TEST_CASE("30.10.8.5 path iterators", "[filesystem][path][fs.path.itr]")
{
    CHECK(iterateResult(fs::path()).empty());
    CHECK("." == iterateResult(fs::path(".")));
    CHECK(".." == iterateResult(fs::path("..")));
    CHECK("foo" == iterateResult(fs::path("foo")));
    CHECK("/" == iterateResult(fs::path("/")));
    CHECK("/,foo" == iterateResult(fs::path("/foo")));
    CHECK("foo," == iterateResult(fs::path("foo/")));
    CHECK("/,foo," == iterateResult(fs::path("/foo/")));
    CHECK("foo,bar" == iterateResult(fs::path("foo/bar")));
    CHECK("/,foo,bar" == iterateResult(fs::path("/foo/bar")));
#ifndef USE_STD_FS
    // ghc::filesystem enforces redundant slashes to be reduced to one
    CHECK("/,foo,bar" == iterateResult(fs::path("///foo/bar")));
#else
    // typically std::filesystem keeps them
    CHECK("///,foo,bar" == iterateResult(fs::path("///foo/bar")));
#endif
    CHECK("/,foo,bar," == iterateResult(fs::path("/foo/bar///")));
    CHECK("foo,.,bar,..," == iterateResult(fs::path("foo/.///bar/../")));
#ifdef GHC_OS_WINDOWS
    CHECK("C:,/,foo" == iterateResult(fs::path("C:/foo")));
#endif

    CHECK(reverseIterateResult(fs::path()).empty());
    CHECK("." == reverseIterateResult(fs::path(".")));
    CHECK(".." == reverseIterateResult(fs::path("..")));
    CHECK("foo" == reverseIterateResult(fs::path("foo")));
    CHECK("/" == reverseIterateResult(fs::path("/")));
    CHECK("foo,/" == reverseIterateResult(fs::path("/foo")));
    CHECK(",foo" == reverseIterateResult(fs::path("foo/")));
    CHECK(",foo,/" == reverseIterateResult(fs::path("/foo/")));
    CHECK("bar,foo" == reverseIterateResult(fs::path("foo/bar")));
    CHECK("bar,foo,/" == reverseIterateResult(fs::path("/foo/bar")));
#ifndef USE_STD_FS
    // ghc::filesystem enforces redundant slashes to be reduced to one
    CHECK("bar,foo,/" == reverseIterateResult(fs::path("///foo/bar")));
#else
    // typically std::filesystem keeps them
    CHECK("bar,foo,///" == reverseIterateResult(fs::path("///foo/bar")));
#endif
    CHECK(",bar,foo,/" == reverseIterateResult(fs::path("/foo/bar///")));
    CHECK(",..,bar,.,foo" == reverseIterateResult(fs::path("foo/.///bar/../")));
#ifdef GHC_OS_WINDOWS
    CHECK("foo,/,C:" == reverseIterateResult(fs::path("C:/foo")));
    CHECK("foo,C:" == reverseIterateResult(fs::path("C:foo")));
#endif
    {
        fs::path p1 = "/foo/bar/test.txt";
        fs::path p2;
        for (auto pe : p1) {
            p2 /= pe;
        }
        CHECK(p1 == p2);
        CHECK("bar" == *(--fs::path("/foo/bar").end()));
        auto p = fs::path("/foo/bar");
        auto pi = p.end();
        pi--;
        CHECK("bar" == *pi);
    }

    if (has_host_root_name_support()) {
        CHECK("foo" == *(--fs::path("//host/foo").end()));
        auto p = fs::path("//host/foo");
        auto pi = p.end();
        pi--;
        CHECK("foo" == *pi);
        CHECK("//host" == iterateResult(fs::path("//host")));
        CHECK("//host,/,foo" == iterateResult(fs::path("//host/foo")));
        CHECK("//host" == reverseIterateResult(fs::path("//host")));
        CHECK("foo,/,//host" == reverseIterateResult(fs::path("//host/foo")));
        {
            fs::path p1 = "//host/foo/bar/test.txt";
            fs::path p2;
            for (auto pe : p1) {
                p2 /= pe;
            }
            CHECK(p1 == p2);
        }
    }
}

TEST_CASE("30.10.8.6 path non-member functions", "[filesystem][path][fs.path.nonmember]")
{
    fs::path p1("foo/bar");
    fs::path p2("some/other");
    fs::swap(p1, p2);
    CHECK(p1 == "some/other");
    CHECK(p2 == "foo/bar");
    CHECK(hash_value(p1));
    CHECK(p2 < p1);
    CHECK(p2 <= p1);
    CHECK(p1 <= p1);
    CHECK(!(p1 < p2));
    CHECK(!(p1 <= p2));
    CHECK(p1 > p2);
    CHECK(p1 >= p2);
    CHECK(p1 >= p1);
    CHECK(!(p2 > p1));
    CHECK(!(p2 >= p1));
    CHECK(p1 != p2);
    CHECK(p1 / p2 == "some/other/foo/bar");
}

TEST_CASE("30.10.8.6.1 path inserter and extractor", "[filesystem][path][fs.path.io]")
{
    {
        std::ostringstream os;
        os << fs::path("/root/foo bar");
#ifdef GHC_OS_WINDOWS
        CHECK(os.str() == "\"\\\\root\\\\foo bar\"");
#else
        CHECK(os.str() == "\"/root/foo bar\"");
#endif
    }
    {
        std::ostringstream os;
        os << fs::path("/root/foo\"bar");
#ifdef GHC_OS_WINDOWS
        CHECK(os.str() == "\"\\\\root\\\\foo\\\"bar\"");
#else
        CHECK(os.str() == "\"/root/foo\\\"bar\"");
#endif
    }

    {
        std::istringstream is("\"/root/foo bar\"");
        fs::path p;
        is >> p;
        CHECK(p == fs::path("/root/foo bar"));
        CHECK((is.flags() & std::ios_base::skipws) == std::ios_base::skipws);
    }
    {
        std::istringstream is("\"/root/foo bar\"");
        is >> std::noskipws;
        fs::path p;
        is >> p;
        CHECK(p == fs::path("/root/foo bar"));
        CHECK((is.flags() & std::ios_base::skipws) != std::ios_base::skipws);
    }
    {
        std::istringstream is("\"/root/foo\\\"bar\"");
        fs::path p;
        is >> p;
        CHECK(p == fs::path("/root/foo\"bar"));
    }
    {
        std::istringstream is("/root/foo");
        fs::path p;
        is >> p;
        CHECK(p == fs::path("/root/foo"));
    }
}

TEST_CASE("30.10.8.6.2 path factory functions", "[filesystem][path][fs.path.factory]")
{
    CHECK(fs::u8path("foo/bar") == fs::path("foo/bar"));
    CHECK(fs::u8path("foo/bar") == fs::path("foo/bar"));
    std::string str("/foo/bar/test.txt");
    CHECK(fs::u8path(str.begin(), str.end()) == str);
}

TEST_CASE("30.10.9 class filesystem_error", "[filesystem][filesystem_error][fs.class.filesystem_error]")
{
    std::error_code ec(1, std::system_category());
    fs::filesystem_error fse("None", std::error_code());
    fse = fs::filesystem_error("Some error", ec);
    CHECK(fse.code().value() == 1);
    CHECK(!std::string(fse.what()).empty());
    CHECK(fse.path1().empty());
    CHECK(fse.path2().empty());
    fse = fs::filesystem_error("Some error", fs::path("foo/bar"), ec);
    CHECK(!std::string(fse.what()).empty());
    CHECK(fse.path1() == "foo/bar");
    CHECK(fse.path2().empty());
    fse = fs::filesystem_error("Some error", fs::path("foo/bar"), fs::path("some/other"), ec);
    CHECK(!std::string(fse.what()).empty());
    CHECK(fse.path1() == "foo/bar");
    CHECK(fse.path2() == "some/other");
}

constexpr fs::perms constExprOwnerAll()
{
    return fs::perms::owner_read | fs::perms::owner_write | fs::perms::owner_exec;
}

TEST_CASE("30.10.10.4 enum class perms", "[filesystem][enum][fs.enum]")
{
    static_assert(constExprOwnerAll() == fs::perms::owner_all, "constexpr didn't result in owner_all");
    CHECK((fs::perms::owner_read | fs::perms::owner_write | fs::perms::owner_exec) == fs::perms::owner_all);
    CHECK((fs::perms::group_read | fs::perms::group_write | fs::perms::group_exec) == fs::perms::group_all);
    CHECK((fs::perms::others_read | fs::perms::others_write | fs::perms::others_exec) == fs::perms::others_all);
    CHECK((fs::perms::owner_all | fs::perms::group_all | fs::perms::others_all) == fs::perms::all);
    CHECK((fs::perms::all | fs::perms::set_uid | fs::perms::set_gid | fs::perms::sticky_bit) == fs::perms::mask);
}

TEST_CASE("30.10.11 class file_status", "[filesystem][file_status][fs.class.file_status]")
{
    {
        fs::file_status fs;
        CHECK(fs.type() == fs::file_type::none);
        CHECK(fs.permissions() == fs::perms::unknown);
    }
    {
        fs::file_status fs{fs::file_type::regular};
        CHECK(fs.type() == fs::file_type::regular);
        CHECK(fs.permissions() == fs::perms::unknown);
    }
    {
        fs::file_status fs{fs::file_type::directory, fs::perms::owner_read | fs::perms::owner_write | fs::perms::owner_exec};
        CHECK(fs.type() == fs::file_type::directory);
        CHECK(fs.permissions() == fs::perms::owner_all);
        fs.type(fs::file_type::block);
        CHECK(fs.type() == fs::file_type::block);
        fs.type(fs::file_type::character);
        CHECK(fs.type() == fs::file_type::character);
        fs.type(fs::file_type::fifo);
        CHECK(fs.type() == fs::file_type::fifo);
        fs.type(fs::file_type::symlink);
        CHECK(fs.type() == fs::file_type::symlink);
        fs.type(fs::file_type::socket);
        CHECK(fs.type() == fs::file_type::socket);
        fs.permissions(fs.permissions() | fs::perms::group_all | fs::perms::others_all);
        CHECK(fs.permissions() == fs::perms::all);
    }
    {
        fs::file_status fst(fs::file_type::regular);
        fs::file_status fs(std::move(fst));
        CHECK(fs.type() == fs::file_type::regular);
        CHECK(fs.permissions() == fs::perms::unknown);
    }
}

TEST_CASE("30.10.12 class directory_entry", "[filesystem][directory_entry][fs.dir.entry]")
{
    TemporaryDirectory t;
    std::error_code ec;
    auto de = fs::directory_entry(t.path());
    CHECK(de.path() == t.path());
    CHECK((fs::path)de == t.path());
    CHECK(de.exists());
    CHECK(!de.is_block_file());
    CHECK(!de.is_character_file());
    CHECK(de.is_directory());
    CHECK(!de.is_fifo());
    CHECK(!de.is_other());
    CHECK(!de.is_regular_file());
    CHECK(!de.is_socket());
    CHECK(!de.is_symlink());
    CHECK(de.status().type() == fs::file_type::directory);
    ec.clear();
    CHECK(de.status(ec).type() == fs::file_type::directory);
    CHECK(!ec);
    CHECK_NOTHROW(de.refresh());
    fs::directory_entry none;
    CHECK_THROWS_AS(none.refresh(), fs::filesystem_error);
    ec.clear();
    CHECK_NOTHROW(none.refresh(ec));
    CHECK(ec);
    CHECK_THROWS_AS(de.assign(""), fs::filesystem_error);
    ec.clear();
    CHECK_NOTHROW(de.assign("", ec));
    CHECK(ec);
    generateFile(t.path() / "foo", 1234);
    auto now = fs::file_time_type::clock::now();
    CHECK_NOTHROW(de.assign(t.path() / "foo"));
    CHECK_NOTHROW(de.assign(t.path() / "foo", ec));
    CHECK(!ec);
    de = fs::directory_entry(t.path() / "foo");
    CHECK(de.path() == t.path() / "foo");
    CHECK(de.exists());
    CHECK(de.exists(ec));
    CHECK(!ec);
    CHECK(!de.is_block_file());
    CHECK(!de.is_block_file(ec));
    CHECK(!ec);
    CHECK(!de.is_character_file());
    CHECK(!de.is_character_file(ec));
    CHECK(!ec);
    CHECK(!de.is_directory());
    CHECK(!de.is_directory(ec));
    CHECK(!ec);
    CHECK(!de.is_fifo());
    CHECK(!de.is_fifo(ec));
    CHECK(!ec);
    CHECK(!de.is_other());
    CHECK(!de.is_other(ec));
    CHECK(!ec);
    CHECK(de.is_regular_file());
    CHECK(de.is_regular_file(ec));
    CHECK(!ec);
    CHECK(!de.is_socket());
    CHECK(!de.is_socket(ec));
    CHECK(!ec);
    CHECK(!de.is_symlink());
    CHECK(!de.is_symlink(ec));
    CHECK(!ec);
    CHECK(de.file_size() == 1234);
    CHECK(de.file_size(ec) == 1234);
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(de.last_write_time() - now).count()) < 3);
    ec.clear();
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(de.last_write_time(ec) - now).count()) < 3);
    CHECK(!ec);
#ifndef GHC_OS_WEB
    CHECK(de.hard_link_count() == 1);
    CHECK(de.hard_link_count(ec) == 1);
    CHECK(!ec);
#endif
    CHECK_THROWS_AS(de.replace_filename("bar"), fs::filesystem_error);
    CHECK_NOTHROW(de.replace_filename("foo"));
    ec.clear();
    CHECK_NOTHROW(de.replace_filename("bar", ec));
    CHECK(ec);
    auto de2none = fs::directory_entry();
    ec.clear();
#ifndef GHC_OS_WEB
    CHECK(de2none.hard_link_count(ec) == static_cast<uintmax_t>(-1));
    CHECK_THROWS_AS(de2none.hard_link_count(), fs::filesystem_error);
    CHECK(ec);
#endif
    ec.clear();
    CHECK_NOTHROW(de2none.last_write_time(ec));
    CHECK_THROWS_AS(de2none.last_write_time(), fs::filesystem_error);
    CHECK(ec);
    ec.clear();
    CHECK_THROWS_AS(de2none.file_size(), fs::filesystem_error);
    CHECK(de2none.file_size(ec) == static_cast<uintmax_t>(-1));
    CHECK(ec);
    ec.clear();
    CHECK(de2none.status().type() == fs::file_type::not_found);
    CHECK(de2none.status(ec).type() == fs::file_type::not_found);
    CHECK(ec);
    generateFile(t.path() / "a");
    generateFile(t.path() / "b");
    auto d1 = fs::directory_entry(t.path() / "a");
    auto d2 = fs::directory_entry(t.path() / "b");
    CHECK(d1 < d2);
    CHECK(!(d2 < d1));
    CHECK(d1 <= d2);
    CHECK(!(d2 <= d1));
    CHECK(d2 > d1);
    CHECK(!(d1 > d2));
    CHECK(d2 >= d1);
    CHECK(!(d1 >= d2));
    CHECK(d1 != d2);
    CHECK(!(d2 != d2));
    CHECK(d1 == d1);
    CHECK(!(d1 == d2));
}

TEST_CASE("30.10.13 class directory_iterator", "[filesystem][directory_iterator][fs.class.directory_iterator]")
{
    {
        TemporaryDirectory t;
        CHECK(fs::directory_iterator(t.path()) == fs::directory_iterator());
        generateFile(t.path() / "test", 1234);
        REQUIRE(fs::directory_iterator(t.path()) != fs::directory_iterator());
        auto iter = fs::directory_iterator(t.path());
        fs::directory_iterator iter2(iter);
        fs::directory_iterator iter3, iter4;
        iter3 = iter;
        CHECK(iter->path().filename() == "test");
        CHECK(iter2->path().filename() == "test");
        CHECK(iter3->path().filename() == "test");
        iter4 = std::move(iter3);
        CHECK(iter4->path().filename() == "test");
        CHECK(iter->path() == t.path() / "test");
        CHECK(!iter->is_symlink());
        CHECK(iter->is_regular_file());
        CHECK(!iter->is_directory());
        CHECK(iter->file_size() == 1234);
        CHECK(++iter == fs::directory_iterator());
        CHECK_THROWS_AS(fs::directory_iterator(t.path() / "non-existing"), fs::filesystem_error);
        int cnt = 0;
        for(auto de : fs::directory_iterator(t.path())) {
            ++cnt;
        }
        CHECK(cnt == 1);
    }
    if (is_symlink_creation_supported()) {
        TemporaryDirectory t;
        fs::path td = t.path() / "testdir";
        CHECK(fs::directory_iterator(t.path()) == fs::directory_iterator());
        generateFile(t.path() / "test", 1234);
        fs::create_directory(td);
        REQUIRE_NOTHROW(fs::create_symlink(t.path() / "test", td / "testlink"));
        std::error_code ec;
        REQUIRE(fs::directory_iterator(td) != fs::directory_iterator());
        auto iter = fs::directory_iterator(td);
        CHECK(iter->path().filename() == "testlink");
        CHECK(iter->path() == td / "testlink");
        CHECK(iter->is_symlink());
        CHECK(iter->is_regular_file());
        CHECK(!iter->is_directory());
        CHECK(iter->file_size() == 1234);
        CHECK(++iter == fs::directory_iterator());
    }
    {
        // Issue #8: check if resources are freed when iterator reaches end()
        TemporaryDirectory t(TempOpt::change_path);
        auto p = fs::path("test/");
        fs::create_directory(p);
        auto iter = fs::directory_iterator(p);
        while (iter != fs::directory_iterator()) {
            ++iter;
        }
        CHECK(fs::remove_all(p) == 1);
        CHECK_NOTHROW(fs::create_directory(p));
    }
}

TEST_CASE("30.10.14 class recursive_directory_iterator", "[filesystem][recursive_directory_iterator][fs.class.rec.dir.itr]")
{
    {
        auto iter = fs::recursive_directory_iterator(".");
        iter.pop();
        CHECK(iter == fs::recursive_directory_iterator());
    }
    {
        TemporaryDirectory t;
        CHECK(fs::recursive_directory_iterator(t.path()) == fs::recursive_directory_iterator());
        generateFile(t.path() / "test", 1234);
        REQUIRE(fs::recursive_directory_iterator(t.path()) != fs::recursive_directory_iterator());
        auto iter = fs::recursive_directory_iterator(t.path());
        CHECK(iter->path().filename() == "test");
        CHECK(iter->path() == t.path() / "test");
        CHECK(!iter->is_symlink());
        CHECK(iter->is_regular_file());
        CHECK(!iter->is_directory());
        CHECK(iter->file_size() == 1234);
        CHECK(++iter == fs::recursive_directory_iterator());
    }

    {
        TemporaryDirectory t;
        fs::path td = t.path() / "testdir";
        fs::create_directories(td);
        generateFile(td / "test", 1234);
        REQUIRE(fs::recursive_directory_iterator(t.path()) != fs::recursive_directory_iterator());
        auto iter = fs::recursive_directory_iterator(t.path());

        CHECK(iter->path().filename() == "testdir");
        CHECK(iter->path() == td);
        CHECK(!iter->is_symlink());
        CHECK(!iter->is_regular_file());
        CHECK(iter->is_directory());

        CHECK(++iter != fs::recursive_directory_iterator());

        CHECK(iter->path().filename() == "test");
        CHECK(iter->path() == td / "test");
        CHECK(!iter->is_symlink());
        CHECK(iter->is_regular_file());
        CHECK(!iter->is_directory());
        CHECK(iter->file_size() == 1234);

        CHECK(++iter == fs::recursive_directory_iterator());
    }
    {
        TemporaryDirectory t;
        std::error_code ec;
        CHECK(fs::recursive_directory_iterator(t.path(), fs::directory_options::none) == fs::recursive_directory_iterator());
        CHECK(fs::recursive_directory_iterator(t.path(), fs::directory_options::none, ec) == fs::recursive_directory_iterator());
        CHECK(!ec);
        CHECK(fs::recursive_directory_iterator(t.path(), ec) == fs::recursive_directory_iterator());
        CHECK(!ec);
        generateFile(t.path() / "test");
        fs::recursive_directory_iterator rd1(t.path());
        CHECK(fs::recursive_directory_iterator(rd1) != fs::recursive_directory_iterator());
        fs::recursive_directory_iterator rd2(t.path());
        CHECK(fs::recursive_directory_iterator(std::move(rd2)) != fs::recursive_directory_iterator());
        fs::recursive_directory_iterator rd3(t.path(), fs::directory_options::skip_permission_denied);
        CHECK(rd3.options() == fs::directory_options::skip_permission_denied);
        fs::recursive_directory_iterator rd4;
        rd4 = std::move(rd3);
        CHECK(rd4 != fs::recursive_directory_iterator());
        CHECK_NOTHROW(++rd4);
        CHECK(rd4 == fs::recursive_directory_iterator());
        fs::recursive_directory_iterator rd5;
        rd5 = rd4;
    }
    {
        TemporaryDirectory t(TempOpt::change_path);
        generateFile("a");
        fs::create_directory("d1");
        fs::create_directory("d1/d2");
        generateFile("d1/b");
        generateFile("d1/c");
        generateFile("d1/d2/d");
        generateFile("e");
        auto iter = fs::recursive_directory_iterator(".");
        std::multimap<std::string, int> result;
        while(iter != fs::recursive_directory_iterator()) {
            result.insert(std::make_pair(iter->path().generic_string(), iter.depth()));
            ++iter;
        }
        std::stringstream os;
        for(auto p : result) {
            os << "[" << p.first << "," << p.second << "],";
        }
        CHECK(os.str() == "[./a,0],[./d1,0],[./d1/b,1],[./d1/c,1],[./d1/d2,1],[./d1/d2/d,2],[./e,0],");
    }
    {
        TemporaryDirectory t(TempOpt::change_path);
        generateFile("a");
        fs::create_directory("d1");
        fs::create_directory("d1/d2");
        generateFile("d1/b");
        generateFile("d1/c");
        generateFile("d1/d2/d");
        generateFile("e");
        std::multiset<std::string> result;
        for(auto de : fs::recursive_directory_iterator(".")) {
            result.insert(de.path().generic_string());
        }
        std::stringstream os;
        for(auto p : result) {
            os << p << ",";
        }
        CHECK(os.str() == "./a,./d1,./d1/b,./d1/c,./d1/d2,./d1/d2/d,./e,");
    }
    {
        TemporaryDirectory t(TempOpt::change_path);
        generateFile("a");
        fs::create_directory("d1");
        fs::create_directory("d1/d2");
        generateFile("d1/d2/b");
        generateFile("e");
        auto iter = fs::recursive_directory_iterator(".");
        std::multimap<std::string, int> result;
        while(iter != fs::recursive_directory_iterator()) {
            result.insert(std::make_pair(iter->path().generic_string(), iter.depth()));
            if(iter->path() == "./d1/d2") {
                iter.disable_recursion_pending();
            }
            ++iter;
        }
        std::stringstream os;
        for(auto p : result) {
            os << "[" << p.first << "," << p.second << "],";
        }
        CHECK(os.str() == "[./a,0],[./d1,0],[./d1/d2,1],[./e,0],");
    }
    {
        TemporaryDirectory t(TempOpt::change_path);
        generateFile("a");
        fs::create_directory("d1");
        fs::create_directory("d1/d2");
        generateFile("d1/d2/b");
        generateFile("e");
        auto iter = fs::recursive_directory_iterator(".");
        std::multimap<std::string, int> result;
        while(iter != fs::recursive_directory_iterator()) {
            result.insert(std::make_pair(iter->path().generic_string(), iter.depth()));
            if(iter->path() == "./d1/d2") {
                iter.pop();
            }
            else {
                ++iter;
            }
        }
        std::stringstream os;
        for(auto p : result) {
            os << "[" << p.first << "," << p.second << "],";
        }
        CHECK(os.str() == "[./a,0],[./d1,0],[./d1/d2,1],[./e,0],");
    }
}

TEST_CASE("30.10.15.1 absolute", "[filesystem][operations][fs.op.absolute]")
{
    CHECK(fs::absolute("") == fs::current_path() / "");
    CHECK(fs::absolute(fs::current_path()) == fs::current_path());
    CHECK(fs::absolute(".") == fs::current_path() / ".");
    CHECK((fs::absolute("..") == fs::current_path().parent_path() || fs::absolute("..") == fs::current_path() / ".."));
    CHECK(fs::absolute("foo") == fs::current_path() / "foo");
    std::error_code ec;
    CHECK(fs::absolute("", ec) == fs::current_path() / "");
    CHECK(!ec);
    CHECK(fs::absolute("foo", ec) == fs::current_path() / "foo");
    CHECK(!ec);
}

TEST_CASE("30.10.15.2 canonical", "[filesystem][operations][fs.op.canonical]")
{
    CHECK_THROWS_AS(fs::canonical(""), fs::filesystem_error);
    {
        std::error_code ec;
        CHECK(fs::canonical("", ec) == "");
        CHECK(ec);
    }
    CHECK(fs::canonical(fs::current_path()) == fs::current_path());

    CHECK(fs::canonical(".") == fs::current_path());
    CHECK(fs::canonical("..") == fs::current_path().parent_path());
    CHECK(fs::canonical("/") == fs::current_path().root_path());
    CHECK_THROWS_AS(fs::canonical("foo"), fs::filesystem_error);
    {
        std::error_code ec;
        CHECK_NOTHROW(fs::canonical("foo", ec));
        CHECK(ec);
    }
    {
        TemporaryDirectory t(TempOpt::change_path);
        auto dir = t.path() / "d0";
        fs::create_directories(dir / "d1");
        generateFile(dir / "f0");
        fs::path rel(dir.filename());
        CHECK(fs::canonical(dir) == dir);
        CHECK(fs::canonical(rel) == dir);
        CHECK(fs::canonical(dir / "f0") == dir / "f0");
        CHECK(fs::canonical(rel / "f0") == dir / "f0");
        CHECK(fs::canonical(rel / "./f0") == dir / "f0");
        CHECK(fs::canonical(rel / "d1/../f0") == dir / "f0");
    }

    if (is_symlink_creation_supported()) {
        TemporaryDirectory t(TempOpt::change_path);
        fs::create_directory(t.path() / "dir1");
        generateFile(t.path() / "dir1/test1");
        fs::create_directory(t.path() / "dir2");
        fs::create_directory_symlink(t.path() / "dir1", t.path() / "dir2/dirSym");
        CHECK(fs::canonical(t.path() / "dir2/dirSym/test1") == t.path() / "dir1/test1");
    }
}

TEST_CASE("30.10.15.3 copy", "[filesystem][operations][fs.op.copy]")
{
    {
        TemporaryDirectory t(TempOpt::change_path);
        std::error_code ec;
        fs::create_directory("dir1");
        generateFile("dir1/file1");
        generateFile("dir1/file2");
        fs::create_directory("dir1/dir2");
        generateFile("dir1/dir2/file3");
        CHECK_NOTHROW(fs::copy("dir1", "dir3"));
        CHECK(fs::exists("dir3/file1"));
        CHECK(fs::exists("dir3/file2"));
        CHECK(!fs::exists("dir3/dir2"));
        CHECK_NOTHROW(fs::copy("dir1", "dir4", fs::copy_options::recursive, ec));
        CHECK(!ec);
        CHECK(fs::exists("dir4/file1"));
        CHECK(fs::exists("dir4/file2"));
        CHECK(fs::exists("dir4/dir2/file3"));
        fs::create_directory("dir5");
        generateFile("dir5/file1");
        CHECK_THROWS_AS(fs::copy("dir1/file1", "dir5/file1"), fs::filesystem_error);
        CHECK_NOTHROW(fs::copy("dir1/file1", "dir5/file1", fs::copy_options::skip_existing));
    }
    if (is_symlink_creation_supported()) {
        TemporaryDirectory t(TempOpt::change_path);
        std::error_code ec;
        fs::create_directory("dir1");
        generateFile("dir1/file1");
        generateFile("dir1/file2");
        fs::create_directory("dir1/dir2");
        generateFile("dir1/dir2/file3");
#ifdef TEST_LWG_2682_BEHAVIOUR
        REQUIRE_THROWS_AS(fs::copy("dir1", "dir3", fs::copy_options::create_symlinks | fs::copy_options::recursive), fs::filesystem_error);
#else
        REQUIRE_NOTHROW(fs::copy("dir1", "dir3", fs::copy_options::create_symlinks | fs::copy_options::recursive));
        CHECK(!ec);
        CHECK(fs::exists("dir3/file1"));
        CHECK(fs::is_symlink("dir3/file1"));
        CHECK(fs::exists("dir3/file2"));
        CHECK(fs::is_symlink("dir3/file2"));
        CHECK(fs::exists("dir3/dir2/file3"));
        CHECK(fs::is_symlink("dir3/dir2/file3"));
#endif
    }
#ifndef GHC_OS_WEB
    {
        TemporaryDirectory t(TempOpt::change_path);
        std::error_code ec;
        fs::create_directory("dir1");
        generateFile("dir1/file1");
        generateFile("dir1/file2");
        fs::create_directory("dir1/dir2");
        generateFile("dir1/dir2/file3");
        auto f1hl = fs::hard_link_count("dir1/file1");
        auto f2hl = fs::hard_link_count("dir1/file2");
        auto f3hl = fs::hard_link_count("dir1/dir2/file3");
        CHECK_NOTHROW(fs::copy("dir1", "dir3", fs::copy_options::create_hard_links | fs::copy_options::recursive, ec));
        REQUIRE(!ec);
        CHECK(fs::exists("dir3/file1"));
        CHECK(fs::hard_link_count("dir1/file1") == f1hl + 1);
        CHECK(fs::exists("dir3/file2"));
        CHECK(fs::hard_link_count("dir1/file2") == f2hl + 1);
        CHECK(fs::exists("dir3/dir2/file3"));
        CHECK(fs::hard_link_count("dir1/dir2/file3") == f3hl + 1);
    }
#endif
}

TEST_CASE("30.10.15.4 copy_file", "[filesystem][operations][fs.op.copy_file]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 100);
    CHECK(!fs::exists("bar"));
    CHECK(fs::copy_file("foo", "bar"));
    CHECK(fs::exists("bar"));
    CHECK(fs::file_size("foo") == fs::file_size("bar"));
    CHECK(fs::copy_file("foo", "bar2", ec));
    CHECK(!ec);
    std::this_thread::sleep_for(std::chrono::seconds(1));
    generateFile("foo2", 200);
    CHECK(fs::copy_file("foo2", "bar", fs::copy_options::update_existing));
    CHECK(fs::file_size("bar") == 200);
    CHECK(!fs::copy_file("foo", "bar", fs::copy_options::update_existing));
    CHECK(fs::file_size("bar") == 200);
    CHECK(fs::copy_file("foo", "bar", fs::copy_options::overwrite_existing));
    CHECK(fs::file_size("bar") == 100);
    CHECK_THROWS_AS(fs::copy_file("foobar", "foobar2"), fs::filesystem_error);
    CHECK_NOTHROW(fs::copy_file("foobar", "foobar2", ec));
    CHECK(ec);
    CHECK(!fs::exists("foobar"));
}

TEST_CASE("30.10.15.5 copy_symlink", "[filesystem][operations][fs.op.copy_symlink]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo");
    fs::create_directory("dir");
    if (is_symlink_creation_supported()) {
        fs::create_symlink("foo", "sfoo");
        fs::create_directory_symlink("dir", "sdir");
        CHECK_NOTHROW(fs::copy_symlink("sfoo", "sfooc"));
        CHECK(fs::exists("sfooc"));
        CHECK_NOTHROW(fs::copy_symlink("sfoo", "sfooc2", ec));
        CHECK(fs::exists("sfooc2"));
        CHECK(!ec);
        CHECK_NOTHROW(fs::copy_symlink("sdir", "sdirc"));
        CHECK(fs::exists("sdirc"));
        CHECK_NOTHROW(fs::copy_symlink("sdir", "sdirc2", ec));
        CHECK(fs::exists("sdirc2"));
        CHECK(!ec);
    }
    CHECK_THROWS_AS(fs::copy_symlink("bar", "barc"), fs::filesystem_error);
    CHECK_NOTHROW(fs::copy_symlink("bar", "barc", ec));
    CHECK(ec);
}

TEST_CASE("30.10.15.6 create_directories", "[filesystem][operations][fs.op.create_directories]")
{
    TemporaryDirectory t;
    fs::path p = t.path() / "testdir";
    fs::path p2 = p / "nested";
    REQUIRE(!fs::exists(p));
    REQUIRE(!fs::exists(p2));
    CHECK(fs::create_directories(p2));
    CHECK(fs::is_directory(p));
    CHECK(fs::is_directory(p2));
    CHECK(!fs::create_directories(p2));
#ifdef TEST_LWG_2935_BEHAVIOUR
    INFO("This test expects LWG #2935 result conformance.");
    p = t.path() / "testfile";
    generateFile(p);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    bool created = false;
    CHECK_NOTHROW((created = fs::create_directories(p)));
    CHECK(!created);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    std::error_code ec;
    CHECK_NOTHROW((created = fs::create_directories(p, ec)));
    CHECK(!created);
    CHECK(!ec);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    CHECK(!fs::create_directories(p, ec));
#else
    INFO("This test expects conformance with P1164R1. (implemented by GCC with issue #86910.)");
    p = t.path() / "testfile";
    generateFile(p);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    CHECK_THROWS_AS(fs::create_directories(p), fs::filesystem_error);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    std::error_code ec;
    CHECK_NOTHROW(fs::create_directories(p, ec));
    CHECK(ec);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    CHECK(!fs::create_directories(p, ec));
#endif
}

TEST_CASE("30.10.15.7 create_directory", "[filesystem][operations][fs.op.create_directory]")
{
    TemporaryDirectory t;
    fs::path p = t.path() / "testdir";
    REQUIRE(!fs::exists(p));
    CHECK(fs::create_directory(p));
    CHECK(fs::is_directory(p));
    CHECK(!fs::is_regular_file(p));
    CHECK(fs::create_directory(p / "nested", p));
    CHECK(fs::is_directory(p / "nested"));
    CHECK(!fs::is_regular_file(p / "nested"));
#ifdef TEST_LWG_2935_BEHAVIOUR
    INFO("This test expects LWG #2935 result conformance.");
    p = t.path() / "testfile";
    generateFile(p);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    bool created = false;
    CHECK_NOTHROW((created = fs::create_directory(p)));
    CHECK(!created);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    std::error_code ec;
    CHECK_NOTHROW((created = fs::create_directory(p, ec)));
    CHECK(!created);
    CHECK(!ec);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    CHECK(!fs::create_directories(p, ec));
#else
    INFO("This test expects conformance with P1164R1. (implemented by GCC with issue #86910.)");
    p = t.path() / "testfile";
    generateFile(p);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    REQUIRE_THROWS_AS(fs::create_directory(p), fs::filesystem_error);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    std::error_code ec;
    REQUIRE_NOTHROW(fs::create_directory(p, ec));
    CHECK(ec);
    CHECK(fs::is_regular_file(p));
    CHECK(!fs::is_directory(p));
    CHECK(!fs::create_directory(p, ec));
#endif
}

TEST_CASE("30.10.15.8 create_directory_symlink", "[filesystem][operations][fs.op.create_directory_symlink]")
{
    if (is_symlink_creation_supported()) {
        TemporaryDirectory t;
        fs::create_directory(t.path() / "dir1");
        generateFile(t.path() / "dir1/test1");
        fs::create_directory(t.path() / "dir2");
        fs::create_directory_symlink(t.path() / "dir1", t.path() / "dir2/dirSym");
        CHECK(fs::exists(t.path() / "dir2/dirSym"));
        CHECK(fs::is_symlink(t.path() / "dir2/dirSym"));
        CHECK(fs::exists(t.path() / "dir2/dirSym/test1"));
        CHECK(fs::is_regular_file(t.path() / "dir2/dirSym/test1"));
        CHECK_THROWS_AS(fs::create_directory_symlink(t.path() / "dir1", t.path() / "dir2/dirSym"), fs::filesystem_error);
        std::error_code ec;
        CHECK_NOTHROW(fs::create_directory_symlink(t.path() / "dir1", t.path() / "dir2/dirSym", ec));
        CHECK(ec);
    }
}

TEST_CASE("30.10.15.9 create_hard_link", "[filesystem][operations][fs.op.create_hard_link]")
{
#ifndef GHC_OS_WEB
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 1234);
    CHECK_NOTHROW(fs::create_hard_link("foo", "bar"));
    CHECK(fs::exists("bar"));
    CHECK(!fs::is_symlink("bar"));
    CHECK_NOTHROW(fs::create_hard_link("foo", "bar2", ec));
    CHECK(fs::exists("bar2"));
    CHECK(!fs::is_symlink("bar2"));
    CHECK(!ec);
    CHECK_THROWS_AS(fs::create_hard_link("nofoo", "bar"), fs::filesystem_error);
    CHECK_NOTHROW(fs::create_hard_link("nofoo", "bar", ec));
    CHECK(ec);
#endif
}

TEST_CASE("30.10.15.10 create_symlink", "[filesystem][operations][fs.op.create_symlink]")
{
    if (is_symlink_creation_supported()) {
        TemporaryDirectory t;
        fs::create_directory(t.path() / "dir1");
        generateFile(t.path() / "dir1/test1");
        fs::create_directory(t.path() / "dir2");
        fs::create_symlink(t.path() / "dir1/test1", t.path() / "dir2/fileSym");
        CHECK(fs::exists(t.path() / "dir2/fileSym"));
        CHECK(fs::is_symlink(t.path() / "dir2/fileSym"));
        CHECK(fs::exists(t.path() / "dir2/fileSym"));
        CHECK(fs::is_regular_file(t.path() / "dir2/fileSym"));
        CHECK_THROWS_AS(fs::create_symlink(t.path() / "dir1", t.path() / "dir2/fileSym"), fs::filesystem_error);
        std::error_code ec;
        CHECK_NOTHROW(fs::create_symlink(t.path() / "dir1", t.path() / "dir2/fileSym", ec));
        CHECK(ec);
    }
}

TEST_CASE("30.10.15.11 current_path", "[filesystem][operations][fs.op.current_path]")
{
    TemporaryDirectory t;
    std::error_code ec;
    fs::path p1 = fs::current_path();
    CHECK_NOTHROW(fs::current_path(t.path()));
    CHECK(p1 != fs::current_path());
    CHECK_NOTHROW(fs::current_path(p1, ec));
    CHECK(!ec);
    CHECK_THROWS_AS(fs::current_path(t.path() / "foo"), fs::filesystem_error);
    CHECK(p1 == fs::current_path());
    CHECK_NOTHROW(fs::current_path(t.path() / "foo", ec));
    CHECK(ec);
}

TEST_CASE("30.10.15.12 equivalent", "[filesystem][operations][fs.op.equivalent]")
{
    TemporaryDirectory t(TempOpt::change_path);
    generateFile("foo", 1234);
    CHECK(fs::equivalent(t.path() / "foo", "foo"));
    if (is_symlink_creation_supported()) {
        std::error_code ec(42, std::system_category());
        fs::create_symlink("foo", "foo2");
        CHECK(fs::equivalent("foo", "foo2"));
        CHECK(fs::equivalent("foo", "foo2", ec));
        CHECK(!ec);
    }
#ifdef TEST_LWG_2937_BEHAVIOUR
    INFO("This test expects LWG #2937 result conformance.");
    std::error_code ec;
    bool result = false;
    REQUIRE_THROWS_AS(fs::equivalent("foo", "foo3"), fs::filesystem_error);
    CHECK_NOTHROW(result = fs::equivalent("foo", "foo3", ec));
    CHECK(!result);
    CHECK(ec);
    ec.clear();
    CHECK_THROWS_AS(fs::equivalent("foo3", "foo"), fs::filesystem_error);
    CHECK_NOTHROW(result = fs::equivalent("foo3", "foo", ec));
    CHECK(!result);
    CHECK(ec);
    ec.clear();
    CHECK_THROWS_AS(fs::equivalent("foo3", "foo4"), fs::filesystem_error);
    CHECK_NOTHROW(result = fs::equivalent("foo3", "foo4", ec));
    CHECK(!result);
    CHECK(ec);
#else
    INFO("This test expects conformance predating LWG #2937 result.");
    std::error_code ec;
    bool result = false;
    REQUIRE_NOTHROW(result = fs::equivalent("foo", "foo3"));
    CHECK(!result);
    CHECK_NOTHROW(result = fs::equivalent("foo", "foo3", ec));
    CHECK(!result);
    CHECK(!ec);
    ec.clear();
    CHECK_NOTHROW(result = fs::equivalent("foo3", "foo"));
    CHECK(!result);
    CHECK_NOTHROW(result = fs::equivalent("foo3", "foo", ec));
    CHECK(!result);
    CHECK(!ec);
    ec.clear();
    CHECK_THROWS_AS(result = fs::equivalent("foo4", "foo3"), fs::filesystem_error);
    CHECK(!result);
    CHECK_NOTHROW(result = fs::equivalent("foo4", "foo3", ec));
    CHECK(!result);
    CHECK(ec);
#endif
}

TEST_CASE("30.10.15.13 exists", "[filesystem][operations][fs.op.exists]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    CHECK(!fs::exists(""));
    CHECK(!fs::exists("foo"));
    CHECK(!fs::exists("foo", ec));
    CHECK(!ec);
    ec = std::error_code(42, std::system_category());
    CHECK(!fs::exists("foo", ec));
    CHECK(!ec);
    ec.clear();
    CHECK(fs::exists(t.path()));
    CHECK(fs::exists(t.path(), ec));
    CHECK(!ec);
    ec = std::error_code(42, std::system_category());
    CHECK(fs::exists(t.path(), ec));
    CHECK(!ec);
#if defined(GHC_OS_WINDOWS) && !defined(GHC_FILESYSTEM_FWD)
    if (::GetFileAttributesW(L"C:\\fs-test") != INVALID_FILE_ATTRIBUTES) {
        CHECK(fs::exists("C:\\fs-test"));    
    }
#endif
}

TEST_CASE("30.10.15.14 file_size", "[filesystem][operations][fs.op.file_size]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 0);
    generateFile("bar", 1234);
    CHECK(fs::file_size("foo") == 0);
    ec = std::error_code(42, std::system_category());
    CHECK(fs::file_size("foo", ec) == 0);
    CHECK(!ec);
    ec.clear();
    CHECK(fs::file_size("bar") == 1234);
    ec = std::error_code(42, std::system_category());
    CHECK(fs::file_size("bar", ec) == 1234);
    CHECK(!ec);
    ec.clear();
    CHECK_THROWS_AS(fs::file_size("foobar"), fs::filesystem_error);
    CHECK(fs::file_size("foobar", ec) == static_cast<uintmax_t>(-1));
    CHECK(ec);
    ec.clear();
}

TEST_CASE("30.10.15.15 hard_link_count", "[filesystem][operations][fs.op.hard_link_count]")
{
#ifndef GHC_OS_WEB
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
#ifdef GHC_OS_WINDOWS
    // windows doesn't implement "."/".." as hardlinks, so it
    // starts with 1 and subdirectories don't change the count
    CHECK(fs::hard_link_count(t.path()) == 1);
    fs::create_directory("dir");
    CHECK(fs::hard_link_count(t.path()) == 1);
#else
    // unix/bsd/linux typically implements "."/".." as hardlinks
    // so an empty dir has 2 (from parent and the ".") and
    // adding a subdirectory adds one due to its ".."
    CHECK(fs::hard_link_count(t.path()) == 2);
    fs::create_directory("dir");
    CHECK(fs::hard_link_count(t.path()) == 3);
#endif
    generateFile("foo");
    CHECK(fs::hard_link_count(t.path() / "foo") == 1);
    ec = std::error_code(42, std::system_category());
    CHECK(fs::hard_link_count(t.path() / "foo", ec) == 1);
    CHECK(!ec);
    CHECK_THROWS_AS(fs::hard_link_count(t.path() / "bar"), fs::filesystem_error);
    CHECK_NOTHROW(fs::hard_link_count(t.path() / "bar", ec));
    CHECK(ec);
    ec.clear();
#else
    WARN("Test for unsupportet features are disabled on JS/Wasm target.");
#endif
}

class FileTypeMixFixture
{
public:
    FileTypeMixFixture()
        : _t(TempOpt::change_path)
        , _hasFifo(false)
        , _hasSocket(false)
    {
        generateFile("regular");
        fs::create_directory("directory");
        if (is_symlink_creation_supported()) {
            fs::create_symlink("regular", "file_symlink");
            fs::create_directory_symlink("directory", "dir_symlink");
        }
#if !defined(GHC_OS_WINDOWS) && !defined(GHC_OS_WEB)
        REQUIRE(::mkfifo("fifo", 0644) == 0);
        _hasFifo = true;
        struct ::sockaddr_un addr;
        addr.sun_family = AF_UNIX;
        std::strncpy(addr.sun_path, "socket", sizeof(addr.sun_path));
        int fd = socket(PF_UNIX, SOCK_STREAM, 0);
        bind(fd, (struct sockaddr*)&addr, sizeof addr);
        _hasSocket = true;
#endif
    }

    ~FileTypeMixFixture() {}

    bool has_fifo() const { return _hasFifo; }

    bool has_socket() const { return _hasSocket; }

    fs::path block_path() const
    {
        std::error_code ec;
        if (fs::exists("/dev/sda", ec)) {
            return "/dev/sda";
        }
        else if (fs::exists("/dev/disk0", ec)) {
            return "/dev/disk0";
        }
        return fs::path();
    }

    fs::path character_path() const
    {
        std::error_code ec;
        if (fs::exists("/dev/null", ec)) {
            return "/dev/null";
        }
        else if (fs::exists("NUL", ec)) {
            return "NUL";
        }
        return fs::path();
    }
    fs::path temp_path() const { return _t.path(); }

private:
    TemporaryDirectory _t;
    bool _hasFifo;
    bool _hasSocket;
};

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.16 is_block_file", "[filesystem][operations][fs.op.is_block_file]")
{
    std::error_code ec;
    CHECK(!fs::is_block_file("directory"));
    CHECK(!fs::is_block_file("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_block_file("dir_symlink"));
        CHECK(!fs::is_block_file("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_block_file("fifo") : true));
    CHECK((has_socket() ? !fs::is_block_file("socket") : true));
    CHECK((block_path().empty() ? true : fs::is_block_file(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_block_file(character_path())));
    CHECK_NOTHROW(fs::is_block_file("notfound"));
    CHECK_NOTHROW(fs::is_block_file("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::symlink)));
    CHECK(fs::is_block_file(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_block_file(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.17 is_character_file", "[filesystem][operations][fs.op.is_character_file]")
{
    std::error_code ec;
    CHECK(!fs::is_character_file("directory"));
    CHECK(!fs::is_character_file("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_character_file("dir_symlink"));
        CHECK(!fs::is_character_file("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_character_file("fifo") : true));
    CHECK((has_socket() ? !fs::is_character_file("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_character_file(block_path())));
    CHECK((character_path().empty() ? true : fs::is_character_file(character_path())));
    CHECK_NOTHROW(fs::is_character_file("notfound"));
    CHECK_NOTHROW(fs::is_character_file("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::block)));
    CHECK(fs::is_character_file(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_character_file(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.18 is_directory", "[filesystem][operations][fs.op.is_directory]")
{
    std::error_code ec;
    CHECK(fs::is_directory("directory"));
    CHECK(!fs::is_directory("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(fs::is_directory("dir_symlink"));
        CHECK(!fs::is_directory("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_directory("fifo") : true));
    CHECK((has_socket() ? !fs::is_directory("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_directory(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_directory(character_path())));
    CHECK_NOTHROW(fs::is_directory("notfound"));
    CHECK_NOTHROW(fs::is_directory("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::regular)));
    CHECK(fs::is_directory(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_directory(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE("30.10.15.19 is_empty", "[filesystem][operations][fs.op.is_empty]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    CHECK(fs::is_empty(t.path()));
    CHECK(fs::is_empty(t.path(), ec));
    CHECK(!ec);
    generateFile("foo", 0);
    generateFile("bar", 1234);
    CHECK(fs::is_empty("foo"));
    CHECK(fs::is_empty("foo", ec));
    CHECK(!ec);
    CHECK(!fs::is_empty("bar"));
    CHECK(!fs::is_empty("bar", ec));
    CHECK(!ec);
    CHECK_THROWS_AS(fs::is_empty("foobar"), fs::filesystem_error);
    bool result = false;
    CHECK_NOTHROW(result = fs::is_empty("foobar", ec));
    CHECK(!result);
    CHECK(ec);
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.20 is_fifo", "[filesystem][operations][fs.op.is_fifo]")
{
    std::error_code ec;
    CHECK(!fs::is_fifo("directory"));
    CHECK(!fs::is_fifo("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_fifo("dir_symlink"));
        CHECK(!fs::is_fifo("file_symlink"));
    }
    CHECK((has_fifo() ? fs::is_fifo("fifo") : true));
    CHECK((has_socket() ? !fs::is_fifo("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_fifo(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_fifo(character_path())));
    CHECK_NOTHROW(fs::is_fifo("notfound"));
    CHECK_NOTHROW(fs::is_fifo("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::character)));
    CHECK(fs::is_fifo(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_fifo(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.21 is_other", "[filesystem][operations][fs.op.is_other]")
{
    std::error_code ec;
    CHECK(!fs::is_other("directory"));
    CHECK(!fs::is_other("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_other("dir_symlink"));
        CHECK(!fs::is_other("file_symlink"));
    }
    CHECK((has_fifo() ? fs::is_other("fifo") : true));
    CHECK((has_socket() ? fs::is_other("socket") : true));
    CHECK((block_path().empty() ? true : fs::is_other(block_path())));
    CHECK((character_path().empty() ? true : fs::is_other(character_path())));
    CHECK_NOTHROW(fs::is_other("notfound"));
    CHECK_NOTHROW(fs::is_other("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_other(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_other(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_other(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_other(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_other(fs::file_status(fs::file_type::symlink)));
    CHECK(fs::is_other(fs::file_status(fs::file_type::block)));
    CHECK(fs::is_other(fs::file_status(fs::file_type::character)));
    CHECK(fs::is_other(fs::file_status(fs::file_type::fifo)));
    CHECK(fs::is_other(fs::file_status(fs::file_type::socket)));
    CHECK(fs::is_other(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.22 is_regular_file", "[filesystem][operations][fs.op.is_regular_file]")
{
    std::error_code ec;
    CHECK(!fs::is_regular_file("directory"));
    CHECK(fs::is_regular_file("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_regular_file("dir_symlink"));
        CHECK(fs::is_regular_file("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_regular_file("fifo") : true));
    CHECK((has_socket() ? !fs::is_regular_file("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_regular_file(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_regular_file(character_path())));
    CHECK_NOTHROW(fs::is_regular_file("notfound"));
    CHECK_NOTHROW(fs::is_regular_file("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::not_found)));
    CHECK(fs::is_regular_file(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_regular_file(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.23 is_socket", "[filesystem][operations][fs.op.is_socket]")
{
    std::error_code ec;
    CHECK(!fs::is_socket("directory"));
    CHECK(!fs::is_socket("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(!fs::is_socket("dir_symlink"));
        CHECK(!fs::is_socket("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_socket("fifo") : true));
    CHECK((has_socket() ? fs::is_socket("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_socket(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_socket(character_path())));
    CHECK_NOTHROW(fs::is_socket("notfound"));
    CHECK_NOTHROW(fs::is_socket("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::directory)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::fifo)));
    CHECK(fs::is_socket(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_socket(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE_METHOD(FileTypeMixFixture, "30.10.15.24 is_symlink", "[filesystem][operations][fs.op.is_symlink]")
{
    std::error_code ec;
    CHECK(!fs::is_symlink("directory"));
    CHECK(!fs::is_symlink("regular"));
    if (is_symlink_creation_supported()) {
        CHECK(fs::is_symlink("dir_symlink"));
        CHECK(fs::is_symlink("file_symlink"));
    }
    CHECK((has_fifo() ? !fs::is_symlink("fifo") : true));
    CHECK((has_socket() ? !fs::is_symlink("socket") : true));
    CHECK((block_path().empty() ? true : !fs::is_symlink(block_path())));
    CHECK((character_path().empty() ? true : !fs::is_symlink(character_path())));
    CHECK_NOTHROW(fs::is_symlink("notfound"));
    CHECK_NOTHROW(fs::is_symlink("notfound", ec));
    CHECK(ec);
    ec.clear();
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::none)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::not_found)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::regular)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::directory)));
    CHECK(fs::is_symlink(fs::file_status(fs::file_type::symlink)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::block)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::character)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::fifo)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::socket)));
    CHECK(!fs::is_symlink(fs::file_status(fs::file_type::unknown)));
}

#ifndef GHC_OS_WEB
static fs::file_time_type timeFromString(const std::string& str)
{
    struct ::tm tm;
    ::memset(&tm, 0, sizeof(::tm));
    std::istringstream is(str);
    is >> std::get_time(&tm, "%Y-%m-%dT%H:%M:%S");
    if (is.fail()) {
        throw std::exception();
    }
    return from_time_t<fs::file_time_type>(std::mktime(&tm));
}
#endif

TEST_CASE("30.10.15.25 last_write_time", "[filesystem][operations][fs.op.last_write_time]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    fs::file_time_type ft;
    generateFile("foo");
    auto now = fs::file_time_type::clock::now();
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(fs::last_write_time(t.path()) - now).count()) < 3);
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(fs::last_write_time("foo") - now).count()) < 3);
    CHECK_THROWS_AS(fs::last_write_time("bar"), fs::filesystem_error);
    CHECK_NOTHROW(ft = fs::last_write_time("bar", ec));
    CHECK(ft == fs::file_time_type::min());
    CHECK(ec);
    ec.clear();
    if (is_symlink_creation_supported()) {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        fs::create_symlink("foo", "foo2");
        ft = fs::last_write_time("foo");
        // checks that the time of the symlink is fetched
        CHECK(ft == fs::last_write_time("foo2"));
    }
#ifndef GHC_OS_WEB
    auto nt = timeFromString("2015-10-21T04:30:00");
    CHECK_NOTHROW(fs::last_write_time(t.path() / "foo", nt));
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(fs::last_write_time("foo") - nt).count()) < 1);
    nt = timeFromString("2015-10-21T04:29:00");
    CHECK_NOTHROW(fs::last_write_time("foo", nt, ec));
    CHECK(std::abs(std::chrono::duration_cast<std::chrono::seconds>(fs::last_write_time("foo") - nt).count()) < 1);
    CHECK(!ec);
    CHECK_THROWS_AS(fs::last_write_time("bar", nt), fs::filesystem_error);
    CHECK_NOTHROW(fs::last_write_time("bar", nt, ec));
    CHECK(ec);
#endif
}

TEST_CASE("30.10.15.26 permissions", "[filesystem][operations][fs.op.permissions]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 512);
    auto allWrite = fs::perms::owner_write | fs::perms::group_write | fs::perms::others_write;
    CHECK_NOTHROW(fs::permissions("foo", allWrite, fs::perm_options::remove));
    CHECK((fs::status("foo").permissions() & fs::perms::owner_write) != fs::perms::owner_write);
    CHECK_THROWS_AS(fs::resize_file("foo", 1024), fs::filesystem_error);
    CHECK(fs::file_size("foo") == 512);
    CHECK_NOTHROW(fs::permissions("foo", fs::perms::owner_write, fs::perm_options::add));
    CHECK((fs::status("foo").permissions() & fs::perms::owner_write) == fs::perms::owner_write);
    CHECK_NOTHROW(fs::resize_file("foo", 2048));
    CHECK(fs::file_size("foo") == 2048);
    CHECK_THROWS_AS(fs::permissions("bar", fs::perms::owner_write, fs::perm_options::add), fs::filesystem_error);
    CHECK_NOTHROW(fs::permissions("bar", fs::perms::owner_write, fs::perm_options::add, ec));
    CHECK(ec);
    CHECK_THROWS_AS(fs::permissions("bar", fs::perms::owner_write, static_cast<fs::perm_options>(0)), fs::filesystem_error);
}

TEST_CASE("30.10.15.27 proximate", "[filesystem][operations][fs.op.proximate]")
{
    std::error_code ec;
    CHECK(fs::proximate("/a/d", "/a/b/c") == "../../d");
    CHECK(fs::proximate("/a/d", "/a/b/c", ec) == "../../d");
    CHECK(!ec);
    CHECK(fs::proximate("/a/b/c", "/a/d") == "../b/c");
    CHECK(fs::proximate("/a/b/c", "/a/d", ec) == "../b/c");
    CHECK(!ec);
    CHECK(fs::proximate("a/b/c", "a") == "b/c");
    CHECK(fs::proximate("a/b/c", "a", ec) == "b/c");
    CHECK(!ec);
    CHECK(fs::proximate("a/b/c", "a/b/c/x/y") == "../..");
    CHECK(fs::proximate("a/b/c", "a/b/c/x/y", ec) == "../..");
    CHECK(!ec);
    CHECK(fs::proximate("a/b/c", "a/b/c") == ".");
    CHECK(fs::proximate("a/b/c", "a/b/c", ec) == ".");
    CHECK(!ec);
    CHECK(fs::proximate("a/b", "c/d") == "../../a/b");
    CHECK(fs::proximate("a/b", "c/d", ec) == "../../a/b");
    CHECK(!ec);
#ifndef GHC_OS_WINDOWS
    if (has_host_root_name_support()) {
        CHECK(fs::proximate("//host1/a/d", "//host2/a/b/c") == "//host1/a/d");
        CHECK(fs::proximate("//host1/a/d", "//host2/a/b/c", ec) == "//host1/a/d");
        CHECK(!ec);
    }
#endif
}

TEST_CASE("30.10.15.28 read_symlink", "[filesystem][operations][fs.op.read_symlink]")
{
    if (is_symlink_creation_supported()) {
        TemporaryDirectory t(TempOpt::change_path);
        std::error_code ec;
        generateFile("foo");
        fs::create_symlink(t.path() / "foo", "bar");
        CHECK(fs::read_symlink("bar") == t.path() / "foo");
        CHECK(fs::read_symlink("bar", ec) == t.path() / "foo");
        CHECK(!ec);
        CHECK_THROWS_AS(fs::read_symlink("foobar"), fs::filesystem_error);
        CHECK(fs::read_symlink("foobar", ec) == fs::path());
        CHECK(ec);
    }
}

TEST_CASE("30.10.15.29 relative", "[filesystem][operations][fs.op.relative]")
{
    CHECK(fs::relative("/a/d", "/a/b/c") == "../../d");
    CHECK(fs::relative("/a/b/c", "/a/d") == "../b/c");
    CHECK(fs::relative("a/b/c", "a") == "b/c");
    CHECK(fs::relative("a/b/c", "a/b/c/x/y") == "../..");
    CHECK(fs::relative("a/b/c", "a/b/c") == ".");
    CHECK(fs::relative("a/b", "c/d") == "../../a/b");
    std::error_code ec;
    CHECK(fs::relative(fs::current_path() / "foo", ec) == "foo");
    CHECK(!ec);
}

TEST_CASE("30.10.15.30 remove", "[filesystem][operations][fs.op.remove]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo");
    CHECK(fs::remove("foo"));
    CHECK(!fs::exists("foo"));
    CHECK(!fs::remove("foo"));
    generateFile("foo");
    CHECK(fs::remove("foo", ec));
    CHECK(!fs::exists("foo"));
    if (is_symlink_creation_supported()) {
        generateFile("foo");
        fs::create_symlink("foo", "bar");
        CHECK(fs::exists(fs::symlink_status("bar")));
        CHECK(fs::remove("bar", ec));
        CHECK(fs::exists("foo"));
        CHECK(!fs::exists(fs::symlink_status("bar")));
    }
    CHECK(!fs::remove("bar"));
    CHECK(!fs::remove("bar", ec));
    CHECK(!ec);
}

TEST_CASE("30.10.15.31 remove_all", "[filesystem][operations][fs.op.remove_all]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo");
    CHECK(fs::remove_all("foo", ec) == 1);
    CHECK(!ec);
    ec.clear();
    CHECK(fs::directory_iterator(t.path()) == fs::directory_iterator());
    fs::create_directories("dir1/dir1a");
    fs::create_directories("dir1/dir1b");
    generateFile("dir1/dir1a/f1");
    generateFile("dir1/dir1b/f2");
    CHECK_NOTHROW(fs::remove_all("dir1/non-existing", ec));
    CHECK(!ec);
    CHECK(fs::remove_all("dir1/non-existing", ec) == 0);
    CHECK(fs::remove_all("dir1") == 5);
    CHECK(fs::directory_iterator(t.path()) == fs::directory_iterator());
}

TEST_CASE("30.10.15.32 rename", "[filesystem][operations][fs.op.rename]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 123);
    fs::create_directory("dir1");
    CHECK_NOTHROW(fs::rename("foo", "bar"));
    CHECK(!fs::exists("foo"));
    CHECK(fs::exists("bar"));
    CHECK_NOTHROW(fs::rename("dir1", "dir2"));
    CHECK(fs::exists("dir2"));
    generateFile("foo2", 42);
    CHECK_NOTHROW(fs::rename("bar", "foo2"));
    CHECK(fs::exists("foo2"));
    CHECK(fs::file_size("foo2") == 123u);
    CHECK(!fs::exists("bar"));
    CHECK_NOTHROW(fs::rename("foo2", "foo", ec));
    CHECK(!ec);
    CHECK_THROWS_AS(fs::rename("foobar", "barfoo"), fs::filesystem_error);
    CHECK_NOTHROW(fs::rename("foobar", "barfoo", ec));
    CHECK(ec);
    CHECK(!fs::exists("barfoo"));
}

TEST_CASE("30.10.15.33 resize_file", "[filesystem][operations][fs.op.resize_file]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    generateFile("foo", 1024);
    CHECK(fs::file_size("foo") == 1024);
    CHECK_NOTHROW(fs::resize_file("foo", 2048));
    CHECK(fs::file_size("foo") == 2048);
    CHECK_NOTHROW(fs::resize_file("foo", 1000, ec));
    CHECK(!ec);
    CHECK(fs::file_size("foo") == 1000);
    CHECK_THROWS_AS(fs::resize_file("bar", 2048), fs::filesystem_error);
    CHECK(!fs::exists("bar"));
    CHECK_NOTHROW(fs::resize_file("bar", 4096, ec));
    CHECK(ec);
    CHECK(!fs::exists("bar"));
}

TEST_CASE("30.10.15.34 space", "[filesystem][operations][fs.op.space]")
{
    {
        fs::space_info si;
        CHECK_NOTHROW(si = fs::space(fs::current_path()));
        CHECK(si.capacity > 1024 * 1024);
        CHECK(si.capacity > si.free);
        CHECK(si.free >= si.available);
    }
    {
        std::error_code ec;
        fs::space_info si;
        CHECK_NOTHROW(si = fs::space(fs::current_path(), ec));
        CHECK(si.capacity > 1024 * 1024);
        CHECK(si.capacity > si.free);
        CHECK(si.free >= si.available);
        CHECK(!ec);
    }
#ifndef GHC_OS_WEB // statvfs under emscripten always returns a result, so this tests would fail
    {
        std::error_code ec;
        fs::space_info si;
        CHECK_NOTHROW(si = fs::space("foobar42", ec));
        CHECK(si.capacity == static_cast<uintmax_t>(-1));
        CHECK(si.free == static_cast<uintmax_t>(-1));
        CHECK(si.available == static_cast<uintmax_t>(-1));
        CHECK(ec);
    }
    CHECK_THROWS_AS(fs::space("foobar42"), fs::filesystem_error);
#endif
}

TEST_CASE("30.10.15.35 status", "[filesystem][operations][fs.op.status]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    fs::file_status fs;
    CHECK_NOTHROW(fs = fs::status("foo"));
    CHECK(fs.type() == fs::file_type::not_found);
    CHECK(fs.permissions() == fs::perms::unknown);
    CHECK_NOTHROW(fs = fs::status("bar", ec));
    CHECK(fs.type() == fs::file_type::not_found);
    CHECK(fs.permissions() == fs::perms::unknown);
    CHECK(ec);
    ec.clear();
    fs = fs::status(t.path());
    CHECK(fs.type() == fs::file_type::directory);
    CHECK((fs.permissions() & (fs::perms::owner_read | fs::perms::owner_write)) == (fs::perms::owner_read | fs::perms::owner_write));
    generateFile("foobar");
    fs = fs::status(t.path() / "foobar");
    CHECK(fs.type() == fs::file_type::regular);
    CHECK((fs.permissions() & (fs::perms::owner_read | fs::perms::owner_write)) == (fs::perms::owner_read | fs::perms::owner_write));
    if (is_symlink_creation_supported()) {
        fs::create_symlink(t.path() / "foobar", t.path() / "barfoo");
        fs = fs::status(t.path() / "barfoo");
        CHECK(fs.type() == fs::file_type::regular);
        CHECK((fs.permissions() & (fs::perms::owner_read | fs::perms::owner_write)) == (fs::perms::owner_read | fs::perms::owner_write));
    }
}

TEST_CASE("30.10.15.36 status_known", "[filesystem][operations][fs.op.status_known]")
{
    CHECK(!fs::status_known(fs::file_status()));
    CHECK(fs::status_known(fs::file_status(fs::file_type::not_found)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::regular)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::directory)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::symlink)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::character)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::fifo)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::socket)));
    CHECK(fs::status_known(fs::file_status(fs::file_type::unknown)));
}

TEST_CASE("30.10.15.37 symlink_status", "[filesystem][operations][fs.op.symlink_status]")
{
    TemporaryDirectory t(TempOpt::change_path);
    std::error_code ec;
    fs::file_status fs;
    CHECK_NOTHROW(fs = fs::symlink_status("foo"));
    CHECK(fs.type() == fs::file_type::not_found);
    CHECK(fs.permissions() == fs::perms::unknown);
    CHECK_NOTHROW(fs = fs::symlink_status("bar", ec));
    CHECK(fs.type() == fs::file_type::not_found);
    CHECK(fs.permissions() == fs::perms::unknown);
    CHECK(ec);
    ec.clear();
    fs = fs::symlink_status(t.path());
    CHECK(fs.type() == fs::file_type::directory);
    CHECK((fs.permissions() & (fs::perms::owner_read | fs::perms::owner_write)) == (fs::perms::owner_read | fs::perms::owner_write));
    generateFile("foobar");
    fs = fs::symlink_status(t.path() / "foobar");
    CHECK(fs.type() == fs::file_type::regular);
    CHECK((fs.permissions() & (fs::perms::owner_read | fs::perms::owner_write)) == (fs::perms::owner_read | fs::perms::owner_write));
    if (is_symlink_creation_supported()) {
        fs::create_symlink(t.path() / "foobar", t.path() / "barfoo");
        fs = fs::symlink_status(t.path() / "barfoo");
        CHECK(fs.type() == fs::file_type::symlink);
    }
}

TEST_CASE("30.10.15.38 temporary_directory_path", "[filesystem][operations][fs.op.temp_dir_path]")
{
    std::error_code ec;
    CHECK_NOTHROW(fs::exists(fs::temp_directory_path()));
    CHECK_NOTHROW(fs::exists(fs::temp_directory_path(ec)));
    CHECK(!fs::temp_directory_path().empty());
    CHECK(!ec);
}

TEST_CASE("30.10.15.39 weakly_canonical", "[filesystem][operations][fs.op.weakly_canonical]")
{
    INFO("This might fail on std::implementations that return fs::current_path() for fs::canonical(\"\")");
    CHECK(fs::weakly_canonical("") == ".");
    if(fs::weakly_canonical("") == ".") {
        CHECK(fs::weakly_canonical("foo/bar") == "foo/bar");
        CHECK(fs::weakly_canonical("foo/./bar") == "foo/bar");
        CHECK(fs::weakly_canonical("foo/../bar") == "bar");
    }
    else {
        CHECK(fs::weakly_canonical("foo/bar") == fs::current_path() / "foo/bar");
        CHECK(fs::weakly_canonical("foo/./bar") == fs::current_path() / "foo/bar");
        CHECK(fs::weakly_canonical("foo/../bar") == fs::current_path() / "bar");
    }

    {
        TemporaryDirectory t(TempOpt::change_path);
        auto dir = t.path() / "d0";
        fs::create_directories(dir / "d1");
        generateFile(dir / "f0");
        fs::path rel(dir.filename());
        CHECK(fs::weakly_canonical(dir) == dir);
        CHECK(fs::weakly_canonical(rel) == dir);
        CHECK(fs::weakly_canonical(dir / "f0") == dir / "f0");
        CHECK(fs::weakly_canonical(dir / "f0/") == dir / "f0/");
        CHECK(fs::weakly_canonical(dir / "f1") == dir / "f1");
        CHECK(fs::weakly_canonical(rel / "f0") == dir / "f0");
        CHECK(fs::weakly_canonical(rel / "f0/") == dir / "f0/");
        CHECK(fs::weakly_canonical(rel / "f1") == dir / "f1");
        CHECK(fs::weakly_canonical(rel / "./f0") == dir / "f0");
        CHECK(fs::weakly_canonical(rel / "./f1") == dir / "f1");
        CHECK(fs::weakly_canonical(rel / "d1/../f0") == dir / "f0");
        CHECK(fs::weakly_canonical(rel / "d1/../f1") == dir / "f1");
        CHECK(fs::weakly_canonical(rel / "d1/../f1/../f2") == dir / "f2");
    }
}

TEST_CASE("std::string_view support", "[filesystem][fs.string_view]")
{
#if __cpp_lib_string_view
    using namespace std::literals;
    {
        std::string p("foo/bar");
        std::string_view sv(p);
        CHECK(fs::path(sv, fs::path::format::generic_format).generic_string() == "foo/bar");
        fs::path p2("fo");
        p2 += std::string_view("o");
        CHECK(p2 == "foo");
        CHECK(p2.compare(std::string_view("foo")) == 0);
    }
    {
        auto p = fs::path{"XYZ"};
        p /= std::string_view("Appendix");
        CHECK(p == "XYZ/Appendix");
    }
#if defined(IS_WCHAR_PATH) || defined(GHC_USE_WCHAR_T)
    {
        std::wstring p(L"foo/bar");
        std::wstring_view sv(p);
        CHECK(fs::path(sv, fs::path::format::generic_format).generic_string() == "foo/bar");
        fs::path p2(L"fo");
        p2 += std::wstring_view(L"o");
        CHECK(p2 == "foo");
        CHECK(p2.compare(std::wstring_view(L"foo")) == 0);
    }
#endif

#else
    WARN("std::string_view specific tests are empty without std::string_view.");
#endif
}

TEST_CASE("Windows: Long filename support", "[filesystem][path][fs.path.win.long]")
{
#ifdef GHC_OS_WINDOWS
    TemporaryDirectory t(TempOpt::change_path);
    char c = 'A';
    fs::path dir{"\\\\?\\"};
    dir += fs::current_path().u8string();
    for (; c <= 'Z'; ++c) {
        std::string part = std::string(16, c);
        dir /= part;
        CHECK_NOTHROW(fs::create_directory(dir));
        CHECK(fs::exists(dir));
        generateFile(dir / "f0");
        CHECK(fs::exists(dir / "f0"));
        auto native = dir.u8string();
        if (native.substr(0, 4) == u8"\\\\?\\") {
            break;
        }
    }
    CHECK(c <= 'Z');
#else
    WARN("Windows specific tests are empty on non-Windows systems.");
#endif
}

TEST_CASE("Windows: path namespace handling", "[filesystem][path][fs.path.win.namespaces]")
{
#ifdef GHC_OS_WINDOWS
    {
        std::error_code ec;
        fs::path p(R"(\\localhost\c$\Windows)");
        auto symstat = fs::symlink_status(p, ec);
        CHECK(!ec);
        auto p2 = fs::canonical(p, ec);
        CHECK(!ec);
        CHECK(p2 == p);
    }
    
    struct TestInfo
    {
        std::string _path;
        std::string _string;
        std::string _rootName;
        std::string _rootPath;
        std::string _iterateResult;
    };
    std::vector<TestInfo> variants = {
        {R"(C:\Windows\notepad.exe)", R"(C:\Windows\notepad.exe)", "C:", "C:\\", "C:,/,Windows,notepad.exe"},
#ifdef USE_STD_FS
        {R"(\\?\C:\Windows\notepad.exe)", R"(\\?\C:\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,C:,Windows,notepad.exe"},
        {R"(\??\C:\Windows\notepad.exe)", R"(\??\C:\Windows\notepad.exe)", "\\??", "\\??\\", "/??,/,C:,Windows,notepad.exe"},
#else
        {R"(\\?\C:\Windows\notepad.exe)", R"(C:\Windows\notepad.exe)", "C:", "C:\\", "C:,/,Windows,notepad.exe"},
        {R"(\??\C:\Windows\notepad.exe)", R"(C:\Windows\notepad.exe)", "C:", "C:\\", "C:,/,Windows,notepad.exe"},
#endif
        {R"(\\.\C:\Windows\notepad.exe)", R"(\\.\C:\Windows\notepad.exe)", "\\\\.", "\\\\.\\", "//.,/,C:,Windows,notepad.exe"},
        {R"(\\?\HarddiskVolume1\Windows\notepad.exe)", R"(\\?\HarddiskVolume1\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,HarddiskVolume1,Windows,notepad.exe"},
        {R"(\\?\Harddisk0Partition1\Windows\notepad.exe)", R"(\\?\Harddisk0Partition1\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,Harddisk0Partition1,Windows,notepad.exe"},
        {R"(\\.\GLOBALROOT\Device\HarddiskVolume1\Windows\notepad.exe)", R"(\\.\GLOBALROOT\Device\HarddiskVolume1\Windows\notepad.exe)", "\\\\.", "\\\\.\\", "//.,/,GLOBALROOT,Device,HarddiskVolume1,Windows,notepad.exe"},
        {R"(\\?\GLOBALROOT\Device\Harddisk0\Partition1\Windows\notepad.exe)", R"(\\?\GLOBALROOT\Device\Harddisk0\Partition1\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,GLOBALROOT,Device,Harddisk0,Partition1,Windows,notepad.exe"},
        {R"(\\?\Volume{e8a4a89d-0000-0000-0000-100000000000}\Windows\notepad.exe)", R"(\\?\Volume{e8a4a89d-0000-0000-0000-100000000000}\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,Volume{e8a4a89d-0000-0000-0000-100000000000},Windows,notepad.exe"},
        {R"(\\LOCALHOST\C$\Windows\notepad.exe)", R"(\\LOCALHOST\C$\Windows\notepad.exe)", "\\\\LOCALHOST", "\\\\LOCALHOST\\", "//LOCALHOST,/,C$,Windows,notepad.exe"},
        {R"(\\?\UNC\C$\Windows\notepad.exe)", R"(\\?\UNC\C$\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,UNC,C$,Windows,notepad.exe"},
        {R"(\\?\GLOBALROOT\Device\Mup\C$\Windows\notepad.exe)", R"(\\?\GLOBALROOT\Device\Mup\C$\Windows\notepad.exe)", "\\\\?", "\\\\?\\", "//?,/,GLOBALROOT,Device,Mup,C$,Windows,notepad.exe"},
    };

    for (auto ti : variants) {
        INFO("Used path: " + ti._path);
        auto p = fs::path(ti._path);
        CHECK(p.string() == ti._string);
        CHECK(p.root_name().string() == ti._rootName);
        CHECK(p.root_path().string() == ti._rootPath);
        CHECK(iterateResult(p) == ti._iterateResult);
    }
#else
    WARN("Windows specific tests are empty on non-Windows systems.");
#endif
}
