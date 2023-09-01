
#include "clar_libgit2.h"

#ifdef GIT_WIN32
#include "win32/path_w32.h"
#endif

#ifdef GIT_WIN32
static void test_utf8_to_utf16(const char *utf8_in, const wchar_t *utf16_expected)
{
	git_win32_path path_utf16;
	int path_utf16len;

	cl_assert((path_utf16len = git_win32_path_from_utf8(path_utf16, utf8_in)) >= 0);
	cl_assert_equal_wcs(utf16_expected, path_utf16);
	cl_assert_equal_i(wcslen(utf16_expected), path_utf16len);
}

static void test_utf8_to_utf16_relative(const char* utf8_in, const wchar_t* utf16_expected)
{
	git_win32_path path_utf16;
	int path_utf16len;

	cl_assert((path_utf16len = git_win32_path_relative_from_utf8(path_utf16, utf8_in)) >= 0);
	cl_assert_equal_wcs(utf16_expected, path_utf16);
	cl_assert_equal_i(wcslen(utf16_expected), path_utf16len);
}
#endif

void test_path_win32__utf8_to_utf16(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("C:\\", L"\\\\?\\C:\\");
	test_utf8_to_utf16("c:\\", L"\\\\?\\c:\\");
	test_utf8_to_utf16("C:/", L"\\\\?\\C:\\");
	test_utf8_to_utf16("c:/", L"\\\\?\\c:\\");
#endif
}

void test_path_win32__removes_trailing_slash(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("C:\\Foo\\", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:\\Foo\\\\", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:\\Foo\\\\", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:/Foo/", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:/Foo///", L"\\\\?\\C:\\Foo");
#endif
}

void test_path_win32__squashes_multiple_slashes(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("C:\\\\Foo\\Bar\\\\Foobar", L"\\\\?\\C:\\Foo\\Bar\\Foobar");
	test_utf8_to_utf16("C://Foo/Bar///Foobar", L"\\\\?\\C:\\Foo\\Bar\\Foobar");
#endif
}

void test_path_win32__unc(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("\\\\server\\c$\\unc\\path", L"\\\\?\\UNC\\server\\c$\\unc\\path");
	test_utf8_to_utf16("//server/git/style/unc/path", L"\\\\?\\UNC\\server\\git\\style\\unc\\path");
#endif
}

void test_path_win32__honors_max_path(void)
{
#ifdef GIT_WIN32
	git_win32_path path_utf16;

	test_utf8_to_utf16("C:\\This path is 261 characters which is fine for our path handling functions which cope with paths longer than MAX_PATH\\0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghijk",
		L"\\\\?\\C:\\This path is 261 characters which is fine for our path handling functions which cope with paths longer than MAX_PATH\\0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghijk");

	cl_check_fail(git_win32_path_from_utf8(path_utf16, "C:\\This path is 4097 chars and exceeds our maximum path length on Windows which is limited to 4096 characters\\alas\\0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01"));

#endif
}

void test_path_win32__dot_and_dotdot(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("C:\\Foo\\..\\Foobar", L"\\\\?\\C:\\Foobar");
	test_utf8_to_utf16("C:\\Foo\\Bar\\..\\Foobar", L"\\\\?\\C:\\Foo\\Foobar");
	test_utf8_to_utf16("C:\\Foo\\Bar\\..\\Foobar\\..", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:\\Foobar\\..", L"\\\\?\\C:\\");
	test_utf8_to_utf16("C:/Foo/Bar/../Foobar", L"\\\\?\\C:\\Foo\\Foobar");
	test_utf8_to_utf16("C:/Foo/Bar/../Foobar/../Asdf/", L"\\\\?\\C:\\Foo\\Asdf");
	test_utf8_to_utf16("C:/Foo/Bar/../Foobar/..", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("C:/Foo/..", L"\\\\?\\C:\\");

	test_utf8_to_utf16("C:\\Foo\\Bar\\.\\Foobar", L"\\\\?\\C:\\Foo\\Bar\\Foobar");
	test_utf8_to_utf16("C:\\.\\Foo\\.\\Bar\\.\\Foobar\\.\\", L"\\\\?\\C:\\Foo\\Bar\\Foobar");
	test_utf8_to_utf16("C:/Foo/Bar/./Foobar", L"\\\\?\\C:\\Foo\\Bar\\Foobar");
	test_utf8_to_utf16("C:/Foo/../Bar/./Foobar/../", L"\\\\?\\C:\\Bar");

	test_utf8_to_utf16("C:\\Foo\\..\\..\\Bar", L"\\\\?\\C:\\Bar");
#endif
}

void test_path_win32__absolute_from_no_drive_letter(void)
{
#ifdef GIT_WIN32
	test_utf8_to_utf16("\\Foo", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("\\Foo\\Bar", L"\\\\?\\C:\\Foo\\Bar");
	test_utf8_to_utf16("/Foo/Bar", L"\\\\?\\C:\\Foo\\Bar");
#endif
}

void test_path_win32__absolute_from_relative(void)
{
#ifdef GIT_WIN32
	char cwd_backup[MAX_PATH];

	cl_must_pass(p_getcwd(cwd_backup, MAX_PATH));
	cl_must_pass(p_chdir("C:/"));

	test_utf8_to_utf16("Foo", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("..\\..\\Foo", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("Foo\\..", L"\\\\?\\C:\\");
	test_utf8_to_utf16("Foo\\..\\..", L"\\\\?\\C:\\");
	test_utf8_to_utf16("", L"\\\\?\\C:\\");

	cl_must_pass(p_chdir("C:/Windows"));

	test_utf8_to_utf16("Foo", L"\\\\?\\C:\\Windows\\Foo");
	test_utf8_to_utf16("Foo\\Bar", L"\\\\?\\C:\\Windows\\Foo\\Bar");
	test_utf8_to_utf16("..\\Foo", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16("Foo\\..\\Bar", L"\\\\?\\C:\\Windows\\Bar");
	test_utf8_to_utf16("", L"\\\\?\\C:\\Windows");

	cl_must_pass(p_chdir(cwd_backup));
#endif
}

void test_path_win32__keeps_relative(void)
{
#ifdef GIT_WIN32
	/* Relative paths stay relative */
	test_utf8_to_utf16_relative("Foo", L"Foo");
	test_utf8_to_utf16_relative("..\\..\\Foo", L"..\\..\\Foo");
	test_utf8_to_utf16_relative("Foo\\..", L"Foo\\..");
	test_utf8_to_utf16_relative("Foo\\..\\..", L"Foo\\..\\..");
	test_utf8_to_utf16_relative("Foo\\Bar", L"Foo\\Bar");
	test_utf8_to_utf16_relative("Foo\\..\\Bar", L"Foo\\..\\Bar");
	test_utf8_to_utf16_relative("../../Foo", L"..\\..\\Foo");
	test_utf8_to_utf16_relative("Foo/..", L"Foo\\..");
	test_utf8_to_utf16_relative("Foo/../..", L"Foo\\..\\..");
	test_utf8_to_utf16_relative("Foo/Bar", L"Foo\\Bar");
	test_utf8_to_utf16_relative("Foo/../Bar", L"Foo\\..\\Bar");
	test_utf8_to_utf16_relative("Foo/../Bar/", L"Foo\\..\\Bar\\");
	test_utf8_to_utf16_relative("", L"");

	/* Absolute paths are canonicalized */
	test_utf8_to_utf16_relative("\\Foo", L"\\\\?\\C:\\Foo");
	test_utf8_to_utf16_relative("/Foo/Bar/", L"\\\\?\\C:\\Foo\\Bar");
	test_utf8_to_utf16_relative("\\\\server\\c$\\unc\\path", L"\\\\?\\UNC\\server\\c$\\unc\\path");
#endif
}

#ifdef GIT_WIN32
static void test_canonicalize(const wchar_t *in, const wchar_t *expected)
{
	git_win32_path canonical;

	cl_assert(wcslen(in) < MAX_PATH);
	wcscpy(canonical, in);

	cl_must_pass(git_win32_path_canonicalize(canonical));
	cl_assert_equal_wcs(expected, canonical);
}
#endif

static void test_remove_namespace(const wchar_t *in, const wchar_t *expected)
{
#ifdef GIT_WIN32
	git_win32_path canonical;

	cl_assert(wcslen(in) < MAX_PATH);
	wcscpy(canonical, in);

	git_win32_path_remove_namespace(canonical, wcslen(in));
	cl_assert_equal_wcs(expected, canonical);
#else
	GIT_UNUSED(in);
	GIT_UNUSED(expected);
#endif
}

void test_path_win32__remove_namespace(void)
{
	test_remove_namespace(L"\\\\?\\C:\\Temp\\Foo", L"C:\\Temp\\Foo");
	test_remove_namespace(L"\\\\?\\C:\\", L"C:\\");
	test_remove_namespace(L"\\\\?\\", L"");

	test_remove_namespace(L"\\??\\C:\\Temp\\Foo", L"C:\\Temp\\Foo");
	test_remove_namespace(L"\\??\\C:\\", L"C:\\");
	test_remove_namespace(L"\\??\\", L"");

	test_remove_namespace(L"\\\\?\\UNC\\server\\C$\\folder", L"\\\\server\\C$\\folder");
	test_remove_namespace(L"\\\\?\\UNC\\server\\C$\\folder", L"\\\\server\\C$\\folder");
	test_remove_namespace(L"\\\\?\\UNC\\server\\C$", L"\\\\server\\C$");
	test_remove_namespace(L"\\\\?\\UNC\\server\\", L"\\\\server");
	test_remove_namespace(L"\\\\?\\UNC\\server", L"\\\\server");

	test_remove_namespace(L"\\??\\UNC\\server\\C$\\folder", L"\\\\server\\C$\\folder");
	test_remove_namespace(L"\\??\\UNC\\server\\C$\\folder", L"\\\\server\\C$\\folder");
	test_remove_namespace(L"\\??\\UNC\\server\\C$", L"\\\\server\\C$");
	test_remove_namespace(L"\\??\\UNC\\server\\", L"\\\\server");
	test_remove_namespace(L"\\??\\UNC\\server", L"\\\\server");

	test_remove_namespace(L"\\\\server\\C$\\folder", L"\\\\server\\C$\\folder");
	test_remove_namespace(L"\\\\server\\C$", L"\\\\server\\C$");
	test_remove_namespace(L"\\\\server\\", L"\\\\server");
	test_remove_namespace(L"\\\\server", L"\\\\server");

	test_remove_namespace(L"C:\\Foo\\Bar", L"C:\\Foo\\Bar");
	test_remove_namespace(L"C:\\", L"C:\\");
	test_remove_namespace(L"", L"");

}

void test_path_win32__canonicalize(void)
{
#ifdef GIT_WIN32
	test_canonicalize(L"C:\\Foo\\Bar", L"C:\\Foo\\Bar");
	test_canonicalize(L"C:\\Foo\\", L"C:\\Foo");
	test_canonicalize(L"C:\\Foo\\\\", L"C:\\Foo");
	test_canonicalize(L"C:\\Foo\\..\\Bar", L"C:\\Bar");
	test_canonicalize(L"C:\\Foo\\..\\..\\Bar", L"C:\\Bar");
	test_canonicalize(L"C:\\Foo\\..\\..\\..\\..\\", L"C:\\");
	test_canonicalize(L"C:/Foo/Bar", L"C:\\Foo\\Bar");
	test_canonicalize(L"C:/", L"C:\\");

	test_canonicalize(L"\\\\?\\C:\\Foo\\Bar", L"\\\\?\\C:\\Foo\\Bar");
	test_canonicalize(L"\\\\?\\C:\\Foo\\Bar\\", L"\\\\?\\C:\\Foo\\Bar");
	test_canonicalize(L"\\\\?\\C:\\\\Foo\\.\\Bar\\\\..\\", L"\\\\?\\C:\\Foo");
	test_canonicalize(L"\\\\?\\C:\\\\", L"\\\\?\\C:\\");
	test_canonicalize(L"//?/C:/", L"\\\\?\\C:\\");
	test_canonicalize(L"//?/C:/../../Foo/", L"\\\\?\\C:\\Foo");
	test_canonicalize(L"//?/C:/Foo/../../", L"\\\\?\\C:\\");

	test_canonicalize(L"\\\\?\\UNC\\server\\C$\\folder", L"\\\\?\\UNC\\server\\C$\\folder");
	test_canonicalize(L"\\\\?\\UNC\\server\\C$\\folder\\", L"\\\\?\\UNC\\server\\C$\\folder");
	test_canonicalize(L"\\\\?\\UNC\\server\\C$\\folder\\", L"\\\\?\\UNC\\server\\C$\\folder");
	test_canonicalize(L"\\\\?\\UNC\\server\\C$\\folder\\..\\..\\..\\..\\share\\", L"\\\\?\\UNC\\server\\share");

	test_canonicalize(L"\\\\server\\share", L"\\\\server\\share");
	test_canonicalize(L"\\\\server\\share\\", L"\\\\server\\share");
	test_canonicalize(L"\\\\server\\share\\\\foo\\\\bar", L"\\\\server\\share\\foo\\bar");
	test_canonicalize(L"\\\\server\\\\share\\\\foo\\\\bar", L"\\\\server\\share\\foo\\bar");
	test_canonicalize(L"\\\\server\\share\\..\\foo", L"\\\\server\\foo");
	test_canonicalize(L"\\\\server\\..\\..\\share\\.\\foo", L"\\\\server\\share\\foo");
#endif
}

void test_path_win32__8dot3_name(void)
{
#ifdef GIT_WIN32
	char *shortname;

	if (!cl_sandbox_supports_8dot3())
		clar__skip();

	/* Some guaranteed short names */
	cl_assert_equal_s("PROGRA~1", (shortname = git_win32_path_8dot3_name("C:\\Program Files")));
	git__free(shortname);

	cl_assert_equal_s("WINDOWS", (shortname = git_win32_path_8dot3_name("C:\\WINDOWS")));
	git__free(shortname);

	/* Create some predictable short names */
	cl_must_pass(p_mkdir(".foo", 0777));
	cl_assert_equal_s("FOO~1", (shortname = git_win32_path_8dot3_name(".foo")));
	git__free(shortname);

	cl_git_write2file("bar~1", "foobar\n", 7, O_RDWR|O_CREAT, 0666);
	cl_must_pass(p_mkdir(".bar", 0777));
	cl_assert_equal_s("BAR~2", (shortname = git_win32_path_8dot3_name(".bar")));
	git__free(shortname);
#endif
}
