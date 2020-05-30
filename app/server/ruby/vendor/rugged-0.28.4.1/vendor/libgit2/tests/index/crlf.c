#include "clar_libgit2.h"
#include "../filter/crlf.h"

#include "git2/checkout.h"
#include "repository.h"
#include "posix.h"

#define FILE_CONTENTS_LF "one\ntwo\nthree\nfour\n"
#define FILE_CONTENTS_CRLF "one\r\ntwo\r\nthree\r\nfour\r\n"

#define FILE_OID_LF "f384549cbeb481e437091320de6d1f2e15e11b4a"
#define FILE_OID_CRLF "7fbf4d847b191141d80f30c8ab03d2ad4cd543a9"

static git_repository *g_repo;
static git_index *g_index;

static git_buf expected_fixture = GIT_BUF_INIT;

void test_index_crlf__initialize(void)
{
	g_repo = cl_git_sandbox_init_new("crlf");
	cl_git_pass(git_repository_index(&g_index, g_repo));
}

void test_index_crlf__cleanup(void)
{
	git_index_free(g_index);
	cl_git_sandbox_cleanup();

	if (expected_fixture.size) {
		cl_fixture_cleanup(expected_fixture.ptr);
		git_buf_dispose(&expected_fixture);
	}
}

struct compare_data
{
	const char *systype;
	const char *dirname;
	const char *safecrlf;
	const char *autocrlf;
	const char *attrs;
};

static int add_and_check_file(void *payload, git_buf *actual_path)
{
	git_buf expected_path = GIT_BUF_INIT;
	git_buf expected_path_fail = GIT_BUF_INIT;
	git_buf expected_contents = GIT_BUF_INIT;
	struct compare_data *cd = payload;
	char *basename;
	const git_index_entry *entry;
	git_blob *blob;
	bool failed = true;

	basename = git_path_basename(actual_path->ptr);

	if (!strcmp(basename, ".git") || !strcmp(basename, ".gitattributes")) {
		failed = false;
		goto done;
	}

	cl_git_pass(git_buf_joinpath(&expected_path, cd->dirname, basename));

	cl_git_pass(git_buf_puts(&expected_path_fail, expected_path.ptr));
	cl_git_pass(git_buf_puts(&expected_path_fail, ".fail"));

	if (git_path_isfile(expected_path.ptr)) {
		cl_git_pass(git_index_add_bypath(g_index, basename));

		cl_assert(entry = git_index_get_bypath(g_index, basename, 0));
		cl_git_pass(git_blob_lookup(&blob, g_repo, &entry->id));

		cl_git_pass(git_futils_readbuffer(&expected_contents, expected_path.ptr));

		if (strcmp(expected_contents.ptr, git_blob_rawcontent(blob)) != 0)
			goto done;

		git_blob_free(blob);
	} else if (git_path_isfile(expected_path_fail.ptr)) {
		cl_git_pass(git_futils_readbuffer(&expected_contents, expected_path_fail.ptr));
		git_buf_rtrim(&expected_contents);

		if (git_index_add_bypath(g_index, basename) == 0 ||
			git_error_last()->klass != GIT_ERROR_FILTER ||
			strcmp(expected_contents.ptr, git_error_last()->message) != 0)
			goto done;
	} else {
		cl_fail("unexpected index failure");
	}

	failed = false;

done:
	if (failed) {
		git_buf details = GIT_BUF_INIT;
		git_buf_printf(&details, "filename=%s, system=%s, autocrlf=%s, safecrlf=%s, attrs={%s}",
			basename, cd->systype, cd->autocrlf, cd->safecrlf, cd->attrs);
		clar__fail(__FILE__, __LINE__,
			"index contents did not match expected", details.ptr, 0);
		git_buf_dispose(&details);
	}

	git__free(basename);
	git_buf_dispose(&expected_contents);
	git_buf_dispose(&expected_path);
	git_buf_dispose(&expected_path_fail);
	return 0;
}

static const char *system_type(void)
{
	if (GIT_EOL_NATIVE == GIT_EOL_CRLF)
		return "windows";
	else
		return "posix";
}

static void test_add_index(const char *safecrlf, const char *autocrlf, const char *attrs)
{
	git_buf attrbuf = GIT_BUF_INIT;
	git_buf expected_dirname = GIT_BUF_INIT;
	git_buf sandboxname = GIT_BUF_INIT;
	git_buf reponame = GIT_BUF_INIT;
	struct compare_data compare_data = { system_type(), NULL, safecrlf, autocrlf, attrs };
	const char *c;

	git_buf_puts(&reponame, "crlf");

	git_buf_puts(&sandboxname, "autocrlf_");
	git_buf_puts(&sandboxname, autocrlf);

	git_buf_puts(&sandboxname, ",safecrlf_");
	git_buf_puts(&sandboxname, safecrlf);

	if (*attrs) {
		git_buf_puts(&sandboxname, ",");

		for (c = attrs; *c; c++) {
			if (*c == ' ')
				git_buf_putc(&sandboxname, ',');
			else if (*c == '=')
				git_buf_putc(&sandboxname, '_');
			else
				git_buf_putc(&sandboxname, *c);
		}

		git_buf_printf(&attrbuf, "* %s\n", attrs);
		cl_git_mkfile("crlf/.gitattributes", attrbuf.ptr);
	}

	cl_repo_set_string(g_repo, "core.safecrlf", safecrlf);
	cl_repo_set_string(g_repo, "core.autocrlf", autocrlf);

	cl_git_pass(git_index_clear(g_index));

	git_buf_joinpath(&expected_dirname, "crlf_data", system_type());
	git_buf_puts(&expected_dirname, "_to_odb");

	git_buf_joinpath(&expected_fixture, expected_dirname.ptr, sandboxname.ptr);
	cl_fixture_sandbox(expected_fixture.ptr);

	compare_data.dirname = sandboxname.ptr;
	cl_git_pass(git_path_direach(&reponame, 0, add_and_check_file, &compare_data));

	cl_fixture_cleanup(expected_fixture.ptr);
	git_buf_dispose(&expected_fixture);

	git_buf_dispose(&attrbuf);
	git_buf_dispose(&expected_fixture);
	git_buf_dispose(&expected_dirname);
	git_buf_dispose(&sandboxname);
	git_buf_dispose(&reponame);
}

static void set_up_workingdir(const char *name)
{
	git_vector contents = GIT_VECTOR_INIT;
	size_t i;
	const char *fn;

	git_path_dirload(&contents, name, 0, 0);
	git_vector_foreach(&contents, i, fn) {
		char *basename = git_path_basename(fn);
		bool skip = strncasecmp(basename, ".git", 4) == 0 && strlen(basename) == 4;

		git__free(basename);

		if (skip)
			continue;
		p_unlink(fn);
	}
	git_vector_free_deep(&contents);

	/* copy input files */
	git_path_dirload(&contents, cl_fixture("crlf"), 0, 0);
	git_vector_foreach(&contents, i, fn) {
		char *basename = git_path_basename(fn);
		git_buf dest_filename = GIT_BUF_INIT;

		if (strcmp(basename, ".gitted") &&
			strcmp(basename, ".gitattributes")) {
			git_buf_joinpath(&dest_filename, name, basename);
			cl_git_pass(git_futils_cp(fn, dest_filename.ptr, 0644));
		}

		git__free(basename);
		git_buf_dispose(&dest_filename);
	}
	git_vector_free_deep(&contents);
}

void test_index_crlf__matches_core_git(void)
{
	const char *safecrlf[] = { "true", "false", "warn", NULL };
	const char *autocrlf[] = { "true", "false", "input", NULL };
	const char *attrs[] = { "", "-crlf", "-text", "eol=crlf", "eol=lf",
		"text", "text eol=crlf", "text eol=lf",
		"text=auto", "text=auto eol=crlf", "text=auto eol=lf",
		NULL };
	const char **a, **b, **c;

	for (a = safecrlf; *a; a++) {
		for (b = autocrlf; *b; b++) {
			for (c = attrs; *c; c++) {
				set_up_workingdir("crlf");
				test_add_index(*a, *b, *c);
			}
		}
	}
}

void test_index_crlf__autocrlf_false_no_attrs(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid,
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_OID_CRLF : FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__autocrlf_true_no_attrs(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__autocrlf_input_no_attrs(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_repo_set_string(g_repo, "core.autocrlf", "input");

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__autocrlf_false_text_auto_attr(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__autocrlf_true_text_auto_attr(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__autocrlf_input_text_auto_attr(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_string(g_repo, "core.autocrlf", "input");

	cl_git_mkfile("./crlf/newfile.txt",
		(GIT_EOL_NATIVE == GIT_EOL_CRLF) ? FILE_CONTENTS_CRLF : FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);
}

void test_index_crlf__safecrlf_true_autocrlf_input_text_auto_attr(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_string(g_repo, "core.autocrlf", "input");
	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_mkfile("./crlf/newfile.txt", FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);
	cl_assert(entry);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);

	cl_git_mkfile("./crlf/newfile2.txt", FILE_CONTENTS_CRLF);
	cl_git_fail(git_index_add_bypath(g_index, "newfile2.txt"));
}

void test_index_crlf__safecrlf_true_autocrlf_input_text__no_attr(void)
{
	const git_index_entry *entry;
	git_oid oid;

	cl_repo_set_string(g_repo, "core.autocrlf", "input");
	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_mkfile("./crlf/newfile.txt", FILE_CONTENTS_LF);

	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));
	entry = git_index_get_bypath(g_index, "newfile.txt", 0);
	cl_assert(entry);

	cl_git_pass(git_oid_fromstr(&oid, FILE_OID_LF));
	cl_assert_equal_oid(&oid, &entry->id);

	cl_git_mkfile("./crlf/newfile2.txt", FILE_CONTENTS_CRLF);
	cl_git_fail(git_index_add_bypath(g_index, "newfile2.txt"));
}

void test_index_crlf__safecrlf_true_no_attrs(void)
{
	cl_repo_set_bool(g_repo, "core.autocrlf", true);
	cl_repo_set_bool(g_repo, "core.safecrlf", true);

	cl_git_mkfile("crlf/newfile.txt", ALL_LF_TEXT_RAW);
	cl_git_fail(git_index_add_bypath(g_index, "newfile.txt"));

	cl_git_mkfile("crlf/newfile.txt", ALL_CRLF_TEXT_RAW);
	cl_git_pass(git_index_add_bypath(g_index, "newfile.txt"));

	cl_git_mkfile("crlf/newfile.txt", MORE_CRLF_TEXT_RAW);
	cl_git_fail(git_index_add_bypath(g_index, "newfile.txt"));

	cl_git_mkfile("crlf/newfile.txt", MORE_LF_TEXT_RAW);
	cl_git_fail(git_index_add_bypath(g_index, "newfile.txt"));
}
