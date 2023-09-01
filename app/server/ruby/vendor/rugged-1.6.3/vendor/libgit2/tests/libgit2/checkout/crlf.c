#include "clar_libgit2.h"
#include "checkout_helpers.h"
#include "../filter/crlf.h"
#include "futils.h"

#include "git2/checkout.h"
#include "repository.h"
#include "index.h"
#include "posix.h"

static git_repository *g_repo;

static const char *systype;
static git_str expected_fixture = GIT_STR_INIT;

static int unlink_file(void *payload, git_str *path)
{
	char *fn;

	cl_assert(fn = git_fs_path_basename(path->ptr));

	GIT_UNUSED(payload);

	if (strcmp(fn, ".git"))
		cl_must_pass(p_unlink(path->ptr));

	git__free(fn);
	return 0;
}

void test_checkout_crlf__initialize(void)
{
	git_str reponame = GIT_STR_INIT;

	g_repo = cl_git_sandbox_init("crlf");

	/*
	 * remove the contents of the working directory so that we can
	 * check out over it.
	 */
	cl_git_pass(git_str_puts(&reponame, "crlf"));
	cl_git_pass(git_fs_path_direach(&reponame, 0, unlink_file, NULL));

	if (GIT_EOL_NATIVE == GIT_EOL_CRLF)
		systype = "windows";
	else
		systype = "posix";

	git_str_dispose(&reponame);
}

void test_checkout_crlf__cleanup(void)
{
	cl_git_sandbox_cleanup();

	if (expected_fixture.size) {
		cl_fixture_cleanup(expected_fixture.ptr);
		git_str_dispose(&expected_fixture);
	}
}

struct compare_data
{
	const char *dirname;
	const char *autocrlf;
	const char *attrs;
};

static int compare_file(void *payload, git_str *actual_path)
{
	git_str expected_path = GIT_STR_INIT;
	git_str actual_contents = GIT_STR_INIT;
	git_str expected_contents = GIT_STR_INIT;
	struct compare_data *cd = payload;
	bool failed = true;
	int cmp_git, cmp_gitattributes;
	char *basename;

	basename = git_fs_path_basename(actual_path->ptr);
	cmp_git = strcmp(basename, ".git");
	cmp_gitattributes = strcmp(basename, ".gitattributes");

	if (cmp_git == 0 || cmp_gitattributes == 0) {
		failed = false;
		goto done;
	}

	cl_git_pass(git_str_joinpath(&expected_path, cd->dirname, basename));

	if (!git_fs_path_isfile(expected_path.ptr) ||
		!git_fs_path_isfile(actual_path->ptr))
		goto done;

	if (git_futils_readbuffer(&actual_contents, actual_path->ptr) < 0 ||
		git_futils_readbuffer(&expected_contents, expected_path.ptr) < 0)
		goto done;

	if (actual_contents.size != expected_contents.size)
		goto done;

	if (memcmp(actual_contents.ptr, expected_contents.ptr, expected_contents.size) != 0)
		goto done;

	failed = false;

done:
	if (failed) {
		git_str details = GIT_STR_INIT;
		git_str_printf(&details, "filename=%s, system=%s, autocrlf=%s, attrs={%s}",
			git_fs_path_basename(actual_path->ptr), systype, cd->autocrlf, cd->attrs);
		clar__fail(__FILE__, __func__, __LINE__,
			"checked out contents did not match expected", details.ptr, 0);
		git_str_dispose(&details);
	}

	git__free(basename);
	git_str_dispose(&expected_contents);
	git_str_dispose(&actual_contents);
	git_str_dispose(&expected_path);

	return 0;
}

static void test_checkout(const char *autocrlf, const char *attrs)
{
	git_str attrbuf = GIT_STR_INIT;
	git_str expected_dirname = GIT_STR_INIT;
	git_str systype_and_direction = GIT_STR_INIT;
	git_str sandboxname = GIT_STR_INIT;
	git_str reponame = GIT_STR_INIT;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct compare_data compare_data = { NULL, autocrlf, attrs };
	const char *c;

	cl_git_pass(git_str_puts(&reponame, "crlf"));

	cl_git_pass(git_str_puts(&systype_and_direction, systype));
	cl_git_pass(git_str_puts(&systype_and_direction, "_to_workdir"));

	cl_git_pass(git_str_puts(&sandboxname, "autocrlf_"));
	cl_git_pass(git_str_puts(&sandboxname, autocrlf));

	if (*attrs) {
		cl_git_pass(git_str_puts(&sandboxname, ","));

		for (c = attrs; *c; c++) {
			if (*c == ' ')
				cl_git_pass(git_str_putc(&sandboxname, ','));
			else if (*c == '=')
				cl_git_pass(git_str_putc(&sandboxname, '_'));
			else
				cl_git_pass(git_str_putc(&sandboxname, *c));
		}

		cl_git_pass(git_str_printf(&attrbuf, "* %s\n", attrs));
		cl_git_mkfile("crlf/.gitattributes", attrbuf.ptr);
	}

	cl_repo_set_string(g_repo, "core.autocrlf", autocrlf);

	cl_git_pass(git_str_joinpath(&expected_dirname, systype_and_direction.ptr, sandboxname.ptr));
	cl_git_pass(git_str_joinpath(&expected_fixture, "crlf_data", expected_dirname.ptr));
	cl_fixture_sandbox(expected_fixture.ptr);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	compare_data.dirname = sandboxname.ptr;
	cl_git_pass(git_fs_path_direach(&reponame, 0, compare_file, &compare_data));

	cl_fixture_cleanup(expected_fixture.ptr);
	git_str_dispose(&expected_fixture);

	git_str_dispose(&attrbuf);
	git_str_dispose(&expected_fixture);
	git_str_dispose(&expected_dirname);
	git_str_dispose(&sandboxname);
	git_str_dispose(&systype_and_direction);
	git_str_dispose(&reponame);
}

static void empty_workdir(const char *name)
{
	git_vector contents = GIT_VECTOR_INIT;
	char *basename;
	int cmp;
	size_t i;
	const char *fn;

	cl_git_pass(git_fs_path_dirload(&contents, name, 0, 0));
	git_vector_foreach(&contents, i, fn) {
		cl_assert(basename = git_fs_path_basename(fn));
		cmp = strncasecmp(basename, ".git", 4);

		git__free(basename);

		if (cmp)
			cl_git_pass(p_unlink(fn));
	}
	git_vector_free_deep(&contents);
}

void test_checkout_crlf__matches_core_git(void)
{
	const char *autocrlf[] = { "true", "false", "input", NULL };
	const char *attrs[] = { "", "-crlf", "-text", "eol=crlf", "eol=lf",
		"text", "text eol=crlf", "text eol=lf",
		"text=auto", "text=auto eol=crlf", "text=auto eol=lf",
		NULL };
	const char **a, **b;

	for (a = autocrlf; *a; a++) {
		for (b = attrs; *b; b++) {
			empty_workdir("crlf");
			test_checkout(*a, *b);
		}
	}
}

void test_checkout_crlf__detect_crlf_autocrlf_false(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	git_checkout_head(g_repo, &opts);

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_RAW);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
}

void test_checkout_crlf__autocrlf_false_index_size_is_unfiltered_size(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_pass(git_repository_index(&index, g_repo));
	tick_index(index);

	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert((entry = git_index_get_bypath(index, "all-lf", 0)) != NULL);
	cl_assert(entry->file_size == strlen(ALL_LF_TEXT_RAW));

	cl_assert((entry = git_index_get_bypath(index, "all-crlf", 0)) != NULL);
	cl_assert(entry->file_size == strlen(ALL_CRLF_TEXT_RAW));

	git_index_free(index);
}

void test_checkout_crlf__detect_crlf_autocrlf_true(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	git_checkout_head(g_repo, &opts);

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_AS_CRLF);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
}

void test_checkout_crlf__detect_crlf_autocrlf_true_utf8(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	git_repository_set_head(g_repo, "refs/heads/master");
	git_checkout_head(g_repo, &opts);

	check_file_contents("./crlf/few-utf8-chars-lf", FEW_UTF8_CRLF_RAW);
	check_file_contents("./crlf/many-utf8-chars-lf", MANY_UTF8_CRLF_RAW);

	check_file_contents("./crlf/few-utf8-chars-crlf", FEW_UTF8_CRLF_RAW);
	check_file_contents("./crlf/many-utf8-chars-crlf", MANY_UTF8_CRLF_RAW);
}

void test_checkout_crlf__autocrlf_true_index_size_is_filtered_size(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	git_repository_index(&index, g_repo);
	tick_index(index);

	git_checkout_head(g_repo, &opts);

	cl_assert((entry = git_index_get_bypath(index, "all-lf", 0)) != NULL);

	cl_assert_equal_sz(strlen(ALL_LF_TEXT_AS_CRLF), entry->file_size);

	cl_assert((entry = git_index_get_bypath(index, "all-crlf", 0)) != NULL);
	cl_assert_equal_sz(strlen(ALL_CRLF_TEXT_RAW), entry->file_size);

	git_index_free(index);
}

void test_checkout_crlf__with_ident(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_blob *blob;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_mkfile("crlf/.gitattributes",
		"*.txt text\n*.bin binary\n"
		"*.crlf text eol=crlf\n"
		"*.lf text eol=lf\n"
		"*.ident text ident\n"
		"*.identcrlf ident text eol=crlf\n"
		"*.identlf ident text eol=lf\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	/* add files with $Id$ */

	cl_git_mkfile("crlf/lf.ident", ALL_LF_TEXT_RAW "\n$Id: initial content$\n");
	cl_git_mkfile("crlf/crlf.ident", ALL_CRLF_TEXT_RAW "\r\n$Id$\r\n\r\n");
	cl_git_mkfile("crlf/more1.identlf", "$Id$\n" MORE_LF_TEXT_RAW);
	cl_git_mkfile("crlf/more2.identcrlf", "\r\n$Id: $\r\n" MORE_CRLF_TEXT_RAW);

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "lf.ident"));
	cl_git_pass(git_index_add_bypath(index, "crlf.ident"));
	cl_git_pass(git_index_add_bypath(index, "more1.identlf"));
	cl_git_pass(git_index_add_bypath(index, "more2.identcrlf"));
	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "Some ident files\n");

	git_checkout_head(g_repo, &opts);

	/* check that blobs have $Id$ */

	cl_assert((entry = git_index_get_bypath(index, "lf.ident", 0)));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &entry->id));
	cl_assert_equal_s(
		ALL_LF_TEXT_RAW "\n$Id$\n", git_blob_rawcontent(blob));
	git_blob_free(blob);

	cl_assert((entry = git_index_get_bypath(index, "more2.identcrlf", 0)));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &entry->id));
	cl_assert_equal_s(
		"\n$Id$\n" MORE_CRLF_TEXT_AS_LF, git_blob_rawcontent(blob));
	git_blob_free(blob);

	/* check that filesystem is initially untouched - matching core Git */

	cl_assert_equal_file(
		ALL_LF_TEXT_RAW "\n$Id: initial content$\n", 0, "crlf/lf.ident");

	/* check that forced checkout rewrites correctly */

	p_unlink("crlf/lf.ident");
	p_unlink("crlf/crlf.ident");
	p_unlink("crlf/more1.identlf");
	p_unlink("crlf/more2.identcrlf");

	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert_equal_file(
		ALL_LF_TEXT_AS_CRLF
		"\r\n$Id: fcf6d4d9c212dc66563b1171b1cd99953c756467 $\r\n",
		0, "crlf/lf.ident");
	cl_assert_equal_file(
		ALL_CRLF_TEXT_RAW
		"\r\n$Id: f2c66ad9b2b5a734d9bf00d5000cc10a62b8a857 $\r\n\r\n",
		0, "crlf/crlf.ident");

	cl_assert_equal_file(
		"$Id: f7830382dac1f1583422be5530fdfbd26289431b $\n"
		MORE_LF_TEXT_AS_LF, 0, "crlf/more1.identlf");

	cl_assert_equal_file(
		"\r\n$Id: 74677a68413012ce8d7e7cfc3f12603df3a3eac4 $\r\n"
		MORE_CRLF_TEXT_AS_CRLF, 0, "crlf/more2.identcrlf");

	git_index_free(index);
}

void test_checkout_crlf__autocrlf_false_no_attrs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_RAW);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
}

void test_checkout_crlf__autocrlf_true_no_attrs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_AS_CRLF);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_AS_CRLF);
}

void test_checkout_crlf__autocrlf_input_no_attrs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_string(g_repo, "core.autocrlf", "input");

	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_RAW);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
}

void test_checkout_crlf__autocrlf_false_text_auto_attr(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	cl_git_pass(git_checkout_head(g_repo, &opts));

	if (GIT_EOL_NATIVE == GIT_EOL_CRLF) {
		check_file_contents("./crlf/all-lf", ALL_LF_TEXT_AS_CRLF);
		check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_AS_CRLF);
	} else {
		check_file_contents("./crlf/all-lf", ALL_LF_TEXT_RAW);
		check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
	}
}

void test_checkout_crlf__autocrlf_true_text_auto_attr(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_AS_CRLF);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_AS_CRLF);
}

void test_checkout_crlf__autocrlf_input_text_auto_attr(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_mkfile("./crlf/.gitattributes", "* text=auto\n");

	cl_repo_set_string(g_repo, "core.autocrlf", "input");

	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/all-lf", ALL_LF_TEXT_RAW);
	check_file_contents("./crlf/all-crlf", ALL_CRLF_TEXT_RAW);
}

void test_checkout_crlf__can_write_empty_file(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/empty-files"));
	cl_git_pass(git_checkout_head(g_repo, &opts));

	check_file_contents("./crlf/test1.txt", "");

	check_file_contents("./crlf/test2.txt", "test2.txt's content\r\n");

	check_file_contents("./crlf/test3.txt", "");
}
