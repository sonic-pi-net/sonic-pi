#include "clar_libgit2.h"

#include "futils.h"
#include "refs.h"
#include "ref_helpers.h"

static git_repository *g_repo;

static const char *loose_tag_ref_name = "refs/tags/e90810b";

void test_refs_basic__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
	cl_git_pass(git_repository_set_ident(g_repo, "me", "foo@example.com"));
}

void test_refs_basic__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_basic__reference_realloc(void)
{
	git_reference *ref;
	git_reference *new_ref;
	const char *new_name = "refs/tags/awful/name-which-is/clearly/really-that-much/longer-than/the-old-one";

	/* Retrieval of the reference to rename */
	cl_git_pass(git_reference_lookup(&ref, g_repo, loose_tag_ref_name));

	new_ref = git_reference__realloc(&ref, new_name);
	cl_assert(new_ref != NULL);
	git_reference_free(new_ref);
	git_reference_free(ref);

	/* Reload, so we restore the value */
	cl_git_pass(git_reference_lookup(&ref, g_repo, loose_tag_ref_name));

	cl_git_pass(git_reference_rename(&new_ref, ref, new_name, 1, "log message"));
	cl_assert(ref != NULL);
	cl_assert(new_ref != NULL);
	git_reference_free(new_ref);
	git_reference_free(ref);
}

void test_refs_basic__longpaths(void)
{
#ifdef GIT_WIN32
	const char *base;
	size_t base_len, extra_len;
	ssize_t remain_len, i;
	git_str refname = GIT_STR_INIT;
	git_reference *one = NULL, *two = NULL;
	git_oid id;

	cl_git_pass(git_oid__fromstr(&id, "099fabac3a9ea935598528c27f866e34089c2eff", GIT_OID_SHA1));

	base = git_repository_path(g_repo);
	base_len = git_utf8_char_length(base, strlen(base));
	extra_len = CONST_STRLEN("logs/refs/heads/") + CONST_STRLEN(".lock");

	remain_len = (ssize_t)MAX_PATH - (base_len + extra_len);
	cl_assert(remain_len > 0);

	cl_git_pass(git_str_puts(&refname, "refs/heads/"));

	for (i = 0; i < remain_len; i++) {
		cl_git_pass(git_str_putc(&refname, 'a'));
	}

	/*
	 * The full path to the reflog lockfile is 260 characters,
	 * this is permitted.
	 */
	cl_git_pass(git_reference_create(&one, g_repo, refname.ptr, &id, 0, NULL));

	/* Adding one more character gives us a path that is too long. */
	cl_git_pass(git_str_putc(&refname, 'z'));
	cl_git_fail(git_reference_create(&two, g_repo, refname.ptr, &id, 0, NULL));

	git_reference_free(one);
	git_reference_free(two);
	git_str_dispose(&refname);
#endif
}
