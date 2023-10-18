#include "clar_libgit2.h"
#include "refs.h"
#include "vector.h"
#include "odb.h"
#include "repository.h"

static git_repository *repo;

void test_refs_iterator__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_iterator__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static const char *refnames[] = {
	"refs/blobs/annotated_tag_to_blob",
	"refs/heads/br2",
	"refs/heads/cannot-fetch",
	"refs/heads/chomped",
	"refs/heads/haacked",
	"refs/heads/master",
	"refs/heads/not-good",
	"refs/heads/packed",
	"refs/heads/packed-test",
	"refs/heads/subtrees",
	"refs/heads/test",
	"refs/heads/track-local",
	"refs/heads/trailing",
	"refs/heads/with-empty-log",
	"refs/notes/fanout",
	"refs/remotes/test/master",
	"refs/tags/annotated_tag_to_blob",
	"refs/tags/e90810b",
	"refs/tags/hard_tag",
	"refs/tags/point_to_blob",
	"refs/tags/taggerless",
	"refs/tags/test",
	"refs/tags/wrapped_tag",
	NULL
};

static const char *refnames_with_symlink[] = {
	"refs/blobs/annotated_tag_to_blob",
	"refs/heads/br2",
	"refs/heads/cannot-fetch",
	"refs/heads/chomped",
	"refs/heads/haacked",
	"refs/heads/link/a",
	"refs/heads/link/b",
	"refs/heads/link/c",
	"refs/heads/link/d",
	"refs/heads/master",
	"refs/heads/not-good",
	"refs/heads/packed",
	"refs/heads/packed-test",
	"refs/heads/subtrees",
	"refs/heads/test",
	"refs/heads/track-local",
	"refs/heads/trailing",
	"refs/heads/with-empty-log",
	"refs/notes/fanout",
	"refs/remotes/test/master",
	"refs/tags/annotated_tag_to_blob",
	"refs/tags/e90810b",
	"refs/tags/hard_tag",
	"refs/tags/point_to_blob",
	"refs/tags/taggerless",
	"refs/tags/test",
	"refs/tags/wrapped_tag",
	NULL
};

static int refcmp_cb(const void *a, const void *b)
{
	const git_reference *refa = (const git_reference *)a;
	const git_reference *refb = (const git_reference *)b;

	return strcmp(refa->name, refb->name);
}

static void assert_all_refnames_match(const char **expected, git_vector *names)
{
	size_t i;
	git_reference *ref;

	git_vector_sort(names);

	git_vector_foreach(names, i, ref) {
		cl_assert(expected[i] != NULL);
		cl_assert_equal_s(expected[i], ref->name);
		git_reference_free(ref);
	}
	cl_assert(expected[i] == NULL);

	git_vector_free(names);
}

void test_refs_iterator__list(void)
{
	git_reference_iterator *iter;
	git_vector output;
	git_reference *ref;

	cl_git_pass(git_vector_init(&output, 33, &refcmp_cb));
	cl_git_pass(git_reference_iterator_new(&iter, repo));

	while (1) {
		int error = git_reference_next(&ref, iter);
		if (error == GIT_ITEROVER)
			break;
		cl_git_pass(error);
		cl_git_pass(git_vector_insert(&output, ref));
	}

	git_reference_iterator_free(iter);

	assert_all_refnames_match(refnames, &output);
}

void test_refs_iterator__empty(void)
{
	git_reference_iterator *iter;
	git_odb *odb;
	git_reference *ref;
	git_repository *empty;

	cl_git_pass(git_odb__new(&odb, NULL));
	cl_git_pass(git_repository__wrap_odb(&empty, odb, GIT_OID_SHA1));

	cl_git_pass(git_reference_iterator_new(&iter, empty));
	cl_assert_equal_i(GIT_ITEROVER, git_reference_next(&ref, iter));

	git_reference_iterator_free(iter);
	git_odb_free(odb);
	git_repository_free(empty);
}

static int refs_foreach_cb(git_reference *reference, void *payload)
{
	git_vector *output = payload;
	cl_git_pass(git_vector_insert(output, reference));
	return 0;
}

void test_refs_iterator__foreach(void)
{
	git_vector output;
	cl_git_pass(git_vector_init(&output, 33, &refcmp_cb));
	cl_git_pass(git_reference_foreach(repo, refs_foreach_cb, &output));
	assert_all_refnames_match(refnames, &output);
}

void test_refs_iterator__foreach_through_symlink(void)
{
	git_vector output;

#ifdef GIT_WIN32
	cl_skip();
#endif

	cl_git_pass(git_vector_init(&output, 32, &refcmp_cb));

	cl_git_pass(p_mkdir("refs", 0777));
	cl_git_mkfile("refs/a", "1234567890123456789012345678901234567890");
	cl_git_mkfile("refs/b", "1234567890123456789012345678901234567890");
	cl_git_mkfile("refs/c", "1234567890123456789012345678901234567890");
	cl_git_mkfile("refs/d", "1234567890123456789012345678901234567890");

	cl_git_pass(p_symlink("../../../refs", "testrepo.git/refs/heads/link"));

	cl_git_pass(git_reference_foreach(repo, refs_foreach_cb, &output));
	assert_all_refnames_match(refnames_with_symlink, &output);
}

static int refs_foreach_cancel_cb(git_reference *reference, void *payload)
{
	int *cancel_after = payload;

	git_reference_free(reference);

	if (!*cancel_after)
		return -333;
	(*cancel_after)--;
	return 0;
}

void test_refs_iterator__foreach_can_cancel(void)
{
	int cancel_after = 3;
	cl_git_fail_with(
		git_reference_foreach(repo, refs_foreach_cancel_cb, &cancel_after),
		-333);
	cl_assert_equal_i(0, cancel_after);
}

static int refs_foreach_name_cb(const char *name, void *payload)
{
	git_vector *output = payload;
	cl_git_pass(git_vector_insert(output, git__strdup(name)));
	return 0;
}

void test_refs_iterator__foreach_name(void)
{
	git_vector output;
	size_t i;
	char *name;

	cl_git_pass(git_vector_init(&output, 32, &git__strcmp_cb));
	cl_git_pass(
		git_reference_foreach_name(repo, refs_foreach_name_cb, &output));

	git_vector_sort(&output);

	git_vector_foreach(&output, i, name) {
		cl_assert(refnames[i] != NULL);
		cl_assert_equal_s(refnames[i], name);
		git__free(name);
	}

	git_vector_free(&output);
}

static int refs_foreach_name_cancel_cb(const char *name, void *payload)
{
	int *cancel_after = payload;
	if (!*cancel_after)
		return -333;
	GIT_UNUSED(name);
	(*cancel_after)--;
	return 0;
}

void test_refs_iterator__foreach_name_can_cancel(void)
{
	int cancel_after = 5;
	cl_git_fail_with(
		git_reference_foreach_name(
			repo, refs_foreach_name_cancel_cb, &cancel_after),
		-333);
	cl_assert_equal_i(0, cancel_after);
}

void test_refs_iterator__concurrent_delete(void)
{
	git_reference_iterator *iter;
	size_t full_count = 0, concurrent_count = 0;
	const char *name;
	int error;

	cl_git_sandbox_cleanup();
	repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_reference_iterator_new(&iter, repo));
	while ((error = git_reference_next_name(&name, iter)) == 0) {
		full_count++;
	}

	git_reference_iterator_free(iter);
	cl_assert_equal_i(GIT_ITEROVER, error);

	cl_git_pass(git_reference_iterator_new(&iter, repo));
	while ((error = git_reference_next_name(&name, iter)) == 0) {
		cl_git_pass(git_reference_remove(repo, name));
		concurrent_count++;
	}

	git_reference_iterator_free(iter);
	cl_assert_equal_i(GIT_ITEROVER, error);

	cl_assert_equal_i(full_count, concurrent_count);
}
