#include "clar_libgit2.h"
#include "refs.h"
#include "vector.h"

static git_repository *repo;

void test_refs_iterator__initialize(void)
{
	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
}

void test_refs_iterator__cleanup(void)
{
	git_repository_free(repo);
}

static const char *refnames[] = {
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
	"refs/notes/fanout",
	"refs/remotes/test/master",
	"refs/tags/annotated_tag_to_blob",
	"refs/tags/e90810b",
	"refs/tags/hard_tag",
	"refs/tags/point_to_blob",
	"refs/tags/taggerless",
	"refs/tags/test",
	"refs/tags/wrapped_tag",
};

static int refcmp_cb(const void *a, const void *b)
{
	const git_reference *refa = (const git_reference *)a;
	const git_reference *refb = (const git_reference *)b;

	return strcmp(refa->name, refb->name);
}

static void assert_all_refnames_match(git_vector *output)
{
	size_t i;
	git_reference *ref;

	cl_assert_equal_sz(output->length, ARRAY_SIZE(refnames));

	git_vector_sort(output);

	git_vector_foreach(output, i, ref) {
		cl_assert_equal_s(ref->name, refnames[i]);
		git_reference_free(ref);
	}

	git_vector_free(output);
}

void test_refs_iterator__list(void)
{
	git_reference_iterator *iter;
	git_vector output;
	git_reference *ref;

	cl_git_pass(git_vector_init(&output, 32, &refcmp_cb));
	cl_git_pass(git_reference_iterator_new(&iter, repo));

	while (1) {
		int error = git_reference_next(&ref, iter);
		if (error == GIT_ITEROVER)
			break;
		cl_git_pass(error);
		cl_git_pass(git_vector_insert(&output, ref));
	}

	git_reference_iterator_free(iter);

	assert_all_refnames_match(&output);
}

void test_refs_iterator__empty(void)
{
	git_reference_iterator *iter;
	git_odb *odb;
	git_reference *ref;
	git_repository *empty;

	cl_git_pass(git_odb_new(&odb));
	cl_git_pass(git_repository_wrap_odb(&empty, odb));

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
	cl_git_pass(git_vector_init(&output, 32, &refcmp_cb));
	cl_git_pass(git_reference_foreach(repo, refs_foreach_cb, &output));
	assert_all_refnames_match(&output);
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

	cl_assert_equal_sz(output.length, ARRAY_SIZE(refnames));
	git_vector_sort(&output);

	git_vector_foreach(&output, i, name) {
		cl_assert_equal_s(name, refnames[i]);
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

	git_repository_free(repo);
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

	cl_git_sandbox_cleanup();
	repo = NULL;
}
