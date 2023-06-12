#include "clar_libgit2.h"
#include "describe_helpers.h"
#include "repository.h"

/* Ported from https://github.com/git/git/blob/adfc1857bdb090786fd9d22c1acec39371c76048/t/t6120-describe.sh */

static git_repository *repo;

void test_describe_t6120__initialize(void)
{
	repo = cl_git_sandbox_init("describe");
}

void test_describe_t6120__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_describe_t6120__default(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;

	assert_describe("A-*", "HEAD", repo, &opts, &fmt_opts);
	assert_describe("A-*", "HEAD^", repo, &opts, &fmt_opts);
	assert_describe("R-*", "HEAD^^", repo, &opts, &fmt_opts);
	assert_describe("A-*", "HEAD^^2", repo, &opts, &fmt_opts);
	assert_describe("B", "HEAD^^2^", repo, &opts, &fmt_opts);
	assert_describe("R-*", "HEAD^^^", repo, &opts, &fmt_opts);
}

void test_describe_t6120__tags(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;
	opts.describe_strategy = GIT_DESCRIBE_TAGS;

	assert_describe("c-*", "HEAD", repo, &opts, &fmt_opts);
	assert_describe("c-*", "HEAD^", repo, &opts, &fmt_opts);
	assert_describe("e-*", "HEAD^^", repo, &opts, &fmt_opts);
	assert_describe("c-*", "HEAD^^2", repo, &opts, &fmt_opts);
	assert_describe("B", "HEAD^^2^", repo, &opts, &fmt_opts);
	assert_describe("e", "HEAD^^^", repo, &opts, &fmt_opts);
}

void test_describe_t6120__all(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;
	opts.describe_strategy = GIT_DESCRIBE_ALL;

	assert_describe("heads/master", "HEAD", repo, &opts, &fmt_opts);
	assert_describe("tags/c-*", "HEAD^", repo, &opts, &fmt_opts);
	assert_describe("tags/e", "HEAD^^^", repo, &opts, &fmt_opts);
}

void test_describe_t6120__longformat(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;

	fmt_opts.always_use_long_format = 1;

	assert_describe("B-0-*", "HEAD^^2^", repo, &opts, &fmt_opts);
	assert_describe("A-3-*", "HEAD^^2", repo, &opts, &fmt_opts);
}

void test_describe_t6120__firstparent(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;
	opts.describe_strategy = GIT_DESCRIBE_TAGS;

	assert_describe("c-7-*", "HEAD", repo, &opts, &fmt_opts);

	opts.only_follow_first_parent = 1;
	assert_describe("e-3-*", "HEAD", repo, &opts, &fmt_opts);
}

void test_describe_t6120__workdir(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;

	assert_describe_workdir("A-*[0-9a-f]", repo, &opts, &fmt_opts);
	cl_git_mkfile("describe/file", "something different");

	fmt_opts.dirty_suffix = "-dirty";
	assert_describe_workdir("A-*[0-9a-f]-dirty", repo, &opts, &fmt_opts);
	fmt_opts.dirty_suffix = ".mod";
	assert_describe_workdir("A-*[0-9a-f].mod", repo, &opts, &fmt_opts);
}

static void commit_and_tag(
	git_time_t *time,
	const char *commit_msg,
	const char *tag_name)
{
	git_index *index;
	git_oid commit_id;
	git_reference *ref;
	
	cl_git_pass(git_repository_index__weakptr(&index, repo));

	cl_git_append2file("describe/file", "\n");
	
	cl_git_pass(git_index_add_bypath(index, "file"));
	cl_git_pass(git_index_write(index));

	*time += 10;
	cl_repo_commit_from_index(&commit_id, repo, NULL, *time, commit_msg);

	if (tag_name == NULL)
		return;

	cl_git_pass(git_reference_create(&ref, repo, tag_name, &commit_id, 0, NULL));
	git_reference_free(ref);
}

void test_describe_t6120__pattern(void)
{
	git_describe_options opts = GIT_DESCRIBE_OPTIONS_INIT;
	git_describe_format_options fmt_opts = GIT_DESCRIBE_FORMAT_OPTIONS_INIT;
	git_oid tag_id;
	git_object *head;
	git_signature *tagger;
	git_time_t time;

	/* set-up matching pattern tests */
	cl_git_pass(git_revparse_single(&head, repo, "HEAD"));

	time = 1380553019;
	cl_git_pass(git_signature_new(&tagger, "tagger", "tagger@libgit2.org", time, 0));
	cl_git_pass(git_tag_create(&tag_id, repo, "test-annotated", head, tagger, "test-annotated", 0));
	git_signature_free(tagger);
	git_object_free(head);

	commit_and_tag(&time, "one more", "refs/tags/test1-lightweight");
	commit_and_tag(&time, "yet another", "refs/tags/test2-lightweight");
	commit_and_tag(&time, "even more", NULL);


	/* Exercize */
	opts.pattern = "test-*";
	assert_describe("test-annotated-*", "HEAD", repo, &opts, &fmt_opts);

	opts.describe_strategy = GIT_DESCRIBE_TAGS;
	opts.pattern = "test1-*";
	assert_describe("test1-lightweight-*", "HEAD", repo, &opts, &fmt_opts);

	opts.pattern = "test2-*";
	assert_describe("test2-lightweight-*", "HEAD", repo, &opts, &fmt_opts);

	fmt_opts.always_use_long_format = 1;
	assert_describe("test2-lightweight-*", "HEAD^", repo, &opts, &fmt_opts);
}
