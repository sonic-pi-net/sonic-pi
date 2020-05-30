#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"

#define TEST_REPO_PATH "merge-resolve"
#define BRANCH_ID "7cb63eed597130ba4abb87b3e544b85021905520"

#define AUTOMERGEABLE_IDSTR "f2e1550a0c9e53d5811175864a29536642ae3821"

static git_repository *repo;
static git_index *repo_index;
static git_oid automergeable_id;

static void test_drivers_register(void);
static void test_drivers_unregister(void);

void test_merge_driver__initialize(void)
{
    git_config *cfg;

    repo = cl_git_sandbox_init(TEST_REPO_PATH);
    git_repository_index(&repo_index, repo);

	git_oid_fromstr(&automergeable_id, AUTOMERGEABLE_IDSTR);

    /* Ensure that the user's merge.conflictstyle doesn't interfere */
    cl_git_pass(git_repository_config(&cfg, repo));

    cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
    cl_git_pass(git_config_set_bool(cfg, "core.autocrlf", false));

	test_drivers_register();

    git_config_free(cfg);
}

void test_merge_driver__cleanup(void)
{
	test_drivers_unregister();

    git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

struct test_merge_driver {
	git_merge_driver base;
	int initialized;
	int shutdown;
};

static int test_driver_init(git_merge_driver *s)
{
	struct test_merge_driver *self = (struct test_merge_driver *)s;
	self->initialized = 1;
	return 0;
}

static void test_driver_shutdown(git_merge_driver *s)
{
	struct test_merge_driver *self = (struct test_merge_driver *)s;
	self->shutdown = 1;
}

static int test_driver_apply(
	git_merge_driver *s,
	const char **path_out,
	uint32_t *mode_out,
	git_buf *merged_out,
	const char *filter_name,
	const git_merge_driver_source *src)
{
	GIT_UNUSED(s);
	GIT_UNUSED(src);

	*path_out = "applied.txt";
	*mode_out = GIT_FILEMODE_BLOB;

	return git_buf_printf(merged_out, "This is the `%s` driver.\n",
		filter_name);
}

static struct test_merge_driver test_driver_custom = {
	{
		GIT_MERGE_DRIVER_VERSION,
		test_driver_init,
		test_driver_shutdown,
		test_driver_apply,
	},
	0,
	0,
};

static struct test_merge_driver test_driver_wildcard = {
	{
		GIT_MERGE_DRIVER_VERSION,
		test_driver_init,
		test_driver_shutdown,
		test_driver_apply,
	},
	0,
	0,
};

static void test_drivers_register(void)
{
	cl_git_pass(git_merge_driver_register("custom", &test_driver_custom.base));
	cl_git_pass(git_merge_driver_register("*", &test_driver_wildcard.base));
}

static void test_drivers_unregister(void)
{
	cl_git_pass(git_merge_driver_unregister("custom"));
	cl_git_pass(git_merge_driver_unregister("*"));
}

static void set_gitattributes_to(const char *driver)
{
	git_buf line = GIT_BUF_INIT;

	if (driver && strcmp(driver, ""))
		git_buf_printf(&line, "automergeable.txt merge=%s\n", driver);
	else if (driver)
		git_buf_printf(&line, "automergeable.txt merge\n");
	else
		git_buf_printf(&line, "automergeable.txt -merge\n");

	cl_assert(!git_buf_oom(&line));

	cl_git_mkfile(TEST_REPO_PATH "/.gitattributes", line.ptr);
	git_buf_dispose(&line);
}

static void merge_branch(void)
{
	git_oid their_id;
	git_annotated_commit *their_head;

	cl_git_pass(git_oid_fromstr(&their_id, BRANCH_ID));
	cl_git_pass(git_annotated_commit_lookup(&their_head, repo, &their_id));

	cl_git_pass(git_merge(repo, (const git_annotated_commit **)&their_head,
		1, NULL, NULL));

	git_annotated_commit_free(their_head);
}

void test_merge_driver__custom(void)
{
	const char *expected = "This is the `custom` driver.\n";
	set_gitattributes_to("custom");
	merge_branch();

	cl_assert_equal_file(expected, strlen(expected),
		TEST_REPO_PATH "/applied.txt");
}

void test_merge_driver__wildcard(void)
{
	const char *expected = "This is the `foobar` driver.\n";
	set_gitattributes_to("foobar");
	merge_branch();

	cl_assert_equal_file(expected, strlen(expected),
		TEST_REPO_PATH "/applied.txt");
}

void test_merge_driver__shutdown_is_called(void)
{
    test_driver_custom.initialized = 0;
    test_driver_custom.shutdown = 0;
    test_driver_wildcard.initialized = 0;
    test_driver_wildcard.shutdown = 0;
    
    /* run the merge with the custom driver */
    set_gitattributes_to("custom");
    merge_branch();
    
	/* unregister the drivers, ensure their shutdown function is called */
	test_drivers_unregister();

    /* since the `custom` driver was used, it should have been initialized and
     * shutdown, but the wildcard driver was not used at all and should not
     * have been initialized or shutdown.
     */
	cl_assert(test_driver_custom.initialized);
	cl_assert(test_driver_custom.shutdown);
	cl_assert(!test_driver_wildcard.initialized);
	cl_assert(!test_driver_wildcard.shutdown);

	test_drivers_register();
}

static int defer_driver_apply(
	git_merge_driver *s,
	const char **path_out,
	uint32_t *mode_out,
	git_buf *merged_out,
	const char *filter_name,
	const git_merge_driver_source *src)
{
	GIT_UNUSED(s);
	GIT_UNUSED(path_out);
	GIT_UNUSED(mode_out);
	GIT_UNUSED(merged_out);
	GIT_UNUSED(filter_name);
	GIT_UNUSED(src);

	return GIT_PASSTHROUGH;
}

static struct test_merge_driver test_driver_defer_apply = {
	{
		GIT_MERGE_DRIVER_VERSION,
		test_driver_init,
		test_driver_shutdown,
		defer_driver_apply,
	},
	0,
	0,
};

void test_merge_driver__apply_can_defer(void)
{
	const git_index_entry *idx;

	cl_git_pass(git_merge_driver_register("defer",
		&test_driver_defer_apply.base));

    set_gitattributes_to("defer");
    merge_branch();

	cl_assert((idx = git_index_get_bypath(repo_index, "automergeable.txt", 0)));
	cl_assert_equal_oid(&automergeable_id, &idx->id);

	git_merge_driver_unregister("defer");
}

static int conflict_driver_apply(
	git_merge_driver *s,
	const char **path_out,
	uint32_t *mode_out,
	git_buf *merged_out,
	const char *filter_name,
	const git_merge_driver_source *src)
{
	GIT_UNUSED(s);
	GIT_UNUSED(path_out);
	GIT_UNUSED(mode_out);
	GIT_UNUSED(merged_out);
	GIT_UNUSED(filter_name);
	GIT_UNUSED(src);

	return GIT_EMERGECONFLICT;
}

static struct test_merge_driver test_driver_conflict_apply = {
	{
		GIT_MERGE_DRIVER_VERSION,
		test_driver_init,
		test_driver_shutdown,
		conflict_driver_apply,
	},
	0,
	0,
};

void test_merge_driver__apply_can_conflict(void)
{
	const git_index_entry *ancestor, *ours, *theirs;

	cl_git_pass(git_merge_driver_register("conflict",
		&test_driver_conflict_apply.base));

    set_gitattributes_to("conflict");
    merge_branch();

	cl_git_pass(git_index_conflict_get(&ancestor, &ours, &theirs,
		repo_index, "automergeable.txt"));

	git_merge_driver_unregister("conflict");
}

void test_merge_driver__default_can_be_specified(void)
{
	git_oid their_id;
	git_annotated_commit *their_head;
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	const char *expected = "This is the `custom` driver.\n";

	merge_opts.default_driver = "custom";

	cl_git_pass(git_oid_fromstr(&their_id, BRANCH_ID));
	cl_git_pass(git_annotated_commit_lookup(&their_head, repo, &their_id));

	cl_git_pass(git_merge(repo, (const git_annotated_commit **)&their_head,
		1, &merge_opts, NULL));

	git_annotated_commit_free(their_head);

	cl_assert_equal_file(expected, strlen(expected),
		TEST_REPO_PATH "/applied.txt");
}

void test_merge_driver__honors_builtin_mergedefault(void)
{
	const git_index_entry *ancestor, *ours, *theirs;

	cl_repo_set_string(repo, "merge.default", "binary");
	merge_branch();

	cl_git_pass(git_index_conflict_get(&ancestor, &ours, &theirs,
		repo_index, "automergeable.txt"));
}

void test_merge_driver__honors_custom_mergedefault(void)
{
	const char *expected = "This is the `custom` driver.\n";

	cl_repo_set_string(repo, "merge.default", "custom");
	merge_branch();

	cl_assert_equal_file(expected, strlen(expected),
		TEST_REPO_PATH "/applied.txt");
}

void test_merge_driver__mergedefault_deferring_falls_back_to_text(void)
{
	const git_index_entry *idx;

	cl_git_pass(git_merge_driver_register("defer",
		&test_driver_defer_apply.base));

	cl_repo_set_string(repo, "merge.default", "defer");
	merge_branch();

	cl_assert((idx = git_index_get_bypath(repo_index, "automergeable.txt", 0)));
	cl_assert_equal_oid(&automergeable_id, &idx->id);

	git_merge_driver_unregister("defer");
}

void test_merge_driver__set_forces_text(void)
{
	const git_index_entry *idx;

	/* `merge` without specifying a driver indicates `text` */
	set_gitattributes_to("");
	cl_repo_set_string(repo, "merge.default", "custom");

	merge_branch();

	cl_assert((idx = git_index_get_bypath(repo_index, "automergeable.txt", 0)));
	cl_assert_equal_oid(&automergeable_id, &idx->id);
}

void test_merge_driver__unset_forces_binary(void)
{
	const git_index_entry *ancestor, *ours, *theirs;

	/* `-merge` without specifying a driver indicates `binary` */
	set_gitattributes_to(NULL);
	cl_repo_set_string(repo, "merge.default", "custom");

	merge_branch();

	cl_git_pass(git_index_conflict_get(&ancestor, &ours, &theirs,
		repo_index, "automergeable.txt"));
}

void test_merge_driver__not_configured_driver_falls_back(void)
{
	const git_index_entry *idx;

	test_drivers_unregister();

	/* `merge` without specifying a driver indicates `text` */
	set_gitattributes_to("notfound");

	merge_branch();

	cl_assert((idx = git_index_get_bypath(repo_index, "automergeable.txt", 0)));
	cl_assert_equal_oid(&automergeable_id, &idx->id);

	test_drivers_register();
}

