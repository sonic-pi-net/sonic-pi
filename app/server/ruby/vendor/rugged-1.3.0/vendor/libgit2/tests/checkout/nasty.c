#include "clar_libgit2.h"
#include "checkout_helpers.h"

#include "git2/checkout.h"
#include "repository.h"
#include "buffer.h"
#include "futils.h"

static const char *repo_name = "nasty";
static git_repository *repo;
static git_checkout_options checkout_opts;

void test_checkout_nasty__initialize(void)
{
	repo = cl_git_sandbox_init(repo_name);

	GIT_INIT_STRUCTURE(&checkout_opts, GIT_CHECKOUT_OPTIONS_VERSION);
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
}

void test_checkout_nasty__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void test_checkout_passes(const char *refname, const char *filename)
{
	git_oid commit_id;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&path, repo_name, filename));

	cl_git_pass(git_reference_name_to_id(&commit_id, repo, refname));
	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE |
		GIT_CHECKOUT_DONT_UPDATE_INDEX;

	cl_git_pass(git_checkout_tree(repo, (const git_object *)commit, &opts));
	cl_assert(!git_path_exists(path.ptr));

	git_commit_free(commit);
	git_buf_dispose(&path);
}

static void test_checkout_fails(const char *refname, const char *filename)
{
	git_oid commit_id;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&path, repo_name, filename));

	cl_git_pass(git_reference_name_to_id(&commit_id, repo, refname));
	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_fail(git_checkout_tree(repo, (const git_object *)commit, &opts));
	cl_assert(!git_path_exists(path.ptr));

	git_commit_free(commit);
	git_buf_dispose(&path);
}

/* A tree that contains ".git" as a tree, with a blob inside
 * (".git/foobar").
 */
void test_checkout_nasty__dotgit_tree(void)
{
	test_checkout_fails("refs/heads/dotgit_tree", ".git/foobar");
}

/* A tree that contains ".GIT" as a tree, with a blob inside
 * (".GIT/foobar").
 */
void test_checkout_nasty__dotcapitalgit_tree(void)
{
	test_checkout_fails("refs/heads/dotcapitalgit_tree", ".GIT/foobar");
}

/* A tree that contains a tree ".", with a blob inside ("./foobar").
 */
void test_checkout_nasty__dot_tree(void)
{
	test_checkout_fails("refs/heads/dot_tree", "foobar");
}

/* A tree that contains a tree ".", with a tree ".git", with a blob
 * inside ("./.git/foobar").
 */
void test_checkout_nasty__dot_dotgit_tree(void)
{
	test_checkout_fails("refs/heads/dot_dotgit_tree", ".git/foobar");
}

/* A tree that contains a tree, with a tree "..", with a tree ".git", with a
 * blob inside ("foo/../.git/foobar").
 */
void test_checkout_nasty__dotdot_dotgit_tree(void)
{
	test_checkout_fails("refs/heads/dotdot_dotgit_tree", ".git/foobar");
}

/* A tree that contains a tree, with a tree "..", with a blob inside
 * ("foo/../foobar").
 */
void test_checkout_nasty__dotdot_tree(void)
{
	test_checkout_fails("refs/heads/dotdot_tree", "foobar");
}

/* A tree that contains a blob with the rogue name ".git/foobar" */
void test_checkout_nasty__dotgit_path(void)
{
	test_checkout_fails("refs/heads/dotgit_path", ".git/foobar");
}

/* A tree that contains a blob with the rogue name ".GIT/foobar" */
void test_checkout_nasty__dotcapitalgit_path(void)
{
	test_checkout_fails("refs/heads/dotcapitalgit_path", ".GIT/foobar");
}

/* A tree that contains a blob with the rogue name "./.git/foobar" */
void test_checkout_nasty__dot_dotgit_path(void)
{
	test_checkout_fails("refs/heads/dot_dotgit_path", ".git/foobar");
}

/* A tree that contains a blob with the rogue name "./.GIT/foobar" */
void test_checkout_nasty__dot_dotcapitalgit_path(void)
{
	test_checkout_fails("refs/heads/dot_dotcapitalgit_path", ".GIT/foobar");
}

/* A tree that contains a blob with the rogue name "foo/../.git/foobar" */
void test_checkout_nasty__dotdot_dotgit_path(void)
{
	test_checkout_fails("refs/heads/dotdot_dotgit_path", ".git/foobar");
}

/* A tree that contains a blob with the rogue name "foo/../.GIT/foobar" */
void test_checkout_nasty__dotdot_dotcapitalgit_path(void)
{
	test_checkout_fails("refs/heads/dotdot_dotcapitalgit_path", ".GIT/foobar");
}

/* A tree that contains a blob with the rogue name "foo/." */
void test_checkout_nasty__dot_path(void)
{
	test_checkout_fails("refs/heads/dot_path", "./foobar");
}

/* A tree that contains a blob with the rogue name "foo/." */
void test_checkout_nasty__dot_path_two(void)
{
	test_checkout_fails("refs/heads/dot_path_two", "foo/.");
}

/* A tree that contains a blob with the rogue name "foo/../foobar" */
void test_checkout_nasty__dotdot_path(void)
{
	test_checkout_fails("refs/heads/dotdot_path", "foobar");
}

/* A tree that contains an entry with a backslash ".git\foobar"  */
void test_checkout_nasty__dotgit_backslash_path(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dotgit_backslash_path", ".git/foobar");
#endif
}

/* A tree that contains an entry with a backslash ".GIT\foobar"  */
void test_checkout_nasty__dotcapitalgit_backslash_path(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dotcapitalgit_backslash_path", ".GIT/foobar");
#endif
}

/* A tree that contains an entry with a backslash ".\.GIT\foobar"  */
void test_checkout_nasty__dot_backslash_dotcapitalgit_path(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dot_backslash_dotcapitalgit_path", ".GIT/foobar");
#endif
}

/* A tree that contains an entry ".git.", because Win32 APIs will drop the
 * trailing slash.
 */
void test_checkout_nasty__dot_git_dot(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dot_git_dot", ".git/foobar");
#endif
}

/* A tree that contains an entry "git~1", because that is typically the
 * short name for ".git".
 */
void test_checkout_nasty__git_tilde1(void)
{
	test_checkout_fails("refs/heads/git_tilde1", ".git/foobar");
	test_checkout_fails("refs/heads/git_tilde1", "git~1/foobar");
}

/* A tree that contains an entry "git~2", when we have forced the short
 * name for ".git" into "GIT~2".
 */
void test_checkout_nasty__git_custom_shortname(void)
{
#ifdef GIT_WIN32
	if (!cl_sandbox_supports_8dot3())
		clar__skip();

	cl_must_pass(p_rename("nasty/.git", "nasty/_temp"));
	cl_git_write2file("nasty/git~1", "", 0, O_RDWR|O_CREAT, 0666);
	cl_must_pass(p_rename("nasty/_temp", "nasty/.git"));
	test_checkout_fails("refs/heads/git_tilde2", ".git/foobar");
#endif
}

/* A tree that contains an entry "git~3", which should be allowed, since
 * it is not the typical short name ("GIT~1") or the actual short name
 * ("GIT~2") for ".git".
 */
void test_checkout_nasty__only_looks_like_a_git_shortname(void)
{
#ifdef GIT_WIN32
	git_oid commit_id;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_must_pass(p_rename("nasty/.git", "nasty/_temp"));
	cl_git_write2file("nasty/git~1", "", 0, O_RDWR|O_CREAT, 0666);
	cl_must_pass(p_rename("nasty/_temp", "nasty/.git"));

	cl_git_pass(git_reference_name_to_id(&commit_id, repo, "refs/heads/git_tilde3"));
	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_checkout_tree(repo, (const git_object *)commit, &opts));
	cl_assert(git_path_exists("nasty/git~3/foobar"));

	git_commit_free(commit);
#endif
}

/* A tree that contains an entry "git:", because Win32 APIs will reject
 * that as looking too similar to a drive letter.
 */
void test_checkout_nasty__dot_git_colon(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dot_git_colon", ".git/foobar");
#endif
}

/* A tree that contains an entry "git:foo", because Win32 APIs will turn
 * that into ".git".
 */
void test_checkout_nasty__dot_git_colon_stuff(void)
{
#ifdef GIT_WIN32
	test_checkout_fails("refs/heads/dot_git_colon_stuff", ".git/foobar");
#endif
}

/* A tree that contains an entry ".git::$INDEX_ALLOCATION" because NTFS
 * will interpret that as a synonym to ".git", even when mounted via SMB
 * on macOS.
 */
void test_checkout_nasty__dotgit_alternate_data_stream(void)
{
	test_checkout_fails("refs/heads/dotgit_alternate_data_stream", ".git/dummy-file");
	test_checkout_fails("refs/heads/dotgit_alternate_data_stream", ".git::$INDEX_ALLOCATION/dummy-file");
}

/* Trees that contains entries with a tree ".git" that contain
 * byte sequences:
 * { 0xe2, 0x80, 0x8c }
 * { 0xe2, 0x80, 0x8d }
 * { 0xe2, 0x80, 0x8e }
 * { 0xe2, 0x80, 0x8f }
 * { 0xe2, 0x80, 0xaa }
 * { 0xe2, 0x80, 0xab }
 * { 0xe2, 0x80, 0xac }
 * { 0xe2, 0x80, 0xad }
 * { 0xe2, 0x81, 0xae }
 * { 0xe2, 0x81, 0xaa }
 * { 0xe2, 0x81, 0xab }
 * { 0xe2, 0x81, 0xac }
 * { 0xe2, 0x81, 0xad }
 * { 0xe2, 0x81, 0xae }
 * { 0xe2, 0x81, 0xaf }
 * { 0xef, 0xbb, 0xbf }
 * Because these map to characters that HFS filesystems "ignore".  Thus
 * ".git<U+200C>" will map to ".git".
 */
void test_checkout_nasty__dot_git_hfs_ignorable(void)
{
#ifdef __APPLE__
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_1", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_2", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_3", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_4", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_5", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_6", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_7", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_8", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_9", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_10", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_11", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_12", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_13", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_14", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_15", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_16", ".git/foobar");
#endif
}

void test_checkout_nasty__honors_core_protecthfs(void)
{
	cl_repo_set_bool(repo, "core.protectHFS", true);

	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_1", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_2", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_3", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_4", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_5", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_6", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_7", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_8", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_9", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_10", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_11", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_12", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_13", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_14", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_15", ".git/foobar");
	test_checkout_fails("refs/heads/dotgit_hfs_ignorable_16", ".git/foobar");
}

void test_checkout_nasty__honors_core_protectntfs(void)
{
	cl_repo_set_bool(repo, "core.protectNTFS", true);

	test_checkout_fails("refs/heads/dotgit_backslash_path", ".git/foobar");
	test_checkout_fails("refs/heads/dotcapitalgit_backslash_path", ".GIT/foobar");
	test_checkout_fails("refs/heads/dot_git_dot", ".git/foobar");
	test_checkout_fails("refs/heads/git_tilde1", ".git/foobar");
}

void test_checkout_nasty__symlink1(void)
{
	test_checkout_passes("refs/heads/symlink1", ".git/foobar");
}

void test_checkout_nasty__symlink2(void)
{
	test_checkout_passes("refs/heads/symlink2", ".git/foobar");
}

void test_checkout_nasty__symlink3(void)
{
	test_checkout_passes("refs/heads/symlink3", ".git/foobar");
}

void test_checkout_nasty__gitmodules_symlink(void)
{
	cl_repo_set_bool(repo, "core.protectHFS", true);
	test_checkout_fails("refs/heads/gitmodules-symlink", ".gitmodules");
	cl_repo_set_bool(repo, "core.protectHFS", false);

	cl_repo_set_bool(repo, "core.protectNTFS", true);
	test_checkout_fails("refs/heads/gitmodules-symlink", ".gitmodules");
	cl_repo_set_bool(repo, "core.protectNTFS", false);

	test_checkout_fails("refs/heads/gitmodules-symlink", ".gitmodules");
}
