#include "clar.h"
#include "clar_libgit2.h"

#include "commit.h"
#include "diff.h"
#include "diff_generate.h"
#include "diff_helpers.h"

static git_repository *_repo;
static git_diff_stats *_stats;

void test_diff_stats__initialize(void)
{
	_repo = cl_git_sandbox_init("diff_format_email");
}

void test_diff_stats__cleanup(void)
{
	git_diff_stats_free(_stats); _stats = NULL;
	cl_git_sandbox_cleanup();
}

static void diff_stats_from_commit_oid(
	git_diff_stats **stats, const char *oidstr, bool rename)
{
	git_oid oid;
	git_commit *commit;
	git_diff *diff;

	git_oid__fromstr(&oid, oidstr, GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&commit, _repo, &oid));
	cl_git_pass(git_diff__commit(&diff, _repo, commit, NULL));
	if (rename)
		cl_git_pass(git_diff_find_similar(diff, NULL));
	cl_git_pass(git_diff_get_stats(stats, diff));

	git_diff_free(diff);
	git_commit_free(commit);
}

void test_diff_stats__stat(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file1.txt | 8 +++++---\n" \
	" 1 file changed, 5 insertions(+), 3 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", false);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(5, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(3, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert(strcmp(buf.ptr, stat) == 0);
	git_buf_dispose(&buf);

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 80));
	cl_assert(strcmp(buf.ptr, stat) == 0);
	git_buf_dispose(&buf);
}

void test_diff_stats__multiple_hunks(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt | 5 +++--\n" \
	" file3.txt | 6 ++++--\n" \
	" 2 files changed, 7 insertions(+), 4 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "cd471f0d8770371e1bc78bcbb38db4c7e4106bd2", false);

	cl_assert_equal_sz(2, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(7, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(4, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__numstat(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	"3       2       file2.txt\n"
	"4       2       file3.txt\n";

	diff_stats_from_commit_oid(
		&_stats, "cd471f0d8770371e1bc78bcbb38db4c7e4106bd2", false);

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_NUMBER, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__shortstat(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" 1 file changed, 5 insertions(+), 3 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", false);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(5, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(3, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_SHORT, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__shortstat_noinsertions(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" 1 file changed, 2 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "06b7b69a62cbd1e53c6c4e0c3f16473dcfdb4af6", false);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(0, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(2, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_SHORT, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__shortstat_nodeletions(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" 1 file changed, 3 insertions(+)\n";

	diff_stats_from_commit_oid(
		&_stats, "5219b9784f9a92d7bd7cb567a6d6a21bfb86697e", false);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(3, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(0, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_SHORT, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt => file2.txt.renamed | 1 +\n"
	" file3.txt => file3.txt.renamed | 4 +++-\n"
	" 2 files changed, 4 insertions(+), 1 deletion(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "8947a46e2097638ca6040ad4877246f4186ec3bd", true);

	cl_assert_equal_sz(2, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(4, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(1, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_nochanges(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt.renamed => file2.txt.renamed2 | 0\n"
	" file3.txt.renamed => file3.txt.renamed2 | 0\n"
	" 2 files changed, 0 insertions(+), 0 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "3991dce9e71a0641ca49a6a4eea6c9e7ff402ed4", true);

	cl_assert_equal_sz(2, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(0, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(0, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_and_modifiy(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt.renamed2                      | 2 +-\n"
	" file3.txt.renamed2 => file3.txt.renamed | 0\n"
	" 2 files changed, 1 insertion(+), 1 deletion(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "4ca10087e696d2ba78d07b146a118e9a7096ed4f", true);

	cl_assert_equal_sz(2, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(1, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(1, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_in_subdirectory(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" dir/{orig.txt => renamed.txt} | 0\n"
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "0db2a262bc8c5c3cba55254730045a8258da7a37", true);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(0, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(0, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_no_find(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt         | 5 -----\n"
	" file2.txt.renamed | 6 ++++++\n"
	" file3.txt         | 5 -----\n"
	" file3.txt.renamed | 7 +++++++\n"
	" 4 files changed, 13 insertions(+), 10 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "8947a46e2097638ca6040ad4877246f4186ec3bd", false);

	cl_assert_equal_sz(4, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(13, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(10, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_nochanges_no_find(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt.renamed  | 6 ------\n"
	" file2.txt.renamed2 | 6 ++++++\n"
	" file3.txt.renamed  | 7 -------\n"
	" file3.txt.renamed2 | 7 +++++++\n"
	" 4 files changed, 13 insertions(+), 13 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "3991dce9e71a0641ca49a6a4eea6c9e7ff402ed4", false);

	cl_assert_equal_sz(4, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(13, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(13, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__rename_and_modify_no_find(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file2.txt.renamed2 | 2 +-\n"
	" file3.txt.renamed  | 7 +++++++\n"
	" file3.txt.renamed2 | 7 -------\n"
	" 3 files changed, 8 insertions(+), 8 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "4ca10087e696d2ba78d07b146a118e9a7096ed4f", false);

	cl_assert_equal_sz(3, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(8, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(8, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__binary(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" binary.bin | Bin 3 -> 5 bytes\n"
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n";

	diff_stats_from_commit_oid(
		&_stats, "8d7523f6fcb2404257889abe0d96f093d9f524f9", false);

	cl_assert_equal_sz(1, git_diff_stats_files_changed(_stats));
	cl_assert_equal_sz(0, git_diff_stats_insertions(_stats));
	cl_assert_equal_sz(0, git_diff_stats_deletions(_stats));

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__binary_numstat(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	"-       -       binary.bin\n";

	diff_stats_from_commit_oid(
		&_stats, "8d7523f6fcb2404257889abe0d96f093d9f524f9", false);

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_NUMBER, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__mode_change(void)
{
	git_buf buf = GIT_BUF_INIT;
	const char *stat =
	" file1.txt.renamed | 0\n" \
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n" \
		" mode change 100644 => 100755 file1.txt.renamed\n";

	diff_stats_from_commit_oid(
		&_stats, "7ade76dd34bba4733cf9878079f9fd4a456a9189", false);

	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL | GIT_DIFF_STATS_INCLUDE_SUMMARY, 0));
	cl_assert_equal_s(stat, buf.ptr);
	git_buf_dispose(&buf);
}

void test_diff_stats__new_file(void)
{
	git_diff *diff;
	git_buf buf = GIT_BUF_INIT;

	const char *input =
	"---\n"
	" Gurjeet Singh | 1 +\n"
	" 1 file changed, 1 insertion(+)\n"
	" create mode 100644 Gurjeet Singh\n"
	"\n"
	"diff --git a/Gurjeet Singh b/Gurjeet Singh\n"
	"new file mode 100644\n"
	"index 0000000..6d0ecfd\n"
	"--- /dev/null\n"
	"+++ b/Gurjeet Singh	\n"
	"@@ -0,0 +1 @@\n"
	"+I'm about to try git send-email\n"
	"-- \n"
	"2.21.0\n";

	const char *stat =
	" Gurjeet Singh | 1 +\n"
	" 1 file changed, 1 insertion(+)\n"
	" create mode 100644 Gurjeet Singh\n";

	cl_git_pass(diff_from_buffer(&diff, input, strlen(input)));
	cl_git_pass(git_diff_get_stats(&_stats, diff));
	cl_git_pass(git_diff_stats_to_buf(&buf, _stats, GIT_DIFF_STATS_FULL | GIT_DIFF_STATS_INCLUDE_SUMMARY, 0));
	cl_assert_equal_s(stat, buf.ptr);

	git_buf_dispose(&buf);
	git_diff_free(diff);
}
