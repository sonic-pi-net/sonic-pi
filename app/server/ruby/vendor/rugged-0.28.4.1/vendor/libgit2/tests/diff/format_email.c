#include "clar.h"
#include "clar_libgit2.h"

#include "buffer.h"
#include "commit.h"
#include "diff.h"
#include "diff_generate.h"

static git_repository *repo;

void test_diff_format_email__initialize(void)
{
	repo = cl_git_sandbox_init("diff_format_email");
}

void test_diff_format_email__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_email_match(
	const char *expected,
	const char *oidstr,
	git_diff_format_email_options *opts)
{
	git_oid oid;
	git_commit *commit = NULL;
	git_diff *diff = NULL;
	git_buf buf = GIT_BUF_INIT;

	git_oid_fromstr(&oid, oidstr);

	cl_git_pass(git_commit_lookup(&commit, repo, &oid));

	opts->id = git_commit_id(commit);
	opts->author = git_commit_author(commit);
	if (!opts->summary)
		opts->summary = git_commit_summary(commit);

	cl_git_pass(git_diff__commit(&diff, repo, commit, NULL));
	cl_git_pass(git_diff_format_email(&buf, diff, opts));

	cl_assert_equal_s(expected, git_buf_cstr(&buf));
	git_buf_clear(&buf);

	cl_git_pass(git_diff_commit_as_email(
		&buf, repo, commit, 1, 1, opts->flags, NULL));
	cl_assert_equal_s(expected, git_buf_cstr(&buf));

	git_diff_free(diff);
	git_commit_free(commit);
	git_buf_dispose(&buf);
}

void test_diff_format_email__simple(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Wed, 9 Apr 2014 20:57:01 +0200\n" \
	"Subject: [PATCH] Modify some content\n" \
	"\n" \
	"---\n" \
	" file1.txt | 8 +++++---\n" \
	" 1 file changed, 5 insertions(+), 3 deletions(-)\n" \
	"\n" \
	"diff --git a/file1.txt b/file1.txt\n" \
	"index 94aaae8..af8f41d 100644\n" \
	"--- a/file1.txt\n" \
	"+++ b/file1.txt\n" \
	"@@ -1,15 +1,17 @@\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+\n" \
	"+\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"+_file1.txt_\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	assert_email_match(
		email, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);
}

void test_diff_format_email__with_message(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email = "From 627e7e12d87e07a83fad5b6bfa25e86ead4a5270 Mon Sep 17 00:00:00 2001\n" \
	"From: Patrick Steinhardt <ps@pks.im>\n" \
	"Date: Tue, 24 Nov 2015 13:34:39 +0100\n" \
	"Subject: [PATCH] Modify content with message\n" \
	"\n" \
	"Modify content of file3.txt by appending a new line. Make this\n" \
	"commit message somewhat longer to test behavior with newlines\n" \
	"embedded in the message body.\n" \
	"\n" \
	"Also test if new paragraphs are included correctly.\n" \
	"---\n" \
	" file3.txt | 1 +\n" \
	" 1 file changed, 1 insertion(+)\n" \
	"\n" \
	"diff --git a/file3.txt b/file3.txt\n" \
	"index 9a2d780..7309653 100644\n" \
	"--- a/file3.txt\n" \
	"+++ b/file3.txt\n" \
	"@@ -3,3 +3,4 @@ file3!\n" \
	" file3\n" \
	" file3\n" \
	" file3\n" \
	"+file3\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	opts.body = "Modify content of file3.txt by appending a new line. Make this\n" \
	"commit message somewhat longer to test behavior with newlines\n" \
	"embedded in the message body.\n" \
	"\n" \
	"Also test if new paragraphs are included correctly.";

	assert_email_match(
		email, "627e7e12d87e07a83fad5b6bfa25e86ead4a5270", &opts);
}


void test_diff_format_email__multiple(void)
{
	git_oid oid;
	git_commit *commit = NULL;
	git_diff *diff = NULL;
 	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	git_buf buf = GIT_BUF_INIT;

	const char *email =
	"From 10808fe9c9be5a190c0ba68d1a002233fb363508 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Thu, 10 Apr 2014 19:37:05 +0200\n" \
	"Subject: [PATCH 1/2] Added file2.txt file3.txt\n" \
	"\n" \
	"---\n" \
	" file2.txt | 5 +++++\n" \
	" file3.txt | 5 +++++\n" \
	" 2 files changed, 10 insertions(+)\n" \
	" create mode 100644 file2.txt\n" \
	" create mode 100644 file3.txt\n" \
	"\n" \
	"diff --git a/file2.txt b/file2.txt\n" \
	"new file mode 100644\n" \
	"index 0000000..e909123\n" \
	"--- /dev/null\n" \
	"+++ b/file2.txt\n" \
	"@@ -0,0 +1,5 @@\n" \
	"+file2\n" \
	"+file2\n" \
	"+file2\n" \
	"+file2\n" \
	"+file2\n" \
	"diff --git a/file3.txt b/file3.txt\n" \
	"new file mode 100644\n" \
	"index 0000000..9435022\n" \
	"--- /dev/null\n" \
	"+++ b/file3.txt\n" \
	"@@ -0,0 +1,5 @@\n" \
	"+file3\n" \
	"+file3\n" \
	"+file3\n" \
	"+file3\n" \
	"+file3\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n" \
	"From 873806f6f27e631eb0b23e4b56bea2bfac14a373 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Thu, 10 Apr 2014 19:37:36 +0200\n" \
	"Subject: [PATCH 2/2] Modified file2.txt, file3.txt\n" \
	"\n" \
	"---\n" \
	" file2.txt | 2 +-\n" \
	" file3.txt | 2 +-\n" \
	" 2 files changed, 2 insertions(+), 2 deletions(-)\n" \
	"\n" \
	"diff --git a/file2.txt b/file2.txt\n" \
	"index e909123..7aff11d 100644\n" \
	"--- a/file2.txt\n" \
	"+++ b/file2.txt\n" \
	"@@ -1,5 +1,5 @@\n" \
	" file2\n" \
	" file2\n" \
	" file2\n" \
	"-file2\n" \
	"+file2!\n" \
	" file2\n" \
	"diff --git a/file3.txt b/file3.txt\n" \
	"index 9435022..9a2d780 100644\n" \
	"--- a/file3.txt\n" \
	"+++ b/file3.txt\n" \
	"@@ -1,5 +1,5 @@\n" \
	" file3\n" \
	"-file3\n" \
	"+file3!\n" \
	" file3\n" \
	" file3\n" \
	" file3\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";


	git_oid_fromstr(&oid, "10808fe9c9be5a190c0ba68d1a002233fb363508");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));

	opts.id = git_commit_id(commit);
	opts.author = git_commit_author(commit);
	opts.summary = git_commit_summary(commit);
	opts.patch_no = 1;
	opts.total_patches = 2;

	cl_git_pass(git_diff__commit(&diff, repo, commit, NULL));
	cl_git_pass(git_diff_format_email(&buf, diff, &opts));

	git_diff_free(diff);
	git_commit_free(commit);
	diff = NULL;
	commit = NULL;

	git_oid_fromstr(&oid, "873806f6f27e631eb0b23e4b56bea2bfac14a373");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));

	opts.id = git_commit_id(commit);
	opts.author = git_commit_author(commit);
	opts.summary = git_commit_summary(commit);
	opts.patch_no = 2;
	opts.total_patches = 2;

	cl_git_pass(git_diff__commit(&diff, repo, commit, NULL));
	cl_git_pass(git_diff_format_email(&buf, diff, &opts));

	cl_assert_equal_s(email, git_buf_cstr(&buf));

	git_diff_free(diff);
	git_commit_free(commit);
	git_buf_dispose(&buf);
}

void test_diff_format_email__exclude_marker(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Wed, 9 Apr 2014 20:57:01 +0200\n" \
	"Subject: Modify some content\n" \
	"\n" \
	"---\n" \
	" file1.txt | 8 +++++---\n" \
	" 1 file changed, 5 insertions(+), 3 deletions(-)\n" \
	"\n" \
	"diff --git a/file1.txt b/file1.txt\n" \
	"index 94aaae8..af8f41d 100644\n" \
	"--- a/file1.txt\n" \
	"+++ b/file1.txt\n" \
	"@@ -1,15 +1,17 @@\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+\n" \
	"+\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"+_file1.txt_\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	opts.flags |= GIT_DIFF_FORMAT_EMAIL_EXCLUDE_SUBJECT_PATCH_MARKER;

	assert_email_match(
		email, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);
}

void test_diff_format_email__invalid_no(void)
{
	git_oid oid;
	git_commit *commit = NULL;
	git_diff *diff = NULL;
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	git_buf buf = GIT_BUF_INIT;

	git_oid_fromstr(&oid, "9264b96c6d104d0e07ae33d3007b6a48246c6f92");

	cl_git_pass(git_commit_lookup(&commit, repo, &oid));

	opts.id = git_commit_id(commit);
	opts.author = git_commit_author(commit);
	opts.summary = git_commit_summary(commit);
	opts.patch_no = 2;
	opts.total_patches = 1;

	cl_git_pass(git_diff__commit(&diff, repo, commit, NULL));
	cl_git_fail(git_diff_format_email(&buf, diff, &opts));
	cl_git_fail(git_diff_commit_as_email(&buf, repo, commit, 2, 1, 0, NULL));
	cl_git_fail(git_diff_commit_as_email(&buf, repo, commit, 0, 0, 0, NULL));

	git_diff_free(diff);
	git_commit_free(commit);
	git_buf_dispose(&buf);
}

void test_diff_format_email__mode_change(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 7ade76dd34bba4733cf9878079f9fd4a456a9189 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Thu, 10 Apr 2014 10:05:03 +0200\n" \
	"Subject: [PATCH] Update permissions\n" \
	"\n" \
	"---\n" \
	" file1.txt.renamed | 0\n" \
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n" \
	" mode change 100644 => 100755 file1.txt.renamed\n" \
	"\n" \
	"diff --git a/file1.txt.renamed b/file1.txt.renamed\n" \
	"old mode 100644\n" \
	"new mode 100755\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	assert_email_match(
		email, "7ade76dd34bba4733cf9878079f9fd4a456a9189", &opts);
}

void test_diff_format_email__rename_add_remove(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 6e05acc5a5dab507d91a0a0cc0fb05a3dd98892d Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Wed, 9 Apr 2014 21:15:56 +0200\n" \
	"Subject: [PATCH] Renamed file1.txt -> file1.txt.renamed\n" \
	"\n" \
	"---\n" \
	" file1.txt         | 17 -----------------\n" \
	" file1.txt.renamed | 17 +++++++++++++++++\n" \
	" 2 files changed, 17 insertions(+), 17 deletions(-)\n" \
	" delete mode 100644 file1.txt\n" \
	" create mode 100644 file1.txt.renamed\n" \
	"\n" \
	"diff --git a/file1.txt b/file1.txt\n" \
	"deleted file mode 100644\n" \
	"index af8f41d..0000000\n" \
	"--- a/file1.txt\n" \
	"+++ /dev/null\n" \
	"@@ -1,17 +0,0 @@\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-_file1.txt_\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-\n" \
	"-\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-_file1.txt_\n" \
	"-_file1.txt_\n" \
	"-file1.txt\n" \
	"diff --git a/file1.txt.renamed b/file1.txt.renamed\n" \
	"new file mode 100644\n" \
	"index 0000000..a97157a\n" \
	"--- /dev/null\n" \
	"+++ b/file1.txt.renamed\n" \
	"@@ -0,0 +1,17 @@\n" \
	"+file1.txt\n" \
	"+file1.txt\n" \
	"+_file1.txt_\n" \
	"+file1.txt\n" \
	"+file1.txt\n" \
	"+file1.txt_renamed\n" \
	"+file1.txt\n" \
	"+\n" \
	"+\n" \
	"+file1.txt\n" \
	"+file1.txt\n" \
	"+file1.txt_renamed\n" \
	"+file1.txt\n" \
	"+file1.txt\n" \
	"+_file1.txt_\n" \
	"+_file1.txt_\n" \
	"+file1.txt\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	assert_email_match(
		email, "6e05acc5a5dab507d91a0a0cc0fb05a3dd98892d", &opts);
}

void test_diff_format_email__multiline_summary(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 9264b96c6d104d0e07ae33d3007b6a48246c6f92 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Wed, 9 Apr 2014 20:57:01 +0200\n" \
	"Subject: [PATCH] Modify some content\n" \
	"\n" \
	"---\n" \
	" file1.txt | 8 +++++---\n" \
	" 1 file changed, 5 insertions(+), 3 deletions(-)\n" \
	"\n" \
	"diff --git a/file1.txt b/file1.txt\n" \
	"index 94aaae8..af8f41d 100644\n" \
	"--- a/file1.txt\n" \
	"+++ b/file1.txt\n" \
	"@@ -1,15 +1,17 @@\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"+\n" \
	"+\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"-file1.txt\n" \
	"+_file1.txt_\n" \
	"+_file1.txt_\n" \
	" file1.txt\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	opts.summary = "Modify some content\nSome extra stuff here";

	assert_email_match(
		email, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);
}

void test_diff_format_email__binary(void)
{
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	const char *email =
	"From 8d7523f6fcb2404257889abe0d96f093d9f524f9 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Sun, 13 Apr 2014 18:10:18 +0200\n" \
	"Subject: [PATCH] Modified binary file\n" \
	"\n" \
	"---\n" \
	" binary.bin | Bin 3 -> 0 bytes\n" \
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n" \
	"\n" \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index bd474b2..9ac35ff 100644\n" \
	"Binary files a/binary.bin and b/binary.bin differ\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";
	/* TODO: Actually 0 bytes here should be 5!. Seems like we don't load the new content for binary files? */

	opts.summary = "Modified binary file";

	assert_email_match(
		email, "8d7523f6fcb2404257889abe0d96f093d9f524f9", &opts);
}

