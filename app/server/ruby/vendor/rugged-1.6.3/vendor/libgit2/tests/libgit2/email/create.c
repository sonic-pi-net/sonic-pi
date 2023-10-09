#include "clar.h"
#include "clar_libgit2.h"

#include "diff_generate.h"

static git_repository *repo;

void test_email_create__initialize(void)
{
	repo = cl_git_sandbox_init("diff_format_email");
}

void test_email_create__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void email_for_commit(
	git_buf *out,
	const char *commit_id,
	git_email_create_options *opts)
{
	git_oid oid;
	git_commit *commit = NULL;
	git_diff *diff = NULL;

	git_oid__fromstr(&oid, commit_id, GIT_OID_SHA1);

	cl_git_pass(git_commit_lookup(&commit, repo, &oid));

	cl_git_pass(git_email_create_from_commit(out, commit, opts));

	git_diff_free(diff);
	git_commit_free(commit);
}

static void assert_email_match(
	const char *expected,
	const char *commit_id,
	git_email_create_options *opts)
{
	git_buf buf = GIT_BUF_INIT;

	email_for_commit(&buf, commit_id, opts);
	cl_assert_equal_s(expected, buf.ptr);

	git_buf_dispose(&buf);
}

static void assert_subject_match(
	const char *expected,
	const char *commit_id,
	git_email_create_options *opts)
{
	git_buf buf = GIT_BUF_INIT;
	char *subject, *nl;

	email_for_commit(&buf, commit_id, opts);

	cl_assert((subject = strstr(buf.ptr, "\nSubject: ")) != NULL);
	subject += 10;

	if ((nl = strchr(subject, '\n')) != NULL)
		*nl = '\0';

	cl_assert_equal_s(expected, subject);

	git_buf_dispose(&buf);
}

void test_email_create__commit(void)
{
	const char *expected =
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
		expected, "9264b96c6d104d0e07ae33d3007b6a48246c6f92", NULL);
}

void test_email_create__rename(void)
{
	const char *expected =
	"From 6e05acc5a5dab507d91a0a0cc0fb05a3dd98892d Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Wed, 9 Apr 2014 21:15:56 +0200\n" \
	"Subject: [PATCH] Renamed file1.txt -> file1.txt.renamed\n" \
	"\n" \
	"---\n" \
	" file1.txt => file1.txt.renamed | 4 ++--\n" \
	" 1 file changed, 2 insertions(+), 2 deletions(-)\n" \
	"\n" \
	"diff --git a/file1.txt b/file1.txt.renamed\n" \
	"similarity index 86%\n" \
	"rename from file1.txt\n" \
	"rename to file1.txt.renamed\n" \
	"index af8f41d..a97157a 100644\n" \
	"--- a/file1.txt\n" \
	"+++ b/file1.txt.renamed\n" \
	"@@ -3,13 +3,13 @@ file1.txt\n" \
	" _file1.txt_\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"-file1.txt\n" \
	"+file1.txt_renamed\n" \
	" file1.txt\n" \
	" \n" \
	" \n" \
	" file1.txt\n" \
	" file1.txt\n" \
	"-file1.txt\n" \
	"+file1.txt_renamed\n" \
	" file1.txt\n" \
	" file1.txt\n" \
	" _file1.txt_\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	assert_email_match(expected, "6e05acc5a5dab507d91a0a0cc0fb05a3dd98892d", NULL);
}

void test_email_create__rename_as_add_delete(void)
{
	const char *expected =
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

	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;
	opts.flags |= GIT_EMAIL_CREATE_NO_RENAMES;

	assert_email_match(expected, "6e05acc5a5dab507d91a0a0cc0fb05a3dd98892d", &opts);
}

void test_email_create__binary(void)
{
	const char *expected =
	"From 8d7523f6fcb2404257889abe0d96f093d9f524f9 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Sun, 13 Apr 2014 18:10:18 +0200\n" \
	"Subject: [PATCH] Modified binary file\n" \
	"\n" \
	"---\n" \
	" binary.bin | Bin 3 -> 5 bytes\n" \
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n" \
	"\n" \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index bd474b2519cc15eab801ff851cc7d50f0dee49a1..9ac35ff15cd8864aeafd889e4826a3150f0b06c4 100644\n" \
	"GIT binary patch\n" \
	"literal 5\n" \
	"Mc${NkU}WL~000&M4gdfE\n" \
	"\n" \
	"literal 3\n" \
	"Kc${Nk-~s>u4FC%O\n" \
	"\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	assert_email_match(expected, "8d7523f6fcb2404257889abe0d96f093d9f524f9", NULL);
}

void test_email_create__binary_not_included(void)
{
	const char *expected =
	"From 8d7523f6fcb2404257889abe0d96f093d9f524f9 Mon Sep 17 00:00:00 2001\n" \
	"From: Jacques Germishuys <jacquesg@striata.com>\n" \
	"Date: Sun, 13 Apr 2014 18:10:18 +0200\n" \
	"Subject: [PATCH] Modified binary file\n" \
	"\n" \
	"---\n" \
	" binary.bin | Bin 3 -> 5 bytes\n" \
	" 1 file changed, 0 insertions(+), 0 deletions(-)\n" \
	"\n" \
	"diff --git a/binary.bin b/binary.bin\n" \
	"index bd474b2..9ac35ff 100644\n" \
	"Binary files a/binary.bin and b/binary.bin differ\n" \
	"--\n" \
	"libgit2 " LIBGIT2_VERSION "\n" \
	"\n";

	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;
	opts.diff_opts.flags &= ~GIT_DIFF_SHOW_BINARY;

	assert_email_match(expected, "8d7523f6fcb2404257889abe0d96f093d9f524f9", &opts);
}

void test_email_create__custom_summary_and_body(void)
{
	const char *expected = "From 627e7e12d87e07a83fad5b6bfa25e86ead4a5270 Mon Sep 17 00:00:00 2001\n" \
	"From: Patrick Steinhardt <ps@pks.im>\n" \
	"Date: Tue, 24 Nov 2015 13:34:39 +0100\n" \
	"Subject: [PPPPPATCH 2/4] This is a subject\n" \
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

	const char *summary = "This is a subject\nwith\nnewlines";
	const char *body = "Modify content of file3.txt by appending a new line. Make this\n" \
	"commit message somewhat longer to test behavior with newlines\n" \
	"embedded in the message body.\n" \
	"\n" \
	"Also test if new paragraphs are included correctly.";

	git_oid oid;
	git_commit *commit = NULL;
	git_diff *diff = NULL;
	git_buf buf = GIT_BUF_INIT;
	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;

	opts.subject_prefix = "PPPPPATCH";

	git_oid__fromstr(&oid, "627e7e12d87e07a83fad5b6bfa25e86ead4a5270", GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_diff__commit(&diff, repo, commit, NULL));
	cl_git_pass(git_email_create_from_diff(&buf, diff, 2, 4, &oid, summary, body, git_commit_author(commit), &opts));

	cl_assert_equal_s(expected, buf.ptr);

	git_diff_free(diff);
	git_commit_free(commit);
	git_buf_dispose(&buf);
}

void test_email_create__commit_subjects(void)
{
	git_email_create_options opts = GIT_EMAIL_CREATE_OPTIONS_INIT;

	assert_subject_match("[PATCH] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.reroll_number = 42;
	assert_subject_match("[PATCH v42] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.flags |= GIT_EMAIL_CREATE_ALWAYS_NUMBER;
	assert_subject_match("[PATCH v42 1/1] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.start_number = 9;
	assert_subject_match("[PATCH v42 9/9] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.subject_prefix = "";
	assert_subject_match("[v42 9/9] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.reroll_number = 0;
	assert_subject_match("[9/9] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.start_number = 0;
	assert_subject_match("[1/1] Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);

	opts.flags = GIT_EMAIL_CREATE_OMIT_NUMBERS;
	assert_subject_match("Modify some content", "9264b96c6d104d0e07ae33d3007b6a48246c6f92", &opts);
}
