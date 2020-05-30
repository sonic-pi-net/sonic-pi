#include "clar_libgit2.h"
#include "git2/rebase.h"

static git_repository *repo;
static git_signature *signature;

/* Fixture setup and teardown */
void test_rebase_sign__initialize(void)
{
	repo = cl_git_sandbox_init("rebase");
	cl_git_pass(git_signature_new(&signature, "Rebaser",
		"rebaser@rebaser.rb", 1405694510, 0));
}

void test_rebase_sign__cleanup(void)
{
	git_signature_free(signature);
	cl_git_sandbox_cleanup();
}

static const char *expected_commit_content = "tree cd99b26250099fc38d30bfaed7797a7275ed3366\n\
parent f87d14a4a236582a0278a916340a793714256864\n\
author Edward Thomson <ethomson@edwardthomson.com> 1405625055 -0400\n\
committer Rebaser <rebaser@rebaser.rb> 1405694510 +0000\n\
\n\
Modification 3 to gravy\n";

int signing_cb_passthrough(
	git_buf *signature,
	git_buf *signature_field,
	const char *commit_content,
	void *payload)
{
	cl_assert_equal_b(false, git_buf_is_allocated(signature));
	cl_assert_equal_b(false, git_buf_is_allocated(signature_field));
	cl_assert_equal_s(expected_commit_content, commit_content);
	cl_assert_equal_p(NULL, payload);
	return GIT_PASSTHROUGH;
}

/* git checkout gravy ; git rebase --merge veal */
void test_rebase_sign__passthrough_signing_cb(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, expected_id;
	git_rebase_options rebase_opts = GIT_REBASE_OPTIONS_INIT;
	git_commit *commit;
	const char *expected_commit_raw_header = "tree cd99b26250099fc38d30bfaed7797a7275ed3366\n\
parent f87d14a4a236582a0278a916340a793714256864\n\
author Edward Thomson <ethomson@edwardthomson.com> 1405625055 -0400\n\
committer Rebaser <rebaser@rebaser.rb> 1405694510 +0000\n";

	rebase_opts.signing_cb = signing_cb_passthrough;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &rebase_opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature, NULL, NULL));

	git_oid_fromstr(&expected_id, "129183968a65abd6c52da35bff43325001bfc630");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));
	cl_assert_equal_s(expected_commit_raw_header, git_commit_raw_header(commit));

	cl_git_fail_with(GIT_ITEROVER, git_rebase_next(&rebase_operation, rebase));

	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_commit_free(commit);
	git_rebase_free(rebase);
}

int signing_cb_gpg(
	git_buf *signature,
	git_buf *signature_field,
	const char *commit_content,
	void *payload)
{
	const char *gpg_signature = "-----BEGIN PGP SIGNATURE-----\n\
\n\
iQIzBAEBCgAdFiEEgVlDEfSlmKn0fvGgK++h5T2/ctIFAlwZcrAACgkQK++h5T2/\n\
ctIPVhAA42RyZhMdKl5Bm0KtQco2scsukIg2y7tjSwhti91zDu3HQgpusjjo0fQx\n\
ZzB+OrmlvQ9CDcGpZ0THIzXD8GRJoDMPqdrvZVrBWkGcHvw7/YPA8skzsjkauJ8W\n\
7lzF5LCuHSS6OUmPT/+5hEHPin5PB3zhfszyC+Q7aujnIuPJMrKiMnUa+w1HWifM\n\
km49OOygQ9S6NQoVuEQede22+c76DlDL7yFghGoo1f0sKCE/9LW6SEnwI/bWv9eo\n\
nom5vOPrvQeJiYCQk+2DyWo8RdSxINtY+G9bPE4RXm+6ZgcXECPm9TYDIWpL36fC\n\
jvtGLs98woWFElOziBMp5Tb630GMcSI+q5ivHfJ3WS5NKLYLHBNK4iSFN0/dgAnB\n\
dj6GcKXKWnIBWn6ZM4o40pcM5KSRUUCLtA0ZmjJH4c4zx3X5fUxd+enwkf3e9VZO\n\
fNKC/+xfq6NfoPUPK9+UnchHpJaJw7RG5tZS+sWCz2xpQ1y3/o49xImNyM3wnpvB\n\
cRAZabqIHpZa9/DIUkELOtCzln6niqkjRgg3M/YCCNznwV+0RNgz87VtyTPerdef\n\
xrqn0+ROMF6ebVqIs6PPtuPkxnAJu7TMKXVB5rFnAewS24e6cIGFzeIA7810py3l\n\
cttVRsdOoego+fiy08eFE+aJIeYiINRGhqOBTsuqG4jIdpdKxPE=\n\
=KbsY\n\
-----END PGP SIGNATURE-----";

	cl_assert_equal_b(false, git_buf_is_allocated(signature));
	cl_assert_equal_b(false, git_buf_is_allocated(signature_field));
	cl_assert_equal_s(expected_commit_content, commit_content);
	cl_assert_equal_p(NULL, payload);

	cl_git_pass(git_buf_set(signature, gpg_signature, strlen(gpg_signature) + 1));
	return GIT_OK;
}

/* git checkout gravy ; git rebase --merge veal */
void test_rebase_sign__gpg_with_no_field(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, expected_id;
	git_rebase_options rebase_opts = GIT_REBASE_OPTIONS_INIT;
	git_commit *commit;
	const char *expected_commit_raw_header = "tree cd99b26250099fc38d30bfaed7797a7275ed3366\n\
parent f87d14a4a236582a0278a916340a793714256864\n\
author Edward Thomson <ethomson@edwardthomson.com> 1405625055 -0400\n\
committer Rebaser <rebaser@rebaser.rb> 1405694510 +0000\n\
gpgsig -----BEGIN PGP SIGNATURE-----\n\
 \n\
 iQIzBAEBCgAdFiEEgVlDEfSlmKn0fvGgK++h5T2/ctIFAlwZcrAACgkQK++h5T2/\n\
 ctIPVhAA42RyZhMdKl5Bm0KtQco2scsukIg2y7tjSwhti91zDu3HQgpusjjo0fQx\n\
 ZzB+OrmlvQ9CDcGpZ0THIzXD8GRJoDMPqdrvZVrBWkGcHvw7/YPA8skzsjkauJ8W\n\
 7lzF5LCuHSS6OUmPT/+5hEHPin5PB3zhfszyC+Q7aujnIuPJMrKiMnUa+w1HWifM\n\
 km49OOygQ9S6NQoVuEQede22+c76DlDL7yFghGoo1f0sKCE/9LW6SEnwI/bWv9eo\n\
 nom5vOPrvQeJiYCQk+2DyWo8RdSxINtY+G9bPE4RXm+6ZgcXECPm9TYDIWpL36fC\n\
 jvtGLs98woWFElOziBMp5Tb630GMcSI+q5ivHfJ3WS5NKLYLHBNK4iSFN0/dgAnB\n\
 dj6GcKXKWnIBWn6ZM4o40pcM5KSRUUCLtA0ZmjJH4c4zx3X5fUxd+enwkf3e9VZO\n\
 fNKC/+xfq6NfoPUPK9+UnchHpJaJw7RG5tZS+sWCz2xpQ1y3/o49xImNyM3wnpvB\n\
 cRAZabqIHpZa9/DIUkELOtCzln6niqkjRgg3M/YCCNznwV+0RNgz87VtyTPerdef\n\
 xrqn0+ROMF6ebVqIs6PPtuPkxnAJu7TMKXVB5rFnAewS24e6cIGFzeIA7810py3l\n\
 cttVRsdOoego+fiy08eFE+aJIeYiINRGhqOBTsuqG4jIdpdKxPE=\n\
 =KbsY\n\
 -----END PGP SIGNATURE-----\n";

	rebase_opts.signing_cb = signing_cb_gpg;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &rebase_opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature, NULL, NULL));

	git_oid_fromstr(&expected_id, "bf78348e45c8286f52b760f1db15cb6da030f2ef");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));
	cl_assert_equal_s(expected_commit_raw_header, git_commit_raw_header(commit));

	cl_git_fail_with(GIT_ITEROVER, git_rebase_next(&rebase_operation, rebase));

	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_commit_free(commit);
	git_rebase_free(rebase);
}


int signing_cb_magic_field(
	git_buf *signature,
	git_buf *signature_field,
	const char *commit_content,
	void *payload)
{
	const char *signature_content = "magic word: pretty please";
	const char *signature_field_content = "magicsig";

	cl_assert_equal_b(false, git_buf_is_allocated(signature));
	cl_assert_equal_b(false, git_buf_is_allocated(signature_field));
	cl_assert_equal_s(expected_commit_content, commit_content);
	cl_assert_equal_p(NULL, payload);

	cl_git_pass(git_buf_set(signature, signature_content,
		strlen(signature_content) + 1));
	cl_git_pass(git_buf_set(signature_field, signature_field_content,
		strlen(signature_field_content) + 1));

	return GIT_OK;
}

/* git checkout gravy ; git rebase --merge veal */
void test_rebase_sign__custom_signature_field(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, expected_id;
	git_rebase_options rebase_opts = GIT_REBASE_OPTIONS_INIT;
	git_commit *commit;
	const char *expected_commit_raw_header = "tree cd99b26250099fc38d30bfaed7797a7275ed3366\n\
parent f87d14a4a236582a0278a916340a793714256864\n\
author Edward Thomson <ethomson@edwardthomson.com> 1405625055 -0400\n\
committer Rebaser <rebaser@rebaser.rb> 1405694510 +0000\n\
magicsig magic word: pretty please\n";

	rebase_opts.signing_cb = signing_cb_magic_field;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &rebase_opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature, NULL, NULL));

	git_oid_fromstr(&expected_id, "f46a4a8d26ae411b02aa61b7d69576627f4a1e1c");
	cl_assert_equal_oid(&expected_id, &commit_id);

	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));
	cl_assert_equal_s(expected_commit_raw_header, git_commit_raw_header(commit));

	cl_git_fail_with(GIT_ITEROVER, git_rebase_next(&rebase_operation, rebase));

	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_commit_free(commit);
	git_rebase_free(rebase);
}
