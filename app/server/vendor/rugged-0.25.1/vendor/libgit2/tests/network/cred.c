#include "clar_libgit2.h"

#include "git2/cred_helpers.h"

void test_network_cred__stock_userpass_validates_args(void)
{
	git_cred_userpass_payload payload = {0};

	cl_git_fail(git_cred_userpass(NULL, NULL, NULL, 0, NULL));

	payload.username = "user";
	cl_git_fail(git_cred_userpass(NULL, NULL, NULL, 0, &payload));

	payload.username = NULL;
	payload.username = "pass";
	cl_git_fail(git_cred_userpass(NULL, NULL, NULL, 0, &payload));
}

void test_network_cred__stock_userpass_validates_that_method_is_allowed(void)
{
	git_cred *cred;
	git_cred_userpass_payload payload = {"user", "pass"};

	cl_git_fail(git_cred_userpass(&cred, NULL, NULL, 0, &payload));
	cl_git_pass(git_cred_userpass(&cred, NULL, NULL, GIT_CREDTYPE_USERPASS_PLAINTEXT, &payload));
	cred->free(cred);
}

void test_network_cred__stock_userpass_properly_handles_username_in_url(void)
{
	git_cred *cred;
	git_cred_userpass_plaintext *plain;
	git_cred_userpass_payload payload = {"alice", "password"};

	cl_git_pass(git_cred_userpass(&cred, NULL, NULL, GIT_CREDTYPE_USERPASS_PLAINTEXT, &payload));
	plain = (git_cred_userpass_plaintext*)cred;
	cl_assert_equal_s(plain->username, "alice");
	cred->free(cred);

	cl_git_pass(git_cred_userpass(&cred, NULL, "bob", GIT_CREDTYPE_USERPASS_PLAINTEXT, &payload));
	plain = (git_cred_userpass_plaintext*)cred;
	cl_assert_equal_s(plain->username, "alice");
	cred->free(cred);

	payload.username = NULL;
	cl_git_pass(git_cred_userpass(&cred, NULL, "bob", GIT_CREDTYPE_USERPASS_PLAINTEXT, &payload));
	plain = (git_cred_userpass_plaintext*)cred;
	cl_assert_equal_s(plain->username, "bob");
	cred->free(cred);
}
