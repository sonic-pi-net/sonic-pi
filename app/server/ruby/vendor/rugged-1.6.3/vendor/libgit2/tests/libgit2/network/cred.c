#include "clar_libgit2.h"

#include "git2/cred_helpers.h"

void test_network_cred__stock_userpass_validates_args(void)
{
	git_credential_userpass_payload payload = {0};

	cl_git_fail(git_credential_userpass(NULL, NULL, NULL, 0, NULL));

	payload.username = "user";
	cl_git_fail(git_credential_userpass(NULL, NULL, NULL, 0, &payload));

	payload.username = NULL;
	payload.username = "pass";
	cl_git_fail(git_credential_userpass(NULL, NULL, NULL, 0, &payload));
}

void test_network_cred__stock_userpass_validates_that_method_is_allowed(void)
{
	git_credential *cred;
	git_credential_userpass_payload payload = {"user", "pass"};

	cl_git_fail(git_credential_userpass(&cred, NULL, NULL, 0, &payload));
	cl_git_pass(git_credential_userpass(&cred, NULL, NULL, GIT_CREDENTIAL_USERPASS_PLAINTEXT, &payload));
	git_credential_free(cred);
}

void test_network_cred__stock_userpass_properly_handles_username_in_url(void)
{
	git_credential *cred;
	git_credential_userpass_payload payload = {"alice", "password"};

	cl_git_pass(git_credential_userpass(&cred, NULL, NULL, GIT_CREDENTIAL_USERPASS_PLAINTEXT, &payload));
	cl_assert_equal_s("alice", git_credential_get_username(cred));
	git_credential_free(cred);

	cl_git_pass(git_credential_userpass(&cred, NULL, "bob", GIT_CREDENTIAL_USERPASS_PLAINTEXT, &payload));
	cl_assert_equal_s("alice", git_credential_get_username(cred));
	git_credential_free(cred);

	payload.username = NULL;
	cl_git_pass(git_credential_userpass(&cred, NULL, "bob", GIT_CREDENTIAL_USERPASS_PLAINTEXT, &payload));
	cl_assert_equal_s("bob", git_credential_get_username(cred));
	git_credential_free(cred);
}
