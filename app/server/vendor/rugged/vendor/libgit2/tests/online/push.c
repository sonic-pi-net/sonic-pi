#include "clar_libgit2.h"
#include "buffer.h"
#include "posix.h"
#include "vector.h"
#include "../submodule/submodule_helpers.h"
#include "push_util.h"
#include "refspec.h"
#include "remote.h"

static git_repository *_repo;

static char *_remote_url;

static char *_remote_ssh_key;
static char *_remote_ssh_pubkey;
static char *_remote_ssh_passphrase;

static char *_remote_user;
static char *_remote_pass;

static char *_remote_default;

static int cred_acquire_cb(git_cred **,	const char *, const char *, unsigned int, void *);

static git_remote *_remote;
static record_callbacks_data _record_cbs_data = {{ 0 }};
static git_remote_callbacks _record_cbs = RECORD_CALLBACKS_INIT(&_record_cbs_data);

static git_oid _oid_b6;
static git_oid _oid_b5;
static git_oid _oid_b4;
static git_oid _oid_b3;
static git_oid _oid_b2;
static git_oid _oid_b1;

static git_oid _tag_commit;
static git_oid _tag_tree;
static git_oid _tag_blob;
static git_oid _tag_lightweight;
static git_oid _tag_tag;

static int cred_acquire_cb(
	git_cred **cred,
	const char *url,
	const char *user_from_url,
	unsigned int allowed_types,
	void *payload)
{
	GIT_UNUSED(url);
	GIT_UNUSED(user_from_url);
	GIT_UNUSED(payload);

	if (GIT_CREDTYPE_DEFAULT & allowed_types) {
		if (!_remote_default) {
			printf("GITTEST_REMOTE_DEFAULT must be set to use NTLM/Negotiate credentials\n");
			return -1;
		}

		return git_cred_default_new(cred);
	}

	if (GIT_CREDTYPE_SSH_KEY & allowed_types) {
		if (!_remote_user || !_remote_ssh_pubkey || !_remote_ssh_key || !_remote_ssh_passphrase) {
			printf("GITTEST_REMOTE_USER, GITTEST_REMOTE_SSH_PUBKEY, GITTEST_REMOTE_SSH_KEY and GITTEST_REMOTE_SSH_PASSPHRASE must be set\n");
			return -1;
		}

		return git_cred_ssh_key_new(cred, _remote_user, _remote_ssh_pubkey, _remote_ssh_key, _remote_ssh_passphrase);
	}

	if (GIT_CREDTYPE_USERPASS_PLAINTEXT & allowed_types) {
		if (!_remote_user || !_remote_pass) {
			printf("GITTEST_REMOTE_USER and GITTEST_REMOTE_PASS must be set\n");
			return -1;
		}

		return git_cred_userpass_plaintext_new(cred, _remote_user, _remote_pass);
	}

	return -1;
}

/* the results of a push status.  when used for expected values, msg may be NULL
 * to indicate that it should not be matched. */
typedef struct {
	const char *ref;
	int success;
	const char *msg;
} push_status;

/**
 * git_push_status_foreach callback that records status entries.
 * @param data (git_vector *) of push_status instances
 */
static int record_push_status_cb(const char *ref, const char *msg, void *data)
{
	git_vector *statuses = (git_vector *)data;
	push_status *s;

	cl_assert(s = git__malloc(sizeof(*s)));
	s->ref = ref;
	s->success = (msg == NULL);
	s->msg = msg;

	git_vector_insert(statuses, s);

	return 0;
}

static void do_verify_push_status(git_push *push, const push_status expected[], const size_t expected_len)
{
	git_vector actual = GIT_VECTOR_INIT;
	push_status *iter;
	bool failed = false;
	size_t i;

	git_push_status_foreach(push, record_push_status_cb, &actual);

	if (expected_len != actual.length)
		failed = true;
	else
		git_vector_foreach(&actual, i, iter)
			if (strcmp(expected[i].ref, iter->ref) ||
				(expected[i].success != iter->success) ||
				(expected[i].msg && (!iter->msg || strcmp(expected[i].msg, iter->msg)))) {
				failed = true;
				break;
			}

	if (failed) {
		git_buf msg = GIT_BUF_INIT;

		git_buf_puts(&msg, "Expected and actual push statuses differ:\nEXPECTED:\n");

		for(i = 0; i < expected_len; i++) {
			git_buf_printf(&msg, "%s: %s\n",
				expected[i].ref,
				expected[i].success ? "success" : "failed");
		}

		git_buf_puts(&msg, "\nACTUAL:\n");

		git_vector_foreach(&actual, i, iter) {
			if (iter->success)
				git_buf_printf(&msg, "%s: success\n", iter->ref);
			else
				git_buf_printf(&msg, "%s: failed with message: %s", iter->ref, iter->msg);
		}

		cl_fail(git_buf_cstr(&msg));

		git_buf_free(&msg);
	}

	git_vector_foreach(&actual, i, iter)
		git__free(iter);

	git_vector_free(&actual);
}

/**
 * Verifies that after git_push_finish(), refs on a remote have the expected
 * names, oids, and order.
 *
 * @param remote remote to verify
 * @param expected_refs expected remote refs after push
 * @param expected_refs_len length of expected_refs
 */
static void verify_refs(git_remote *remote, expected_ref expected_refs[], size_t expected_refs_len)
{
	const git_remote_head **actual_refs;
	size_t actual_refs_len;

	git_remote_ls(&actual_refs, &actual_refs_len, remote);
	verify_remote_refs(actual_refs, actual_refs_len, expected_refs, expected_refs_len);
}

/**
 * Verifies that after git_push_update_tips(), remote tracking branches have the expected
 * names and oids.
 *
 * @param remote remote to verify
 * @param expected_refs expected remote refs after push
 * @param expected_refs_len length of expected_refs
 */
static void verify_tracking_branches(git_remote *remote, expected_ref expected_refs[], size_t expected_refs_len)
{
	git_refspec *fetch_spec;
	size_t i, j;
	git_buf msg = GIT_BUF_INIT;
	git_buf ref_name = GIT_BUF_INIT;
	git_vector actual_refs = GIT_VECTOR_INIT;
	git_branch_iterator *iter;
	char *actual_ref;
	git_oid oid;
	int failed = 0, error;
	git_branch_t branch_type;
	git_reference *ref;

	/* Get current remote branches */
	cl_git_pass(git_branch_iterator_new(&iter, remote->repo, GIT_BRANCH_REMOTE));

	while ((error = git_branch_next(&ref, &branch_type, iter)) == 0) {
		cl_assert_equal_i(branch_type, GIT_BRANCH_REMOTE);

		cl_git_pass(git_vector_insert(&actual_refs, git__strdup(git_reference_name(ref))));
	}

	cl_assert_equal_i(error, GIT_ITEROVER);
	git_branch_iterator_free(iter);

	/* Loop through expected refs, make sure they exist */
	for (i = 0; i < expected_refs_len; i++) {

		/* Convert remote reference name into tracking branch name.
		 * If the spec is not under refs/heads/, then skip.
		 */
		fetch_spec = git_remote__matching_refspec(remote, expected_refs[i].name);
		if (!fetch_spec)
			continue;

		cl_git_pass(git_refspec_transform(&ref_name, fetch_spec, expected_refs[i].name));

		/* Find matching remote branch */
		git_vector_foreach(&actual_refs, j, actual_ref) {
			if (!strcmp(git_buf_cstr(&ref_name), actual_ref))
				break;
		}

		if (j == actual_refs.length) {
			git_buf_printf(&msg, "Did not find expected tracking branch '%s'.", git_buf_cstr(&ref_name));
			failed = 1;
			goto failed;
		}

		/* Make sure tracking branch is at expected commit ID */
		cl_git_pass(git_reference_name_to_id(&oid, remote->repo, actual_ref));

		if (git_oid_cmp(expected_refs[i].oid, &oid) != 0) {
			git_buf_puts(&msg, "Tracking branch commit does not match expected ID.");
			failed = 1;
			goto failed;
		}

		git__free(actual_ref);
		cl_git_pass(git_vector_remove(&actual_refs, j));
	}

	/* Make sure there are no extra branches */
	if (actual_refs.length > 0) {
		git_buf_puts(&msg, "Unexpected remote tracking branches exist.");
		failed = 1;
		goto failed;
	}

failed:
	if (failed)
		cl_fail(git_buf_cstr(&msg));

	git_vector_foreach(&actual_refs, i, actual_ref)
		git__free(actual_ref);

	git_vector_free(&actual_refs);
	git_buf_free(&msg);
	git_buf_free(&ref_name);
}

static void verify_update_tips_callback(git_remote *remote, expected_ref expected_refs[], size_t expected_refs_len)
{
	git_refspec *fetch_spec;
	git_buf msg = GIT_BUF_INIT;
	git_buf ref_name = GIT_BUF_INIT;
	updated_tip *tip = NULL;
	size_t i, j;
	int failed = 0;

	for (i = 0; i < expected_refs_len; ++i) {
		/* Convert remote reference name into tracking branch name.
		 * If the spec is not under refs/heads/, then skip.
		 */
		fetch_spec = git_remote__matching_refspec(remote, expected_refs[i].name);
		if (!fetch_spec)
			continue;

		cl_git_pass(git_refspec_transform(&ref_name, fetch_spec, expected_refs[i].name));

		/* Find matching update_tip entry */
		git_vector_foreach(&_record_cbs_data.updated_tips, j, tip) {
			if (!strcmp(git_buf_cstr(&ref_name), tip->name))
				break;
		}

		if (j == _record_cbs_data.updated_tips.length) {
			git_buf_printf(&msg, "Did not find expected updated tip entry for branch '%s'.", git_buf_cstr(&ref_name));
			failed = 1;
			goto failed;
		}

		if (git_oid_cmp(expected_refs[i].oid, tip->new_oid) != 0) {
			git_buf_printf(&msg, "Updated tip ID does not match expected ID");
			failed = 1;
			goto failed;
		}
	}

failed:
	if (failed)
		cl_fail(git_buf_cstr(&msg));

	git_buf_free(&ref_name);
	git_buf_free(&msg);
}

void test_online_push__initialize(void)
{
	git_vector delete_specs = GIT_VECTOR_INIT;
	const git_remote_head **heads;
	size_t i, heads_len;
	char *curr_del_spec;

	_repo = cl_git_sandbox_init("push_src");

	cl_fixture_sandbox("testrepo.git");
	cl_rename("push_src/submodule/.gitted", "push_src/submodule/.git");

	rewrite_gitmodules(git_repository_workdir(_repo));

	/* git log --format=oneline --decorate --graph
	 * *-.   951bbbb90e2259a4c8950db78946784fb53fcbce (HEAD, b6) merge b3, b4, and b5 to b6
	 * |\ \
	 * | | * fa38b91f199934685819bea316186d8b008c52a2 (b5) added submodule named 'submodule' pointing to '../testrepo.git'
	 * | * | 27b7ce66243eb1403862d05f958c002312df173d (b4) edited fold\b.txt
	 * | |/
	 * * | d9b63a88223d8367516f50bd131a5f7349b7f3e4 (b3) edited a.txt
	 * |/
	 * * a78705c3b2725f931d3ee05348d83cc26700f247 (b2, b1) added fold and fold/b.txt
	 * * 5c0bb3d1b9449d1cc69d7519fd05166f01840915 added a.txt
	 */
	git_oid_fromstr(&_oid_b6, "951bbbb90e2259a4c8950db78946784fb53fcbce");
	git_oid_fromstr(&_oid_b5, "fa38b91f199934685819bea316186d8b008c52a2");
	git_oid_fromstr(&_oid_b4, "27b7ce66243eb1403862d05f958c002312df173d");
	git_oid_fromstr(&_oid_b3, "d9b63a88223d8367516f50bd131a5f7349b7f3e4");
	git_oid_fromstr(&_oid_b2, "a78705c3b2725f931d3ee05348d83cc26700f247");
	git_oid_fromstr(&_oid_b1, "a78705c3b2725f931d3ee05348d83cc26700f247");

	git_oid_fromstr(&_tag_commit, "805c54522e614f29f70d2413a0470247d8b424ac");
	git_oid_fromstr(&_tag_tree, "ff83aa4c5e5d28e3bcba2f5c6e2adc61286a4e5e");
	git_oid_fromstr(&_tag_blob, "b483ae7ba66decee9aee971f501221dea84b1498");
	git_oid_fromstr(&_tag_lightweight, "951bbbb90e2259a4c8950db78946784fb53fcbce");
	git_oid_fromstr(&_tag_tag, "eea4f2705eeec2db3813f2430829afce99cd00b5");

	/* Remote URL environment variable must be set.  User and password are optional.  */
	_remote_url = cl_getenv("GITTEST_REMOTE_URL");
	_remote_user = cl_getenv("GITTEST_REMOTE_USER");
	_remote_pass = cl_getenv("GITTEST_REMOTE_PASS");
	_remote_ssh_key = cl_getenv("GITTEST_REMOTE_SSH_KEY");
	_remote_ssh_pubkey = cl_getenv("GITTEST_REMOTE_SSH_PUBKEY");
	_remote_ssh_passphrase = cl_getenv("GITTEST_REMOTE_SSH_PASSPHRASE");
	_remote_default = cl_getenv("GITTEST_REMOTE_DEFAULT");
	_remote = NULL;

	/* Skip the test if we're missing the remote URL */
	if (!_remote_url)
		cl_skip();

	cl_git_pass(git_remote_create(&_remote, _repo, "test", _remote_url));

	record_callbacks_data_clear(&_record_cbs_data);
	git_remote_set_callbacks(_remote, &_record_cbs);

	cl_git_pass(git_remote_connect(_remote, GIT_DIRECTION_PUSH));

	/* Clean up previously pushed branches.  Fails if receive.denyDeletes is
	 * set on the remote.  Also, on Git 1.7.0 and newer, you must run
	 * 'git config receive.denyDeleteCurrent ignore' in the remote repo in
	 * order to delete the remote branch pointed to by HEAD (usually master).
	 * See: https://raw.github.com/git/git/master/Documentation/RelNotes/1.7.0.txt
	 */
	cl_git_pass(git_remote_ls(&heads, &heads_len, _remote));
	cl_git_pass(create_deletion_refspecs(&delete_specs, heads, heads_len));
	if (delete_specs.length) {
		git_push *push;

		cl_git_pass(git_push_new(&push, _remote));

		git_vector_foreach(&delete_specs, i, curr_del_spec) {
			git_push_add_refspec(push, curr_del_spec);
			git__free(curr_del_spec);
		}

		cl_git_pass(git_push_finish(push));
		git_push_free(push);
	}

	git_remote_disconnect(_remote);
	git_vector_free(&delete_specs);

	/* Now that we've deleted everything, fetch from the remote */
	cl_git_pass(git_remote_connect(_remote, GIT_DIRECTION_FETCH));
	cl_git_pass(git_remote_download(_remote));
	cl_git_pass(git_remote_update_tips(_remote, NULL, NULL));
	git_remote_disconnect(_remote);
}

void test_online_push__cleanup(void)
{
	if (_remote)
		git_remote_free(_remote);
	_remote = NULL;

	/* Freed by cl_git_sandbox_cleanup */
	_repo = NULL;

	record_callbacks_data_clear(&_record_cbs_data);

	cl_fixture_cleanup("testrepo.git");
	cl_git_sandbox_cleanup();
}

static int push_pack_progress_cb(
	int stage, unsigned int current, unsigned int total, void* payload)
{
	int *calls = (int *)payload;
	GIT_UNUSED(stage); GIT_UNUSED(current); GIT_UNUSED(total);
	if (*calls < 0)
		return *calls;
	(*calls)++;
	return 0;
}

static int push_transfer_progress_cb(
	unsigned int current, unsigned int total, size_t bytes, void* payload)
{
	int *calls = (int *)payload;
	GIT_UNUSED(current); GIT_UNUSED(total); GIT_UNUSED(bytes);
	if (*calls < 0)
		return *calls;
	(*calls)++;
	return 0;
}

/**
 * Calls push and relists refs on remote to verify success.
 *
 * @param refspecs refspecs to push
 * @param refspecs_len length of refspecs
 * @param expected_refs expected remote refs after push
 * @param expected_refs_len length of expected_refs
 * @param expected_ret expected return value from git_push_finish()
 * @param check_progress_cb Check that the push progress callbacks are called
 */
static void do_push(
	const char *refspecs[], size_t refspecs_len,
	push_status expected_statuses[], size_t expected_statuses_len,
	expected_ref expected_refs[], size_t expected_refs_len,
	int expected_ret, int check_progress_cb, int check_update_tips_cb)
{
	git_push *push;
	git_push_options opts = GIT_PUSH_OPTIONS_INIT;
	size_t i;
	int pack_progress_calls = 0, transfer_progress_calls = 0;
	git_signature *pusher;

	if (_remote) {
		/* Auto-detect the number of threads to use */
		opts.pb_parallelism = 0;

		cl_git_pass(git_signature_now(&pusher, "Foo Bar", "foo@example.com"));
		cl_git_pass(git_remote_connect(_remote, GIT_DIRECTION_PUSH));

		cl_git_pass(git_push_new(&push, _remote));
		cl_git_pass(git_push_set_options(push, &opts));

		if (check_progress_cb) {
			/* if EUSER, then abort in transfer */
			if (expected_ret == GIT_EUSER)
				transfer_progress_calls = GIT_EUSER;

			cl_git_pass(
				git_push_set_callbacks(
					push, push_pack_progress_cb, &pack_progress_calls,
					push_transfer_progress_cb, &transfer_progress_calls));
		}

		for (i = 0; i < refspecs_len; i++)
			cl_git_pass(git_push_add_refspec(push, refspecs[i]));

		if (expected_ret < 0) {
			cl_git_fail_with(git_push_finish(push), expected_ret);
			cl_assert_equal_i(0, git_push_unpack_ok(push));
		} else {
			cl_git_pass(git_push_finish(push));
			cl_assert_equal_i(1, git_push_unpack_ok(push));
		}

		if (check_progress_cb && !expected_ret) {
			cl_assert(pack_progress_calls > 0);
			cl_assert(transfer_progress_calls > 0);
		}

		do_verify_push_status(push, expected_statuses, expected_statuses_len);

		verify_refs(_remote, expected_refs, expected_refs_len);

		cl_git_pass(git_push_update_tips(push, pusher, "test push"));
		verify_tracking_branches(_remote, expected_refs, expected_refs_len);

		if (check_update_tips_cb)
			verify_update_tips_callback(_remote, expected_refs, expected_refs_len);

		git_push_free(push);

		git_remote_disconnect(_remote);
		git_signature_free(pusher);
	}

}

/* Call push_finish() without ever calling git_push_add_refspec() */
void test_online_push__noop(void)
{
	do_push(NULL, 0, NULL, 0, NULL, 0, 0, 0, 1);
}

void test_online_push__b1(void)
{
	const char *specs[] = { "refs/heads/b1:refs/heads/b1" };
	push_status exp_stats[] = { { "refs/heads/b1", 1 } };
	expected_ref exp_refs[] = { { "refs/heads/b1", &_oid_b1 } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__b2(void)
{
	const char *specs[] = { "refs/heads/b2:refs/heads/b2" };
	push_status exp_stats[] = { { "refs/heads/b2", 1 } };
	expected_ref exp_refs[] = { { "refs/heads/b2", &_oid_b2 } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__b3(void)
{
	const char *specs[] = { "refs/heads/b3:refs/heads/b3" };
	push_status exp_stats[] = { { "refs/heads/b3", 1 } };
	expected_ref exp_refs[] = { { "refs/heads/b3", &_oid_b3 } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__b4(void)
{
	const char *specs[] = { "refs/heads/b4:refs/heads/b4" };
	push_status exp_stats[] = { { "refs/heads/b4", 1 } };
	expected_ref exp_refs[] = { { "refs/heads/b4", &_oid_b4 } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__b5(void)
{
	const char *specs[] = { "refs/heads/b5:refs/heads/b5" };
	push_status exp_stats[] = { { "refs/heads/b5", 1 } };
	expected_ref exp_refs[] = { { "refs/heads/b5", &_oid_b5 } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__b5_cancel(void)
{
	const char *specs[] = { "refs/heads/b5:refs/heads/b5" };
	do_push(specs, ARRAY_SIZE(specs), NULL, 0, NULL, 0, GIT_EUSER, 1, 1);
}

void test_online_push__multi(void)
{
	git_reflog *log;
	const git_reflog_entry *entry;

	const char *specs[] = {
		"refs/heads/b1:refs/heads/b1",
		"refs/heads/b2:refs/heads/b2",
		"refs/heads/b3:refs/heads/b3",
		"refs/heads/b4:refs/heads/b4",
		"refs/heads/b5:refs/heads/b5"
	};
	push_status exp_stats[] = {
		{ "refs/heads/b1", 1 },
		{ "refs/heads/b2", 1 },
		{ "refs/heads/b3", 1 },
		{ "refs/heads/b4", 1 },
		{ "refs/heads/b5", 1 }
	};
	expected_ref exp_refs[] = {
		{ "refs/heads/b1", &_oid_b1 },
		{ "refs/heads/b2", &_oid_b2 },
		{ "refs/heads/b3", &_oid_b3 },
		{ "refs/heads/b4", &_oid_b4 },
		{ "refs/heads/b5", &_oid_b5 }
	};
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);

	cl_git_pass(git_reflog_read(&log, _repo, "refs/remotes/test/b1"));
	entry = git_reflog_entry_byindex(log, 0);
	if (entry) {
		cl_assert_equal_s("test push", git_reflog_entry_message(entry));
		cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);
	}

	git_reflog_free(log);
}

void test_online_push__implicit_tgt(void)
{
	const char *specs1[] = { "refs/heads/b1:" };
	push_status exp_stats1[] = { { "refs/heads/b1", 1 } };
	expected_ref exp_refs1[] = { { "refs/heads/b1", &_oid_b1 } };

	const char *specs2[] = { "refs/heads/b2:" };
	push_status exp_stats2[] = { { "refs/heads/b2", 1 } };
	expected_ref exp_refs2[] = {
	{ "refs/heads/b1", &_oid_b1 },
	{ "refs/heads/b2", &_oid_b2 }
	};

	do_push(specs1, ARRAY_SIZE(specs1),
		exp_stats1, ARRAY_SIZE(exp_stats1),
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 1, 1);
	do_push(specs2, ARRAY_SIZE(specs2),
		exp_stats2, ARRAY_SIZE(exp_stats2),
		exp_refs2, ARRAY_SIZE(exp_refs2), 0, 0, 0);
}

void test_online_push__fast_fwd(void)
{
	/* Fast forward b1 in tgt from _oid_b1 to _oid_b6. */

	const char *specs_init[] = { "refs/heads/b1:refs/heads/b1" };
	push_status exp_stats_init[] = { { "refs/heads/b1", 1 } };
	expected_ref exp_refs_init[] = { { "refs/heads/b1", &_oid_b1 } };

	const char *specs_ff[] = { "refs/heads/b6:refs/heads/b1" };
	push_status exp_stats_ff[] = { { "refs/heads/b1", 1 } };
	expected_ref exp_refs_ff[] = { { "refs/heads/b1", &_oid_b6 } };

	/* Do a force push to reset b1 in target back to _oid_b1 */
	const char *specs_reset[] = { "+refs/heads/b1:refs/heads/b1" };
	/* Force should have no effect on a fast forward push */
	const char *specs_ff_force[] = { "+refs/heads/b6:refs/heads/b1" };

	do_push(specs_init, ARRAY_SIZE(specs_init),
		exp_stats_init, ARRAY_SIZE(exp_stats_init),
		exp_refs_init, ARRAY_SIZE(exp_refs_init), 0, 1, 1);

	do_push(specs_ff, ARRAY_SIZE(specs_ff),
		exp_stats_ff, ARRAY_SIZE(exp_stats_ff),
		exp_refs_ff, ARRAY_SIZE(exp_refs_ff), 0, 0, 0);

	do_push(specs_reset, ARRAY_SIZE(specs_reset),
		exp_stats_init, ARRAY_SIZE(exp_stats_init),
		exp_refs_init, ARRAY_SIZE(exp_refs_init), 0, 0, 0);

	do_push(specs_ff_force, ARRAY_SIZE(specs_ff_force),
		exp_stats_ff, ARRAY_SIZE(exp_stats_ff),
		exp_refs_ff, ARRAY_SIZE(exp_refs_ff), 0, 0, 0);
}

void test_online_push__tag_commit(void)
{
	const char *specs[] = { "refs/tags/tag-commit:refs/tags/tag-commit" };
	push_status exp_stats[] = { { "refs/tags/tag-commit", 1 } };
	expected_ref exp_refs[] = { { "refs/tags/tag-commit", &_tag_commit } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__tag_tree(void)
{
	const char *specs[] = { "refs/tags/tag-tree:refs/tags/tag-tree" };
	push_status exp_stats[] = { { "refs/tags/tag-tree", 1 } };
	expected_ref exp_refs[] = { { "refs/tags/tag-tree", &_tag_tree } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__tag_blob(void)
{
	const char *specs[] = { "refs/tags/tag-blob:refs/tags/tag-blob" };
	push_status exp_stats[] = { { "refs/tags/tag-blob", 1 } };
	expected_ref exp_refs[] = { { "refs/tags/tag-blob", &_tag_blob } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__tag_lightweight(void)
{
	const char *specs[] = { "refs/tags/tag-lightweight:refs/tags/tag-lightweight" };
	push_status exp_stats[] = { { "refs/tags/tag-lightweight", 1 } };
	expected_ref exp_refs[] = { { "refs/tags/tag-lightweight", &_tag_lightweight } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);
}

void test_online_push__tag_to_tag(void)
{
	const char *specs[] = { "refs/tags/tag-tag:refs/tags/tag-tag" };
	push_status exp_stats[] = { { "refs/tags/tag-tag", 1 } };
	expected_ref exp_refs[] = { { "refs/tags/tag-tag", &_tag_tag } };
	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 0, 0);
}

void test_online_push__force(void)
{
	const char *specs1[] = {"refs/heads/b3:refs/heads/tgt"};
	push_status exp_stats1[] = { { "refs/heads/tgt", 1 } };
	expected_ref exp_refs1[] = { { "refs/heads/tgt", &_oid_b3 } };

	const char *specs2[] = {"refs/heads/b4:refs/heads/tgt"};

	const char *specs2_force[] = {"+refs/heads/b4:refs/heads/tgt"};
	push_status exp_stats2_force[] = { { "refs/heads/tgt", 1 } };
	expected_ref exp_refs2_force[] = { { "refs/heads/tgt", &_oid_b4 } };

	do_push(specs1, ARRAY_SIZE(specs1),
		exp_stats1, ARRAY_SIZE(exp_stats1),
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 1, 1);

	do_push(specs2, ARRAY_SIZE(specs2),
		NULL, 0,
		exp_refs1, ARRAY_SIZE(exp_refs1), GIT_ENONFASTFORWARD, 0, 0);

	/* Non-fast-forward update with force should pass. */
	record_callbacks_data_clear(&_record_cbs_data);
	do_push(specs2_force, ARRAY_SIZE(specs2_force),
		exp_stats2_force, ARRAY_SIZE(exp_stats2_force),
		exp_refs2_force, ARRAY_SIZE(exp_refs2_force), 0, 1, 1);
}

void test_online_push__delete(void)
{
	const char *specs1[] = {
		"refs/heads/b1:refs/heads/tgt1",
		"refs/heads/b1:refs/heads/tgt2"
	};
	push_status exp_stats1[] = {
		{ "refs/heads/tgt1", 1 },
		{ "refs/heads/tgt2", 1 }
	};
	expected_ref exp_refs1[] = {
		{ "refs/heads/tgt1", &_oid_b1 },
		{ "refs/heads/tgt2", &_oid_b1 }
	};

	const char *specs_del_fake[] = { ":refs/heads/fake" };
	/* Force has no effect for delete. */
	const char *specs_del_fake_force[] = { "+:refs/heads/fake" };
	push_status exp_stats_fake[] = { { "refs/heads/fake", 1 } };

	const char *specs_delete[] = { ":refs/heads/tgt1" };
	push_status exp_stats_delete[] = { { "refs/heads/tgt1", 1 } };
	expected_ref exp_refs_delete[] = { { "refs/heads/tgt2", &_oid_b1 } };
	/* Force has no effect for delete. */
	const char *specs_delete_force[] = { "+:refs/heads/tgt1" };

	do_push(specs1, ARRAY_SIZE(specs1),
		exp_stats1, ARRAY_SIZE(exp_stats1),
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 1, 1);

	/* When deleting a non-existent branch, the git client sends zero for both
	 * the old and new commit id.  This should succeed on the server with the
	 * same status report as if the branch were actually deleted.  The server
	 * returns a warning on the side-band iff the side-band is supported.
	 *  Since libgit2 doesn't support the side-band yet, there are no warnings.
	 */
	do_push(specs_del_fake, ARRAY_SIZE(specs_del_fake),
		exp_stats_fake, 1,
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 0, 0);
	do_push(specs_del_fake_force, ARRAY_SIZE(specs_del_fake_force),
		exp_stats_fake, 1,
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 0, 0);

	/* Delete one of the pushed branches. */
	do_push(specs_delete, ARRAY_SIZE(specs_delete),
		exp_stats_delete, ARRAY_SIZE(exp_stats_delete),
		exp_refs_delete, ARRAY_SIZE(exp_refs_delete), 0, 0, 0);

	/* Re-push branches and retry delete with force. */
	do_push(specs1, ARRAY_SIZE(specs1),
		exp_stats1, ARRAY_SIZE(exp_stats1),
		exp_refs1, ARRAY_SIZE(exp_refs1), 0, 0, 0);
	do_push(specs_delete_force, ARRAY_SIZE(specs_delete_force),
		exp_stats_delete, ARRAY_SIZE(exp_stats_delete),
		exp_refs_delete, ARRAY_SIZE(exp_refs_delete), 0, 0, 0);
}

void test_online_push__bad_refspecs(void)
{
	/* All classes of refspecs that should be rejected by
	 * git_push_add_refspec() should go in this test.
	 */
	git_push *push;

	if (_remote) {
/*		cl_git_pass(git_remote_connect(_remote, GIT_DIRECTION_PUSH)); */
		cl_git_pass(git_push_new(&push, _remote));

		/* Unexpanded branch names not supported */
		cl_git_fail(git_push_add_refspec(push, "b6:b6"));

		git_push_free(push);
	}
}

void test_online_push__expressions(void)
{
	/* TODO: Expressions in refspecs doesn't actually work yet */
	const char *specs_left_expr[] = { "refs/heads/b2~1:refs/heads/b2" };

	/* expect not NULL to indicate failure (core git replies "funny refname",
	 * other servers may be less pithy. */
	const char *specs_right_expr[] = { "refs/heads/b2:refs/heads/b2~1" };
	push_status exp_stats_right_expr[] = { { "refs/heads/b2~1", 0 } };

	/* TODO: Find a more precise way of checking errors than a exit code of -1. */
	do_push(specs_left_expr, ARRAY_SIZE(specs_left_expr),
		NULL, 0,
		NULL, 0, -1, 0, 0);

	do_push(specs_right_expr, ARRAY_SIZE(specs_right_expr),
		exp_stats_right_expr, ARRAY_SIZE(exp_stats_right_expr),
		NULL, 0, 0, 1, 1);
}

void test_online_push__notes(void)
{
	git_oid note_oid, *target_oid, expected_oid;
	git_signature *signature;
	const char *specs[] = { "refs/notes/commits:refs/notes/commits" };
	push_status exp_stats[] = { { "refs/notes/commits", 1 } };
	expected_ref exp_refs[] = { { "refs/notes/commits", &expected_oid } };
	git_oid_fromstr(&expected_oid, "8461a99b27b7043e58ff6e1f5d2cf07d282534fb");

	target_oid = &_oid_b6;

	/* Create note to push */
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */
	cl_git_pass(git_note_create(&note_oid, _repo, signature, signature, NULL, target_oid, "hello world\n", 0));

	do_push(specs, ARRAY_SIZE(specs),
		exp_stats, ARRAY_SIZE(exp_stats),
		exp_refs, ARRAY_SIZE(exp_refs), 0, 1, 1);

	git_signature_free(signature);
}
