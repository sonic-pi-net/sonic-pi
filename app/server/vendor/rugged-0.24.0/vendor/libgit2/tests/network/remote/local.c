#include "clar_libgit2.h"
#include "buffer.h"
#include "path.h"
#include "posix.h"

static git_repository *repo;
static git_buf file_path_buf = GIT_BUF_INIT;
static git_remote *remote;

static char *push_refspec_strings[] = {
	"refs/heads/master",
};
static git_strarray push_array = {
	push_refspec_strings,
	1,
};

void test_network_remote_local__initialize(void)
{
	cl_git_pass(git_repository_init(&repo, "remotelocal/", 0));
	cl_git_pass(git_repository_set_ident(repo, "Foo Bar", "foo@example.com"));
	cl_assert(repo != NULL);
}

void test_network_remote_local__cleanup(void)
{
	git_buf_free(&file_path_buf);

	git_remote_free(remote);
	remote = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_fixture_cleanup("remotelocal");
}

static void connect_to_local_repository(const char *local_repository)
{
	git_buf_sets(&file_path_buf, cl_git_path_url(local_repository));

	cl_git_pass(git_remote_create_anonymous(&remote, repo, git_buf_cstr(&file_path_buf)));
	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL));
}

void test_network_remote_local__connected(void)
{
	connect_to_local_repository(cl_fixture("testrepo.git"));
	cl_assert(git_remote_connected(remote));

	git_remote_disconnect(remote);
	cl_assert(!git_remote_connected(remote));
}

void test_network_remote_local__retrieve_advertised_references(void)
{
	const git_remote_head **refs;
	size_t refs_len;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_ls(&refs, &refs_len, remote));

	cl_assert_equal_i(refs_len, 28);
}

void test_network_remote_local__retrieve_advertised_before_connect(void)
{
	const git_remote_head **refs;
	size_t refs_len = 0;

	git_buf_sets(&file_path_buf, cl_git_path_url(cl_fixture("testrepo.git")));

	cl_git_pass(git_remote_create_anonymous(&remote, repo, git_buf_cstr(&file_path_buf)));
	cl_git_fail(git_remote_ls(&refs, &refs_len, remote));
}

void test_network_remote_local__retrieve_advertised_references_after_disconnect(void)
{
	const git_remote_head **refs;
	size_t refs_len;

	connect_to_local_repository(cl_fixture("testrepo.git"));
	git_remote_disconnect(remote);

	cl_git_pass(git_remote_ls(&refs, &refs_len, remote));

	cl_assert_equal_i(refs_len, 28);
}

void test_network_remote_local__retrieve_advertised_references_from_spaced_repository(void)
{
	const git_remote_head **refs;
	size_t refs_len;

	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(p_rename("testrepo.git", "spaced testrepo.git"));

	connect_to_local_repository("spaced testrepo.git");

	cl_git_pass(git_remote_ls(&refs, &refs_len, remote));

	cl_assert_equal_i(refs_len, 28);

	git_remote_free(remote);	/* Disconnect from the "spaced repo" before the cleanup */
	remote = NULL;

	cl_fixture_cleanup("spaced testrepo.git");
}

void test_network_remote_local__nested_tags_are_completely_peeled(void)
{
	const git_remote_head **refs;
	size_t refs_len, i;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_ls(&refs, &refs_len, remote));

	for (i = 0; i < refs_len; i++) {
		if (!strcmp(refs[i]->name, "refs/tags/test^{}"))
			cl_git_pass(git_oid_streq(&refs[i]->oid, "e90810b8df3e80c413d903f631643c716887138d"));
	}
}

void test_network_remote_local__shorthand_fetch_refspec0(void)
{
	char *refspec_strings[] = {
		"master:remotes/sloppy/master",
		"master:boh/sloppy/master",
	};
	git_strarray array = {
		refspec_strings,
		2,
	};

	git_reference *ref;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/sloppy/master"));
	git_reference_free(ref);

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/heads/boh/sloppy/master"));
	git_reference_free(ref);
}

void test_network_remote_local__shorthand_fetch_refspec1(void)
{
	char *refspec_strings[] = {
		"master",
		"hard_tag",
	};
	git_strarray array = {
		refspec_strings,
		2,
	};

	git_reference *ref;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	cl_git_fail(git_reference_lookup(&ref, repo, "refs/remotes/origin/master"));
	cl_git_fail(git_reference_lookup(&ref, repo, "refs/tags/hard_tag"));
}

void test_network_remote_local__tagopt(void)
{
	git_reference *ref;
	git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;

	cl_git_pass(git_remote_create(&remote, repo, "tagopt", cl_git_path_url(cl_fixture("testrepo.git"))));
	fetch_opts.download_tags = GIT_REMOTE_DOWNLOAD_TAGS_ALL;
	cl_git_pass(git_remote_fetch(remote, NULL, &fetch_opts, NULL));

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/tagopt/master"));
	git_reference_free(ref);
	cl_git_pass(git_reference_lookup(&ref, repo, "refs/tags/hard_tag"));
	git_reference_free(ref);

	fetch_opts.download_tags = GIT_REMOTE_DOWNLOAD_TAGS_AUTO;
	cl_git_pass(git_remote_fetch(remote, NULL, &fetch_opts, NULL));
	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/tagopt/master"));
	git_reference_free(ref);
}

void test_network_remote_local__push_to_bare_remote(void)
{
	char *refspec_strings[] = {
		"master:master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};

	/* Should be able to push to a bare remote */
	git_remote *localremote;

	/* Get some commits */
	connect_to_local_repository(cl_fixture("testrepo.git"));
	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	/* Set up an empty bare repo to push into */
	{
		git_repository *localbarerepo;
		cl_git_pass(git_repository_init(&localbarerepo, "./localbare.git", 1));
		git_repository_free(localbarerepo);
	}

	/* Connect to the bare repo */
	cl_git_pass(git_remote_create_anonymous(&localremote, repo, "./localbare.git"));
	cl_git_pass(git_remote_connect(localremote, GIT_DIRECTION_PUSH, NULL, NULL));

	/* Try to push */
	cl_git_pass(git_remote_upload(localremote, &push_array, NULL));

	/* Clean up */
	git_remote_free(localremote);
	cl_fixture_cleanup("localbare.git");
}

void test_network_remote_local__push_to_bare_remote_with_file_url(void)
{
	char *refspec_strings[] = {
		"master:master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};
	/* Should be able to push to a bare remote */
	git_remote *localremote;
	const char *url;

	/* Get some commits */
	connect_to_local_repository(cl_fixture("testrepo.git"));
	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	/* Set up an empty bare repo to push into */
	{
		git_repository *localbarerepo;
		cl_git_pass(git_repository_init(&localbarerepo, "./localbare.git", 1));
		git_repository_free(localbarerepo);
	}

	/* Create a file URL */
	url = cl_git_path_url("./localbare.git");

	/* Connect to the bare repo */
	cl_git_pass(git_remote_create_anonymous(&localremote, repo, url));
	cl_git_pass(git_remote_connect(localremote, GIT_DIRECTION_PUSH, NULL, NULL));

	/* Try to push */
	cl_git_pass(git_remote_upload(localremote, &push_array, NULL));

	/* Clean up */
	git_remote_free(localremote);
	cl_fixture_cleanup("localbare.git");
}


void test_network_remote_local__push_to_non_bare_remote(void)
{
	char *refspec_strings[] = {
		"master:master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};
	/* Shouldn't be able to push to a non-bare remote */
	git_remote *localremote;
	git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;

	/* Get some commits */
	connect_to_local_repository(cl_fixture("testrepo.git"));
	cl_git_pass(git_remote_fetch(remote, &array, &fetch_opts, NULL));

	/* Set up an empty non-bare repo to push into */
	{
		git_repository *remoterepo = NULL;
		cl_git_pass(git_repository_init(&remoterepo, "localnonbare", 0));
		git_repository_free(remoterepo);
	}

	/* Connect to the bare repo */
	cl_git_pass(git_remote_create_anonymous(&localremote, repo, "./localnonbare"));
	cl_git_pass(git_remote_connect(localremote, GIT_DIRECTION_PUSH, NULL, NULL));

	/* Try to push */
	cl_git_fail_with(GIT_EBAREREPO, git_remote_upload(localremote, &push_array, NULL));

	/* Clean up */
	git_remote_free(localremote);
	cl_fixture_cleanup("localbare.git");
}

void test_network_remote_local__fetch(void)
{
	char *refspec_strings[] = {
		"master:remotes/sloppy/master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};

	git_reflog *log;
	const git_reflog_entry *entry;
	git_reference *ref;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_fetch(remote, &array, NULL, "UPDAAAAAATE!!"));

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/sloppy/master"));
	git_reference_free(ref);

	cl_git_pass(git_reflog_read(&log, repo, "refs/remotes/sloppy/master"));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);
	cl_assert_equal_s("UPDAAAAAATE!!", git_reflog_entry_message(entry));

	git_reflog_free(log);
}

void test_network_remote_local__reflog(void)
{
	char *refspec_strings[] = {
		"master:remotes/sloppy/master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};

	git_reflog *log;
	const git_reflog_entry *entry;

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_fetch(remote, &array, NULL, "UPDAAAAAATE!!"));

	cl_git_pass(git_reflog_read(&log, repo, "refs/remotes/sloppy/master"));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);
	cl_assert_equal_s("UPDAAAAAATE!!", git_reflog_entry_message(entry));

	git_reflog_free(log);
}

void test_network_remote_local__fetch_default_reflog_message(void)
{
	char *refspec_strings[] = {
		"master:remotes/sloppy/master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};

	git_reflog *log;
	const git_reflog_entry *entry;
	char expected_reflog_msg[1024];

	connect_to_local_repository(cl_fixture("testrepo.git"));

	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	cl_git_pass(git_reflog_read(&log, repo, "refs/remotes/sloppy/master"));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	sprintf(expected_reflog_msg, "fetch %s", git_remote_url(remote));
	cl_assert_equal_s(expected_reflog_msg, git_reflog_entry_message(entry));

	git_reflog_free(log);
}

void test_network_remote_local__opportunistic_update(void)
{
	git_reference *ref;
	char *refspec_strings[] = {
		"master",
	};
	git_strarray array = {
		refspec_strings,
		1,
	};

	/* this remote has a passive refspec of "refs/heads/<star>:refs/remotes/origin/<star>" */
	cl_git_pass(git_remote_create(&remote, repo, "origin", cl_git_fixture_url("testrepo.git")));
	/* and we pass the active refspec "master" */
	cl_git_pass(git_remote_fetch(remote, &array, NULL, NULL));

	/* and we expect that to update our copy of origin's master */
	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/origin/master"));
	git_reference_free(ref);
}

void test_network_remote_local__update_tips_for_new_remote(void) {
	git_repository *src_repo;
	git_repository *dst_repo;
	git_remote *new_remote;
	git_reference* branch;

	/* Copy test repo */
	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&src_repo, "testrepo.git"));

	/* Set up an empty bare repo to push into */
	cl_git_pass(git_repository_init(&dst_repo, "./localbare.git", 1));

	/* Push to bare repo */
	cl_git_pass(git_remote_create(&new_remote, src_repo, "bare", "./localbare.git"));
	cl_git_pass(git_remote_push(new_remote, &push_array, NULL));
	/* Make sure remote branch has been created */
	cl_git_pass(git_branch_lookup(&branch, src_repo, "bare/master", GIT_BRANCH_REMOTE));

	git_reference_free(branch);
	git_remote_free(new_remote);
	git_repository_free(dst_repo);
	cl_fixture_cleanup("localbare.git");
	git_repository_free(src_repo);
	cl_fixture_cleanup("testrepo.git");
}

void test_network_remote_local__push_delete(void)
{
	git_repository *src_repo;
	git_repository *dst_repo;
	git_remote *remote;
	git_reference *ref;
	char *spec_push[] = { "refs/heads/master" };
	char *spec_delete[] = { ":refs/heads/master" };
	git_strarray specs = {
		spec_push,
		1,
	};

	src_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_init(&dst_repo, "target.git", 1));

	cl_git_pass(git_remote_create(&remote, src_repo, "origin", "./target.git"));

	/* Push the master branch and verify it's there */
	cl_git_pass(git_remote_push(remote, &specs, NULL));
	cl_git_pass(git_reference_lookup(&ref, dst_repo, "refs/heads/master"));
	git_reference_free(ref);

	specs.strings = spec_delete;
	cl_git_pass(git_remote_push(remote, &specs, NULL));
	cl_git_fail(git_reference_lookup(&ref, dst_repo, "refs/heads/master"));

	git_remote_free(remote);
	git_repository_free(dst_repo);
	cl_fixture_cleanup("target.git");
	cl_git_sandbox_cleanup();
}
