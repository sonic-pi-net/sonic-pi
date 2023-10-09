#include "common.h"

static int use_remote(git_repository *repo, char *name)
{
	git_remote *remote = NULL;
	int error;
	const git_remote_head **refs;
	size_t refs_len, i;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;

	/* Find the remote by name */
	error = git_remote_lookup(&remote, repo, name);
	if (error < 0) {
		error = git_remote_create_anonymous(&remote, repo, name);
		if (error < 0)
			goto cleanup;
	}

	/**
	 * Connect to the remote and call the printing function for
	 * each of the remote references.
	 */
	callbacks.credentials = cred_acquire_cb;

	error = git_remote_connect(remote, GIT_DIRECTION_FETCH, &callbacks, NULL, NULL);
	if (error < 0)
		goto cleanup;

	/**
	 * Get the list of references on the remote and print out
	 * their name next to what they point to.
	 */
	if (git_remote_ls(&refs, &refs_len, remote) < 0)
		goto cleanup;

	for (i = 0; i < refs_len; i++) {
		char oid[GIT_OID_SHA1_HEXSIZE + 1] = {0};
		git_oid_fmt(oid, &refs[i]->oid);
		printf("%s\t%s\n", oid, refs[i]->name);
	}

cleanup:
	git_remote_free(remote);
	return error;
}

/** Entry point for this command */
int lg2_ls_remote(git_repository *repo, int argc, char **argv)
{
	int error;

	if (argc < 2) {
		fprintf(stderr, "usage: %s ls-remote <remote>\n", argv[-1]);
		return EXIT_FAILURE;
	}

	error = use_remote(repo, argv[1]);

	return error;
}
