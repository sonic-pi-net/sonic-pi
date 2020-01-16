/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "fetch.h"

#include "git2/oid.h"
#include "git2/refs.h"
#include "git2/revwalk.h"
#include "git2/transport.h"

#include "remote.h"
#include "refspec.h"
#include "pack.h"
#include "netops.h"
#include "repository.h"
#include "refs.h"

static int maybe_want(git_remote *remote, git_remote_head *head, git_odb *odb, git_refspec *tagspec, git_remote_autotag_option_t tagopt)
{
	int match = 0;

	if (!git_reference_is_valid_name(head->name))
		return 0;

	if (tagopt == GIT_REMOTE_DOWNLOAD_TAGS_ALL) {
		/*
		 * If tagopt is --tags, always request tags
		 * in addition to the remote's refspecs
		 */
		if (git_refspec_src_matches(tagspec, head->name))
			match = 1;
	}

	if (!match && git_remote__matching_refspec(remote, head->name))
		match = 1;

	if (!match)
		return 0;

	/* If we have the object, mark it so we don't ask for it */
	if (git_odb_exists(odb, &head->oid)) {
		head->local = 1;
	}
	else
		remote->need_pack = 1;

	return git_vector_insert(&remote->refs, head);
}

static int filter_wants(git_remote *remote, const git_fetch_options *opts)
{
	git_remote_head **heads;
	git_refspec tagspec, head;
	int error = 0;
	git_odb *odb;
	size_t i, heads_len;
	git_remote_autotag_option_t tagopt = remote->download_tags;

	if (opts && opts->download_tags != GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED)
		tagopt = opts->download_tags;

	git_vector_clear(&remote->refs);
	if ((error = git_refspec__parse(&tagspec, GIT_REFSPEC_TAGS, true)) < 0)
		return error;

	/*
	 * The fetch refspec can be NULL, and what this means is that the
	 * user didn't specify one. This is fine, as it means that we're
	 * not interested in any particular branch but just the remote's
	 * HEAD, which will be stored in FETCH_HEAD after the fetch.
	 */
	if (remote->active_refspecs.length == 0) {
		if ((error = git_refspec__parse(&head, "HEAD", true)) < 0)
			goto cleanup;

		error = git_refspec__dwim_one(&remote->active_refspecs, &head, &remote->refs);
		git_refspec__dispose(&head);

		if (error < 0)
			goto cleanup;
	}

	if (git_repository_odb__weakptr(&odb, remote->repo) < 0)
		goto cleanup;

	if (git_remote_ls((const git_remote_head ***)&heads, &heads_len, remote) < 0)
		goto cleanup;

	for (i = 0; i < heads_len; i++) {
		if ((error = maybe_want(remote, heads[i], odb, &tagspec, tagopt)) < 0)
			break;
	}

cleanup:
	git_refspec__dispose(&tagspec);

	return error;
}

/*
 * In this first version, we push all our refs in and start sending
 * them out. When we get an ACK we hide that commit and continue
 * traversing until we're done
 */
int git_fetch_negotiate(git_remote *remote, const git_fetch_options *opts)
{
	git_transport *t = remote->transport;

	remote->need_pack = 0;

	if (filter_wants(remote, opts) < 0) {
		git_error_set(GIT_ERROR_NET, "failed to filter the reference list for wants");
		return -1;
	}

	/* Don't try to negotiate when we don't want anything */
	if (!remote->need_pack)
		return 0;

	/*
	 * Now we have everything set up so we can start tell the
	 * server what we want and what we have.
	 */
	return t->negotiate_fetch(t,
		remote->repo,
		(const git_remote_head * const *)remote->refs.contents,
		remote->refs.length);
}

int git_fetch_download_pack(git_remote *remote, const git_remote_callbacks *callbacks)
{
	git_transport *t = remote->transport;
	git_indexer_progress_cb progress = NULL;
	void *payload = NULL;

	if (!remote->need_pack)
		return 0;

	if (callbacks) {
		progress = callbacks->transfer_progress;
		payload  = callbacks->payload;
	}

	return t->download_pack(t, remote->repo, &remote->stats, progress, payload);
}

int git_fetch_options_init(git_fetch_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_fetch_options, GIT_FETCH_OPTIONS_INIT);
	return 0;
}

int git_fetch_init_options(git_fetch_options *opts, unsigned int version)
{
	return git_fetch_options_init(opts, version);
}
