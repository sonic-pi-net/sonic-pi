/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/oid.h"
#include "git2/refs.h"
#include "git2/revwalk.h"
#include "git2/transport.h"

#include "common.h"
#include "remote.h"
#include "refspec.h"
#include "pack.h"
#include "fetch.h"
#include "netops.h"
#include "repository.h"
#include "refs.h"

static int maybe_want(git_remote *remote, git_remote_head *head, git_odb *odb, git_refspec *tagspec)
{
	int match = 0;

	if (!git_reference_is_valid_name(head->name))
		return 0;

	if (remote->download_tags == GIT_REMOTE_DOWNLOAD_TAGS_ALL) {
		/*
		 * If tagopt is --tags, then we only use the default
		 * tags refspec and ignore the remote's
		 */
		if (git_refspec_src_matches(tagspec, head->name))
			match = 1;
		else
			return 0;
	} else if (git_remote__matching_refspec(remote, head->name))
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

static int filter_wants(git_remote *remote)
{
	git_remote_head **heads;
	git_refspec tagspec, head;
	int error = 0;
	git_odb *odb;
	size_t i, heads_len;

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
		git_refspec__free(&head);

		if (error < 0)
			goto cleanup;
	}

	if (git_repository_odb__weakptr(&odb, remote->repo) < 0)
		goto cleanup;

	if (git_remote_ls((const git_remote_head ***)&heads, &heads_len, remote) < 0)
		goto cleanup;

	for (i = 0; i < heads_len; i++) {
		if ((error = maybe_want(remote, heads[i], odb, &tagspec)) < 0)
			break;
	}

cleanup:
	git_refspec__free(&tagspec);

	return error;
}

/*
 * In this first version, we push all our refs in and start sending
 * them out. When we get an ACK we hide that commit and continue
 * traversing until we're done
 */
int git_fetch_negotiate(git_remote *remote)
{
	git_transport *t = remote->transport;

    remote->need_pack = 0;

	if (filter_wants(remote) < 0) {
		giterr_set(GITERR_NET, "Failed to filter the reference list for wants");
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

int git_fetch_download_pack(git_remote *remote)
{
	git_transport *t = remote->transport;

	if (!remote->need_pack)
		return 0;

	return t->download_pack(t, remote->repo, &remote->stats,
			remote->callbacks.transfer_progress, remote->callbacks.payload);
}
