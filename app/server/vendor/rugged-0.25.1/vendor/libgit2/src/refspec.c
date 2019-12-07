/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/errors.h"

#include "common.h"
#include "refspec.h"
#include "util.h"
#include "posix.h"
#include "refs.h"
#include "vector.h"

int git_refspec__parse(git_refspec *refspec, const char *input, bool is_fetch)
{
	// Ported from https://github.com/git/git/blob/f06d47e7e0d9db709ee204ed13a8a7486149f494/remote.c#L518-636

	size_t llen;
	int is_glob = 0;
	const char *lhs, *rhs;
	int flags;

	assert(refspec && input);

	memset(refspec, 0x0, sizeof(git_refspec));
	refspec->push = !is_fetch;

	lhs = input;
	if (*lhs == '+') {
		refspec->force = 1;
		lhs++;
	}

	rhs = strrchr(lhs, ':');

	/*
	 * Before going on, special case ":" (or "+:") as a refspec
	 * for matching refs.
	 */
	if (!is_fetch && rhs == lhs && rhs[1] == '\0') {
		refspec->matching = 1;
		refspec->string = git__strdup(input);
		GITERR_CHECK_ALLOC(refspec->string);
		refspec->src = git__strdup("");
		GITERR_CHECK_ALLOC(refspec->src);
		refspec->dst = git__strdup("");
		GITERR_CHECK_ALLOC(refspec->dst);
		return 0;
	}

	if (rhs) {
		size_t rlen = strlen(++rhs);
		if (rlen || !is_fetch) {
			is_glob = (1 <= rlen && strchr(rhs, '*'));
			refspec->dst = git__strndup(rhs, rlen);
		}
	}

	llen = (rhs ? (size_t)(rhs - lhs - 1) : strlen(lhs));
	if (1 <= llen && memchr(lhs, '*', llen)) {
		if ((rhs && !is_glob) || (!rhs && is_fetch))
			goto invalid;
		is_glob = 1;
	} else if (rhs && is_glob)
		goto invalid;

	refspec->pattern = is_glob;
	refspec->src = git__strndup(lhs, llen);
	flags = GIT_REF_FORMAT_ALLOW_ONELEVEL | GIT_REF_FORMAT_REFSPEC_SHORTHAND
		| (is_glob ? GIT_REF_FORMAT_REFSPEC_PATTERN : 0);

	if (is_fetch) {
		/*
			* LHS
			* - empty is allowed; it means HEAD.
			* - otherwise it must be a valid looking ref.
			*/
		if (!*refspec->src)
			; /* empty is ok */
		else if (!git_reference__is_valid_name(refspec->src, flags))
			goto invalid;
		/*
			* RHS
			* - missing is ok, and is same as empty.
			* - empty is ok; it means not to store.
			* - otherwise it must be a valid looking ref.
			*/
		if (!refspec->dst)
			; /* ok */
		else if (!*refspec->dst)
			; /* ok */
		else if (!git_reference__is_valid_name(refspec->dst, flags))
			goto invalid;
	} else {
		/*
			* LHS
			* - empty is allowed; it means delete.
			* - when wildcarded, it must be a valid looking ref.
			* - otherwise, it must be an extended SHA-1, but
			*   there is no existing way to validate this.
			*/
		if (!*refspec->src)
			; /* empty is ok */
		else if (is_glob) {
			if (!git_reference__is_valid_name(refspec->src, flags))
				goto invalid;
		}
		else {
			; /* anything goes, for now */
		}
		/*
			* RHS
			* - missing is allowed, but LHS then must be a
			*   valid looking ref.
			* - empty is not allowed.
			* - otherwise it must be a valid looking ref.
			*/
		if (!refspec->dst) {
			if (!git_reference__is_valid_name(refspec->src, flags))
				goto invalid;
		} else if (!*refspec->dst) {
			goto invalid;
		} else {
			if (!git_reference__is_valid_name(refspec->dst, flags))
				goto invalid;
		}

		/* if the RHS is empty, then it's a copy of the LHS */
		if (!refspec->dst) {
			refspec->dst = git__strdup(refspec->src);
			GITERR_CHECK_ALLOC(refspec->dst);
		}
	}

	refspec->string = git__strdup(input);
	GITERR_CHECK_ALLOC(refspec->string);

	return 0;

 invalid:
        giterr_set(
                GITERR_INVALID,
                "'%s' is not a valid refspec.", input);
        git_refspec__free(refspec);
	return -1;
}

void git_refspec__free(git_refspec *refspec)
{
	if (refspec == NULL)
		return;

	git__free(refspec->src);
	git__free(refspec->dst);
	git__free(refspec->string);

	memset(refspec, 0x0, sizeof(git_refspec));
}

const char *git_refspec_src(const git_refspec *refspec)
{
	return refspec == NULL ? NULL : refspec->src;
}

const char *git_refspec_dst(const git_refspec *refspec)
{
	return refspec == NULL ? NULL : refspec->dst;
}

const char *git_refspec_string(const git_refspec *refspec)
{
	return refspec == NULL ? NULL : refspec->string;
}

int git_refspec_force(const git_refspec *refspec)
{
	assert(refspec);

	return refspec->force;
}

int git_refspec_src_matches(const git_refspec *refspec, const char *refname)
{
	if (refspec == NULL || refspec->src == NULL)
		return false;

	return (p_fnmatch(refspec->src, refname, 0) == 0);
}

int git_refspec_dst_matches(const git_refspec *refspec, const char *refname)
{
	if (refspec == NULL || refspec->dst == NULL)
		return false;

	return (p_fnmatch(refspec->dst, refname, 0) == 0);
}

static int refspec_transform(
	git_buf *out, const char *from, const char *to, const char *name)
{
	const char *from_star, *to_star;
	const char *name_slash, *from_slash;
	size_t replacement_len, star_offset;

	git_buf_sanitize(out);
	git_buf_clear(out);

	/*
	 * There are two parts to each side of a refspec, the bit
	 * before the star and the bit after it. The star can be in
	 * the middle of the pattern, so we need to look at each bit
	 * individually.
	 */
	from_star = strchr(from, '*');
	to_star = strchr(to, '*');

	assert(from_star && to_star);

	/* star offset, both in 'from' and in 'name' */
	star_offset = from_star - from;

	/* the first half is copied over */
	git_buf_put(out, to, to_star - to);

	/* then we copy over the replacement, from the star's offset to the next slash in 'name' */
	name_slash = strchr(name + star_offset, '/');
	if (!name_slash)
		name_slash = strrchr(name, '\0');

	/* if there is no slash after the star in 'from', we want to copy everything over */
	from_slash = strchr(from + star_offset, '/');
	if (!from_slash)
		name_slash = strrchr(name, '\0');

	replacement_len = (name_slash - name) - star_offset;
	git_buf_put(out, name + star_offset, replacement_len);

	return git_buf_puts(out, to_star + 1);
}

int git_refspec_transform(git_buf *out, const git_refspec *spec, const char *name)
{
	assert(out && spec && name);
	git_buf_sanitize(out);

	if (!git_refspec_src_matches(spec, name)) {
		giterr_set(GITERR_INVALID, "ref '%s' doesn't match the source", name);
		return -1;
	}

	if (!spec->pattern)
		return git_buf_puts(out, spec->dst);

	return refspec_transform(out, spec->src, spec->dst, name);
}

int git_refspec_rtransform(git_buf *out, const git_refspec *spec, const char *name)
{
	assert(out && spec && name);
	git_buf_sanitize(out);

	if (!git_refspec_dst_matches(spec, name)) {
		giterr_set(GITERR_INVALID, "ref '%s' doesn't match the destination", name);
		return -1;
	}

	if (!spec->pattern)
		return git_buf_puts(out, spec->src);

	return refspec_transform(out, spec->dst, spec->src, name);
}

int git_refspec__serialize(git_buf *out, const git_refspec *refspec)
{
	if (refspec->force)
		git_buf_putc(out, '+');

	git_buf_printf(out, "%s:%s",
		refspec->src != NULL ? refspec->src : "",
		refspec->dst != NULL ? refspec->dst : "");

	return git_buf_oom(out) == false;
}

int git_refspec_is_wildcard(const git_refspec *spec)
{
	assert(spec && spec->src);

	return (spec->src[strlen(spec->src) - 1] == '*');
}

git_direction git_refspec_direction(const git_refspec *spec)
{
	assert(spec);

	return spec->push;
}

int git_refspec__dwim_one(git_vector *out, git_refspec *spec, git_vector *refs)
{
	git_buf buf = GIT_BUF_INIT;
	size_t j, pos;
	git_remote_head key;

	const char* formatters[] = {
		GIT_REFS_DIR "%s",
		GIT_REFS_TAGS_DIR "%s",
		GIT_REFS_HEADS_DIR "%s",
		NULL
	};

	git_refspec *cur = git__calloc(1, sizeof(git_refspec));
	GITERR_CHECK_ALLOC(cur);

	cur->force = spec->force;
	cur->push = spec->push;
	cur->pattern = spec->pattern;
	cur->matching = spec->matching;
	cur->string = git__strdup(spec->string);

	/* shorthand on the lhs */
	if (git__prefixcmp(spec->src, GIT_REFS_DIR)) {
		for (j = 0; formatters[j]; j++) {
			git_buf_clear(&buf);
			git_buf_printf(&buf, formatters[j], spec->src);
			GITERR_CHECK_ALLOC_BUF(&buf);

			key.name = (char *) git_buf_cstr(&buf);
			if (!git_vector_search(&pos, refs, &key)) {
				/* we found something to match the shorthand, set src to that */
				cur->src = git_buf_detach(&buf);
			}
		}
	}

	/* No shorthands found, copy over the name */
	if (cur->src == NULL && spec->src != NULL) {
		cur->src = git__strdup(spec->src);
		GITERR_CHECK_ALLOC(cur->src);
	}

	if (spec->dst && git__prefixcmp(spec->dst, GIT_REFS_DIR)) {
		/* if it starts with "remotes" then we just prepend "refs/" */
		if (!git__prefixcmp(spec->dst, "remotes/")) {
			git_buf_puts(&buf, GIT_REFS_DIR);
		} else {
			git_buf_puts(&buf, GIT_REFS_HEADS_DIR);
		}

		git_buf_puts(&buf, spec->dst);
		GITERR_CHECK_ALLOC_BUF(&buf);

		cur->dst = git_buf_detach(&buf);
	}

	git_buf_free(&buf);

	if (cur->dst == NULL && spec->dst != NULL) {
		cur->dst = git__strdup(spec->dst);
		GITERR_CHECK_ALLOC(cur->dst);
	}

	return git_vector_insert(out, cur);
}
