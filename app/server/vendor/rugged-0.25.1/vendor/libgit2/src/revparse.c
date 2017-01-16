/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <assert.h>

#include "common.h"
#include "buffer.h"
#include "tree.h"
#include "refdb.h"

#include "git2.h"

static int maybe_sha_or_abbrev(git_object** out, git_repository *repo, const char *spec, size_t speclen)
{
	git_oid oid;

	if (git_oid_fromstrn(&oid, spec, speclen) < 0)
		return GIT_ENOTFOUND;

	return git_object_lookup_prefix(out, repo, &oid, speclen, GIT_OBJ_ANY);
}

static int maybe_sha(git_object** out, git_repository *repo, const char *spec)
{
	size_t speclen = strlen(spec);

	if (speclen != GIT_OID_HEXSZ)
		return GIT_ENOTFOUND;

	return maybe_sha_or_abbrev(out, repo, spec, speclen);
}

static int maybe_abbrev(git_object** out, git_repository *repo, const char *spec)
{
	size_t speclen = strlen(spec);

	return maybe_sha_or_abbrev(out, repo, spec, speclen);
}

static int build_regex(regex_t *regex, const char *pattern)
{
	int error;

	if (*pattern == '\0') {
		giterr_set(GITERR_REGEX, "Empty pattern");
		return GIT_EINVALIDSPEC;
	}

	error = p_regcomp(regex, pattern, REG_EXTENDED);
	if (!error)
		return 0;

	error = giterr_set_regex(regex, error);

	regfree(regex);

	return error;
}

static int maybe_describe(git_object**out, git_repository *repo, const char *spec)
{
	const char *substr;
	int error;
	regex_t regex;

	substr = strstr(spec, "-g");

	if (substr == NULL)
		return GIT_ENOTFOUND;

	if (build_regex(&regex, ".+-[0-9]+-g[0-9a-fA-F]+") < 0)
		return -1;

	error = regexec(&regex, spec, 0, NULL, 0);
	regfree(&regex);

	if (error)
		return GIT_ENOTFOUND;

	return maybe_abbrev(out, repo, substr+2);
}

static int revparse_lookup_object(
	git_object **object_out,
	git_reference **reference_out,
	git_repository *repo,
	const char *spec)
{
	int error;
	git_reference *ref;

	if ((error = maybe_sha(object_out, repo, spec)) != GIT_ENOTFOUND)
		return error;

	error = git_reference_dwim(&ref, repo, spec);
	if (!error) {

		error = git_object_lookup(
			object_out, repo, git_reference_target(ref), GIT_OBJ_ANY);

		if (!error)
			*reference_out = ref;

		return error;
	}

	if (error != GIT_ENOTFOUND)
		return error;

	if ((strlen(spec) < GIT_OID_HEXSZ) &&
		((error = maybe_abbrev(object_out, repo, spec)) != GIT_ENOTFOUND))
			return error;

	if ((error = maybe_describe(object_out, repo, spec)) != GIT_ENOTFOUND)
		return error;

	giterr_set(GITERR_REFERENCE, "Revspec '%s' not found.", spec);
	return GIT_ENOTFOUND;
}

static int try_parse_numeric(int *n, const char *curly_braces_content)
{
	int32_t content;
	const char *end_ptr;

	if (git__strtol32(&content, curly_braces_content, &end_ptr, 10) < 0)
		return -1;

	if (*end_ptr != '\0')
		return -1;

	*n = (int)content;
	return 0;
}

static int retrieve_previously_checked_out_branch_or_revision(git_object **out, git_reference **base_ref, git_repository *repo, const char *identifier, size_t position)
{
	git_reference *ref = NULL;
	git_reflog *reflog = NULL;
	regex_t preg;
	int error = -1;
	size_t i, numentries, cur;
	const git_reflog_entry *entry;
	const char *msg;
	regmatch_t regexmatches[2];
	git_buf buf = GIT_BUF_INIT;

	cur = position;

	if (*identifier != '\0' || *base_ref != NULL)
		return GIT_EINVALIDSPEC;

	if (build_regex(&preg, "checkout: moving from (.*) to .*") < 0)
		return -1;

	if (git_reference_lookup(&ref, repo, GIT_HEAD_FILE) < 0)
		goto cleanup;

	if (git_reflog_read(&reflog, repo, GIT_HEAD_FILE) < 0)
		goto cleanup;

	numentries  = git_reflog_entrycount(reflog);

	for (i = 0; i < numentries; i++) {
		entry = git_reflog_entry_byindex(reflog, i);
		msg = git_reflog_entry_message(entry);
		if (!msg)
			continue;

		if (regexec(&preg, msg, 2, regexmatches, 0))
			continue;

		cur--;

		if (cur > 0)
			continue;

		git_buf_put(&buf, msg+regexmatches[1].rm_so, regexmatches[1].rm_eo - regexmatches[1].rm_so);

		if ((error = git_reference_dwim(base_ref, repo, git_buf_cstr(&buf))) == 0)
			goto cleanup;

		if (error < 0 && error != GIT_ENOTFOUND)
			goto cleanup;

		error = maybe_abbrev(out, repo, git_buf_cstr(&buf));

		goto cleanup;
	}

	error = GIT_ENOTFOUND;

cleanup:
	git_reference_free(ref);
	git_buf_free(&buf);
	regfree(&preg);
	git_reflog_free(reflog);
	return error;
}

static int retrieve_oid_from_reflog(git_oid *oid, git_reference *ref, size_t identifier)
{
	git_reflog *reflog;
	size_t numentries;
	const git_reflog_entry *entry;
	bool search_by_pos = (identifier <= 100000000);

	if (git_reflog_read(&reflog, git_reference_owner(ref), git_reference_name(ref)) < 0)
		return -1;

	numentries = git_reflog_entrycount(reflog);

	if (search_by_pos) {
		if (numentries < identifier + 1)
			goto notfound;

		entry = git_reflog_entry_byindex(reflog, identifier);
		git_oid_cpy(oid, git_reflog_entry_id_new(entry));
	} else {
		size_t i;
		git_time commit_time;

		for (i = 0; i < numentries; i++) {
			entry = git_reflog_entry_byindex(reflog, i);
			commit_time = git_reflog_entry_committer(entry)->when;

			if (commit_time.time > (git_time_t)identifier)
				continue;

			git_oid_cpy(oid, git_reflog_entry_id_new(entry));
			break;
		}

		if (i == numentries)
			goto notfound;
	}

	git_reflog_free(reflog);
	return 0;

notfound:
	giterr_set(
		GITERR_REFERENCE,
		"Reflog for '%s' has only %"PRIuZ" entries, asked for %"PRIuZ,
		git_reference_name(ref), numentries, identifier);

	git_reflog_free(reflog);
	return GIT_ENOTFOUND;
}

static int retrieve_revobject_from_reflog(git_object **out, git_reference **base_ref, git_repository *repo, const char *identifier, size_t position)
{
	git_reference *ref;
	git_oid oid;
	int error = -1;

	if (*base_ref == NULL) {
		if ((error = git_reference_dwim(&ref, repo, identifier)) < 0)
			return error;
	} else {
		ref = *base_ref;
		*base_ref = NULL;
	}

	if (position == 0) {
		error = git_object_lookup(out, repo, git_reference_target(ref), GIT_OBJ_ANY);
		goto cleanup;
	}

	if ((error = retrieve_oid_from_reflog(&oid, ref, position)) < 0)
		goto cleanup;

	error = git_object_lookup(out, repo, &oid, GIT_OBJ_ANY);

cleanup:
	git_reference_free(ref);
	return error;
}

static int retrieve_remote_tracking_reference(git_reference **base_ref, const char *identifier, git_repository *repo)
{
	git_reference *tracking, *ref;
	int error = -1;

	if (*base_ref == NULL) {
		if ((error = git_reference_dwim(&ref, repo, identifier)) < 0)
			return error;
	} else {
		ref = *base_ref;
		*base_ref = NULL;
	}

	if (!git_reference_is_branch(ref)) {
		error = GIT_EINVALIDSPEC;
		goto cleanup;
	}

	if ((error = git_branch_upstream(&tracking, ref)) < 0)
		goto cleanup;

	*base_ref = tracking;

cleanup:
	git_reference_free(ref);
	return error;
}

static int handle_at_syntax(git_object **out, git_reference **ref, const char *spec, size_t identifier_len, git_repository* repo, const char *curly_braces_content)
{
	bool is_numeric;
	int parsed = 0, error = -1;
	git_buf identifier = GIT_BUF_INIT;
	git_time_t timestamp;

	assert(*out == NULL);

	if (git_buf_put(&identifier, spec, identifier_len) < 0)
		return -1;

	is_numeric = !try_parse_numeric(&parsed, curly_braces_content);

	if (*curly_braces_content == '-' && (!is_numeric || parsed == 0)) {
		error = GIT_EINVALIDSPEC;
		goto cleanup;
	}

	if (is_numeric) {
		if (parsed < 0)
			error = retrieve_previously_checked_out_branch_or_revision(out, ref, repo, git_buf_cstr(&identifier), -parsed);
		else
			error = retrieve_revobject_from_reflog(out, ref, repo, git_buf_cstr(&identifier), parsed);

		goto cleanup;
	}

	if (!strcmp(curly_braces_content, "u") || !strcmp(curly_braces_content, "upstream")) {
		error = retrieve_remote_tracking_reference(ref, git_buf_cstr(&identifier), repo);

		goto cleanup;
	}

	if (git__date_parse(&timestamp, curly_braces_content) < 0)
		goto cleanup;

	error = retrieve_revobject_from_reflog(out, ref, repo, git_buf_cstr(&identifier), (size_t)timestamp);

cleanup:
	git_buf_free(&identifier);
	return error;
}

static git_otype parse_obj_type(const char *str)
{
	if (!strcmp(str, "commit"))
		return GIT_OBJ_COMMIT;

	if (!strcmp(str, "tree"))
		return GIT_OBJ_TREE;

	if (!strcmp(str, "blob"))
		return GIT_OBJ_BLOB;

	if (!strcmp(str, "tag"))
		return GIT_OBJ_TAG;

	return GIT_OBJ_BAD;
}

static int dereference_to_non_tag(git_object **out, git_object *obj)
{
	if (git_object_type(obj) == GIT_OBJ_TAG)
		return git_tag_peel(out, (git_tag *)obj);

	return git_object_dup(out, obj);
}

static int handle_caret_parent_syntax(git_object **out, git_object *obj, int n)
{
	git_object *temp_commit = NULL;
	int error;

	if ((error = git_object_peel(&temp_commit, obj, GIT_OBJ_COMMIT)) < 0)
		return (error == GIT_EAMBIGUOUS || error == GIT_ENOTFOUND) ?
			GIT_EINVALIDSPEC : error;

	if (n == 0) {
		*out = temp_commit;
		return 0;
	}

	error = git_commit_parent((git_commit **)out, (git_commit*)temp_commit, n - 1);

	git_object_free(temp_commit);
	return error;
}

static int handle_linear_syntax(git_object **out, git_object *obj, int n)
{
	git_object *temp_commit = NULL;
	int error;

	if ((error = git_object_peel(&temp_commit, obj, GIT_OBJ_COMMIT)) < 0)
		return (error == GIT_EAMBIGUOUS || error == GIT_ENOTFOUND) ?
			GIT_EINVALIDSPEC : error;

	error = git_commit_nth_gen_ancestor((git_commit **)out, (git_commit*)temp_commit, n);

	git_object_free(temp_commit);
	return error;
}

static int handle_colon_syntax(
	git_object **out,
	git_object *obj,
	const char *path)
{
	git_object *tree;
	int error = -1;
	git_tree_entry *entry = NULL;

	if ((error = git_object_peel(&tree, obj, GIT_OBJ_TREE)) < 0)
		return error == GIT_ENOTFOUND ? GIT_EINVALIDSPEC : error;

	if (*path == '\0') {
		*out = tree;
		return 0;
	}

	/*
	 * TODO: Handle the relative path syntax
	 * (:./relative/path and :../relative/path)
	 */
	if ((error = git_tree_entry_bypath(&entry, (git_tree *)tree, path)) < 0)
		goto cleanup;

	error = git_tree_entry_to_object(out, git_object_owner(tree), entry);

cleanup:
	git_tree_entry_free(entry);
	git_object_free(tree);

	return error;
}

static int walk_and_search(git_object **out, git_revwalk *walk, regex_t *regex)
{
	int error;
	git_oid oid;
	git_object *obj;

	while (!(error = git_revwalk_next(&oid, walk))) {

		error = git_object_lookup(&obj, git_revwalk_repository(walk), &oid, GIT_OBJ_COMMIT);
		if ((error < 0) && (error != GIT_ENOTFOUND))
			return -1;

		if (!regexec(regex, git_commit_message((git_commit*)obj), 0, NULL, 0)) {
			*out = obj;
			return 0;
		}

		git_object_free(obj);
	}

	if (error < 0 && error == GIT_ITEROVER)
		error = GIT_ENOTFOUND;

	return error;
}

static int handle_grep_syntax(git_object **out, git_repository *repo, const git_oid *spec_oid, const char *pattern)
{
	regex_t preg;
	git_revwalk *walk = NULL;
	int error;

	if ((error = build_regex(&preg, pattern)) < 0)
		return error;

	if ((error = git_revwalk_new(&walk, repo)) < 0)
		goto cleanup;

	git_revwalk_sorting(walk, GIT_SORT_TIME);

	if (spec_oid == NULL) {
		if ((error = git_revwalk_push_glob(walk, "refs/*")) < 0)
			goto cleanup;
	} else if ((error = git_revwalk_push(walk, spec_oid)) < 0)
			goto cleanup;

	error = walk_and_search(out, walk, &preg);

cleanup:
	regfree(&preg);
	git_revwalk_free(walk);

	return error;
}

static int handle_caret_curly_syntax(git_object **out, git_object *obj, const char *curly_braces_content)
{
	git_otype expected_type;

	if (*curly_braces_content == '\0')
		return dereference_to_non_tag(out, obj);

	if (*curly_braces_content == '/')
		return handle_grep_syntax(out, git_object_owner(obj), git_object_id(obj), curly_braces_content + 1);

	expected_type = parse_obj_type(curly_braces_content);

	if (expected_type == GIT_OBJ_BAD)
		return GIT_EINVALIDSPEC;

	return git_object_peel(out, obj, expected_type);
}

static int extract_curly_braces_content(git_buf *buf, const char *spec, size_t *pos)
{
	git_buf_clear(buf);

	assert(spec[*pos] == '^' || spec[*pos] == '@');

	(*pos)++;

	if (spec[*pos] == '\0' || spec[*pos] != '{')
		return GIT_EINVALIDSPEC;

	(*pos)++;

	while (spec[*pos] != '}') {
		if (spec[*pos] == '\0')
			return GIT_EINVALIDSPEC;

		git_buf_putc(buf, spec[(*pos)++]);
	}

	(*pos)++;

	return 0;
}

static int extract_path(git_buf *buf, const char *spec, size_t *pos)
{
	git_buf_clear(buf);

	assert(spec[*pos] == ':');

	(*pos)++;

	if (git_buf_puts(buf, spec + *pos) < 0)
		return -1;

	*pos += git_buf_len(buf);

	return 0;
}

static int extract_how_many(int *n, const char *spec, size_t *pos)
{
	const char *end_ptr;
	int parsed, accumulated;
	char kind = spec[*pos];

	assert(spec[*pos] == '^' || spec[*pos] == '~');

	accumulated = 0;

	do {
		do {
			(*pos)++;
			accumulated++;
		} while (spec[(*pos)] == kind && kind == '~');

		if (git__isdigit(spec[*pos])) {
			if (git__strtol32(&parsed, spec + *pos, &end_ptr, 10) < 0)
				return GIT_EINVALIDSPEC;

			accumulated += (parsed - 1);
			*pos = end_ptr - spec;
		}

	} 	while (spec[(*pos)] == kind && kind == '~');

	*n = accumulated;

	return 0;
}

static int object_from_reference(git_object **object, git_reference *reference)
{
	git_reference *resolved = NULL;
	int error;

	if (git_reference_resolve(&resolved, reference) < 0)
		return -1;

	error = git_object_lookup(object, reference->db->repo, git_reference_target(resolved), GIT_OBJ_ANY);
	git_reference_free(resolved);

	return error;
}

static int ensure_base_rev_loaded(git_object **object, git_reference **reference, const char *spec, size_t identifier_len, git_repository *repo, bool allow_empty_identifier)
{
	int error;
	git_buf identifier = GIT_BUF_INIT;

	if (*object != NULL)
		return 0;

	if (*reference != NULL)
		return object_from_reference(object, *reference);

	if (!allow_empty_identifier && identifier_len == 0)
		return GIT_EINVALIDSPEC;

	if (git_buf_put(&identifier, spec, identifier_len) < 0)
		return -1;

	error = revparse_lookup_object(object, reference, repo, git_buf_cstr(&identifier));
	git_buf_free(&identifier);

	return error;
}

static int ensure_base_rev_is_not_known_yet(git_object *object)
{
	if (object == NULL)
		return 0;

	return GIT_EINVALIDSPEC;
}

static bool any_left_hand_identifier(git_object *object, git_reference *reference, size_t identifier_len)
{
	if (object != NULL)
		return true;

	if (reference != NULL)
		return true;

	if (identifier_len > 0)
		return true;

	return false;
}

static int ensure_left_hand_identifier_is_not_known_yet(git_object *object, git_reference *reference)
{
	if (!ensure_base_rev_is_not_known_yet(object) && reference == NULL)
		return 0;

	return GIT_EINVALIDSPEC;
}

int revparse__ext(
	git_object **object_out,
	git_reference **reference_out,
	size_t *identifier_len_out,
	git_repository *repo,
	const char *spec)
{
	size_t pos = 0, identifier_len = 0;
	int error = -1, n;
	git_buf buf = GIT_BUF_INIT;

	git_reference *reference = NULL;
	git_object *base_rev = NULL;

	bool should_return_reference = true;

	assert(object_out && reference_out && repo && spec);

	*object_out = NULL;
	*reference_out = NULL;

	while (spec[pos]) {
		switch (spec[pos]) {
		case '^':
			should_return_reference = false;

			if ((error = ensure_base_rev_loaded(&base_rev, &reference, spec, identifier_len, repo, false)) < 0)
				goto cleanup;

			if (spec[pos+1] == '{') {
				git_object *temp_object = NULL;

				if ((error = extract_curly_braces_content(&buf, spec, &pos)) < 0)
					goto cleanup;

				if ((error = handle_caret_curly_syntax(&temp_object, base_rev, git_buf_cstr(&buf))) < 0)
					goto cleanup;

				git_object_free(base_rev);
				base_rev = temp_object;
			} else {
				git_object *temp_object = NULL;

				if ((error = extract_how_many(&n, spec, &pos)) < 0)
					goto cleanup;

				if ((error = handle_caret_parent_syntax(&temp_object, base_rev, n)) < 0)
					goto cleanup;

				git_object_free(base_rev);
				base_rev = temp_object;
			}
			break;

		case '~':
		{
			git_object *temp_object = NULL;

			should_return_reference = false;

			if ((error = extract_how_many(&n, spec, &pos)) < 0)
				goto cleanup;

			if ((error = ensure_base_rev_loaded(&base_rev, &reference, spec, identifier_len, repo, false)) < 0)
				goto cleanup;

			if ((error = handle_linear_syntax(&temp_object, base_rev, n)) < 0)
				goto cleanup;

			git_object_free(base_rev);
			base_rev = temp_object;
			break;
		}

		case ':':
		{
			git_object *temp_object = NULL;

			should_return_reference = false;

			if ((error = extract_path(&buf, spec, &pos)) < 0)
				goto cleanup;

			if (any_left_hand_identifier(base_rev, reference, identifier_len)) {
				if ((error = ensure_base_rev_loaded(&base_rev, &reference, spec, identifier_len, repo, true)) < 0)
					goto cleanup;

				if ((error = handle_colon_syntax(&temp_object, base_rev, git_buf_cstr(&buf))) < 0)
					goto cleanup;
			} else {
				if (*git_buf_cstr(&buf) == '/') {
					if ((error = handle_grep_syntax(&temp_object, repo, NULL, git_buf_cstr(&buf) + 1)) < 0)
						goto cleanup;
				} else {

					/*
					 * TODO: support merge-stage path lookup (":2:Makefile")
					 * and plain index blob lookup (:i-am/a/blob)
					 */
					giterr_set(GITERR_INVALID, "Unimplemented");
					error = GIT_ERROR;
					goto cleanup;
				}
			}

			git_object_free(base_rev);
			base_rev = temp_object;
			break;
		}

		case '@':
		{
			if (spec[pos+1] == '{') {
				git_object *temp_object = NULL;

				if ((error = extract_curly_braces_content(&buf, spec, &pos)) < 0)
					goto cleanup;

				if ((error = ensure_base_rev_is_not_known_yet(base_rev)) < 0)
					goto cleanup;

				if ((error = handle_at_syntax(&temp_object, &reference, spec, identifier_len, repo, git_buf_cstr(&buf))) < 0)
					goto cleanup;

				if (temp_object != NULL)
					base_rev = temp_object;
				break;
			} else {
				/* Fall through */
			}
		}

		default:
			if ((error = ensure_left_hand_identifier_is_not_known_yet(base_rev, reference)) < 0)
				goto cleanup;

			pos++;
			identifier_len++;
		}
	}

	if ((error = ensure_base_rev_loaded(&base_rev, &reference, spec, identifier_len, repo, false)) < 0)
		goto cleanup;

	if (!should_return_reference) {
		git_reference_free(reference);
		reference = NULL;
	}

	*object_out = base_rev;
	*reference_out = reference;
	*identifier_len_out = identifier_len;
	error = 0;

cleanup:
	if (error) {
		if (error == GIT_EINVALIDSPEC)
			giterr_set(GITERR_INVALID,
				"Failed to parse revision specifier - Invalid pattern '%s'", spec);

		git_object_free(base_rev);
		git_reference_free(reference);
	}

	git_buf_free(&buf);
	return error;
}

int git_revparse_ext(
	git_object **object_out,
	git_reference **reference_out,
	git_repository *repo,
	const char *spec)
{
	int error;
	size_t identifier_len;
	git_object *obj = NULL;
	git_reference *ref = NULL;

	if ((error = revparse__ext(&obj, &ref, &identifier_len, repo, spec)) < 0)
		goto cleanup;

	*object_out = obj;
	*reference_out = ref;
	GIT_UNUSED(identifier_len);

	return 0;

cleanup:
	git_object_free(obj);
	git_reference_free(ref);
	return error;
}

int git_revparse_single(git_object **out, git_repository *repo, const char *spec)
{
	int error;
	git_object *obj = NULL;
	git_reference *ref = NULL;

	*out = NULL;

	if ((error = git_revparse_ext(&obj, &ref, repo, spec)) < 0)
		goto cleanup;

	git_reference_free(ref);

	*out = obj;

	return 0;

cleanup:
	git_object_free(obj);
	git_reference_free(ref);
	return error;
}

int git_revparse(
	git_revspec *revspec,
	git_repository *repo,
	const char *spec)
{
	const char *dotdot;
	int error = 0;

	assert(revspec && repo && spec);

	memset(revspec, 0x0, sizeof(*revspec));

	if ((dotdot = strstr(spec, "..")) != NULL) {
		char *lstr;
		const char *rstr;
		revspec->flags = GIT_REVPARSE_RANGE;

		lstr = git__substrdup(spec, dotdot - spec);
		rstr = dotdot + 2;
		if (dotdot[2] == '.') {
			revspec->flags |= GIT_REVPARSE_MERGE_BASE;
			rstr++;
		}

		error = git_revparse_single(&revspec->from, repo, lstr);
		if (!error)
			error = git_revparse_single(&revspec->to, repo, rstr);

		git__free((void*)lstr);
	} else {
		revspec->flags = GIT_REVPARSE_SINGLE;
		error = git_revparse_single(&revspec->from, repo, spec);
	}

	return error;
}
