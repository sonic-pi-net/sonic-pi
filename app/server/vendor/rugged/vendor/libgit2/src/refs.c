/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "refs.h"
#include "hash.h"
#include "repository.h"
#include "fileops.h"
#include "filebuf.h"
#include "pack.h"
#include "reflog.h"
#include "refdb.h"

#include <git2/tag.h>
#include <git2/object.h>
#include <git2/oid.h>
#include <git2/branch.h>
#include <git2/refs.h>
#include <git2/refdb.h>
#include <git2/sys/refs.h>
#include <git2/signature.h>
#include <git2/commit.h>

GIT__USE_STRMAP;

#define DEFAULT_NESTING_LEVEL	5
#define MAX_NESTING_LEVEL		10

enum {
	GIT_PACKREF_HAS_PEEL = 1,
	GIT_PACKREF_WAS_LOOSE = 2
};

static git_reference *alloc_ref(const char *name)
{
	git_reference *ref;
	size_t namelen = strlen(name);

	if ((ref = git__calloc(1, sizeof(git_reference) + namelen + 1)) == NULL)
		return NULL;

	memcpy(ref->name, name, namelen + 1);

	return ref;
}

git_reference *git_reference__alloc_symbolic(
	const char *name, const char *target)
{
	git_reference *ref;

	assert(name && target);

	ref = alloc_ref(name);
	if (!ref)
		return NULL;

	ref->type = GIT_REF_SYMBOLIC;

	if ((ref->target.symbolic = git__strdup(target)) == NULL) {
		git__free(ref);
		return NULL;
	}

	return ref;
}

git_reference *git_reference__alloc(
	const char *name,
	const git_oid *oid,
	const git_oid *peel)
{
	git_reference *ref;

	assert(name && oid);

	ref = alloc_ref(name);
	if (!ref)
		return NULL;

	ref->type = GIT_REF_OID;
	git_oid_cpy(&ref->target.oid, oid);

	if (peel != NULL)
		git_oid_cpy(&ref->peel, peel);

	return ref;
}

git_reference *git_reference__set_name(
	git_reference *ref, const char *name)
{
	size_t namelen = strlen(name);
	git_reference *rewrite =
		git__realloc(ref, sizeof(git_reference) + namelen + 1);
	if (rewrite != NULL)
		memcpy(rewrite->name, name, namelen + 1);
	return rewrite;
}

void git_reference_free(git_reference *reference)
{
	if (reference == NULL)
		return;

	if (reference->type == GIT_REF_SYMBOLIC)
		git__free(reference->target.symbolic);

	if (reference->db)
		GIT_REFCOUNT_DEC(reference->db, git_refdb__free);

	git__free(reference);
}

int git_reference_delete(git_reference *ref)
{
	const git_oid *old_id = NULL;
	const char *old_target = NULL;

	if (ref->type == GIT_REF_OID)
		old_id = &ref->target.oid;
	else
		old_target = ref->target.symbolic;

	return git_refdb_delete(ref->db, ref->name, old_id, old_target);
}

int git_reference_remove(git_repository *repo, const char *name)
{
	git_refdb *db;
	int error;

	if ((error = git_repository_refdb__weakptr(&db, repo)) < 0)
		return error;

	return git_refdb_delete(db, name, NULL, NULL);
}

int git_reference_lookup(git_reference **ref_out,
	git_repository *repo, const char *name)
{
	return git_reference_lookup_resolved(ref_out, repo, name, 0);
}

int git_reference_name_to_id(
	git_oid *out, git_repository *repo, const char *name)
{
	int error;
	git_reference *ref;

	if ((error = git_reference_lookup_resolved(&ref, repo, name, -1)) < 0)
		return error;

	git_oid_cpy(out, git_reference_target(ref));
	git_reference_free(ref);
	return 0;
}

static int reference_normalize_for_repo(
	git_refname_t out,
	git_repository *repo,
	const char *name)
{
	int precompose;
	unsigned int flags = GIT_REF_FORMAT_ALLOW_ONELEVEL;

	if (!git_repository__cvar(&precompose, repo, GIT_CVAR_PRECOMPOSE) &&
		precompose)
		flags |= GIT_REF_FORMAT__PRECOMPOSE_UNICODE;

	return git_reference_normalize_name(out, GIT_REFNAME_MAX, name, flags);
}

int git_reference_lookup_resolved(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	int max_nesting)
{
	git_refname_t scan_name;
	git_ref_t scan_type;
	int error = 0, nesting;
	git_reference *ref = NULL;
	git_refdb *refdb;

	assert(ref_out && repo && name);

	*ref_out = NULL;

	if (max_nesting > MAX_NESTING_LEVEL)
		max_nesting = MAX_NESTING_LEVEL;
	else if (max_nesting < 0)
		max_nesting = DEFAULT_NESTING_LEVEL;

	scan_type = GIT_REF_SYMBOLIC;

	if ((error = reference_normalize_for_repo(scan_name, repo, name)) < 0)
		return error;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	for (nesting = max_nesting;
		 nesting >= 0 && scan_type == GIT_REF_SYMBOLIC;
		 nesting--)
	{
		if (nesting != max_nesting) {
			strncpy(scan_name, ref->target.symbolic, sizeof(scan_name));
			git_reference_free(ref);
		}

		if ((error = git_refdb_lookup(&ref, refdb, scan_name)) < 0)
			return error;

		scan_type = ref->type;
	}

	if (scan_type != GIT_REF_OID && max_nesting != 0) {
		giterr_set(GITERR_REFERENCE,
			"Cannot resolve reference (>%u levels deep)", max_nesting);
		git_reference_free(ref);
		return -1;
	}

	*ref_out = ref;
	return 0;
}

int git_reference_dwim(git_reference **out, git_repository *repo, const char *refname)
{
	int error = 0, i;
	bool fallbackmode = true, foundvalid = false;
	git_reference *ref;
	git_buf refnamebuf = GIT_BUF_INIT, name = GIT_BUF_INIT;

	static const char* formatters[] = {
		"%s",
		GIT_REFS_DIR "%s",
		GIT_REFS_TAGS_DIR "%s",
		GIT_REFS_HEADS_DIR "%s",
		GIT_REFS_REMOTES_DIR "%s",
		GIT_REFS_REMOTES_DIR "%s/" GIT_HEAD_FILE,
		NULL
	};

	if (*refname)
		git_buf_puts(&name, refname);
	else {
		git_buf_puts(&name, GIT_HEAD_FILE);
		fallbackmode = false;
	}

	for (i = 0; formatters[i] && (fallbackmode || i == 0); i++) {

		git_buf_clear(&refnamebuf);

		if ((error = git_buf_printf(&refnamebuf, formatters[i], git_buf_cstr(&name))) < 0)
			goto cleanup;

		if (!git_reference_is_valid_name(git_buf_cstr(&refnamebuf))) {
			error = GIT_EINVALIDSPEC;
			continue;
		}
		foundvalid = true;

		error = git_reference_lookup_resolved(&ref, repo, git_buf_cstr(&refnamebuf), -1);

		if (!error) {
			*out = ref;
			error = 0;
			goto cleanup;
		}

		if (error != GIT_ENOTFOUND)
			goto cleanup;
	}

cleanup:
	if (error && !foundvalid) {
		/* never found a valid reference name */
		giterr_set(GITERR_REFERENCE,
			"Could not use '%s' as valid reference name", git_buf_cstr(&name));
	}

	git_buf_free(&name);
	git_buf_free(&refnamebuf);
	return error;
}

/**
 * Getters
 */
git_ref_t git_reference_type(const git_reference *ref)
{
	assert(ref);
	return ref->type;
}

const char *git_reference_name(const git_reference *ref)
{
	assert(ref);
	return ref->name;
}

git_repository *git_reference_owner(const git_reference *ref)
{
	assert(ref);
	return ref->db->repo;
}

const git_oid *git_reference_target(const git_reference *ref)
{
	assert(ref);

	if (ref->type != GIT_REF_OID)
		return NULL;

	return &ref->target.oid;
}

const git_oid *git_reference_target_peel(const git_reference *ref)
{
	assert(ref);

	if (ref->type != GIT_REF_OID || git_oid_iszero(&ref->peel))
		return NULL;

	return &ref->peel;
}

const char *git_reference_symbolic_target(const git_reference *ref)
{
	assert(ref);

	if (ref->type != GIT_REF_SYMBOLIC)
		return NULL;

	return ref->target.symbolic;
}

static int reference__create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *oid,
	const char *symbolic,
	int force,
	const git_signature *signature,
	const char *log_message,
	const git_oid *old_id,
	const char *old_target)
{
	git_refname_t normalized;
	git_refdb *refdb;
	git_reference *ref = NULL;
	int error = 0;

	assert(repo && name);
	assert(symbolic || signature);

	if (ref_out)
		*ref_out = NULL;

	error = reference_normalize_for_repo(normalized, repo, name);
	if (error < 0)
		return error;

	error = git_repository_refdb__weakptr(&refdb, repo);
	if (error < 0)
		return error;

	if (oid != NULL) {
		git_odb *odb;

		assert(symbolic == NULL);

		/* Sanity check the reference being created - target must exist. */
		if ((error = git_repository_odb__weakptr(&odb, repo)) < 0)
			return error;

		if (!git_odb_exists(odb, oid)) {
			giterr_set(GITERR_REFERENCE,
				"Target OID for the reference doesn't exist on the repository");
			return -1;
		}

		ref = git_reference__alloc(normalized, oid, NULL);
	} else {
		git_refname_t normalized_target;

		if ((error = reference_normalize_for_repo(normalized_target, repo, symbolic)) < 0)
			return error;

		ref = git_reference__alloc_symbolic(normalized, normalized_target);
	}

	GITERR_CHECK_ALLOC(ref);

	if ((error = git_refdb_write(refdb, ref, force, signature, log_message, old_id, old_target)) < 0) {
		git_reference_free(ref);
		return error;
	}

	if (ref_out == NULL)
		git_reference_free(ref);
	else
		*ref_out = ref;

	return 0;
}

int git_reference__log_signature(git_signature **out, git_repository *repo)
{
	int error;
	git_signature *who;

	if(((error = git_signature_default(&who, repo)) < 0) &&
	   ((error = git_signature_now(&who, "unknown", "unknown")) < 0))
		return error;

	*out = who;
	return 0;
}

int git_reference_create_matching(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *id,
	int force,
	const git_oid *old_id,
	const git_signature *signature,
	const char *log_message)

{
	int error;
	git_signature *who = NULL;
	
	assert(id);

	if (!signature) {
		if ((error = git_reference__log_signature(&who, repo)) < 0)
			return error;
		else
			signature = who;
	}

	error = reference__create(
		ref_out, repo, name, id, NULL, force, signature, log_message, old_id, NULL);

	git_signature_free(who);
	return error;
}

int git_reference_create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const git_oid *id,
	int force,
	const git_signature *signature,
	const char *log_message)
{
        return git_reference_create_matching(ref_out, repo, name, id, force, NULL, signature, log_message);
}

int git_reference_symbolic_create_matching(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const char *target,
	int force,
	const char *old_target,
	const git_signature *signature,
	const char *log_message)
{
	int error;
	git_signature *who = NULL;

	assert(target);

	if (!signature) {
		if ((error = git_reference__log_signature(&who, repo)) < 0)
			return error;
		else
			signature = who;
	}

	error = reference__create(
		ref_out, repo, name, NULL, target, force, signature, log_message, NULL, old_target);

	git_signature_free(who);
	return error;
}

int git_reference_symbolic_create(
	git_reference **ref_out,
	git_repository *repo,
	const char *name,
	const char *target,
	int force,
	const git_signature *signature,
	const char *log_message)
{
	return git_reference_symbolic_create_matching(ref_out, repo, name, target, force, NULL, signature, log_message);
}

static int ensure_is_an_updatable_direct_reference(git_reference *ref)
{
	if (ref->type == GIT_REF_OID)
		return 0;

	giterr_set(GITERR_REFERENCE, "Cannot set OID on symbolic reference");
	return -1;
}

int git_reference_set_target(
	git_reference **out,
	git_reference *ref,
	const git_oid *id,
	const git_signature *signature,
	const char *log_message)
{
	int error;
	git_repository *repo;

	assert(out && ref && id);

	repo = ref->db->repo;

	if ((error = ensure_is_an_updatable_direct_reference(ref)) < 0)
		return error;

	return git_reference_create_matching(out, repo, ref->name, id, 1, &ref->target.oid, signature, log_message);
}

static int ensure_is_an_updatable_symbolic_reference(git_reference *ref)
{
	if (ref->type == GIT_REF_SYMBOLIC)
		return 0;

	giterr_set(GITERR_REFERENCE, "Cannot set symbolic target on a direct reference");
	return -1;
}

int git_reference_symbolic_set_target(
	git_reference **out,
	git_reference *ref,
	const char *target,
	const git_signature *signature,
	const char *log_message)
{
	int error;

	assert(out && ref && target);

	if ((error = ensure_is_an_updatable_symbolic_reference(ref)) < 0)
		return error;

	return git_reference_symbolic_create_matching(
		out, ref->db->repo, ref->name, target, 1, ref->target.symbolic, signature, log_message);
}

static int reference__rename(git_reference **out, git_reference *ref, const char *new_name, int force,
				 const git_signature *signature, const char *message)
{
	git_refname_t normalized;
	bool should_head_be_updated = false;
	int error = 0;

	assert(ref && new_name && signature);

	if ((error = reference_normalize_for_repo(
			normalized, git_reference_owner(ref), new_name)) < 0)
		return error;


	/* Check if we have to update HEAD. */
	if ((error = git_branch_is_head(ref)) < 0)
		return error;

	should_head_be_updated = (error > 0);

	if ((error = git_refdb_rename(out, ref->db, ref->name, normalized, force, signature, message)) < 0)
		return error;

	/* Update HEAD it was pointing to the reference being renamed */
	if (should_head_be_updated &&
		(error = git_repository_set_head(ref->db->repo, normalized, signature, message)) < 0) {
		giterr_set(GITERR_REFERENCE, "Failed to update HEAD after renaming reference");
		return error;
	}

	return 0;
}


int git_reference_rename(
	git_reference **out,
	git_reference *ref,
	const char *new_name,
	int force,
	const git_signature *signature,
	const char *log_message)
{
	git_signature *who = (git_signature*)signature;
	int error;

	/* Should we return an error if there is no default? */
	if (!who &&
	    ((error = git_signature_default(&who, ref->db->repo)) < 0) &&
	    ((error = git_signature_now(&who, "unknown", "unknown")) < 0)) {
		return error;
	}

	error = reference__rename(out, ref, new_name, force, who, log_message);

	if (!signature)
		git_signature_free(who);

	return error;
}

int git_reference_resolve(git_reference **ref_out, const git_reference *ref)
{
	switch (git_reference_type(ref)) {
	case GIT_REF_OID:
		return git_reference_lookup(ref_out, ref->db->repo, ref->name);

	case GIT_REF_SYMBOLIC:
		return git_reference_lookup_resolved(ref_out, ref->db->repo, ref->target.symbolic, -1);

	default:
		giterr_set(GITERR_REFERENCE, "Invalid reference");
		return -1;
	}
}

int git_reference_foreach(
	git_repository *repo,
	git_reference_foreach_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	git_reference *ref;
	int error;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		return error;

	while (!(error = git_reference_next(&ref, iter))) {
		if ((error = callback(ref, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_foreach_name(
	git_repository *repo,
	git_reference_foreach_name_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	const char *refname;
	int error;

	if ((error = git_reference_iterator_new(&iter, repo)) < 0)
		return error;

	while (!(error = git_reference_next_name(&refname, iter))) {
		if ((error = callback(refname, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_foreach_glob(
	git_repository *repo,
	const char *glob,
	git_reference_foreach_name_cb callback,
	void *payload)
{
	git_reference_iterator *iter;
	const char *refname;
	int error;

	if ((error = git_reference_iterator_glob_new(&iter, repo, glob)) < 0)
		return error;

	while (!(error = git_reference_next_name(&refname, iter))) {
		if ((error = callback(refname, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_reference_iterator_free(iter);
	return error;
}

int git_reference_iterator_new(git_reference_iterator **out, git_repository *repo)
{
	git_refdb *refdb;

	if (git_repository_refdb__weakptr(&refdb, repo) < 0)
		return -1;

	return git_refdb_iterator(out, refdb, NULL);
}

int git_reference_iterator_glob_new(
	git_reference_iterator **out, git_repository *repo, const char *glob)
{
	git_refdb *refdb;

	if (git_repository_refdb__weakptr(&refdb, repo) < 0)
		return -1;

	return git_refdb_iterator(out, refdb, glob);
}

int git_reference_next(git_reference **out, git_reference_iterator *iter)
{
	return git_refdb_iterator_next(out, iter);
}

int git_reference_next_name(const char **out, git_reference_iterator *iter)
{
	return git_refdb_iterator_next_name(out, iter);
}

void git_reference_iterator_free(git_reference_iterator *iter)
{
	if (iter == NULL)
		return;

	git_refdb_iterator_free(iter);
}

static int cb__reflist_add(const char *ref, void *data)
{
	char *name = git__strdup(ref);
	GITERR_CHECK_ALLOC(name);
	return git_vector_insert((git_vector *)data, name);
}

int git_reference_list(
	git_strarray *array,
	git_repository *repo)
{
	git_vector ref_list;

	assert(array && repo);

	array->strings = NULL;
	array->count = 0;

	if (git_vector_init(&ref_list, 8, NULL) < 0)
		return -1;

	if (git_reference_foreach_name(
			repo, &cb__reflist_add, (void *)&ref_list) < 0) {
		git_vector_free(&ref_list);
		return -1;
	}

	array->strings = (char **)git_vector_detach(&array->count, NULL, &ref_list);

	return 0;
}

static int is_valid_ref_char(char ch)
{
	if ((unsigned) ch <= ' ')
		return 0;

	switch (ch) {
	case '~':
	case '^':
	case ':':
	case '\\':
	case '?':
	case '[':
	case '*':
		return 0;
	default:
		return 1;
	}
}

static int ensure_segment_validity(const char *name)
{
	const char *current = name;
	char prev = '\0';
	const int lock_len = (int)strlen(GIT_FILELOCK_EXTENSION);
	int segment_len;

	if (*current == '.')
		return -1; /* Refname starts with "." */

	for (current = name; ; current++) {
		if (*current == '\0' || *current == '/')
			break;

		if (!is_valid_ref_char(*current))
			return -1; /* Illegal character in refname */

		if (prev == '.' && *current == '.')
			return -1; /* Refname contains ".." */

		if (prev == '@' && *current == '{')
			return -1; /* Refname contains "@{" */

		prev = *current;
	}

	segment_len = (int)(current - name);

	/* A refname component can not end with ".lock" */
	if (segment_len >= lock_len &&
		!memcmp(current - lock_len, GIT_FILELOCK_EXTENSION, lock_len))
			return -1;

	return segment_len;
}

static bool is_all_caps_and_underscore(const char *name, size_t len)
{
	size_t i;
	char c;

	assert(name && len > 0);

	for (i = 0; i < len; i++)
	{
		c = name[i];
		if ((c < 'A' || c > 'Z') && c != '_')
			return false;
	}

	if (*name == '_' || name[len - 1] == '_')
		return false;

	return true;
}

/* Inspired from https://github.com/git/git/blob/f06d47e7e0d9db709ee204ed13a8a7486149f494/refs.c#L36-100 */
int git_reference__normalize_name(
	git_buf *buf,
	const char *name,
	unsigned int flags)
{
	char *current;
	int segment_len, segments_count = 0, error = GIT_EINVALIDSPEC;
	unsigned int process_flags;
	bool normalize = (buf != NULL);

#ifdef GIT_USE_ICONV
	git_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	assert(name);

	process_flags = flags;
	current = (char *)name;

	if (*current == '/')
		goto cleanup;

	if (normalize)
		git_buf_clear(buf);

#ifdef GIT_USE_ICONV
	if ((flags & GIT_REF_FORMAT__PRECOMPOSE_UNICODE) != 0) {
		size_t namelen = strlen(current);
		if ((error = git_path_iconv_init_precompose(&ic)) < 0 ||
			(error = git_path_iconv(&ic, &current, &namelen)) < 0)
			goto cleanup;
		error = GIT_EINVALIDSPEC;
	}
#endif

	while (true) {
		segment_len = ensure_segment_validity(current);
		if (segment_len < 0) {
			if ((process_flags & GIT_REF_FORMAT_REFSPEC_PATTERN) &&
					current[0] == '*' &&
					(current[1] == '\0' || current[1] == '/')) {
				/* Accept one wildcard as a full refname component. */
				process_flags &= ~GIT_REF_FORMAT_REFSPEC_PATTERN;
				segment_len = 1;
			} else
				goto cleanup;
		}

		if (segment_len > 0) {
			if (normalize) {
				size_t cur_len = git_buf_len(buf);

				git_buf_joinpath(buf, git_buf_cstr(buf), current);
				git_buf_truncate(buf,
					cur_len + segment_len + (segments_count ? 1 : 0));

				if (git_buf_oom(buf)) {
					error = -1;
					goto cleanup;
				}
			}

			segments_count++;
		}

		/* No empty segment is allowed when not normalizing */
		if (segment_len == 0 && !normalize)
			goto cleanup;

		if (current[segment_len] == '\0')
			break;

		current += segment_len + 1;
	}

	/* A refname can not be empty */
	if (segment_len == 0 && segments_count == 0)
		goto cleanup;

	/* A refname can not end with "." */
	if (current[segment_len - 1] == '.')
		goto cleanup;

	/* A refname can not end with "/" */
	if (current[segment_len - 1] == '/')
		goto cleanup;

	if ((segments_count == 1 ) && !(flags & GIT_REF_FORMAT_ALLOW_ONELEVEL))
		goto cleanup;

	if ((segments_count == 1 ) &&
	    !(flags & GIT_REF_FORMAT_REFSPEC_SHORTHAND) &&
		!(is_all_caps_and_underscore(name, (size_t)segment_len) ||
			((flags & GIT_REF_FORMAT_REFSPEC_PATTERN) && !strcmp("*", name))))
			goto cleanup;

	if ((segments_count > 1)
		&& (is_all_caps_and_underscore(name, strchr(name, '/') - name)))
			goto cleanup;

	error = 0;

cleanup:
	if (error == GIT_EINVALIDSPEC)
		giterr_set(
			GITERR_REFERENCE,
			"The given reference name '%s' is not valid", name);

	if (error && normalize)
		git_buf_free(buf);

#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&ic);
#endif

	return error;
}

int git_reference_normalize_name(
	char *buffer_out,
	size_t buffer_size,
	const char *name,
	unsigned int flags)
{
	git_buf buf = GIT_BUF_INIT;
	int error;

	if ((error = git_reference__normalize_name(&buf, name, flags)) < 0)
		goto cleanup;

	if (git_buf_len(&buf) > buffer_size - 1) {
		giterr_set(
		GITERR_REFERENCE,
		"The provided buffer is too short to hold the normalization of '%s'", name);
		error = GIT_EBUFS;
		goto cleanup;
	}

	git_buf_copy_cstr(buffer_out, buffer_size, &buf);

	error = 0;

cleanup:
	git_buf_free(&buf);
	return error;
}

#define GIT_REF_TYPEMASK (GIT_REF_OID | GIT_REF_SYMBOLIC)

int git_reference_cmp(
	const git_reference *ref1,
	const git_reference *ref2)
{
	git_ref_t type1, type2;
	assert(ref1 && ref2);

	type1 = git_reference_type(ref1);
	type2 = git_reference_type(ref2);

	/* let's put symbolic refs before OIDs */
	if (type1 != type2)
		return (type1 == GIT_REF_SYMBOLIC) ? -1 : 1;

	if (type1 == GIT_REF_SYMBOLIC)
		return strcmp(ref1->target.symbolic, ref2->target.symbolic);

	return git_oid__cmp(&ref1->target.oid, &ref2->target.oid);
}

static int reference__update_terminal(
	git_repository *repo,
	const char *ref_name,
	const git_oid *oid,
	int nesting,
	const git_signature *signature,
	const char *log_message)
{
	git_reference *ref;
	int error = 0;

	if (nesting > MAX_NESTING_LEVEL) {
		giterr_set(GITERR_REFERENCE, "Reference chain too deep (%d)", nesting);
		return GIT_ENOTFOUND;
	}

	error = git_reference_lookup(&ref, repo, ref_name);

	/* If we haven't found the reference at all, create a new reference. */
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		return git_reference_create(NULL, repo, ref_name, oid, 0, signature, log_message);
	}

	if (error < 0)
		return error;

	/* If the ref is a symbolic reference, follow its target. */
	if (git_reference_type(ref) == GIT_REF_SYMBOLIC) {
		error = reference__update_terminal(repo, git_reference_symbolic_target(ref), oid,
			nesting+1, signature, log_message);
		git_reference_free(ref);
	} else {
		/* If we're not moving the target, don't recreate the ref */
		if (0 != git_oid_cmp(git_reference_target(ref), oid))
			error = git_reference_create(NULL, repo, ref_name, oid, 1, signature, log_message);
		git_reference_free(ref);
	}

	return error;
}

/*
 * Starting with the reference given by `ref_name`, follows symbolic
 * references until a direct reference is found and updated the OID
 * on that direct reference to `oid`.
 */
int git_reference__update_terminal(
	git_repository *repo,
	const char *ref_name,
	const git_oid *oid,
	const git_signature *signature,
	const char *log_message)
{
	return reference__update_terminal(repo, ref_name, oid, 0, signature, log_message);
}

int git_reference__update_for_commit(
	git_repository *repo,
	git_reference *ref,
	const char *ref_name,
	const git_oid *id,
	const git_signature *committer,
	const char *operation)
{
	git_reference *ref_new = NULL;
	git_commit *commit = NULL;
	git_buf reflog_msg = GIT_BUF_INIT;
	int error;

	if ((error = git_commit_lookup(&commit, repo, id)) < 0 ||
		(error = git_buf_printf(&reflog_msg, "%s%s: %s",
			operation ? operation : "commit",
			git_commit_parentcount(commit) == 0 ? " (initial)" : "",
			git_commit_summary(commit))) < 0)
		goto done;

	if (ref)
		error = git_reference_set_target(
			&ref_new, ref, id, committer, git_buf_cstr(&reflog_msg));
	else
		error = git_reference__update_terminal(
			repo, ref_name, id, committer, git_buf_cstr(&reflog_msg));

done:
	git_reference_free(ref_new);
	git_buf_free(&reflog_msg);
	git_commit_free(commit);
	return error;
}

int git_reference_has_log(git_repository *repo, const char *refname)
{
	int error;
	git_refdb *refdb;

	assert(repo && refname);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_has_log(refdb, refname);
}

int git_reference_ensure_log(git_repository *repo, const char *refname)
{
	int error;
	git_refdb *refdb;

	assert(repo && refname);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_ensure_log(refdb, refname);
}

int git_reference__is_branch(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_HEADS_DIR) == 0;
}

int git_reference_is_branch(const git_reference *ref)
{
	assert(ref);
	return git_reference__is_branch(ref->name);
}

int git_reference__is_remote(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_REMOTES_DIR) == 0;
}

int git_reference_is_remote(const git_reference *ref)
{
	assert(ref);
	return git_reference__is_remote(ref->name);
}

int git_reference__is_tag(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_TAGS_DIR) == 0;
}

int git_reference_is_tag(const git_reference *ref)
{
	assert(ref);
	return git_reference__is_tag(ref->name);
}

int git_reference__is_note(const char *ref_name)
{
	return git__prefixcmp(ref_name, GIT_REFS_NOTES_DIR) == 0;
}

int git_reference_is_note(const git_reference *ref)
{
	assert(ref);
	return git_reference__is_note(ref->name);
}

static int peel_error(int error, git_reference *ref, const char* msg)
{
	giterr_set(
		GITERR_INVALID,
		"The reference '%s' cannot be peeled - %s", git_reference_name(ref), msg);
	return error;
}

int git_reference_peel(
	git_object **peeled,
	git_reference *ref,
	git_otype target_type)
{
	git_reference *resolved = NULL;
	git_object *target = NULL;
	int error;

	assert(ref);

	if (ref->type == GIT_REF_OID) {
		resolved = ref;
	} else {
		if ((error = git_reference_resolve(&resolved, ref)) < 0)
			return peel_error(error, ref, "Cannot resolve reference");
	}

	if (!git_oid_iszero(&resolved->peel)) {
		error = git_object_lookup(&target,
			git_reference_owner(ref), &resolved->peel, GIT_OBJ_ANY);
	} else {
		error = git_object_lookup(&target,
			git_reference_owner(ref), &resolved->target.oid, GIT_OBJ_ANY);
	}

	if (error < 0) {
		peel_error(error, ref, "Cannot retrieve reference target");
		goto cleanup;
	}

	if (target_type == GIT_OBJ_ANY && git_object_type(target) != GIT_OBJ_TAG)
		error = git_object_dup(peeled, target);
	else
		error = git_object_peel(peeled, target, target_type);

cleanup:
	git_object_free(target);

	if (resolved != ref)
		git_reference_free(resolved);

	return error;
}

int git_reference__is_valid_name(const char *refname, unsigned int flags)
{
	if (git_reference__normalize_name(NULL, refname, flags) < 0) {
		giterr_clear();
		return false;
	}

	return true;
}

int git_reference_is_valid_name(const char *refname)
{
	return git_reference__is_valid_name(refname, GIT_REF_FORMAT_ALLOW_ONELEVEL);
}

const char *git_reference_shorthand(const git_reference *ref)
{
	const char *name = ref->name;

	if (!git__prefixcmp(name, GIT_REFS_HEADS_DIR))
		return name + strlen(GIT_REFS_HEADS_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_TAGS_DIR))
		return name + strlen(GIT_REFS_TAGS_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_REMOTES_DIR))
		return name + strlen(GIT_REFS_REMOTES_DIR);
	else if (!git__prefixcmp(name, GIT_REFS_DIR))
		return name + strlen(GIT_REFS_DIR);

	/* No shorthands are avaiable, so just return the name */
	return name;
}
