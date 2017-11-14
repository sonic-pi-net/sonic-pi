/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_object_h__
#define INCLUDE_object_h__

#include "common.h"

#include "repository.h"

extern bool git_object__strict_input_validation;

/** Base git object for inheritance */
struct git_object {
	git_cached_obj cached;
	git_repository *repo;
};

/* fully free the object; internal method, DO NOT EXPORT */
void git_object__free(void *object);

int git_object__from_odb_object(
	git_object **object_out,
	git_repository *repo,
	git_odb_object *odb_obj,
	git_otype type);

int git_object__resolve_to_type(git_object **obj, git_otype type);

int git_oid__parse(git_oid *oid, const char **buffer_out, const char *buffer_end, const char *header);

void git_oid__writebuf(git_buf *buf, const char *header, const git_oid *oid);

bool git_object__is_valid(
	git_repository *repo, const git_oid *id, git_otype expected_type);

GIT_INLINE(git_otype) git_object__type_from_filemode(git_filemode_t mode)
{
	switch (mode) {
	case GIT_FILEMODE_TREE:
		return GIT_OBJ_TREE;
	case GIT_FILEMODE_COMMIT:
		return GIT_OBJ_COMMIT;
	case GIT_FILEMODE_BLOB:
	case GIT_FILEMODE_BLOB_EXECUTABLE:
	case GIT_FILEMODE_LINK:
		return GIT_OBJ_BLOB;
	default:
		return GIT_OBJ_BAD;
	}
}

#endif
