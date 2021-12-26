#include "clar_libgit2.h"
#include "git2/sys/odb_backend.h"
#include "backend_helpers.h"

static int search_object(const fake_object **out, fake_backend *fake, const git_oid *oid, size_t len)
{
	const fake_object *obj = fake->objects, *found = NULL;

	while (obj && obj->oid) {
		git_oid current_oid;

		git_oid_fromstr(&current_oid, obj->oid);

		if (git_oid_ncmp(&current_oid, oid, len) == 0) {
			if (found)
				return GIT_EAMBIGUOUS;
			found = obj;
		}

		obj++;
	}

	if (found && out)
		*out = found;

	return found ? GIT_OK : GIT_ENOTFOUND;
}

static int fake_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	fake_backend *fake;

	fake = (fake_backend *)backend;

	fake->exists_calls++;

	return search_object(NULL, fake, oid, GIT_OID_HEXSZ) == GIT_OK;
}

static int fake_backend__exists_prefix(
	git_oid *out, git_odb_backend *backend, const git_oid *oid, size_t len)
{
	const fake_object *obj;
	fake_backend *fake;
	int error;

	fake = (fake_backend *)backend;

	fake->exists_prefix_calls++;

	if ((error = search_object(&obj, fake, oid, len)) < 0)
		return error;

	if (out)
		git_oid_fromstr(out, obj->oid);

	return 0;
}

static int fake_backend__read(
	void **buffer_p, size_t *len_p, git_object_t *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	const fake_object *obj;
	fake_backend *fake;
	int error;

	fake = (fake_backend *)backend;

	fake->read_calls++;

	if ((error = search_object(&obj, fake, oid, GIT_OID_HEXSZ)) < 0)
		return error;

	*len_p = strlen(obj->content);
	*buffer_p = git__strdup(obj->content);
	*type_p = GIT_OBJECT_BLOB;

	return 0;
}

static int fake_backend__read_header(
	size_t *len_p, git_object_t *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	const fake_object *obj;
	fake_backend *fake;
	int error;

	fake = (fake_backend *)backend;

	fake->read_header_calls++;

	if ((error = search_object(&obj, fake, oid, GIT_OID_HEXSZ)) < 0)
		return error;

	*len_p = strlen(obj->content);
	*type_p = GIT_OBJECT_BLOB;

	return 0;
}

static int fake_backend__read_prefix(
	git_oid *out_oid, void **buffer_p, size_t *len_p, git_object_t *type_p,
	git_odb_backend *backend, const git_oid *short_oid, size_t len)
{
	const fake_object *obj;
	fake_backend *fake;
	int error;

	fake = (fake_backend *)backend;

	fake->read_prefix_calls++;

	if ((error = search_object(&obj, fake, short_oid, len)) < 0)
		return error;

	git_oid_fromstr(out_oid, obj->oid);
	*len_p = strlen(obj->content);
	*buffer_p = git__strdup(obj->content);
	*type_p = GIT_OBJECT_BLOB;

	return 0;
}

static void fake_backend__free(git_odb_backend *_backend)
{
	fake_backend *backend;

	backend = (fake_backend *)_backend;

	git__free(backend);
}

int build_fake_backend(
	git_odb_backend **out,
	const fake_object *objects)
{
	fake_backend *backend;

	backend = git__calloc(1, sizeof(fake_backend));
	GIT_ERROR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_ODB_BACKEND_VERSION;

	backend->parent.refresh = NULL;
	backend->objects = objects;

	backend->parent.read = fake_backend__read;
	backend->parent.read_prefix = fake_backend__read_prefix;
	backend->parent.read_header = fake_backend__read_header;
	backend->parent.exists = fake_backend__exists;
	backend->parent.exists_prefix = fake_backend__exists_prefix;
	backend->parent.free = &fake_backend__free;

	*out = (git_odb_backend *)backend;

	return 0;
}
