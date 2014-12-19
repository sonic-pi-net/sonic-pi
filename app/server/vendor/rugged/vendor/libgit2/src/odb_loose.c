/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include <zlib.h>
#include "git2/object.h"
#include "git2/sys/odb_backend.h"
#include "fileops.h"
#include "hash.h"
#include "odb.h"
#include "delta-apply.h"
#include "filebuf.h"

#include "git2/odb_backend.h"
#include "git2/types.h"

typedef struct { /* object header data */
	git_otype type; /* object type */
	size_t	size; /* object size */
} obj_hdr;

typedef struct {
	git_odb_stream stream;
	git_filebuf fbuf;
} loose_writestream;

typedef struct loose_backend {
	git_odb_backend parent;

	int object_zlib_level; /** loose object zlib compression level. */
	int fsync_object_files; /** loose object file fsync flag. */
	mode_t object_file_mode;
	mode_t object_dir_mode;

	size_t objects_dirlen;
	char objects_dir[GIT_FLEX_ARRAY];
} loose_backend;

/* State structure for exploring directories,
 * in order to locate objects matching a short oid.
 */
typedef struct {
	size_t dir_len;
	unsigned char short_oid[GIT_OID_HEXSZ]; /* hex formatted oid to match */
	size_t short_oid_len;
	int found;				/* number of matching
						 * objects already found */
	unsigned char res_oid[GIT_OID_HEXSZ];	/* hex formatted oid of
						 * the object found */
} loose_locate_object_state;


/***********************************************************
 *
 * MISCELLANEOUS HELPER FUNCTIONS
 *
 ***********************************************************/

static int object_file_name(
	git_buf *name, const loose_backend *be, const git_oid *id)
{
	/* expand length for object root + 40 hex sha1 chars + 2 * '/' + '\0' */
	if (git_buf_grow(name, be->objects_dirlen + GIT_OID_HEXSZ + 3) < 0)
		return -1;

	git_buf_set(name, be->objects_dir, be->objects_dirlen);
	git_path_to_dir(name);

	/* loose object filename: aa/aaa... (41 bytes) */
	git_oid_pathfmt(name->ptr + name->size, id);
	name->size += GIT_OID_HEXSZ + 1;
	name->ptr[name->size] = '\0';

	return 0;
}

static int object_mkdir(const git_buf *name, const loose_backend *be)
{
	return git_futils_mkdir(
		name->ptr + be->objects_dirlen, be->objects_dir, be->object_dir_mode,
		GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST | GIT_MKDIR_VERIFY_DIR);
}

static size_t get_binary_object_header(obj_hdr *hdr, git_buf *obj)
{
	unsigned char c;
	unsigned char *data = (unsigned char *)obj->ptr;
	size_t shift, size, used = 0;

	if (git_buf_len(obj) == 0)
		return 0;

	c = data[used++];
	hdr->type = (c >> 4) & 7;

	size = c & 15;
	shift = 4;
	while (c & 0x80) {
		if (git_buf_len(obj) <= used)
			return 0;
		if (sizeof(size_t) * 8 <= shift)
			return 0;
		c = data[used++];
		size += (c & 0x7f) << shift;
		shift += 7;
	}
	hdr->size = size;

	return used;
}

static size_t get_object_header(obj_hdr *hdr, unsigned char *data)
{
	char c, typename[10];
	size_t size, used = 0;

	/*
	 * type name string followed by space.
	 */
	while ((c = data[used]) != ' ') {
		typename[used++] = c;
		if (used >= sizeof(typename))
			return 0;
	}
	typename[used] = 0;
	if (used == 0)
		return 0;
	hdr->type = git_object_string2type(typename);
	used++; /* consume the space */

	/*
	 * length follows immediately in decimal (without
	 * leading zeros).
	 */
	size = data[used++] - '0';
	if (size > 9)
		return 0;
	if (size) {
		while ((c = data[used]) != '\0') {
			size_t d = c - '0';
			if (d > 9)
				break;
			used++;
			size = size * 10 + d;
		}
	}
	hdr->size = size;

	/*
	 * the length must be followed by a zero byte
	 */
	if (data[used++] != '\0')
		return 0;

	return used;
}



/***********************************************************
 *
 * ZLIB RELATED FUNCTIONS
 *
 ***********************************************************/

static void init_stream(z_stream *s, void *out, size_t len)
{
	memset(s, 0, sizeof(*s));
	s->next_out = out;
	s->avail_out = (uInt)len;
}

static void set_stream_input(z_stream *s, void *in, size_t len)
{
	s->next_in = in;
	s->avail_in = (uInt)len;
}

static void set_stream_output(z_stream *s, void *out, size_t len)
{
	s->next_out = out;
	s->avail_out = (uInt)len;
}


static int start_inflate(z_stream *s, git_buf *obj, void *out, size_t len)
{
	int status;

	init_stream(s, out, len);
	set_stream_input(s, obj->ptr, git_buf_len(obj));

	if ((status = inflateInit(s)) < Z_OK)
		return status;

	return inflate(s, 0);
}

static int finish_inflate(z_stream *s)
{
	int status = Z_OK;

	while (status == Z_OK)
		status = inflate(s, Z_FINISH);

	inflateEnd(s);

	if ((status != Z_STREAM_END) || (s->avail_in != 0)) {
		giterr_set(GITERR_ZLIB, "Failed to finish ZLib inflation. Stream aborted prematurely");
		return -1;
	}

	return 0;
}

static int is_zlib_compressed_data(unsigned char *data)
{
	unsigned int w;

	w = ((unsigned int)(data[0]) << 8) + data[1];
	return (data[0] & 0x8F) == 0x08 && !(w % 31);
}

static int inflate_buffer(void *in, size_t inlen, void *out, size_t outlen)
{
	z_stream zs;
	int status = Z_OK;

	memset(&zs, 0x0, sizeof(zs));

	zs.next_out = out;
	zs.avail_out = (uInt)outlen;

	zs.next_in = in;
	zs.avail_in = (uInt)inlen;

	if (inflateInit(&zs) < Z_OK) {
		giterr_set(GITERR_ZLIB, "Failed to inflate buffer");
		return -1;
	}

	while (status == Z_OK)
		status = inflate(&zs, Z_FINISH);

	inflateEnd(&zs);

	if (status != Z_STREAM_END /* || zs.avail_in != 0 */ ||
		zs.total_out != outlen)
	{
		giterr_set(GITERR_ZLIB, "Failed to inflate buffer. Stream aborted prematurely");
		return -1;
	}

	return 0;
}

static void *inflate_tail(z_stream *s, void *hb, size_t used, obj_hdr *hdr)
{
	unsigned char *buf, *head = hb;
	size_t tail;

	/*
	 * allocate a buffer to hold the inflated data and copy the
	 * initial sequence of inflated data from the tail of the
	 * head buffer, if any.
	 */
	if ((buf = git__malloc(hdr->size + 1)) == NULL) {
		inflateEnd(s);
		return NULL;
	}
	tail = s->total_out - used;
	if (used > 0 && tail > 0) {
		if (tail > hdr->size)
			tail = hdr->size;
		memcpy(buf, head + used, tail);
	}
	used = tail;

	/*
	 * inflate the remainder of the object data, if any
	 */
	if (hdr->size < used)
		inflateEnd(s);
	else {
		set_stream_output(s, buf + used, hdr->size - used);
		if (finish_inflate(s)) {
			git__free(buf);
			return NULL;
		}
	}

	return buf;
}

/*
 * At one point, there was a loose object format that was intended to
 * mimic the format used in pack-files. This was to allow easy copying
 * of loose object data into packs. This format is no longer used, but
 * we must still read it.
 */
static int inflate_packlike_loose_disk_obj(git_rawobj *out, git_buf *obj)
{
	unsigned char *in, *buf;
	obj_hdr hdr;
	size_t len, used;

	/*
	 * read the object header, which is an (uncompressed)
	 * binary encoding of the object type and size.
	 */
	if ((used = get_binary_object_header(&hdr, obj)) == 0 ||
		!git_object_typeisloose(hdr.type)) {
		giterr_set(GITERR_ODB, "Failed to inflate loose object.");
		return -1;
	}

	/*
	 * allocate a buffer and inflate the data into it
	 */
	buf = git__malloc(hdr.size + 1);
	GITERR_CHECK_ALLOC(buf);

	in = ((unsigned char *)obj->ptr) + used;
	len = obj->size - used;
	if (inflate_buffer(in, len, buf, hdr.size) < 0) {
		git__free(buf);
		return -1;
	}
	buf[hdr.size] = '\0';

	out->data = buf;
	out->len = hdr.size;
	out->type = hdr.type;

	return 0;
}

static int inflate_disk_obj(git_rawobj *out, git_buf *obj)
{
	unsigned char head[64], *buf;
	z_stream zs;
	obj_hdr hdr;
	size_t used;

	/*
	 * check for a pack-like loose object
	 */
	if (!is_zlib_compressed_data((unsigned char *)obj->ptr))
		return inflate_packlike_loose_disk_obj(out, obj);

	/*
	 * inflate the initial part of the io buffer in order
	 * to parse the object header (type and size).
	 */
	if (start_inflate(&zs, obj, head, sizeof(head)) < Z_OK ||
		(used = get_object_header(&hdr, head)) == 0 ||
		!git_object_typeisloose(hdr.type))
	{
		giterr_set(GITERR_ODB, "Failed to inflate disk object.");
		return -1;
	}

	/*
	 * allocate a buffer and inflate the object data into it
	 * (including the initial sequence in the head buffer).
	 */
	if ((buf = inflate_tail(&zs, head, used, &hdr)) == NULL)
		return -1;
	buf[hdr.size] = '\0';

	out->data = buf;
	out->len = hdr.size;
	out->type = hdr.type;

	return 0;
}






/***********************************************************
 *
 * ODB OBJECT READING & WRITING
 *
 * Backend for the public API; read headers and full objects
 * from the ODB. Write raw data to the ODB.
 *
 ***********************************************************/

static int read_loose(git_rawobj *out, git_buf *loc)
{
	int error;
	git_buf obj = GIT_BUF_INIT;

	assert(out && loc);

	if (git_buf_oom(loc))
		return -1;

	out->data = NULL;
	out->len = 0;
	out->type = GIT_OBJ_BAD;

	if (!(error = git_futils_readbuffer(&obj, loc->ptr)))
		error = inflate_disk_obj(out, &obj);

	git_buf_free(&obj);

	return error;
}

static int read_header_loose(git_rawobj *out, git_buf *loc)
{
	int error = 0, z_return = Z_ERRNO, read_bytes;
	git_file fd;
	z_stream zs;
	obj_hdr header_obj;
	unsigned char raw_buffer[16], inflated_buffer[64];

	assert(out && loc);

	if (git_buf_oom(loc))
		return -1;

	out->data = NULL;

	if ((fd = git_futils_open_ro(loc->ptr)) < 0)
		return fd;

	init_stream(&zs, inflated_buffer, sizeof(inflated_buffer));

	z_return = inflateInit(&zs);

	while (z_return == Z_OK) {
		if ((read_bytes = p_read(fd, raw_buffer, sizeof(raw_buffer))) > 0) {
			set_stream_input(&zs, raw_buffer, read_bytes);
			z_return = inflate(&zs, 0);
		} else
			z_return = Z_STREAM_END;
	}

	if ((z_return != Z_STREAM_END && z_return != Z_BUF_ERROR)
		|| get_object_header(&header_obj, inflated_buffer) == 0
		|| git_object_typeisloose(header_obj.type) == 0)
	{
		giterr_set(GITERR_ZLIB, "Failed to read loose object header");
		error = -1;
	} else {
		out->len = header_obj.size;
		out->type = header_obj.type;
	}

	finish_inflate(&zs);
	p_close(fd);

	return error;
}

static int locate_object(
	git_buf *object_location,
	loose_backend *backend,
	const git_oid *oid)
{
	int error = object_file_name(object_location, backend, oid);

	if (!error && !git_path_exists(object_location->ptr))
		return GIT_ENOTFOUND;

	return error;
}

/* Explore an entry of a directory and see if it matches a short oid */
static int fn_locate_object_short_oid(void *state, git_buf *pathbuf) {
	loose_locate_object_state *sstate = (loose_locate_object_state *)state;

	if (git_buf_len(pathbuf) - sstate->dir_len != GIT_OID_HEXSZ - 2) {
		/* Entry cannot be an object. Continue to next entry */
		return 0;
	}

	if (git_path_isdir(pathbuf->ptr) == false) {
		/* We are already in the directory matching the 2 first hex characters,
		 * compare the first ncmp characters of the oids */
		if (!memcmp(sstate->short_oid + 2,
			(unsigned char *)pathbuf->ptr + sstate->dir_len,
			sstate->short_oid_len - 2)) {

			if (!sstate->found) {
				sstate->res_oid[0] = sstate->short_oid[0];
				sstate->res_oid[1] = sstate->short_oid[1];
				memcpy(sstate->res_oid+2, pathbuf->ptr+sstate->dir_len, GIT_OID_HEXSZ-2);
			}
			sstate->found++;
		}
	}

	if (sstate->found > 1)
		return GIT_EAMBIGUOUS;

	return 0;
}

/* Locate an object matching a given short oid */
static int locate_object_short_oid(
	git_buf *object_location,
	git_oid *res_oid,
	loose_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	char *objects_dir = backend->objects_dir;
	size_t dir_len = strlen(objects_dir);
	loose_locate_object_state state;
	int error;

	/* prealloc memory for OBJ_DIR/xx/xx..38x..xx */
	if (git_buf_grow(object_location, dir_len + 3 + GIT_OID_HEXSZ) < 0)
		return -1;

	git_buf_set(object_location, objects_dir, dir_len);
	git_path_to_dir(object_location);

	/* save adjusted position at end of dir so it can be restored later */
	dir_len = git_buf_len(object_location);

	/* Convert raw oid to hex formatted oid */
	git_oid_fmt((char *)state.short_oid, short_oid);

	/* Explore OBJ_DIR/xx/ where xx is the beginning of hex formatted short oid */
	if (git_buf_put(object_location, (char *)state.short_oid, 3) < 0)
		return -1;
	object_location->ptr[object_location->size - 1] = '/';

	/* Check that directory exists */
	if (git_path_isdir(object_location->ptr) == false)
		return git_odb__error_notfound("no matching loose object for prefix", short_oid);

	state.dir_len = git_buf_len(object_location);
	state.short_oid_len = len;
	state.found = 0;

	/* Explore directory to find a unique object matching short_oid */
	error = git_path_direach(
		object_location, 0, fn_locate_object_short_oid, &state);
	if (error < 0 && error != GIT_EAMBIGUOUS)
		return error;

	if (!state.found)
		return git_odb__error_notfound("no matching loose object for prefix", short_oid);

	if (state.found > 1)
		return git_odb__error_ambiguous("multiple matches in loose objects");

	/* Convert obtained hex formatted oid to raw */
	error = git_oid_fromstr(res_oid, (char *)state.res_oid);
	if (error)
		return error;

	/* Update the location according to the oid obtained */

	git_buf_truncate(object_location, dir_len);
	if (git_buf_grow(object_location, dir_len + GIT_OID_HEXSZ + 2) < 0)
		return -1;

	git_oid_pathfmt(object_location->ptr + dir_len, res_oid);

	object_location->size += GIT_OID_HEXSZ + 1;
	object_location->ptr[object_location->size] = '\0';

	return 0;
}









/***********************************************************
 *
 * LOOSE BACKEND PUBLIC API
 *
 * Implement the git_odb_backend API calls
 *
 ***********************************************************/

static int loose_backend__read_header(size_t *len_p, git_otype *type_p, git_odb_backend *backend, const git_oid *oid)
{
	git_buf object_path = GIT_BUF_INIT;
	git_rawobj raw;
	int error;

	assert(backend && oid);

	raw.len = 0;
	raw.type = GIT_OBJ_BAD;

	if (locate_object(&object_path, (loose_backend *)backend, oid) < 0)
		error = git_odb__error_notfound("no matching loose object", oid);
	else if ((error = read_header_loose(&raw, &object_path)) == 0) {
		*len_p = raw.len;
		*type_p = raw.type;
	}

	git_buf_free(&object_path);

	return error;
}

static int loose_backend__read(void **buffer_p, size_t *len_p, git_otype *type_p, git_odb_backend *backend, const git_oid *oid)
{
	git_buf object_path = GIT_BUF_INIT;
	git_rawobj raw;
	int error = 0;

	assert(backend && oid);

	if (locate_object(&object_path, (loose_backend *)backend, oid) < 0)
		error = git_odb__error_notfound("no matching loose object", oid);
	else if ((error = read_loose(&raw, &object_path)) == 0) {
		*buffer_p = raw.data;
		*len_p = raw.len;
		*type_p = raw.type;
	}

	git_buf_free(&object_path);

	return error;
}

static int loose_backend__read_prefix(
	git_oid *out_oid,
	void **buffer_p,
	size_t *len_p,
	git_otype *type_p,
	git_odb_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	int error = 0;

	assert(len >= GIT_OID_MINPREFIXLEN && len <= GIT_OID_HEXSZ);

	if (len == GIT_OID_HEXSZ) {
		/* We can fall back to regular read method */
		error = loose_backend__read(buffer_p, len_p, type_p, backend, short_oid);
		if (!error)
			git_oid_cpy(out_oid, short_oid);
	} else {
		git_buf object_path = GIT_BUF_INIT;
		git_rawobj raw;

		assert(backend && short_oid);

		if ((error = locate_object_short_oid(&object_path, out_oid,
				(loose_backend *)backend, short_oid, len)) == 0 &&
			(error = read_loose(&raw, &object_path)) == 0)
		{
			*buffer_p = raw.data;
			*len_p = raw.len;
			*type_p = raw.type;
		}

		git_buf_free(&object_path);
	}

	return error;
}

static int loose_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	git_buf object_path = GIT_BUF_INIT;
	int error;

	assert(backend && oid);

	error = locate_object(&object_path, (loose_backend *)backend, oid);

	git_buf_free(&object_path);

	return !error;
}

static int loose_backend__exists_prefix(
	git_oid *out, git_odb_backend *backend, const git_oid *short_id, size_t len)
{
	git_buf object_path = GIT_BUF_INIT;
	int error;

	assert(backend && out && short_id && len >= GIT_OID_MINPREFIXLEN);

	error = locate_object_short_oid(
		&object_path, out, (loose_backend *)backend, short_id, len);

	git_buf_free(&object_path);

	return error;
}

struct foreach_state {
	size_t dir_len;
	git_odb_foreach_cb cb;
	void *data;
};

GIT_INLINE(int) filename_to_oid(git_oid *oid, const char *ptr)
{
	int v, i = 0;
	if (strlen(ptr) != GIT_OID_HEXSZ+1)
		return -1;

	if (ptr[2] != '/') {
		return -1;
	}

	v = (git__fromhex(ptr[i]) << 4) | git__fromhex(ptr[i+1]);
	if (v < 0)
		return -1;

	oid->id[0] = (unsigned char) v;

	ptr += 3;
	for (i = 0; i < 38; i += 2) {
		v = (git__fromhex(ptr[i]) << 4) | git__fromhex(ptr[i + 1]);
		if (v < 0)
			return -1;

		oid->id[1 + i/2] = (unsigned char) v;
	}

	return 0;
}

static int foreach_object_dir_cb(void *_state, git_buf *path)
{
	git_oid oid;
	struct foreach_state *state = (struct foreach_state *) _state;

	if (filename_to_oid(&oid, path->ptr + state->dir_len) < 0)
		return 0;

	return giterr_set_after_callback_function(
		state->cb(&oid, state->data), "git_odb_foreach");
}

static int foreach_cb(void *_state, git_buf *path)
{
	struct foreach_state *state = (struct foreach_state *) _state;

	/* non-dir is some stray file, ignore it */
	if (!git_path_isdir(git_buf_cstr(path)))
		return 0;

	return git_path_direach(path, 0, foreach_object_dir_cb, state);
}

static int loose_backend__foreach(git_odb_backend *_backend, git_odb_foreach_cb cb, void *data)
{
	char *objects_dir;
	int error;
	git_buf buf = GIT_BUF_INIT;
	struct foreach_state state;
	loose_backend *backend = (loose_backend *) _backend;

	assert(backend && cb);

	objects_dir = backend->objects_dir;

	git_buf_sets(&buf, objects_dir);
	git_path_to_dir(&buf);
	if (git_buf_oom(&buf))
		return -1;

	memset(&state, 0, sizeof(state));
	state.cb = cb;
	state.data = data;
	state.dir_len = git_buf_len(&buf);

	error = git_path_direach(&buf, 0, foreach_cb, &state);

	git_buf_free(&buf);

	return error;
}

static int loose_backend__stream_fwrite(git_odb_stream *_stream, const git_oid *oid)
{
	loose_writestream *stream = (loose_writestream *)_stream;
	loose_backend *backend = (loose_backend *)_stream->backend;
	git_buf final_path = GIT_BUF_INIT;
	int error = 0;

	if (object_file_name(&final_path, backend, oid) < 0 ||
		object_mkdir(&final_path, backend) < 0)
		error = -1;
	else
		error = git_filebuf_commit_at(
			&stream->fbuf, final_path.ptr);

	git_buf_free(&final_path);

	return error;
}

static int loose_backend__stream_write(git_odb_stream *_stream, const char *data, size_t len)
{
	loose_writestream *stream = (loose_writestream *)_stream;
	return git_filebuf_write(&stream->fbuf, data, len);
}

static void loose_backend__stream_free(git_odb_stream *_stream)
{
	loose_writestream *stream = (loose_writestream *)_stream;

	git_filebuf_cleanup(&stream->fbuf);
	git__free(stream);
}

static int loose_backend__stream(git_odb_stream **stream_out, git_odb_backend *_backend, size_t length, git_otype type)
{
	loose_backend *backend;
	loose_writestream *stream = NULL;
	char hdr[64];
	git_buf tmp_path = GIT_BUF_INIT;
	int hdrlen;

	assert(_backend);

	backend = (loose_backend *)_backend;
	*stream_out = NULL;

	hdrlen = git_odb__format_object_header(hdr, sizeof(hdr), length, type);

	stream = git__calloc(1, sizeof(loose_writestream));
	GITERR_CHECK_ALLOC(stream);

	stream->stream.backend = _backend;
	stream->stream.read = NULL; /* read only */
	stream->stream.write = &loose_backend__stream_write;
	stream->stream.finalize_write = &loose_backend__stream_fwrite;
	stream->stream.free = &loose_backend__stream_free;
	stream->stream.mode = GIT_STREAM_WRONLY;

	if (git_buf_joinpath(&tmp_path, backend->objects_dir, "tmp_object") < 0 ||
		git_filebuf_open(&stream->fbuf, tmp_path.ptr,
			GIT_FILEBUF_TEMPORARY |
			(backend->object_zlib_level << GIT_FILEBUF_DEFLATE_SHIFT),
			backend->object_file_mode) < 0 ||
		stream->stream.write((git_odb_stream *)stream, hdr, hdrlen) < 0)
	{
		git_filebuf_cleanup(&stream->fbuf);
		git__free(stream);
		stream = NULL;
	}
	git_buf_free(&tmp_path);
	*stream_out = (git_odb_stream *)stream;

	return !stream ? -1 : 0;
}

static int loose_backend__write(git_odb_backend *_backend, const git_oid *oid, const void *data, size_t len, git_otype type)
{
	int error = 0, header_len;
	git_buf final_path = GIT_BUF_INIT;
	char header[64];
	git_filebuf fbuf = GIT_FILEBUF_INIT;
	loose_backend *backend;

	backend = (loose_backend *)_backend;

	/* prepare the header for the file */
	header_len = git_odb__format_object_header(header, sizeof(header), len, type);

	if (git_buf_joinpath(&final_path, backend->objects_dir, "tmp_object") < 0 ||
		git_filebuf_open(&fbuf, final_path.ptr,
			GIT_FILEBUF_TEMPORARY |
			(backend->object_zlib_level << GIT_FILEBUF_DEFLATE_SHIFT),
			backend->object_file_mode) < 0)
	{
		error = -1;
		goto cleanup;
	}

	git_filebuf_write(&fbuf, header, header_len);
	git_filebuf_write(&fbuf, data, len);

	if (object_file_name(&final_path, backend, oid) < 0 ||
		object_mkdir(&final_path, backend) < 0 ||
		git_filebuf_commit_at(&fbuf, final_path.ptr) < 0)
		error = -1;

cleanup:
	if (error < 0)
		git_filebuf_cleanup(&fbuf);
	git_buf_free(&final_path);
	return error;
}

static void loose_backend__free(git_odb_backend *_backend)
{
	loose_backend *backend;
	assert(_backend);
	backend = (loose_backend *)_backend;

	git__free(backend);
}

int git_odb_backend_loose(
	git_odb_backend **backend_out,
	const char *objects_dir,
	int compression_level,
	int do_fsync,
	unsigned int dir_mode,
	unsigned int file_mode)
{
	loose_backend *backend;
	size_t objects_dirlen;

	assert(backend_out && objects_dir);

	objects_dirlen = strlen(objects_dir);

	backend = git__calloc(1, sizeof(loose_backend) + objects_dirlen + 2);
	GITERR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_ODB_BACKEND_VERSION;
	backend->objects_dirlen = objects_dirlen;
	memcpy(backend->objects_dir, objects_dir, objects_dirlen);
	if (backend->objects_dir[backend->objects_dirlen - 1] != '/')
		backend->objects_dir[backend->objects_dirlen++] = '/';

	if (compression_level < 0)
		compression_level = Z_BEST_SPEED;

	if (dir_mode == 0)
		dir_mode = GIT_OBJECT_DIR_MODE;

	if (file_mode == 0)
		file_mode = GIT_OBJECT_FILE_MODE;

	backend->object_zlib_level = compression_level;
	backend->fsync_object_files = do_fsync;
	backend->object_dir_mode = dir_mode;
	backend->object_file_mode = file_mode;

	backend->parent.read = &loose_backend__read;
	backend->parent.write = &loose_backend__write;
	backend->parent.read_prefix = &loose_backend__read_prefix;
	backend->parent.read_header = &loose_backend__read_header;
	backend->parent.writestream = &loose_backend__stream;
	backend->parent.exists = &loose_backend__exists;
	backend->parent.exists_prefix = &loose_backend__exists_prefix;
	backend->parent.foreach = &loose_backend__foreach;
	backend->parent.free = &loose_backend__free;

	*backend_out = (git_odb_backend *)backend;
	return 0;
}
