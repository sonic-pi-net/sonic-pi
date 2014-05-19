/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "config.h"
#include "filebuf.h"
#include "sysdir.h"
#include "buffer.h"
#include "buf_text.h"
#include "git2/config.h"
#include "git2/sys/config.h"
#include "git2/types.h"
#include "strmap.h"
#include "array.h"

#include <ctype.h>
#include <sys/types.h>
#include <regex.h>

GIT__USE_STRMAP;

typedef struct cvar_t {
	struct cvar_t *next;
	git_config_entry *entry;
	bool included; /* whether this is part of [include] */
} cvar_t;

typedef struct git_config_file_iter {
	git_config_iterator parent;
	git_strmap_iter iter;
	cvar_t* next_var;
} git_config_file_iter;

/* Max depth for [include] directives */
#define MAX_INCLUDE_DEPTH 10

#define CVAR_LIST_HEAD(list) ((list)->head)

#define CVAR_LIST_TAIL(list) ((list)->tail)

#define CVAR_LIST_NEXT(var) ((var)->next)

#define CVAR_LIST_EMPTY(list) ((list)->head == NULL)

#define CVAR_LIST_APPEND(list, var) do {\
	if (CVAR_LIST_EMPTY(list)) {\
		CVAR_LIST_HEAD(list) = CVAR_LIST_TAIL(list) = var;\
	} else {\
		CVAR_LIST_NEXT(CVAR_LIST_TAIL(list)) = var;\
		CVAR_LIST_TAIL(list) = var;\
	}\
} while(0)

#define CVAR_LIST_REMOVE_HEAD(list) do {\
	CVAR_LIST_HEAD(list) = CVAR_LIST_NEXT(CVAR_LIST_HEAD(list));\
} while(0)

#define CVAR_LIST_REMOVE_AFTER(var) do {\
	CVAR_LIST_NEXT(var) = CVAR_LIST_NEXT(CVAR_LIST_NEXT(var));\
} while(0)

#define CVAR_LIST_FOREACH(list, iter)\
	for ((iter) = CVAR_LIST_HEAD(list);\
		 (iter) != NULL;\
		 (iter) = CVAR_LIST_NEXT(iter))

/*
 * Inspired by the FreeBSD functions
 */
#define CVAR_LIST_FOREACH_SAFE(start, iter, tmp)\
	for ((iter) = CVAR_LIST_HEAD(vars);\
		 (iter) && (((tmp) = CVAR_LIST_NEXT(iter) || 1));\
		 (iter) = (tmp))

struct reader {
	time_t file_mtime;
	size_t file_size;
	char *file_path;
	git_buf buffer;
	char *read_ptr;
	int line_number;
	int eof;
};

typedef struct {
	git_atomic refcount;
	git_strmap *values;
} refcounted_strmap;

typedef struct {
	git_config_backend parent;
	/* mutex to coordinate accessing the values */
	git_mutex values_mutex;
	refcounted_strmap *values;
	int readonly;
} diskfile_header;

typedef struct {
	diskfile_header header;

	git_config_level_t level;

	git_array_t(struct reader) readers;

	char  *file_path;
} diskfile_backend;

typedef struct {
	diskfile_header header;

	diskfile_backend *snapshot_from;
} diskfile_readonly_backend;

static int config_parse(git_strmap *values, diskfile_backend *cfg_file, struct reader *reader, git_config_level_t level, int depth);
static int parse_variable(struct reader *reader, char **var_name, char **var_value);
static int config_write(diskfile_backend *cfg, const char *key, const regex_t *preg, const char *value);
static char *escape_value(const char *ptr);

int git_config_file__snapshot(git_config_backend **out, diskfile_backend *in);
static int config_snapshot(git_config_backend **out, git_config_backend *in);

static void set_parse_error(struct reader *reader, int col, const char *error_str)
{
	giterr_set(GITERR_CONFIG, "Failed to parse config file: %s (in %s:%d, column %d)",
		error_str, reader->file_path, reader->line_number, col);
}

static int config_error_readonly(void)
{
	giterr_set(GITERR_CONFIG, "this backend is read-only");
	return -1;
}

static void cvar_free(cvar_t *var)
{
	if (var == NULL)
		return;

	git__free((char*)var->entry->name);
	git__free((char *)var->entry->value);
	git__free(var->entry);
	git__free(var);
}

int git_config_file_normalize_section(char *start, char *end)
{
	char *scan;

	if (start == end)
		return GIT_EINVALIDSPEC;

	/* Validate and downcase range */
	for (scan = start; *scan; ++scan) {
		if (end && scan >= end)
			break;
		if (isalnum(*scan))
			*scan = (char)tolower(*scan);
		else if (*scan != '-' || scan == start)
			return GIT_EINVALIDSPEC;
	}

	if (scan == start)
		return GIT_EINVALIDSPEC;

	return 0;
}

/* Add or append the new config option */
static int append_entry(git_strmap *values, cvar_t *var)
{
	git_strmap_iter pos;
	cvar_t *existing;
	int error = 0;

	pos = git_strmap_lookup_index(values, var->entry->name);
	if (!git_strmap_valid_index(values, pos)) {
		git_strmap_insert(values, var->entry->name, var, error);
	} else {
		existing = git_strmap_value_at(values, pos);
		while (existing->next != NULL) {
			existing = existing->next;
		}
		existing->next = var;
	}

	if (error > 0)
		error = 0;

	return error;
}

static void free_vars(git_strmap *values)
{
	cvar_t *var = NULL;

	if (values == NULL)
		return;

	git_strmap_foreach_value(values, var,
		while (var != NULL) {
			cvar_t *next = CVAR_LIST_NEXT(var);
			cvar_free(var);
			var = next;
		});

	git_strmap_free(values);
}

static void refcounted_strmap_free(refcounted_strmap *map)
{
	if (!map)
		return;

	if (git_atomic_dec(&map->refcount) != 0)
		return;

	free_vars(map->values);
	git__free(map);
}

/**
 * Take the current values map from the backend and increase its
 * refcount. This is its own function to make sure we use the mutex to
 * avoid the map pointer from changing under us.
 */
static refcounted_strmap *refcounted_strmap_take(diskfile_header *h)
{
	refcounted_strmap *map;

	git_mutex_lock(&h->values_mutex);

	map = h->values;
	git_atomic_inc(&map->refcount);

	git_mutex_unlock(&h->values_mutex);

	return map;
}

static int refcounted_strmap_alloc(refcounted_strmap **out)
{
	refcounted_strmap *map;
	int error;

	map = git__calloc(1, sizeof(refcounted_strmap));
	GITERR_CHECK_ALLOC(map);

	git_atomic_set(&map->refcount, 1);

	if ((error = git_strmap_alloc(&map->values)) < 0)
		git__free(map);
	else
		*out = map;

	return error;
}

static int config_open(git_config_backend *cfg, git_config_level_t level)
{
	int res;
	struct reader *reader;
	diskfile_backend *b = (diskfile_backend *)cfg;

	b->level = level;

	if ((res = refcounted_strmap_alloc(&b->header.values)) < 0)
		return res;

	git_array_init(b->readers);
	reader = git_array_alloc(b->readers);
	if (!reader) {
		refcounted_strmap_free(b->header.values);
		return -1;
	}
	memset(reader, 0, sizeof(struct reader));

	reader->file_path = git__strdup(b->file_path);
	GITERR_CHECK_ALLOC(reader->file_path);

	git_buf_init(&reader->buffer, 0);
	res = git_futils_readbuffer_updated(
		&reader->buffer, b->file_path, &reader->file_mtime, &reader->file_size, NULL);

	/* It's fine if the file doesn't exist */
	if (res == GIT_ENOTFOUND)
		return 0;

	if (res < 0 || (res = config_parse(b->header.values->values, b, reader, level, 0)) < 0) {
		refcounted_strmap_free(b->header.values);
		b->header.values = NULL;
	}

	reader = git_array_get(b->readers, 0);
	git_buf_free(&reader->buffer);

	return res;
}

/* The meat of the refresh, as we want to use it in different places */
static int config__refresh(git_config_backend *cfg)
{
	refcounted_strmap *values = NULL, *tmp;
	diskfile_backend *b = (diskfile_backend *)cfg;
	struct reader *reader = NULL;
	int error = 0;

	if ((error = refcounted_strmap_alloc(&values)) < 0)
		goto out;

	reader = git_array_get(b->readers, git_array_size(b->readers) - 1);
	GITERR_CHECK_ALLOC(reader);

	if ((error = config_parse(values->values, b, reader, b->level, 0)) < 0)
		goto out;

	git_mutex_lock(&b->header.values_mutex);

	tmp = b->header.values;
	b->header.values = values;
	values = tmp;

	git_mutex_unlock(&b->header.values_mutex);

out:
	refcounted_strmap_free(values);
	if (reader)
		git_buf_free(&reader->buffer);
	return error;
}

static int config_refresh(git_config_backend *cfg)
{
	int error = 0, updated = 0, any_updated = 0;
	diskfile_backend *b = (diskfile_backend *)cfg;
	struct reader *reader = NULL;
	uint32_t i;

	for (i = 0; i < git_array_size(b->readers); i++) {
		reader = git_array_get(b->readers, i);
		error = git_futils_readbuffer_updated(
			&reader->buffer, reader->file_path,
			&reader->file_mtime, &reader->file_size, &updated);

		if (error < 0 && error != GIT_ENOTFOUND)
			return error;

		if (updated)
			any_updated = 1;
	}

	if (!any_updated)
		return (error == GIT_ENOTFOUND) ? 0 : error;

	return config__refresh(cfg);
}

static void backend_free(git_config_backend *_backend)
{
	diskfile_backend *backend = (diskfile_backend *)_backend;
	uint32_t i;

	if (backend == NULL)
		return;

	for (i = 0; i < git_array_size(backend->readers); i++) {
		struct reader *r = git_array_get(backend->readers, i);
		git__free(r->file_path);
	}
	git_array_clear(backend->readers);

	git__free(backend->file_path);
	refcounted_strmap_free(backend->header.values);
	git_mutex_free(&backend->header.values_mutex);
	git__free(backend);
}

static void config_iterator_free(
	git_config_iterator* iter)
{
	iter->backend->free(iter->backend);
	git__free(iter);
}

static int config_iterator_next(
	git_config_entry **entry,
	git_config_iterator *iter)
{
	git_config_file_iter *it = (git_config_file_iter *) iter;
	diskfile_header *h = (diskfile_header *) it->parent.backend;
	git_strmap *values = h->values->values;
	int err = 0;
	cvar_t * var;

	if (it->next_var == NULL) {
		err = git_strmap_next((void**) &var, &(it->iter), values);
	} else {
		var = it->next_var;
	}

	if (err < 0) {
		it->next_var = NULL;
		return err;
	}

	*entry = var->entry;
	it->next_var = CVAR_LIST_NEXT(var);

	return 0;
}

static int config_iterator_new(
	git_config_iterator **iter,
	struct git_config_backend* backend)
{
	diskfile_header *h;
	git_config_file_iter *it;
	git_config_backend *snapshot;
	diskfile_backend *b = (diskfile_backend *) backend;
	int error;

	if ((error = config_snapshot(&snapshot, backend)) < 0)
		return error;

	if ((error = snapshot->open(snapshot, b->level)) < 0)
		return error;

	it = git__calloc(1, sizeof(git_config_file_iter));
	GITERR_CHECK_ALLOC(it);

	h = (diskfile_header *)snapshot;

	/* strmap_begin() is currently a macro returning 0 */
	GIT_UNUSED(h);

	it->parent.backend = snapshot;
	it->iter = git_strmap_begin(h->values);
	it->next_var = NULL;

	it->parent.next = config_iterator_next;
	it->parent.free = config_iterator_free;
	*iter = (git_config_iterator *) it;

	return 0;
}

static int config_set(git_config_backend *cfg, const char *name, const char *value)
{
	diskfile_backend *b = (diskfile_backend *)cfg;
	refcounted_strmap *map;
	git_strmap *values;
	char *key, *esc_value = NULL;
	khiter_t pos;
	int rval, ret;

	if ((rval = git_config__normalize_name(name, &key)) < 0)
		return rval;

	map = refcounted_strmap_take(&b->header);
	values = map->values;

	/*
	 * Try to find it in the existing values and update it if it
	 * only has one value.
	 */
	pos = git_strmap_lookup_index(values, key);
	if (git_strmap_valid_index(values, pos)) {
		cvar_t *existing = git_strmap_value_at(values, pos);

		if (existing->next != NULL) {
			giterr_set(GITERR_CONFIG, "Multivar incompatible with simple set");
			ret = -1;
			goto out;
		}

		/* don't update if old and new values already match */
		if ((!existing->entry->value && !value) ||
			(existing->entry->value && value &&
			 !strcmp(existing->entry->value, value))) {
			ret = 0;
			goto out;
		}
	}

	/* No early returns due to sanity checks, let's write it out and refresh */

	if (value) {
		esc_value = escape_value(value);
		GITERR_CHECK_ALLOC(esc_value);
	}

	if ((ret = config_write(b, key, NULL, esc_value)) < 0)
		goto out;

	ret = config_refresh(cfg);

out:
	refcounted_strmap_free(map);
	git__free(esc_value);
	git__free(key);
	return ret;
}

/*
 * Internal function that actually gets the value in string form
 */
static int config_get(git_config_backend *cfg, const char *key, const git_config_entry **out)
{
	diskfile_header *h = (diskfile_header *)cfg;
	refcounted_strmap *map;
	git_strmap *values;
	khiter_t pos;
	cvar_t *var;
	int error;

	if (!h->readonly && ((error = config_refresh(cfg)) < 0))
		return error;

	map = refcounted_strmap_take(h);
	values = map->values;

	pos = git_strmap_lookup_index(values, key);

	/* no error message; the config system will write one */
	if (!git_strmap_valid_index(values, pos)) {
		refcounted_strmap_free(map);
		return GIT_ENOTFOUND;
	}

	var = git_strmap_value_at(values, pos);
	while (var->next)
		var = var->next;

	refcounted_strmap_free(map);
	*out = var->entry;
	return 0;
}

static int config_set_multivar(
	git_config_backend *cfg, const char *name, const char *regexp, const char *value)
{
	diskfile_backend *b = (diskfile_backend *)cfg;
	refcounted_strmap *map;
	git_strmap *values;
	char *key;
	regex_t preg;
	int result;
	khiter_t pos;

	assert(regexp);

	if ((result = git_config__normalize_name(name, &key)) < 0)
		return result;

	map = refcounted_strmap_take(&b->header);
	values = b->header.values->values;

	pos = git_strmap_lookup_index(values, key);
	if (!git_strmap_valid_index(values, pos)) {
		/* If we don't have it, behave like a normal set */
		result = config_set(cfg, name, value);
		refcounted_strmap_free(map);
		git__free(key);
		return result;
	}

	result = regcomp(&preg, regexp, REG_EXTENDED);
	if (result < 0) {
		giterr_set_regex(&preg, result);
		result = -1;
		goto out;
	}

	/* If we do have it, set call config_write() and reload */
	if ((result = config_write(b, key, &preg, value)) < 0)
		goto out;

	result = config_refresh(cfg);

out:
	refcounted_strmap_free(map);
	git__free(key);
	regfree(&preg);

	return result;
}

static int config_delete(git_config_backend *cfg, const char *name)
{
	cvar_t *var;
	diskfile_backend *b = (diskfile_backend *)cfg;
	refcounted_strmap *map;	git_strmap *values;
	char *key;
	int result;
	khiter_t pos;

	if ((result = git_config__normalize_name(name, &key)) < 0)
		return result;

	map = refcounted_strmap_take(&b->header);
	values = b->header.values->values;

	pos = git_strmap_lookup_index(values, key);
	git__free(key);

	if (!git_strmap_valid_index(values, pos)) {
		refcounted_strmap_free(map);
		giterr_set(GITERR_CONFIG, "Could not find key '%s' to delete", name);
		return GIT_ENOTFOUND;
	}

	var = git_strmap_value_at(values, pos);
	refcounted_strmap_free(map);

	if (var->next != NULL) {
		giterr_set(GITERR_CONFIG, "Cannot delete multivar with a single delete");
		return -1;
	}

	if ((result = config_write(b, var->entry->name, NULL, NULL)) < 0)
		return result;

	return config_refresh(cfg);
}

static int config_delete_multivar(git_config_backend *cfg, const char *name, const char *regexp)
{
	diskfile_backend *b = (diskfile_backend *)cfg;
	refcounted_strmap *map;
	git_strmap *values;
	char *key;
	regex_t preg;
	int result;
	khiter_t pos;

	if ((result = git_config__normalize_name(name, &key)) < 0)
		return result;

	map = refcounted_strmap_take(&b->header);
	values = b->header.values->values;

	pos = git_strmap_lookup_index(values, key);

	if (!git_strmap_valid_index(values, pos)) {
		refcounted_strmap_free(map);
		git__free(key);
		giterr_set(GITERR_CONFIG, "Could not find key '%s' to delete", name);
		return GIT_ENOTFOUND;
	}

	refcounted_strmap_free(map);

	result = regcomp(&preg, regexp, REG_EXTENDED);
	if (result < 0) {
		giterr_set_regex(&preg, result);
		result = -1;
		goto out;
	}

	if ((result = config_write(b, key, &preg, NULL)) < 0)
		goto out;

	result = config_refresh(cfg);

out:
	git__free(key);
	regfree(&preg);
	return result;
}

static int config_snapshot(git_config_backend **out, git_config_backend *in)
{
	diskfile_backend *b = (diskfile_backend *) in;

	return git_config_file__snapshot(out, b);
}

int git_config_file__ondisk(git_config_backend **out, const char *path)
{
	diskfile_backend *backend;

	backend = git__calloc(1, sizeof(diskfile_backend));
	GITERR_CHECK_ALLOC(backend);

	backend->header.parent.version = GIT_CONFIG_BACKEND_VERSION;
	git_mutex_init(&backend->header.values_mutex);

	backend->file_path = git__strdup(path);
	GITERR_CHECK_ALLOC(backend->file_path);

	backend->header.parent.open = config_open;
	backend->header.parent.get = config_get;
	backend->header.parent.set = config_set;
	backend->header.parent.set_multivar = config_set_multivar;
	backend->header.parent.del = config_delete;
	backend->header.parent.del_multivar = config_delete_multivar;
	backend->header.parent.iterator = config_iterator_new;
	backend->header.parent.refresh = config_refresh;
	backend->header.parent.snapshot = config_snapshot;
	backend->header.parent.free = backend_free;

	*out = (git_config_backend *)backend;

	return 0;
}

static int config_set_readonly(git_config_backend *cfg, const char *name, const char *value)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(value);

	return config_error_readonly();
}

static int config_set_multivar_readonly(
	git_config_backend *cfg, const char *name, const char *regexp, const char *value)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);
	GIT_UNUSED(value);

	return config_error_readonly();
}

static int config_delete_multivar_readonly(git_config_backend *cfg, const char *name, const char *regexp)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);
	GIT_UNUSED(regexp);

	return config_error_readonly();
}

static int config_delete_readonly(git_config_backend *cfg, const char *name)
{
	GIT_UNUSED(cfg);
	GIT_UNUSED(name);

	return config_error_readonly();
}

static int config_refresh_readonly(git_config_backend *cfg)
{
	GIT_UNUSED(cfg);

	return config_error_readonly();
}

static void backend_readonly_free(git_config_backend *_backend)
{
	diskfile_backend *backend = (diskfile_backend *)_backend;

	if (backend == NULL)
		return;

	refcounted_strmap_free(backend->header.values);
	git_mutex_free(&backend->header.values_mutex);
	git__free(backend);
}

static int config_readonly_open(git_config_backend *cfg, git_config_level_t level)
{
	diskfile_readonly_backend *b = (diskfile_readonly_backend *) cfg;
	diskfile_backend *src = b->snapshot_from;
	refcounted_strmap *src_map;

	/* We're just copying data, don't care about the level */
	GIT_UNUSED(level);

	src_map = refcounted_strmap_take(&src->header);
	b->header.values = src_map;

	return 0;
}

int git_config_file__snapshot(git_config_backend **out, diskfile_backend *in)
{
	diskfile_readonly_backend *backend;

	backend = git__calloc(1, sizeof(diskfile_readonly_backend));
	GITERR_CHECK_ALLOC(backend);

	backend->header.parent.version = GIT_CONFIG_BACKEND_VERSION;
	git_mutex_init(&backend->header.values_mutex);

	backend->snapshot_from = in;

	backend->header.readonly = 1;
	backend->header.parent.version = GIT_CONFIG_BACKEND_VERSION;
	backend->header.parent.open = config_readonly_open;
	backend->header.parent.get = config_get;
	backend->header.parent.set = config_set_readonly;
	backend->header.parent.set_multivar = config_set_multivar_readonly;
	backend->header.parent.del = config_delete_readonly;
	backend->header.parent.del_multivar = config_delete_multivar_readonly;
	backend->header.parent.iterator = config_iterator_new;
	backend->header.parent.refresh = config_refresh_readonly;
	backend->header.parent.free = backend_readonly_free;

	*out = (git_config_backend *)backend;

	return 0;
}

static int reader_getchar_raw(struct reader *reader)
{
	int c;

	c = *reader->read_ptr++;

	/*
	Win 32 line breaks: if we find a \r\n sequence,
	return only the \n as a newline
	*/
	if (c == '\r' && *reader->read_ptr == '\n') {
		reader->read_ptr++;
		c = '\n';
	}

	if (c == '\n')
		reader->line_number++;

	if (c == 0) {
		reader->eof = 1;
		c = '\n';
	}

	return c;
}

#define SKIP_WHITESPACE (1 << 1)
#define SKIP_COMMENTS (1 << 2)

static int reader_getchar(struct reader *reader, int flags)
{
	const int skip_whitespace = (flags & SKIP_WHITESPACE);
	const int skip_comments = (flags & SKIP_COMMENTS);
	int c;

	assert(reader->read_ptr);

	do {
		c = reader_getchar_raw(reader);
	} while (skip_whitespace && git__isspace(c) &&
	       !reader->eof);

	if (skip_comments && (c == '#' || c == ';')) {
		do {
			c = reader_getchar_raw(reader);
		} while (c != '\n');
	}

	return c;
}

/*
 * Read the next char, but don't move the reading pointer.
 */
static int reader_peek(struct reader *reader, int flags)
{
	void *old_read_ptr;
	int old_lineno, old_eof;
	int ret;

	assert(reader->read_ptr);

	old_read_ptr = reader->read_ptr;
	old_lineno = reader->line_number;
	old_eof = reader->eof;

	ret = reader_getchar(reader, flags);

	reader->read_ptr = old_read_ptr;
	reader->line_number = old_lineno;
	reader->eof = old_eof;

	return ret;
}

/*
 * Read and consume a line, returning it in newly-allocated memory.
 */
static char *reader_readline(struct reader *reader, bool skip_whitespace)
{
	char *line = NULL;
	char *line_src, *line_end;
	size_t line_len;

	line_src = reader->read_ptr;

	if (skip_whitespace) {
		/* Skip empty empty lines */
		while (git__isspace(*line_src))
			++line_src;
	}

	line_end = strchr(line_src, '\n');

	/* no newline at EOF */
	if (line_end == NULL)
		line_end = strchr(line_src, 0);

	line_len = line_end - line_src;

	line = git__malloc(line_len + 1);
	if (line == NULL)
		return NULL;

	memcpy(line, line_src, line_len);

	do line[line_len] = '\0';
	while (line_len-- > 0 && git__isspace(line[line_len]));

	if (*line_end == '\n')
		line_end++;

	if (*line_end == '\0')
		reader->eof = 1;

	reader->line_number++;
	reader->read_ptr = line_end;

	return line;
}

/*
 * Consume a line, without storing it anywhere
 */
static void reader_consume_line(struct reader *reader)
{
	char *line_start, *line_end;

	line_start = reader->read_ptr;
	line_end = strchr(line_start, '\n');
	/* No newline at EOF */
	if(line_end == NULL){
		line_end = strchr(line_start, '\0');
	}

	if (*line_end == '\n')
		line_end++;

	if (*line_end == '\0')
		reader->eof = 1;

	reader->line_number++;
	reader->read_ptr = line_end;
}

GIT_INLINE(int) config_keychar(int c)
{
	return isalnum(c) || c == '-';
}

static int parse_section_header_ext(struct reader *reader, const char *line, const char *base_name, char **section_name)
{
	int c, rpos;
	char *first_quote, *last_quote;
	git_buf buf = GIT_BUF_INIT;
	/*
	 * base_name is what came before the space. We should be at the
	 * first quotation mark, except for now, line isn't being kept in
	 * sync so we only really use it to calculate the length.
	 */

	first_quote = strchr(line, '"');
	last_quote = strrchr(line, '"');

	if (last_quote - first_quote == 0) {
		set_parse_error(reader, 0, "Missing closing quotation mark in section header");
		return -1;
	}

	git_buf_grow(&buf, strlen(base_name) + last_quote - first_quote + 2);
	git_buf_printf(&buf, "%s.", base_name);

	rpos = 0;

	line = first_quote;
	c = line[++rpos];

	/*
	 * At the end of each iteration, whatever is stored in c will be
	 * added to the string. In case of error, jump to out
	 */
	do {

		switch (c) {
		case 0:
			set_parse_error(reader, 0, "Unexpected end-of-line in section header");
			git_buf_free(&buf);
			return -1;

		case '"':
			goto end_parse;

		case '\\':
			c = line[++rpos];

			if (c == 0) {
				set_parse_error(reader, rpos, "Unexpected end-of-line in section header");
				git_buf_free(&buf);
				return -1;
			}

		default:
			break;
		}

		git_buf_putc(&buf, (char)c);
		c = line[++rpos];
	} while (line + rpos < last_quote);

end_parse:
	if (line[rpos] != '"' || line[rpos + 1] != ']') {
		set_parse_error(reader, rpos, "Unexpected text after closing quotes");
		git_buf_free(&buf);
		return -1;
	}

	*section_name = git_buf_detach(&buf);
	return 0;
}

static int parse_section_header(struct reader *reader, char **section_out)
{
	char *name, *name_end;
	int name_length, c, pos;
	int result;
	char *line;

	line = reader_readline(reader, true);
	if (line == NULL)
		return -1;

	/* find the end of the variable's name */
	name_end = strrchr(line, ']');
	if (name_end == NULL) {
		git__free(line);
		set_parse_error(reader, 0, "Missing ']' in section header");
		return -1;
	}

	name = (char *)git__malloc((size_t)(name_end - line) + 1);
	GITERR_CHECK_ALLOC(name);

	name_length = 0;
	pos = 0;

	/* Make sure we were given a section header */
	c = line[pos++];
	assert(c == '[');

	c = line[pos++];

	do {
		if (git__isspace(c)){
			name[name_length] = '\0';
			result = parse_section_header_ext(reader, line, name, section_out);
			git__free(line);
			git__free(name);
			return result;
		}

		if (!config_keychar(c) && c != '.') {
			set_parse_error(reader, pos, "Unexpected character in header");
			goto fail_parse;
		}

		name[name_length++] = (char) tolower(c);

	} while ((c = line[pos++]) != ']');

	if (line[pos - 1] != ']') {
		set_parse_error(reader, pos, "Unexpected end of file");
		goto fail_parse;
	}

	git__free(line);

	name[name_length] = 0;
	*section_out = name;

	return 0;

fail_parse:
	git__free(line);
	git__free(name);
	return -1;
}

static int skip_bom(struct reader *reader)
{
	git_bom_t bom;
	int bom_offset = git_buf_text_detect_bom(&bom,
		&reader->buffer, reader->read_ptr - reader->buffer.ptr);

	if (bom == GIT_BOM_UTF8)
		reader->read_ptr += bom_offset;

	/* TODO: reference implementation is pretty stupid with BoM */

	return 0;
}

/*
	(* basic types *)
	digit = "0".."9"
	integer = digit { digit }
	alphabet = "a".."z" + "A" .. "Z"

	section_char = alphabet | "." | "-"
	extension_char = (* any character except newline *)
	any_char = (* any character *)
	variable_char = "alphabet" | "-"


	(* actual grammar *)
	config = { section }

	section = header { definition }

	header = "[" section [subsection | subsection_ext] "]"

	subsection = "." section
	subsection_ext = "\"" extension "\""

	section = section_char { section_char }
	extension = extension_char { extension_char }

	definition = variable_name ["=" variable_value] "\n"

	variable_name = variable_char { variable_char }
	variable_value = string | boolean | integer

	string = quoted_string | plain_string
	quoted_string = "\"" plain_string "\""
	plain_string = { any_char }

	boolean = boolean_true | boolean_false
	boolean_true = "yes" | "1" | "true" | "on"
	boolean_false = "no" | "0" | "false" | "off"
*/

static int strip_comments(char *line, int in_quotes)
{
	int quote_count = in_quotes;
	char *ptr;

	for (ptr = line; *ptr; ++ptr) {
		if (ptr[0] == '"' && ptr > line && ptr[-1] != '\\')
			quote_count++;

		if ((ptr[0] == ';' || ptr[0] == '#') && (quote_count % 2) == 0) {
			ptr[0] = '\0';
			break;
		}
	}

	/* skip any space at the end */
	if (ptr > line && git__isspace(ptr[-1])) {
		ptr--;
	}
	ptr[0] = '\0';

	return quote_count;
}

static int included_path(git_buf *out, const char *dir, const char *path)
{
	/* From the user's home */
	if (path[0] == '~' && path[1] == '/')
		return git_sysdir_find_global_file(out, &path[1]);

	return git_path_join_unrooted(out, path, dir, NULL);
}

static int config_parse(git_strmap *values, diskfile_backend *cfg_file, struct reader *reader, git_config_level_t level, int depth)
{
	int c;
	char *current_section = NULL;
	char *var_name;
	char *var_value;
	cvar_t *var;
	git_buf buf = GIT_BUF_INIT;
	int result = 0;
	uint32_t reader_idx;

	if (depth >= MAX_INCLUDE_DEPTH) {
		giterr_set(GITERR_CONFIG, "Maximum config include depth reached");
		return -1;
	}

	reader_idx = git_array_size(cfg_file->readers) - 1;
	/* Initialize the reading position */
	reader->read_ptr = reader->buffer.ptr;
	reader->eof = 0;

	/* If the file is empty, there's nothing for us to do */
	if (*reader->read_ptr == '\0')
		return 0;

	skip_bom(reader);

	while (result == 0 && !reader->eof) {

		c = reader_peek(reader, SKIP_WHITESPACE);

		switch (c) {
		case '\n': /* EOF when peeking, set EOF in the reader to exit the loop */
			reader->eof = 1;
			break;

		case '[': /* section header, new section begins */
			git__free(current_section);
			current_section = NULL;
			result = parse_section_header(reader, &current_section);
			break;

		case ';':
		case '#':
			reader_consume_line(reader);
			break;

		default: /* assume variable declaration */
			result = parse_variable(reader, &var_name, &var_value);
			if (result < 0)
				break;

			var = git__malloc(sizeof(cvar_t));
			GITERR_CHECK_ALLOC(var);
			memset(var, 0x0, sizeof(cvar_t));
			var->entry = git__malloc(sizeof(git_config_entry));
			GITERR_CHECK_ALLOC(var->entry);
			memset(var->entry, 0x0, sizeof(git_config_entry));

			git__strtolower(var_name);
			git_buf_printf(&buf, "%s.%s", current_section, var_name);
			git__free(var_name);

			if (git_buf_oom(&buf)) {
				git__free(var_value);
				return -1;
			}

			var->entry->name = git_buf_detach(&buf);
			var->entry->value = var_value;
			var->entry->level = level;
			var->included = !!depth;


			if ((result = append_entry(values, var)) < 0)
				break;
			else
				result = 0;

			/* Add or append the new config option */
			if (!git__strcmp(var->entry->name, "include.path")) {
				struct reader *r;
				git_buf path = GIT_BUF_INIT;
				char *dir;
				uint32_t index;

				r = git_array_alloc(cfg_file->readers);
				/* The reader may have been reallocated */
				reader = git_array_get(cfg_file->readers, reader_idx);
				memset(r, 0, sizeof(struct reader));
				if ((result = git_path_dirname_r(&path, reader->file_path)) < 0)
					break;

				/* We need to know out index in the array, as the next config_parse call may realloc */
				index = git_array_size(cfg_file->readers) - 1;
				dir = git_buf_detach(&path);
				result = included_path(&path, dir, var->entry->value);
				git__free(dir);

				if (result < 0)
					break;

				r->file_path = git_buf_detach(&path);
				git_buf_init(&r->buffer, 0);
				if ((result = git_futils_readbuffer_updated(&r->buffer, r->file_path, &r->file_mtime,
									    &r->file_size, NULL)) < 0)
					break;

				result = config_parse(values, cfg_file, r, level, depth+1);
				r = git_array_get(cfg_file->readers, index);
				git_buf_free(&r->buffer);

				if (result < 0)
					break;
			}

			break;
		}
	}

	git__free(current_section);
	return result;
}

static int write_section(git_filebuf *file, const char *key)
{
	int result;
	const char *dot;
	git_buf buf = GIT_BUF_INIT;

	/* All of this just for [section "subsection"] */
	dot = strchr(key, '.');
	git_buf_putc(&buf, '[');
	if (dot == NULL) {
		git_buf_puts(&buf, key);
	} else {
		char *escaped;
		git_buf_put(&buf, key, dot - key);
		escaped = escape_value(dot + 1);
		GITERR_CHECK_ALLOC(escaped);
		git_buf_printf(&buf, " \"%s\"", escaped);
		git__free(escaped);
	}
	git_buf_puts(&buf, "]\n");

	if (git_buf_oom(&buf))
		return -1;

	result = git_filebuf_write(file, git_buf_cstr(&buf), buf.size);
	git_buf_free(&buf);

	return result;
}

static const char *quotes_for_value(const char *value)
{
	const char *ptr;

	if (value[0] == ' ' || value[0] == '\0')
		return "\"";

	for (ptr = value; *ptr; ++ptr) {
		if (*ptr == ';' || *ptr == '#')
			return "\"";
	}

	if (ptr[-1] == ' ')
		return "\"";

	return "";
}

/*
 * This is pretty much the parsing, except we write out anything we don't have
 */
static int config_write(diskfile_backend *cfg, const char *key, const regex_t *preg, const char* value)
{
	int result, c;
	int section_matches = 0, last_section_matched = 0, preg_replaced = 0, write_trailer = 0;
	const char *pre_end = NULL, *post_start = NULL, *data_start, *write_start;
	char *current_section = NULL, *section, *name, *ldot;
	git_filebuf file = GIT_FILEBUF_INIT;
	struct reader *reader = git_array_get(cfg->readers, 0);

	/* We need to read in our own config file */
	result = git_futils_readbuffer(&reader->buffer, cfg->file_path);

	/* Initialise the reading position */
	if (result == GIT_ENOTFOUND) {
		reader->read_ptr = NULL;
		reader->eof = 1;
		data_start = NULL;
		git_buf_clear(&reader->buffer);
	} else if (result == 0) {
		reader->read_ptr = reader->buffer.ptr;
		reader->eof = 0;
		data_start = reader->read_ptr;
	} else {
		return -1; /* OS error when reading the file */
	}

	write_start = data_start;

	/* Lock the file */
	if ((result = git_filebuf_open(
		&file, cfg->file_path, 0, GIT_CONFIG_FILE_MODE)) < 0) {
			git_buf_free(&reader->buffer);
			return result;
	}

	skip_bom(reader);
	ldot = strrchr(key, '.');
	name = ldot + 1;
	section = git__strndup(key, ldot - key);

	while (!reader->eof) {
		c = reader_peek(reader, SKIP_WHITESPACE);

		if (c == '\0') { /* We've arrived at the end of the file */
			break;

		} else if (c == '[') { /* section header, new section begins */
			/*
			 * We set both positions to the current one in case we
			 * need to add a variable to the end of a section. In that
			 * case, we want both variables to point just before the
			 * new section. If we actually want to replace it, the
			 * default case will take care of updating them.
			 */
			pre_end = post_start = reader->read_ptr;

			git__free(current_section);
			current_section = NULL;
			if (parse_section_header(reader, &current_section) < 0)
				goto rewrite_fail;

			/* Keep track of when it stops matching */
			last_section_matched = section_matches;
			section_matches = !strcmp(current_section, section);
		}

		else if (c == ';' || c == '#') {
			reader_consume_line(reader);
		}

		else {
			/*
			 * If the section doesn't match, but the last section did,
			 * it means we need to add a variable (so skip the line
			 * otherwise). If both the section and name match, we need
			 * to overwrite the variable (so skip the line
			 * otherwise). pre_end needs to be updated each time so we
			 * don't loose that information, but we only need to
			 * update post_start if we're going to use it in this
			 * iteration.
			 */
			if (!section_matches) {
				if (!last_section_matched) {
					reader_consume_line(reader);
					continue;
				}
			} else {
				int has_matched = 0;
				char *var_name, *var_value;

				pre_end = reader->read_ptr;
				if (parse_variable(reader, &var_name, &var_value) < 0)
					goto rewrite_fail;

				/* First try to match the name of the variable */
				if (strcasecmp(name, var_name) == 0)
					has_matched = 1;

				/* If the name matches, and we have a regex to match the
				 * value, try to match it */
				if (has_matched && preg != NULL)
					has_matched = (regexec(preg, var_value, 0, NULL, 0) == 0);

				git__free(var_name);
				git__free(var_value);

				/* if there is no match, keep going */
				if (!has_matched)
					continue;

				post_start = reader->read_ptr;
			}

			/* We've found the variable we wanted to change, so
			 * write anything up to it */
			git_filebuf_write(&file, write_start, pre_end - write_start);
			preg_replaced = 1;

			/* Then replace the variable. If the value is NULL, it
			 * means we want to delete it, so don't write anything. */
			if (value != NULL) {
				const char *q = quotes_for_value(value);
				git_filebuf_printf(&file, "\t%s = %s%s%s\n", name, q, value, q);
			}

			/*
			 * If we have a multivar, we should keep looking for entries,
			 * but only if we're in the right section. Otherwise we'll end up
			 * looping on the edge of a matching and a non-matching section.
			 */
			if (section_matches && preg != NULL) {
				write_start = post_start;
				continue;
			}

			write_trailer = 1;
			break; /* break from the loop */
		}
	}

	/*
	 * Being here can mean that
	 *
	 * 1) our section is the last one in the file and we're
	 * adding a variable
	 *
	 * 2) we didn't find a section for us so we need to create it
	 * ourselves.
	 *
	 * 3) we're setting a multivar with a regex, which means we
	 * continue to search for matching values
	 *
	 * In the last case, if we've already replaced a value, we
	 * want to write the rest of the file. Otherwise we need to write
	 * out the whole file and then the new variable.
	 */
	if (write_trailer) {
		/* Write out rest of the file */
		git_filebuf_write(&file, post_start, reader->buffer.size - (post_start - data_start));
	} else {
		if (preg_replaced) {
			git_filebuf_printf(&file, "\n%s", write_start);
		} else {
			const char *q;

			git_filebuf_write(&file, reader->buffer.ptr, reader->buffer.size);

			/* And now if we just need to add a variable */
			if (!section_matches && write_section(&file, section) < 0)
				goto rewrite_fail;

			/* Sanity check: if we are here, and value is NULL, that means that somebody
			 * touched the config file after our intial read. We should probably assert()
			 * this, but instead we'll handle it gracefully with an error. */
			if (value == NULL) {
				giterr_set(GITERR_CONFIG,
					"race condition when writing a config file (a cvar has been removed)");
				goto rewrite_fail;
			}

			/* If we are here, there is at least a section line */
			if (reader->buffer.size > 0 && *(reader->buffer.ptr + reader->buffer.size - 1) != '\n')
				git_filebuf_write(&file, "\n", 1);

			q = quotes_for_value(value);
			git_filebuf_printf(&file, "\t%s = %s%s%s\n", name, q, value, q);
		}
	}

	git__free(section);
	git__free(current_section);

	/* refresh stats - if this errors, then commit will error too */
	(void)git_filebuf_stats(&reader->file_mtime, &reader->file_size, &file);

	result = git_filebuf_commit(&file);
	git_buf_free(&reader->buffer);

	return result;

rewrite_fail:
	git__free(section);
	git__free(current_section);

	git_filebuf_cleanup(&file);
	git_buf_free(&reader->buffer);
	return -1;
}

static const char *escapes = "ntb\"\\";
static const char *escaped = "\n\t\b\"\\";

/* Escape the values to write them to the file */
static char *escape_value(const char *ptr)
{
	git_buf buf = GIT_BUF_INIT;
	size_t len;
	const char *esc;

	assert(ptr);

	len = strlen(ptr);
	if (!len)
		return git__calloc(1, sizeof(char));

	git_buf_grow(&buf, len);

	while (*ptr != '\0') {
		if ((esc = strchr(escaped, *ptr)) != NULL) {
			git_buf_putc(&buf, '\\');
			git_buf_putc(&buf, escapes[esc - escaped]);
		} else {
			git_buf_putc(&buf, *ptr);
		}
		ptr++;
	}

	if (git_buf_oom(&buf)) {
		git_buf_free(&buf);
		return NULL;
	}

	return git_buf_detach(&buf);
}

/* '\"' -> '"' etc */
static char *fixup_line(const char *ptr, int quote_count)
{
	char *str = git__malloc(strlen(ptr) + 1);
	char *out = str, *esc;

	if (str == NULL)
		return NULL;

	while (*ptr != '\0') {
		if (*ptr == '"') {
			quote_count++;
		} else if (*ptr != '\\') {
			*out++ = *ptr;
		} else {
			/* backslash, check the next char */
			ptr++;
			/* if we're at the end, it's a multiline, so keep the backslash */
			if (*ptr == '\0') {
				*out++ = '\\';
				goto out;
			}
			if ((esc = strchr(escapes, *ptr)) != NULL) {
				*out++ = escaped[esc - escapes];
			} else {
				git__free(str);
				giterr_set(GITERR_CONFIG, "Invalid escape at %s", ptr);
				return NULL;
			}
		}
		ptr++;
	}

out:
	*out = '\0';

	return str;
}

static int is_multiline_var(const char *str)
{
	int count = 0;
	const char *end = str + strlen(str);
	while (end > str && end[-1] == '\\') {
		count++;
		end--;
	}

	/* An odd number means last backslash wasn't escaped, so it's multiline */
	return (end > str) && (count & 1);
}

static int parse_multiline_variable(struct reader *reader, git_buf *value, int in_quotes)
{
	char *line = NULL, *proc_line = NULL;
	int quote_count;

	/* Check that the next line exists */
	line = reader_readline(reader, false);
	if (line == NULL)
		return -1;

	/* We've reached the end of the file, there is input missing */
	if (line[0] == '\0') {
		set_parse_error(reader, 0, "Unexpected end of file while parsing multine var");
		git__free(line);
		return -1;
	}

	quote_count = strip_comments(line, !!in_quotes);

	/* If it was just a comment, pretend it didn't exist */
	if (line[0] == '\0') {
		git__free(line);
		return parse_multiline_variable(reader, value, quote_count);
		/* TODO: unbounded recursion. This **could** be exploitable */
	}

	/* Drop the continuation character '\': to closely follow the UNIX
	 * standard, this character **has** to be last one in the buf, with
	 * no whitespace after it */
	assert(is_multiline_var(value->ptr));
	git_buf_shorten(value, 1);

	proc_line = fixup_line(line, in_quotes);
	if (proc_line == NULL) {
		git__free(line);
		return -1;
	}
	/* add this line to the multiline var */
	git_buf_puts(value, proc_line);
	git__free(line);
	git__free(proc_line);

	/*
	 * If we need to continue reading the next line, let's just
	 * keep putting stuff in the buffer
	 */
	if (is_multiline_var(value->ptr))
		return parse_multiline_variable(reader, value, quote_count);

	return 0;
}

static int parse_variable(struct reader *reader, char **var_name, char **var_value)
{
	const char *var_end = NULL;
	const char *value_start = NULL;
	char *line;
	int quote_count;

	line = reader_readline(reader, true);
	if (line == NULL)
		return -1;

	quote_count = strip_comments(line, 0);

	var_end = strchr(line, '=');

	if (var_end == NULL)
		var_end = strchr(line, '\0');
	else
		value_start = var_end + 1;

	do var_end--;
	while (var_end>line && git__isspace(*var_end));

	*var_name = git__strndup(line, var_end - line + 1);
	GITERR_CHECK_ALLOC(*var_name);

	/* If there is no value, boolean true is assumed */
	*var_value = NULL;

	/*
	 * Now, let's try to parse the value
	 */
	if (value_start != NULL) {
		while (git__isspace(value_start[0]))
			value_start++;

		if (is_multiline_var(value_start)) {
			git_buf multi_value = GIT_BUF_INIT;
			char *proc_line = fixup_line(value_start, 0);
			GITERR_CHECK_ALLOC(proc_line);
			git_buf_puts(&multi_value, proc_line);
			git__free(proc_line);
			if (parse_multiline_variable(reader, &multi_value, quote_count) < 0 || git_buf_oom(&multi_value)) {
				git__free(*var_name);
				git__free(line);
				git_buf_free(&multi_value);
				return -1;
			}

			*var_value = git_buf_detach(&multi_value);

		}
		else if (value_start[0] != '\0') {
			*var_value = fixup_line(value_start, 0);
			GITERR_CHECK_ALLOC(*var_value);
		} else { /* equals sign but missing rhs */
			*var_value = git__strdup("");
			GITERR_CHECK_ALLOC(*var_value);
		}
	}

	git__free(line);
	return 0;
}
