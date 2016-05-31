/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "sysdir.h"
#include "config.h"
#include "git2/config.h"
#include "git2/sys/config.h"
#include "vector.h"
#include "buf_text.h"
#include "config_file.h"
#include "transaction.h"
#if GIT_WIN32
# include <windows.h>
#endif

#include <ctype.h>

void git_config_entry_free(git_config_entry *entry)
{
	if (!entry)
		return;

	entry->free(entry);
}

typedef struct {
	git_refcount rc;

	git_config_backend *file;
	git_config_level_t level;
} file_internal;

static void file_internal_free(file_internal *internal)
{
	git_config_backend *file;

	file = internal->file;
	file->free(file);
	git__free(internal);
}

static void config_free(git_config *cfg)
{
	size_t i;
	file_internal *internal;

	for (i = 0; i < cfg->files.length; ++i) {
		internal = git_vector_get(&cfg->files, i);
		GIT_REFCOUNT_DEC(internal, file_internal_free);
	}

	git_vector_free(&cfg->files);

	git__memzero(cfg, sizeof(*cfg));
	git__free(cfg);
}

void git_config_free(git_config *cfg)
{
	if (cfg == NULL)
		return;

	GIT_REFCOUNT_DEC(cfg, config_free);
}

static int config_backend_cmp(const void *a, const void *b)
{
	const file_internal *bk_a = (const file_internal *)(a);
	const file_internal *bk_b = (const file_internal *)(b);

	return bk_b->level - bk_a->level;
}

int git_config_new(git_config **out)
{
	git_config *cfg;

	cfg = git__malloc(sizeof(git_config));
	GITERR_CHECK_ALLOC(cfg);

	memset(cfg, 0x0, sizeof(git_config));

	if (git_vector_init(&cfg->files, 3, config_backend_cmp) < 0) {
		git__free(cfg);
		return -1;
	}

	*out = cfg;
	GIT_REFCOUNT_INC(cfg);
	return 0;
}

int git_config_add_file_ondisk(
	git_config *cfg,
	const char *path,
	git_config_level_t level,
	int force)
{
	git_config_backend *file = NULL;
	struct stat st;
	int res;

	assert(cfg && path);

	res = p_stat(path, &st);
	if (res < 0 && errno != ENOENT) {
		giterr_set(GITERR_CONFIG, "Error stat'ing config file '%s'", path);
		return -1;
	}

	if (git_config_file__ondisk(&file, path) < 0)
		return -1;

	if ((res = git_config_add_backend(cfg, file, level, force)) < 0) {
		/*
		 * free manually; the file is not owned by the config
		 * instance yet and will not be freed on cleanup
		 */
		file->free(file);
		return res;
	}

	return 0;
}

int git_config_open_ondisk(git_config **out, const char *path)
{
	int error;
	git_config *config;

	*out = NULL;

	if (git_config_new(&config) < 0)
		return -1;

	if ((error = git_config_add_file_ondisk(config, path, GIT_CONFIG_LEVEL_LOCAL, 0)) < 0)
		git_config_free(config);
	else
		*out = config;

	return error;
}

int git_config_snapshot(git_config **out, git_config *in)
{
	int error = 0;
	size_t i;
	file_internal *internal;
	git_config *config;

	*out = NULL;

	if (git_config_new(&config) < 0)
		return -1;

	git_vector_foreach(&in->files, i, internal) {
		git_config_backend *b;

		if ((error = internal->file->snapshot(&b, internal->file)) < 0)
			break;

		if ((error = git_config_add_backend(config, b, internal->level, 0)) < 0) {
			b->free(b);
			break;
		}
	}

	if (error < 0)
		git_config_free(config);
	else
		*out = config;

	return error;
}

static int find_internal_file_by_level(
	file_internal **internal_out,
	const git_config *cfg,
	git_config_level_t level)
{
	int pos = -1;
	file_internal *internal;
	size_t i;

	/* when passing GIT_CONFIG_HIGHEST_LEVEL, the idea is to get the config file
	 * which has the highest level. As config files are stored in a vector
	 * sorted by decreasing order of level, getting the file at position 0
	 * will do the job.
	 */
	if (level == GIT_CONFIG_HIGHEST_LEVEL) {
		pos = 0;
	} else {
		git_vector_foreach(&cfg->files, i, internal) {
			if (internal->level == level)
				pos = (int)i;
		}
	}

	if (pos == -1) {
		giterr_set(GITERR_CONFIG,
			"No config file exists for the given level '%i'", (int)level);
		return GIT_ENOTFOUND;
	}

	*internal_out = git_vector_get(&cfg->files, pos);

	return 0;
}

static int duplicate_level(void **old_raw, void *new_raw)
{
	file_internal **old = (file_internal **)old_raw;

	GIT_UNUSED(new_raw);

	giterr_set(GITERR_CONFIG, "A file with the same level (%i) has already been added to the config", (int)(*old)->level);
	return GIT_EEXISTS;
}

static void try_remove_existing_file_internal(
	git_config *cfg,
	git_config_level_t level)
{
	int pos = -1;
	file_internal *internal;
	size_t i;

	git_vector_foreach(&cfg->files, i, internal) {
		if (internal->level == level)
			pos = (int)i;
	}

	if (pos == -1)
		return;

	internal = git_vector_get(&cfg->files, pos);

	if (git_vector_remove(&cfg->files, pos) < 0)
		return;

	GIT_REFCOUNT_DEC(internal, file_internal_free);
}

static int git_config__add_internal(
	git_config *cfg,
	file_internal *internal,
	git_config_level_t level,
	int force)
{
	int result;

	/* delete existing config file for level if it exists */
	if (force)
		try_remove_existing_file_internal(cfg, level);

	if ((result = git_vector_insert_sorted(&cfg->files,
			internal, &duplicate_level)) < 0)
		return result;

	git_vector_sort(&cfg->files);
	internal->file->cfg = cfg;

	GIT_REFCOUNT_INC(internal);

	return 0;
}

int git_config_open_global(git_config **cfg_out, git_config *cfg)
{
	if (!git_config_open_level(cfg_out, cfg, GIT_CONFIG_LEVEL_XDG))
		return 0;

	return git_config_open_level(cfg_out, cfg, GIT_CONFIG_LEVEL_GLOBAL);
}

int git_config_open_level(
	git_config **cfg_out,
	const git_config *cfg_parent,
	git_config_level_t level)
{
	git_config *cfg;
	file_internal *internal;
	int res;

	if ((res = find_internal_file_by_level(&internal, cfg_parent, level)) < 0)
		return res;

	if ((res = git_config_new(&cfg)) < 0)
		return res;

	if ((res = git_config__add_internal(cfg, internal, level, true)) < 0) {
		git_config_free(cfg);
		return res;
	}

	*cfg_out = cfg;

	return 0;
}

int git_config_add_backend(
	git_config *cfg,
	git_config_backend *file,
	git_config_level_t level,
	int force)
{
	file_internal *internal;
	int result;

	assert(cfg && file);

	GITERR_CHECK_VERSION(file, GIT_CONFIG_BACKEND_VERSION, "git_config_backend");

	if ((result = file->open(file, level)) < 0)
		return result;

	internal = git__malloc(sizeof(file_internal));
	GITERR_CHECK_ALLOC(internal);

	memset(internal, 0x0, sizeof(file_internal));

	internal->file = file;
	internal->level = level;

	if ((result = git_config__add_internal(cfg, internal, level, force)) < 0) {
		git__free(internal);
		return result;
	}

	return 0;
}

/*
 * Loop over all the variables
 */

typedef struct {
	git_config_iterator parent;
	git_config_iterator *current;
	const git_config *cfg;
	regex_t regex;
	size_t i;
} all_iter;

static int find_next_backend(size_t *out, const git_config *cfg, size_t i)
{
	file_internal *internal;

	for (; i > 0; --i) {
		internal = git_vector_get(&cfg->files, i - 1);
		if (!internal || !internal->file)
			continue;

		*out = i;
		return 0;
	}

	return -1;
}

static int all_iter_next(git_config_entry **entry, git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;
	file_internal *internal;
	git_config_backend *backend;
	size_t i;
	int error = 0;

	if (iter->current != NULL &&
	    (error = iter->current->next(entry, iter->current)) == 0) {
		return 0;
	}

	if (error < 0 && error != GIT_ITEROVER)
		return error;

	do {
		if (find_next_backend(&i, iter->cfg, iter->i) < 0)
			return GIT_ITEROVER;

		internal = git_vector_get(&iter->cfg->files, i - 1);
		backend = internal->file;
		iter->i = i - 1;

		if (iter->current)
			iter->current->free(iter->current);

		iter->current = NULL;
		error = backend->iterator(&iter->current, backend);
		if (error == GIT_ENOTFOUND)
			continue;

		if (error < 0)
			return error;

		error = iter->current->next(entry, iter->current);
		/* If this backend is empty, then keep going */
		if (error == GIT_ITEROVER)
			continue;

		return error;

	} while(1);

	return GIT_ITEROVER;
}

static int all_iter_glob_next(git_config_entry **entry, git_config_iterator *_iter)
{
	int error;
	all_iter *iter = (all_iter *) _iter;

	/*
	 * We use the "normal" function to grab the next one across
	 * backends and then apply the regex
	 */
	while ((error = all_iter_next(entry, _iter)) == 0) {
		/* skip non-matching keys if regexp was provided */
		if (regexec(&iter->regex, (*entry)->name, 0, NULL, 0) != 0)
			continue;

		/* and simply return if we like the entry's name */
		return 0;
	}

	return error;
}

static void all_iter_free(git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;

	if (iter->current)
		iter->current->free(iter->current);

	git__free(iter);
}

static void all_iter_glob_free(git_config_iterator *_iter)
{
	all_iter *iter = (all_iter *) _iter;

	regfree(&iter->regex);
	all_iter_free(_iter);
}

int git_config_iterator_new(git_config_iterator **out, const git_config *cfg)
{
	all_iter *iter;

	iter = git__calloc(1, sizeof(all_iter));
	GITERR_CHECK_ALLOC(iter);

	iter->parent.free = all_iter_free;
	iter->parent.next = all_iter_next;

	iter->i = cfg->files.length;
	iter->cfg = cfg;

	*out = (git_config_iterator *) iter;

	return 0;
}

int git_config_iterator_glob_new(git_config_iterator **out, const git_config *cfg, const char *regexp)
{
	all_iter *iter;
	int result;

	if (regexp == NULL)
		return git_config_iterator_new(out, cfg);

	iter = git__calloc(1, sizeof(all_iter));
	GITERR_CHECK_ALLOC(iter);

	if ((result = regcomp(&iter->regex, regexp, REG_EXTENDED)) != 0) {
		giterr_set_regex(&iter->regex, result);
		git__free(iter);
		return -1;
	}

	iter->parent.next = all_iter_glob_next;
	iter->parent.free = all_iter_glob_free;
	iter->i = cfg->files.length;
	iter->cfg = cfg;

	*out = (git_config_iterator *) iter;

	return 0;
}

int git_config_foreach(
	const git_config *cfg, git_config_foreach_cb cb, void *payload)
{
	return git_config_foreach_match(cfg, NULL, cb, payload);
}

int git_config_backend_foreach_match(
	git_config_backend *backend,
	const char *regexp,
	git_config_foreach_cb cb,
	void *payload)
{
	git_config_entry *entry;
	git_config_iterator* iter;
	regex_t regex;
	int error = 0;

	if (regexp != NULL) {
		if ((error = regcomp(&regex, regexp, REG_EXTENDED)) != 0) {
			giterr_set_regex(&regex, error);
			regfree(&regex);
			return -1;
		}
	}

	if ((error = backend->iterator(&iter, backend)) < 0) {
		iter = NULL;
		return -1;
	}

	while (!(iter->next(&entry, iter) < 0)) {
		/* skip non-matching keys if regexp was provided */
		if (regexp && regexec(&regex, entry->name, 0, NULL, 0) != 0)
			continue;

		/* abort iterator on non-zero return value */
		if ((error = cb(entry, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	if (regexp != NULL)
		regfree(&regex);

	iter->free(iter);

	return error;
}

int git_config_foreach_match(
	const git_config *cfg,
	const char *regexp,
	git_config_foreach_cb cb,
	void *payload)
{
	int error;
	git_config_iterator *iter;
	git_config_entry *entry;

	if ((error = git_config_iterator_glob_new(&iter, cfg, regexp)) < 0)
		return error;

	while (!(error = git_config_next(&entry, iter))) {
		if ((error = cb(entry, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	git_config_iterator_free(iter);

	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

/**************
 * Setters
 **************/

static int config_error_nofiles(const char *name)
{
	giterr_set(GITERR_CONFIG,
		"Cannot set value for '%s' when no config files exist", name);
	return GIT_ENOTFOUND;
}

int git_config_delete_entry(git_config *cfg, const char *name)
{
	git_config_backend *file;
	file_internal *internal;

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file)
		return config_error_nofiles(name);
	file = internal->file;

	return file->del(file, name);
}

int git_config_set_int64(git_config *cfg, const char *name, int64_t value)
{
	char str_value[32]; /* All numbers should fit in here */
	p_snprintf(str_value, sizeof(str_value), "%" PRId64, value);
	return git_config_set_string(cfg, name, str_value);
}

int git_config_set_int32(git_config *cfg, const char *name, int32_t value)
{
	return git_config_set_int64(cfg, name, (int64_t)value);
}

int git_config_set_bool(git_config *cfg, const char *name, int value)
{
	return git_config_set_string(cfg, name, value ? "true" : "false");
}

int git_config_set_string(git_config *cfg, const char *name, const char *value)
{
	int error;
	git_config_backend *file;
	file_internal *internal;

	if (!value) {
		giterr_set(GITERR_CONFIG, "The value to set cannot be NULL");
		return -1;
	}

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file)
		return config_error_nofiles(name);
	file = internal->file;

	error = file->set(file, name, value);

	if (!error && GIT_REFCOUNT_OWNER(cfg) != NULL)
		git_repository__cvar_cache_clear(GIT_REFCOUNT_OWNER(cfg));

	return error;
}

int git_config__update_entry(
	git_config *config,
	const char *key,
	const char *value,
	bool overwrite_existing,
	bool only_if_existing)
{
	int error = 0;
	git_config_entry *ce = NULL;

	if ((error = git_config__lookup_entry(&ce, config, key, false)) < 0)
		return error;

	if (!ce && only_if_existing) /* entry doesn't exist */
		return 0;
	if (ce && !overwrite_existing) /* entry would be overwritten */
		return 0;
	if (value && ce && ce->value && !strcmp(ce->value, value)) /* no change */
		return 0;
	if (!value && (!ce || !ce->value)) /* asked to delete absent entry */
		return 0;

	if (!value)
		error = git_config_delete_entry(config, key);
	else
		error = git_config_set_string(config, key, value);

	git_config_entry_free(ce);
	return error;
}

/***********
 * Getters
 ***********/

static int config_error_notfound(const char *name)
{
	giterr_set(GITERR_CONFIG, "Config value '%s' was not found", name);
	return GIT_ENOTFOUND;
}

enum {
	GET_ALL_ERRORS = 0,
	GET_NO_MISSING = 1,
	GET_NO_ERRORS  = 2
};

static int get_entry(
	git_config_entry **out,
	const git_config *cfg,
	const char *name,
	bool normalize_name,
	int want_errors)
{
	int res = GIT_ENOTFOUND;
	const char *key = name;
	char *normalized = NULL;
	size_t i;
	file_internal *internal;

	*out = NULL;

	if (normalize_name) {
		if ((res = git_config__normalize_name(name, &normalized)) < 0)
			goto cleanup;
		key = normalized;
	}

	res = GIT_ENOTFOUND;
	git_vector_foreach(&cfg->files, i, internal) {
		if (!internal || !internal->file)
			continue;

		res = internal->file->get(internal->file, key, out);
		if (res != GIT_ENOTFOUND)
			break;
	}

	git__free(normalized);

cleanup:
	if (res == GIT_ENOTFOUND)
		res = (want_errors > GET_ALL_ERRORS) ? 0 : config_error_notfound(name);
	else if (res && (want_errors == GET_NO_ERRORS)) {
		giterr_clear();
		res = 0;
	}

	return res;
}

int git_config_get_entry(
	git_config_entry **out, const git_config *cfg, const char *name)
{
	return get_entry(out, cfg, name, true, GET_ALL_ERRORS);
}

int git_config__lookup_entry(
	git_config_entry **out,
	const git_config *cfg,
	const char *key,
	bool no_errors)
{
	return get_entry(
		out, cfg, key, false, no_errors ? GET_NO_ERRORS : GET_NO_MISSING);
}

int git_config_get_mapped(
	int *out,
	const git_config *cfg,
	const char *name,
	const git_cvar_map *maps,
	size_t map_n)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_lookup_map_value(out, maps, map_n, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_int64(int64_t *out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_int64(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_int32(int32_t *out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_int32(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

int git_config_get_bool(int *out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int ret;

	if ((ret = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS)) < 0)
		return ret;

	ret = git_config_parse_bool(out, entry->value);
	git_config_entry_free(entry);

	return ret;
}

static int is_readonly(const git_config *cfg)
{
	size_t i;
	file_internal *internal;

	git_vector_foreach(&cfg->files, i, internal) {
		if (!internal || !internal->file)
			continue;

		if (!internal->file->readonly)
			return 0;
	}

	return 1;
}

int git_config_get_path(git_buf *out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int error;

	if ((error = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS)) < 0)
		return error;

	 error = git_config_parse_path(out, entry->value);
	 git_config_entry_free(entry);

	 return error;
}

int git_config_get_string(
	const char **out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int ret;

	if (!is_readonly(cfg)) {
		giterr_set(GITERR_CONFIG, "get_string called on a live config object");
		return -1;
	}

	ret = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS);
	*out = !ret ? (entry->value ? entry->value : "") : NULL;

	git_config_entry_free(entry);

	return ret;
}

int git_config_get_string_buf(
	git_buf *out, const git_config *cfg, const char *name)
{
	git_config_entry *entry;
	int ret;
	const char *str;

	git_buf_sanitize(out);

	ret  = get_entry(&entry, cfg, name, true, GET_ALL_ERRORS);
	str = !ret ? (entry->value ? entry->value : "") : NULL;

	if (str)
		ret = git_buf_puts(out, str);

	git_config_entry_free(entry);

	return ret;
}

char *git_config__get_string_force(
	const git_config *cfg, const char *key, const char *fallback_value)
{
	git_config_entry *entry;
	char *ret;

	get_entry(&entry, cfg, key, false, GET_NO_ERRORS);
	ret = (entry && entry->value) ? git__strdup(entry->value) : fallback_value ? git__strdup(fallback_value) : NULL;
	git_config_entry_free(entry);

	return ret;
}

int git_config__get_bool_force(
	const git_config *cfg, const char *key, int fallback_value)
{
	int val = fallback_value;
	git_config_entry *entry;

	get_entry(&entry, cfg, key, false, GET_NO_ERRORS);

	if (entry && git_config_parse_bool(&val, entry->value) < 0)
		giterr_clear();

	git_config_entry_free(entry);
	return val;
}

int git_config__get_int_force(
	const git_config *cfg, const char *key, int fallback_value)
{
	int32_t val = (int32_t)fallback_value;
	git_config_entry *entry;

	get_entry(&entry, cfg, key, false, GET_NO_ERRORS);

	if (entry && git_config_parse_int32(&val, entry->value) < 0)
		giterr_clear();

	git_config_entry_free(entry);
	return (int)val;
}

int git_config_get_multivar_foreach(
	const git_config *cfg, const char *name, const char *regexp,
	git_config_foreach_cb cb, void *payload)
{
	int err, found;
	git_config_iterator *iter;
	git_config_entry *entry;

	if ((err = git_config_multivar_iterator_new(&iter, cfg, name, regexp)) < 0)
		return err;

	found = 0;
	while ((err = iter->next(&entry, iter)) == 0) {
		found = 1;

		if ((err = cb(entry, payload)) != 0) {
			giterr_set_after_callback(err);
			break;
		}
	}

	iter->free(iter);
	if (err == GIT_ITEROVER)
		err = 0;

	if (found == 0 && err == 0)
		err = config_error_notfound(name);

	return err;
}

typedef struct {
	git_config_iterator parent;
	git_config_iterator *iter;
	char *name;
	regex_t regex;
	int have_regex;
} multivar_iter;

static int multivar_iter_next(git_config_entry **entry, git_config_iterator *_iter)
{
	multivar_iter *iter = (multivar_iter *) _iter;
	int error = 0;

	while ((error = iter->iter->next(entry, iter->iter)) == 0) {
		if (git__strcmp(iter->name, (*entry)->name))
			continue;

		if (!iter->have_regex)
			return 0;

		if (regexec(&iter->regex, (*entry)->value, 0, NULL, 0) == 0)
			return 0;
	}

	return error;
}

void multivar_iter_free(git_config_iterator *_iter)
{
	multivar_iter *iter = (multivar_iter *) _iter;

	iter->iter->free(iter->iter);

	git__free(iter->name);
	if (iter->have_regex)
		regfree(&iter->regex);
	git__free(iter);
}

int git_config_multivar_iterator_new(git_config_iterator **out, const git_config *cfg, const char *name, const char *regexp)
{
	multivar_iter *iter = NULL;
	git_config_iterator *inner = NULL;
	int error;

	if ((error = git_config_iterator_new(&inner, cfg)) < 0)
		return error;

	iter = git__calloc(1, sizeof(multivar_iter));
	GITERR_CHECK_ALLOC(iter);

	if ((error = git_config__normalize_name(name, &iter->name)) < 0)
		goto on_error;

	if (regexp != NULL) {
		error = regcomp(&iter->regex, regexp, REG_EXTENDED);
		if (error != 0) {
			giterr_set_regex(&iter->regex, error);
			error = -1;
			regfree(&iter->regex);
			goto on_error;
		}

		iter->have_regex = 1;
	}

	iter->iter = inner;
	iter->parent.free = multivar_iter_free;
	iter->parent.next = multivar_iter_next;

	*out = (git_config_iterator *) iter;

	return 0;

on_error:

	inner->free(inner);
	git__free(iter);
	return error;
}

int git_config_set_multivar(git_config *cfg, const char *name, const char *regexp, const char *value)
{
	git_config_backend *file;
	file_internal *internal;

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file)
		return config_error_nofiles(name);
	file = internal->file;

	return file->set_multivar(file, name, regexp, value);
}

int git_config_delete_multivar(git_config *cfg, const char *name, const char *regexp)
{
	git_config_backend *file;
	file_internal *internal;

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file)
		return config_error_nofiles(name);
	file = internal->file;

	return file->del_multivar(file, name, regexp);
}

int git_config_next(git_config_entry **entry, git_config_iterator *iter)
{
	return iter->next(entry, iter);
}

void git_config_iterator_free(git_config_iterator *iter)
{
	if (iter == NULL)
		return;

	iter->free(iter);
}

int git_config_find_global(git_buf *path)
{
	git_buf_sanitize(path);
	return git_sysdir_find_global_file(path, GIT_CONFIG_FILENAME_GLOBAL);
}

int git_config_find_xdg(git_buf *path)
{
	git_buf_sanitize(path);
	return git_sysdir_find_xdg_file(path, GIT_CONFIG_FILENAME_XDG);
}

int git_config_find_system(git_buf *path)
{
	git_buf_sanitize(path);
	return git_sysdir_find_system_file(path, GIT_CONFIG_FILENAME_SYSTEM);
}

int git_config_find_programdata(git_buf *path)
{
	git_buf_sanitize(path);
	return git_sysdir_find_programdata_file(path, GIT_CONFIG_FILENAME_PROGRAMDATA);
}

int git_config__global_location(git_buf *buf)
{
	const git_buf *paths;
	const char *sep, *start;

	if (git_sysdir_get(&paths, GIT_SYSDIR_GLOBAL) < 0)
		return -1;

	/* no paths, so give up */
	if (!paths || !git_buf_len(paths))
		return -1;

	/* find unescaped separator or end of string */
	for (sep = start = git_buf_cstr(paths); *sep; ++sep) {
		if (*sep == GIT_PATH_LIST_SEPARATOR &&
			(sep <= start || sep[-1] != '\\'))
			break;
	}

	if (git_buf_set(buf, start, (size_t)(sep - start)) < 0)
		return -1;

	return git_buf_joinpath(buf, buf->ptr, GIT_CONFIG_FILENAME_GLOBAL);
}

int git_config_open_default(git_config **out)
{
	int error;
	git_config *cfg = NULL;
	git_buf buf = GIT_BUF_INIT;

	if ((error = git_config_new(&cfg)) < 0)
		return error;

	if (!git_config_find_global(&buf) || !git_config__global_location(&buf)) {
		error = git_config_add_file_ondisk(cfg, buf.ptr,
			GIT_CONFIG_LEVEL_GLOBAL, 0);
	}

	if (!error && !git_config_find_xdg(&buf))
		error = git_config_add_file_ondisk(cfg, buf.ptr,
			GIT_CONFIG_LEVEL_XDG, 0);

	if (!error && !git_config_find_system(&buf))
		error = git_config_add_file_ondisk(cfg, buf.ptr,
			GIT_CONFIG_LEVEL_SYSTEM, 0);

	if (!error && !git_config_find_programdata(&buf))
		error = git_config_add_file_ondisk(cfg, buf.ptr,
			GIT_CONFIG_LEVEL_PROGRAMDATA, 0);

	git_buf_free(&buf);

	if (error) {
		git_config_free(cfg);
		cfg = NULL;
	}

	*out = cfg;

	return error;
}

int git_config_lock(git_transaction **out, git_config *cfg)
{
	int error;
	git_config_backend *file;
	file_internal *internal;

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file) {
		giterr_set(GITERR_CONFIG, "cannot lock; the config has no backends/files");
		return -1;
	}
	file = internal->file;

	if ((error = file->lock(file)) < 0)
		return error;

	return git_transaction_config_new(out, cfg);
}

int git_config_unlock(git_config *cfg, int commit)
{
	git_config_backend *file;
	file_internal *internal;

	internal = git_vector_get(&cfg->files, 0);
	if (!internal || !internal->file) {
		giterr_set(GITERR_CONFIG, "cannot lock; the config has no backends/files");
		return -1;
	}

	file = internal->file;

	return file->unlock(file, commit);
}

/***********
 * Parsers
 ***********/

int git_config_lookup_map_value(
	int *out,
	const git_cvar_map *maps,
	size_t map_n,
	const char *value)
{
	size_t i;

	if (!value)
		goto fail_parse;

	for (i = 0; i < map_n; ++i) {
		const git_cvar_map *m = maps + i;

		switch (m->cvar_type) {
		case GIT_CVAR_FALSE:
		case GIT_CVAR_TRUE: {
			int bool_val;

			if (git__parse_bool(&bool_val, value) == 0 &&
				bool_val == (int)m->cvar_type) {
				*out = m->map_value;
				return 0;
			}
			break;
		}

		case GIT_CVAR_INT32:
			if (git_config_parse_int32(out, value) == 0)
				return 0;
			break;

		case GIT_CVAR_STRING:
			if (strcasecmp(value, m->str_match) == 0) {
				*out = m->map_value;
				return 0;
			}
			break;
		}
	}

fail_parse:
	giterr_set(GITERR_CONFIG, "Failed to map '%s'", value);
	return -1;
}

int git_config_lookup_map_enum(git_cvar_t *type_out, const char **str_out,
			       const git_cvar_map *maps, size_t map_n, int enum_val)
{
	size_t i;

	for (i = 0; i < map_n; i++) {
		const git_cvar_map *m = &maps[i];

		if (m->map_value != enum_val)
			continue;

		*type_out = m->cvar_type;
		*str_out = m->str_match;
		return 0;
	}

	giterr_set(GITERR_CONFIG, "invalid enum value");
	return GIT_ENOTFOUND;
}

int git_config_parse_bool(int *out, const char *value)
{
	if (git__parse_bool(out, value) == 0)
		return 0;

	if (git_config_parse_int32(out, value) == 0) {
		*out = !!(*out);
		return 0;
	}

	giterr_set(GITERR_CONFIG, "Failed to parse '%s' as a boolean value", value);
	return -1;
}

int git_config_parse_int64(int64_t *out, const char *value)
{
	const char *num_end;
	int64_t num;

	if (!value || git__strtol64(&num, value, &num_end, 0) < 0)
		goto fail_parse;

	switch (*num_end) {
	case 'g':
	case 'G':
		num *= 1024;
		/* fallthrough */

	case 'm':
	case 'M':
		num *= 1024;
		/* fallthrough */

	case 'k':
	case 'K':
		num *= 1024;

		/* check that that there are no more characters after the
		 * given modifier suffix */
		if (num_end[1] != '\0')
			return -1;

		/* fallthrough */

	case '\0':
		*out = num;
		return 0;

	default:
		goto fail_parse;
	}

fail_parse:
	giterr_set(GITERR_CONFIG, "Failed to parse '%s' as an integer", value ? value : "(null)");
	return -1;
}

int git_config_parse_int32(int32_t *out, const char *value)
{
	int64_t tmp;
	int32_t truncate;

	if (git_config_parse_int64(&tmp, value) < 0)
		goto fail_parse;

	truncate = tmp & 0xFFFFFFFF;
	if (truncate != tmp)
		goto fail_parse;

	*out = truncate;
	return 0;

fail_parse:
	giterr_set(GITERR_CONFIG, "Failed to parse '%s' as a 32-bit integer", value ? value : "(null)");
	return -1;
}

int git_config_parse_path(git_buf *out, const char *value)
{
	int error = 0;
	const git_buf *home;

	assert(out && value);

	git_buf_sanitize(out);

	if (value[0] == '~') {
		if (value[1] != '\0' && value[1] != '/') {
			giterr_set(GITERR_CONFIG, "retrieving a homedir by name is not supported");
			return -1;
		}

		if ((error = git_sysdir_get(&home, GIT_SYSDIR_GLOBAL)) < 0)
			return error;

		git_buf_sets(out, home->ptr);
		git_buf_puts(out, value + 1);

		if (git_buf_oom(out))
			return -1;

		return 0;
	}

	return git_buf_sets(out, value);
}

/* Take something the user gave us and make it nice for our hash function */
int git_config__normalize_name(const char *in, char **out)
{
	char *name, *fdot, *ldot;

	assert(in && out);

	name = git__strdup(in);
	GITERR_CHECK_ALLOC(name);

	fdot = strchr(name, '.');
	ldot = strrchr(name, '.');

	if (fdot == NULL || fdot == name || ldot == NULL || !ldot[1])
		goto invalid;

	/* Validate and downcase up to first dot and after last dot */
	if (git_config_file_normalize_section(name, fdot) < 0 ||
		git_config_file_normalize_section(ldot + 1, NULL) < 0)
		goto invalid;

	/* If there is a middle range, make sure it doesn't have newlines */
	while (fdot < ldot)
		if (*fdot++ == '\n')
			goto invalid;

	*out = name;
	return 0;

invalid:
	git__free(name);
	giterr_set(GITERR_CONFIG, "Invalid config item name '%s'", in);
	return GIT_EINVALIDSPEC;
}

struct rename_data {
	git_config *config;
	git_buf *name;
	size_t old_len;
};

static int rename_config_entries_cb(
	const git_config_entry *entry,
	void *payload)
{
	int error = 0;
	struct rename_data *data = (struct rename_data *)payload;
	size_t base_len = git_buf_len(data->name);

	if (base_len > 0 &&
		!(error = git_buf_puts(data->name, entry->name + data->old_len)))
	{
		error = git_config_set_string(
			data->config, git_buf_cstr(data->name), entry->value);

		git_buf_truncate(data->name, base_len);
	}

	if (!error)
		error = git_config_delete_entry(data->config, entry->name);

	return error;
}

int git_config_rename_section(
	git_repository *repo,
	const char *old_section_name,
	const char *new_section_name)
{
	git_config *config;
	git_buf pattern = GIT_BUF_INIT, replace = GIT_BUF_INIT;
	int error = 0;
	struct rename_data data;

	git_buf_text_puts_escape_regex(&pattern, old_section_name);

	if ((error = git_buf_puts(&pattern, "\\..+")) < 0)
		goto cleanup;

	if ((error = git_repository_config__weakptr(&config, repo)) < 0)
		goto cleanup;

	data.config  = config;
	data.name    = &replace;
	data.old_len = strlen(old_section_name) + 1;

	if ((error = git_buf_join(&replace, '.', new_section_name, "")) < 0)
		goto cleanup;

	if (new_section_name != NULL &&
		(error = git_config_file_normalize_section(
			replace.ptr, strchr(replace.ptr, '.'))) < 0)
	{
		giterr_set(
			GITERR_CONFIG, "Invalid config section '%s'", new_section_name);
		goto cleanup;
	}

	error = git_config_foreach_match(
		config, git_buf_cstr(&pattern), rename_config_entries_cb, &data);

cleanup:
	git_buf_free(&pattern);
	git_buf_free(&replace);

	return error;
}

int git_config_init_backend(git_config_backend *backend, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		backend, version, git_config_backend, GIT_CONFIG_BACKEND_INIT);
	return 0;
}
