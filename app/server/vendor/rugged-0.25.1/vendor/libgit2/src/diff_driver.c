/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"

#include "git2/attr.h"

#include "diff.h"
#include "diff_driver.h"
#include "strmap.h"
#include "map.h"
#include "buf_text.h"
#include "config.h"
#include "repository.h"

GIT__USE_STRMAP

typedef enum {
	DIFF_DRIVER_AUTO = 0,
	DIFF_DRIVER_BINARY = 1,
	DIFF_DRIVER_TEXT = 2,
	DIFF_DRIVER_PATTERNLIST = 3,
} git_diff_driver_t;

typedef struct {
	regex_t re;
	int flags;
} git_diff_driver_pattern;

enum {
	REG_NEGATE = (1 << 15) /* get out of the way of existing flags */
};

/* data for finding function context for a given file type */
struct git_diff_driver {
	git_diff_driver_t type;
	uint32_t binary_flags;
	uint32_t other_flags;
	git_array_t(git_diff_driver_pattern) fn_patterns;
	regex_t  word_pattern;
	char name[GIT_FLEX_ARRAY];
};

#include "userdiff.h"

struct git_diff_driver_registry {
	git_strmap *drivers;
};

#define FORCE_DIFFABLE (GIT_DIFF_FORCE_TEXT | GIT_DIFF_FORCE_BINARY)

static git_diff_driver global_drivers[3] = {
	{ DIFF_DRIVER_AUTO,   0, 0, },
	{ DIFF_DRIVER_BINARY, GIT_DIFF_FORCE_BINARY, 0 },
	{ DIFF_DRIVER_TEXT,   GIT_DIFF_FORCE_TEXT, 0 },
};

git_diff_driver_registry *git_diff_driver_registry_new(void)
{
	git_diff_driver_registry *reg =
		git__calloc(1, sizeof(git_diff_driver_registry));
	if (!reg)
		return NULL;

	if (git_strmap_alloc(&reg->drivers) < 0) {
		git_diff_driver_registry_free(reg);
		return NULL;
	}

	return reg;
}

void git_diff_driver_registry_free(git_diff_driver_registry *reg)
{
	git_diff_driver *drv;

	if (!reg)
		return;

	git_strmap_foreach_value(reg->drivers, drv, git_diff_driver_free(drv));
	git_strmap_free(reg->drivers);
	git__free(reg);
}

static int diff_driver_add_patterns(
	git_diff_driver *drv, const char *regex_str, int regex_flags)
{
	int error = 0;
	const char *scan, *end;
	git_diff_driver_pattern *pat = NULL;
	git_buf buf = GIT_BUF_INIT;

	for (scan = regex_str; scan; scan = end) {
		/* get pattern to fill in */
		if ((pat = git_array_alloc(drv->fn_patterns)) == NULL) {
			return -1;
		}

		pat->flags = regex_flags;
		if (*scan == '!') {
			pat->flags |= REG_NEGATE;
			++scan;
		}

		if ((end = strchr(scan, '\n')) != NULL) {
			error = git_buf_set(&buf, scan, end - scan);
			end++;
		} else {
			error = git_buf_sets(&buf, scan);
		}
		if (error < 0)
			break;

		if ((error = p_regcomp(&pat->re, buf.ptr, regex_flags)) != 0) {
			/*
			 * TODO: issue a warning
			 */
		}
	}

	if (error && pat != NULL)
		(void)git_array_pop(drv->fn_patterns); /* release last item */
	git_buf_free(&buf);

	/* We want to ignore bad patterns, so return success regardless */
	return 0;
}

static int diff_driver_xfuncname(const git_config_entry *entry, void *payload)
{
	return diff_driver_add_patterns(payload, entry->value, REG_EXTENDED);
}

static int diff_driver_funcname(const git_config_entry *entry, void *payload)
{
	return diff_driver_add_patterns(payload, entry->value, 0);
}

static git_diff_driver_registry *git_repository_driver_registry(
	git_repository *repo)
{
	if (!repo->diff_drivers) {
		git_diff_driver_registry *reg = git_diff_driver_registry_new();
		reg = git__compare_and_swap(&repo->diff_drivers, NULL, reg);

		if (reg != NULL) /* if we race, free losing allocation */
			git_diff_driver_registry_free(reg);
	}

	if (!repo->diff_drivers)
		giterr_set(GITERR_REPOSITORY, "Unable to create diff driver registry");

	return repo->diff_drivers;
}

static int diff_driver_alloc(
	git_diff_driver **out, size_t *namelen_out, const char *name)
{
	git_diff_driver *driver;
	size_t driverlen = sizeof(git_diff_driver),
		namelen = strlen(name),
		alloclen;

	GITERR_CHECK_ALLOC_ADD(&alloclen, driverlen, namelen);
	GITERR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);

	driver = git__calloc(1, alloclen);
	GITERR_CHECK_ALLOC(driver);

	memcpy(driver->name, name, namelen);

	*out = driver;

	if (namelen_out)
		*namelen_out = namelen;

	return 0;
}

static int git_diff_driver_builtin(
	git_diff_driver **out,
	git_diff_driver_registry *reg,
	const char *driver_name)
{
	int error = 0;
	git_diff_driver_definition *ddef = NULL;
	git_diff_driver *drv = NULL;
	size_t idx;

	for (idx = 0; idx < ARRAY_SIZE(builtin_defs); ++idx) {
		if (!strcasecmp(driver_name, builtin_defs[idx].name)) {
			ddef = &builtin_defs[idx];
			break;
		}
	}
	if (!ddef)
		goto done;

	if ((error = diff_driver_alloc(&drv, NULL, ddef->name)) < 0)
		goto done;

	drv->type = DIFF_DRIVER_PATTERNLIST;

	if (ddef->fns &&
		(error = diff_driver_add_patterns(
			drv, ddef->fns, ddef->flags | REG_EXTENDED)) < 0)
		goto done;

	if (ddef->words &&
		(error = p_regcomp(
			&drv->word_pattern, ddef->words, ddef->flags | REG_EXTENDED)))
	{
		error = giterr_set_regex(&drv->word_pattern, error);
		goto done;
	}

	git_strmap_insert(reg->drivers, drv->name, drv, error);
	if (error > 0)
		error = 0;

done:
	if (error && drv)
		git_diff_driver_free(drv);
	else
		*out = drv;

	return error;
}

static int git_diff_driver_load(
	git_diff_driver **out, git_repository *repo, const char *driver_name)
{
	int error = 0;
	git_diff_driver_registry *reg;
	git_diff_driver *drv = NULL;
	size_t namelen;
	khiter_t pos;
	git_config *cfg = NULL;
	git_buf name = GIT_BUF_INIT;
	git_config_entry *ce = NULL;
	bool found_driver = false;

	if ((reg = git_repository_driver_registry(repo)) == NULL)
		return -1;

	pos = git_strmap_lookup_index(reg->drivers, driver_name);
	if (git_strmap_valid_index(reg->drivers, pos)) {
		*out = git_strmap_value_at(reg->drivers, pos);
		return 0;
	}

	if ((error = diff_driver_alloc(&drv, &namelen, driver_name)) < 0)
		goto done;

	drv->type = DIFF_DRIVER_AUTO;

	/* if you can't read config for repo, just use default driver */
	if (git_repository_config_snapshot(&cfg, repo) < 0) {
		giterr_clear();
		goto done;
	}

	if ((error = git_buf_printf(&name, "diff.%s.binary", driver_name)) < 0)
		goto done;

	switch (git_config__get_bool_force(cfg, name.ptr, -1)) {
	case true:
		/* if diff.<driver>.binary is true, just return the binary driver */
		*out = &global_drivers[DIFF_DRIVER_BINARY];
		goto done;
	case false:
		/* if diff.<driver>.binary is false, force binary checks off */
		/* but still may have custom function context patterns, etc. */
		drv->binary_flags = GIT_DIFF_FORCE_TEXT;
		found_driver = true;
		break;
	default:
		/* diff.<driver>.binary unspecified or "auto", so just continue */
		break;
	}

	/* TODO: warn if diff.<name>.command or diff.<name>.textconv are set */

	git_buf_truncate(&name, namelen + strlen("diff.."));
	git_buf_put(&name, "xfuncname", strlen("xfuncname"));
	if ((error = git_config_get_multivar_foreach(
			cfg, name.ptr, NULL, diff_driver_xfuncname, drv)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto done;
		giterr_clear(); /* no diff.<driver>.xfuncname, so just continue */
	}

	git_buf_truncate(&name, namelen + strlen("diff.."));
	git_buf_put(&name, "funcname", strlen("funcname"));
	if ((error = git_config_get_multivar_foreach(
			cfg, name.ptr, NULL, diff_driver_funcname, drv)) < 0) {
		if (error != GIT_ENOTFOUND)
			goto done;
		giterr_clear(); /* no diff.<driver>.funcname, so just continue */
	}

	/* if we found any patterns, set driver type to use correct callback */
	if (git_array_size(drv->fn_patterns) > 0) {
		drv->type = DIFF_DRIVER_PATTERNLIST;
		found_driver = true;
	}

	git_buf_truncate(&name, namelen + strlen("diff.."));
	git_buf_put(&name, "wordregex", strlen("wordregex"));
	if ((error = git_config__lookup_entry(&ce, cfg, name.ptr, false)) < 0)
		goto done;
	if (!ce || !ce->value)
		/* no diff.<driver>.wordregex, so just continue */;
	else if (!(error = p_regcomp(&drv->word_pattern, ce->value, REG_EXTENDED)))
		found_driver = true;
	else {
		/* TODO: warn about bad regex instead of failure */
		error = giterr_set_regex(&drv->word_pattern, error);
		goto done;
	}

	/* TODO: look up diff.<driver>.algorithm to turn on minimal / patience
	 * diff in drv->other_flags
	 */

	/* if no driver config found at all, fall back on AUTO driver */
	if (!found_driver)
		goto done;

	/* store driver in registry */
	git_strmap_insert(reg->drivers, drv->name, drv, error);
	if (error < 0)
		goto done;
	error = 0;

	*out = drv;

done:
	git_config_entry_free(ce);
	git_buf_free(&name);
	git_config_free(cfg);

	if (!*out) {
		int error2 = git_diff_driver_builtin(out, reg, driver_name);
		if (!error)
			error = error2;
	}

	if (drv && drv != *out)
		git_diff_driver_free(drv);

	return error;
}

int git_diff_driver_lookup(
	git_diff_driver **out, git_repository *repo, const char *path)
{
	int error = 0;
	const char *value;

	assert(out);
	*out = NULL;

	if (!repo || !path || !strlen(path))
		/* just use the auto value */;
	else if ((error = git_attr_get(&value, repo, 0, path, "diff")) < 0)
		/* return error below */;
	else if (GIT_ATTR_UNSPECIFIED(value))
		/* just use the auto value */;
	else if (GIT_ATTR_FALSE(value))
		*out = &global_drivers[DIFF_DRIVER_BINARY];
	else if (GIT_ATTR_TRUE(value))
		*out = &global_drivers[DIFF_DRIVER_TEXT];

	/* otherwise look for driver information in config and build driver */
	else if ((error = git_diff_driver_load(out, repo, value)) < 0) {
		if (error == GIT_ENOTFOUND) {
			error = 0;
			giterr_clear();
		}
	}

	if (!*out)
		*out = &global_drivers[DIFF_DRIVER_AUTO];

	return error;
}

void git_diff_driver_free(git_diff_driver *driver)
{
	size_t i;

	if (!driver)
		return;

	for (i = 0; i < git_array_size(driver->fn_patterns); ++i)
		regfree(& git_array_get(driver->fn_patterns, i)->re);
	git_array_clear(driver->fn_patterns);

	regfree(&driver->word_pattern);

	git__free(driver);
}

void git_diff_driver_update_options(
	uint32_t *option_flags, git_diff_driver *driver)
{
	if ((*option_flags & FORCE_DIFFABLE) == 0)
		*option_flags |= driver->binary_flags;

	*option_flags |= driver->other_flags;
}

int git_diff_driver_content_is_binary(
	git_diff_driver *driver, const char *content, size_t content_len)
{
	git_buf search = GIT_BUF_INIT;

	GIT_UNUSED(driver);

	git_buf_attach_notowned(&search, content,
		min(content_len, GIT_FILTER_BYTES_TO_CHECK_NUL));

	/* TODO: provide encoding / binary detection callbacks that can
	 * be UTF-8 aware, etc.  For now, instead of trying to be smart,
	 * let's just use the simple NUL-byte detection that core git uses.
	 */

	/* previously was: if (git_buf_text_is_binary(&search)) */
	if (git_buf_text_contains_nul(&search))
		return 1;

	return 0;
}

static int diff_context_line__simple(
	git_diff_driver *driver, git_buf *line)
{
	char firstch = line->ptr[0];
	GIT_UNUSED(driver);
	return (git__isalpha(firstch) || firstch == '_' || firstch == '$');
}

static int diff_context_line__pattern_match(
	git_diff_driver *driver, git_buf *line)
{
	size_t i, maxi = git_array_size(driver->fn_patterns);
	regmatch_t pmatch[2];

	for (i = 0; i < maxi; ++i) {
		git_diff_driver_pattern *pat = git_array_get(driver->fn_patterns, i);

		if (!regexec(&pat->re, line->ptr, 2, pmatch, 0)) {
			if (pat->flags & REG_NEGATE)
				return false;

			/* use pmatch data to trim line data */
			i = (pmatch[1].rm_so >= 0) ? 1 : 0;
			git_buf_consume(line, git_buf_cstr(line) + pmatch[i].rm_so);
			git_buf_truncate(line, pmatch[i].rm_eo - pmatch[i].rm_so);
			git_buf_rtrim(line);

			return true;
		}
	}

	return false;
}

static long diff_context_find(
	const char *line,
	long line_len,
	char *out,
	long out_size,
	void *payload)
{
	git_diff_find_context_payload *ctxt = payload;

	if (git_buf_set(&ctxt->line, line, (size_t)line_len) < 0)
		return -1;
	git_buf_rtrim(&ctxt->line);

	if (!ctxt->line.size)
		return -1;

	if (!ctxt->match_line || !ctxt->match_line(ctxt->driver, &ctxt->line))
		return -1;

	if (out_size > (long)ctxt->line.size)
		out_size = (long)ctxt->line.size;
	memcpy(out, ctxt->line.ptr, (size_t)out_size);

	return out_size;
}

void git_diff_find_context_init(
	git_diff_find_context_fn *findfn_out,
	git_diff_find_context_payload *payload_out,
	git_diff_driver *driver)
{
	*findfn_out = driver ? diff_context_find : NULL;

	memset(payload_out, 0, sizeof(*payload_out));
	if (driver) {
		payload_out->driver = driver;
		payload_out->match_line = (driver->type == DIFF_DRIVER_PATTERNLIST) ?
			diff_context_line__pattern_match : diff_context_line__simple;
		git_buf_init(&payload_out->line, 0);
	}
}

void git_diff_find_context_clear(git_diff_find_context_payload *payload)
{
	if (payload) {
		git_buf_free(&payload->line);
		payload->driver = NULL;
	}
}
