/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "fileops.h"
#include "repository.h"
#include "config.h"
#include "git2/config.h"
#include "vector.h"
#include "filter.h"

struct map_data {
	const char *cvar_name;
	git_cvar_map *maps;
	size_t map_count;
	int default_value;
};

/*
 *	core.eol
 *		Sets the line ending type to use in the working directory for
 *	files that have the text property set. Alternatives are lf, crlf
 *	and native, which uses the platform's native line ending. The default
 *	value is native. See gitattributes(5) for more information on
 *	end-of-line conversion.
 */
static git_cvar_map _cvar_map_eol[] = {
	{GIT_CVAR_FALSE, NULL, GIT_EOL_UNSET},
	{GIT_CVAR_STRING, "lf", GIT_EOL_LF},
	{GIT_CVAR_STRING, "crlf", GIT_EOL_CRLF},
	{GIT_CVAR_STRING, "native", GIT_EOL_NATIVE}
};

/*
 *	core.autocrlf
 *		Setting this variable to "true" is almost the same as setting
 *	the text attribute to "auto" on all files except that text files are
 *	not guaranteed to be normalized: files that contain CRLF in the
 *	repository will not be touched. Use this setting if you want to have
 *	CRLF line endings in your working directory even though the repository
 *	does not have normalized line endings. This variable can be set to input,
 *	in which case no output conversion is performed.
 */
static git_cvar_map _cvar_map_autocrlf[] = {
	{GIT_CVAR_FALSE, NULL, GIT_AUTO_CRLF_FALSE},
	{GIT_CVAR_TRUE, NULL, GIT_AUTO_CRLF_TRUE},
	{GIT_CVAR_STRING, "input", GIT_AUTO_CRLF_INPUT}
};

/*
 * Generic map for integer values
 */
static git_cvar_map _cvar_map_int[] = {
	{GIT_CVAR_INT32, NULL, 0},
};

static struct map_data _cvar_maps[] = {
	{"core.autocrlf", _cvar_map_autocrlf, ARRAY_SIZE(_cvar_map_autocrlf), GIT_AUTO_CRLF_DEFAULT},
	{"core.eol", _cvar_map_eol, ARRAY_SIZE(_cvar_map_eol), GIT_EOL_DEFAULT},
	{"core.symlinks", NULL, 0, GIT_SYMLINKS_DEFAULT },
	{"core.ignorecase", NULL, 0, GIT_IGNORECASE_DEFAULT },
	{"core.filemode", NULL, 0, GIT_FILEMODE_DEFAULT },
	{"core.ignorestat", NULL, 0, GIT_IGNORESTAT_DEFAULT },
	{"core.trustctime", NULL, 0, GIT_TRUSTCTIME_DEFAULT },
	{"core.abbrev", _cvar_map_int, 1, GIT_ABBREV_DEFAULT },
	{"core.precomposeunicode", NULL, 0, GIT_PRECOMPOSE_DEFAULT },
	{"core.safecrlf", NULL, 0, GIT_SAFE_CRLF_DEFAULT},
	{"core.logallrefupdates", NULL, 0, GIT_LOGALLREFUPDATES_DEFAULT },
};

int git_config__cvar(int *out, git_config *config, git_cvar_cached cvar)
{
	int error = 0;
	struct map_data *data = &_cvar_maps[(int)cvar];
	const git_config_entry *entry;

	git_config__lookup_entry(&entry, config, data->cvar_name, false);

	if (!entry)
		*out = data->default_value;
	else if (data->maps)
		error = git_config_lookup_map_value(
			out, data->maps, data->map_count, entry->value);
	else
		error = git_config_parse_bool(out, entry->value);

	return error;
}

int git_repository__cvar(int *out, git_repository *repo, git_cvar_cached cvar)
{
	*out = repo->cvar_cache[(int)cvar];

	if (*out == GIT_CVAR_NOT_CACHED) {
		int error;
		git_config *config;

		if ((error = git_repository_config__weakptr(&config, repo)) < 0 ||
			(error = git_config__cvar(out, config, cvar)) < 0)
			return error;

		repo->cvar_cache[(int)cvar] = *out;
	}

	return 0;
}

void git_repository__cvar_cache_clear(git_repository *repo)
{
	int i;

	for (i = 0; i < GIT_CVAR_CACHE_MAX; ++i)
		repo->cvar_cache[i] = GIT_CVAR_NOT_CACHED;
}

