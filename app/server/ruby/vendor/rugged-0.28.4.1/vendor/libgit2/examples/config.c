/*
 * libgit2 "config" example - shows how to use the config API
 *
 * Written by the libgit2 contributors
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication along
 * with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#include "common.h"

static int config_get(git_config *cfg, const char *key)
{
	git_config_entry *entry;
	int error;

	if ((error = git_config_get_entry(&entry, cfg, key)) < 0) {
		if (error != GIT_ENOTFOUND)
			printf("Unable to get configuration: %s\n", git_error_last()->message);
		return 1;
	}

	puts(entry->value);
	return 0;
}

static int config_set(git_config *cfg, const char *key, const char *value)
{
	if (git_config_set_string(cfg, key, value) < 0) {
		printf("Unable to set configuration: %s\n", git_error_last()->message);
		return 1;
	}
	return 0;
}

int lg2_config(git_repository *repo, int argc, char **argv)
{
	git_config *cfg;
	int error;

	if ((error = git_repository_config(&cfg, repo)) < 0) {
		printf("Unable to obtain repository config: %s\n", git_error_last()->message);
		goto out;
	}

	if (argc == 2) {
		error = config_get(cfg, argv[1]);
	} else if (argc == 3) {
		error = config_set(cfg, argv[1], argv[2]);
	} else {
		printf("USAGE: %s config <KEY> [<VALUE>]\n", argv[0]);
		error = 1;
	}

out:
	return error;
}
