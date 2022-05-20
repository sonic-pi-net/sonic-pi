/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/sys/config.h"
#include "config.h"

typedef struct git_config_entries git_config_entries;

int git_config_entries_new(git_config_entries **out);
int git_config_entries_dup(git_config_entries **out, git_config_entries *entries);
int git_config_entries_dup_entry(git_config_entries *entries, const git_config_entry *entry);
void git_config_entries_incref(git_config_entries *entries);
void git_config_entries_free(git_config_entries *entries);
/* Add or append the new config option */
int git_config_entries_append(git_config_entries *entries, git_config_entry *entry);
int git_config_entries_get(git_config_entry **out, git_config_entries *entries, const char *key);
int git_config_entries_get_unique(git_config_entry **out, git_config_entries *entries, const char *key);
int git_config_entries_iterator_new(git_config_iterator **out, git_config_entries *entries);
