/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "config_entries.h"

typedef struct config_entry_list {
	struct config_entry_list *next;
	struct config_entry_list *last;
	git_config_entry *entry;
} config_entry_list;

typedef struct {
	git_config_entry *entry;
	bool multivar;
} config_entry_map_head;

typedef struct config_entries_iterator {
	git_config_iterator parent;
	git_config_entries *entries;
	config_entry_list *head;
} config_entries_iterator;

struct git_config_entries {
	git_refcount rc;
	git_strmap *map;
	config_entry_list *list;
};

int git_config_entries_new(git_config_entries **out)
{
	git_config_entries *entries;
	int error;

	entries = git__calloc(1, sizeof(git_config_entries));
	GIT_ERROR_CHECK_ALLOC(entries);
	GIT_REFCOUNT_INC(entries);

	if ((error = git_strmap_new(&entries->map)) < 0)
		git__free(entries);
	else
		*out = entries;

	return error;
}

int git_config_entries_dup_entry(git_config_entries *entries, const git_config_entry *entry)
{
	git_config_entry *duplicated;
	int error;

	duplicated = git__calloc(1, sizeof(git_config_entry));
	GIT_ERROR_CHECK_ALLOC(duplicated);

	duplicated->name = git__strdup(entry->name);
	GIT_ERROR_CHECK_ALLOC(duplicated->name);

	if (entry->value) {
		duplicated->value = git__strdup(entry->value);
		GIT_ERROR_CHECK_ALLOC(duplicated->value);
	}
	duplicated->level = entry->level;
	duplicated->include_depth = entry->include_depth;

	if ((error = git_config_entries_append(entries, duplicated)) < 0)
		goto out;

out:
	if (error && duplicated) {
		git__free((char *) duplicated->name);
		git__free((char *) duplicated->value);
		git__free(duplicated);
	}
	return error;
}

int git_config_entries_dup(git_config_entries **out, git_config_entries *entries)
{
	git_config_entries *result = NULL;
	config_entry_list *head;
	int error;

	if ((error = git_config_entries_new(&result)) < 0)
		goto out;

	for (head = entries->list; head; head = head->next)
		if ((git_config_entries_dup_entry(result, head->entry)) < 0)
			goto out;

	*out = result;
	result = NULL;

out:
	git_config_entries_free(result);
	return error;
}

void git_config_entries_incref(git_config_entries *entries)
{
	GIT_REFCOUNT_INC(entries);
}

static void config_entries_free(git_config_entries *entries)
{
	config_entry_list *list = NULL, *next;
	config_entry_map_head *head;

	git_strmap_foreach_value(entries->map, head,
		git__free((char *) head->entry->name); git__free(head)
	);
	git_strmap_free(entries->map);

	list = entries->list;
	while (list != NULL) {
		next = list->next;
		git__free((char *) list->entry->value);
		git__free(list->entry);
		git__free(list);
		list = next;
	}

	git__free(entries);
}

void git_config_entries_free(git_config_entries *entries)
{
	if (entries)
		GIT_REFCOUNT_DEC(entries, config_entries_free);
}

int git_config_entries_append(git_config_entries *entries, git_config_entry *entry)
{
	config_entry_list *list_head;
	config_entry_map_head *map_head;

	if ((map_head = git_strmap_get(entries->map, entry->name)) != NULL) {
		map_head->multivar = true;
		/*
		 * This is a micro-optimization for configuration files
		 * with a lot of same keys. As for multivars the entry's
		 * key will be the same for all entries, we can just free
		 * all except the first entry's name and just re-use it.
		 */
		git__free((char *) entry->name);
		entry->name = map_head->entry->name;
	} else {
		map_head = git__calloc(1, sizeof(*map_head));
		if ((git_strmap_set(entries->map, entry->name, map_head)) < 0)
			return -1;
	}
	map_head->entry = entry;

	list_head = git__calloc(1, sizeof(config_entry_list));
	GIT_ERROR_CHECK_ALLOC(list_head);
	list_head->entry = entry;

	if (entries->list)
		entries->list->last->next = list_head;
	else
		entries->list = list_head;
	entries->list->last = list_head;

	return 0;
}

int git_config_entries_get(git_config_entry **out, git_config_entries *entries, const char *key)
{
	config_entry_map_head *entry;
	if ((entry = git_strmap_get(entries->map, key)) == NULL)
		return GIT_ENOTFOUND;
	*out = entry->entry;
	return 0;
}

int git_config_entries_get_unique(git_config_entry **out, git_config_entries *entries, const char *key)
{
	config_entry_map_head *entry;

	if ((entry = git_strmap_get(entries->map, key)) == NULL)
		return GIT_ENOTFOUND;

	if (entry->multivar) {
		git_error_set(GIT_ERROR_CONFIG, "entry is not unique due to being a multivar");
		return -1;
	}

	if (entry->entry->include_depth) {
		git_error_set(GIT_ERROR_CONFIG, "entry is not unique due to being included");
		return -1;
	}

	*out = entry->entry;

	return 0;
}

static void config_iterator_free(git_config_iterator *iter)
{
	config_entries_iterator *it = (config_entries_iterator *) iter;
	git_config_entries_free(it->entries);
	git__free(it);
}

static int config_iterator_next(
	git_config_entry **entry,
	git_config_iterator *iter)
{
	config_entries_iterator *it = (config_entries_iterator *) iter;

	if (!it->head)
		return GIT_ITEROVER;

	*entry = it->head->entry;
	it->head = it->head->next;

	return 0;
}

int git_config_entries_iterator_new(git_config_iterator **out, git_config_entries *entries)
{
	config_entries_iterator *it;

	it = git__calloc(1, sizeof(config_entries_iterator));
	GIT_ERROR_CHECK_ALLOC(it);
	it->parent.next = config_iterator_next;
	it->parent.free = config_iterator_free;
	it->head = entries->list;
	it->entries = entries;

	git_config_entries_incref(entries);
	*out = &it->parent;

	return 0;
}
