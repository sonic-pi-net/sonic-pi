/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "mwindow.h"

#include "vector.h"
#include "fileops.h"
#include "map.h"
#include "global.h"
#include "strmap.h"
#include "pack.h"

#define DEFAULT_WINDOW_SIZE \
	(sizeof(void*) >= 8 \
		? 1 * 1024 * 1024 * 1024 \
		: 32 * 1024 * 1024)

#define DEFAULT_MAPPED_LIMIT \
	((1024 * 1024) * (sizeof(void*) >= 8 ? 8192ULL : 256UL))

size_t git_mwindow__window_size = DEFAULT_WINDOW_SIZE;
size_t git_mwindow__mapped_limit = DEFAULT_MAPPED_LIMIT;

/* Whenever you want to read or modify this, grab git__mwindow_mutex */
static git_mwindow_ctl mem_ctl;

/* Global list of mwindow files, to open packs once across repos */
git_strmap *git__pack_cache = NULL;

static void git_mwindow_files_free(void)
{
	git_strmap *tmp = git__pack_cache;

	git__pack_cache = NULL;
	git_strmap_free(tmp);
}

int git_mwindow_global_init(void)
{
	assert(!git__pack_cache);

	git__on_shutdown(git_mwindow_files_free);
	return git_strmap_alloc(&git__pack_cache);
}

int git_mwindow_get_pack(struct git_pack_file **out, const char *path)
{
	int error;
	char *packname;
	git_strmap_iter pos;
	struct git_pack_file *pack;

	if ((error = git_packfile__name(&packname, path)) < 0)
		return error;

	if (git_mutex_lock(&git__mwindow_mutex) < 0) {
		giterr_set(GITERR_OS, "failed to lock mwindow mutex");
		return -1;
	}

	pos = git_strmap_lookup_index(git__pack_cache, packname);
	git__free(packname);

	if (git_strmap_valid_index(git__pack_cache, pos)) {
		pack = git_strmap_value_at(git__pack_cache, pos);
		git_atomic_inc(&pack->refcount);

		git_mutex_unlock(&git__mwindow_mutex);
		*out = pack;
		return 0;
	}

	/* If we didn't find it, we need to create it */
	if ((error = git_packfile_alloc(&pack, path)) < 0) {
		git_mutex_unlock(&git__mwindow_mutex);
		return error;
	}

	git_atomic_inc(&pack->refcount);

	git_strmap_insert(git__pack_cache, pack->pack_name, pack, &error);
	git_mutex_unlock(&git__mwindow_mutex);

	if (error < 0) {
		git_packfile_free(pack);
		return -1;
	}

	*out = pack;
	return 0;
}

void git_mwindow_put_pack(struct git_pack_file *pack)
{
	int count;
	git_strmap_iter pos;

	if (git_mutex_lock(&git__mwindow_mutex) < 0)
		return;

	/* put before get would be a corrupted state */
	assert(git__pack_cache);

	pos = git_strmap_lookup_index(git__pack_cache, pack->pack_name);
	/* if we cannot find it, the state is corrupted */
	assert(git_strmap_valid_index(git__pack_cache, pos));

	count = git_atomic_dec(&pack->refcount);
	if (count == 0) {
		git_strmap_delete_at(git__pack_cache, pos);
		git_packfile_free(pack);
	}

	git_mutex_unlock(&git__mwindow_mutex);
	return;
}

void git_mwindow_free_all(git_mwindow_file *mwf)
{
	if (git_mutex_lock(&git__mwindow_mutex)) {
		giterr_set(GITERR_THREAD, "unable to lock mwindow mutex");
		return;
	}

	git_mwindow_free_all_locked(mwf);

	git_mutex_unlock(&git__mwindow_mutex);
}

/*
 * Free all the windows in a sequence, typically because we're done
 * with the file
 */
void git_mwindow_free_all_locked(git_mwindow_file *mwf)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	size_t i;

	/*
	 * Remove these windows from the global list
	 */
	for (i = 0; i < ctl->windowfiles.length; ++i){
		if (git_vector_get(&ctl->windowfiles, i) == mwf) {
			git_vector_remove(&ctl->windowfiles, i);
			break;
		}
	}

	if (ctl->windowfiles.length == 0) {
		git_vector_free(&ctl->windowfiles);
		ctl->windowfiles.contents = NULL;
	}

	while (mwf->windows) {
		git_mwindow *w = mwf->windows;
		assert(w->inuse_cnt == 0);

		ctl->mapped -= w->window_map.len;
		ctl->open_windows--;

		git_futils_mmap_free(&w->window_map);

		mwf->windows = w->next;
		git__free(w);
	}
}

/*
 * Check if a window 'win' contains the address 'offset'
 */
int git_mwindow_contains(git_mwindow *win, git_off_t offset)
{
	git_off_t win_off = win->offset;
	return win_off <= offset
		&& offset <= (git_off_t)(win_off + win->window_map.len);
}

/*
 * Find the least-recently-used window in a file
 */
static void git_mwindow_scan_lru(
	git_mwindow_file *mwf,
	git_mwindow **lru_w,
	git_mwindow **lru_l)
{
	git_mwindow *w, *w_l;

	for (w_l = NULL, w = mwf->windows; w; w = w->next) {
		if (!w->inuse_cnt) {
			/*
			 * If the current one is more recent than the last one,
			 * store it in the output parameter. If lru_w is NULL,
			 * it's the first loop, so store it as well.
			 */
			if (!*lru_w || w->last_used < (*lru_w)->last_used) {
				*lru_w = w;
				*lru_l = w_l;
			}
		}
		w_l = w;
	}
}

/*
 * Close the least recently used window. You should check to see if
 * the file descriptors need closing from time to time. Called under
 * lock from new_window.
 */
static int git_mwindow_close_lru(git_mwindow_file *mwf)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	size_t i;
	git_mwindow *lru_w = NULL, *lru_l = NULL, **list = &mwf->windows;

	/* FIXME: Does this give us any advantage? */
	if(mwf->windows)
		git_mwindow_scan_lru(mwf, &lru_w, &lru_l);

	for (i = 0; i < ctl->windowfiles.length; ++i) {
		git_mwindow *last = lru_w;
		git_mwindow_file *cur = git_vector_get(&ctl->windowfiles, i);
		git_mwindow_scan_lru(cur, &lru_w, &lru_l);
		if (lru_w != last)
			list = &cur->windows;
	}

	if (!lru_w) {
		giterr_set(GITERR_OS, "failed to close memory window; couldn't find LRU");
		return -1;
	}

	ctl->mapped -= lru_w->window_map.len;
	git_futils_mmap_free(&lru_w->window_map);

	if (lru_l)
		lru_l->next = lru_w->next;
	else
		*list = lru_w->next;

	git__free(lru_w);
	ctl->open_windows--;

	return 0;
}

/* This gets called under lock from git_mwindow_open */
static git_mwindow *new_window(
	git_mwindow_file *mwf,
	git_file fd,
	git_off_t size,
	git_off_t offset)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	size_t walign = git_mwindow__window_size / 2;
	git_off_t len;
	git_mwindow *w;

	w = git__malloc(sizeof(*w));

	if (w == NULL)
		return NULL;

	memset(w, 0x0, sizeof(*w));
	w->offset = (offset / walign) * walign;

	len = size - w->offset;
	if (len > (git_off_t)git_mwindow__window_size)
		len = (git_off_t)git_mwindow__window_size;

	ctl->mapped += (size_t)len;

	while (git_mwindow__mapped_limit < ctl->mapped &&
			git_mwindow_close_lru(mwf) == 0) /* nop */;

	/*
	 * We treat `mapped_limit` as a soft limit. If we can't find a
	 * window to close and are above the limit, we still mmap the new
	 * window.
	 */

	if (git_futils_mmap_ro(&w->window_map, fd, w->offset, (size_t)len) < 0) {
		/*
		 * The first error might be down to memory fragmentation even if
		 * we're below our soft limits, so free up what we can and try again.
		 */

		while (git_mwindow_close_lru(mwf) == 0)
			/* nop */;

		if (git_futils_mmap_ro(&w->window_map, fd, w->offset, (size_t)len) < 0) {
			git__free(w);
			return NULL;
		}
	}

	ctl->mmap_calls++;
	ctl->open_windows++;

	if (ctl->mapped > ctl->peak_mapped)
		ctl->peak_mapped = ctl->mapped;

	if (ctl->open_windows > ctl->peak_open_windows)
		ctl->peak_open_windows = ctl->open_windows;

	return w;
}

/*
 * Open a new window, closing the least recenty used until we have
 * enough space. Don't forget to add it to your list
 */
unsigned char *git_mwindow_open(
	git_mwindow_file *mwf,
	git_mwindow **cursor,
	git_off_t offset,
	size_t extra,
	unsigned int *left)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	git_mwindow *w = *cursor;

	if (git_mutex_lock(&git__mwindow_mutex)) {
		giterr_set(GITERR_THREAD, "unable to lock mwindow mutex");
		return NULL;
	}

	if (!w || !(git_mwindow_contains(w, offset) && git_mwindow_contains(w, offset + extra))) {
		if (w) {
			w->inuse_cnt--;
		}

		for (w = mwf->windows; w; w = w->next) {
			if (git_mwindow_contains(w, offset) &&
				git_mwindow_contains(w, offset + extra))
				break;
		}

		/*
		 * If there isn't a suitable window, we need to create a new
		 * one.
		 */
		if (!w) {
			w = new_window(mwf, mwf->fd, mwf->size, offset);
			if (w == NULL) {
				git_mutex_unlock(&git__mwindow_mutex);
				return NULL;
			}
			w->next = mwf->windows;
			mwf->windows = w;
		}
	}

	/* If we changed w, store it in the cursor */
	if (w != *cursor) {
		w->last_used = ctl->used_ctr++;
		w->inuse_cnt++;
		*cursor = w;
	}

	offset -= w->offset;

	if (left)
		*left = (unsigned int)(w->window_map.len - offset);

	git_mutex_unlock(&git__mwindow_mutex);
	return (unsigned char *) w->window_map.data + offset;
}

int git_mwindow_file_register(git_mwindow_file *mwf)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	int ret;

	if (git_mutex_lock(&git__mwindow_mutex)) {
		giterr_set(GITERR_THREAD, "unable to lock mwindow mutex");
		return -1;
	}

	if (ctl->windowfiles.length == 0 &&
	    git_vector_init(&ctl->windowfiles, 8, NULL) < 0) {
		git_mutex_unlock(&git__mwindow_mutex);
		return -1;
	}

	ret = git_vector_insert(&ctl->windowfiles, mwf);
	git_mutex_unlock(&git__mwindow_mutex);

	return ret;
}

void git_mwindow_file_deregister(git_mwindow_file *mwf)
{
	git_mwindow_ctl *ctl = &mem_ctl;
	git_mwindow_file *cur;
	size_t i;

	if (git_mutex_lock(&git__mwindow_mutex))
		return;

	git_vector_foreach(&ctl->windowfiles, i, cur) {
		if (cur == mwf) {
			git_vector_remove(&ctl->windowfiles, i);
			git_mutex_unlock(&git__mwindow_mutex);
			return;
		}
	}
	git_mutex_unlock(&git__mwindow_mutex);
}

void git_mwindow_close(git_mwindow **window)
{
	git_mwindow *w = *window;
	if (w) {
		if (git_mutex_lock(&git__mwindow_mutex)) {
			giterr_set(GITERR_THREAD, "unable to lock mwindow mutex");
			return;
		}

		w->inuse_cnt--;
		git_mutex_unlock(&git__mwindow_mutex);
		*window = NULL;
	}
}
