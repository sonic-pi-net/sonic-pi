/*
 * Utilities library for libgit2 examples
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
#ifndef INCLUDE_examples_common_h__
#define INCLUDE_examples_common_h__

#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <git2.h>
#include <fcntl.h>

#ifdef _WIN32
# include <io.h>
# include <Windows.h>
# define open _open
# define read _read
# define close _close
# define ssize_t int
# define sleep(a) Sleep(a * 1000)
#else
# include <unistd.h>
#endif

#ifndef PRIuZ
/* Define the printf format specifier to use for size_t output */
#if defined(_MSC_VER) || defined(__MINGW32__)
#	define PRIuZ "Iu"
#else
#	define PRIuZ "zu"
#endif
#endif

#ifdef _MSC_VER
#define snprintf sprintf_s
#define strcasecmp strcmpi
#endif

#define ARRAY_SIZE(x) (sizeof(x)/sizeof(*x))
#define UNUSED(x) (void)(x)

#include "args.h"

extern int lg2_add(git_repository *repo, int argc, char **argv);
extern int lg2_blame(git_repository *repo, int argc, char **argv);
extern int lg2_cat_file(git_repository *repo, int argc, char **argv);
extern int lg2_checkout(git_repository *repo, int argc, char **argv);
extern int lg2_clone(git_repository *repo, int argc, char **argv);
extern int lg2_commit(git_repository *repo, int argc, char **argv);
extern int lg2_config(git_repository *repo, int argc, char **argv);
extern int lg2_describe(git_repository *repo, int argc, char **argv);
extern int lg2_diff(git_repository *repo, int argc, char **argv);
extern int lg2_fetch(git_repository *repo, int argc, char **argv);
extern int lg2_for_each_ref(git_repository *repo, int argc, char **argv);
extern int lg2_general(git_repository *repo, int argc, char **argv);
extern int lg2_index_pack(git_repository *repo, int argc, char **argv);
extern int lg2_init(git_repository *repo, int argc, char **argv);
extern int lg2_log(git_repository *repo, int argc, char **argv);
extern int lg2_ls_files(git_repository *repo, int argc, char **argv);
extern int lg2_ls_remote(git_repository *repo, int argc, char **argv);
extern int lg2_merge(git_repository *repo, int argc, char **argv);
extern int lg2_push(git_repository *repo, int argc, char **argv);
extern int lg2_remote(git_repository *repo, int argc, char **argv);
extern int lg2_rev_list(git_repository *repo, int argc, char **argv);
extern int lg2_rev_parse(git_repository *repo, int argc, char **argv);
extern int lg2_show_index(git_repository *repo, int argc, char **argv);
extern int lg2_stash(git_repository *repo, int argc, char **argv);
extern int lg2_status(git_repository *repo, int argc, char **argv);
extern int lg2_tag(git_repository *repo, int argc, char **argv);

/**
 * Check libgit2 error code, printing error to stderr on failure and
 * exiting the program.
 */
extern void check_lg2(int error, const char *message, const char *extra);

/**
 * Read a file into a buffer
 *
 * @param path The path to the file that shall be read
 * @return NUL-terminated buffer if the file was successfully read, NULL-pointer otherwise
 */
extern char *read_file(const char *path);

/**
 * Exit the program, printing error to stderr
 */
extern void fatal(const char *message, const char *extra);

/**
 * Basic output function for plain text diff output
 * Pass `FILE*` such as `stdout` or `stderr` as payload (or NULL == `stdout`)
 */
extern int diff_output(
	const git_diff_delta*, const git_diff_hunk*, const git_diff_line*, void*);

/**
 * Convert a treeish argument to an actual tree; this will call check_lg2
 * and exit the program if `treeish` cannot be resolved to a tree
 */
extern void treeish_to_tree(
	git_tree **out, git_repository *repo, const char *treeish);

/**
 * A realloc that exits on failure
 */
extern void *xrealloc(void *oldp, size_t newsz);

/**
 * Convert a refish to an annotated commit.
 */
extern int resolve_refish(git_annotated_commit **commit, git_repository *repo, const char *refish);

/**
 * Acquire credentials via command line
 */
extern int cred_acquire_cb(git_credential **out,
		const char *url,
		const char *username_from_url,
		unsigned int allowed_types,
		void *payload);

#endif
