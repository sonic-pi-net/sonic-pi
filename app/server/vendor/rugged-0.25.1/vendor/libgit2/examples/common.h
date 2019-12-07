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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <git2.h>

/**
 * Check libgit2 error code, printing error to stderr on failure and
 * exiting the program.
 */
extern void check_lg2(int error, const char *message, const char *extra);

/**
 * Exit the program, printing error to stderr
 */
extern void fatal(const char *message, const char *extra);

/**
 * Check if a string has the given prefix.  Returns 0 if not prefixed
 * or the length of the prefix if it is.
 */
extern size_t is_prefixed(const char *str, const char *pfx);

/**
 * Match an integer string, returning 1 if matched, 0 if not.
 */
extern int is_integer(int *out, const char *str, int allow_negative);

struct args_info {
	int    argc;
	char **argv;
	int    pos;
};
#define ARGS_INFO_INIT { argc, argv, 0 }

/**
 * Check current `args` entry against `opt` string.  If it matches
 * exactly, take the next arg as a string; if it matches as a prefix with
 * an equal sign, take the remainder as a string; if value not supplied, 
 * default value `def` will be given. otherwise return 0.
 */
extern int optional_str_arg(
	const char **out, struct args_info *args, const char *opt, const char *def);

/**
 * Check current `args` entry against `opt` string.  If it matches
 * exactly, take the next arg as a string; if it matches as a prefix with
 * an equal sign, take the remainder as a string; otherwise return 0.
 */
extern int match_str_arg(
	const char **out, struct args_info *args, const char *opt);

/**
 * Check current `args` entry against `opt` string parsing as uint16.  If
 * `opt` matches exactly, take the next arg as a uint16_t value; if `opt`
 * is a prefix (equal sign optional), take the remainder of the arg as a
 * uint16_t value; otherwise return 0.
 */
extern int match_uint16_arg(
	uint16_t *out, struct args_info *args, const char *opt);

/**
 * Check current `args` entry against `opt` string parsing as uint32.  If
 * `opt` matches exactly, take the next arg as a uint16_t value; if `opt`
 * is a prefix (equal sign optional), take the remainder of the arg as a
 * uint32_t value; otherwise return 0.
 */
extern int match_uint32_arg(
	uint32_t *out, struct args_info *args, const char *opt);

/**
 * Check current `args` entry against `opt` string parsing as int.  If
 * `opt` matches exactly, take the next arg as an int value; if it matches
 * as a prefix (equal sign optional), take the remainder of the arg as a
 * int value; otherwise return 0.
 */
extern int match_int_arg(
	int *out, struct args_info *args, const char *opt, int allow_negative);

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
