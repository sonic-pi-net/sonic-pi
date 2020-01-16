#ifndef INCLUDE_examples_args_h__
#define INCLUDE_examples_args_h__

/**
 * Argument-processing helper structure
 */
struct args_info {
	int    argc;
	char **argv;
	int    pos;
	int    opts_done : 1; /**< Did we see a -- separator */
};
#define ARGS_INFO_INIT { argc, argv, 0, 0 }
#define ARGS_CURRENT(args) args->argv[args->pos]

/**
 * Check if a string has the given prefix.  Returns 0 if not prefixed
 * or the length of the prefix if it is.
 */
extern size_t is_prefixed(const char *str, const char *pfx);

/**
 * Match an integer string, returning 1 if matched, 0 if not.
 */
extern int is_integer(int *out, const char *str, int allow_negative);

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
 * Check current `args` entry against a "bool" `opt` (ie. --[no-]progress).
 * If `opt` matches positively, out will be set to 1, or if `opt` matches
 * negatively, out will be set to 0, and in both cases 1 will be returned.
 * If neither the positive or the negative form of opt matched, out will be -1,
 * and 0 will be returned.
 */
extern int match_bool_arg(int *out, struct args_info *args, const char *opt);

/**
 * Check if we're processing past the single -- separator
 */
extern int match_arg_separator(struct args_info *args);

/**
 * Consume all remaining arguments in a git_strarray
 */
extern void strarray_from_args(git_strarray *array, struct args_info *args);

#endif
