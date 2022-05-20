#include "common.h"
#include "args.h"

size_t is_prefixed(const char *str, const char *pfx)
{
	size_t len = strlen(pfx);
	return strncmp(str, pfx, len) ? 0 : len;
}

int optional_str_arg(
	const char **out, struct args_info *args, const char *opt, const char *def)
{
	const char *found = args->argv[args->pos];
	size_t len = is_prefixed(found, opt);

	if (!len)
		return 0;

	if (!found[len]) {
		if (args->pos + 1 == args->argc) {
			*out = def;
			return 1;
		}
		args->pos += 1;
		*out = args->argv[args->pos];
		return 1;
	}

	if (found[len] == '=') {
		*out = found + len + 1;
		return 1;
	}

	return 0;
}

int match_str_arg(
	const char **out, struct args_info *args, const char *opt)
{
	const char *found = args->argv[args->pos];
	size_t len = is_prefixed(found, opt);

	if (!len)
		return 0;

	if (!found[len]) {
		if (args->pos + 1 == args->argc)
			fatal("expected value following argument", opt);
		args->pos += 1;
		*out = args->argv[args->pos];
		return 1;
	}

	if (found[len] == '=') {
		*out = found + len + 1;
		return 1;
	}

	return 0;
}

static const char *match_numeric_arg(struct args_info *args, const char *opt)
{
	const char *found = args->argv[args->pos];
	size_t len = is_prefixed(found, opt);

	if (!len)
		return NULL;

	if (!found[len]) {
		if (args->pos + 1 == args->argc)
			fatal("expected numeric value following argument", opt);
		args->pos += 1;
		found = args->argv[args->pos];
	} else {
		found = found + len;
		if (*found == '=')
			found++;
	}

	return found;
}

int match_uint16_arg(
	uint16_t *out, struct args_info *args, const char *opt)
{
	const char *found = match_numeric_arg(args, opt);
	uint16_t val;
	char *endptr = NULL;

	if (!found)
		return 0;

	val = (uint16_t)strtoul(found, &endptr, 0);
	if (!endptr || *endptr != '\0')
		fatal("expected number after argument", opt);

	if (out)
		*out = val;
	return 1;
}

int match_uint32_arg(
	uint32_t *out, struct args_info *args, const char *opt)
{
	const char *found = match_numeric_arg(args, opt);
	uint16_t val;
	char *endptr = NULL;

	if (!found)
		return 0;

	val = (uint32_t)strtoul(found, &endptr, 0);
	if (!endptr || *endptr != '\0')
		fatal("expected number after argument", opt);

	if (out)
		*out = val;
	return 1;
}

static int match_int_internal(
	int *out, const char *str, int allow_negative, const char *opt)
{
	char *endptr = NULL;
	int	  val = (int)strtol(str, &endptr, 10);

	if (!endptr || *endptr != '\0')
		fatal("expected number", opt);
	else if (val < 0 && !allow_negative)
		fatal("negative values are not allowed", opt);

	if (out)
		*out = val;

	return 1;
}

int match_bool_arg(int *out, struct args_info *args, const char *opt)
{
	const char *found = args->argv[args->pos];

	if (!strcmp(found, opt)) {
		*out = 1;
		return 1;
	}

	if (!strncmp(found, "--no-", strlen("--no-")) &&
	    !strcmp(found + strlen("--no-"), opt + 2)) {
		*out = 0;
		return 1;
	}

	*out = -1;
	return 0;
}

int is_integer(int *out, const char *str, int allow_negative)
{
	return match_int_internal(out, str, allow_negative, NULL);
}

int match_int_arg(
	int *out, struct args_info *args, const char *opt, int allow_negative)
{
	const char *found = match_numeric_arg(args, opt);
	if (!found)
		return 0;
	return match_int_internal(out, found, allow_negative, opt);
}

int match_arg_separator(struct args_info *args)
{
	if (args->opts_done)
		return 1;

	if (strcmp(args->argv[args->pos], "--") != 0)
		return 0;

	args->opts_done = 1;
	args->pos++;
	return 1;
}

void strarray_from_args(git_strarray *array, struct args_info *args)
{
	size_t i;

	array->count = args->argc - args->pos;
	array->strings = calloc(array->count, sizeof(char *));
	assert(array->strings != NULL);

	for (i = 0; args->pos < args->argc; ++args->pos) {
		array->strings[i++] = args->argv[args->pos];
	}
	args->pos = args->argc;
}
