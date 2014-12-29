#include "common.h"
#include "repository.h"
#include "filebuf.h"
#include "attr_file.h"
#include "attrcache.h"
#include "git2/blob.h"
#include "git2/tree.h"
#include "index.h"
#include <ctype.h>

static void attr_file_free(git_attr_file *file)
{
	bool unlock = !git_mutex_lock(&file->lock);
	git_attr_file__clear_rules(file, false);
	git_pool_clear(&file->pool);
	if (unlock)
		git_mutex_unlock(&file->lock);
	git_mutex_free(&file->lock);

	git__memzero(file, sizeof(*file));
	git__free(file);
}

int git_attr_file__new(
	git_attr_file **out,
	git_attr_file_entry *entry,
	git_attr_file_source source)
{
	git_attr_file *attrs = git__calloc(1, sizeof(git_attr_file));
	GITERR_CHECK_ALLOC(attrs);

	if (git_mutex_init(&attrs->lock) < 0) {
		giterr_set(GITERR_OS, "Failed to initialize lock");
		git__free(attrs);
		return -1;
	}

	if (git_pool_init(&attrs->pool, 1, 0) < 0) {
		attr_file_free(attrs);
		return -1;
	}

	GIT_REFCOUNT_INC(attrs);
	attrs->entry  = entry;
	attrs->source = source;
	*out = attrs;
	return 0;
}

int git_attr_file__clear_rules(git_attr_file *file, bool need_lock)
{
	unsigned int i;
	git_attr_rule *rule;

	if (need_lock && git_mutex_lock(&file->lock) < 0) {
		giterr_set(GITERR_OS, "Failed to lock attribute file");
		return -1;
	}

	git_vector_foreach(&file->rules, i, rule)
		git_attr_rule__free(rule);
	git_vector_free(&file->rules);

	if (need_lock)
		git_mutex_unlock(&file->lock);

	return 0;
}

void git_attr_file__free(git_attr_file *file)
{
	if (!file)
		return;
	GIT_REFCOUNT_DEC(file, attr_file_free);
}

static int attr_file_oid_from_index(
	git_oid *oid, git_repository *repo, const char *path)
{
	int error;
	git_index *idx;
	size_t pos;
	const git_index_entry *entry;

	if ((error = git_repository_index__weakptr(&idx, repo)) < 0 ||
		(error = git_index__find_pos(&pos, idx, path, 0, 0)) < 0)
		return error;

	if (!(entry = git_index_get_byindex(idx, pos)))
		return GIT_ENOTFOUND;

	*oid = entry->id;
	return 0;
}

int git_attr_file__load(
	git_attr_file **out,
	git_repository *repo,
	git_attr_file_entry *entry,
	git_attr_file_source source,
	git_attr_file_parser parser)
{
	int error = 0;
	git_blob *blob = NULL;
	git_buf content = GIT_BUF_INIT;
	const char *data = NULL;
	git_attr_file *file;
	struct stat st;

	*out = NULL;

	switch (source) {
	case GIT_ATTR_FILE__IN_MEMORY:
		/* in-memory attribute file doesn't need data */
		break;
	case GIT_ATTR_FILE__FROM_INDEX: {
		git_oid id;

		if ((error = attr_file_oid_from_index(&id, repo, entry->path)) < 0 ||
			(error = git_blob_lookup(&blob, repo, &id)) < 0)
			return error;

		data = git_blob_rawcontent(blob);
		break;
	}
	case GIT_ATTR_FILE__FROM_FILE: {
		int fd;

		if (p_stat(entry->fullpath, &st) < 0)
			return git_path_set_error(errno, entry->fullpath, "stat");
		if (S_ISDIR(st.st_mode))
			return GIT_ENOTFOUND;

		/* For open or read errors, return ENOTFOUND to skip item */
		/* TODO: issue warning when warning API is available */

		if ((fd = git_futils_open_ro(entry->fullpath)) < 0)
			return GIT_ENOTFOUND;

		error = git_futils_readbuffer_fd(&content, fd, (size_t)st.st_size);
		p_close(fd);

		if (error < 0)
			return GIT_ENOTFOUND;

		data = content.ptr;
		break;
	}
	default:
		giterr_set(GITERR_INVALID, "Unknown file source %d", source);
		return -1;
	}

	if ((error = git_attr_file__new(&file, entry, source)) < 0)
		goto cleanup;

	if (parser && (error = parser(repo, file, data)) < 0) {
		git_attr_file__free(file);
		goto cleanup;
	}

	/* write cache breaker */
	if (source == GIT_ATTR_FILE__FROM_INDEX)
		git_oid_cpy(&file->cache_data.oid, git_blob_id(blob));
	else if (source == GIT_ATTR_FILE__FROM_FILE)
		git_futils_filestamp_set_from_stat(&file->cache_data.stamp, &st);
	/* else always cacheable */

	*out = file;

cleanup:
	git_blob_free(blob);
	git_buf_free(&content);

	return error;
}

int git_attr_file__out_of_date(git_repository *repo, git_attr_file *file)
{
	if (!file)
		return 1;

	switch (file->source) {
	case GIT_ATTR_FILE__IN_MEMORY:
		return 0;

	case GIT_ATTR_FILE__FROM_FILE:
		return git_futils_filestamp_check(
			&file->cache_data.stamp, file->entry->fullpath);

	case GIT_ATTR_FILE__FROM_INDEX: {
		int error;
		git_oid id;

		if ((error = attr_file_oid_from_index(
				&id, repo, file->entry->path)) < 0)
			return error;

		return (git_oid__cmp(&file->cache_data.oid, &id) != 0);
	}

	default:
		giterr_set(GITERR_INVALID, "Invalid file type %d", file->source);
		return -1;
	}
}

static int sort_by_hash_and_name(const void *a_raw, const void *b_raw);
static void git_attr_rule__clear(git_attr_rule *rule);
static bool parse_optimized_patterns(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *pattern);

int git_attr_file__parse_buffer(
	git_repository *repo, git_attr_file *attrs, const char *data)
{
	int error = 0;
	const char *scan = data, *context = NULL;
	git_attr_rule *rule = NULL;

	/* if subdir file path, convert context for file paths */
	if (attrs->entry &&
		git_path_root(attrs->entry->path) < 0 &&
		!git__suffixcmp(attrs->entry->path, "/" GIT_ATTR_FILE))
		context = attrs->entry->path;

	if (git_mutex_lock(&attrs->lock) < 0) {
		giterr_set(GITERR_OS, "Failed to lock attribute file");
		return -1;
	}

	while (!error && *scan) {
		/* allocate rule if needed */
		if (!rule && !(rule = git__calloc(1, sizeof(*rule)))) {
			error = -1;
			break;
		}

		rule->match.flags =
			GIT_ATTR_FNMATCH_ALLOWNEG | GIT_ATTR_FNMATCH_ALLOWMACRO;

		/* parse the next "pattern attr attr attr" line */
		if (!(error = git_attr_fnmatch__parse(
				&rule->match, &attrs->pool, context, &scan)) &&
			!(error = git_attr_assignment__parse(
				repo, &attrs->pool, &rule->assigns, &scan)))
		{
			if (rule->match.flags & GIT_ATTR_FNMATCH_MACRO)
				/* TODO: warning if macro found in file below repo root */
				error = git_attr_cache__insert_macro(repo, rule);
			else
				error = git_vector_insert(&attrs->rules, rule);
		}

		/* if the rule wasn't a pattern, on to the next */
		if (error < 0) {
			git_attr_rule__clear(rule); /* reset rule contents */
			if (error == GIT_ENOTFOUND)
				error = 0;
		} else {
			rule = NULL; /* vector now "owns" the rule */
		}
	}

	git_mutex_unlock(&attrs->lock);
	git_attr_rule__free(rule);

	return error;
}

uint32_t git_attr_file__name_hash(const char *name)
{
	uint32_t h = 5381;
	int c;
	assert(name);
	while ((c = (int)*name++) != 0)
		h = ((h << 5) + h) + c;
	return h;
}

int git_attr_file__lookup_one(
	git_attr_file *file,
	git_attr_path *path,
	const char *attr,
	const char **value)
{
	size_t i;
	git_attr_name name;
	git_attr_rule *rule;

	*value = NULL;

	name.name = attr;
	name.name_hash = git_attr_file__name_hash(attr);

	git_attr_file__foreach_matching_rule(file, path, i, rule) {
		size_t pos;

		if (!git_vector_bsearch(&pos, &rule->assigns, &name)) {
			*value = ((git_attr_assignment *)
					  git_vector_get(&rule->assigns, pos))->value;
			break;
		}
	}

	return 0;
}

int git_attr_file__load_standalone(git_attr_file **out, const char *path)
{
	int error;
	git_attr_file *file;
	git_buf content = GIT_BUF_INIT;

	error = git_attr_file__new(&file, NULL, GIT_ATTR_FILE__FROM_FILE);
	if (error < 0)
		return error;

	error = git_attr_cache__alloc_file_entry(
		&file->entry, NULL, path, &file->pool);
	if (error < 0) {
		git_attr_file__free(file);
		return error;
	}
	/* because the cache entry is allocated from the file's own pool, we
	 * don't have to free it - freeing file+pool will free cache entry, too.
	 */

	if (!(error = git_futils_readbuffer(&content, path))) {
		error = git_attr_file__parse_buffer(NULL, file, content.ptr);
		git_buf_free(&content);
	}

	if (error < 0)
		git_attr_file__free(file);
	else
		*out = file;

	return error;
}

bool git_attr_fnmatch__match(
	git_attr_fnmatch *match,
	git_attr_path *path)
{
	const char *filename;
	int flags = 0;

	if (match->flags & GIT_ATTR_FNMATCH_ICASE)
		flags |= FNM_CASEFOLD;
	if (match->flags & GIT_ATTR_FNMATCH_LEADINGDIR)
		flags |= FNM_LEADING_DIR;

	if (match->flags & GIT_ATTR_FNMATCH_FULLPATH) {
		filename = path->path;
		flags |= FNM_PATHNAME;
	} else {
		filename = path->basename;

		if (path->is_dir)
			flags |= FNM_LEADING_DIR;
	}

	if ((match->flags & GIT_ATTR_FNMATCH_DIRECTORY) && !path->is_dir) {
		int matchval;

		/* for attribute checks or root ignore checks, fail match */
		if (!(match->flags & GIT_ATTR_FNMATCH_IGNORE) ||
			path->basename == path->path)
			return false;

		/* for ignore checks, use container of current item for check */
		path->basename[-1] = '\0';
		flags |= FNM_LEADING_DIR;
		matchval = p_fnmatch(match->pattern, path->path, flags);
		path->basename[-1] = '/';
		return (matchval != FNM_NOMATCH);
	}

	return (p_fnmatch(match->pattern, filename, flags) != FNM_NOMATCH);
}

bool git_attr_rule__match(
	git_attr_rule *rule,
	git_attr_path *path)
{
	bool matched = git_attr_fnmatch__match(&rule->match, path);

	if (rule->match.flags & GIT_ATTR_FNMATCH_NEGATIVE)
		matched = !matched;

	return matched;
}

git_attr_assignment *git_attr_rule__lookup_assignment(
	git_attr_rule *rule, const char *name)
{
	size_t pos;
	git_attr_name key;
	key.name = name;
	key.name_hash = git_attr_file__name_hash(name);

	if (git_vector_bsearch(&pos, &rule->assigns, &key))
		return NULL;

	return git_vector_get(&rule->assigns, pos);
}

int git_attr_path__init(
	git_attr_path *info, const char *path, const char *base)
{
	ssize_t root;

	/* build full path as best we can */
	git_buf_init(&info->full, 0);

	if (git_path_join_unrooted(&info->full, path, base, &root) < 0)
		return -1;

	info->path = info->full.ptr + root;

	/* remove trailing slashes */
	while (info->full.size > 0) {
		if (info->full.ptr[info->full.size - 1] != '/')
			break;
		info->full.size--;
	}
	info->full.ptr[info->full.size] = '\0';

	/* skip leading slashes in path */
	while (*info->path == '/')
		info->path++;

	/* find trailing basename component */
	info->basename = strrchr(info->path, '/');
	if (info->basename)
		info->basename++;
	if (!info->basename || !*info->basename)
		info->basename = info->path;

	info->is_dir = (int)git_path_isdir(info->full.ptr);

	return 0;
}

void git_attr_path__free(git_attr_path *info)
{
	git_buf_free(&info->full);
	info->path = NULL;
	info->basename = NULL;
}

/*
 * From gitattributes(5):
 *
 * Patterns have the following format:
 *
 * - A blank line matches no files, so it can serve as a separator for
 *   readability.
 *
 * - A line starting with # serves as a comment.
 *
 * - An optional prefix ! which negates the pattern; any matching file
 *   excluded by a previous pattern will become included again. If a negated
 *   pattern matches, this will override lower precedence patterns sources.
 *
 * - If the pattern ends with a slash, it is removed for the purpose of the
 *   following description, but it would only find a match with a directory. In
 *   other words, foo/ will match a directory foo and paths underneath it, but
 *   will not match a regular file or a symbolic link foo (this is consistent
 *   with the way how pathspec works in general in git).
 *
 * - If the pattern does not contain a slash /, git treats it as a shell glob
 *   pattern and checks for a match against the pathname without leading
 *   directories.
 *
 * - Otherwise, git treats the pattern as a shell glob suitable for consumption
 *   by fnmatch(3) with the FNM_PATHNAME flag: wildcards in the pattern will
 *   not match a / in the pathname. For example, "Documentation/\*.html" matches
 *   "Documentation/git.html" but not "Documentation/ppc/ppc.html". A leading
 *   slash matches the beginning of the pathname; for example, "/\*.c" matches
 *   "cat-file.c" but not "mozilla-sha1/sha1.c".
 */

/*
 * This will return 0 if the spec was filled out,
 * GIT_ENOTFOUND if the fnmatch does not require matching, or
 * another error code there was an actual problem.
 */
int git_attr_fnmatch__parse(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *context,
	const char **base)
{
	const char *pattern, *scan;
	int slash_count, allow_space;

	assert(spec && base && *base);

	if (parse_optimized_patterns(spec, pool, *base))
		return 0;

	spec->flags = (spec->flags & GIT_ATTR_FNMATCH__INCOMING);
	allow_space = ((spec->flags & GIT_ATTR_FNMATCH_ALLOWSPACE) != 0);

	pattern = *base;

	while (git__isspace(*pattern)) pattern++;
	if (!*pattern || *pattern == '#') {
		*base = git__next_line(pattern);
		return GIT_ENOTFOUND;
	}

	if (*pattern == '[' && (spec->flags & GIT_ATTR_FNMATCH_ALLOWMACRO) != 0) {
		if (strncmp(pattern, "[attr]", 6) == 0) {
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_MACRO;
			pattern += 6;
		}
		/* else a character range like [a-e]* which is accepted */
	}

	if (*pattern == '!' && (spec->flags & GIT_ATTR_FNMATCH_ALLOWNEG) != 0) {
		spec->flags = spec->flags | GIT_ATTR_FNMATCH_NEGATIVE;
		pattern++;
	}

	slash_count = 0;
	for (scan = pattern; *scan != '\0'; ++scan) {
		/* scan until (non-escaped) white space */
		if (git__isspace(*scan) && *(scan - 1) != '\\') {
			if (!allow_space || (*scan != ' ' && *scan != '\t'))
				break;
		}

		if (*scan == '/') {
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_FULLPATH;
			slash_count++;
			if (pattern == scan)
				pattern++;
		}
		/* remember if we see an unescaped wildcard in pattern */
		else if (git__iswildcard(*scan) &&
			(scan == pattern || (*(scan - 1) != '\\')))
			spec->flags = spec->flags | GIT_ATTR_FNMATCH_HASWILD;
	}

	*base = scan;

	if ((spec->length = scan - pattern) == 0)
		return GIT_ENOTFOUND;

	if (pattern[spec->length - 1] == '/') {
		spec->length--;
		spec->flags = spec->flags | GIT_ATTR_FNMATCH_DIRECTORY;
		if (--slash_count <= 0)
			spec->flags = spec->flags & ~GIT_ATTR_FNMATCH_FULLPATH;
	}
	if ((spec->flags & GIT_ATTR_FNMATCH_NOLEADINGDIR) == 0 &&
		spec->length >= 2 &&
		pattern[spec->length - 1] == '*' &&
		pattern[spec->length - 2] == '/') {
		spec->length -= 2;
		spec->flags = spec->flags | GIT_ATTR_FNMATCH_LEADINGDIR;
		/* leave FULLPATH match on, however */
	}

	if ((spec->flags & GIT_ATTR_FNMATCH_FULLPATH) != 0 &&
		context != NULL && git_path_root(pattern) < 0)
	{
		/* use context path minus the trailing filename */
		char *slash = strrchr(context, '/');
		size_t contextlen = slash ? slash - context + 1 : 0;

		/* given an unrooted fullpath match from a file inside a repo,
		 * prefix the pattern with the relative directory of the source file
		 */
		spec->pattern = git_pool_malloc(
			pool, (uint32_t)(contextlen + spec->length + 1));
		if (spec->pattern) {
			memcpy(spec->pattern, context, contextlen);
			memcpy(spec->pattern + contextlen, pattern, spec->length);
			spec->length += contextlen;
			spec->pattern[spec->length] = '\0';
		}
	} else {
		spec->pattern = git_pool_strndup(pool, pattern, spec->length);
	}

	if (!spec->pattern) {
		*base = git__next_line(pattern);
		return -1;
	} else {
		/* strip '\' that might have be used for internal whitespace */
		spec->length = git__unescape(spec->pattern);
		/* TODO: convert remaining '\' into '/' for POSIX ??? */
	}

	return 0;
}

static bool parse_optimized_patterns(
	git_attr_fnmatch *spec,
	git_pool *pool,
	const char *pattern)
{
	if (!pattern[1] && (pattern[0] == '*' || pattern[0] == '.')) {
		spec->flags = GIT_ATTR_FNMATCH_MATCH_ALL;
		spec->pattern = git_pool_strndup(pool, pattern, 1);
		spec->length = 1;

		return true;
	}

	return false;
}

static int sort_by_hash_and_name(const void *a_raw, const void *b_raw)
{
	const git_attr_name *a = a_raw;
	const git_attr_name *b = b_raw;

	if (b->name_hash < a->name_hash)
		return 1;
	else if (b->name_hash > a->name_hash)
		return -1;
	else
		return strcmp(b->name, a->name);
}

static void git_attr_assignment__free(git_attr_assignment *assign)
{
	/* name and value are stored in a git_pool associated with the
	 * git_attr_file, so they do not need to be freed here
	 */
	assign->name = NULL;
	assign->value = NULL;
	git__free(assign);
}

static int merge_assignments(void **old_raw, void *new_raw)
{
	git_attr_assignment **old = (git_attr_assignment **)old_raw;
	git_attr_assignment *new = (git_attr_assignment *)new_raw;

	GIT_REFCOUNT_DEC(*old, git_attr_assignment__free);
	*old = new;
	return GIT_EEXISTS;
}

int git_attr_assignment__parse(
	git_repository *repo,
	git_pool *pool,
	git_vector *assigns,
	const char **base)
{
	int error;
	const char *scan = *base;
	git_attr_assignment *assign = NULL;

	assert(assigns && !assigns->length);

	git_vector_set_cmp(assigns, sort_by_hash_and_name);

	while (*scan && *scan != '\n') {
		const char *name_start, *value_start;

		/* skip leading blanks */
		while (git__isspace(*scan) && *scan != '\n') scan++;

		/* allocate assign if needed */
		if (!assign) {
			assign = git__calloc(1, sizeof(git_attr_assignment));
			GITERR_CHECK_ALLOC(assign);
			GIT_REFCOUNT_INC(assign);
		}

		assign->name_hash = 5381;
		assign->value = git_attr__true;

		/* look for magic name prefixes */
		if (*scan == '-') {
			assign->value = git_attr__false;
			scan++;
		} else if (*scan == '!') {
			assign->value = git_attr__unset; /* explicit unspecified state */
			scan++;
		} else if (*scan == '#') /* comment rest of line */
			break;

		/* find the name */
		name_start = scan;
		while (*scan && !git__isspace(*scan) && *scan != '=') {
			assign->name_hash =
				((assign->name_hash << 5) + assign->name_hash) + *scan;
			scan++;
		}
		if (scan == name_start) {
			/* must have found lone prefix (" - ") or leading = ("=foo")
			 * or end of buffer -- advance until whitespace and continue
			 */
			while (*scan && !git__isspace(*scan)) scan++;
			continue;
		}

		/* allocate permanent storage for name */
		assign->name = git_pool_strndup(pool, name_start, scan - name_start);
		GITERR_CHECK_ALLOC(assign->name);

		/* if there is an equals sign, find the value */
		if (*scan == '=') {
			for (value_start = ++scan; *scan && !git__isspace(*scan); ++scan);

			/* if we found a value, allocate permanent storage for it */
			if (scan > value_start) {
				assign->value = git_pool_strndup(pool, value_start, scan - value_start);
				GITERR_CHECK_ALLOC(assign->value);
			}
		}

		/* expand macros (if given a repo with a macro cache) */
		if (repo != NULL && assign->value == git_attr__true) {
			git_attr_rule *macro =
				git_attr_cache__lookup_macro(repo, assign->name);

			if (macro != NULL) {
				unsigned int i;
				git_attr_assignment *massign;

				git_vector_foreach(&macro->assigns, i, massign) {
					GIT_REFCOUNT_INC(massign);

					error = git_vector_insert_sorted(
						assigns, massign, &merge_assignments);
					if (error < 0 && error != GIT_EEXISTS)
						return error;
				}
			}
		}

		/* insert allocated assign into vector */
		error = git_vector_insert_sorted(assigns, assign, &merge_assignments);
		if (error < 0 && error != GIT_EEXISTS)
			return error;

		/* clear assign since it is now "owned" by the vector */
		assign = NULL;
	}

	if (assign != NULL)
		git_attr_assignment__free(assign);

	*base = git__next_line(scan);

	return (assigns->length == 0) ? GIT_ENOTFOUND : 0;
}

static void git_attr_rule__clear(git_attr_rule *rule)
{
	unsigned int i;
	git_attr_assignment *assign;

	if (!rule)
		return;

	if (!(rule->match.flags & GIT_ATTR_FNMATCH_IGNORE)) {
		git_vector_foreach(&rule->assigns, i, assign)
			GIT_REFCOUNT_DEC(assign, git_attr_assignment__free);
		git_vector_free(&rule->assigns);
	}

	/* match.pattern is stored in a git_pool, so no need to free */
	rule->match.pattern = NULL;
	rule->match.length = 0;
}

void git_attr_rule__free(git_attr_rule *rule)
{
	git_attr_rule__clear(rule);
	git__free(rule);
}

