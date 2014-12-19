#ifndef __CLAR_LIBGIT2__
#define __CLAR_LIBGIT2__

#include "clar.h"
#include <git2.h>
#include <posix.h>
#include "common.h"

/**
 * Replace for `clar_must_pass` that passes the last library error as the
 * test failure message.
 *
 * Use this wrapper around all `git_` library calls that return error codes!
 */
#define cl_git_pass(expr) cl_git_pass_((expr), __FILE__, __LINE__)

#define cl_git_pass_(expr, file, line) do { \
	int _lg2_error; \
	giterr_clear(); \
	if ((_lg2_error = (expr)) != 0) \
		cl_git_report_failure(_lg2_error, file, line, "Function call failed: " #expr); \
	} while (0)

/**
 * Wrapper for `clar_must_fail` -- this one is
 * just for consistency. Use with `git_` library
 * calls that are supposed to fail!
 */
#define cl_git_fail(expr) cl_must_fail(expr)

#define cl_git_fail_with(expr, error) cl_assert_equal_i(error,expr)

/**
 * Like cl_git_pass, only for Win32 error code conventions
 */
#define cl_win32_pass(expr) do { \
	int _win32_res; \
	if ((_win32_res = (expr)) == 0) { \
		giterr_set(GITERR_OS, "Returned: %d, system error code: %d", _win32_res, GetLastError()); \
		cl_git_report_failure(_win32_res, __FILE__, __LINE__, "System call failed: " #expr); \
	} \
	} while(0)

void cl_git_report_failure(int, const char *, int, const char *);

#define cl_assert_at_line(expr,file,line) \
	clar__assert((expr) != 0, file, line, "Expression is not true: " #expr, NULL, 1)

GIT_INLINE(void) clar__assert_in_range(
	int lo, int val, int hi,
	const char *file, int line, const char *err, int should_abort)
{
	if (lo > val || hi < val) {
		char buf[128];
		p_snprintf(buf, sizeof(buf), "%d not in [%d,%d]", val, lo, hi);
		clar__fail(file, line, err, buf, should_abort);
	}
}

#define cl_assert_equal_sz(sz1,sz2) do { \
	size_t __sz1 = (size_t)(sz1), __sz2 = (size_t)(sz2); \
	clar__assert_equal(__FILE__,__LINE__,#sz1 " != " #sz2, 1, "%"PRIuZ, __sz1, __sz2); \
} while (0)

#define cl_assert_in_range(L,V,H) \
	clar__assert_in_range((L),(V),(H),__FILE__,__LINE__,"Range check: " #V " in [" #L "," #H "]", 1)

#define cl_assert_equal_file(DATA,SIZE,PATH) \
	clar__assert_equal_file(DATA,SIZE,0,PATH,__FILE__,(int)__LINE__)

#define cl_assert_equal_file_ignore_cr(DATA,SIZE,PATH) \
	clar__assert_equal_file(DATA,SIZE,1,PATH,__FILE__,(int)__LINE__)

void clar__assert_equal_file(
	const char *expected_data,
	size_t expected_size,
	int ignore_cr,
	const char *path,
	const char *file,
	int line);

GIT_INLINE(void) clar__assert_equal_oid(
	const char *file, int line, const char *desc,
	const git_oid *one, const git_oid *two)
{
	if (git_oid_cmp(one, two)) {
		char err[] = "\"........................................\" != \"........................................\"";

		git_oid_fmt(&err[1], one);
		git_oid_fmt(&err[47], two);

		clar__fail(file, line, desc, err, 1);
	}
}

#define cl_assert_equal_oid(one, two) \
	clar__assert_equal_oid(__FILE__, __LINE__, \
		"OID mismatch: " #one " != " #two, (one), (two))

/*
 * Some utility macros for building long strings
 */
#define REP4(STR)	 STR STR STR STR
#define REP15(STR)	 REP4(STR) REP4(STR) REP4(STR) STR STR STR
#define REP16(STR)	 REP4(REP4(STR))
#define REP256(STR)  REP16(REP16(STR))
#define REP1024(STR) REP4(REP256(STR))

/* Write the contents of a buffer to disk */
void cl_git_mkfile(const char *filename, const char *content);
void cl_git_append2file(const char *filename, const char *new_content);
void cl_git_rewritefile(const char *filename, const char *new_content);
void cl_git_write2file(const char *path, const char *data,
	size_t datalen, int flags, unsigned int mode);

bool cl_toggle_filemode(const char *filename);
bool cl_is_chmod_supported(void);

/* Environment wrappers */
char *cl_getenv(const char *name);
int cl_setenv(const char *name, const char *value);

/* Reliable rename */
int cl_rename(const char *source, const char *dest);

/* Git sandbox setup helpers */

git_repository *cl_git_sandbox_init(const char *sandbox);
void cl_git_sandbox_cleanup(void);
git_repository *cl_git_sandbox_reopen(void);

/* Local-repo url helpers */
const char* cl_git_fixture_url(const char *fixturename);
const char* cl_git_path_url(const char *path);

/* Test repository cleaner */
int cl_git_remove_placeholders(const char *directory_path, const char *filename);

/* commit creation helpers */
void cl_repo_commit_from_index(
	git_oid *out,
	git_repository *repo,
	git_signature *sig,
	git_time_t time,
	const char *msg);

/* config setting helpers */
void cl_repo_set_bool(git_repository *repo, const char *cfg, int value);
int cl_repo_get_bool(git_repository *repo, const char *cfg);

void cl_repo_set_string(git_repository *repo, const char *cfg, const char *value);

/* set up a fake "home" directory and set libgit2 GLOBAL search path.
 *
 * automatically configures cleanup function to restore the regular search
 * path, although you can call it explicitly if you wish (with NULL).
 */
void cl_fake_home(void);
void cl_fake_home_cleanup(void *);

void cl_sandbox_set_search_path_defaults(void);

#endif
