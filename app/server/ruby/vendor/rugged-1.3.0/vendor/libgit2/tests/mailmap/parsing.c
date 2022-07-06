#include "clar_libgit2.h"
#include "repository.h"
#include "git2/sys/repository.h"
#include "mailmap_testdata.h"

static git_repository *g_repo;
static git_mailmap *g_mailmap;
static git_config *g_config;

static const char string_mailmap[] =
	"# Simple Comment line\n"
	"<cto@company.xx>                       <cto@coompany.xx>\n"
	"Some Dude <some@dude.xx>         nick1 <bugs@company.xx>\n"
	"Other Author <other@author.xx>   nick2 <bugs@company.xx>\n"
	"Other Author <other@author.xx>         <nick2@company.xx>\n"
	"Phil Hill <phil@company.xx>  # Comment at end of line\n"
	"<joseph@company.xx>             Joseph <bugs@company.xx>\n"
	"Santa Claus <santa.claus@northpole.xx> <me@company.xx>\n"
	"Untracked <untracked@company.xx>";

static const mailmap_entry entries[] = {
	{ NULL, "cto@company.xx", NULL, "cto@coompany.xx" },
	{ "Some Dude", "some@dude.xx", "nick1", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "nick2", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", NULL, "nick2@company.xx" },
	{ "Phil Hill", NULL, NULL, "phil@company.xx" },
	{ NULL, "joseph@company.xx", "Joseph", "bugs@company.xx" },
	{ "Santa Claus", "santa.claus@northpole.xx", NULL, "me@company.xx" },
	/* This entry isn't in the bare repository */
	{ "Untracked", NULL, NULL, "untracked@company.xx" }
};

void test_mailmap_parsing__initialize(void)
{
	g_repo = NULL;
	g_mailmap = NULL;
	g_config = NULL;
}

void test_mailmap_parsing__cleanup(void)
{
	git_mailmap_free(g_mailmap);
	git_config_free(g_config);
	cl_git_sandbox_cleanup();
}

static void check_mailmap_entries(
	const git_mailmap *mailmap, const mailmap_entry *entries, size_t entries_size)
{
	const git_mailmap_entry *parsed;
	size_t idx;

	/* Check the correct # of entries were parsed */
	cl_assert_equal_sz(entries_size, git_vector_length(&mailmap->entries));

	/* Make sure looking up each entry succeeds */
	for (idx = 0; idx < entries_size; ++idx) {
		parsed = git_mailmap_entry_lookup(
			mailmap, entries[idx].replace_name, entries[idx].replace_email);

		cl_assert(parsed);
		cl_assert_equal_s(parsed->real_name, entries[idx].real_name);
		cl_assert_equal_s(parsed->real_email, entries[idx].real_email);
		cl_assert_equal_s(parsed->replace_name, entries[idx].replace_name);
		cl_assert_equal_s(parsed->replace_email, entries[idx].replace_email);
	}
}

static void check_mailmap_resolve(
	const git_mailmap *mailmap, const mailmap_entry *resolved, size_t resolved_size)
{
	const char *resolved_name = NULL;
	const char *resolved_email = NULL;
	size_t idx;

	/* Check that the resolver behaves correctly */
	for (idx = 0; idx < resolved_size; ++idx) {
		cl_git_pass(git_mailmap_resolve(
			&resolved_name, &resolved_email, mailmap,
			resolved[idx].replace_name, resolved[idx].replace_email));
		cl_assert_equal_s(resolved_name, resolved[idx].real_name);
		cl_assert_equal_s(resolved_email, resolved[idx].real_email);
	}
}

static const mailmap_entry resolved_untracked[] = {
	{ "Untracked", "untracked@company.xx", "xx", "untracked@company.xx" }
};

void test_mailmap_parsing__string(void)
{
	cl_git_pass(git_mailmap_from_buffer(
		&g_mailmap, string_mailmap, strlen(string_mailmap)));

	/* We should have parsed all of the entries */
	check_mailmap_entries(g_mailmap, entries, ARRAY_SIZE(entries));

	/* Check that resolving the entries works */
	check_mailmap_resolve(g_mailmap, resolved, ARRAY_SIZE(resolved));
	check_mailmap_resolve(
		g_mailmap, resolved_untracked, ARRAY_SIZE(resolved_untracked));
}

void test_mailmap_parsing__windows_string(void)
{
	git_buf unixbuf = GIT_BUF_INIT;
	git_buf winbuf = GIT_BUF_INIT;

	/* Parse with windows-style line endings */
	git_buf_attach_notowned(&unixbuf, string_mailmap, strlen(string_mailmap));
	cl_git_pass(git_buf_lf_to_crlf(&winbuf, &unixbuf));

	cl_git_pass(git_mailmap_from_buffer(&g_mailmap, winbuf.ptr, winbuf.size));
	git_buf_dispose(&winbuf);

	/* We should have parsed all of the entries */
	check_mailmap_entries(g_mailmap, entries, ARRAY_SIZE(entries));

	/* Check that resolving the entries works */
	check_mailmap_resolve(g_mailmap, resolved, ARRAY_SIZE(resolved));
	check_mailmap_resolve(
		g_mailmap, resolved_untracked, ARRAY_SIZE(resolved_untracked));
}

void test_mailmap_parsing__fromrepo(void)
{
	g_repo = cl_git_sandbox_init("mailmap");
	cl_check(!git_repository_is_bare(g_repo));

	cl_git_pass(git_mailmap_from_repository(&g_mailmap, g_repo));

	/* We should have parsed all of the entries */
	check_mailmap_entries(g_mailmap, entries, ARRAY_SIZE(entries));

	/* Check that resolving the entries works */
	check_mailmap_resolve(g_mailmap, resolved, ARRAY_SIZE(resolved));
	check_mailmap_resolve(
		g_mailmap, resolved_untracked, ARRAY_SIZE(resolved_untracked));
}

static const mailmap_entry resolved_bare[] = {
	{ "xx", "untracked@company.xx", "xx", "untracked@company.xx" }
};

void test_mailmap_parsing__frombare(void)
{
	g_repo = cl_git_sandbox_init("mailmap/.gitted");
	cl_git_pass(git_repository_set_bare(g_repo));
	cl_check(git_repository_is_bare(g_repo));

	cl_git_pass(git_mailmap_from_repository(&g_mailmap, g_repo));

	/* We should have parsed all of the entries, except for the untracked one */
	check_mailmap_entries(g_mailmap, entries, ARRAY_SIZE(entries) - 1);

	/* Check that resolving the entries works */
	check_mailmap_resolve(g_mailmap, resolved, ARRAY_SIZE(resolved));
	check_mailmap_resolve(
		g_mailmap, resolved_bare, ARRAY_SIZE(resolved_bare));
}

static const mailmap_entry resolved_with_file_override[] = {
	{ "Brad", "cto@company.xx", "Brad", "cto@coompany.xx" },
	{ "Brad L", "cto@company.xx", "Brad L", "cto@coompany.xx" },
	{ "Some Dude", "some@dude.xx", "nick1", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "nick2", "bugs@company.xx" },
	{ "nick3", "bugs@company.xx", "nick3", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "Some Garbage", "nick2@company.xx" },
	{ "Joseph", "joseph@company.xx", "Joseph", "bugs@company.xx" },
	{ "Santa Claus", "santa.claus@northpole.xx", "Clause", "me@company.xx" },
	{ "Charles", "charles@charles.xx", "Charles", "charles@charles.xx" },

	/* This name is overridden by file_override */
	{ "File Override", "phil@company.xx", "unknown", "phil@company.xx" },
	{ "Other Name", "fileoverridename@company.xx", "override", "fileoverridename@company.xx" }
};

void test_mailmap_parsing__file_config(void)
{
	g_repo = cl_git_sandbox_init("mailmap");
	cl_git_pass(git_repository_config(&g_config, g_repo));

	cl_git_pass(git_config_set_string(
		g_config, "mailmap.file", cl_fixture("mailmap/file_override")));

	cl_git_pass(git_mailmap_from_repository(&g_mailmap, g_repo));

	/* Check we don't have duplicate entries */
	cl_assert_equal_sz(git_vector_length(&g_mailmap->entries), 9);

	/* Check that resolving the entries works */
	check_mailmap_resolve(
		g_mailmap, resolved_with_file_override,
		ARRAY_SIZE(resolved_with_file_override));
}

static const mailmap_entry resolved_with_blob_override[] = {
	{ "Brad", "cto@company.xx", "Brad", "cto@coompany.xx" },
	{ "Brad L", "cto@company.xx", "Brad L", "cto@coompany.xx" },
	{ "Some Dude", "some@dude.xx", "nick1", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "nick2", "bugs@company.xx" },
	{ "nick3", "bugs@company.xx", "nick3", "bugs@company.xx" },
	{ "Other Author", "other@author.xx", "Some Garbage", "nick2@company.xx" },
	{ "Joseph", "joseph@company.xx", "Joseph", "bugs@company.xx" },
	{ "Santa Claus", "santa.claus@northpole.xx", "Clause", "me@company.xx" },
	{ "Charles", "charles@charles.xx", "Charles", "charles@charles.xx" },

	/* This name is overridden by blob_override */
	{ "Blob Override", "phil@company.xx", "unknown", "phil@company.xx" },
	{ "Other Name", "bloboverridename@company.xx", "override", "bloboverridename@company.xx" }
};

void test_mailmap_parsing__blob_config(void)
{
	g_repo = cl_git_sandbox_init("mailmap");
	cl_git_pass(git_repository_config(&g_config, g_repo));

	cl_git_pass(git_config_set_string(
		g_config, "mailmap.blob", "HEAD:blob_override"));

	cl_git_pass(git_mailmap_from_repository(&g_mailmap, g_repo));

	/* Check we don't have duplicate entries */
	cl_assert_equal_sz(git_vector_length(&g_mailmap->entries), 9);

	/* Check that resolving the entries works */
	check_mailmap_resolve(
		g_mailmap, resolved_with_blob_override,
		ARRAY_SIZE(resolved_with_blob_override));
}

static const mailmap_entry bare_resolved_with_blob_override[] = {
	/* As mailmap.blob is set, we won't load HEAD:.mailmap */
	{ "Brad", "cto@coompany.xx", "Brad", "cto@coompany.xx" },
	{ "Brad L", "cto@coompany.xx", "Brad L", "cto@coompany.xx" },
	{ "nick1", "bugs@company.xx", "nick1", "bugs@company.xx" },
	{ "nick2", "bugs@company.xx", "nick2", "bugs@company.xx" },
	{ "nick3", "bugs@company.xx", "nick3", "bugs@company.xx" },
	{ "Some Garbage", "nick2@company.xx", "Some Garbage", "nick2@company.xx" },
	{ "Joseph", "bugs@company.xx", "Joseph", "bugs@company.xx" },
	{ "Clause", "me@company.xx", "Clause", "me@company.xx" },
	{ "Charles", "charles@charles.xx", "Charles", "charles@charles.xx" },

	/* This name is overridden by blob_override */
	{ "Blob Override", "phil@company.xx", "unknown", "phil@company.xx" },
	{ "Other Name", "bloboverridename@company.xx", "override", "bloboverridename@company.xx" }
};

void test_mailmap_parsing__bare_blob_config(void)
{
	g_repo = cl_git_sandbox_init("mailmap/.gitted");
	cl_git_pass(git_repository_set_bare(g_repo));
	cl_check(git_repository_is_bare(g_repo));

	cl_git_pass(git_repository_config(&g_config, g_repo));

	cl_git_pass(git_config_set_string(
		g_config, "mailmap.blob", "HEAD:blob_override"));

	cl_git_pass(git_mailmap_from_repository(&g_mailmap, g_repo));

	/* Check that we only have the 2 entries */
	cl_assert_equal_sz(git_vector_length(&g_mailmap->entries), 2);

	/* Check that resolving the entries works */
	check_mailmap_resolve(
		g_mailmap, bare_resolved_with_blob_override,
		ARRAY_SIZE(bare_resolved_with_blob_override));
}
