#include "clar_libgit2.h"
#include <git2/types.h>
#include "commit.h"
#include "signature.h"

// Fixture setup
static git_repository *g_repo;
void test_commit_parse__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}
void test_commit_parse__cleanup(void)
{
	cl_git_sandbox_cleanup();
}


// Header parsing
typedef struct {
	const char *line;
	const char *header;
} parse_test_case;

static parse_test_case passing_header_cases[] = {
	{ "parent 05452d6349abcd67aa396dfb28660d765d8b2a36\n", "parent " },
	{ "tree 05452d6349abcd67aa396dfb28660d765d8b2a36\n", "tree " },
	{ "random_heading 05452d6349abcd67aa396dfb28660d765d8b2a36\n", "random_heading " },
	{ "stuck_heading05452d6349abcd67aa396dfb28660d765d8b2a36\n", "stuck_heading" },
	{ "tree 5F4BEFFC0759261D015AA63A3A85613FF2F235DE\n", "tree " },
	{ "tree 1A669B8AB81B5EB7D9DB69562D34952A38A9B504\n", "tree " },
	{ "tree 5B20DCC6110FCC75D31C6CEDEBD7F43ECA65B503\n", "tree " },
	{ "tree 173E7BF00EA5C33447E99E6C1255954A13026BE4\n", "tree " },
	{ NULL, NULL }
};

static parse_test_case failing_header_cases[] = {
	{ "parent 05452d6349abcd67aa396dfb28660d765d8b2a36", "parent " },
	{ "05452d6349abcd67aa396dfb28660d765d8b2a36\n", "tree " },
	{ "parent05452d6349abcd67aa396dfb28660d765d8b2a6a\n", "parent " },
	{ "parent 05452d6349abcd67aa396dfb280d765d8b2a6\n", "parent " },
	{ "tree  05452d6349abcd67aa396dfb28660d765d8b2a36\n", "tree " },
	{ "parent 0545xd6349abcd67aa396dfb28660d765d8b2a36\n", "parent " },
	{ "parent 0545xd6349abcd67aa396dfb28660d765d8b2a36FF\n", "parent " },
	{ "", "tree " },
	{ "", "" },
	{ NULL, NULL }
};

void test_commit_parse__header(void)
{
	git_oid oid;

	parse_test_case *testcase;
	for (testcase = passing_header_cases; testcase->line != NULL; testcase++)
	{
		const char *line = testcase->line;
		const char *line_end = line + strlen(line);

		cl_git_pass(git_oid__parse(&oid, &line, line_end, testcase->header));
		cl_assert(line == line_end);
	}

	for (testcase = failing_header_cases; testcase->line != NULL; testcase++)
	{
		const char *line = testcase->line;
		const char *line_end = line + strlen(line);

		cl_git_fail(git_oid__parse(&oid, &line, line_end, testcase->header));
	}
}


// Signature parsing
typedef struct {
	const char *string;
	const char *header;
	const char *name;
	const char *email;
	git_time_t time;
	int offset;
} passing_signature_test_case;

passing_signature_test_case passing_signature_cases[] = {
	{"author Vicent Marti <tanoku@gmail.com> 12345 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 12345, 0},
	{"author Vicent Marti <> 12345 \n", "author ", "Vicent Marti", "", 12345, 0},
	{"author Vicent Marti <tanoku@gmail.com> 231301 +1020\n", "author ", "Vicent Marti", "tanoku@gmail.com", 231301, 620},
	{"author Vicent Marti with an outrageously long name which will probably overflow the buffer <tanoku@gmail.com> 12345 \n", "author ", "Vicent Marti with an outrageously long name which will probably overflow the buffer", "tanoku@gmail.com", 12345, 0},
	{"author Vicent Marti <tanokuwithaveryveryverylongemailwhichwillprobablyvoverflowtheemailbuffer@gmail.com> 12345 \n", "author ", "Vicent Marti", "tanokuwithaveryveryverylongemailwhichwillprobablyvoverflowtheemailbuffer@gmail.com", 12345, 0},
	{"committer Vicent Marti <tanoku@gmail.com> 123456 +0000 \n", "committer ", "Vicent Marti", "tanoku@gmail.com", 123456, 0},
	{"committer Vicent Marti <tanoku@gmail.com> 123456 +0100 \n", "committer ", "Vicent Marti", "tanoku@gmail.com", 123456, 60},
	{"committer Vicent Marti <tanoku@gmail.com> 123456 -0100 \n", "committer ", "Vicent Marti", "tanoku@gmail.com", 123456, -60},
	// Parse a signature without an author field
	{"committer <tanoku@gmail.com> 123456 -0100 \n", "committer ", "", "tanoku@gmail.com", 123456, -60},
	// Parse a signature without an author field
	{"committer  <tanoku@gmail.com> 123456 -0100 \n", "committer ", "", "tanoku@gmail.com", 123456, -60},
	// Parse a signature with an empty author field
	{"committer   <tanoku@gmail.com> 123456 -0100 \n", "committer ", "", "tanoku@gmail.com", 123456, -60},
	// Parse a signature with an empty email field
	{"committer Vicent Marti <> 123456 -0100 \n", "committer ", "Vicent Marti", "", 123456, -60},
	// Parse a signature with an empty email field
	{"committer Vicent Marti < > 123456 -0100 \n", "committer ", "Vicent Marti", "", 123456, -60},
	// Parse a signature with empty name and email
	{"committer <> 123456 -0100 \n", "committer ", "", "", 123456, -60},
	// Parse a signature with empty name and email
	{"committer  <> 123456 -0100 \n", "committer ", "", "", 123456, -60},
	// Parse a signature with empty name and email
	{"committer  < > 123456 -0100 \n", "committer ", "", "", 123456, -60},
	// Parse an obviously invalid signature
	{"committer foo<@bar> 123456 -0100 \n", "committer ", "foo", "@bar", 123456, -60},
	// Parse an obviously invalid signature
	{"committer    foo<@bar> 123456 -0100 \n", "committer ", "foo", "@bar", 123456, -60},
	// Parse an obviously invalid signature
	{"committer <>\n", "committer ", "", "", 0, 0},
	{"committer Vicent Marti <tanoku@gmail.com> 123456 -1500 \n", "committer ", "Vicent Marti", "tanoku@gmail.com", 123456, 0},
	{"committer Vicent Marti <tanoku@gmail.com> 123456 +0163 \n", "committer ", "Vicent Marti", "tanoku@gmail.com", 123456, 0},
	{"author Vicent Marti <tanoku@gmail.com>\n", "author ", "Vicent Marti", "tanoku@gmail.com", 0, 0},
	/* a variety of dates */
	{"author Vicent Marti <tanoku@gmail.com> 0 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 0, 0},
	{"author Vicent Marti <tanoku@gmail.com> 1234567890 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 1234567890, 0},
	{"author Vicent Marti <tanoku@gmail.com> 2147483647 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 0x7fffffff, 0},
	{"author Vicent Marti <tanoku@gmail.com> 4294967295 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 0xffffffff, 0},
	{"author Vicent Marti <tanoku@gmail.com> 4294967296 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 4294967296, 0},
	{"author Vicent Marti <tanoku@gmail.com> 8589934592 \n", "author ", "Vicent Marti", "tanoku@gmail.com", 8589934592, 0},

	{NULL,NULL,NULL,NULL,0,0}
};

typedef struct {
	const char *string;
	const char *header;
} failing_signature_test_case;

failing_signature_test_case failing_signature_cases[] = {
	{"committer Vicent Marti tanoku@gmail.com> 123456 -0100 \n", "committer "},
	{"author Vicent Marti <tanoku@gmail.com> 12345 \n", "author  "},
	{"author Vicent Marti <tanoku@gmail.com> 12345 \n", "committer "},
	{"author Vicent Marti 12345 \n", "author "},
	{"author Vicent Marti <broken@email 12345 \n", "author "},
	{"committer Vicent Marti ><\n", "committer "},
	{"author ", "author "},
	{NULL, NULL,}
};

void test_commit_parse__signature(void)
{
	passing_signature_test_case *passcase;
	failing_signature_test_case *failcase;

	for (passcase = passing_signature_cases; passcase->string != NULL; passcase++)
	{
		const char *str = passcase->string;
		size_t len = strlen(passcase->string);
		struct git_signature person = {0};

		cl_git_pass(git_signature__parse(&person, &str, str + len, passcase->header, '\n'));
		cl_assert_equal_s(passcase->name, person.name);
		cl_assert_equal_s(passcase->email, person.email);
		cl_assert_equal_i((int)passcase->time, (int)person.when.time);
		cl_assert_equal_i(passcase->offset, person.when.offset);
		git__free(person.name); git__free(person.email);
	}

	for (failcase = failing_signature_cases; failcase->string != NULL; failcase++)
	{
		const char *str = failcase->string;
		size_t len = strlen(failcase->string);
		git_signature person = {0};
		cl_git_fail(git_signature__parse(&person, &str, str + len, failcase->header, '\n'));
		git__free(person.name); git__free(person.email);
	}
}



static char *failing_commit_cases[] = {
// empty commit
"",
// random garbage
"asd97sa9du902e9a0jdsuusad09as9du098709aweu8987sd\n",
// broken endlines 1
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\r\n\
parent 05452d6349abcd67aa396dfb28660d765d8b2a36\r\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\r\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\r\n\
\r\n\
a test commit with broken endlines\r\n",
// broken endlines 2
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\
parent 05452d6349abcd67aa396dfb28660d765d8b2a36\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\
\
another test commit with broken endlines",
// starting endlines
"\ntree f6c0dad3c7b3481caa9d73db21f91964894a945b\n\
parent 05452d6349abcd67aa396dfb28660d765d8b2a36\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
a test commit with a starting endline\n",
// corrupted commit 1
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\n\
parent 05452d6349abcd67aa396df",
// corrupted commit 2
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\n\
parent ",
// corrupted commit 3
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\n\
parent ",
// corrupted commit 4
"tree f6c0dad3c7b3481caa9d73db21f91964894a945b\n\
par",
};


static char *passing_commit_cases[] = {
// simple commit with no message
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n",
// simple commit, no parent
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
a simple commit which works\n",
// simple commit, no parent, no newline in message
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
a simple commit which works",
// simple commit, 1 parent
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
parent e90810b8df3e80c413d903f631643c716887138d\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
a simple commit which works\n",
/* simple commit with GPG signature */
"tree 6b79e22d69bf46e289df0345a14ca059dfc9bdf6\n\
parent 34734e478d6cf50c27c9d69026d93974d052c454\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
gpgsig -----BEGIN PGP SIGNATURE-----\n\
 Version: GnuPG v1.4.12 (Darwin)\n\
 \n\
 iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al\n\
 o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8\n\
 JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq\n\
 AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq\n\
 SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW\n\
 who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok\n\
 6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG\n\
 cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu\n\
 c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9\n\
 ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J\n\
 7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc\n\
 cpxtDQQMGYFpXK/71stq\n\
 =ozeK\n\
 -----END PGP SIGNATURE-----\n\
\n\
a simple commit which works\n",
/* some tools create two author entries */
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
author Helpful Coworker <helpful@coworker> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
a simple commit which works",
};

static int parse_commit(git_commit **out, const char *buffer)
{
	git_commit *commit;
	git_odb_object fake_odb_object;
	int error;

	commit = (git_commit*)git__malloc(sizeof(git_commit));
	memset(commit, 0x0, sizeof(git_commit));
	commit->object.repo = g_repo;

	memset(&fake_odb_object, 0x0, sizeof(git_odb_object));
	fake_odb_object.buffer = (char *)buffer;
	fake_odb_object.cached.size = strlen(fake_odb_object.buffer);

	error = git_commit__parse(commit, &fake_odb_object);

	*out = commit;
	return error;
}

void test_commit_parse__entire_commit(void)
{
	const int failing_commit_count = ARRAY_SIZE(failing_commit_cases);
	const int passing_commit_count = ARRAY_SIZE(passing_commit_cases);
	int i;
	git_commit *commit;

	for (i = 0; i < failing_commit_count; ++i) {
		cl_git_fail(parse_commit(&commit, failing_commit_cases[i]));
		git_commit__free(commit);
	}

	for (i = 0; i < passing_commit_count; ++i) {
		cl_git_pass(parse_commit(&commit, passing_commit_cases[i]));

		if (!i)
			cl_assert_equal_s("", git_commit_message(commit));
		else
			cl_assert(git__prefixcmp(
				git_commit_message(commit), "a simple commit which works") == 0);

		git_commit__free(commit);
	}
}


// query the details on a parsed commit
void test_commit_parse__details0(void) {
	static const char *commit_ids[] = {
		"a4a7dce85cf63874e984719f4fdd239f5145052f", /* 0 */
		"9fd738e8f7967c078dceed8190330fc8648ee56a", /* 1 */
		"4a202b346bb0fb0db7eff3cffeb3c70babbd2045", /* 2 */
		"c47800c7266a2be04c571c04d5a6614691ea99bd", /* 3 */
		"8496071c1b46c854b31185ea97743be6a8774479", /* 4 */
		"5b5b025afb0b4c913b4c338a42934a3863bf3644", /* 5 */
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", /* 6 */
	};
	const size_t commit_count = sizeof(commit_ids) / sizeof(const char *);
	unsigned int i;

	for (i = 0; i < commit_count; ++i) {
		git_oid id;
		git_commit *commit;

		const git_signature *author, *committer;
		const char *message;
		git_time_t commit_time;
		unsigned int parents, p;
		git_commit *parent = NULL, *old_parent = NULL;

		git_oid_fromstr(&id, commit_ids[i]);

		cl_git_pass(git_commit_lookup(&commit, g_repo, &id));

		message = git_commit_message(commit);
		author = git_commit_author(commit);
		committer = git_commit_committer(commit);
		commit_time = git_commit_time(commit);
		parents = git_commit_parentcount(commit);

		cl_assert_equal_s("Scott Chacon", author->name);
		cl_assert_equal_s("schacon@gmail.com", author->email);
		cl_assert_equal_s("Scott Chacon", committer->name);
		cl_assert_equal_s("schacon@gmail.com", committer->email);
		cl_assert(message != NULL);
		cl_assert(commit_time > 0);
		cl_assert(parents <= 2);
		for (p = 0;p < parents;p++) {
			if (old_parent != NULL)
				git_commit_free(old_parent);

			old_parent = parent;
			cl_git_pass(git_commit_parent(&parent, commit, p));
			cl_assert(parent != NULL);
			cl_assert(git_commit_author(parent) != NULL); // is it really a commit?
		}
		git_commit_free(old_parent);
		git_commit_free(parent);

		cl_git_fail(git_commit_parent(&parent, commit, parents));
		git_commit_free(commit);
	}
}

void test_commit_parse__leading_lf(void)
{
	git_commit *commit;
	const char *buffer =
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
parent e90810b8df3e80c413d903f631643c716887138d\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
\n\
\n\
This commit has a few LF at the start of the commit message";
	const char *message =
"This commit has a few LF at the start of the commit message";
	const char *raw_message =
"\n\
\n\
This commit has a few LF at the start of the commit message";
	cl_git_pass(parse_commit(&commit, buffer));
	cl_assert_equal_s(message, git_commit_message(commit));
	cl_assert_equal_s(raw_message, git_commit_message_raw(commit));
	git_commit__free(commit);
}

void test_commit_parse__only_lf(void)
{
	git_commit *commit;
	const char *buffer =
"tree 1810dff58d8a660512d4832e740f692884338ccd\n\
parent e90810b8df3e80c413d903f631643c716887138d\n\
author Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
committer Vicent Marti <tanoku@gmail.com> 1273848544 +0200\n\
\n\
\n\
\n";
	const char *message = "";
	const char *raw_message = "\n\n";

	cl_git_pass(parse_commit(&commit, buffer));
	cl_assert_equal_s(message, git_commit_message(commit));
	cl_assert_equal_s(raw_message, git_commit_message_raw(commit));
	git_commit__free(commit);
}

void test_commit_parse__arbitrary_field(void)
{
	git_commit *commit;
	git_buf buf = GIT_BUF_INIT;
	const char *gpgsig = "-----BEGIN PGP SIGNATURE-----\n\
Version: GnuPG v1.4.12 (Darwin)\n\
\n\
iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al\n\
o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8\n\
JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq\n\
AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq\n\
SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW\n\
who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok\n\
6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG\n\
cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu\n\
c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9\n\
ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J\n\
7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc\n\
cpxtDQQMGYFpXK/71stq\n\
=ozeK\n\
-----END PGP SIGNATURE-----";

	cl_git_pass(parse_commit(&commit, passing_commit_cases[4]));

	cl_git_pass(git_commit_header_field(&buf, commit, "tree"));
	cl_assert_equal_s("6b79e22d69bf46e289df0345a14ca059dfc9bdf6", buf.ptr);
	git_buf_clear(&buf);

	cl_git_pass(git_commit_header_field(&buf, commit, "parent"));
	cl_assert_equal_s("34734e478d6cf50c27c9d69026d93974d052c454", buf.ptr);
	git_buf_clear(&buf);

	cl_git_pass(git_commit_header_field(&buf, commit, "gpgsig"));
	cl_assert_equal_s(gpgsig, buf.ptr);
	git_buf_clear(&buf);

	cl_git_fail_with(GIT_ENOTFOUND, git_commit_header_field(&buf, commit, "awesomeness"));
	cl_git_fail_with(GIT_ENOTFOUND, git_commit_header_field(&buf, commit, "par"));

	git_commit__free(commit);
	cl_git_pass(parse_commit(&commit, passing_commit_cases[0]));

	cl_git_pass(git_commit_header_field(&buf, commit, "committer"));
	cl_assert_equal_s("Vicent Marti <tanoku@gmail.com> 1273848544 +0200", buf.ptr);

	git_buf_free(&buf);
	git_commit__free(commit);
}

void test_commit_parse__extract_signature(void)
{
	git_odb *odb;
	git_oid commit_id;
	git_buf signature = GIT_BUF_INIT, signed_data = GIT_BUF_INIT;
	const char *gpgsig = "-----BEGIN PGP SIGNATURE-----\n\
Version: GnuPG v1.4.12 (Darwin)\n\
\n\
iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al\n\
o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8\n\
JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq\n\
AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq\n\
SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW\n\
who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok\n\
6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG\n\
cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu\n\
c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9\n\
ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J\n\
7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc\n\
cpxtDQQMGYFpXK/71stq\n\
=ozeK\n\
-----END PGP SIGNATURE-----";

	const char *data =  "tree 6b79e22d69bf46e289df0345a14ca059dfc9bdf6\n\
parent 34734e478d6cf50c27c9d69026d93974d052c454\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
\n\
a simple commit which works\n";

	const char *oneline_signature = "tree 51832e6397b30309c8bcad9c55fa6ae67778f378\n\
parent a1b6decaaac768b5e01e1b5dbf5b2cc081bed1eb\n\
author Some User <someuser@gmail.com> 1454537944 -0700\n\
committer Some User <someuser@gmail.com> 1454537944 -0700\n\
gpgsig bad\n\
\n\
corrupt signature\n";

	const char *oneline_data = "tree 51832e6397b30309c8bcad9c55fa6ae67778f378\n\
parent a1b6decaaac768b5e01e1b5dbf5b2cc081bed1eb\n\
author Some User <someuser@gmail.com> 1454537944 -0700\n\
committer Some User <someuser@gmail.com> 1454537944 -0700\n\
\n\
corrupt signature\n";


	cl_git_pass(git_repository_odb__weakptr(&odb, g_repo));
	cl_git_pass(git_odb_write(&commit_id, odb, passing_commit_cases[4], strlen(passing_commit_cases[4]), GIT_OBJ_COMMIT));

	cl_git_pass(git_commit_extract_signature(&signature, &signed_data, g_repo, &commit_id, NULL));
	cl_assert_equal_s(gpgsig, signature.ptr);
	cl_assert_equal_s(data, signed_data.ptr);

	git_buf_clear(&signature);
	git_buf_clear(&signed_data);

	cl_git_pass(git_commit_extract_signature(&signature, &signed_data, g_repo, &commit_id, "gpgsig"));
	cl_assert_equal_s(gpgsig, signature.ptr);
	cl_assert_equal_s(data, signed_data.ptr);

	/* Try to parse a tree */
	cl_git_pass(git_oid_fromstr(&commit_id, "45dd856fdd4d89b884c340ba0e047752d9b085d6"));
	cl_git_fail_with(GIT_ENOTFOUND, git_commit_extract_signature(&signature, &signed_data, g_repo, &commit_id, NULL));
	cl_assert_equal_i(GITERR_INVALID, giterr_last()->klass);

	/* Try to parse an unsigned commit */
	cl_git_pass(git_odb_write(&commit_id, odb, passing_commit_cases[1], strlen(passing_commit_cases[1]), GIT_OBJ_COMMIT));
	cl_git_fail_with(GIT_ENOTFOUND, git_commit_extract_signature(&signature, &signed_data, g_repo, &commit_id, NULL));
	cl_assert_equal_i(GITERR_OBJECT, giterr_last()->klass);

	/* Parse the commit with a single-line signature */
	git_buf_clear(&signature);
	git_buf_clear(&signed_data);
	cl_git_pass(git_odb_write(&commit_id, odb, oneline_signature, strlen(oneline_signature), GIT_OBJ_COMMIT));
	cl_git_pass(git_commit_extract_signature(&signature, &signed_data, g_repo, &commit_id, NULL));
	cl_assert_equal_s("bad", signature.ptr);
	cl_assert_equal_s(oneline_data, signed_data.ptr);


	git_buf_free(&signature);
	git_buf_free(&signed_data);

}
