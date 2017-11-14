#include "clar_libgit2.h"

#include "tag.h"

static const char *tag1_id = "b25fa35b38051e4ae45d4222e795f9df2e43f1d1";
static const char *tag2_id = "7b4384978d2493e851f9cca7858815fac9b10980";
static const char *tagged_commit = "e90810b8df3e80c413d903f631643c716887138d";
static const char *bad_tag_id = "eda9f45a2a98d4c17a09d681d88569fa4ea91755";
static const char *badly_tagged_commit = "e90810b8df3e80c413d903f631643c716887138d";
static const char *short_tag_id = "5da7760512a953e3c7c4e47e4392c7a4338fb729";
static const char *short_tagged_commit = "4a5ed60bafcf4638b7c8356bd4ce1916bfede93c";
static const char *taggerless = "4a23e2e65ad4e31c4c9db7dc746650bfad082679";

static git_repository *g_repo;

// Fixture setup and teardown
void test_object_tag_read__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tag_read__cleanup(void)
{
	cl_git_sandbox_cleanup();
}


void test_object_tag_read__parse(void)
{
	// read and parse a tag from the repository
	git_tag *tag1, *tag2;
	git_commit *commit;
	git_oid id1, id2, id_commit;

	git_oid_fromstr(&id1, tag1_id);
	git_oid_fromstr(&id2, tag2_id);
	git_oid_fromstr(&id_commit, tagged_commit);

	cl_git_pass(git_tag_lookup(&tag1, g_repo, &id1));

	cl_assert_equal_s(git_tag_name(tag1), "test");
	cl_assert(git_tag_target_type(tag1) == GIT_OBJ_TAG);

	cl_git_pass(git_tag_target((git_object **)&tag2, tag1));
	cl_assert(tag2 != NULL);

	cl_assert(git_oid_cmp(&id2, git_tag_id(tag2)) == 0);

	cl_git_pass(git_tag_target((git_object **)&commit, tag2));
	cl_assert(commit != NULL);

	cl_assert(git_oid_cmp(&id_commit, git_commit_id(commit)) == 0);

	git_tag_free(tag1);
	git_tag_free(tag2);
	git_commit_free(commit);
}

void test_object_tag_read__parse_without_tagger(void)
{
	// read and parse a tag without a tagger field
	git_repository *bad_tag_repo;
	git_tag *bad_tag;
	git_commit *commit;
	git_oid id, id_commit;

	// TODO: This is a little messy
	cl_git_pass(git_repository_open(&bad_tag_repo, cl_fixture("bad_tag.git")));

	git_oid_fromstr(&id, bad_tag_id);
	git_oid_fromstr(&id_commit, badly_tagged_commit);

	cl_git_pass(git_tag_lookup(&bad_tag, bad_tag_repo, &id));
	cl_assert(bad_tag != NULL);

	cl_assert_equal_s(git_tag_name(bad_tag), "e90810b");
	cl_assert(git_oid_cmp(&id, git_tag_id(bad_tag)) == 0);
	cl_assert(bad_tag->tagger == NULL);

	cl_git_pass(git_tag_target((git_object **)&commit, bad_tag));
	cl_assert(commit != NULL);

	cl_assert(git_oid_cmp(&id_commit, git_commit_id(commit)) == 0);


	git_tag_free(bad_tag);
	git_commit_free(commit);
	git_repository_free(bad_tag_repo);
}

void test_object_tag_read__parse_without_message(void)
{
	// read and parse a tag without a message field
	git_repository *short_tag_repo;
	git_tag *short_tag;
	git_commit *commit;
	git_oid id, id_commit;

	// TODO: This is a little messy
	cl_git_pass(git_repository_open(&short_tag_repo, cl_fixture("short_tag.git")));

	git_oid_fromstr(&id, short_tag_id);
	git_oid_fromstr(&id_commit, short_tagged_commit);

	cl_git_pass(git_tag_lookup(&short_tag, short_tag_repo, &id));
	cl_assert(short_tag != NULL);

	cl_assert_equal_s(git_tag_name(short_tag), "no_description");
	cl_assert(git_oid_cmp(&id, git_tag_id(short_tag)) == 0);
	cl_assert(short_tag->message == NULL);

	cl_git_pass(git_tag_target((git_object **)&commit, short_tag));
	cl_assert(commit != NULL);

	cl_assert(git_oid_cmp(&id_commit, git_commit_id(commit)) == 0);

	git_tag_free(short_tag);
	git_commit_free(commit);
	git_repository_free(short_tag_repo);
}

void test_object_tag_read__without_tagger_nor_message(void)
{
	git_tag *tag;
	git_oid id;
	git_repository *repo;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_oid_fromstr(&id, taggerless));

	cl_git_pass(git_tag_lookup(&tag, repo, &id));

	cl_assert_equal_s(git_tag_name(tag), "taggerless");
	cl_assert(git_tag_target_type(tag) == GIT_OBJ_COMMIT);

	cl_assert(tag->message == NULL);
	cl_assert(tag->tagger == NULL);

	git_tag_free(tag);
	git_repository_free(repo);
}

static const char *silly_tag = "object c054ccaefbf2da31c3b19178f9e3ef20a3867924\n\
type commit\n\
tag v1_0_1\n\
tagger Jamis Buck <jamis@37signals.com> 1107717917\n\
diff --git a/lib/sqlite3/version.rb b/lib/sqlite3/version.rb\n\
index 0b3bf69..4ee8fc2 100644\n\
--- a/lib/sqlite3/version.rb\n\
+++ b/lib/sqlite3/version.rb\n\
@@ -36,7 +36,7 @@ module SQLite3\n\
 \n\
     MAJOR = 1\n\
     MINOR = 0\n\
-    TINY  = 0\n\
+    TINY  = 1\n\
 \n\
     STRING = [ MAJOR, MINOR, TINY ].join( \".\" )\n\
 \n\
 -0600\n\
\n\
v1_0_1 release\n";

void test_object_tag_read__extra_header_fields(void)
{
	git_tag *tag;
	git_odb *odb;
	git_oid id;

	cl_git_pass(git_repository_odb__weakptr(&odb, g_repo));

	cl_git_pass(git_odb_write(&id, odb, silly_tag, strlen(silly_tag), GIT_OBJ_TAG));
	cl_git_pass(git_tag_lookup(&tag, g_repo, &id));

	cl_assert_equal_s("v1_0_1 release\n", git_tag_message(tag));

	git_tag_free(tag);
}
