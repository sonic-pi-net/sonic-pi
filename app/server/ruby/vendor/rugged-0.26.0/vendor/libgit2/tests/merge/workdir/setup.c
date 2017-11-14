#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "refs.h"
#include "fileops.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH		"merge-resolve"
#define TEST_INDEX_PATH TEST_REPO_PATH	"/.git/index"

#define ORIG_HEAD			"bd593285fc7fe4ca18ccdbabf027f5d689101452"

#define THEIRS_SIMPLE_BRANCH	"branch"
#define THEIRS_SIMPLE_OID	"7cb63eed597130ba4abb87b3e544b85021905520"

#define OCTO1_BRANCH		"octo1"
#define OCTO1_OID			"16f825815cfd20a07a75c71554e82d8eede0b061"

#define OCTO2_BRANCH		"octo2"
#define OCTO2_OID			"158dc7bedb202f5b26502bf3574faa7f4238d56c"

#define OCTO3_BRANCH		"octo3"
#define OCTO3_OID			"50ce7d7d01217679e26c55939eef119e0c93e272"

#define OCTO4_BRANCH		"octo4"
#define OCTO4_OID			"54269b3f6ec3d7d4ede24dd350dd5d605495c3ae"

#define OCTO5_BRANCH		"octo5"
#define OCTO5_OID			"e4f618a2c3ed0669308735727df5ebf2447f022f"

// Fixture setup and teardown
void test_merge_workdir_setup__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);
}

void test_merge_workdir_setup__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

static bool test_file_contents(const char *filename, const char *expected)
{
	git_buf file_path_buf = GIT_BUF_INIT, file_buf = GIT_BUF_INIT;
	bool equals;
	
	git_buf_printf(&file_path_buf, "%s/%s", git_repository_path(repo), filename);
	
	cl_git_pass(git_futils_readbuffer(&file_buf, file_path_buf.ptr));
	equals = (strcmp(file_buf.ptr, expected) == 0);

	git_buf_free(&file_path_buf);
	git_buf_free(&file_buf);
	
	return equals;
}

static void write_file_contents(const char *filename, const char *output)
{
	git_buf file_path_buf = GIT_BUF_INIT;

	git_buf_printf(&file_path_buf, "%s/%s", git_repository_path(repo),
		filename);
	cl_git_rewritefile(file_path_buf.ptr, output);

	git_buf_free(&file_path_buf);
}

/* git merge --no-ff octo1 */
void test_merge_workdir_setup__one_branch(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 1));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO1_BRANCH "'\n"));

	git_reference_free(octo1_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

/* git merge --no-ff 16f825815cfd20a07a75c71554e82d8eede0b061 */
void test_merge_workdir_setup__one_oid(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_annotated_commit *our_head, *their_heads[1];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &octo1_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 1));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge commit '" OCTO1_OID "'\n"));

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

/* git merge octo1 octo2 */
void test_merge_workdir_setup__two_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_annotated_commit *our_head, *their_heads[2];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "' and '" OCTO2_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git merge octo1 octo2 octo3 */
void test_merge_workdir_setup__three_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_reference *octo3_ref;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));
	
	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_reference_lookup(&octo3_ref, repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[2], repo, octo3_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "', '" OCTO2_BRANCH "' and '" OCTO3_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	git_reference_free(octo3_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

/* git merge 16f825815cfd20a07a75c71554e82d8eede0b061 158dc7bedb202f5b26502bf3574faa7f4238d56c 50ce7d7d01217679e26c55939eef119e0c93e272 */
void test_merge_workdir_setup__three_oids(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_oid octo2_oid;
	git_oid octo3_oid;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &octo1_oid));
	
	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[1], repo, &octo2_oid));

	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[2], repo, &octo3_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge commit '" OCTO1_OID "'; commit '" OCTO2_OID "'; commit '" OCTO3_OID "'\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

/* git merge octo1 158dc7bedb202f5b26502bf3574faa7f4238d56c */
void test_merge_workdir_setup__branches_and_oids_1(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_oid octo2_oid;
	git_annotated_commit *our_head, *their_heads[2];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[1], repo, &octo2_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO1_BRANCH "'; commit '" OCTO2_OID "'\n"));
	
	git_reference_free(octo1_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git merge octo1 158dc7bedb202f5b26502bf3574faa7f4238d56c octo3 54269b3f6ec3d7d4ede24dd350dd5d605495c3ae */
void test_merge_workdir_setup__branches_and_oids_2(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_oid octo2_oid;
	git_reference *octo3_ref;
	git_oid octo4_oid;
	git_annotated_commit *our_head, *their_heads[4];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));
	
	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[1], repo, &octo2_oid));

	cl_git_pass(git_reference_lookup(&octo3_ref, repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[2], repo, octo3_ref));
	
	cl_git_pass(git_oid_fromstr(&octo4_oid, OCTO4_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[3], repo, &octo4_oid));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 4));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n" OCTO4_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "' and '" OCTO3_BRANCH "'; commit '" OCTO2_OID "'; commit '" OCTO4_OID "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo3_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
	git_annotated_commit_free(their_heads[3]);
}

/* git merge 16f825815cfd20a07a75c71554e82d8eede0b061 octo2 50ce7d7d01217679e26c55939eef119e0c93e272 octo4 */
void test_merge_workdir_setup__branches_and_oids_3(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_reference *octo2_ref;
	git_oid octo3_oid;
	git_reference *octo4_ref;
	git_annotated_commit *our_head, *their_heads[4];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &octo1_oid));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[2], repo, &octo3_oid));
	
	cl_git_pass(git_reference_lookup(&octo4_ref, repo, GIT_REFS_HEADS_DIR OCTO4_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[3], repo, octo4_ref));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 4));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n" OCTO4_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge commit '" OCTO1_OID "'; branches '" OCTO2_BRANCH "' and '" OCTO4_BRANCH "'; commit '" OCTO3_OID "'\n"));
	
	git_reference_free(octo2_ref);
	git_reference_free(octo4_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
	git_annotated_commit_free(their_heads[3]);
}

/* git merge 16f825815cfd20a07a75c71554e82d8eede0b061 octo2 50ce7d7d01217679e26c55939eef119e0c93e272 octo4 octo5 */
void test_merge_workdir_setup__branches_and_oids_4(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_reference *octo2_ref;
	git_oid octo3_oid;
	git_reference *octo4_ref;
	git_reference *octo5_ref;
	git_annotated_commit *our_head, *their_heads[5];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &octo1_oid));
	
	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));
	
	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[2], repo, &octo3_oid));
	
	cl_git_pass(git_reference_lookup(&octo4_ref, repo, GIT_REFS_HEADS_DIR OCTO4_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[3], repo, octo4_ref));

	cl_git_pass(git_reference_lookup(&octo5_ref, repo, GIT_REFS_HEADS_DIR OCTO5_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[4], repo, octo5_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 5));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n" OCTO4_OID "\n" OCTO5_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge commit '" OCTO1_OID "'; branches '" OCTO2_BRANCH "', '" OCTO4_BRANCH "' and '" OCTO5_BRANCH "'; commit '" OCTO3_OID "'\n"));
	
	git_reference_free(octo2_ref);
	git_reference_free(octo4_ref);
	git_reference_free(octo5_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
	git_annotated_commit_free(their_heads[3]);
	git_annotated_commit_free(their_heads[4]);
}

/* git merge octo1 octo1 octo1 */
void test_merge_workdir_setup__three_same_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_1_ref;
	git_reference *octo1_2_ref;
	git_reference *octo1_3_ref;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_1_ref));
	
	cl_git_pass(git_reference_lookup(&octo1_2_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo1_2_ref));
	
	cl_git_pass(git_reference_lookup(&octo1_3_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[2], repo, octo1_3_ref));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO1_OID "\n" OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "', '" OCTO1_BRANCH "' and '" OCTO1_BRANCH "'\n"));
	
	git_reference_free(octo1_1_ref);
	git_reference_free(octo1_2_ref);
	git_reference_free(octo1_3_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

/* git merge 16f825815cfd20a07a75c71554e82d8eede0b061 16f825815cfd20a07a75c71554e82d8eede0b061 16f825815cfd20a07a75c71554e82d8eede0b061 */
void test_merge_workdir_setup__three_same_oids(void)
{
	git_oid our_oid;
	git_oid octo1_1_oid;
	git_oid octo1_2_oid;
	git_oid octo1_3_oid;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_oid_fromstr(&octo1_1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &octo1_1_oid));
	
	cl_git_pass(git_oid_fromstr(&octo1_2_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[1], repo, &octo1_2_oid));
	
	cl_git_pass(git_oid_fromstr(&octo1_3_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[2], repo, &octo1_3_oid));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO1_OID "\n" OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge commit '" OCTO1_OID "'; commit '" OCTO1_OID "'; commit '" OCTO1_OID "'\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

static int create_remote_tracking_branch(const char *branch_name, const char *oid_str)
{
	int error = 0;

	git_buf remotes_path = GIT_BUF_INIT,
		origin_path = GIT_BUF_INIT,
		filename = GIT_BUF_INIT,
		data = GIT_BUF_INIT;

	if ((error = git_buf_puts(&remotes_path, git_repository_path(repo))) < 0 ||
		(error = git_buf_puts(&remotes_path, GIT_REFS_REMOTES_DIR)) < 0)
		goto done;

	if (!git_path_exists(git_buf_cstr(&remotes_path)) &&
		(error = p_mkdir(git_buf_cstr(&remotes_path), 0777)) < 0)
		goto done;

	if ((error = git_buf_puts(&origin_path, git_buf_cstr(&remotes_path))) < 0 ||
		(error = git_buf_puts(&origin_path, "origin")) < 0)
		goto done;

	if (!git_path_exists(git_buf_cstr(&origin_path)) &&
		(error = p_mkdir(git_buf_cstr(&origin_path), 0777)) < 0)
		goto done;

	if ((error = git_buf_puts(&filename, git_buf_cstr(&origin_path))) < 0 ||
		(error = git_buf_puts(&filename, "/")) < 0 ||
		(error = git_buf_puts(&filename, branch_name)) < 0 ||
		(error = git_buf_puts(&data, oid_str)) < 0 ||
		(error = git_buf_puts(&data, "\n")) < 0)
		goto done;

	cl_git_rewritefile(git_buf_cstr(&filename), git_buf_cstr(&data));

done:
	git_buf_free(&remotes_path);
	git_buf_free(&origin_path);
	git_buf_free(&filename);
	git_buf_free(&data);

	return error;
}

/* git merge refs/remotes/origin/octo1 */
void test_merge_workdir_setup__remote_tracking_one_branch(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(create_remote_tracking_branch(OCTO1_BRANCH, OCTO1_OID));

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 1));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge remote-tracking branch 'refs/remotes/origin/" OCTO1_BRANCH "'\n"));

	git_reference_free(octo1_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

/* git merge refs/remotes/origin/octo1 refs/remotes/origin/octo2 */
void test_merge_workdir_setup__remote_tracking_two_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_annotated_commit *our_head, *their_heads[2];

	cl_git_pass(create_remote_tracking_branch(OCTO1_BRANCH, OCTO1_OID));
	cl_git_pass(create_remote_tracking_branch(OCTO2_BRANCH, OCTO2_OID));

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge remote-tracking branches 'refs/remotes/origin/" OCTO1_BRANCH "' and 'refs/remotes/origin/" OCTO2_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git merge refs/remotes/origin/octo1 refs/remotes/origin/octo2 refs/remotes/origin/octo3 */
void test_merge_workdir_setup__remote_tracking_three_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_reference *octo3_ref;
	git_annotated_commit *our_head, *their_heads[3];

	cl_git_pass(create_remote_tracking_branch(OCTO1_BRANCH, OCTO1_OID));
	cl_git_pass(create_remote_tracking_branch(OCTO2_BRANCH, OCTO2_OID));
	cl_git_pass(create_remote_tracking_branch(OCTO3_BRANCH, OCTO3_OID));
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));
	
	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_reference_lookup(&octo3_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO3_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[2], repo, octo3_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge remote-tracking branches 'refs/remotes/origin/" OCTO1_BRANCH "', 'refs/remotes/origin/" OCTO2_BRANCH "' and 'refs/remotes/origin/" OCTO3_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	git_reference_free(octo3_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

/* git merge octo1 refs/remotes/origin/octo2 */
void test_merge_workdir_setup__normal_branch_and_remote_tracking_branch(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_annotated_commit *our_head, *their_heads[2];

	cl_git_pass(create_remote_tracking_branch(OCTO2_BRANCH, OCTO2_OID));

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO1_BRANCH "', remote-tracking branch 'refs/remotes/origin/" OCTO2_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git merge refs/remotes/origin/octo1 octo2 */
void test_merge_workdir_setup__remote_tracking_branch_and_normal_branch(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_annotated_commit *our_head, *their_heads[2];

	cl_git_pass(create_remote_tracking_branch(OCTO1_BRANCH, OCTO1_OID));

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO2_BRANCH "', remote-tracking branch 'refs/remotes/origin/" OCTO1_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git merge octo1 refs/remotes/origin/octo2 octo3 refs/remotes/origin/octo4 */
void test_merge_workdir_setup__two_remote_tracking_branch_and_two_normal_branches(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_reference *octo2_ref;
	git_reference *octo3_ref;
	git_reference *octo4_ref;
	git_annotated_commit *our_head, *their_heads[4];

	cl_git_pass(create_remote_tracking_branch(OCTO2_BRANCH, OCTO2_OID));
	cl_git_pass(create_remote_tracking_branch(OCTO4_BRANCH, OCTO4_OID));

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));
	
	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_reference_lookup(&octo2_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO2_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[1], repo, octo2_ref));

	cl_git_pass(git_reference_lookup(&octo3_ref, repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[2], repo, octo3_ref));

	cl_git_pass(git_reference_lookup(&octo4_ref, repo, GIT_REFS_REMOTES_DIR "origin/" OCTO4_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[3], repo, octo4_ref));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 4));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n" OCTO4_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "' and '" OCTO3_BRANCH "', remote-tracking branches 'refs/remotes/origin/" OCTO2_BRANCH "' and 'refs/remotes/origin/" OCTO4_BRANCH "'\n"));
	
	git_reference_free(octo1_ref);
	git_reference_free(octo2_ref);
	git_reference_free(octo3_ref);
	git_reference_free(octo4_ref);
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
	git_annotated_commit_free(their_heads[3]);
}

/* git pull origin branch octo1 */
void test_merge_workdir_setup__pull_one(void)
{
	git_oid our_oid;
	git_oid octo1_1_oid;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.url/repo.git", &octo1_1_oid));
	
	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 1));
	
	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch 'octo1' of http://remote.url/repo.git\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

/* git pull origin octo1 octo2 */
void test_merge_workdir_setup__pull_two(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_oid octo2_oid;
	git_annotated_commit *our_head, *their_heads[2];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.url/repo.git", &octo1_oid));

	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[1], repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH, "http://remote.url/repo.git", &octo2_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 2));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "' and '" OCTO2_BRANCH "' of http://remote.url/repo.git\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
}

/* git pull origin octo1 octo2 octo3 */
void test_merge_workdir_setup__pull_three(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_oid octo2_oid;
	git_oid octo3_oid;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.url/repo.git", &octo1_oid));

	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[1], repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH, "http://remote.url/repo.git", &octo2_oid));

	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[2], repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH, "http://remote.url/repo.git", &octo3_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "', '" OCTO2_BRANCH "' and '" OCTO3_BRANCH "' of http://remote.url/repo.git\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

void test_merge_workdir_setup__three_remotes(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_oid octo2_oid;
	git_oid octo3_oid;
	git_annotated_commit *our_head, *their_heads[3];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.first/repo.git", &octo1_oid));

	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[1], repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH, "http://remote.second/repo.git", &octo2_oid));

	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[2], repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH, "http://remote.third/repo.git", &octo3_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 3));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO1_BRANCH "' of http://remote.first/repo.git, branch '" OCTO2_BRANCH "' of http://remote.second/repo.git, branch '" OCTO3_BRANCH "' of http://remote.third/repo.git\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
}

void test_merge_workdir_setup__two_remotes(void)
{
	git_oid our_oid;
	git_oid octo1_oid;
	git_oid octo2_oid;
	git_oid octo3_oid;
	git_oid octo4_oid;
	git_annotated_commit *our_head, *their_heads[4];
	
	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_oid_fromstr(&octo1_oid, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.first/repo.git", &octo1_oid));

	cl_git_pass(git_oid_fromstr(&octo2_oid, OCTO2_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[1], repo, GIT_REFS_HEADS_DIR OCTO2_BRANCH, "http://remote.second/repo.git", &octo2_oid));

	cl_git_pass(git_oid_fromstr(&octo3_oid, OCTO3_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[2], repo, GIT_REFS_HEADS_DIR OCTO3_BRANCH, "http://remote.first/repo.git", &octo3_oid));

	cl_git_pass(git_oid_fromstr(&octo4_oid, OCTO4_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&their_heads[3], repo, GIT_REFS_HEADS_DIR OCTO4_BRANCH, "http://remote.second/repo.git", &octo4_oid));

	cl_git_pass(git_merge__setup(repo, our_head, (const git_annotated_commit **)their_heads, 4));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n" OCTO2_OID "\n" OCTO3_OID "\n" OCTO4_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branches '" OCTO1_BRANCH "' and '" OCTO3_BRANCH "' of http://remote.first/repo.git, branches '" OCTO2_BRANCH "' and '" OCTO4_BRANCH "' of http://remote.second/repo.git\n"));
	
	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
	git_annotated_commit_free(their_heads[1]);
	git_annotated_commit_free(their_heads[2]);
	git_annotated_commit_free(their_heads[3]);
}

void test_merge_workdir_setup__id_from_head(void)
{
	git_oid expected_id;
	const git_oid *id;
	git_reference *ref;
	git_annotated_commit *heads[3];

	cl_git_pass(git_oid_fromstr(&expected_id, OCTO1_OID));
	cl_git_pass(git_annotated_commit_from_fetchhead(&heads[0], repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH, "http://remote.url/repo.git", &expected_id));
	id = git_annotated_commit_id(heads[0]);
	cl_assert_equal_i(1, git_oid_equal(id, &expected_id));

	cl_git_pass(git_annotated_commit_lookup(&heads[1], repo, &expected_id));
	id = git_annotated_commit_id(heads[1]);
	cl_assert_equal_i(1, git_oid_equal(id, &expected_id));

	cl_git_pass(git_reference_lookup(&ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&heads[2], repo, ref));
	id = git_annotated_commit_id(heads[2]);
	cl_assert_equal_i(1, git_oid_equal(id, &expected_id));

	git_reference_free(ref);
	git_annotated_commit_free(heads[0]);
	git_annotated_commit_free(heads[1]);
	git_annotated_commit_free(heads[2]);
}

struct annotated_commit_cb_data {
	const char **oid_str;
	unsigned int len;

	unsigned int i;
};

static int annotated_commit_foreach_cb(const git_oid *oid, void *payload)
{
	git_oid expected_oid;
	struct annotated_commit_cb_data *cb_data = payload;

	git_oid_fromstr(&expected_oid, cb_data->oid_str[cb_data->i]);
	cl_assert(git_oid_cmp(&expected_oid, oid) == 0);
	cb_data->i++;
	return 0;
}

void test_merge_workdir_setup__head_notfound(void)
{
	int error;

	cl_git_fail((error = git_repository_mergehead_foreach(repo,
		annotated_commit_foreach_cb, NULL)));
	cl_assert(error == GIT_ENOTFOUND);
}

void test_merge_workdir_setup__head_invalid_oid(void)
{
	int error;

	write_file_contents(GIT_MERGE_HEAD_FILE, "invalid-oid\n");

	cl_git_fail((error = git_repository_mergehead_foreach(repo,
		annotated_commit_foreach_cb, NULL)));
	cl_assert(error == -1);
}

void test_merge_workdir_setup__head_foreach_nonewline(void)
{
	int error;

	write_file_contents(GIT_MERGE_HEAD_FILE, THEIRS_SIMPLE_OID);

	cl_git_fail((error = git_repository_mergehead_foreach(repo,
		annotated_commit_foreach_cb, NULL)));
	cl_assert(error == -1);
}

void test_merge_workdir_setup__head_foreach_one(void)
{
	const char *expected = THEIRS_SIMPLE_OID;

	struct annotated_commit_cb_data cb_data = { &expected, 1 };

	write_file_contents(GIT_MERGE_HEAD_FILE, THEIRS_SIMPLE_OID "\n");

	cl_git_pass(git_repository_mergehead_foreach(repo,
		annotated_commit_foreach_cb, &cb_data));

	cl_assert(cb_data.i == cb_data.len);
}

void test_merge_workdir_setup__head_foreach_octopus(void)
{
	const char *expected[] = { THEIRS_SIMPLE_OID,
		OCTO1_OID, OCTO2_OID, OCTO3_OID, OCTO4_OID, OCTO5_OID };

	struct annotated_commit_cb_data cb_data = { expected, 6 };

	write_file_contents(GIT_MERGE_HEAD_FILE,
		THEIRS_SIMPLE_OID "\n"
		OCTO1_OID "\n"
		OCTO2_OID "\n"
		OCTO3_OID "\n"
		OCTO4_OID "\n"
		OCTO5_OID "\n");

	cl_git_pass(git_repository_mergehead_foreach(repo,
		annotated_commit_foreach_cb, &cb_data));

	cl_assert(cb_data.i == cb_data.len);
}

void test_merge_workdir_setup__retained_after_success(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));

	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_merge(repo, (const git_annotated_commit **)&their_heads[0], 1, NULL, NULL));

	cl_assert(test_file_contents(GIT_MERGE_HEAD_FILE, OCTO1_OID "\n"));
	cl_assert(test_file_contents(GIT_ORIG_HEAD_FILE, ORIG_HEAD "\n"));
	cl_assert(test_file_contents(GIT_MERGE_MODE_FILE, "no-ff"));
	cl_assert(test_file_contents(GIT_MERGE_MSG_FILE, "Merge branch '" OCTO1_BRANCH "'\n"));

	git_reference_free(octo1_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}


void test_merge_workdir_setup__removed_after_failure(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_write2file("merge-resolve/.git/index.lock", "foo\n", 4, O_RDWR|O_CREAT, 0666);

	cl_git_fail(git_merge(
		repo, (const git_annotated_commit **)&their_heads[0], 1, NULL, NULL));

	cl_assert(!git_path_exists("merge-resolve/.git/" GIT_MERGE_HEAD_FILE));
	cl_assert(!git_path_exists("merge-resolve/.git/" GIT_MERGE_MODE_FILE));
	cl_assert(!git_path_exists("merge-resolve/.git/" GIT_MERGE_MSG_FILE));

	git_reference_free(octo1_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

void test_merge_workdir_setup__unlocked_after_success(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_pass(git_merge(
		repo, (const git_annotated_commit **)&their_heads[0], 1, NULL, NULL));

	cl_assert(!git_path_exists("merge-resolve/.git/index.lock"));

	git_reference_free(octo1_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}

void test_merge_workdir_setup__unlocked_after_conflict(void)
{
	git_oid our_oid;
	git_reference *octo1_ref;
	git_annotated_commit *our_head, *their_heads[1];

	cl_git_pass(git_oid_fromstr(&our_oid, ORIG_HEAD));
	cl_git_pass(git_annotated_commit_lookup(&our_head, repo, &our_oid));

	cl_git_pass(git_reference_lookup(&octo1_ref, repo, GIT_REFS_HEADS_DIR OCTO1_BRANCH));
	cl_git_pass(git_annotated_commit_from_ref(&their_heads[0], repo, octo1_ref));

	cl_git_rewritefile("merge-resolve/new-in-octo1.txt",
		"Conflicting file!\n\nMerge will fail!\n");

	cl_git_fail(git_merge(
		repo, (const git_annotated_commit **)&their_heads[0], 1, NULL, NULL));

	cl_assert(!git_path_exists("merge-resolve/.git/index.lock"));

	git_reference_free(octo1_ref);

	git_annotated_commit_free(our_head);
	git_annotated_commit_free(their_heads[0]);
}
