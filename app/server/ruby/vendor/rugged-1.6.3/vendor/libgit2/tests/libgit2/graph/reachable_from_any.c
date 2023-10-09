#include "clar_libgit2.h"

#include <git2.h>

#include "commit_graph.h"
#include "bitvec.h"
#include "vector.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"

void test_graph_reachable_from_any__initialize(void)
{
	git_oid oid;
	git_commit *commit;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);

	git_oid__fromstr(&oid, "539bd011c4822c560c1d17cab095006b7a10f707", GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_reset(repo, (git_object *)commit, GIT_RESET_HARD, NULL));
	git_commit_free(commit);
}

void test_graph_reachable_from_any__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_graph_reachable_from_any__returns_correct_result(void)
{
	git_object *branchA1, *branchA2, *branchB1, *branchB2, *branchC1, *branchC2, *branchH1,
			*branchH2;
	git_oid descendants[7];

	cl_git_pass(git_revparse_single(&branchA1, repo, "branchA-1"));
	cl_git_pass(git_revparse_single(&branchA2, repo, "branchA-2"));
	cl_git_pass(git_revparse_single(&branchB1, repo, "branchB-1"));
	cl_git_pass(git_revparse_single(&branchB2, repo, "branchB-2"));
	cl_git_pass(git_revparse_single(&branchC1, repo, "branchC-1"));
	cl_git_pass(git_revparse_single(&branchC2, repo, "branchC-2"));
	cl_git_pass(git_revparse_single(&branchH1, repo, "branchH-1"));
	cl_git_pass(git_revparse_single(&branchH2, repo, "branchH-2"));

	cl_assert_equal_i(
			git_graph_reachable_from_any(
					repo, git_object_id(branchH1), git_object_id(branchA1), 1),
			0);
	cl_assert_equal_i(
			git_graph_reachable_from_any(
					repo, git_object_id(branchH1), git_object_id(branchA2), 1),
			0);

	cl_git_pass(git_oid_cpy(&descendants[0], git_object_id(branchA1)));
	cl_git_pass(git_oid_cpy(&descendants[1], git_object_id(branchA2)));
	cl_git_pass(git_oid_cpy(&descendants[2], git_object_id(branchB1)));
	cl_git_pass(git_oid_cpy(&descendants[3], git_object_id(branchB2)));
	cl_git_pass(git_oid_cpy(&descendants[4], git_object_id(branchC1)));
	cl_git_pass(git_oid_cpy(&descendants[5], git_object_id(branchC2)));
	cl_git_pass(git_oid_cpy(&descendants[6], git_object_id(branchH2)));
	cl_assert_equal_i(
			git_graph_reachable_from_any(repo, git_object_id(branchH2), descendants, 6),
			0);
	cl_assert_equal_i(
			git_graph_reachable_from_any(repo, git_object_id(branchH2), descendants, 7),
			1);

	git_object_free(branchA1);
	git_object_free(branchA2);
	git_object_free(branchB1);
	git_object_free(branchB2);
	git_object_free(branchC1);
	git_object_free(branchC2);
	git_object_free(branchH1);
	git_object_free(branchH2);
}

struct exhaustive_state {
	git_odb *db;
	git_vector commits;
};

/** Get all commits from the repository. */
static int exhaustive_commits(const git_oid *id, void *payload)
{
	struct exhaustive_state *mc = (struct exhaustive_state *)payload;
	size_t header_len;
	git_object_t header_type;
	int error = 0;

	error = git_odb_read_header(&header_len, &header_type, mc->db, id);
	if (error < 0)
		return error;

	if (header_type == GIT_OBJECT_COMMIT) {
		git_commit *commit = NULL;

		cl_git_pass(git_commit_lookup(&commit, repo, id));
		cl_git_pass(git_vector_insert(&mc->commits, commit));
	}

	return 0;
}

/** Compare the `git_oid`s of two `git_commit` objects. */
static int commit_id_cmp(const void *a, const void *b)
{
	return git_oid_cmp(
			git_commit_id((const git_commit *)a), git_commit_id((const git_commit *)b));
}

/** Find a `git_commit` whose ID matches the provided `git_oid` key. */
static int id_commit_id_cmp(const void *key, const void *commit)
{
	return git_oid_cmp((const git_oid *)key, git_commit_id((const git_commit *)commit));
}

void test_graph_reachable_from_any__exhaustive(void)
{
	struct exhaustive_state mc = {
			.db = NULL,
			.commits = GIT_VECTOR_INIT,
	};
	size_t child_idx, commit_count;
	size_t n_descendants;
	git_commit *child_commit;
	git_bitvec reachable;

	cl_git_pass(git_repository_odb(&mc.db, repo));
	cl_git_pass(git_odb_foreach(mc.db, &exhaustive_commits, &mc));
	git_vector_set_cmp(&mc.commits, commit_id_cmp);
	git_vector_sort(&mc.commits);
	cl_git_pass(git_bitvec_init(
			&reachable,
			git_vector_length(&mc.commits) * git_vector_length(&mc.commits)));

	commit_count = git_vector_length(&mc.commits);
	git_vector_foreach (&mc.commits, child_idx, child_commit) {
		unsigned int parent_i;

		/* We treat each commit as being able to reach itself. */
		git_bitvec_set(&reachable, child_idx * commit_count + child_idx, true);

		for (parent_i = 0; parent_i < git_commit_parentcount(child_commit); ++parent_i) {
			size_t parent_idx = -1;
			cl_git_pass(git_vector_bsearch2(
					&parent_idx,
					&mc.commits,
					id_commit_id_cmp,
					git_commit_parent_id(child_commit, parent_i)));

			/* We have established that parent_idx is reachable from child_idx */
			git_bitvec_set(&reachable, parent_idx * commit_count + child_idx, true);
		}
	}

	/* Floyd-Warshall */
	{
		size_t i, j, k;
		for (k = 0; k < commit_count; ++k) {
			for (i = 0; i < commit_count; ++i) {
				if (!git_bitvec_get(&reachable, i * commit_count + k))
					continue;
				for (j = 0; j < commit_count; ++j) {
					if (!git_bitvec_get(&reachable, k * commit_count + j))
						continue;
					git_bitvec_set(&reachable, i * commit_count + j, true);
				}
			}
		}
	}

	/* Try 1000 subsets of 1 through 10 entries each. */
	srand(0x223ddc4b);
	for (n_descendants = 1; n_descendants < 10; ++n_descendants) {
		size_t test_iteration;
		git_oid descendants[10];

		for (test_iteration = 0; test_iteration < 1000; ++test_iteration) {
			size_t descendant_i;
			size_t child_idx, parent_idx;
			int expected_reachable = false, actual_reachable;
			git_commit *child_commit, *parent_commit;

			parent_idx = rand() % commit_count;
			parent_commit = (git_commit *)git_vector_get(&mc.commits, parent_idx);
			for (descendant_i = 0; descendant_i < n_descendants; ++descendant_i) {
				child_idx = rand() % commit_count;
				child_commit = (git_commit *)git_vector_get(&mc.commits, child_idx);
				expected_reachable |= git_bitvec_get(
						&reachable, parent_idx * commit_count + child_idx);
				git_oid_cpy(&descendants[descendant_i],
					    git_commit_id(child_commit));
			}

			actual_reachable = git_graph_reachable_from_any(
					repo,
					git_commit_id(parent_commit),
					descendants,
					n_descendants);
			if (actual_reachable != expected_reachable) {
				git_str error_message_buf = GIT_STR_INIT;
				char parent_oidbuf[9] = {0}, child_oidbuf[9] = {0};

				cl_git_pass(git_oid_nfmt(
						parent_oidbuf, 8, git_commit_id(parent_commit)));
				git_str_printf(&error_message_buf,
					       "git_graph_reachable_from_any(\"%s\", %zu, "
					       "{",
					       parent_oidbuf,
					       n_descendants);
				for (descendant_i = 0; descendant_i < n_descendants;
				     ++descendant_i) {
					cl_git_pass(
							git_oid_nfmt(child_oidbuf,
								     8,
								     &descendants[descendant_i]));
					git_str_printf(&error_message_buf, " \"%s\"", child_oidbuf);
				}
				git_str_printf(&error_message_buf,
					       " }) = %d, expected = %d",
					       actual_reachable,
					       expected_reachable);
				cl_check_(actual_reachable == expected_reachable,
					  git_str_cstr(&error_message_buf));
			}
		}
	}

	git_vector_foreach (&mc.commits, child_idx, child_commit)
		git_commit_free(child_commit);
	git_bitvec_free(&reachable);
	git_vector_free(&mc.commits);
	git_odb_free(mc.db);
}
