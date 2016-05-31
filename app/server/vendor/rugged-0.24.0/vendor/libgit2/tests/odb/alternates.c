#include "clar_libgit2.h"
#include "odb.h"
#include "filebuf.h"

static git_buf destpath, filepath;
static const char *paths[] = {
	"A.git", "B.git", "C.git", "D.git", "E.git", "F.git", "G.git"
};
static git_filebuf file;
static git_repository *repo;

void test_odb_alternates__cleanup(void)
{
	size_t i;

	git_buf_free(&destpath);
	git_buf_free(&filepath);

	for (i = 0; i < ARRAY_SIZE(paths); i++)
		cl_fixture_cleanup(paths[i]);
}

static void init_linked_repo(const char *path, const char *alternate)
{
	git_buf_clear(&destpath);
	git_buf_clear(&filepath);

	cl_git_pass(git_repository_init(&repo, path, 1));
	cl_git_pass(git_path_prettify(&destpath, alternate, NULL));
	cl_git_pass(git_buf_joinpath(&destpath, destpath.ptr, "objects"));
	cl_git_pass(git_buf_joinpath(&filepath, git_repository_path(repo), "objects/info"));
	cl_git_pass(git_futils_mkdir(filepath.ptr, 0755, GIT_MKDIR_PATH));
	cl_git_pass(git_buf_joinpath(&filepath, filepath.ptr , "alternates"));

	cl_git_pass(git_filebuf_open(&file, git_buf_cstr(&filepath), 0, 0666));
	git_filebuf_printf(&file, "%s\n", git_buf_cstr(&destpath));
	cl_git_pass(git_filebuf_commit(&file));

	git_repository_free(repo);
}

void test_odb_alternates__chained(void)
{
	git_commit *commit;
	git_oid oid;

	/* Set the alternate A -> testrepo.git */
	init_linked_repo(paths[0], cl_fixture("testrepo.git"));

	/* Set the alternate B -> A */
	init_linked_repo(paths[1], paths[0]);

	/* Now load B and see if we can find an object from testrepo.git */
	cl_git_pass(git_repository_open(&repo, paths[1]));
	git_oid_fromstr(&oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	git_commit_free(commit);
	git_repository_free(repo);
}

void test_odb_alternates__long_chain(void)
{
	git_commit *commit;
	git_oid oid;
	size_t i;

	/* Set the alternate A -> testrepo.git */
	init_linked_repo(paths[0], cl_fixture("testrepo.git"));

	/* Set up the five-element chain */
	for (i = 1; i < ARRAY_SIZE(paths); i++) {
		init_linked_repo(paths[i], paths[i-1]);
	}

	/* Now load the last one and see if we can find an object from testrepo.git */
	cl_git_pass(git_repository_open(&repo, paths[ARRAY_SIZE(paths)-1]));
	git_oid_fromstr(&oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_git_fail(git_commit_lookup(&commit, repo, &oid));
	git_repository_free(repo);
}
