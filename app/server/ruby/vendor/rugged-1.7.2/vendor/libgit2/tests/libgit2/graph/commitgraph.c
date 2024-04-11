#include "clar_libgit2.h"

#include <git2.h>
#include <git2/sys/commit_graph.h>

#include "commit_graph.h"
#include "futils.h"

void test_graph_commitgraph__parse(void)
{
	git_repository *repo;
	struct git_commit_graph_file *file;
	struct git_commit_graph_entry e, parent;
	git_oid id;
	git_str commit_graph_path = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_str_joinpath(&commit_graph_path, git_repository_path(repo), "objects/info/commit-graph"));
	cl_git_pass(git_commit_graph_file_open(&file, git_str_cstr(&commit_graph_path), GIT_OID_SHA1));
	cl_assert_equal_i(git_commit_graph_file_needs_refresh(file, git_str_cstr(&commit_graph_path)), 0);

	cl_git_pass(git_oid__fromstr(&id, "5001298e0c09ad9c34e4249bc5801c75e9754fa5", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_find(&e, file, &id, GIT_OID_SHA1_HEXSIZE));
	cl_assert_equal_oid(&e.sha1, &id);
	cl_git_pass(git_oid__fromstr(&id, "418382dff1ffb8bdfba833f4d8bbcde58b1e7f47", GIT_OID_SHA1));
	cl_assert_equal_oid(&e.tree_oid, &id);
	cl_assert_equal_i(e.generation, 1);
	cl_assert_equal_i(e.commit_time, UINT64_C(1273610423));
	cl_assert_equal_i(e.parent_count, 0);

	cl_git_pass(git_oid__fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_find(&e, file, &id, GIT_OID_SHA1_HEXSIZE));
	cl_assert_equal_oid(&e.sha1, &id);
	cl_assert_equal_i(e.generation, 5);
	cl_assert_equal_i(e.commit_time, UINT64_C(1274813907));
	cl_assert_equal_i(e.parent_count, 2);

	cl_git_pass(git_oid__fromstr(&id, "9fd738e8f7967c078dceed8190330fc8648ee56a", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_parent(&parent, file, &e, 0));
	cl_assert_equal_oid(&parent.sha1, &id);
	cl_assert_equal_i(parent.generation, 4);

	cl_git_pass(git_oid__fromstr(&id, "c47800c7266a2be04c571c04d5a6614691ea99bd", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_parent(&parent, file, &e, 1));
	cl_assert_equal_oid(&parent.sha1, &id);
	cl_assert_equal_i(parent.generation, 3);

	git_commit_graph_file_free(file);
	git_repository_free(repo);
	git_str_dispose(&commit_graph_path);
}

void test_graph_commitgraph__parse_octopus_merge(void)
{
	git_repository *repo;
	struct git_commit_graph_file *file;
	struct git_commit_graph_entry e, parent;
	git_oid id;
	git_str commit_graph_path = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("merge-recursive/.gitted")));
	cl_git_pass(git_str_joinpath(&commit_graph_path, git_repository_path(repo), "objects/info/commit-graph"));
	cl_git_pass(git_commit_graph_file_open(&file, git_str_cstr(&commit_graph_path), GIT_OID_SHA1));

	cl_git_pass(git_oid__fromstr(&id, "d71c24b3b113fd1d1909998c5bfe33b86a65ee03", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_find(&e, file, &id, GIT_OID_SHA1_HEXSIZE));
	cl_assert_equal_oid(&e.sha1, &id);
	cl_git_pass(git_oid__fromstr(&id, "348f16ffaeb73f319a75cec5b16a0a47d2d5e27c", GIT_OID_SHA1));
	cl_assert_equal_oid(&e.tree_oid, &id);
	cl_assert_equal_i(e.generation, 7);
	cl_assert_equal_i(e.commit_time, UINT64_C(1447083009));
	cl_assert_equal_i(e.parent_count, 3);

	cl_git_pass(git_oid__fromstr(&id, "ad2ace9e15f66b3d1138922e6ffdc3ea3f967fa6", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_parent(&parent, file, &e, 0));
	cl_assert_equal_oid(&parent.sha1, &id);
	cl_assert_equal_i(parent.generation, 6);

	cl_git_pass(git_oid__fromstr(&id, "483065df53c0f4a02cdc6b2910b05d388fc17ffb", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_parent(&parent, file, &e, 1));
	cl_assert_equal_oid(&parent.sha1, &id);
	cl_assert_equal_i(parent.generation, 2);

	cl_git_pass(git_oid__fromstr(&id, "815b5a1c80ca749d705c7aa0cb294a00cbedd340", GIT_OID_SHA1));
	cl_git_pass(git_commit_graph_entry_parent(&parent, file, &e, 2));
	cl_assert_equal_oid(&parent.sha1, &id);
	cl_assert_equal_i(parent.generation, 6);

	git_commit_graph_file_free(file);
	git_repository_free(repo);
	git_str_dispose(&commit_graph_path);
}

void test_graph_commitgraph__writer(void)
{
	git_repository *repo;
	git_commit_graph_writer *w = NULL;
	git_revwalk *walk;
	git_commit_graph_writer_options opts = GIT_COMMIT_GRAPH_WRITER_OPTIONS_INIT;
	git_buf cgraph = GIT_BUF_INIT;
	git_str expected_cgraph = GIT_STR_INIT, path = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_str_joinpath(&path, git_repository_path(repo), "objects/info"));

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_commit_graph_writer_new(&w, git_str_cstr(&path), GIT_OID_SHA1));
#else
	cl_git_pass(git_commit_graph_writer_new(&w, git_str_cstr(&path)));
#endif

	/* This is equivalent to `git commit-graph write --reachable`. */
	cl_git_pass(git_revwalk_new(&walk, repo));
	cl_git_pass(git_revwalk_push_glob(walk, "refs/*"));
	cl_git_pass(git_commit_graph_writer_add_revwalk(w, walk));
	git_revwalk_free(walk);

	cl_git_pass(git_commit_graph_writer_dump(&cgraph, w, &opts));
	cl_git_pass(git_str_joinpath(&path, git_repository_path(repo), "objects/info/commit-graph"));
	cl_git_pass(git_futils_readbuffer(&expected_cgraph, git_str_cstr(&path)));

	cl_assert_equal_i(cgraph.size, git_str_len(&expected_cgraph));
	cl_assert_equal_i(memcmp(cgraph.ptr, git_str_cstr(&expected_cgraph), cgraph.size), 0);

	git_buf_dispose(&cgraph);
	git_str_dispose(&expected_cgraph);
	git_str_dispose(&path);
	git_commit_graph_writer_free(w);
	git_repository_free(repo);
}

void test_graph_commitgraph__validate(void)
{
	git_repository *repo;
	struct git_commit_graph *cgraph;
	git_str objects_dir = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_str_joinpath(&objects_dir, git_repository_path(repo), "objects"));

	/* git_commit_graph_open() calls git_commit_graph_validate() */
#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_commit_graph_open(&cgraph, git_str_cstr(&objects_dir), GIT_OID_SHA1));
#else
	cl_git_pass(git_commit_graph_open(&cgraph, git_str_cstr(&objects_dir)));
#endif

	git_commit_graph_free(cgraph);
	git_str_dispose(&objects_dir);
	git_repository_free(repo);
}

void test_graph_commitgraph__validate_corrupt(void)
{
	git_repository *repo;
	struct git_commit_graph *cgraph;
	int fd = -1;

	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, cl_git_sandbox_path(1, "testrepo.git", NULL)));

	/* corrupt commit graph checksum at the end of the file */
	cl_assert((fd = p_open(cl_git_sandbox_path(0, "testrepo.git", "objects", "info", "commit-graph", NULL), O_WRONLY)) > 0);
	cl_assert(p_lseek(fd, -5, SEEK_END) > 0);
	cl_must_pass(p_write(fd, "\0\0", 2));
	cl_must_pass(p_close(fd));

	/* git_commit_graph_open() calls git_commit_graph_validate() */
#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_fail(git_commit_graph_open(&cgraph, cl_git_sandbox_path(1, "testrepo.git", "objects", NULL), GIT_OID_SHA1));
#else
	cl_git_fail(git_commit_graph_open(&cgraph, cl_git_sandbox_path(1, "testrepo.git", "objects", NULL)));
#endif

	git_commit_graph_free(cgraph);
	git_repository_free(repo);

	cl_fixture_cleanup("testrepo.git");
}
