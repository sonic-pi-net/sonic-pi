#include "clar_libgit2.h"

#include <git2.h>
#include <git2/sys/midx.h>

#include "futils.h"
#include "midx.h"

void test_pack_midx__parse(void)
{
	git_repository *repo;
	struct git_midx_file *idx;
	struct git_midx_entry e;
	git_oid id;
	git_str midx_path = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_str_joinpath(&midx_path, git_repository_path(repo), "objects/pack/multi-pack-index"));
	cl_git_pass(git_midx_open(&idx, git_str_cstr(&midx_path), GIT_OID_SHA1));
	cl_assert_equal_i(git_midx_needs_refresh(idx, git_str_cstr(&midx_path)), 0);

	cl_git_pass(git_oid__fromstr(&id, "5001298e0c09ad9c34e4249bc5801c75e9754fa5", GIT_OID_SHA1));
	cl_git_pass(git_midx_entry_find(&e, idx, &id, GIT_OID_SHA1_HEXSIZE));
	cl_assert_equal_oid(&e.sha1, &id);
	cl_assert_equal_s(
			(const char *)git_vector_get(&idx->packfile_names, e.pack_index),
			"pack-d7c6adf9f61318f041845b01440d09aa7a91e1b5.idx");

	git_midx_free(idx);
	git_repository_free(repo);
	git_str_dispose(&midx_path);
}

void test_pack_midx__lookup(void)
{
	git_repository *repo;
	git_commit *commit;
	git_oid id;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_oid__fromstr(&id, "5001298e0c09ad9c34e4249bc5801c75e9754fa5", GIT_OID_SHA1));
	cl_git_pass(git_commit_lookup_prefix(&commit, repo, &id, GIT_OID_SHA1_HEXSIZE));
	cl_assert_equal_s(git_commit_message(commit), "packed commit one\n");

	git_commit_free(commit);
	git_repository_free(repo);
}

void test_pack_midx__writer(void)
{
	git_repository *repo;
	git_midx_writer *w = NULL;
	git_buf midx = GIT_BUF_INIT;
	git_str expected_midx = GIT_STR_INIT, path = GIT_STR_INIT;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_str_joinpath(&path, git_repository_path(repo), "objects/pack"));

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_midx_writer_new(&w, git_str_cstr(&path), GIT_OID_SHA1));
#else
	cl_git_pass(git_midx_writer_new(&w, git_str_cstr(&path)));
#endif

	cl_git_pass(git_midx_writer_add(w, "pack-d7c6adf9f61318f041845b01440d09aa7a91e1b5.idx"));
	cl_git_pass(git_midx_writer_add(w, "pack-d85f5d483273108c9d8dd0e4728ccf0b2982423a.idx"));
	cl_git_pass(git_midx_writer_add(w, "pack-a81e489679b7d3418f9ab594bda8ceb37dd4c695.idx"));

	cl_git_pass(git_midx_writer_dump(&midx, w));
	cl_git_pass(git_str_joinpath(&path, git_repository_path(repo), "objects/pack/multi-pack-index"));
	cl_git_pass(git_futils_readbuffer(&expected_midx, git_str_cstr(&path)));

	cl_assert_equal_i(midx.size, git_str_len(&expected_midx));
	cl_assert_equal_strn(midx.ptr, git_str_cstr(&expected_midx), midx.size);

	git_buf_dispose(&midx);
	git_str_dispose(&expected_midx);
	git_str_dispose(&path);
	git_midx_writer_free(w);
	git_repository_free(repo);
}

void test_pack_midx__odb_create(void)
{
	git_repository *repo;
	git_odb *odb;
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_str midx = GIT_STR_INIT, expected_midx = GIT_STR_INIT, midx_path = GIT_STR_INIT;
	struct stat st;

	opts.bare = true;
	opts.local = GIT_CLONE_LOCAL;
	cl_git_pass(git_clone(&repo, cl_fixture("testrepo/.gitted"), "./clone.git", &opts));
	cl_git_pass(git_str_joinpath(&midx_path, git_repository_path(repo), "objects/pack/multi-pack-index"));
	cl_git_fail(p_stat(git_str_cstr(&midx_path), &st));

	cl_git_pass(git_repository_odb(&odb, repo));
	cl_git_pass(git_odb_write_multi_pack_index(odb));
	git_odb_free(odb);

	cl_git_pass(p_stat(git_str_cstr(&midx_path), &st));

	cl_git_pass(git_futils_readbuffer(&expected_midx, cl_fixture("testrepo.git/objects/pack/multi-pack-index")));
	cl_git_pass(git_futils_readbuffer(&midx, git_str_cstr(&midx_path)));
	cl_assert_equal_i(git_str_len(&midx), git_str_len(&expected_midx));
	cl_assert_equal_strn(git_str_cstr(&midx), git_str_cstr(&expected_midx), git_str_len(&midx));

	git_repository_free(repo);
	git_str_dispose(&midx);
	git_str_dispose(&midx_path);
	git_str_dispose(&expected_midx);

	cl_git_pass(git_futils_rmdir_r("./clone.git", NULL, GIT_RMDIR_REMOVE_FILES));
}
