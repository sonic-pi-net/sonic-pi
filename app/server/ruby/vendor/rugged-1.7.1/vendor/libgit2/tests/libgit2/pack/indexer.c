#include "clar_libgit2.h"
#include <git2.h>
#include "futils.h"
#include "hash.h"
#include "iterator.h"
#include "vector.h"
#include "posix.h"


/*
 * This is a packfile with three objects. The second is a delta which
 * depends on the third, which is also a delta.
 */
static const unsigned char out_of_order_pack[] = {
  0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03,
  0x32, 0x78, 0x9c, 0x63, 0x67, 0x00, 0x00, 0x00, 0x10, 0x00, 0x08, 0x76,
  0xe6, 0x8f, 0xe8, 0x12, 0x9b, 0x54, 0x6b, 0x10, 0x1a, 0xee, 0x95, 0x10,
  0xc5, 0x32, 0x8e, 0x7f, 0x21, 0xca, 0x1d, 0x18, 0x78, 0x9c, 0x63, 0x62,
  0x66, 0x4e, 0xcb, 0xcf, 0x07, 0x00, 0x02, 0xac, 0x01, 0x4d, 0x75, 0x01,
  0xd7, 0x71, 0x36, 0x66, 0xf4, 0xde, 0x82, 0x27, 0x76, 0xc7, 0x62, 0x2c,
  0x10, 0xf1, 0xb0, 0x7d, 0xe2, 0x80, 0xdc, 0x78, 0x9c, 0x63, 0x62, 0x62,
  0x62, 0xb7, 0x03, 0x00, 0x00, 0x69, 0x00, 0x4c, 0xde, 0x7d, 0xaa, 0xe4,
  0x19, 0x87, 0x58, 0x80, 0x61, 0x09, 0x9a, 0x33, 0xca, 0x7a, 0x31, 0x92,
  0x6f, 0xae, 0x66, 0x75
};
static const unsigned int out_of_order_pack_len = 112;

/*
 * Packfile with two objects. The second is a delta against an object
 * which is not in the packfile
 */
static const unsigned char thin_pack[] = {
  0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x02,
  0x32, 0x78, 0x9c, 0x63, 0x67, 0x00, 0x00, 0x00, 0x10, 0x00, 0x08, 0x76,
  0xe6, 0x8f, 0xe8, 0x12, 0x9b, 0x54, 0x6b, 0x10, 0x1a, 0xee, 0x95, 0x10,
  0xc5, 0x32, 0x8e, 0x7f, 0x21, 0xca, 0x1d, 0x18, 0x78, 0x9c, 0x63, 0x62,
  0x66, 0x4e, 0xcb, 0xcf, 0x07, 0x00, 0x02, 0xac, 0x01, 0x4d, 0x42, 0x52,
  0x3a, 0x6f, 0x39, 0xd1, 0xfe, 0x66, 0x68, 0x6b, 0xa5, 0xe5, 0xe2, 0x97,
  0xac, 0x94, 0x6c, 0x76, 0x0b, 0x04
};
static const unsigned int thin_pack_len = 78;

/*
 * Packfile with one object. It references an object which is not in the
 * packfile and has a corrupt length (states the deltified stream is 1 byte
 * long, where it is actually 6).
 */
static const unsigned char corrupt_thin_pack[] = {
  0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01,
  0x71, 0xe6, 0x8f, 0xe8, 0x12, 0x9b, 0x54, 0x6b, 0x10, 0x1a, 0xee, 0x95,
  0x10, 0xc5, 0x32, 0x8e, 0x7f, 0x21, 0xca, 0x1d, 0x18, 0x78, 0x9c, 0x63,
  0x62, 0x66, 0x4e, 0xcb, 0xcf, 0x07, 0x00, 0x02, 0xac, 0x01, 0x4d, 0x07,
  0x67, 0x03, 0xc5, 0x40, 0x99, 0x49, 0xb1, 0x3b, 0x7d, 0xae, 0x9b, 0x0e,
  0xdd, 0xde, 0xc6, 0x76, 0x43, 0x24, 0x64
};
static const unsigned int corrupt_thin_pack_len = 67;

/*
 * Packfile with a missing trailer.
 */
static const unsigned char missing_trailer_pack[] = {
  0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x50, 0xf4, 0x3b,
};
static const unsigned int missing_trailer_pack_len = 12;

/*
 * Packfile that causes the packfile stream to open in a way in which it leaks
 * the stream reader.
 */
static const unsigned char leaky_pack[] = {
	0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03,
	0xf4, 0xbd, 0x51, 0x51, 0x51, 0x51, 0x51, 0x72, 0x65, 0x41, 0x4b, 0x63,
	0x5f, 0x64, 0x65, 0x70, 0x74, 0x68, 0xbd, 0x41, 0x4b
};
static const unsigned int leaky_pack_len = 33;

/*
 * Packfile with a three objects. The first one is a tree referencing two blobs,
 * the second object is one of those blobs. The second blob is missing.
 */
unsigned char incomplete_pack[] = {
  0x50, 0x41, 0x43, 0x4b, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x02,
  0xae, 0x03, 0x78, 0x9c, 0x33, 0x34, 0x30, 0x30, 0x33, 0x31, 0x51, 0x48,
  0x4a, 0x2c, 0x62, 0x08, 0x17, 0x3b, 0x15, 0xd9, 0x7e, 0xfa, 0x67, 0x6d,
  0xf6, 0x56, 0x4f, 0x85, 0x7d, 0xcb, 0xd6, 0xde, 0x53, 0xd1, 0x6d, 0x7f,
  0x66, 0x08, 0x91, 0x4e, 0xcb, 0xcf, 0x67, 0x50, 0xad, 0x39, 0x9a, 0xa2,
  0xb3, 0x71, 0x41, 0xc8, 0x87, 0x9e, 0x13, 0xf6, 0xba, 0x53, 0xec, 0xc2,
  0xfe, 0xda, 0xed, 0x9b, 0x09, 0x00, 0xe8, 0xc8, 0x19, 0xab, 0x34, 0x78,
  0x9c, 0x4b, 0x4a, 0x2c, 0xe2, 0x02, 0x00, 0x03, 0x9d, 0x01, 0x40, 0x4b,
  0x72, 0xa2, 0x6f, 0xb6, 0x88, 0x2d, 0x6c, 0xa5, 0x07, 0xb2, 0xa5, 0x45,
  0xe8, 0xdb, 0xe6, 0x53, 0xb3, 0x52, 0xe2
};
unsigned int incomplete_pack_len = 115;

static const unsigned char base_obj[] = { 07, 076 };
static const unsigned int base_obj_len = 2;

void test_pack_indexer__out_of_order(void)
{
	git_indexer *idx = 0;
	git_indexer_progress stats = { 0 };

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, NULL));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL));
#endif

	cl_git_pass(git_indexer_append(
		idx, out_of_order_pack, out_of_order_pack_len, &stats));
	cl_git_pass(git_indexer_commit(idx, &stats));

	cl_assert_equal_i(stats.total_objects, 3);
	cl_assert_equal_i(stats.received_objects, 3);
	cl_assert_equal_i(stats.indexed_objects, 3);

	git_indexer_free(idx);
}

void test_pack_indexer__missing_trailer(void)
{
	git_indexer *idx = 0;
	git_indexer_progress stats = { 0 };

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, NULL));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL));
#endif

	cl_git_pass(git_indexer_append(
		idx, missing_trailer_pack, missing_trailer_pack_len, &stats));
	cl_git_fail(git_indexer_commit(idx, &stats));

	cl_assert(git_error_last() != NULL);
	cl_assert_equal_i(git_error_last()->klass, GIT_ERROR_INDEXER);

	git_indexer_free(idx);
}

void test_pack_indexer__leaky(void)
{
	git_indexer *idx = 0;
	git_indexer_progress stats = { 0 };

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, NULL));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL));
#endif

	cl_git_pass(git_indexer_append(
		idx, leaky_pack, leaky_pack_len, &stats));
	cl_git_fail(git_indexer_commit(idx, &stats));

	cl_assert(git_error_last() != NULL);
	cl_assert_equal_i(git_error_last()->klass, GIT_ERROR_INDEXER);

	git_indexer_free(idx);
}

void test_pack_indexer__fix_thin(void)
{
	git_indexer *idx = NULL;
	git_indexer_progress stats = { 0 };
	git_repository *repo;
	git_odb *odb;
	git_oid id, should_id;
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;

	cl_git_pass(git_repository_init(&repo, "thin.git", true));
	cl_git_pass(git_repository_odb(&odb, repo));

	/* Store the missing base into your ODB so the indexer can fix the pack */
	cl_git_pass(git_odb_write(&id, odb, base_obj, base_obj_len, GIT_OBJECT_BLOB));
	git_oid__fromstr(&should_id, "e68fe8129b546b101aee9510c5328e7f21ca1d18", GIT_OID_SHA1);
	cl_assert_equal_oid(&should_id, &id);

#ifdef GIT_EXPERIMENTAL_SHA256
	opts.odb = odb;
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, &opts));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, odb, &opts));
#endif

	cl_git_pass(git_indexer_append(idx, thin_pack, thin_pack_len, &stats));
	cl_git_pass(git_indexer_commit(idx, &stats));

	cl_assert_equal_i(stats.total_objects, 2);
	cl_assert_equal_i(stats.received_objects, 2);
	cl_assert_equal_i(stats.indexed_objects, 2);
	cl_assert_equal_i(stats.local_objects, 1);

	cl_assert_equal_s("fefdb2d740a3a6b6c03a0c7d6ce431c6d5810e13", git_indexer_name(idx));

	git_indexer_free(idx);
	git_odb_free(odb);
	git_repository_free(repo);

	/*
	 * The pack's name/hash only tells us what objects there are,
	 * so we need to go through the packfile again in order to
	 * figure out whether we calculated the trailer correctly.
	 */
	{
		unsigned char buffer[128];
		int fd;
		ssize_t read;
		struct stat st;
		const char *name = "pack-fefdb2d740a3a6b6c03a0c7d6ce431c6d5810e13.pack";

		fd = p_open(name, O_RDONLY);
		cl_assert(fd != -1);

		cl_git_pass(p_stat(name, &st));

#ifdef GIT_EXPERIMENTAL_SHA256
		cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, NULL));
#else
		cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL));
#endif

		read = p_read(fd, buffer, sizeof(buffer));
		cl_assert(read != -1);
		p_close(fd);

		cl_git_pass(git_indexer_append(idx, buffer, read, &stats));
		cl_git_pass(git_indexer_commit(idx, &stats));

		cl_assert_equal_i(stats.total_objects, 3);
		cl_assert_equal_i(stats.received_objects, 3);
		cl_assert_equal_i(stats.indexed_objects, 3);
		cl_assert_equal_i(stats.local_objects, 0);

		git_indexer_free(idx);
	}
}

void test_pack_indexer__corrupt_length(void)
{
	git_indexer *idx = NULL;
	git_indexer_progress stats = { 0 };
	git_repository *repo;
	git_odb *odb;
	git_oid id, should_id;
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;

	cl_git_pass(git_repository_init(&repo, "thin.git", true));
	cl_git_pass(git_repository_odb(&odb, repo));

	/* Store the missing base into your ODB so the indexer can fix the pack */
	cl_git_pass(git_odb_write(&id, odb, base_obj, base_obj_len, GIT_OBJECT_BLOB));
	git_oid__fromstr(&should_id, "e68fe8129b546b101aee9510c5328e7f21ca1d18", GIT_OID_SHA1);
	cl_assert_equal_oid(&should_id, &id);

#ifdef GIT_EXPERIMENTAL_SHA256
	opts.odb = odb;
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, &opts));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, odb, &opts));
#endif

	cl_git_pass(git_indexer_append(
		idx, corrupt_thin_pack, corrupt_thin_pack_len, &stats));
	cl_git_fail(git_indexer_commit(idx, &stats));

	cl_assert(git_error_last() != NULL);
	cl_assert_equal_i(git_error_last()->klass, GIT_ERROR_ZLIB);

	git_indexer_free(idx);
	git_odb_free(odb);
	git_repository_free(repo);
}

void test_pack_indexer__incomplete_pack_fails_with_strict(void)
{
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;
	git_indexer *idx = 0;
	git_indexer_progress stats = { 0 };

	opts.verify = 1;

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, &opts));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, &opts));
#endif

	cl_git_pass(git_indexer_append(
		idx, incomplete_pack, incomplete_pack_len, &stats));
	cl_git_fail(git_indexer_commit(idx, &stats));

	cl_assert_equal_i(stats.total_objects, 2);
	cl_assert_equal_i(stats.received_objects, 2);
	cl_assert_equal_i(stats.indexed_objects, 2);

	git_indexer_free(idx);
}

void test_pack_indexer__out_of_order_with_connectivity_checks(void)
{
	git_indexer_options opts = GIT_INDEXER_OPTIONS_INIT;
	git_indexer *idx = 0;
	git_indexer_progress stats = { 0 };

	opts.verify = 1;

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, &opts));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, &opts));
#endif

	cl_git_pass(git_indexer_append(
		idx, out_of_order_pack, out_of_order_pack_len, &stats));
	cl_git_pass(git_indexer_commit(idx, &stats));

	cl_assert_equal_i(stats.total_objects, 3);
	cl_assert_equal_i(stats.received_objects, 3);
	cl_assert_equal_i(stats.indexed_objects, 3);

	git_indexer_free(idx);
}

static int find_tmp_file_recurs(void *opaque, git_str *path)
{
	int error = 0;
	git_str *first_tmp_file = opaque;
	struct stat st;

	if ((error = p_lstat_posixly(path->ptr, &st)) < 0)
		return error;

	if (S_ISDIR(st.st_mode))
		return git_fs_path_direach(path, 0, find_tmp_file_recurs, opaque);

	/* This is the template that's used in git_futils_mktmp. */
	if (strstr(git_str_cstr(path), "_git2_") != NULL)
		return git_str_sets(first_tmp_file, git_str_cstr(path));

	return 0;
}

void test_pack_indexer__no_tmp_files(void)
{
	git_indexer *idx = NULL;
	git_str path = GIT_STR_INIT;
	git_str first_tmp_file = GIT_STR_INIT;

	/* Precondition: there are no temporary files. */
	cl_git_pass(git_str_sets(&path, clar_sandbox_path()));
	cl_git_pass(find_tmp_file_recurs(&first_tmp_file, &path));
	git_str_dispose(&path);
	cl_assert(git_str_len(&first_tmp_file) == 0);

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_indexer_new(&idx, ".", GIT_OID_SHA1, NULL));
#else
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL));
#endif

	git_indexer_free(idx);

	cl_git_pass(git_str_sets(&path, clar_sandbox_path()));
	cl_git_pass(find_tmp_file_recurs(&first_tmp_file, &path));
	git_str_dispose(&path);
	cl_assert(git_str_len(&first_tmp_file) == 0);
	git_str_dispose(&first_tmp_file);
}
