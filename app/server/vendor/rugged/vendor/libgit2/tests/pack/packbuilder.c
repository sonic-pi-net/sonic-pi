#include "clar_libgit2.h"
#include "fileops.h"
#include "pack.h"
#include "hash.h"
#include "iterator.h"
#include "vector.h"
#include "posix.h"

static git_repository *_repo;
static git_revwalk *_revwalker;
static git_packbuilder *_packbuilder;
static git_indexer *_indexer;
static git_vector _commits;
static int _commits_is_initialized;
static git_transfer_progress _stats;

void test_pack_packbuilder__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(p_chdir("testrepo.git"));
	cl_git_pass(git_revwalk_new(&_revwalker, _repo));
	cl_git_pass(git_packbuilder_new(&_packbuilder, _repo));
	cl_git_pass(git_vector_init(&_commits, 0, NULL));
	_commits_is_initialized = 1;
	memset(&_stats, 0, sizeof(_stats));
}

void test_pack_packbuilder__cleanup(void)
{
	git_oid *o;
	unsigned int i;

	if (_commits_is_initialized) {
		_commits_is_initialized = 0;
		git_vector_foreach(&_commits, i, o) {
			git__free(o);
		}
		git_vector_free(&_commits);
	}

	git_packbuilder_free(_packbuilder);
	_packbuilder = NULL;

	git_revwalk_free(_revwalker);
	_revwalker = NULL;

	git_indexer_free(_indexer);
	_indexer = NULL;

	p_chdir("..");
	cl_git_sandbox_cleanup();
	_repo = NULL;
}

static void seed_packbuilder(void)
{
	git_oid oid, *o;
	unsigned int i;

	git_revwalk_sorting(_revwalker, GIT_SORT_TIME);
	cl_git_pass(git_revwalk_push_ref(_revwalker, "HEAD"));

	while (git_revwalk_next(&oid, _revwalker) == 0) {
		o = git__malloc(GIT_OID_RAWSZ);
		cl_assert(o != NULL);
		git_oid_cpy(o, &oid);
		cl_git_pass(git_vector_insert(&_commits, o));
	}

	git_vector_foreach(&_commits, i, o) {
		cl_git_pass(git_packbuilder_insert(_packbuilder, o, NULL));
	}

	git_vector_foreach(&_commits, i, o) {
		git_object *obj;
		cl_git_pass(git_object_lookup(&obj, _repo, o, GIT_OBJ_COMMIT));
		cl_git_pass(git_packbuilder_insert_tree(_packbuilder,
					git_commit_tree_id((git_commit *)obj)));
		git_object_free(obj);
	}
}

static int feed_indexer(void *ptr, size_t len, void *payload)
{
	git_transfer_progress *stats = (git_transfer_progress *)payload;

	return git_indexer_append(_indexer, ptr, len, stats);
}

void test_pack_packbuilder__create_pack(void)
{
	git_transfer_progress stats;
	git_buf buf = GIT_BUF_INIT, path = GIT_BUF_INIT;
	git_hash_ctx ctx;
	git_oid hash;
	char hex[41]; hex[40] = '\0';

	seed_packbuilder();

	cl_git_pass(git_indexer_new(&_indexer, ".", 0, NULL, NULL, NULL));
	cl_git_pass(git_packbuilder_foreach(_packbuilder, feed_indexer, &stats));
	cl_git_pass(git_indexer_commit(_indexer, &stats));

	git_oid_fmt(hex, git_indexer_hash(_indexer));
	git_buf_printf(&path, "pack-%s.pack", hex);

	/*
	 * By default, packfiles are created with only one thread.
	 * Therefore we can predict the object ordering and make sure
	 * we create exactly the same pack as git.git does when *not*
	 * reusing existing deltas (as libgit2).
	 *
	 * $ cd tests/resources/testrepo.git
	 * $ git rev-list --objects HEAD | \
	 * 	git pack-objects -q --no-reuse-delta --threads=1 pack
	 * $ sha1sum git-80e61eb315239ef3c53033e37fee43b744d57122.pack
	 * 5d410bdf97cf896f9007681b92868471d636954b
	 *
	 */

	cl_git_pass(git_futils_readbuffer(&buf, git_buf_cstr(&path)));

	cl_git_pass(git_hash_ctx_init(&ctx));
	cl_git_pass(git_hash_update(&ctx, buf.ptr, buf.size));
	cl_git_pass(git_hash_final(&hash, &ctx));
	git_hash_ctx_cleanup(&ctx);

	git_buf_free(&path);
	git_buf_free(&buf);

	git_oid_fmt(hex, &hash);

	cl_assert_equal_s(hex, "5d410bdf97cf896f9007681b92868471d636954b");
}

void test_pack_packbuilder__get_hash(void)
{
	char hex[41]; hex[40] = '\0';

	seed_packbuilder();

	git_packbuilder_write(_packbuilder, ".", 0, NULL, NULL);
	git_oid_fmt(hex, git_packbuilder_hash(_packbuilder));

	cl_assert_equal_s(hex, "80e61eb315239ef3c53033e37fee43b744d57122");
}

static void test_write_pack_permission(mode_t given, mode_t expected)
{
	struct stat statbuf;
	mode_t mask, os_mask;

	seed_packbuilder();

	git_packbuilder_write(_packbuilder, ".", given, NULL, NULL);

	/* Windows does not return group/user bits from stat,
	* files are never executable.
	*/
#ifdef GIT_WIN32
	os_mask = 0600;
#else
	os_mask = 0777;
#endif

	mask = p_umask(0);
	p_umask(mask);

	cl_git_pass(p_stat("pack-80e61eb315239ef3c53033e37fee43b744d57122.idx", &statbuf));
	cl_assert_equal_i(statbuf.st_mode & os_mask, (expected & ~mask) & os_mask);

	cl_git_pass(p_stat("pack-80e61eb315239ef3c53033e37fee43b744d57122.pack", &statbuf));
	cl_assert_equal_i(statbuf.st_mode & os_mask, (expected & ~mask) & os_mask);
}

void test_pack_packbuilder__permissions_standard(void)
{
	test_write_pack_permission(0, GIT_PACK_FILE_MODE);
}

void test_pack_packbuilder__permissions_readonly(void)
{
	test_write_pack_permission(0444, 0444);
}

void test_pack_packbuilder__permissions_readwrite(void)
{
	test_write_pack_permission(0666, 0666);
}

static int foreach_cb(void *buf, size_t len, void *payload)
{
	git_indexer *idx = (git_indexer *) payload;
	cl_git_pass(git_indexer_append(idx, buf, len, &_stats));
	return 0;
}

void test_pack_packbuilder__foreach(void)
{
	git_indexer *idx;

	seed_packbuilder();
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL, NULL));
	cl_git_pass(git_packbuilder_foreach(_packbuilder, foreach_cb, idx));
	cl_git_pass(git_indexer_commit(idx, &_stats));
	git_indexer_free(idx);
}

static int foreach_cancel_cb(void *buf, size_t len, void *payload)
{
	git_indexer *idx = (git_indexer *)payload;
	cl_git_pass(git_indexer_append(idx, buf, len, &_stats));
	return (_stats.total_objects > 2) ? -1111 : 0;
}

void test_pack_packbuilder__foreach_with_cancel(void)
{
	git_indexer *idx;

	seed_packbuilder();
	cl_git_pass(git_indexer_new(&idx, ".", 0, NULL, NULL, NULL));
	cl_git_fail_with(
		git_packbuilder_foreach(_packbuilder, foreach_cancel_cb, idx), -1111);
	git_indexer_free(idx);
}
