#include "clar_libgit2.h"
#include "vector.h"
#include <stdarg.h>

static git_repository *_repo;
static git_repository *_repo2;

void test_revwalk_mergebase__initialize(void)
{
	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_open(&_repo2, cl_fixture("twowaymerge.git")));
}

void test_revwalk_mergebase__cleanup(void)
{
	git_repository_free(_repo);
	_repo = NULL;

	git_repository_free(_repo2);
	_repo2 = NULL;
}

void test_revwalk_mergebase__single1(void)
{
	git_oid result, one, two, expected;
	size_t ahead, behind;

	cl_git_pass(git_oid_fromstr(&one, "c47800c7266a2be04c571c04d5a6614691ea99bd "));
	cl_git_pass(git_oid_fromstr(&two, "9fd738e8f7967c078dceed8190330fc8648ee56a"));
	cl_git_pass(git_oid_fromstr(&expected, "5b5b025afb0b4c913b4c338a42934a3863bf3644"));

	cl_git_pass(git_merge_base(&result, _repo, &one, &two));
	cl_assert_equal_oid(&expected, &result);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &one, &two));
	cl_assert_equal_sz(ahead, 1);
	cl_assert_equal_sz(behind, 2);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &two, &one));
	cl_assert_equal_sz(ahead,  2);
	cl_assert_equal_sz(behind,  1);
}

void test_revwalk_mergebase__single2(void)
{
	git_oid result, one, two, expected;
	size_t ahead, behind;

	cl_git_pass(git_oid_fromstr(&one, "763d71aadf09a7951596c9746c024e7eece7c7af"));
	cl_git_pass(git_oid_fromstr(&two, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750"));
	cl_git_pass(git_oid_fromstr(&expected, "c47800c7266a2be04c571c04d5a6614691ea99bd"));

	cl_git_pass(git_merge_base(&result, _repo, &one, &two));
	cl_assert_equal_oid(&expected, &result);

	cl_git_pass(git_graph_ahead_behind( &ahead, &behind, _repo, &one, &two));
	cl_assert_equal_sz(ahead,  1);
	cl_assert_equal_sz(behind,  4);

	cl_git_pass(git_graph_ahead_behind( &ahead, &behind, _repo, &two, &one));
	cl_assert_equal_sz(ahead,  4);
	cl_assert_equal_sz(behind,  1);
}

void test_revwalk_mergebase__merged_branch(void)
{
	git_oid result, one, two, expected;
	size_t ahead, behind;

	cl_git_pass(git_oid_fromstr(&one, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750"));
	cl_git_pass(git_oid_fromstr(&two, "9fd738e8f7967c078dceed8190330fc8648ee56a"));
	cl_git_pass(git_oid_fromstr(&expected, "9fd738e8f7967c078dceed8190330fc8648ee56a"));

	cl_git_pass(git_merge_base(&result, _repo, &one, &two));
	cl_assert_equal_oid(&expected, &result);

	cl_git_pass(git_merge_base(&result, _repo, &two, &one));
	cl_assert_equal_oid(&expected, &result);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &one, &two));
	cl_assert_equal_sz(ahead,  3);
	cl_assert_equal_sz(behind,  0);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &two, &one));
	cl_assert_equal_sz(ahead,  0);
	cl_assert_equal_sz(behind,  3);
}

void test_revwalk_mergebase__two_way_merge(void)
{
	git_oid one, two;
	size_t ahead, behind;

	cl_git_pass(git_oid_fromstr(&one, "9b219343610c88a1187c996d0dc58330b55cee28"));
	cl_git_pass(git_oid_fromstr(&two, "a953a018c5b10b20c86e69fef55ebc8ad4c5a417"));
	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo2, &one, &two));

	cl_assert_equal_sz(ahead,  8);
	cl_assert_equal_sz(behind,  2);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo2, &two, &one));

	cl_assert_equal_sz(ahead,  2);
	cl_assert_equal_sz(behind,  8);
}

void test_revwalk_mergebase__no_common_ancestor_returns_ENOTFOUND(void)
{
	git_oid result, one, two;
	size_t ahead, behind;
	int error;

	cl_git_pass(git_oid_fromstr(&one, "763d71aadf09a7951596c9746c024e7eece7c7af"));
	cl_git_pass(git_oid_fromstr(&two, "e90810b8df3e80c413d903f631643c716887138d"));

	error = git_merge_base(&result, _repo, &one, &two);
	cl_git_fail(error);

	cl_assert_equal_i(GIT_ENOTFOUND, error);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &one, &two));
	cl_assert_equal_sz(4, ahead);
	cl_assert_equal_sz(2, behind);
}

void test_revwalk_mergebase__prefer_youngest_merge_base(void)
{
	git_oid result, one, two, expected;

	cl_git_pass(git_oid_fromstr(&one, "a4a7dce85cf63874e984719f4fdd239f5145052f"));
	cl_git_pass(git_oid_fromstr(&two, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_oid_fromstr(&expected, "c47800c7266a2be04c571c04d5a6614691ea99bd"));

	cl_git_pass(git_merge_base(&result, _repo, &one, &two));
	cl_assert_equal_oid(&expected, &result);
}

void test_revwalk_mergebase__multiple_merge_bases(void)
{
	git_oid one, two, expected1, expected2;
	git_oidarray result = {NULL, 0};

	cl_git_pass(git_oid_fromstr(&one, "a4a7dce85cf63874e984719f4fdd239f5145052f"));
	cl_git_pass(git_oid_fromstr(&two, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_oid_fromstr(&expected1, "c47800c7266a2be04c571c04d5a6614691ea99bd"));
	cl_git_pass(git_oid_fromstr(&expected2, "9fd738e8f7967c078dceed8190330fc8648ee56a"));

	cl_git_pass(git_merge_bases(&result, _repo, &one, &two));
	cl_assert_equal_i(2, result.count);
	cl_assert_equal_oid(&expected1, &result.ids[0]);
	cl_assert_equal_oid(&expected2, &result.ids[1]);

	git_oidarray_free(&result);
}

void test_revwalk_mergebase__multiple_merge_bases_many_commits(void)
{
	git_oid expected1, expected2;
	git_oidarray result = {NULL, 0};

	git_oid *input = git__malloc(sizeof(git_oid) * 2);

	cl_git_pass(git_oid_fromstr(&input[0], "a4a7dce85cf63874e984719f4fdd239f5145052f"));
	cl_git_pass(git_oid_fromstr(&input[1], "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_oid_fromstr(&expected1, "c47800c7266a2be04c571c04d5a6614691ea99bd"));
	cl_git_pass(git_oid_fromstr(&expected2, "9fd738e8f7967c078dceed8190330fc8648ee56a"));

	cl_git_pass(git_merge_bases_many(&result, _repo, 2, input));
	cl_assert_equal_i(2, result.count);
	cl_assert_equal_oid(&expected1, &result.ids[0]);
	cl_assert_equal_oid(&expected2, &result.ids[1]);

	git_oidarray_free(&result);
	git__free(input);
}

void test_revwalk_mergebase__no_off_by_one_missing(void)
{
	git_oid result, one, two;

	cl_git_pass(git_oid_fromstr(&one, "1a443023183e3f2bfbef8ac923cd81c1018a18fd"));
	cl_git_pass(git_oid_fromstr(&two, "9f13f7d0a9402c681f91dc590cf7b5470e6a77d2"));
	cl_git_pass(git_merge_base(&result, _repo, &one, &two));
}

static void assert_mergebase_many(const char *expected_sha, int count, ...)
{
	va_list ap;
	int i; 
	git_oid *oids;
	git_oid oid, expected;
	char *partial_oid;
	git_object *object;

	oids = git__malloc(count * sizeof(git_oid));
	cl_assert(oids != NULL);

	memset(oids, 0x0, count * sizeof(git_oid));

	va_start(ap, count);
	
	for (i = 0; i < count; ++i) {
		partial_oid = va_arg(ap, char *);
		cl_git_pass(git_oid_fromstrn(&oid, partial_oid, strlen(partial_oid)));

		cl_git_pass(git_object_lookup_prefix(&object, _repo, &oid, strlen(partial_oid), GIT_OBJ_COMMIT));
		git_oid_cpy(&oids[i], git_object_id(object));
		git_object_free(object);
	}

	va_end(ap);

	if (expected_sha == NULL)
		cl_assert_equal_i(GIT_ENOTFOUND, git_merge_base_many(&oid, _repo, count, oids));
	else {
		cl_git_pass(git_merge_base_many(&oid, _repo, count, oids));
		cl_git_pass(git_oid_fromstr(&expected, expected_sha));

		cl_assert_equal_oid(&expected, &oid);
	}

	git__free(oids);
}

void test_revwalk_mergebase__many_no_common_ancestor_returns_ENOTFOUND(void)
{
	assert_mergebase_many(NULL, 3, "41bc8c", "e90810", "a65fed");
	assert_mergebase_many(NULL, 3, "e90810", "41bc8c", "a65fed");
	assert_mergebase_many(NULL, 3, "e90810", "a65fed", "41bc8c");
	assert_mergebase_many(NULL, 3, "a65fed", "e90810", "41bc8c");
	assert_mergebase_many(NULL, 3, "a65fed", "41bc8c", "e90810");

	assert_mergebase_many(NULL, 3, "e90810", "763d71", "a65fed");
}

void test_revwalk_mergebase__many_merge_branch(void)
{
	assert_mergebase_many("c47800c7266a2be04c571c04d5a6614691ea99bd", 3, "a65fed", "763d71", "849607");

	assert_mergebase_many("c47800c7266a2be04c571c04d5a6614691ea99bd", 3, "763d71", "e90810", "a65fed");
	assert_mergebase_many("c47800c7266a2be04c571c04d5a6614691ea99bd", 3, "763d71", "a65fed", "e90810");

	assert_mergebase_many("c47800c7266a2be04c571c04d5a6614691ea99bd", 3, "a65fed", "763d71", "849607");
	assert_mergebase_many("c47800c7266a2be04c571c04d5a6614691ea99bd", 3, "a65fed", "849607", "763d71");
	assert_mergebase_many("8496071c1b46c854b31185ea97743be6a8774479", 3, "849607", "a65fed", "763d71");

	assert_mergebase_many("5b5b025afb0b4c913b4c338a42934a3863bf3644", 5, "5b5b02", "763d71", "a4a7dc", "a65fed", "41bc8c");
}

static void assert_mergebase_octopus(const char *expected_sha, int count, ...)
{
	va_list ap;
	int i;
	git_oid *oids;
	git_oid oid, expected;
	char *partial_oid;
	git_object *object;

	oids = git__malloc(count * sizeof(git_oid));
	cl_assert(oids != NULL);

	memset(oids, 0x0, count * sizeof(git_oid));

	va_start(ap, count);

	for (i = 0; i < count; ++i) {
		partial_oid = va_arg(ap, char *);
		cl_git_pass(git_oid_fromstrn(&oid, partial_oid, strlen(partial_oid)));

		cl_git_pass(git_object_lookup_prefix(&object, _repo, &oid, strlen(partial_oid), GIT_OBJ_COMMIT));
		git_oid_cpy(&oids[i], git_object_id(object));
		git_object_free(object);
	}

	va_end(ap);

	if (expected_sha == NULL)
		cl_assert_equal_i(GIT_ENOTFOUND, git_merge_base_octopus(&oid, _repo, count, oids));
	else {
		cl_git_pass(git_merge_base_octopus(&oid, _repo, count, oids));
		cl_git_pass(git_oid_fromstr(&expected, expected_sha));

		cl_assert_equal_oid(&expected, &oid);
	}

	git__free(oids);
}

void test_revwalk_mergebase__octopus_no_common_ancestor_returns_ENOTFOUND(void)
{
	assert_mergebase_octopus(NULL, 3, "41bc8c", "e90810", "a65fed");
	assert_mergebase_octopus(NULL, 3, "e90810", "41bc8c", "a65fed");
	assert_mergebase_octopus(NULL, 3, "e90810", "a65fed", "41bc8c");
	assert_mergebase_octopus(NULL, 3, "a65fed", "e90810", "41bc8c");
	assert_mergebase_octopus(NULL, 3, "a65fed", "41bc8c", "e90810");

	assert_mergebase_octopus(NULL, 3, "e90810", "763d71", "a65fed");

	assert_mergebase_octopus(NULL, 3, "763d71", "e90810", "a65fed");
	assert_mergebase_octopus(NULL, 3, "763d71", "a65fed", "e90810");

	assert_mergebase_octopus(NULL, 5, "5b5b02", "763d71", "a4a7dc", "a65fed", "41bc8c");
}

void test_revwalk_mergebase__octopus_merge_branch(void)
{
	assert_mergebase_octopus("8496071c1b46c854b31185ea97743be6a8774479", 3, "a65fed", "763d71", "849607");

	assert_mergebase_octopus("8496071c1b46c854b31185ea97743be6a8774479", 3, "a65fed", "763d71", "849607");
	assert_mergebase_octopus("8496071c1b46c854b31185ea97743be6a8774479", 3, "a65fed", "849607", "763d71");
	assert_mergebase_octopus("8496071c1b46c854b31185ea97743be6a8774479", 3, "849607", "a65fed", "763d71");
}

/*
 * testrepo.git $ git log --graph --all
 * * commit 763d71aadf09a7951596c9746c024e7eece7c7af
 * | Author: nulltoken <emeric.fermas@gmail.com>
 * | Date:   Sun Oct 9 12:54:47 2011 +0200
 * |
 * |     Add some files into subdirectories
 * |
 * | * commit a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * | | Author: Scott Chacon <schacon@gmail.com>
 * | | Date:   Tue Aug 9 19:33:46 2011 -0700
 * | |
 * | *   commit be3563ae3f795b2b4353bcce3a527ad0a4f7f644
 * | |\  Merge: 9fd738e c47800c
 * | |/  Author: Scott Chacon <schacon@gmail.com>
 * |/|   Date:   Tue May 25 11:58:27 2010 -0700
 * | |
 * | |       Merge branch 'br2'
 * | |
 * | | * commit e90810b8df3e80c413d903f631643c716887138d
 * | | | Author: Vicent Marti <tanoku@gmail.com>
 * | | | Date:   Thu Aug 5 18:42:20 2010 +0200
 * | | |
 * | | |     Test commit 2
 * | | |
 * | | * commit 6dcf9bf7541ee10456529833502442f385010c3d
 * | |   Author: Vicent Marti <tanoku@gmail.com>
 * | |   Date:   Thu Aug 5 18:41:33 2010 +0200
 * | |
 * | |       Test commit 1
 * | |
 * | | *   commit a4a7dce85cf63874e984719f4fdd239f5145052f
 * | | |\  Merge: c47800c 9fd738e
 * | |/ /  Author: Scott Chacon <schacon@gmail.com>
 * |/| /   Date:   Tue May 25 12:00:23 2010 -0700
 * | |/
 * | |         Merge branch 'master' into br2
 * | |
 * | * commit 9fd738e8f7967c078dceed8190330fc8648ee56a
 * | | Author: Scott Chacon <schacon@gmail.com>
 * | | Date:   Mon May 24 10:19:19 2010 -0700
 * | |
 * | |     a fourth commit
 * | |
 * | * commit 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 * | | Author: Scott Chacon <schacon@gmail.com>
 * | | Date:   Mon May 24 10:19:04 2010 -0700
 * | |
 * | |     a third commit
 * | |
 * * | commit c47800c7266a2be04c571c04d5a6614691ea99bd
 * |/  Author: Scott Chacon <schacon@gmail.com>
 * |   Date:   Tue May 25 11:58:14 2010 -0700
 * |
 * |       branch commit one
 * |
 * * commit 5b5b025afb0b4c913b4c338a42934a3863bf3644
 * | Author: Scott Chacon <schacon@gmail.com>
 * | Date:   Tue May 11 13:38:42 2010 -0700
 * |
 * |     another commit
 * |
 * * commit 8496071c1b46c854b31185ea97743be6a8774479
 *   Author: Scott Chacon <schacon@gmail.com>
 *   Date:   Sat May 8 16:13:06 2010 -0700
 * 
 *       testing
 * 
 * * commit 41bc8c69075bbdb46c5c6f0566cc8cc5b46e8bd9
 * | Author: Scott Chacon <schacon@gmail.com>
 * | Date:   Tue May 11 13:40:41 2010 -0700
 * |
 * |     packed commit two
 * |
 * * commit 5001298e0c09ad9c34e4249bc5801c75e9754fa5
 *   Author: Scott Chacon <schacon@gmail.com>
 *   Date:   Tue May 11 13:40:23 2010 -0700
 * 
 *       packed commit one
 */

/*
 * twowaymerge.git $ git log --graph --all
 * *   commit 9b219343610c88a1187c996d0dc58330b55cee28
 * |\  Merge: c37a783 2224e19
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:31:04 2012 -0800
 * | | 
 * | |     Merge branch 'first-branch' into second-branch
 * | |   
 * | * commit 2224e191514cb4bd8c566d80dac22dfcb1e9bb83
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:28:51 2012 -0800
 * | | 
 * | |     j
 * | |   
 * | * commit a41a49f8f5cd9b6cb14a076bf8394881ed0b4d19
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:28:39 2012 -0800
 * | | 
 * | |     i
 * | |   
 * | * commit 82bf9a1a10a4b25c1f14c9607b60970705e92545
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:28:28 2012 -0800
 * | | 
 * | |     h
 * | |   
 * * | commit c37a783c20d92ac92362a78a32860f7eebf938ef
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:30:57 2012 -0800
 * | | 
 * | |     n
 * | |   
 * * | commit 8b82fb1794cb1c8c7f172ec730a4c2db0ae3e650
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:30:43 2012 -0800
 * | | 
 * | |     m
 * | |   
 * * | commit 6ab5d28acbf3c3bdff276f7ccfdf29c1520e542f
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:30:38 2012 -0800
 * | | 
 * | |     l
 * | |   
 * * | commit 7b8c336c45fc6895c1c60827260fe5d798e5d247
 * | | Author: Scott J. Goldman <scottjg@github.com>
 * | | Date:   Tue Nov 27 20:30:24 2012 -0800
 * | | 
 * | |     k
 * | |     
 * | | * commit 1c30b88f5f3ee66d78df6520a7de9e89b890818b
 * | | | Author: Scott J. Goldman <scottjg@github.com>
 * | | | Date:   Tue Nov 27 20:28:10 2012 -0800
 * | | | 
 * | | |     e
 * | | |    
 * | | * commit 42b7311aa626e712891940c1ec5d5cba201946a4
 * | | | Author: Scott J. Goldman <scottjg@github.com>
 * | | | Date:   Tue Nov 27 20:28:06 2012 -0800
 * | | | 
 * | | |     d
 * | | |      
 * | | *   commit a953a018c5b10b20c86e69fef55ebc8ad4c5a417
 * | | |\  Merge: bd1732c cdf97fd
 * | | |/  Author: Scott J. Goldman <scottjg@github.com>
 * | |/|   Date:   Tue Nov 27 20:26:43 2012 -0800
 * | | |   
 * | | |       Merge branch 'first-branch'
 * | | |    
 * | * | commit cdf97fd3bb48eb3827638bb33d208f5fd32d0aa6
 * | | | Author: Scott J. Goldman <scottjg@github.com>
 * | | | Date:   Tue Nov 27 20:24:46 2012 -0800
 * | | | 
 * | | |     g
 * | | |    
 * | * | commit ef0488f0b722f0be8bcb90a7730ac7efafd1d694
 * | | | Author: Scott J. Goldman <scottjg@github.com>
 * | | | Date:   Tue Nov 27 20:24:39 2012 -0800
 * | | | 
 * | | |     f
 * | | |    
 * | | * commit bd1732c43c68d712ad09e1d872b9be6d4b9efdc4
 * | |/  Author: Scott J. Goldman <scottjg@github.com>
 * | |   Date:   Tue Nov 27 17:43:58 2012 -0800
 * | |   
 * | |       c
 * | |   
 * | * commit 0c8a3f1f3d5f421cf83048c7c73ee3b55a5e0f29
 * |/  Author: Scott J. Goldman <scottjg@github.com>
 * |   Date:   Tue Nov 27 17:43:48 2012 -0800
 * |   
 * |       b
 * |  
 * * commit 1f4c0311a24b63f6fc209a59a1e404942d4a5006
 *   Author: Scott J. Goldman <scottjg@github.com>
 *   Date:   Tue Nov 27 17:43:41 2012 -0800
 *
 *       a
 */

void test_revwalk_mergebase__remove_redundant(void)
{
	git_repository *repo;
	git_oid one, two, base;
	git_oidarray result = {NULL, 0};

	cl_git_pass(git_repository_open(&repo, cl_fixture("redundant.git")));

	cl_git_pass(git_oid_fromstr(&one, "d89137c93ba1ee749214ff4ce52ae9137bc833f9"));
	cl_git_pass(git_oid_fromstr(&two, "91f4b95df4a59504a9813ba66912562931d990e3"));
	cl_git_pass(git_oid_fromstr(&base, "6cb1f2352d974e1c5a776093017e8772416ac97a"));

	cl_git_pass(git_merge_bases(&result, repo, &one, &two));
	cl_assert_equal_i(1, result.count);
	cl_assert_equal_oid(&base, &result.ids[0]);

	git_oidarray_free(&result);
	git_repository_free(repo);
}
