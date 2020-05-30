#include "clar_libgit2.h"

git_repository *_repo;

void test_object_shortid__initialize(void)
{
	cl_git_pass(git_repository_open(&_repo, cl_fixture("duplicate.git")));
}

void test_object_shortid__cleanup(void)
{
	git_repository_free(_repo);
	_repo = NULL;
}

void test_object_shortid__select(void)
{
	git_oid full;
	git_object *obj;
	git_buf shorty = {0};

	git_oid_fromstr(&full, "ce013625030ba8dba906f756967f9e9ca394464a");
	cl_git_pass(git_object_lookup(&obj, _repo, &full, GIT_OBJECT_ANY));
	cl_git_pass(git_object_short_id(&shorty, obj));
	cl_assert_equal_i(7, shorty.size);
	cl_assert_equal_s("ce01362", shorty.ptr);
	git_object_free(obj);

	git_oid_fromstr(&full, "038d718da6a1ebbc6a7780a96ed75a70cc2ad6e2");
	cl_git_pass(git_object_lookup(&obj, _repo, &full, GIT_OBJECT_ANY));
	cl_git_pass(git_object_short_id(&shorty, obj));
	cl_assert_equal_i(7, shorty.size);
	cl_assert_equal_s("038d718", shorty.ptr);
	git_object_free(obj);

	git_oid_fromstr(&full, "dea509d097ce692e167dfc6a48a7a280cc5e877e");
	cl_git_pass(git_object_lookup(&obj, _repo, &full, GIT_OBJECT_ANY));
	cl_git_pass(git_object_short_id(&shorty, obj));
	cl_assert_equal_i(9, shorty.size);
	cl_assert_equal_s("dea509d09", shorty.ptr);
	git_object_free(obj);

	git_oid_fromstr(&full, "dea509d0b3cb8ee0650f6ca210bc83f4678851ba");
	cl_git_pass(git_object_lookup(&obj, _repo, &full, GIT_OBJECT_ANY));
	cl_git_pass(git_object_short_id(&shorty, obj));
	cl_assert_equal_i(9, shorty.size);
	cl_assert_equal_s("dea509d0b", shorty.ptr);
	git_object_free(obj);

	git_buf_dispose(&shorty);
}
