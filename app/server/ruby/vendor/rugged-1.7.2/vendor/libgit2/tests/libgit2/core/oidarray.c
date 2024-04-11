#include "clar_libgit2.h"

#include "git2/oid.h"
#include "git2/transport.h"

#include "common.h"
#include "transports/smart.h"
#include "oid.h"
#include "oidarray.h"

#include <assert.h>

#define oid_0 "c070ad8c08840c8116da865b2d65593a6bb9cd2a"
#define oid_1 "0966a434eb1a025db6b71485ab63a3bfbea520b6"
#define oid_2 "83834a7afdaa1a1260568567f6ad90020389f664"
#define oid_3 "746fb4c91a7b6190bc4761adf7410afc4b59812c"

void test_core_oidarray__add_and_remove_oid_from_shallowarray(void)
{
	git_oid oid_0_obj, oid_1_obj, oid_2_obj, oid_3_obj;
	git_array_oid_t array = GIT_ARRAY_INIT;

	git_oid__fromstr(&oid_0_obj, oid_0, GIT_OID_SHA1);
	git_oid__fromstr(&oid_1_obj, oid_1, GIT_OID_SHA1);
	git_oid__fromstr(&oid_2_obj, oid_2, GIT_OID_SHA1);
	git_oid__fromstr(&oid_3_obj, oid_3, GIT_OID_SHA1);

	/* add some initial ids */
	git_oidarray__add(&array, &oid_0_obj);
	git_oidarray__add(&array, &oid_1_obj);
	git_oidarray__add(&array, &oid_2_obj);

	cl_assert_equal_i(3, array.size);
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[1]));
	cl_assert_equal_s("83834a7afdaa1a1260568567f6ad90020389f664", git_oid_tostr_s(&array.ptr[2]));

	/* don't duplicate existing ids */
	git_oidarray__add(&array, &oid_1_obj);

	cl_assert_equal_i(3, array.size);
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[1]));
	cl_assert_equal_s("83834a7afdaa1a1260568567f6ad90020389f664", git_oid_tostr_s(&array.ptr[2]));

	/* remove the last id */
	cl_assert_equal_i(1, git_oidarray__remove(&array, &oid_2_obj));

	cl_assert_equal_i(2, array.size);
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[1]));

	/* add another id */
	git_oidarray__add(&array, &oid_3_obj);

	cl_assert_equal_i(3, array.size);
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[1]));
	cl_assert_equal_s("746fb4c91a7b6190bc4761adf7410afc4b59812c", git_oid_tostr_s(&array.ptr[2]));

	/* remove the first id */
	cl_assert_equal_i(1, git_oidarray__remove(&array, &oid_0_obj));

	cl_assert_equal_i(2, array.size);
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("746fb4c91a7b6190bc4761adf7410afc4b59812c", git_oid_tostr_s(&array.ptr[1]));

	/* removing a nonexistent oid does nothing */
	cl_assert_equal_i(0, git_oidarray__remove(&array, &oid_2_obj));

	/* add another id */
	git_oidarray__add(&array, &oid_0_obj);

	cl_assert_equal_i(3, array.size);
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("746fb4c91a7b6190bc4761adf7410afc4b59812c", git_oid_tostr_s(&array.ptr[1]));
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[2]));

	/* remove another id */
	cl_assert_equal_i(1, git_oidarray__remove(&array, &oid_3_obj));

	cl_assert_equal_i(2, array.size);
	cl_assert_equal_s("0966a434eb1a025db6b71485ab63a3bfbea520b6", git_oid_tostr_s(&array.ptr[0]));
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[1]));

	/* remove another id */
	cl_assert_equal_i(1, git_oidarray__remove(&array, &oid_1_obj));

	cl_assert_equal_i(1, array.size);
	cl_assert_equal_s("c070ad8c08840c8116da865b2d65593a6bb9cd2a", git_oid_tostr_s(&array.ptr[0]));

	/* remove the final id */
	cl_assert_equal_i(1, git_oidarray__remove(&array, &oid_0_obj));

	cl_assert_equal_i(0, array.size);

	git_array_clear(array);
}
