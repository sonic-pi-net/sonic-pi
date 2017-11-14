
#include "clar_libgit2.h"

#include "odb.h"
#include "hash.h"

#include "data.h"

static void hash_object_pass(git_oid *oid, git_rawobj *obj)
{
	cl_git_pass(git_odb_hash(oid, obj->data, obj->len, obj->type));
}
static void hash_object_fail(git_oid *oid, git_rawobj *obj)
{
	cl_git_fail(git_odb_hash(oid, obj->data, obj->len, obj->type));
}

static char *hello_id = "22596363b3de40b06f981fb85d82312e8c0ed511";
static char *hello_text = "hello world\n";

static char *bye_id = "ce08fe4884650f067bd5703b6a59a8b3b3c99a09";
static char *bye_text = "bye world\n";

void test_object_raw_hash__hash_by_blocks(void)
{
	git_hash_ctx ctx;
	git_oid id1, id2;

	cl_git_pass(git_hash_ctx_init(&ctx));

	/* should already be init'd */
	cl_git_pass(git_hash_update(&ctx, hello_text, strlen(hello_text)));
	cl_git_pass(git_hash_final(&id2, &ctx));
	cl_git_pass(git_oid_fromstr(&id1, hello_id));
	cl_assert(git_oid_cmp(&id1, &id2) == 0);

	/* reinit should permit reuse */
	cl_git_pass(git_hash_init(&ctx));
	cl_git_pass(git_hash_update(&ctx, bye_text, strlen(bye_text)));
	cl_git_pass(git_hash_final(&id2, &ctx));
	cl_git_pass(git_oid_fromstr(&id1, bye_id));
	cl_assert(git_oid_cmp(&id1, &id2) == 0);

	git_hash_ctx_cleanup(&ctx);
}

void test_object_raw_hash__hash_buffer_in_single_call(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, hello_id));
	git_hash_buf(&id2, hello_text, strlen(hello_text));
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_vector(void)
{
	git_oid id1, id2;
	git_buf_vec vec[2];

	cl_git_pass(git_oid_fromstr(&id1, hello_id));

	vec[0].data = hello_text;
	vec[0].len  = 4;
	vec[1].data = hello_text+4;
	vec[1].len  = strlen(hello_text)-4;

	git_hash_vec(&id2, vec, 2);

	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_junk_data(void)
{
	git_oid id, id_zero;

	cl_git_pass(git_oid_fromstr(&id_zero, zero_id));

	/* invalid types: */
	junk_obj.data = some_data;
	hash_object_fail(&id, &junk_obj);

	junk_obj.type = GIT_OBJ__EXT1;
	hash_object_fail(&id, &junk_obj);

	junk_obj.type = GIT_OBJ__EXT2;
	hash_object_fail(&id, &junk_obj);

	junk_obj.type = GIT_OBJ_OFS_DELTA;
	hash_object_fail(&id, &junk_obj);

	junk_obj.type = GIT_OBJ_REF_DELTA;
	hash_object_fail(&id, &junk_obj);

	/* data can be NULL only if len is zero: */
	junk_obj.type = GIT_OBJ_BLOB;
	junk_obj.data = NULL;
	hash_object_pass(&id, &junk_obj);
	cl_assert(git_oid_cmp(&id, &id_zero) == 0);

	junk_obj.len = 1;
	hash_object_fail(&id, &junk_obj);
}

void test_object_raw_hash__hash_commit_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, commit_id));
	hash_object_pass(&id2, &commit_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_tree_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, tree_id));
	hash_object_pass(&id2, &tree_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_tag_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, tag_id));
	hash_object_pass(&id2, &tag_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_zero_length_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, zero_id));
	hash_object_pass(&id2, &zero_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_one_byte_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, one_id));
	hash_object_pass(&id2, &one_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_two_byte_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, two_id));
	hash_object_pass(&id2, &two_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}

void test_object_raw_hash__hash_multi_byte_object(void)
{
	git_oid id1, id2;

	cl_git_pass(git_oid_fromstr(&id1, some_id));
	hash_object_pass(&id2, &some_obj);
	cl_assert(git_oid_cmp(&id1, &id2) == 0);
}
