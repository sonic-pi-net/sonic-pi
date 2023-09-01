#include "clar_libgit2.h"

static const char* tagger_name = "Vicent Marti";
static const char* tagger_email = "vicent@github.com";
static const char* tagger_message = "This is my tag.\n\nThere are many tags, but this one is mine\n";

static const char *tag2_id = "7b4384978d2493e851f9cca7858815fac9b10980";
static const char *tagged_commit = "e90810b8df3e80c413d903f631643c716887138d";

static git_repository *g_repo;

/* Fixture setup and teardown */
void test_object_tag_write__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tag_write__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_object_tag_write__basic(void)
{
	/* write a tag to the repository and read it again */
	git_tag *tag;
	git_oid target_id, tag_id;
	git_signature *tagger;
	const git_signature *tagger1;
	git_reference *ref_tag;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	/* create signature */
	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_git_pass(
		git_tag_create(&tag_id, g_repo,
		  "the-tag", target, tagger, tagger_message, 0)
	);

	git_object_free(target);
	git_signature_free(tagger);

	cl_git_pass(git_tag_lookup(&tag, g_repo, &tag_id));
	cl_assert(git_oid_cmp(git_tag_target_id(tag), &target_id) == 0);

	/* Check attributes were set correctly */
	tagger1 = git_tag_tagger(tag);
	cl_assert(tagger1 != NULL);
	cl_assert_equal_s(tagger1->name, tagger_name);
	cl_assert_equal_s(tagger1->email, tagger_email);
	cl_assert(tagger1->when.time == 123456789);
	cl_assert(tagger1->when.offset == 60);

	cl_assert_equal_s(git_tag_message(tag), tagger_message);

	cl_git_pass(git_reference_lookup(&ref_tag, g_repo, "refs/tags/the-tag"));
	cl_assert(git_oid_cmp(git_reference_target(ref_tag), &tag_id) == 0);
	cl_git_pass(git_reference_delete(ref_tag));
	git_reference_free(ref_tag);

	git_tag_free(tag);
}

void test_object_tag_write__overwrite(void)
{
	/* Attempt to write a tag bearing the same name than an already existing tag */
	git_oid target_id, tag_id;
	git_signature *tagger;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	/* create signature */
	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_assert_equal_i(GIT_EEXISTS, git_tag_create(
                              &tag_id, /* out id */
                              g_repo,
                              "e90810b",
                              target,
                              tagger,
                              tagger_message,
                              0));

	git_object_free(target);
	git_signature_free(tagger);
}

void test_object_tag_write__replace(void)
{
	/* Replace an already existing tag */
	git_oid target_id, tag_id, old_tag_id;
	git_signature *tagger;
	git_reference *ref_tag;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	cl_git_pass(git_reference_lookup(&ref_tag, g_repo, "refs/tags/e90810b"));
	git_oid_cpy(&old_tag_id, git_reference_target(ref_tag));
	git_reference_free(ref_tag);

	/* create signature */
	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_git_pass(git_tag_create(
                              &tag_id, /* out id */
                              g_repo,
                              "e90810b",
                              target,
                              tagger,
                              tagger_message,
                              1));

	git_object_free(target);
	git_signature_free(tagger);

	cl_git_pass(git_reference_lookup(&ref_tag, g_repo, "refs/tags/e90810b"));
	cl_assert(git_oid_cmp(git_reference_target(ref_tag), &tag_id) == 0);
	cl_assert(git_oid_cmp(git_reference_target(ref_tag), &old_tag_id) != 0);

	git_reference_free(ref_tag);
}

void test_object_tag_write__lightweight(void)
{
	/* write a lightweight tag to the repository and read it again */
	git_oid target_id, object_id;
	git_reference *ref_tag;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	cl_git_pass(git_tag_create_lightweight(
                                          &object_id,
                                          g_repo,
                                          "light-tag",
                                          target,
                                          0));

	git_object_free(target);

	cl_assert(git_oid_cmp(&object_id, &target_id) == 0);

	cl_git_pass(git_reference_lookup(&ref_tag, g_repo, "refs/tags/light-tag"));
	cl_assert(git_oid_cmp(git_reference_target(ref_tag), &target_id) == 0);

	cl_git_pass(git_tag_delete(g_repo, "light-tag"));

	git_reference_free(ref_tag);
}

void test_object_tag_write__lightweight_over_existing(void)
{
	/* Attempt to write a lightweight tag bearing the same name than an already existing tag */
	git_oid target_id, object_id, existing_object_id;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	cl_assert_equal_i(GIT_EEXISTS, git_tag_create_lightweight(
                                          &object_id,
                                          g_repo,
                                          "e90810b",
                                          target,
                                          0));

	git_oid__fromstr(&existing_object_id, tag2_id, GIT_OID_SHA1);
	cl_assert(git_oid_cmp(&object_id, &existing_object_id) == 0);

	git_object_free(target);
}

void test_object_tag_write__delete(void)
{
	/* Delete an already existing tag */
	git_reference *ref_tag;

	cl_git_pass(git_tag_delete(g_repo, "e90810b"));

	cl_git_fail(git_reference_lookup(&ref_tag, g_repo, "refs/tags/e90810b"));

	git_reference_free(ref_tag);
}

void test_object_tag_write__creating_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_oid target_id, tag_id;
	git_signature *tagger;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_tag_create(&tag_id, g_repo,
		  "Inv@{id", target, tagger, tagger_message, 0)
	);

	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_tag_create_lightweight(&tag_id, g_repo,
		  "Inv@{id", target, 0)
	);

	git_object_free(target);
	git_signature_free(tagger);
}

void test_object_tag_write__deleting_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	cl_assert_equal_i(GIT_EINVALIDSPEC, git_tag_delete(g_repo, "Inv@{id"));
}

static void create_annotation(git_oid *tag_id, const char *name)
{
	git_object *target;
	git_oid target_id;
	git_signature *tagger;

	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));

	cl_git_pass(git_tag_annotation_create(tag_id, g_repo, name, target, tagger, "boom!"));
	git_object_free(target);
	git_signature_free(tagger);
}

void test_object_tag_write__creating_an_annotation_stores_the_new_object_in_the_odb(void)
{
	git_oid tag_id;
	git_tag *tag;

	create_annotation(&tag_id, "new_tag");

	cl_git_pass(git_tag_lookup(&tag, g_repo, &tag_id));
	cl_assert_equal_s("new_tag", git_tag_name(tag));

	git_tag_free(tag);
}

void test_object_tag_write__creating_an_annotation_does_not_create_a_reference(void)
{
	git_oid tag_id;
	git_reference *tag_ref;

	create_annotation(&tag_id, "new_tag");
	cl_git_fail_with(git_reference_lookup(&tag_ref, g_repo, "refs/tags/new_tag"), GIT_ENOTFOUND);
}

void test_object_tag_write__error_when_create_tag_with_invalid_name(void)
{
	git_oid target_id, tag_id;
	git_signature *tagger;
	git_object *target;

	git_oid__fromstr(&target_id, tagged_commit, GIT_OID_SHA1);
	cl_git_pass(git_object_lookup(&target, g_repo, &target_id, GIT_OBJECT_COMMIT));
	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_git_fail(
		git_tag_create(&tag_id, g_repo,
		  "-dash", target, tagger, tagger_message, 0)
	);

	git_object_free(target);
	git_signature_free(tagger);
}
