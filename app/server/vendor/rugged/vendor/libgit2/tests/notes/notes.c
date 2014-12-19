#include "clar_libgit2.h"

static git_repository *_repo;
static git_signature *_sig;

void test_notes_notes__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_signature_now(&_sig, "alice", "alice@example.com"));
}

void test_notes_notes__cleanup(void)
{
	git_signature_free(_sig);
	_sig = NULL;

	cl_git_sandbox_cleanup();
}

static void assert_note_equal(git_note *note, char *message, git_oid *note_oid) {
	git_blob *blob;

	cl_assert_equal_s(git_note_message(note), message);
	cl_assert_equal_oid(git_note_id(note), note_oid);

	cl_git_pass(git_blob_lookup(&blob, _repo, note_oid));
	cl_assert_equal_s(git_note_message(note), (const char *)git_blob_rawcontent(blob));

	git_blob_free(blob);
}

static void create_note(git_oid *note_oid, const char *canonical_namespace, const char *target_sha, const char *message)
{
	git_oid oid;

	cl_git_pass(git_oid_fromstr(&oid, target_sha));
	cl_git_pass(git_note_create(note_oid, _repo, canonical_namespace, _sig, _sig, &oid, message, 0));
}

static struct {
	const char *note_sha;
	const char *annotated_object_sha;
}
list_expectations[] = {
	{ "1c73b1f51762155d357bcd1fd4f2c409ef80065b", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045" },
	{ "1c73b1f51762155d357bcd1fd4f2c409ef80065b", "9fd738e8f7967c078dceed8190330fc8648ee56a" },
	{ "257b43746b6b46caa4aa788376c647cce0a33e2b", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750" },
	{ "1ec1c8e03f461f4f5d3f3702172483662e7223f3", "c47800c7266a2be04c571c04d5a6614691ea99bd" },
	{ NULL, NULL }
};

#define EXPECTATIONS_COUNT (sizeof(list_expectations)/sizeof(list_expectations[0])) - 1

static int note_list_cb(
	const git_oid *blob_id, const git_oid *annotated_obj_id, void *payload)
{
	git_oid expected_note_oid, expected_target_oid;

	unsigned int *count = (unsigned int *)payload;

	cl_assert(*count < EXPECTATIONS_COUNT);

	cl_git_pass(git_oid_fromstr(&expected_note_oid, list_expectations[*count].note_sha));
	cl_assert_equal_oid(&expected_note_oid, blob_id);

	cl_git_pass(git_oid_fromstr(&expected_target_oid, list_expectations[*count].annotated_object_sha));
	cl_assert_equal_oid(&expected_target_oid, annotated_obj_id);

	(*count)++;

	return 0;
}

/*
 * $ git notes --ref i-can-see-dead-notes add -m "I decorate a65f" a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * $ git notes --ref i-can-see-dead-notes add -m "I decorate c478" c47800c7266a2be04c571c04d5a6614691ea99bd
 * $ git notes --ref i-can-see-dead-notes add -m "I decorate 9fd7 and 4a20" 9fd738e8f7967c078dceed8190330fc8648ee56a
 * $ git notes --ref i-can-see-dead-notes add -m "I decorate 9fd7 and 4a20" 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 *
 * $ git notes --ref i-can-see-dead-notes list
 * 1c73b1f51762155d357bcd1fd4f2c409ef80065b 4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 * 1c73b1f51762155d357bcd1fd4f2c409ef80065b 9fd738e8f7967c078dceed8190330fc8648ee56a
 * 257b43746b6b46caa4aa788376c647cce0a33e2b a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * 1ec1c8e03f461f4f5d3f3702172483662e7223f3 c47800c7266a2be04c571c04d5a6614691ea99bd
 *
 * $ git ls-tree refs/notes/i-can-see-dead-notes
 * 100644 blob 1c73b1f51762155d357bcd1fd4f2c409ef80065b    4a202b346bb0fb0db7eff3cffeb3c70babbd2045
 * 100644 blob 1c73b1f51762155d357bcd1fd4f2c409ef80065b    9fd738e8f7967c078dceed8190330fc8648ee56a
 * 100644 blob 257b43746b6b46caa4aa788376c647cce0a33e2b    a65fedf39aefe402d3bb6e24df4d4f5fe4547750
 * 100644 blob 1ec1c8e03f461f4f5d3f3702172483662e7223f3    c47800c7266a2be04c571c04d5a6614691ea99bd
*/
void test_notes_notes__can_retrieve_a_list_of_notes_for_a_given_namespace(void)
{
	git_oid note_oid1, note_oid2, note_oid3, note_oid4;
	unsigned int retrieved_notes = 0;

	create_note(&note_oid1, "refs/notes/i-can-see-dead-notes", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", "I decorate a65f\n");
	create_note(&note_oid2, "refs/notes/i-can-see-dead-notes", "c47800c7266a2be04c571c04d5a6614691ea99bd", "I decorate c478\n");
	create_note(&note_oid3, "refs/notes/i-can-see-dead-notes", "9fd738e8f7967c078dceed8190330fc8648ee56a", "I decorate 9fd7 and 4a20\n");
	create_note(&note_oid4, "refs/notes/i-can-see-dead-notes", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045", "I decorate 9fd7 and 4a20\n");

	cl_git_pass(git_note_foreach
(_repo, "refs/notes/i-can-see-dead-notes", note_list_cb, &retrieved_notes));

	cl_assert_equal_i(4, retrieved_notes);
}

static int note_cancel_cb(
	const git_oid *blob_id, const git_oid *annotated_obj_id, void *payload)
{
	unsigned int *count = (unsigned int *)payload;

	GIT_UNUSED(blob_id);
	GIT_UNUSED(annotated_obj_id);

	(*count)++;

	return (*count > 2);
}

void test_notes_notes__can_cancel_foreach(void)
{
	git_oid note_oid1, note_oid2, note_oid3, note_oid4;
	unsigned int retrieved_notes = 0;

	create_note(&note_oid1, "refs/notes/i-can-see-dead-notes", "a65fedf39aefe402d3bb6e24df4d4f5fe4547750", "I decorate a65f\n");
	create_note(&note_oid2, "refs/notes/i-can-see-dead-notes", "c47800c7266a2be04c571c04d5a6614691ea99bd", "I decorate c478\n");
	create_note(&note_oid3, "refs/notes/i-can-see-dead-notes", "9fd738e8f7967c078dceed8190330fc8648ee56a", "I decorate 9fd7 and 4a20\n");
	create_note(&note_oid4, "refs/notes/i-can-see-dead-notes", "4a202b346bb0fb0db7eff3cffeb3c70babbd2045", "I decorate 9fd7 and 4a20\n");

	cl_assert_equal_i(
		1,
		git_note_foreach(_repo, "refs/notes/i-can-see-dead-notes",
			note_cancel_cb, &retrieved_notes));
}

void test_notes_notes__retrieving_a_list_of_notes_for_an_unknown_namespace_returns_ENOTFOUND(void)
{
	int error;
	unsigned int retrieved_notes = 0;

	error = git_note_foreach(_repo, "refs/notes/i-am-not", note_list_cb, &retrieved_notes);
	cl_git_fail(error);
	cl_assert_equal_i(GIT_ENOTFOUND, error);

	cl_assert_equal_i(0, retrieved_notes);
}

void test_notes_notes__inserting_a_note_without_passing_a_namespace_uses_the_default_namespace(void)
{
	git_oid note_oid, target_oid;
	git_note *note, *default_namespace_note;
	const char *default_ref;

	cl_git_pass(git_oid_fromstr(&target_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));
	cl_git_pass(git_note_default_ref(&default_ref, _repo));

	create_note(&note_oid, NULL, "08b041783f40edfe12bb406c9c9a8a040177c125", "hello world\n");

	cl_git_pass(git_note_read(&note, _repo, NULL, &target_oid));
	cl_git_pass(git_note_read(&default_namespace_note, _repo, default_ref, &target_oid));

	assert_note_equal(note, "hello world\n", &note_oid);
	assert_note_equal(default_namespace_note, "hello world\n", &note_oid);

	git_note_free(note);
	git_note_free(default_namespace_note);
}

void test_notes_notes__can_insert_a_note_with_a_custom_namespace(void)
{
	git_oid note_oid, target_oid;
	git_note *note;

	cl_git_pass(git_oid_fromstr(&target_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));

	create_note(&note_oid, "refs/notes/some/namespace", "08b041783f40edfe12bb406c9c9a8a040177c125", "hello world on a custom namespace\n");

	cl_git_pass(git_note_read(&note, _repo, "refs/notes/some/namespace", &target_oid));

	assert_note_equal(note, "hello world on a custom namespace\n", &note_oid);

	git_note_free(note);
}

/*
 * $ git notes --ref fanout list 8496071c1b46c854b31185ea97743be6a8774479
 * 08b041783f40edfe12bb406c9c9a8a040177c125
 */
void test_notes_notes__creating_a_note_on_a_target_which_already_has_one_returns_EEXISTS(void)
{
	int error;
	git_oid note_oid, target_oid;

	cl_git_pass(git_oid_fromstr(&target_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));

	create_note(&note_oid, NULL, "08b041783f40edfe12bb406c9c9a8a040177c125", "hello world\n");
	error = git_note_create(&note_oid, _repo, NULL, _sig, _sig, &target_oid, "hello world\n", 0);
	cl_git_fail(error);
	cl_assert_equal_i(GIT_EEXISTS, error);

	create_note(&note_oid, "refs/notes/some/namespace", "08b041783f40edfe12bb406c9c9a8a040177c125", "hello world\n");
	error = git_note_create(&note_oid, _repo, "refs/notes/some/namespace", _sig, _sig, &target_oid, "hello world\n", 0);
	cl_git_fail(error);
	cl_assert_equal_i(GIT_EEXISTS, error);
}


void test_notes_notes__creating_a_note_on_a_target_can_overwrite_existing_note(void)
{
	git_oid note_oid, target_oid;
	git_note *note, *namespace_note;

	cl_git_pass(git_oid_fromstr(&target_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));

	create_note(&note_oid, NULL, "08b041783f40edfe12bb406c9c9a8a040177c125", "hello old world\n");
	cl_git_pass(git_note_create(&note_oid, _repo, NULL, _sig, _sig, &target_oid, "hello new world\n", 1));

	cl_git_pass(git_note_read(&note, _repo, NULL, &target_oid));
	assert_note_equal(note, "hello new world\n", &note_oid);

	create_note(&note_oid, "refs/notes/some/namespace", "08b041783f40edfe12bb406c9c9a8a040177c125", "hello old world\n");
	cl_git_pass(git_note_create(&note_oid, _repo, "refs/notes/some/namespace", _sig, _sig, &target_oid, "hello new ref world\n", 1));

	cl_git_pass(git_note_read(&namespace_note, _repo, "refs/notes/some/namespace", &target_oid));
	assert_note_equal(namespace_note, "hello new ref world\n", &note_oid);

	git_note_free(note);
	git_note_free(namespace_note);
}

static char *messages[] = {
	"08c041783f40edfe12bb406c9c9a8a040177c125",
	"96c45fbe09ab7445fc7c60fd8d17f32494399343",
	"48cc7e38dcfc1ec87e70ec03e08c3e83d7a16aa1",
	"24c3eaafb681c3df668f9df96f58e7b8c756eb04",
	"96ca1b6ccc7858ae94684777f85ac0e7447f7040",
	"7ac2db4378a08bb244a427c357e0082ee0d57ac6",
	"e6cba23dbf4ef84fe35e884f017f4e24dc228572",
	"c8cf3462c7d8feba716deeb2ebe6583bd54589e2",
	"39c16b9834c2d665ac5f68ad91dc5b933bad8549",
	"f3c582b1397df6a664224ebbaf9d4cc952706597",
	"29cec67037fe8e89977474988219016ae7f342a6",
	"36c4cd238bf8e82e27b740e0741b025f2e8c79ab",
	"f1c45a47c02e01d5a9a326f1d9f7f756373387f8",
	"4aca84406f5daee34ab513a60717c8d7b1763ead",
	"84ce167da452552f63ed8407b55d5ece4901845f",
	NULL
};

#define MESSAGES_COUNT (sizeof(messages)/sizeof(messages[0])) - 1

/*
 * $ git ls-tree refs/notes/fanout
 * 040000 tree 4b22b35d44b5a4f589edf3dc89196399771796ea    84
 *
 * $ git ls-tree 4b22b35
 * 040000 tree d71aab4f9b04b45ce09bcaa636a9be6231474759    96
 *
 * $ git ls-tree d71aab4
 * 100644 blob 08b041783f40edfe12bb406c9c9a8a040177c125    071c1b46c854b31185ea97743be6a8774479
 */
void test_notes_notes__can_insert_a_note_in_an_existing_fanout(void)
{
	size_t i;
	git_oid note_oid, target_oid;
	git_note *_note;

	cl_git_pass(git_oid_fromstr(&target_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));
	
	for (i = 0; i <  MESSAGES_COUNT; i++) {
		cl_git_pass(git_note_create(&note_oid, _repo, "refs/notes/fanout", _sig, _sig, &target_oid, messages[i], 0));
		cl_git_pass(git_note_read(&_note, _repo, "refs/notes/fanout", &target_oid));
		git_note_free(_note);

		git_oid_cpy(&target_oid, &note_oid);
	}
}

/*
 * $ git notes --ref fanout list 8496071c1b46c854b31185ea97743be6a8774479
 * 08b041783f40edfe12bb406c9c9a8a040177c125
 */
void test_notes_notes__can_read_a_note_in_an_existing_fanout(void)
{
	git_oid note_oid, target_oid;
	git_note *note;

	cl_git_pass(git_oid_fromstr(&target_oid, "8496071c1b46c854b31185ea97743be6a8774479"));
	cl_git_pass(git_note_read(&note, _repo, "refs/notes/fanout", &target_oid));

	cl_git_pass(git_oid_fromstr(&note_oid, "08b041783f40edfe12bb406c9c9a8a040177c125"));
	cl_assert_equal_oid(git_note_id(note), &note_oid);

	git_note_free(note);
}

void test_notes_notes__can_remove_a_note_in_an_existing_fanout(void)
{
	git_oid target_oid;
	git_note *note;

	cl_git_pass(git_oid_fromstr(&target_oid, "8496071c1b46c854b31185ea97743be6a8774479"));
	cl_git_pass(git_note_remove(_repo, "refs/notes/fanout", _sig, _sig, &target_oid));

	cl_git_fail(git_note_read(&note, _repo, "refs/notes/fanout", &target_oid));
}

void test_notes_notes__removing_a_note_which_doesnt_exists_returns_ENOTFOUND(void)
{
	int error;
	git_oid target_oid;

	cl_git_pass(git_oid_fromstr(&target_oid, "8496071c1b46c854b31185ea97743be6a8774479"));
	cl_git_pass(git_note_remove(_repo, "refs/notes/fanout", _sig, _sig, &target_oid));
	
	error = git_note_remove(_repo, "refs/notes/fanout", _sig, _sig, &target_oid);
	cl_git_fail(error);
	cl_assert_equal_i(GIT_ENOTFOUND, error);
}

void test_notes_notes__can_iterate_default_namespace(void)
{
	git_note_iterator *iter;
	git_note *note;
	git_oid note_id, annotated_id;
	git_oid note_created[2];
	const char* note_message[] = {
		"I decorate a65f\n",
		"I decorate c478\n"
	};
	int i, err;

	create_note(&note_created[0], "refs/notes/commits",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", note_message[0]);
	create_note(&note_created[1], "refs/notes/commits",
		"c47800c7266a2be04c571c04d5a6614691ea99bd", note_message[1]);

	cl_git_pass(git_note_iterator_new(&iter, _repo, NULL));

	for (i = 0; (err = git_note_next(&note_id, &annotated_id, iter)) >= 0; ++i) {
		cl_git_pass(git_note_read(&note, _repo, NULL, &annotated_id));
		cl_assert_equal_s(git_note_message(note), note_message[i]);
		git_note_free(note);
	}

	cl_assert_equal_i(GIT_ITEROVER, err);
	cl_assert_equal_i(2, i);
	git_note_iterator_free(iter);
}

void test_notes_notes__can_iterate_custom_namespace(void)
{
	git_note_iterator *iter;
	git_note *note;
	git_oid note_id, annotated_id;
	git_oid note_created[2];
	const char* note_message[] = {
		"I decorate a65f\n",
		"I decorate c478\n"
	};
	int i, err;

	create_note(&note_created[0], "refs/notes/beer",
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", note_message[0]);
	create_note(&note_created[1], "refs/notes/beer",
		"c47800c7266a2be04c571c04d5a6614691ea99bd", note_message[1]);

	cl_git_pass(git_note_iterator_new(&iter, _repo, "refs/notes/beer"));

	for (i = 0; (err = git_note_next(&note_id, &annotated_id, iter)) >= 0; ++i) {
		cl_git_pass(git_note_read(&note, _repo, "refs/notes/beer", &annotated_id));
		cl_assert_equal_s(git_note_message(note), note_message[i]);
		git_note_free(note);
	}

	cl_assert_equal_i(GIT_ITEROVER, err);
	cl_assert_equal_i(2, i);
	git_note_iterator_free(iter);
}

void test_notes_notes__empty_iterate(void)
{
	git_note_iterator *iter;

	cl_git_fail(git_note_iterator_new(&iter, _repo, "refs/notes/commits"));
}
