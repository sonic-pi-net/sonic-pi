#include "clar_libgit2.h"

#include "notes.h"
#include "buffer.h"

static git_repository *_repo;
static git_note *_note;
static git_signature *_sig;
static git_config *_cfg;

void test_notes_notesref__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&_repo, "testrepo.git"));
}

void test_notes_notesref__cleanup(void)
{
	git_note_free(_note);
	_note = NULL;

	git_signature_free(_sig);
	_sig = NULL;

	git_config_free(_cfg);
	_cfg = NULL;

	git_repository_free(_repo);
	_repo = NULL;

	cl_fixture_cleanup("testrepo.git");
}

void test_notes_notesref__config_corenotesref(void)
{
	git_oid oid, note_oid;
	git_buf default_ref = GIT_BUF_INIT;

	cl_git_pass(git_signature_now(&_sig, "alice", "alice@example.com"));
	cl_git_pass(git_oid_fromstr(&oid, "8496071c1b46c854b31185ea97743be6a8774479"));

	cl_git_pass(git_repository_config(&_cfg, _repo));

	cl_git_pass(git_config_set_string(_cfg, "core.notesRef", "refs/notes/mydefaultnotesref"));

	cl_git_pass(git_note_create(&note_oid, _repo, NULL, _sig, _sig, &oid, "test123test\n", 0));

	cl_git_pass(git_note_read(&_note, _repo, NULL, &oid));
	cl_assert_equal_s("test123test\n", git_note_message(_note));
	cl_assert_equal_oid(git_note_id(_note), &note_oid);

	git_note_free(_note);

	cl_git_pass(git_note_read(&_note, _repo, "refs/notes/mydefaultnotesref", &oid));
	cl_assert_equal_s("test123test\n", git_note_message(_note));
	cl_assert_equal_oid(git_note_id(_note), &note_oid);

	cl_git_pass(git_note_default_ref(&default_ref, _repo));
	cl_assert_equal_s("refs/notes/mydefaultnotesref", default_ref.ptr);
	git_buf_clear(&default_ref);

	cl_git_pass(git_config_delete_entry(_cfg, "core.notesRef"));

	cl_git_pass(git_note_default_ref(&default_ref, _repo));
	cl_assert_equal_s(GIT_NOTES_DEFAULT_REF, default_ref.ptr);

	git_buf_dispose(&default_ref);
}
