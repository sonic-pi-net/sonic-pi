
#include "clar_libgit2.h"
#include "buffer.h"
#include "vector.h"
#include "push_util.h"

const git_oid OID_ZERO = {{ 0 }};

void updated_tip_free(updated_tip *t)
{
	git__free(t->name);
	git__free(t);
}

void push_status_free(push_status *s)
{
	git__free(s->ref);
	git__free(s->msg);
	git__free(s);
}

void record_callbacks_data_clear(record_callbacks_data *data)
{
	size_t i;
	updated_tip *tip;
	push_status *status;

	git_vector_foreach(&data->updated_tips, i, tip)
		updated_tip_free(tip);

	git_vector_free(&data->updated_tips);

	git_vector_foreach(&data->statuses, i, status)
		push_status_free(status);

	git_vector_free(&data->statuses);

	data->pack_progress_calls = 0;
	data->transfer_progress_calls = 0;
}

int record_update_tips_cb(const char *refname, const git_oid *a, const git_oid *b, void *data)
{
	updated_tip *t;
	record_callbacks_data *record_data = (record_callbacks_data *)data;

	cl_assert(t = git__calloc(1, sizeof(*t)));

	cl_assert(t->name = git__strdup(refname));
	git_oid_cpy(&t->old_oid, a);
	git_oid_cpy(&t->new_oid, b);

	git_vector_insert(&record_data->updated_tips, t);

	return 0;
}

int create_deletion_refspecs(git_vector *out, const git_remote_head **heads, size_t heads_len)
{
	git_buf del_spec = GIT_BUF_INIT;
	size_t i;

	for (i = 0; i < heads_len; i++) {
		const git_remote_head *head = heads[i];
		/* Ignore malformed ref names (which also saves us from tag^{} */
		if (!git_reference_is_valid_name(head->name))
			return 0;

		/* Create a refspec that deletes a branch in the remote */
		if (strcmp(head->name, "refs/heads/master")) {
			cl_git_pass(git_buf_putc(&del_spec, ':'));
			cl_git_pass(git_buf_puts(&del_spec, head->name));
			cl_git_pass(git_vector_insert(out, git_buf_detach(&del_spec)));
		}
	}

	return 0;
}

int record_ref_cb(git_remote_head *head, void *payload)
{
	git_vector *refs = (git_vector *) payload;
	return git_vector_insert(refs, head);
}

void verify_remote_refs(const git_remote_head *actual_refs[], size_t actual_refs_len, const expected_ref expected_refs[], size_t expected_refs_len)
{
	size_t i, j = 0;
	git_buf msg = GIT_BUF_INIT;
	const git_remote_head *actual;
	char *oid_str;
	bool master_present = false;

	/* We don't care whether "master" is present on the other end or not */
	for (i = 0; i < actual_refs_len; i++) {
		actual = actual_refs[i];
		if (!strcmp(actual->name, "refs/heads/master")) {
			master_present = true;
			break;
		}
	}

	if (expected_refs_len + (master_present ? 1 : 0) != actual_refs_len)
		goto failed;

	for (i = 0; i < actual_refs_len; i++) {
		actual = actual_refs[i];
		if (master_present && !strcmp(actual->name, "refs/heads/master"))
			continue;

		if (strcmp(expected_refs[j].name, actual->name) ||
			git_oid_cmp(expected_refs[j].oid, &actual->oid))
			goto failed;

		j++;
	}

	return;

failed:
	git_buf_puts(&msg, "Expected and actual refs differ:\nEXPECTED:\n");

	for(i = 0; i < expected_refs_len; i++) {
		oid_str = git_oid_tostr_s(expected_refs[i].oid);
		cl_git_pass(git_buf_printf(&msg, "%s = %s\n", expected_refs[i].name, oid_str));
	}

	git_buf_puts(&msg, "\nACTUAL:\n");
	for (i = 0; i < actual_refs_len; i++) {
		actual = actual_refs[i];
		if (master_present && !strcmp(actual->name, "refs/heads/master"))
			continue;

		oid_str = git_oid_tostr_s(&actual->oid);
		cl_git_pass(git_buf_printf(&msg, "%s = %s\n", actual->name, oid_str));
	}

	cl_fail(git_buf_cstr(&msg));

	git_buf_dispose(&msg);
}
