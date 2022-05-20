#include "clar_libgit2.h"

#include "repository.h"
#include "reflog.h"

static int reflog_entry_tostr(git_buf *out, const git_reflog_entry *entry)
{
	char old_oid[GIT_OID_HEXSZ], new_oid[GIT_OID_HEXSZ];

	assert(out && entry);

	git_oid_tostr((char *)&old_oid, GIT_OID_HEXSZ, git_reflog_entry_id_old(entry));
	git_oid_tostr((char *)&new_oid, GIT_OID_HEXSZ, git_reflog_entry_id_new(entry));

	return git_buf_printf(out, "%s %s %s %s", old_oid, new_oid, "somesig", git_reflog_entry_message(entry));
}

size_t reflog_entrycount(git_repository *repo, const char *name)
{
	git_reflog *log;
	size_t ret;

	cl_git_pass(git_reflog_read(&log, repo, name));
	ret = git_reflog_entrycount(log);
	git_reflog_free(log);

	return ret;
}

void cl_reflog_check_entry_(git_repository *repo, const char *reflog, size_t idx,
	const char *old_spec, const char *new_spec,
	const char *email, const char *message,
	const char *file, const char *func, int line)
{
	git_reflog *log;
	const git_reflog_entry *entry;
	git_buf result = GIT_BUF_INIT;

	cl_git_pass(git_reflog_read(&log, repo, reflog));
	entry = git_reflog_entry_byindex(log, idx);
	if (entry == NULL)
		clar__fail(file, func, line, "Reflog has no such entry", NULL, 1);

	if (old_spec) {
		git_object *obj = NULL;
		if (git_revparse_single(&obj, repo, old_spec) == GIT_OK) {
			if (git_oid_cmp(git_object_id(obj), git_reflog_entry_id_old(entry)) != 0) {
				git_oid__writebuf(&result, "\tOld OID: \"", git_object_id(obj));
				git_oid__writebuf(&result, "\" != \"", git_reflog_entry_id_old(entry));
				git_buf_puts(&result, "\"\n");
			}
			git_object_free(obj);
		} else {
			git_oid *oid = git__calloc(1, sizeof(*oid));
			git_oid_fromstr(oid, old_spec);
			if (git_oid_cmp(oid, git_reflog_entry_id_old(entry)) != 0) {
				git_oid__writebuf(&result, "\tOld OID: \"", oid);
				git_oid__writebuf(&result, "\" != \"", git_reflog_entry_id_old(entry));
				git_buf_puts(&result, "\"\n");
			}
			git__free(oid);
		}
	}
	if (new_spec) {
		git_object *obj = NULL;
		if (git_revparse_single(&obj, repo, new_spec) == GIT_OK) {
			if (git_oid_cmp(git_object_id(obj), git_reflog_entry_id_new(entry)) != 0) {
				git_oid__writebuf(&result, "\tNew OID: \"", git_object_id(obj));
				git_oid__writebuf(&result, "\" != \"", git_reflog_entry_id_new(entry));
				git_buf_puts(&result, "\"\n");
			}
			git_object_free(obj);
		} else {
			git_oid *oid = git__calloc(1, sizeof(*oid));
			git_oid_fromstr(oid, new_spec);
			if (git_oid_cmp(oid, git_reflog_entry_id_new(entry)) != 0) {
				git_oid__writebuf(&result, "\tNew OID: \"", oid);
				git_oid__writebuf(&result, "\" != \"", git_reflog_entry_id_new(entry));
				git_buf_puts(&result, "\"\n");
			}
			git__free(oid);
		}
	}

	if (email && strcmp(email, git_reflog_entry_committer(entry)->email) != 0)
		git_buf_printf(&result, "\tEmail: \"%s\" != \"%s\"\n", email, git_reflog_entry_committer(entry)->email);

	if (message) {
		const char *entry_msg = git_reflog_entry_message(entry);
		if (entry_msg == NULL) entry_msg = "";

		if (entry_msg && strcmp(message, entry_msg) != 0)
			git_buf_printf(&result, "\tMessage: \"%s\" != \"%s\"\n", message, entry_msg);
	}
	if (git_buf_len(&result) != 0)
		clar__fail(file, func, line, "Reflog entry mismatch", git_buf_cstr(&result), 1);

	git_buf_dispose(&result);
	git_reflog_free(log);
}

void reflog_print(git_repository *repo, const char *reflog_name)
{
	git_reflog *reflog;
	size_t idx;
	git_buf out = GIT_BUF_INIT;

	git_reflog_read(&reflog, repo, reflog_name);

	for (idx = 0; idx < git_reflog_entrycount(reflog); idx++) {
		const git_reflog_entry *entry = git_reflog_entry_byindex(reflog, idx);
		reflog_entry_tostr(&out, entry);
		git_buf_putc(&out, '\n');
	}

	fprintf(stderr, "%s", git_buf_cstr(&out));
	git_buf_dispose(&out);
	git_reflog_free(reflog);
}
