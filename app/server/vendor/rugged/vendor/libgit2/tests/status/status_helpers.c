#include "clar_libgit2.h"
#include "status_helpers.h"

int cb_status__normal(
	const char *path, unsigned int status_flags, void *payload)
{
	status_entry_counts *counts = payload;

	if (counts->debug)
		cb_status__print(path, status_flags, NULL);

	if (counts->entry_count >= counts->expected_entry_count)
		counts->wrong_status_flags_count++;
	else if (strcmp(path, counts->expected_paths[counts->entry_count]))
		counts->wrong_sorted_path++;
	else if (status_flags != counts->expected_statuses[counts->entry_count])
		counts->wrong_status_flags_count++;

	counts->entry_count++;
	return 0;
}

int cb_status__count(const char *p, unsigned int s, void *payload)
{
	volatile int *count = (int *)payload;

	GIT_UNUSED(p);
	GIT_UNUSED(s);

	(*count)++;

	return 0;
}

int cb_status__single(const char *p, unsigned int s, void *payload)
{
	status_entry_single *data = (status_entry_single *)payload;

	if (data->debug)
		fprintf(stderr, "%02d: %s (%04x)\n", data->count, p, s);

	data->count++;
	data->status = s;

	return 0;
}

int cb_status__print(
	const char *path, unsigned int status_flags, void *payload)
{
	char istatus = ' ', wstatus = ' ';
	int icount = 0, wcount = 0;

	if (status_flags & GIT_STATUS_INDEX_NEW) {
		istatus = 'A'; icount++;
	}
	if (status_flags & GIT_STATUS_INDEX_MODIFIED) {
		istatus = 'M'; icount++;
	}
	if (status_flags & GIT_STATUS_INDEX_DELETED) {
		istatus = 'D'; icount++;
	}
	if (status_flags & GIT_STATUS_INDEX_RENAMED) {
		istatus = 'R'; icount++;
	}
	if (status_flags & GIT_STATUS_INDEX_TYPECHANGE) {
		istatus = 'T'; icount++;
	}

	if (status_flags & GIT_STATUS_WT_NEW) {
		wstatus = 'A'; wcount++;
	}
	if (status_flags & GIT_STATUS_WT_MODIFIED) {
		wstatus = 'M'; wcount++;
	}
	if (status_flags & GIT_STATUS_WT_DELETED) {
		wstatus = 'D'; wcount++;
	}
	if (status_flags & GIT_STATUS_WT_TYPECHANGE) {
		wstatus = 'T'; wcount++;
	}
	if (status_flags & GIT_STATUS_IGNORED) {
		wstatus = 'I'; wcount++;
	}

	fprintf(stderr, "%c%c %s (%d/%d%s)\n",
			istatus, wstatus, path, icount, wcount,
			(icount > 1 || wcount > 1) ? " INVALID COMBO" : "");

	if (payload)
		*((int *)payload) += 1;

	return 0;
}
