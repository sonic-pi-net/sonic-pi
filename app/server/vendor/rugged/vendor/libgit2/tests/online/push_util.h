#ifndef INCLUDE_cl_push_util_h__
#define INCLUDE_cl_push_util_h__

#include "git2/oid.h"

/* Constant for zero oid */
extern const git_oid OID_ZERO;

/**
 * Macro for initializing git_remote_callbacks to use test helpers that
 * record data in a record_callbacks_data instance.
 * @param data pointer to a record_callbacks_data instance
 */
#define RECORD_CALLBACKS_INIT(data) \
	{ GIT_REMOTE_CALLBACKS_VERSION, NULL, NULL, cred_acquire_cb, NULL, NULL, record_update_tips_cb, NULL, NULL, NULL, data }

typedef struct {
	char *name;
	git_oid *old_oid;
	git_oid *new_oid;
} updated_tip;

typedef struct {
	git_vector updated_tips;
	git_vector statuses;
	int pack_progress_calls;
	int transfer_progress_calls;
} record_callbacks_data;

typedef struct {
	const char *name;
	const git_oid *oid;
} expected_ref;

/* the results of a push status.  when used for expected values, msg may be NULL
 * to indicate that it should not be matched. */
typedef struct {
	char *ref;
	int success;
	char *msg;
} push_status;


void updated_tip_free(updated_tip *t);

void record_callbacks_data_clear(record_callbacks_data *data);

/**
 * Callback for git_remote_update_tips that records updates
 *
 * @param data (git_vector *) of updated_tip instances
 */
int record_update_tips_cb(const char *refname, const git_oid *a, const git_oid *b, void *data);

/**
 * Create a set of refspecs that deletes each of the inputs
 *
 * @param out the vector in which to store the refspecs
 * @param heads the remote heads
 * @param heads_len the size of the array
 */
int create_deletion_refspecs(git_vector *out, const git_remote_head **heads, size_t heads_len);

/**
 * Callback for git_remote_list that adds refspecs to vector
 *
 * @param head a ref on the remote
 * @param payload (git_vector *) of git_remote_head instances
 */
int record_ref_cb(git_remote_head *head, void *payload);

/**
 * Verifies that refs on remote stored by record_ref_cb match the expected
 * names, oids, and order.
 *
 * @param actual_refs actual refs in the remote
 * @param actual_refs_len length of actual_refs
 * @param expected_refs expected remote refs
 * @param expected_refs_len length of expected_refs
 */
void verify_remote_refs(const git_remote_head *actual_refs[], size_t actual_refs_len, const expected_ref expected_refs[], size_t expected_refs_len);

#endif /* INCLUDE_cl_push_util_h__ */
