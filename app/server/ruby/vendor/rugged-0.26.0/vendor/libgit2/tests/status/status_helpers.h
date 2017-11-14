#ifndef INCLUDE_cl_status_helpers_h__
#define INCLUDE_cl_status_helpers_h__

typedef struct {
	int wrong_status_flags_count;
	int wrong_sorted_path;
	int entry_count;
	const unsigned int* expected_statuses;
	const char** expected_paths;
	int expected_entry_count;
	const char *file;
	int line;
	bool debug;
} status_entry_counts;

#define status_counts_init(counts, paths, statuses) do { \
	memset(&(counts), 0, sizeof(counts)); \
	(counts).expected_statuses = (statuses); \
	(counts).expected_paths = (paths); \
	(counts).file = __FILE__; \
	(counts).line = __LINE__; \
	} while (0)

/* cb_status__normal takes payload of "status_entry_counts *" */

extern int cb_status__normal(
	const char *path, unsigned int status_flags, void *payload);


/* cb_status__count takes payload of "int *" */

extern int cb_status__count(const char *p, unsigned int s, void *payload);


typedef struct {
	int count;
	unsigned int status;
	bool debug;
} status_entry_single;

/* cb_status__single takes payload of "status_entry_single *" */

extern int cb_status__single(const char *p, unsigned int s, void *payload);

/* cb_status__print takes optional payload of "int *" */

extern int cb_status__print(const char *p, unsigned int s, void *payload);

#endif
