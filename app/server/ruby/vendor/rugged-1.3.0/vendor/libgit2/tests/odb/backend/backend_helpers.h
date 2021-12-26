#include "git2/sys/odb_backend.h"

typedef struct {
	const char *oid;
	const char *content;
} fake_object;

typedef struct {
	git_odb_backend parent;

	int exists_calls;
	int exists_prefix_calls;
	int read_calls;
	int read_header_calls;
	int read_prefix_calls;

	const fake_object *objects;
} fake_backend;

int build_fake_backend(
	git_odb_backend **out,
	const fake_object *objects);
