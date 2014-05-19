#include "common.h"

#define KNOWN_COMMIT_IN_BARE_REPO "e90810b8df3e80c413d903f631643c716887138d"
#define KNOWN_COMMIT_IN_ATTR_REPO "217878ab49e1314388ea2e32dc6fdb58a1b969e0"

void reflog_check(git_repository *repo, const char *refname,
		size_t exp_count, const char *exp_email, const char *exp_msg);
