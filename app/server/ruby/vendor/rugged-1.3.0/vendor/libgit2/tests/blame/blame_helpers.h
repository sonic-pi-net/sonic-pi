#include "clar_libgit2.h"
#include "blame.h"

void hunk_message(size_t idx, const git_blame_hunk *hunk, const char *fmt, ...) GIT_FORMAT_PRINTF(3, 4);

void check_blame_hunk_index(
		git_repository *repo,
		git_blame *blame,
		int idx,
		size_t start_line,
		size_t len,
		char boundary,
		const char *commit_id,
		const char *orig_path);
