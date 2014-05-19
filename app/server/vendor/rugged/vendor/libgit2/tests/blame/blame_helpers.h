#include "clar_libgit2.h"
#include "blame.h"

void hunk_message(size_t idx, const git_blame_hunk *hunk, const char *fmt, ...);

void check_blame_hunk_index(
		git_repository *repo,
		git_blame *blame,
		int idx,
		int start_line,
		int len,
		char boundary,
		const char *commit_id,
		const char *orig_path);


