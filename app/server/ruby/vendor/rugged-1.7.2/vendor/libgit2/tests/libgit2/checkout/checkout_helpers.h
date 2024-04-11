#include "git2/object.h"
#include "git2/repository.h"

extern void assert_on_branch(git_repository *repo, const char *branch);
extern void reset_index_to_treeish(git_object *treeish);

#define check_file_contents(PATH,EXP) \
	cl_assert_equal_file(EXP,0,PATH)

#define check_file_contents_nocr(PATH,EXP) \
	cl_assert_equal_file_ignore_cr(EXP,0,PATH)

typedef struct {
	int n_conflicts;
	int n_dirty;
	int n_updates;
	int n_untracked;
	int n_ignored;
	int debug;
} checkout_counts;

extern int checkout_count_callback(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload);

extern void tick_index(git_index *index);
