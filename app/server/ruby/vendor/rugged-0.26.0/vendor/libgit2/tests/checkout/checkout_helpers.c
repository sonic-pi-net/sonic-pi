#include "clar_libgit2.h"
#include "checkout_helpers.h"
#include "refs.h"
#include "fileops.h"
#include "index.h"

void assert_on_branch(git_repository *repo, const char *branch)
{
	git_reference *head;
	git_buf bname = GIT_BUF_INIT;

	cl_git_pass(git_reference_lookup(&head, repo, GIT_HEAD_FILE));
	cl_assert_(git_reference_type(head) == GIT_REF_SYMBOLIC, branch);

	cl_git_pass(git_buf_joinpath(&bname, "refs/heads", branch));
	cl_assert_equal_s(bname.ptr, git_reference_symbolic_target(head));

	git_reference_free(head);
	git_buf_free(&bname);
}

void reset_index_to_treeish(git_object *treeish)
{
	git_object *tree;
	git_index *index;
	git_repository *repo = git_object_owner(treeish);

	cl_git_pass(git_object_peel(&tree, treeish, GIT_OBJ_TREE));

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_read_tree(index, (git_tree *)tree));
	cl_git_pass(git_index_write(index));

	git_object_free(tree);
	git_index_free(index);
}

int checkout_count_callback(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	checkout_counts *ct = payload;

	GIT_UNUSED(baseline); GIT_UNUSED(target); GIT_UNUSED(workdir);

	if (why & GIT_CHECKOUT_NOTIFY_CONFLICT) {
		ct->n_conflicts++;

		if (ct->debug) {
			if (workdir) {
				if (baseline) {
					if (target)
						fprintf(stderr, "M %s (conflicts with M %s)\n",
							workdir->path, target->path);
					else
						fprintf(stderr, "M %s (conflicts with D %s)\n",
							workdir->path, baseline->path);
				} else {
					if (target)
						fprintf(stderr, "Existing %s (conflicts with A %s)\n",
							workdir->path, target->path);
					else
						fprintf(stderr, "How can an untracked file be a conflict (%s)\n", workdir->path);
				}
			} else {
				if (baseline) {
					if (target)
						fprintf(stderr, "D %s (conflicts with M %s)\n",
							target->path, baseline->path);
					else
						fprintf(stderr, "D %s (conflicts with D %s)\n",
							baseline->path, baseline->path);
				} else {
					if (target)
						fprintf(stderr, "How can an added file with no workdir be a conflict (%s)\n", target->path);
					else
						fprintf(stderr, "How can a nonexistent file be a conflict (%s)\n", path);
				}
			}
		}
	}

	if (why & GIT_CHECKOUT_NOTIFY_DIRTY) {
		ct->n_dirty++;

		if (ct->debug) {
			if (workdir)
				fprintf(stderr, "M %s\n", workdir->path);
			else
				fprintf(stderr, "D %s\n", baseline->path);
		}
	}

	if (why & GIT_CHECKOUT_NOTIFY_UPDATED) {
		ct->n_updates++;

		if (ct->debug) {
			if (baseline) {
				if (target)
					fprintf(stderr, "update: M %s\n", path);
				else
					fprintf(stderr, "update: D %s\n", path);
			} else {
				if (target)
					fprintf(stderr, "update: A %s\n", path);
				else
					fprintf(stderr, "update: this makes no sense %s\n", path);
			}
		}
	}

	if (why & GIT_CHECKOUT_NOTIFY_UNTRACKED) {
		ct->n_untracked++;

		if (ct->debug)
			fprintf(stderr, "? %s\n", path);
	}

	if (why & GIT_CHECKOUT_NOTIFY_IGNORED) {
		ct->n_ignored++;

		if (ct->debug)
			fprintf(stderr, "I %s\n", path);
	}

	return 0;
}

void tick_index(git_index *index)
{
	struct timespec ts;
	struct p_timeval times[2];

	cl_assert(index->on_disk);
	cl_assert(git_index_path(index));

	cl_git_pass(git_index_read(index, true));
	ts = index->stamp.mtime;

	times[0].tv_sec = ts.tv_sec;
	times[0].tv_usec = ts.tv_nsec / 1000;
	times[1].tv_sec = ts.tv_sec + 5;
	times[1].tv_usec = ts.tv_nsec / 1000;

	cl_git_pass(p_utimes(git_index_path(index), times));
	cl_git_pass(git_index_read(index, true));
}
