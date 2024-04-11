#include "clar_libgit2.h"
#include "git2/checkout.h"
#include "git2/rebase.h"
#include "posix.h"
#include "signature.h"
#include "../submodule/submodule_helpers.h"

#include <fcntl.h>

static git_repository *repo;
static git_signature *signature;

/* Fixture setup and teardown */
void test_rebase_submodule__initialize(void)
{
	git_index *index;
	git_oid tree_oid, commit_id;
	git_tree *tree;
	git_commit *parent;
	git_object *obj;
	git_reference *master_ref;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	repo = cl_git_sandbox_init("rebase-submodule");
	cl_git_pass(git_signature_new(&signature,
		"Rebaser", "rebaser@rebaser.rb", 1405694510, 0));

	rewrite_gitmodules(git_repository_workdir(repo));

	cl_git_pass(git_submodule_set_url(repo, "my-submodule", git_repository_path(repo)));

	/* We have to commit the rewritten .gitmodules file */
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, ".gitmodules"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_write_tree(&tree_oid, index));
	cl_git_pass(git_tree_lookup(&tree, repo, &tree_oid));

	cl_git_pass(git_repository_head(&master_ref, repo));
	cl_git_pass(git_commit_lookup(&parent, repo, git_reference_target(master_ref)));

	cl_git_pass(git_commit_create_v(&commit_id, repo, git_reference_name(master_ref), signature, signature, NULL, "Fixup .gitmodules", tree, 1, parent));

	/* And a final reset, for good measure */
	cl_git_pass(git_object_lookup(&obj, repo, &commit_id, GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(repo, obj, GIT_RESET_HARD, &opts));

	git_index_free(index);
	git_object_free(obj);
	git_commit_free(parent);
	git_reference_free(master_ref);
	git_tree_free(tree);
}

void test_rebase_submodule__cleanup(void)
{
	git_signature_free(signature);
	cl_git_sandbox_cleanup();
}

void test_rebase_submodule__init_untracked(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_str untracked_path = GIT_STR_INIT;
	FILE *fp;
	git_submodule *submodule;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/asparagus"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_submodule_lookup(&submodule, repo, "my-submodule"));
	cl_git_pass(git_submodule_update(submodule, 1, NULL));

	git_str_printf(&untracked_path, "%s/my-submodule/untracked", git_repository_workdir(repo));
	fp = fopen(git_str_cstr(&untracked_path), "w");
	fprintf(fp, "An untracked file in a submodule should not block a rebase\n");
	fclose(fp);
	git_str_dispose(&untracked_path);

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, NULL));

	git_submodule_free(submodule);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}
