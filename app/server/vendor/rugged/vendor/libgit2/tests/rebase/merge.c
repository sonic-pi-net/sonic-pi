#include "clar_libgit2.h"
#include "git2/rebase.h"
#include "posix.h"
#include "signature.h"

#include <fcntl.h>

static git_repository *repo;
static git_signature *signature;

static void set_core_autocrlf_to(git_repository *repo, bool value)
{
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_bool(cfg, "core.autocrlf", value));

	git_config_free(cfg);
}

// Fixture setup and teardown
void test_rebase_merge__initialize(void)
{
	repo = cl_git_sandbox_init("rebase");
	cl_git_pass(git_signature_new(&signature,
		"Rebaser", "rebaser@rebaser.rb", 1405694510, 0));

	set_core_autocrlf_to(repo, false);
}

void test_rebase_merge__cleanup(void)
{
	git_signature_free(signature);
	cl_git_sandbox_cleanup();
}

void test_rebase_merge__next(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_status_list *status_list;
	const git_status_entry *status_entry;
	git_oid pick_id, file1_id;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));

	git_oid_fromstr(&pick_id, "da9c51a23d02d931a486f45ad18cda05cf5d2b94");

	cl_assert_equal_i(GIT_REBASE_OPERATION_PICK, rebase_operation->type);
	cl_assert_equal_oid(&pick_id, &rebase_operation->id);
	cl_assert_equal_file("da9c51a23d02d931a486f45ad18cda05cf5d2b94\n", 41, "rebase/.git/rebase-merge/current");
	cl_assert_equal_file("1\n", 2, "rebase/.git/rebase-merge/msgnum");

	cl_git_pass(git_status_list_new(&status_list, repo, NULL));
	cl_assert_equal_i(1, git_status_list_entrycount(status_list));
	cl_assert(status_entry = git_status_byindex(status_list, 0));

	cl_assert_equal_s("beef.txt", status_entry->head_to_index->new_file.path);

	git_oid_fromstr(&file1_id, "8d95ea62e621f1d38d230d9e7d206e41096d76af");
	cl_assert_equal_oid(&file1_id, &status_entry->head_to_index->new_file.id);

	git_status_list_free(status_list);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__next_with_conflicts(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_status_list *status_list;
	const git_status_entry *status_entry;
	git_oid pick_id;

	const char *expected_merge =
"ASPARAGUS SOUP.\n"
"\n"
"<<<<<<< master\n"
"TAKE FOUR LARGE BUNCHES of asparagus, scrape it nicely, cut off one inch\n"
"OF THE TOPS, and lay them in water, chop the stalks and put them on the\n"
"FIRE WITH A PIECE OF BACON, a large onion cut up, and pepper and salt;\n"
"ADD TWO QUARTS OF WATER, boil them till the stalks are quite soft, then\n"
"PULP THEM THROUGH A SIEVE, and strain the water to it, which must be put\n"
"=======\n"
"Take four large bunches of asparagus, scrape it nicely, CUT OFF ONE INCH\n"
"of the tops, and lay them in water, chop the stalks and PUT THEM ON THE\n"
"fire with a piece of bacon, a large onion cut up, and pepper and salt;\n"
"add two quarts of water, boil them till the stalks are quite soft, then\n"
"pulp them through a sieve, and strain the water to it, which must be put\n"
">>>>>>> Conflicting modification 1 to asparagus\n"
"back in the pot; put into it a chicken cut up, with the tops of\n"
"asparagus which had been laid by, boil it until these last articles are\n"
"sufficiently done, thicken with flour, butter and milk, and serve it up.\n";

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/asparagus"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));

	git_oid_fromstr(&pick_id, "33f915f9e4dbd9f4b24430e48731a59b45b15500");

	cl_assert_equal_i(GIT_REBASE_OPERATION_PICK, rebase_operation->type);
	cl_assert_equal_oid(&pick_id, &rebase_operation->id);
	cl_assert_equal_file("33f915f9e4dbd9f4b24430e48731a59b45b15500\n", 41, "rebase/.git/rebase-merge/current");
	cl_assert_equal_file("1\n", 2, "rebase/.git/rebase-merge/msgnum");

	cl_git_pass(git_status_list_new(&status_list, repo, NULL));
	cl_assert_equal_i(1, git_status_list_entrycount(status_list));
	cl_assert(status_entry = git_status_byindex(status_list, 0));

	cl_assert_equal_s("asparagus.txt", status_entry->head_to_index->new_file.path);

	cl_assert_equal_file(expected_merge, strlen(expected_merge), "rebase/asparagus.txt");

	git_status_list_free(status_list);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__next_stops_with_iterover(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid commit_id;
	int error;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_fail(error = git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_assert_equal_i(GIT_ITEROVER, error);

	cl_assert_equal_file("5\n", 2, "rebase/.git/rebase-merge/end");
	cl_assert_equal_file("5\n", 2, "rebase/.git/rebase-merge/msgnum");

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__commit(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid commit_id, tree_id, parent_id;
	git_signature *author;
	git_commit *commit;
	git_reflog *reflog;
	const git_reflog_entry *reflog_entry;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));

	git_oid_fromstr(&parent_id, "efad0b11c47cb2f0220cbd6f5b0f93bb99064b00");
	cl_assert_equal_i(1, git_commit_parentcount(commit));
	cl_assert_equal_oid(&parent_id, git_commit_parent_id(commit, 0));

	git_oid_fromstr(&tree_id, "4461379789c777d2a6c1f2ee0e9d6c86731b9992");
	cl_assert_equal_oid(&tree_id, git_commit_tree_id(commit));

	cl_assert_equal_s(NULL, git_commit_message_encoding(commit));
	cl_assert_equal_s("Modification 1 to beef\n", git_commit_message(commit));

	cl_git_pass(git_signature_new(&author,
		"Edward Thomson", "ethomson@edwardthomson.com", 1405621769, 0-(4*60)));
	cl_assert(git_signature__equal(author, git_commit_author(commit)));

	cl_assert(git_signature__equal(signature, git_commit_committer(commit)));

	/* Make sure the reflogs are updated appropriately */
	cl_git_pass(git_reflog_read(&reflog, repo, "HEAD"));
	cl_assert(reflog_entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_oid(&parent_id, git_reflog_entry_id_old(reflog_entry));
	cl_assert_equal_oid(&commit_id, git_reflog_entry_id_new(reflog_entry));
	cl_assert_equal_s("rebase: Modification 1 to beef", git_reflog_entry_message(reflog_entry));

	git_reflog_free(reflog);
	git_signature_free(author);
	git_commit_free(commit);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__commit_updates_rewritten(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid commit_id;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_assert_equal_file(
		"da9c51a23d02d931a486f45ad18cda05cf5d2b94 776e4c48922799f903f03f5f6e51da8b01e4cce0\n"
		"8d1f13f93c4995760ac07d129246ac1ff64c0be9 ba1f9b4fd5cf8151f7818be2111cc0869f1eb95a\n",
		164, "rebase/.git/rebase-merge/rewritten");

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__commit_drops_already_applied(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid commit_id;
	int error;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/green_pea"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_fail(error = git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_assert_equal_i(GIT_EAPPLIED, error);

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_assert_equal_file(
		"8d1f13f93c4995760ac07d129246ac1ff64c0be9 2ac4fb7b74c1287f6c792acad759e1ec01e18dae\n",
		82, "rebase/.git/rebase-merge/rewritten");

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__finish(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref, *head_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid commit_id;
	git_reflog *reflog;
	const git_reflog_entry *reflog_entry;
	int error;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_fail(error = git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_assert_equal_i(GIT_ITEROVER, error);

	cl_git_pass(git_rebase_finish(rebase, signature, NULL));

	cl_assert_equal_i(GIT_REPOSITORY_STATE_NONE, git_repository_state(repo));

	cl_git_pass(git_reference_lookup(&head_ref, repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head_ref));
	cl_assert_equal_s("refs/heads/gravy", git_reference_symbolic_target(head_ref));

	/* Make sure the reflogs are updated appropriately */
	cl_git_pass(git_reflog_read(&reflog, repo, "HEAD"));
	cl_assert(reflog_entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_oid(&commit_id, git_reflog_entry_id_old(reflog_entry));
	cl_assert_equal_oid(&commit_id, git_reflog_entry_id_new(reflog_entry));
	cl_assert_equal_s("rebase finished: returning to refs/heads/gravy", git_reflog_entry_message(reflog_entry));
	git_reflog_free(reflog);

	cl_git_pass(git_reflog_read(&reflog, repo, "refs/heads/gravy"));
	cl_assert(reflog_entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_oid(git_annotated_commit_id(branch_head), git_reflog_entry_id_old(reflog_entry));
	cl_assert_equal_oid(&commit_id, git_reflog_entry_id_new(reflog_entry));
	cl_assert_equal_s("rebase finished: refs/heads/gravy onto f87d14a4a236582a0278a916340a793714256864", git_reflog_entry_message(reflog_entry));

	git_reflog_free(reflog);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(head_ref);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

static void test_copy_note(
	const git_rebase_options *opts,
	bool should_exist)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_commit *branch_commit;
	git_rebase_operation *rebase_operation;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid note_id, commit_id;
	git_note *note = NULL;
	int error;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_reference_peel((git_object **)&branch_commit,
		branch_ref, GIT_OBJ_COMMIT));

	/* Add a note to a commit */
	cl_git_pass(git_note_create(&note_id, repo, "refs/notes/test",
		git_commit_author(branch_commit), git_commit_committer(branch_commit),
		git_commit_id(branch_commit),
		"This is a commit note.", 0));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, signature, opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase, &checkout_opts));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_finish(rebase, signature, opts));

	cl_assert_equal_i(GIT_REPOSITORY_STATE_NONE, git_repository_state(repo));

	if (should_exist) {
		cl_git_pass(git_note_read(&note, repo, "refs/notes/test", &commit_id));
		cl_assert_equal_s("This is a commit note.", git_note_message(note));
	} else {
		cl_git_fail(error =
			git_note_read(&note, repo, "refs/notes/test", &commit_id));
		cl_assert_equal_i(GIT_ENOTFOUND, error);
	}

	git_note_free(note);
	git_commit_free(branch_commit);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_merge__copy_notes_off_by_default(void)
{
	test_copy_note(NULL, 0);
}

void test_rebase_merge__copy_notes_specified_in_options(void)
{
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;
	opts.rewrite_notes_ref = "refs/notes/test";

	test_copy_note(&opts, 1);
}

void test_rebase_merge__copy_notes_specified_in_config(void)
{
	git_config *config;

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_string(config,
		"notes.rewriteRef", "refs/notes/test"));

	test_copy_note(NULL, 1);
}

void test_rebase_merge__copy_notes_disabled_in_config(void)
{
	git_config *config;

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "notes.rewrite.rebase", 0));
	cl_git_pass(git_config_set_string(config,
		"notes.rewriteRef", "refs/notes/test"));

	test_copy_note(NULL, 0);
}

