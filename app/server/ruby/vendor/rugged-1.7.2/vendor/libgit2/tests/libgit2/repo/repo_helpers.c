#include "clar_libgit2.h"
#include "refs.h"
#include "repo_helpers.h"
#include "posix.h"

void make_head_unborn(git_repository* repo, const char *target)
{
	git_reference *head;

	cl_git_pass(git_reference_symbolic_create(&head, repo, GIT_HEAD_FILE, target, 1, NULL));
	git_reference_free(head);
}

void delete_head(git_repository* repo)
{
	git_str head_path = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&head_path, git_repository_path(repo), GIT_HEAD_FILE));
	cl_git_pass(p_unlink(git_str_cstr(&head_path)));

	git_str_dispose(&head_path);
}

void create_tmp_global_config(const char *dirname, const char *key, const char *val)
{
	git_str path = GIT_STR_INIT;
	git_config *config;

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH,
		GIT_CONFIG_LEVEL_GLOBAL, dirname));
	cl_must_pass(p_mkdir(dirname, 0777));
	cl_git_pass(git_str_joinpath(&path, dirname, ".gitconfig"));
	cl_git_pass(git_config_open_ondisk(&config, path.ptr));
	cl_git_pass(git_config_set_string(config, key, val));
	git_config_free(config);
	git_str_dispose(&path);
}
