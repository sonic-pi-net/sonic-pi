#include "git2/repository.h"
#include "git2/refs.h"
#include "common.h"
#include "util.h"
#include "path.h"
#include "ref_helpers.h"

int reference_is_packed(git_reference *ref)
{
	git_str ref_path = GIT_STR_INIT;
	int packed;

	assert(ref);

	if (git_str_joinpath(&ref_path,
		git_repository_path(git_reference_owner(ref)),
		git_reference_name(ref)) < 0)
		return -1;

	packed = !git_fs_path_isfile(ref_path.ptr);

	git_str_dispose(&ref_path);

	return packed;
}
