#include "git2/repository.h"
#include "git2/refs.h"
#include "common.h"
#include "util.h"
#include "buffer.h"
#include "path.h"

int reference_is_packed(git_reference *ref)
{
	git_buf ref_path = GIT_BUF_INIT;
	int packed;

	assert(ref);

	if (git_buf_joinpath(&ref_path,
		git_repository_path(git_reference_owner(ref)),
		git_reference_name(ref)) < 0)
		return -1;

	packed = !git_path_isfile(ref_path.ptr);

	git_buf_dispose(&ref_path);

	return packed;
}
