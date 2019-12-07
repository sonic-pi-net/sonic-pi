#include "common.h"

#define NON_EXISTING_HEAD "refs/heads/hide/and/seek"

extern void make_head_unborn(git_repository* repo, const char *target);
extern void delete_head(git_repository* repo);
