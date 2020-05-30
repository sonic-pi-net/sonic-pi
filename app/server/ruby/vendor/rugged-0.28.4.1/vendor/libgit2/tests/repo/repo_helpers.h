#include "common.h"

#define NON_EXISTING_HEAD "refs/heads/hide/and/seek"

extern void make_head_unborn(git_repository* repo, const char *target);
extern void delete_head(git_repository* repo);
extern void create_tmp_global_config(const char *path, const char *key, const char *val);
