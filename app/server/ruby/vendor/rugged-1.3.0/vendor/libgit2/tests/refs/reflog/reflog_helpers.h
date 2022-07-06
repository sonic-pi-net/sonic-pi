size_t reflog_entrycount(git_repository *repo, const char *name);

#define cl_reflog_check_entry(repo, reflog, idx, old_spec, new_spec, email, message) \
    cl_reflog_check_entry_(repo, reflog, idx, old_spec, new_spec, email, message, __FILE__, __LINE__)

void cl_reflog_check_entry_(git_repository *repo, const char *reflog, size_t idx,
						const char *old_spec, const char *new_spec,
						const char *email, const char *message, const char *file, int line);

void reflog_print(git_repository *repo, const char *reflog_name);
