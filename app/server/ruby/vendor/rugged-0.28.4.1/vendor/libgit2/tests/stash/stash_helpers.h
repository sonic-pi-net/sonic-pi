void setup_stash(
	git_repository *repo,
	git_signature *signature);

void assert_status(
	git_repository *repo,
	const char *path,
	int status_flags);
