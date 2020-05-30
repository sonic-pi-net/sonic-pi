
extern void expect_iterator_items(
	git_iterator *i,
	size_t expected_flat,
	const char **expected_flat_paths,
	size_t expected_total,
	const char **expected_total_paths);

extern void expect_advance_over(
	git_iterator *i,
	const char *expected_path,
	git_iterator_status_t expected_status);

void expect_advance_into(
	git_iterator *i,
	const char *expected_path);
