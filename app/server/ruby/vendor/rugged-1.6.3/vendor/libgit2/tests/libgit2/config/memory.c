#include "clar_libgit2.h"

#include "config_backend.h"

static git_config_backend *backend;

void test_config_memory__initialize(void)
{
	backend = NULL;
}

void test_config_memory__cleanup(void)
{
	git_config_backend_free(backend);
}

static void assert_config_contains(git_config_backend *backend,
	const char *name, const char *value)
{
	git_config_entry *entry;
	cl_git_pass(git_config_backend_get_string(&entry, backend, name));
	cl_assert_equal_s(entry->value, value);
}

struct expected_entry {
	const char *name;
	const char *value;
	int seen;
};

static int contains_all_cb(const git_config_entry *entry, void *payload)
{
	struct expected_entry *entries = (struct expected_entry *) payload;
	int i;

	for (i = 0; entries[i].name; i++) {
		if (strcmp(entries[i].name, entry->name) ||
		    strcmp(entries[i].value , entry->value))
			continue;

		if (entries[i].seen)
			cl_fail("Entry seen more than once");
		entries[i].seen = 1;
		return 0;
	}

	cl_fail("Unexpected entry");
	return -1;
}

static void assert_config_contains_all(git_config_backend *backend,
	struct expected_entry *entries)
{
	int i;

	cl_git_pass(git_config_backend_foreach(backend, contains_all_cb, entries));

	for (i = 0; entries[i].name; i++)
		cl_assert(entries[i].seen);
}

static void setup_backend(const char *cfg)
{
	cl_git_pass(git_config_backend_from_string(&backend, cfg, strlen(cfg)));
	cl_git_pass(git_config_backend_open(backend, 0, NULL));
}

void test_config_memory__write_operations_fail(void)
{
	setup_backend("");
	cl_git_fail(git_config_backend_set_string(backend, "general.foo", "var"));
	cl_git_fail(git_config_backend_delete(backend, "general.foo"));
	cl_git_fail(git_config_backend_lock(backend));
	cl_git_fail(git_config_backend_unlock(backend, 0));
}

void test_config_memory__simple(void)
{
	setup_backend(
		"[general]\n"
		"foo=bar\n");

	assert_config_contains(backend, "general.foo", "bar");
}

void test_config_memory__malformed_fails_to_open(void)
{
	const char *cfg =
		"[general\n"
		"foo=bar\n";
	cl_git_pass(git_config_backend_from_string(&backend, cfg, strlen(cfg)));
	cl_git_fail(git_config_backend_open(backend, 0, NULL));
}

void test_config_memory__multiple_vars(void)
{
	setup_backend(
		"[general]\n"
		"foo=bar\n"
		"key=value\n");
	assert_config_contains(backend, "general.foo", "bar");
	assert_config_contains(backend, "general.key", "value");
}

void test_config_memory__multiple_sections(void)
{
	setup_backend(
		"[general]\n"
		"foo=bar\n"
		"\n"
		"[other]\n"
		"key=value\n");
	assert_config_contains(backend, "general.foo", "bar");
	assert_config_contains(backend, "other.key", "value");
}

void test_config_memory__multivar_gets_correct_string(void)
{
	setup_backend(
		"[general]\n"
		"foo=bar1\n"
		"foo=bar2\n");
	assert_config_contains(backend, "general.foo", "bar2");
}

void test_config_memory__foreach_sees_multivar(void)
{
	struct expected_entry entries[] = {
		{ "general.foo", "bar1", 0 },
		{ "general.foo", "bar2", 0 },
		{ NULL, NULL, 0 },
	};

	setup_backend(
		"[general]\n"
		"foo=bar1\n"
		"foo=bar2\n");
	assert_config_contains_all(backend, entries);
}
