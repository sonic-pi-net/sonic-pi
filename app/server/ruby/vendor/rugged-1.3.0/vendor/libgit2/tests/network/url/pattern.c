#include "clar_libgit2.h"
#include "net.h"

struct url_pattern {
	const char *url;
	const char *pattern;
	bool matches;
};

void test_network_url_pattern__single(void)
{
	git_net_url url;
	size_t i;

	struct url_pattern url_patterns[] = {
		/* Wildcard matches */
		{ "https://example.com/", "", false },
		{ "https://example.com/", "*", true },

		/* Literal and wildcard matches */
		{ "https://example.com/", "example.com", true },
		{ "https://example.com/", ".example.com", true },
		{ "https://example.com/", "*.example.com", true },
		{ "https://www.example.com/", "www.example.com", true },
		{ "https://www.example.com/", ".example.com", true },
		{ "https://www.example.com/", "*.example.com", true },

		/* Literal and wildcard failures */
		{ "https://example.com/", "example.org", false },
		{ "https://example.com/", ".example.org", false },
		{ "https://example.com/", "*.example.org", false },
		{ "https://foo.example.com/", "www.example.com", false },

		/*
		 * A port in the pattern is optional; if no port is
		 * present, it matches *all* ports.
		 */
		{ "https://example.com/", "example.com:443", true },
		{ "https://example.com/", "example.com:80", false },
		{ "https://example.com:1443/", "example.com", true },

		/* Failures with similar prefix/suffix */
		{ "https://texample.com/", "example.com", false },
		{ "https://example.com/", "mexample.com", false },
		{ "https://example.com:44/", "example.com:443", false },
		{ "https://example.com:443/", "example.com:44", false },
	};

	for (i = 0; i < ARRAY_SIZE(url_patterns); i++) {
		cl_git_pass(git_net_url_parse(&url, url_patterns[i].url));
		cl_assert_(git_net_url_matches_pattern(&url, url_patterns[i].pattern) == url_patterns[i].matches, url_patterns[i].pattern);
		git_net_url_dispose(&url);
	}
}

void test_network_url_pattern__list(void)
{
	git_net_url url;
	size_t i;

	struct url_pattern url_patterns[] = {
		/* Wildcard matches */
		{ "https://example.com/", "", false },
		{ "https://example.com/", "*", true },
		{ "https://example.com/", ",example.com,", true },
		{ "https://example.com/", "foo,,example.com,,bar", true },
		{ "https://example.com/", "foo,,zzz,,*,,bar", true },

		/* Literals */
		{ "https://example.com/", "example.com", true },
		{ "https://example.com/", "foo.bar,example.com", true },
		{ "https://example.com/", "foo.bar", false },
		{ "https://example.com/", "foo.bar,example.org", false },
		{ "https://www.example.com/", "foo.example.com,www.example.com,bar.example.com", true },
		{ "https://www.example.com/", "foo.example.com,baz.example.com,bar.example.com", false },
		{ "https://foo.example.com/", "www.example.com", false },
		{ "https://foo.example.com/", "bar.example.com,www.example.com,", false },

		/* Wildcards */
		{ "https://example.com/", ".example.com", true },
		{ "https://example.com/", "*.example.com", true },
		{ "https://example.com/", "foo.com,bar.com,.example.com", true },
		{ "https://example.com/", ".foo.com,.bar.com,.example.com", true },
		{ "https://example.com/", ".foo.com,.bar.com,asdf.com", false },
		{ "https://example.com/", "*.foo,*.bar,*.example.com,*.asdf", true },
		{ "https://example.com/", "*.foo,*.bar,*.asdf", false },


		/* Ports! */
		{ "https://example.com/", "example.com:443", true },
		{ "https://example.com/", "example.com:42,example.com:443,example.com:99", true },
		{ "https://example.com/", "example.com:42,example.com:80,example.org:443", false },
		{ "https://example.com:1443/", "example.com", true },
		{ "https://example.com:44/", "example.com:443", false },
		{ "https://example.com:443/", "example.com:44", false },
	};

	for (i = 0; i < ARRAY_SIZE(url_patterns); i++) {
		cl_git_pass(git_net_url_parse(&url, url_patterns[i].url));
		cl_assert_(git_net_url_matches_pattern_list(&url, url_patterns[i].pattern) == url_patterns[i].matches, url_patterns[i].pattern);
		git_net_url_dispose(&url);
	}
}
