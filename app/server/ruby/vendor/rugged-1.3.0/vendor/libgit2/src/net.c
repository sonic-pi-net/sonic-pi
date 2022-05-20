/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "net.h"
#include "netops.h"

#include <ctype.h>
#include "git2/errors.h"

#include "posix.h"
#include "buffer.h"
#include "http_parser.h"
#include "runtime.h"

#define DEFAULT_PORT_HTTP  "80"
#define DEFAULT_PORT_HTTPS "443"
#define DEFAULT_PORT_GIT   "9418"
#define DEFAULT_PORT_SSH   "22"

static const char *default_port_for_scheme(const char *scheme)
{
	if (strcmp(scheme, "http") == 0)
		return DEFAULT_PORT_HTTP;
	else if (strcmp(scheme, "https") == 0)
		return DEFAULT_PORT_HTTPS;
	else if (strcmp(scheme, "git") == 0)
		return DEFAULT_PORT_GIT;
	else if (strcmp(scheme, "ssh") == 0)
		return DEFAULT_PORT_SSH;

	return NULL;
}

int git_net_url_dup(git_net_url *out, git_net_url *in)
{
	if (in->scheme) {
		out->scheme = git__strdup(in->scheme);
		GIT_ERROR_CHECK_ALLOC(out->scheme);
	}

	if (in->host) {
		out->host = git__strdup(in->host);
		GIT_ERROR_CHECK_ALLOC(out->host);
	}

	if (in->port) {
		out->port = git__strdup(in->port);
		GIT_ERROR_CHECK_ALLOC(out->port);
	}

	if (in->path) {
		out->path = git__strdup(in->path);
		GIT_ERROR_CHECK_ALLOC(out->path);
	}

	if (in->query) {
		out->query = git__strdup(in->query);
		GIT_ERROR_CHECK_ALLOC(out->query);
	}

	if (in->username) {
		out->username = git__strdup(in->username);
		GIT_ERROR_CHECK_ALLOC(out->username);
	}

	if (in->password) {
		out->password = git__strdup(in->password);
		GIT_ERROR_CHECK_ALLOC(out->password);
	}

	return 0;
}

int git_net_url_parse(git_net_url *url, const char *given)
{
	struct http_parser_url u = {0};
	bool has_scheme, has_host, has_port, has_path, has_query, has_userinfo;
	git_buf scheme = GIT_BUF_INIT,
		host = GIT_BUF_INIT,
		port = GIT_BUF_INIT,
		path = GIT_BUF_INIT,
		username = GIT_BUF_INIT,
		password = GIT_BUF_INIT,
		query = GIT_BUF_INIT;
	int error = GIT_EINVALIDSPEC;

	if (http_parser_parse_url(given, strlen(given), false, &u)) {
		git_error_set(GIT_ERROR_NET, "malformed URL '%s'", given);
		goto done;
	}

	has_scheme = !!(u.field_set & (1 << UF_SCHEMA));
	has_host = !!(u.field_set & (1 << UF_HOST));
	has_port = !!(u.field_set & (1 << UF_PORT));
	has_path = !!(u.field_set & (1 << UF_PATH));
	has_query = !!(u.field_set & (1 << UF_QUERY));
	has_userinfo = !!(u.field_set & (1 << UF_USERINFO));

	if (has_scheme) {
		const char *url_scheme = given + u.field_data[UF_SCHEMA].off;
		size_t url_scheme_len = u.field_data[UF_SCHEMA].len;
		git_buf_put(&scheme, url_scheme, url_scheme_len);
		git__strntolower(scheme.ptr, scheme.size);
	} else {
		git_error_set(GIT_ERROR_NET, "malformed URL '%s'", given);
		goto done;
	}

	if (has_host) {
		const char *url_host = given + u.field_data[UF_HOST].off;
		size_t url_host_len = u.field_data[UF_HOST].len;
		git_buf_decode_percent(&host, url_host, url_host_len);
	}

	if (has_port) {
		const char *url_port = given + u.field_data[UF_PORT].off;
		size_t url_port_len = u.field_data[UF_PORT].len;
		git_buf_put(&port, url_port, url_port_len);
	} else {
		const char *default_port = default_port_for_scheme(scheme.ptr);

		if (default_port == NULL) {
			git_error_set(GIT_ERROR_NET, "unknown scheme for URL '%s'", given);
			goto done;
		}

		git_buf_puts(&port, default_port);
	}

	if (has_path) {
		const char *url_path = given + u.field_data[UF_PATH].off;
		size_t url_path_len = u.field_data[UF_PATH].len;
		git_buf_put(&path, url_path, url_path_len);
	} else {
		git_buf_puts(&path, "/");
	}

	if (has_query) {
		const char *url_query = given + u.field_data[UF_QUERY].off;
		size_t url_query_len = u.field_data[UF_QUERY].len;
		git_buf_decode_percent(&query, url_query, url_query_len);
	}

	if (has_userinfo) {
		const char *url_userinfo = given + u.field_data[UF_USERINFO].off;
		size_t url_userinfo_len = u.field_data[UF_USERINFO].len;
		const char *colon = memchr(url_userinfo, ':', url_userinfo_len);

		if (colon) {
			const char *url_username = url_userinfo;
			size_t url_username_len = colon - url_userinfo;
			const char *url_password = colon + 1;
			size_t url_password_len = url_userinfo_len - (url_username_len + 1);

			git_buf_decode_percent(&username, url_username, url_username_len);
			git_buf_decode_percent(&password, url_password, url_password_len);
		} else {
			git_buf_decode_percent(&username, url_userinfo, url_userinfo_len);
		}
	}

	if (git_buf_oom(&scheme) ||
	    git_buf_oom(&host) ||
	    git_buf_oom(&port) ||
	    git_buf_oom(&path) ||
	    git_buf_oom(&query) ||
	    git_buf_oom(&username) ||
	    git_buf_oom(&password))
		return -1;

	url->scheme = git_buf_detach(&scheme);
	url->host = git_buf_detach(&host);
	url->port = git_buf_detach(&port);
	url->path = git_buf_detach(&path);
	url->query = git_buf_detach(&query);
	url->username = git_buf_detach(&username);
	url->password = git_buf_detach(&password);

	error = 0;

done:
	git_buf_dispose(&scheme);
	git_buf_dispose(&host);
	git_buf_dispose(&port);
	git_buf_dispose(&path);
	git_buf_dispose(&query);
	git_buf_dispose(&username);
	git_buf_dispose(&password);
	return error;
}

int git_net_url_joinpath(
	git_net_url *out,
	git_net_url *one,
	const char *two)
{
	git_buf path = GIT_BUF_INIT;
	const char *query;
	size_t one_len, two_len;

	git_net_url_dispose(out);

	if ((query = strchr(two, '?')) != NULL) {
		two_len = query - two;

		if (*(++query) != '\0') {
			out->query = git__strdup(query);
			GIT_ERROR_CHECK_ALLOC(out->query);
		}
	} else {
		two_len = strlen(two);
	}

	/* Strip all trailing `/`s from the first path */
	one_len = one->path ? strlen(one->path) : 0;
	while (one_len && one->path[one_len - 1] == '/')
		one_len--;

	/* Strip all leading `/`s from the second path */
	while (*two == '/') {
		two++;
		two_len--;
	}

	git_buf_put(&path, one->path, one_len);
	git_buf_putc(&path, '/');
	git_buf_put(&path, two, two_len);

	if (git_buf_oom(&path))
		return -1;

	out->path = git_buf_detach(&path);

	if (one->scheme) {
		out->scheme = git__strdup(one->scheme);
		GIT_ERROR_CHECK_ALLOC(out->scheme);
	}

	if (one->host) {
		out->host = git__strdup(one->host);
		GIT_ERROR_CHECK_ALLOC(out->host);
	}

	if (one->port) {
		out->port = git__strdup(one->port);
		GIT_ERROR_CHECK_ALLOC(out->port);
	}

	if (one->username) {
		out->username = git__strdup(one->username);
		GIT_ERROR_CHECK_ALLOC(out->username);
	}

	if (one->password) {
		out->password = git__strdup(one->password);
		GIT_ERROR_CHECK_ALLOC(out->password);
	}

	return 0;
}

/*
 * Some servers strip the query parameters from the Location header
 * when sending a redirect. Others leave it in place.
 * Check for both, starting with the stripped case first,
 * since it appears to be more common.
 */
static void remove_service_suffix(
	git_net_url *url,
	const char *service_suffix)
{
	const char *service_query = strchr(service_suffix, '?');
	size_t full_suffix_len = strlen(service_suffix);
	size_t suffix_len = service_query ?
		(size_t)(service_query - service_suffix) : full_suffix_len;
	size_t path_len = strlen(url->path);
	ssize_t truncate = -1;

	/*
	 * Check for a redirect without query parameters,
	 * like "/newloc/info/refs"'
	 */
	if (suffix_len && path_len >= suffix_len) {
		size_t suffix_offset = path_len - suffix_len;

		if (git__strncmp(url->path + suffix_offset, service_suffix, suffix_len) == 0 &&
		    (!service_query || git__strcmp(url->query, service_query + 1) == 0)) {
			truncate = suffix_offset;
		}
	}

	/*
	 * If we haven't already found where to truncate to remove the
	 * suffix, check for a redirect with query parameters, like
	 * "/newloc/info/refs?service=git-upload-pack"
	 */
	if (truncate < 0 && git__suffixcmp(url->path, service_suffix) == 0)
		truncate = path_len - full_suffix_len;

	/* Ensure we leave a minimum of '/' as the path */
	if (truncate == 0)
		truncate++;

	if (truncate > 0) {
		url->path[truncate] = '\0';

		git__free(url->query);
		url->query = NULL;
	}
}

int git_net_url_apply_redirect(
	git_net_url *url,
	const char *redirect_location,
	const char *service_suffix)
{
	git_net_url tmp = GIT_NET_URL_INIT;
	int error = 0;

	GIT_ASSERT(url);
	GIT_ASSERT(redirect_location);

	if (redirect_location[0] == '/') {
		git__free(url->path);

		if ((url->path = git__strdup(redirect_location)) == NULL) {
			error = -1;
			goto done;
		}
	} else {
		git_net_url *original = url;

		if ((error = git_net_url_parse(&tmp, redirect_location)) < 0)
			goto done;

		/* Validate that this is a legal redirection */

		if (original->scheme &&
			strcmp(original->scheme, tmp.scheme) != 0 &&
			strcmp(tmp.scheme, "https") != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->scheme, tmp.scheme);

			error = -1;
			goto done;
		}

		if (original->host &&
		    git__strcasecmp(original->host, tmp.host) != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->host, tmp.host);

			error = -1;
			goto done;
		}

		git_net_url_swap(url, &tmp);
	}

	/* Remove the service suffix if it was given to us */
	if (service_suffix)
		remove_service_suffix(url, service_suffix);

done:
	git_net_url_dispose(&tmp);
	return error;
}

bool git_net_url_valid(git_net_url *url)
{
	return (url->host && url->port && url->path);
}

bool git_net_url_is_default_port(git_net_url *url)
{
	const char *default_port;

	if ((default_port = default_port_for_scheme(url->scheme)) != NULL)
		return (strcmp(url->port, default_port) == 0);
	else
		return false;
}

bool git_net_url_is_ipv6(git_net_url *url)
{
	return (strchr(url->host, ':') != NULL);
}

void git_net_url_swap(git_net_url *a, git_net_url *b)
{
	git_net_url tmp = GIT_NET_URL_INIT;

	memcpy(&tmp, a, sizeof(git_net_url));
	memcpy(a, b, sizeof(git_net_url));
	memcpy(b, &tmp, sizeof(git_net_url));
}

int git_net_url_fmt(git_buf *buf, git_net_url *url)
{
	GIT_ASSERT_ARG(url);
	GIT_ASSERT_ARG(url->scheme);
	GIT_ASSERT_ARG(url->host);

	git_buf_puts(buf, url->scheme);
	git_buf_puts(buf, "://");

	if (url->username) {
		git_buf_puts(buf, url->username);

		if (url->password) {
			git_buf_puts(buf, ":");
			git_buf_puts(buf, url->password);
		}

		git_buf_putc(buf, '@');
	}

	git_buf_puts(buf, url->host);

	if (url->port && !git_net_url_is_default_port(url)) {
		git_buf_putc(buf, ':');
		git_buf_puts(buf, url->port);
	}

	git_buf_puts(buf, url->path ? url->path : "/");

	if (url->query) {
		git_buf_putc(buf, '?');
		git_buf_puts(buf, url->query);
	}

	return git_buf_oom(buf) ? -1 : 0;
}

int git_net_url_fmt_path(git_buf *buf, git_net_url *url)
{
	git_buf_puts(buf, url->path ? url->path : "/");

	if (url->query) {
		git_buf_putc(buf, '?');
		git_buf_puts(buf, url->query);
	}

	return git_buf_oom(buf) ? -1 : 0;
}

static bool matches_pattern(
	git_net_url *url,
	const char *pattern,
	size_t pattern_len)
{
	const char *domain, *port = NULL, *colon;
	size_t host_len, domain_len, port_len = 0, wildcard = 0;

	GIT_UNUSED(url);
	GIT_UNUSED(pattern);

	if (!pattern_len)
		return false;
	else if (pattern_len == 1 && pattern[0] == '*')
		return true;
	else if (pattern_len > 1 && pattern[0] == '*' && pattern[1] == '.')
		wildcard = 2;
	else if (pattern[0] == '.')
		wildcard = 1;

	domain = pattern + wildcard;
	domain_len = pattern_len - wildcard;

	if ((colon = memchr(domain, ':', domain_len)) != NULL) {
		domain_len = colon - domain;
		port = colon + 1;
		port_len = pattern_len - wildcard - domain_len - 1;
	}

	/* A pattern's port *must* match if it's specified */
	if (port_len && git__strlcmp(url->port, port, port_len) != 0)
		return false;

	/* No wildcard?  Host must match exactly. */
	if (!wildcard)
		return !git__strlcmp(url->host, domain, domain_len);

	/* Wildcard: ensure there's (at least) a suffix match */
	if ((host_len = strlen(url->host)) < domain_len ||
	    memcmp(url->host + (host_len - domain_len), domain, domain_len))
		return false;

	/* The pattern is *.domain and the host is simply domain */
	if (host_len == domain_len)
		return true;

	/* The pattern is *.domain and the host is foo.domain */
	return (url->host[host_len - domain_len - 1] == '.');
}

bool git_net_url_matches_pattern(git_net_url *url, const char *pattern)
{
	return matches_pattern(url, pattern, strlen(pattern));
}

bool git_net_url_matches_pattern_list(
	git_net_url *url,
	const char *pattern_list)
{
	const char *pattern, *pattern_end, *sep;

	for (pattern = pattern_list;
	     pattern && *pattern;
	     pattern = sep ? sep + 1 : NULL) {
		sep = strchr(pattern, ',');
		pattern_end = sep ? sep : strchr(pattern, '\0');

		if (matches_pattern(url, pattern, (pattern_end - pattern)))
			return true;
	}

	return false;
}

void git_net_url_dispose(git_net_url *url)
{
	if (url->username)
		git__memzero(url->username, strlen(url->username));

	if (url->password)
		git__memzero(url->password, strlen(url->password));

	git__free(url->scheme); url->scheme = NULL;
	git__free(url->host); url->host = NULL;
	git__free(url->port); url->port = NULL;
	git__free(url->path); url->path = NULL;
	git__free(url->query); url->query = NULL;
	git__free(url->username); url->username = NULL;
	git__free(url->password); url->password = NULL;
}
