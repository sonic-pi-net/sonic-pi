/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#ifdef GIT_WINHTTP

#include "git2.h"
#include "git2/transport.h"
#include "buffer.h"
#include "posix.h"
#include "netops.h"
#include "smart.h"
#include "remote.h"
#include "repository.h"
#include "global.h"

#include <wincrypt.h>
#include <winhttp.h>

/* For IInternetSecurityManager zone check */
#include <objbase.h>
#include <urlmon.h>

#define WIDEN2(s) L ## s
#define WIDEN(s) WIDEN2(s)

#define MAX_CONTENT_TYPE_LEN	100
#define WINHTTP_OPTION_PEERDIST_EXTENSION_STATE	109
#define CACHED_POST_BODY_BUF_SIZE	4096
#define UUID_LENGTH_CCH	32
#define TIMEOUT_INFINITE -1
#define DEFAULT_CONNECT_TIMEOUT 60000
#ifndef WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH
#define WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH 0
#endif

static const char *prefix_https = "https://";
static const char *upload_pack_service = "upload-pack";
static const char *upload_pack_ls_service_url = "/info/refs?service=git-upload-pack";
static const char *upload_pack_service_url = "/git-upload-pack";
static const char *receive_pack_service = "receive-pack";
static const char *receive_pack_ls_service_url = "/info/refs?service=git-receive-pack";
static const char *receive_pack_service_url = "/git-receive-pack";
static const wchar_t *get_verb = L"GET";
static const wchar_t *post_verb = L"POST";
static const wchar_t *pragma_nocache = L"Pragma: no-cache";
static const wchar_t *transfer_encoding = L"Transfer-Encoding: chunked";
static const int no_check_cert_flags = SECURITY_FLAG_IGNORE_CERT_CN_INVALID |
	SECURITY_FLAG_IGNORE_CERT_DATE_INVALID |
	SECURITY_FLAG_IGNORE_UNKNOWN_CA;

#if defined(__MINGW32__)
static const CLSID CLSID_InternetSecurityManager_mingw =
	{ 0x7B8A2D94, 0x0AC9, 0x11D1,
	{ 0x89, 0x6C, 0x00, 0xC0, 0x4F, 0xB6, 0xBF, 0xC4 } };
static const IID IID_IInternetSecurityManager_mingw =
	{ 0x79EAC9EE, 0xBAF9, 0x11CE,
	{ 0x8C, 0x82, 0x00, 0xAA, 0x00, 0x4B, 0xA9, 0x0B } };

# define CLSID_InternetSecurityManager CLSID_InternetSecurityManager_mingw
# define IID_IInternetSecurityManager IID_IInternetSecurityManager_mingw
#endif

#define OWNING_SUBTRANSPORT(s) ((winhttp_subtransport *)(s)->parent.subtransport)

typedef enum {
	GIT_WINHTTP_AUTH_BASIC = 1,
	GIT_WINHTTP_AUTH_NTLM = 2,
	GIT_WINHTTP_AUTH_NEGOTIATE = 4,
	GIT_WINHTTP_AUTH_DIGEST = 8,
} winhttp_authmechanism_t;

typedef struct {
	git_smart_subtransport_stream parent;
	const char *service;
	const char *service_url;
	const wchar_t *verb;
	HINTERNET request;
	wchar_t *request_uri;
	char *chunk_buffer;
	unsigned chunk_buffer_len;
	HANDLE post_body;
	DWORD post_body_len;
	unsigned sent_request : 1,
		received_response : 1,
		chunked : 1;
} winhttp_stream;

typedef struct {
	git_smart_subtransport parent;
	transport_smart *owner;
	gitno_connection_data connection_data;
	gitno_connection_data proxy_connection_data;
	git_cred *cred;
	git_cred *url_cred;
	git_cred *proxy_cred;
	int auth_mechanisms;
	HINTERNET session;
	HINTERNET connection;
} winhttp_subtransport;

static int _apply_userpass_credential(HINTERNET request, DWORD target, DWORD scheme, git_cred *cred)
{
	git_cred_userpass_plaintext *c = (git_cred_userpass_plaintext *)cred;
	wchar_t *user, *pass;
	int user_len = 0, pass_len = 0, error = 0;

	if ((error = user_len = git__utf8_to_16_alloc(&user, c->username)) < 0)
		goto done;

	if ((error = pass_len = git__utf8_to_16_alloc(&pass, c->password)) < 0)
		goto done;

	if (!WinHttpSetCredentials(request, target, scheme, user, pass, NULL)) {
		giterr_set(GITERR_OS, "failed to set credentials");
		error = -1;
	}

done:
	if (user_len > 0)
		git__memzero(user, user_len * sizeof(wchar_t));

	if (pass_len > 0)
		git__memzero(pass, pass_len * sizeof(wchar_t));

	git__free(user);
	git__free(pass);

	return error;
}

static int apply_userpass_credential_proxy(HINTERNET request, git_cred *cred, int mechanisms)
{
	if (GIT_WINHTTP_AUTH_DIGEST & mechanisms) {
		return _apply_userpass_credential(request, WINHTTP_AUTH_TARGET_PROXY,
			WINHTTP_AUTH_SCHEME_DIGEST, cred);
	}

	return _apply_userpass_credential(request, WINHTTP_AUTH_TARGET_PROXY,
		WINHTTP_AUTH_SCHEME_BASIC, cred);
}

static int apply_userpass_credential(HINTERNET request, int mechanisms, git_cred *cred)
{
	DWORD native_scheme;

	if ((mechanisms & GIT_WINHTTP_AUTH_NTLM) ||
		(mechanisms & GIT_WINHTTP_AUTH_NEGOTIATE)) {
		native_scheme = WINHTTP_AUTH_SCHEME_NTLM;
	} else if (mechanisms & GIT_WINHTTP_AUTH_BASIC) {
		native_scheme = WINHTTP_AUTH_SCHEME_BASIC;
	} else {
		giterr_set(GITERR_NET, "invalid authentication scheme");
		return -1;
	}

	return _apply_userpass_credential(request, WINHTTP_AUTH_TARGET_SERVER,
		native_scheme, cred);
}

static int apply_default_credentials(HINTERNET request, int mechanisms)
{
	/* Either the caller explicitly requested that default credentials be passed,
	 * or our fallback credential callback was invoked and checked that the target
	 * URI was in the appropriate Internet Explorer security zone. By setting this
	 * flag, we guarantee that the credentials are delivered by WinHTTP. The default
	 * is "medium" which applies to the intranet and sounds like it would correspond
	 * to Internet Explorer security zones, but in fact does not. */
	DWORD data = WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW;

	if ((mechanisms & GIT_WINHTTP_AUTH_NTLM) == 0 &&
		(mechanisms & GIT_WINHTTP_AUTH_NEGOTIATE) == 0) {
		giterr_set(GITERR_NET, "invalid authentication scheme");
		return -1;
	}

	if (!WinHttpSetOption(request, WINHTTP_OPTION_AUTOLOGON_POLICY, &data, sizeof(DWORD)))
		return -1;

	return 0;
}

static int fallback_cred_acquire_cb(
	git_cred **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *payload)
{
	int error = 1;

	GIT_UNUSED(username_from_url);
	GIT_UNUSED(payload);

	/* If the target URI supports integrated Windows authentication
	 * as an authentication mechanism */
	if (GIT_CREDTYPE_DEFAULT & allowed_types) {
		wchar_t *wide_url;

		/* Convert URL to wide characters */
		if (git__utf8_to_16_alloc(&wide_url, url) < 0) {
			giterr_set(GITERR_OS, "failed to convert string to wide form");
			return -1;
		}

		if (SUCCEEDED(CoInitializeEx(NULL, COINIT_MULTITHREADED))) {
			IInternetSecurityManager* pISM;

			/* And if the target URI is in the My Computer, Intranet, or Trusted zones */
			if (SUCCEEDED(CoCreateInstance(&CLSID_InternetSecurityManager, NULL,
				CLSCTX_ALL, &IID_IInternetSecurityManager, (void **)&pISM))) {
				DWORD dwZone;

				if (SUCCEEDED(pISM->lpVtbl->MapUrlToZone(pISM, wide_url, &dwZone, 0)) &&
					(URLZONE_LOCAL_MACHINE == dwZone ||
					URLZONE_INTRANET == dwZone ||
					URLZONE_TRUSTED == dwZone)) {
					git_cred *existing = *cred;

					if (existing)
						existing->free(existing);

					/* Then use default Windows credentials to authenticate this request */
					error = git_cred_default_new(cred);
				}

				pISM->lpVtbl->Release(pISM);
			}

			CoUninitialize();
		}

		git__free(wide_url);
	}

	return error;
}

static int certificate_check(winhttp_stream *s, int valid)
{
	int error;
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	PCERT_CONTEXT cert_ctx;
	DWORD cert_ctx_size = sizeof(cert_ctx);
	git_cert_x509 cert;

	/* If there is no override, we should fail if WinHTTP doesn't think it's fine */
	if (t->owner->certificate_check_cb == NULL && !valid) {
		if (!giterr_last())
			giterr_set(GITERR_NET, "unknown certificate check failure");

		return GIT_ECERTIFICATE;
	}

	if (t->owner->certificate_check_cb == NULL || !t->connection_data.use_ssl)
		return 0;

	if (!WinHttpQueryOption(s->request, WINHTTP_OPTION_SERVER_CERT_CONTEXT, &cert_ctx, &cert_ctx_size)) {
		giterr_set(GITERR_OS, "failed to get server certificate");
		return -1;
	}

	giterr_clear();
	cert.parent.cert_type = GIT_CERT_X509;
	cert.data = cert_ctx->pbCertEncoded;
	cert.len = cert_ctx->cbCertEncoded;
	error = t->owner->certificate_check_cb((git_cert *) &cert, valid, t->connection_data.host, t->owner->cred_acquire_payload);
	CertFreeCertificateContext(cert_ctx);

	if (error < 0 && !giterr_last())
		giterr_set(GITERR_NET, "user cancelled certificate check");

	return error;
}

static void winhttp_stream_close(winhttp_stream *s)
{
	if (s->chunk_buffer) {
		git__free(s->chunk_buffer);
		s->chunk_buffer = NULL;
	}

	if (s->post_body) {
		CloseHandle(s->post_body);
		s->post_body = NULL;
	}

	if (s->request_uri) {
		git__free(s->request_uri);
		s->request_uri = NULL;
	}

	if (s->request) {
		WinHttpCloseHandle(s->request);
		s->request = NULL;
	}

	s->sent_request = 0;
}

/**
 * Extract the url and password from a URL. The outputs are pointers
 * into the input.
 */
static int userpass_from_url(wchar_t **user, int *user_len,
			     wchar_t **pass, int *pass_len,
			     const wchar_t *url, int url_len)
{
	URL_COMPONENTS components = { 0 };

	components.dwStructSize = sizeof(components);
	/* These tell WinHttpCrackUrl that we're interested in the fields */
	components.dwUserNameLength = 1;
	components.dwPasswordLength = 1;

	if (!WinHttpCrackUrl(url, url_len, 0, &components)) {
		giterr_set(GITERR_OS, "failed to extract user/pass from url");
		return -1;
	}

	*user     = components.lpszUserName;
	*user_len = components.dwUserNameLength;
	*pass     = components.lpszPassword;
	*pass_len = components.dwPasswordLength;

	return 0;
}

#define SCHEME_HTTP  "http://"
#define SCHEME_HTTPS "https://"

static int winhttp_stream_connect(winhttp_stream *s)
{
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	git_buf buf = GIT_BUF_INIT;
	char *proxy_url = NULL;
	wchar_t ct[MAX_CONTENT_TYPE_LEN];
	LPCWSTR types[] = { L"*/*", NULL };
	BOOL peerdist = FALSE;
	int error = -1;
	unsigned long disable_redirects = WINHTTP_DISABLE_REDIRECTS;
	int default_timeout = TIMEOUT_INFINITE;
	int default_connect_timeout = DEFAULT_CONNECT_TIMEOUT;
	size_t i;
	const git_proxy_options *proxy_opts;

	/* Prepare URL */
	git_buf_printf(&buf, "%s%s", t->connection_data.path, s->service_url);

	if (git_buf_oom(&buf))
		return -1;

	/* Convert URL to wide characters */
	if (git__utf8_to_16_alloc(&s->request_uri, git_buf_cstr(&buf)) < 0) {
		giterr_set(GITERR_OS, "failed to convert string to wide form");
		goto on_error;
	}

	/* Establish request */
	s->request = WinHttpOpenRequest(
			t->connection,
			s->verb,
			s->request_uri,
			NULL,
			WINHTTP_NO_REFERER,
			types,
			t->connection_data.use_ssl ? WINHTTP_FLAG_SECURE : 0);

	if (!s->request) {
		giterr_set(GITERR_OS, "failed to open request");
		goto on_error;
	}

	if (!WinHttpSetTimeouts(s->request, default_timeout, default_connect_timeout, default_timeout, default_timeout)) {
		giterr_set(GITERR_OS, "failed to set timeouts for WinHTTP");
		goto on_error;
	}

	proxy_opts = &t->owner->proxy;
	if (proxy_opts->type == GIT_PROXY_AUTO) {
		/* Set proxy if necessary */
		if (git_remote__get_http_proxy(t->owner->owner, !!t->connection_data.use_ssl, &proxy_url) < 0)
			goto on_error;
	}
	else if (proxy_opts->type == GIT_PROXY_SPECIFIED) {
		proxy_url = git__strdup(proxy_opts->url);
		GITERR_CHECK_ALLOC(proxy_url);
	}

	if (proxy_url) {
		git_buf processed_url = GIT_BUF_INIT;
		WINHTTP_PROXY_INFO proxy_info;
		wchar_t *proxy_wide;

		if (!git__prefixcmp(proxy_url, SCHEME_HTTP)) {
			t->proxy_connection_data.use_ssl = false;
		} else if (!git__prefixcmp(proxy_url, SCHEME_HTTPS)) {
			t->proxy_connection_data.use_ssl = true;
		} else {
			giterr_set(GITERR_NET, "invalid URL: '%s'", proxy_url);
			return -1;
		}

		gitno_connection_data_free_ptrs(&t->proxy_connection_data);

		if ((error = gitno_extract_url_parts(&t->proxy_connection_data.host, &t->proxy_connection_data.port, NULL,
				&t->proxy_connection_data.user, &t->proxy_connection_data.pass, proxy_url, NULL)) < 0)
			goto on_error;

		if (t->proxy_connection_data.user && t->proxy_connection_data.pass) {
			if (t->proxy_cred) {
				t->proxy_cred->free(t->proxy_cred);
			}

			if ((error = git_cred_userpass_plaintext_new(&t->proxy_cred, t->proxy_connection_data.user, t->proxy_connection_data.pass)) < 0)
				goto on_error;
		}

		if (t->proxy_connection_data.use_ssl)
			git_buf_PUTS(&processed_url, SCHEME_HTTPS);
		else
			git_buf_PUTS(&processed_url, SCHEME_HTTP);

		git_buf_puts(&processed_url, t->proxy_connection_data.host);
		if (t->proxy_connection_data.port)
			git_buf_printf(&processed_url, ":%s", t->proxy_connection_data.port);

		if (git_buf_oom(&processed_url)) {
			error = -1;
			goto on_error;
		}

		/* Convert URL to wide characters */
		error = git__utf8_to_16_alloc(&proxy_wide, processed_url.ptr);
		git_buf_free(&processed_url);
		if (error < 0)
			goto on_error;

		proxy_info.dwAccessType = WINHTTP_ACCESS_TYPE_NAMED_PROXY;
		proxy_info.lpszProxy = proxy_wide;
		proxy_info.lpszProxyBypass = NULL;

		if (!WinHttpSetOption(s->request,
			WINHTTP_OPTION_PROXY,
			&proxy_info,
			sizeof(WINHTTP_PROXY_INFO))) {
			giterr_set(GITERR_OS, "failed to set proxy");
			git__free(proxy_wide);
			goto on_error;
		}

		git__free(proxy_wide);

		if (t->proxy_cred) {
			if (t->proxy_cred->credtype == GIT_CREDTYPE_USERPASS_PLAINTEXT) {
				if ((error = apply_userpass_credential_proxy(s->request, t->proxy_cred, t->auth_mechanisms)) < 0)
					goto on_error;
			}
		}

	}

	/* Disable WinHTTP redirects so we can handle them manually. Why, you ask?
	 * http://social.msdn.microsoft.com/Forums/windowsdesktop/en-US/b2ff8879-ab9f-4218-8f09-16d25dff87ae
	 */
	if (!WinHttpSetOption(s->request,
		WINHTTP_OPTION_DISABLE_FEATURE,
		&disable_redirects,
		sizeof(disable_redirects))) {
			giterr_set(GITERR_OS, "failed to disable redirects");
			goto on_error;
	}

	/* Strip unwanted headers (X-P2P-PeerDist, X-P2P-PeerDistEx) that WinHTTP
	 * adds itself. This option may not be supported by the underlying
	 * platform, so we do not error-check it */
	WinHttpSetOption(s->request,
		WINHTTP_OPTION_PEERDIST_EXTENSION_STATE,
		&peerdist,
		sizeof(peerdist));

	/* Send Pragma: no-cache header */
	if (!WinHttpAddRequestHeaders(s->request, pragma_nocache, (ULONG) -1L, WINHTTP_ADDREQ_FLAG_ADD)) {
		giterr_set(GITERR_OS, "failed to add a header to the request");
		goto on_error;
	}

	if (post_verb == s->verb) {
		/* Send Content-Type and Accept headers -- only necessary on a POST */
		git_buf_clear(&buf);
		if (git_buf_printf(&buf,
			"Content-Type: application/x-git-%s-request",
			s->service) < 0)
			goto on_error;

		if (git__utf8_to_16(ct, MAX_CONTENT_TYPE_LEN, git_buf_cstr(&buf)) < 0) {
			giterr_set(GITERR_OS, "failed to convert content-type to wide characters");
			goto on_error;
		}

		if (!WinHttpAddRequestHeaders(s->request, ct, (ULONG)-1L,
			WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
			giterr_set(GITERR_OS, "failed to add a header to the request");
			goto on_error;
		}

		git_buf_clear(&buf);
		if (git_buf_printf(&buf,
			"Accept: application/x-git-%s-result",
			s->service) < 0)
			goto on_error;

		if (git__utf8_to_16(ct, MAX_CONTENT_TYPE_LEN, git_buf_cstr(&buf)) < 0) {
			giterr_set(GITERR_OS, "failed to convert accept header to wide characters");
			goto on_error;
		}

		if (!WinHttpAddRequestHeaders(s->request, ct, (ULONG)-1L,
			WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
			giterr_set(GITERR_OS, "failed to add a header to the request");
			goto on_error;
		}
	}

	for (i = 0; i < t->owner->custom_headers.count; i++) {
		if (t->owner->custom_headers.strings[i]) {
			git_buf_clear(&buf);
			git_buf_puts(&buf, t->owner->custom_headers.strings[i]);
			if (git__utf8_to_16(ct, MAX_CONTENT_TYPE_LEN, git_buf_cstr(&buf)) < 0) {
				giterr_set(GITERR_OS, "failed to convert custom header to wide characters");
				goto on_error;
			}

			if (!WinHttpAddRequestHeaders(s->request, ct, (ULONG)-1L,
				WINHTTP_ADDREQ_FLAG_ADD | WINHTTP_ADDREQ_FLAG_REPLACE)) {
				giterr_set(GITERR_OS, "failed to add a header to the request");
				goto on_error;
			}
		}
	}

	/* If requested, disable certificate validation */
	if (t->connection_data.use_ssl) {
		int flags;

		if (t->owner->parent.read_flags(&t->owner->parent, &flags) < 0)
			goto on_error;
	}

	/* If we have a credential on the subtransport, apply it to the request */
	if (t->cred &&
		t->cred->credtype == GIT_CREDTYPE_USERPASS_PLAINTEXT &&
		apply_userpass_credential(s->request, t->auth_mechanisms, t->cred) < 0)
		goto on_error;
	else if (t->cred &&
		t->cred->credtype == GIT_CREDTYPE_DEFAULT &&
		apply_default_credentials(s->request, t->auth_mechanisms) < 0)
		goto on_error;

	/* If no other credentials have been applied and the URL has username and
	 * password, use those */
	if (!t->cred && t->connection_data.user && t->connection_data.pass) {
		if (!t->url_cred &&
			git_cred_userpass_plaintext_new(&t->url_cred, t->connection_data.user, t->connection_data.pass) < 0)
			goto on_error;
		if (apply_userpass_credential(s->request, GIT_WINHTTP_AUTH_BASIC, t->url_cred) < 0)
			goto on_error;
	}

	/* We've done everything up to calling WinHttpSendRequest. */

	error = 0;

on_error:
	if (error < 0)
		winhttp_stream_close(s);

	git__free(proxy_url);
	git_buf_free(&buf);
	return error;
}

static int parse_unauthorized_response(
	HINTERNET request,
	int *allowed_types,
	int *allowed_mechanisms)
{
	DWORD supported, first, target;

	*allowed_types = 0;
	*allowed_mechanisms = 0;

	/* WinHttpQueryHeaders() must be called before WinHttpQueryAuthSchemes().
	 * We can assume this was already done, since we know we are unauthorized.
	 */
	if (!WinHttpQueryAuthSchemes(request, &supported, &first, &target)) {
		giterr_set(GITERR_OS, "failed to parse supported auth schemes");
		return -1;
	}

	if (WINHTTP_AUTH_SCHEME_NTLM & supported) {
		*allowed_types |= GIT_CREDTYPE_USERPASS_PLAINTEXT;
		*allowed_types |= GIT_CREDTYPE_DEFAULT;
		*allowed_mechanisms = GIT_WINHTTP_AUTH_NEGOTIATE;
	}

	if (WINHTTP_AUTH_SCHEME_NEGOTIATE & supported) {
		*allowed_types |= GIT_CREDTYPE_DEFAULT;
		*allowed_mechanisms = GIT_WINHTTP_AUTH_NEGOTIATE;
	}

	if (WINHTTP_AUTH_SCHEME_BASIC & supported) {
		*allowed_types |= GIT_CREDTYPE_USERPASS_PLAINTEXT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_BASIC;
	}

	if (WINHTTP_AUTH_SCHEME_DIGEST & supported) {
		*allowed_types |= GIT_CREDTYPE_USERPASS_PLAINTEXT;
		*allowed_mechanisms |= GIT_WINHTTP_AUTH_DIGEST;
	}

	return 0;
}

static int write_chunk(HINTERNET request, const char *buffer, size_t len)
{
	DWORD bytes_written;
	git_buf buf = GIT_BUF_INIT;

	/* Chunk header */
	git_buf_printf(&buf, "%X\r\n", len);

	if (git_buf_oom(&buf))
		return -1;

	if (!WinHttpWriteData(request,
		git_buf_cstr(&buf),	(DWORD)git_buf_len(&buf),
		&bytes_written)) {
		git_buf_free(&buf);
		giterr_set(GITERR_OS, "failed to write chunk header");
		return -1;
	}

	git_buf_free(&buf);

	/* Chunk body */
	if (!WinHttpWriteData(request,
		buffer, (DWORD)len,
		&bytes_written)) {
		giterr_set(GITERR_OS, "failed to write chunk");
		return -1;
	}

	/* Chunk footer */
	if (!WinHttpWriteData(request,
		"\r\n", 2,
		&bytes_written)) {
		giterr_set(GITERR_OS, "failed to write chunk footer");
		return -1;
	}

	return 0;
}

static int winhttp_close_connection(winhttp_subtransport *t)
{
	int ret = 0;

	if (t->connection) {
		if (!WinHttpCloseHandle(t->connection)) {
			giterr_set(GITERR_OS, "unable to close connection");
			ret = -1;
		}

		t->connection = NULL;
	}

	if (t->session) {
		if (!WinHttpCloseHandle(t->session)) {
			giterr_set(GITERR_OS, "unable to close session");
			ret = -1;
		}

		t->session = NULL;
	}

	return ret;
}

static int user_agent(git_buf *ua)
{
	const char *custom = git_libgit2__user_agent();

	git_buf_clear(ua);
	git_buf_PUTS(ua, "git/1.0 (");

	if (custom)
		git_buf_puts(ua, custom);
	else
		git_buf_PUTS(ua, "libgit2 " LIBGIT2_VERSION);

	return git_buf_putc(ua, ')');
}

static void CALLBACK winhttp_status(
	HINTERNET connection,
	DWORD_PTR ctx,
	DWORD code,
	LPVOID info,
	DWORD info_len)
{
	DWORD status;

	if (code != WINHTTP_CALLBACK_STATUS_SECURE_FAILURE)
		return;

	status = *((DWORD *)info);

	if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID))
		giterr_set(GITERR_NET, "SSL certificate issued for different common name");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID))
		giterr_set(GITERR_NET, "SSL certificate has expired");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA))
		giterr_set(GITERR_NET, "SSL certificate signed by unknown CA");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT))
		giterr_set(GITERR_NET, "SSL certificate is invalid");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED))
		giterr_set(GITERR_NET, "certificate revocation check failed");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED))
		giterr_set(GITERR_NET, "SSL certificate was revoked");
	else if ((status & WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR))
		giterr_set(GITERR_NET, "security libraries could not be loaded");
	else
		giterr_set(GITERR_NET, "unknown security error %d", status);
}

static int winhttp_connect(
	winhttp_subtransport *t)
{
	wchar_t *wide_host;
	int32_t port;
	wchar_t *wide_ua;
	git_buf ua = GIT_BUF_INIT;
	int error = -1;
	int default_timeout = TIMEOUT_INFINITE;
	int default_connect_timeout = DEFAULT_CONNECT_TIMEOUT;

	t->session = NULL;
	t->connection = NULL;

	/* Prepare port */
	if (git__strtol32(&port, t->connection_data.port, NULL, 10) < 0)
		return -1;

	/* Prepare host */
	if (git__utf8_to_16_alloc(&wide_host, t->connection_data.host) < 0) {
		giterr_set(GITERR_OS, "unable to convert host to wide characters");
		return -1;
	}

	if ((error = user_agent(&ua)) < 0) {
		git__free(wide_host);
		return error;
	}

	if (git__utf8_to_16_alloc(&wide_ua, git_buf_cstr(&ua)) < 0) {
		giterr_set(GITERR_OS, "unable to convert host to wide characters");
		git__free(wide_host);
		git_buf_free(&ua);
		return -1;
	}

	git_buf_free(&ua);

	/* Establish session */
	t->session = WinHttpOpen(
		wide_ua,
		WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
		WINHTTP_NO_PROXY_NAME,
		WINHTTP_NO_PROXY_BYPASS,
		0);

	if (!t->session) {
		giterr_set(GITERR_OS, "failed to init WinHTTP");
		goto on_error;
	}

	if (!WinHttpSetTimeouts(t->session, default_timeout, default_connect_timeout, default_timeout, default_timeout)) {
		giterr_set(GITERR_OS, "failed to set timeouts for WinHTTP");
		goto on_error;
	}


	/* Establish connection */
	t->connection = WinHttpConnect(
		t->session,
		wide_host,
		(INTERNET_PORT) port,
		0);

	if (!t->connection) {
		giterr_set(GITERR_OS, "failed to connect to host");
		goto on_error;
	}

	if (WinHttpSetStatusCallback(t->connection, winhttp_status, WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, 0) == WINHTTP_INVALID_STATUS_CALLBACK) {
		giterr_set(GITERR_OS, "failed to set status callback");
		goto on_error;
	}

	error = 0;

on_error:
	if (error < 0)
		winhttp_close_connection(t);

	git__free(wide_host);
	git__free(wide_ua);

	return error;
}

static int do_send_request(winhttp_stream *s, size_t len, int ignore_length)
{
	if (ignore_length) {
		if (!WinHttpSendRequest(s->request,
			WINHTTP_NO_ADDITIONAL_HEADERS, 0,
			WINHTTP_NO_REQUEST_DATA, 0,
			WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH, 0)) {
			return -1;
		}
	} else {
		if (!WinHttpSendRequest(s->request,
			WINHTTP_NO_ADDITIONAL_HEADERS, 0,
			WINHTTP_NO_REQUEST_DATA, 0,
			len, 0)) {
			return -1;
		}
	}

	return 0;
}

static int send_request(winhttp_stream *s, size_t len, int ignore_length)
{
	int request_failed = 0, cert_valid = 1, error = 0;
	DWORD ignore_flags;

	giterr_clear();
	if ((error = do_send_request(s, len, ignore_length)) < 0) {
		if (GetLastError() != ERROR_WINHTTP_SECURE_FAILURE) {
			giterr_set(GITERR_OS, "failed to send request");
			return -1;
		}

		request_failed = 1;
		cert_valid = 0;
	}

	giterr_clear();
	if ((error = certificate_check(s, cert_valid)) < 0) {
		if (!giterr_last())
			giterr_set(GITERR_OS, "user cancelled certificate check");

		return error;
	}

	/* if neither the request nor the certificate check returned errors, we're done */
	if (!request_failed)
		return 0;

	ignore_flags = no_check_cert_flags;

	if (!WinHttpSetOption(s->request, WINHTTP_OPTION_SECURITY_FLAGS, &ignore_flags, sizeof(ignore_flags))) {
		giterr_set(GITERR_OS, "failed to set security options");
		return -1;
	}

	if ((error = do_send_request(s, len, ignore_length)) < 0)
		giterr_set(GITERR_OS, "failed to send request with unchecked certificate");

	return error;
}

static int winhttp_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	winhttp_subtransport *t = OWNING_SUBTRANSPORT(s);
	DWORD dw_bytes_read;
	char replay_count = 0;
	int error;

replay:
	/* Enforce a reasonable cap on the number of replays */
	if (++replay_count >= 7) {
		giterr_set(GITERR_NET, "too many redirects or authentication replays");
		return -1;
	}

	/* Connect if necessary */
	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	if (!s->received_response) {
		DWORD status_code, status_code_length, content_type_length, bytes_written;
		char expected_content_type_8[MAX_CONTENT_TYPE_LEN];
		wchar_t expected_content_type[MAX_CONTENT_TYPE_LEN], content_type[MAX_CONTENT_TYPE_LEN];

		if (!s->sent_request) {

			if ((error = send_request(s, s->post_body_len, 0)) < 0)
				return error;

			s->sent_request = 1;
		}

		if (s->chunked) {
			assert(s->verb == post_verb);

			/* Flush, if necessary */
			if (s->chunk_buffer_len > 0 &&
				write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			/* Write the final chunk. */
			if (!WinHttpWriteData(s->request,
				"0\r\n\r\n", 5,
				&bytes_written)) {
				giterr_set(GITERR_OS, "failed to write final chunk");
				return -1;
			}
		}
		else if (s->post_body) {
			char *buffer;
			DWORD len = s->post_body_len, bytes_read;

			if (INVALID_SET_FILE_POINTER == SetFilePointer(s->post_body,
					0, 0, FILE_BEGIN) &&
				NO_ERROR != GetLastError()) {
				giterr_set(GITERR_OS, "failed to reset file pointer");
				return -1;
			}

			buffer = git__malloc(CACHED_POST_BODY_BUF_SIZE);

			while (len > 0) {
				DWORD bytes_written;

				if (!ReadFile(s->post_body, buffer,
					min(CACHED_POST_BODY_BUF_SIZE, len),
					&bytes_read, NULL) ||
					!bytes_read) {
					git__free(buffer);
					giterr_set(GITERR_OS, "failed to read from temp file");
					return -1;
				}

				if (!WinHttpWriteData(s->request, buffer,
					bytes_read, &bytes_written)) {
					git__free(buffer);
					giterr_set(GITERR_OS, "failed to write data");
					return -1;
				}

				len -= bytes_read;
				assert(bytes_read == bytes_written);
			}

			git__free(buffer);

			/* Eagerly close the temp file */
			CloseHandle(s->post_body);
			s->post_body = NULL;
		}

		if (!WinHttpReceiveResponse(s->request, 0)) {
			giterr_set(GITERR_OS, "failed to receive response");
			return -1;
		}

		/* Verify that we got a 200 back */
		status_code_length = sizeof(status_code);

		if (!WinHttpQueryHeaders(s->request,
			WINHTTP_QUERY_STATUS_CODE | WINHTTP_QUERY_FLAG_NUMBER,
			WINHTTP_HEADER_NAME_BY_INDEX,
			&status_code, &status_code_length,
			WINHTTP_NO_HEADER_INDEX)) {
				giterr_set(GITERR_OS, "failed to retrieve status code");
				return -1;
		}

		/* The implementation of WinHTTP prior to Windows 7 will not
		 * redirect to an identical URI. Some Git hosters use self-redirects
		 * as part of their DoS mitigation strategy. Check first to see if we
		 * have a redirect status code, and that we haven't already streamed
		 * a post body. (We can't replay a streamed POST.) */
		if (!s->chunked &&
			(HTTP_STATUS_MOVED == status_code ||
			 HTTP_STATUS_REDIRECT == status_code ||
			 (HTTP_STATUS_REDIRECT_METHOD == status_code &&
			  get_verb == s->verb) ||
			 HTTP_STATUS_REDIRECT_KEEP_VERB == status_code)) {

			/* Check for Windows 7. This workaround is only necessary on
			 * Windows Vista and earlier. Windows 7 is version 6.1. */
			wchar_t *location;
			DWORD location_length;
			char *location8;

			/* OK, fetch the Location header from the redirect. */
			if (WinHttpQueryHeaders(s->request,
				WINHTTP_QUERY_LOCATION,
				WINHTTP_HEADER_NAME_BY_INDEX,
				WINHTTP_NO_OUTPUT_BUFFER,
				&location_length,
				WINHTTP_NO_HEADER_INDEX) ||
				GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
				giterr_set(GITERR_OS, "failed to read Location header");
				return -1;
			}

			location = git__malloc(location_length);
			GITERR_CHECK_ALLOC(location);

			if (!WinHttpQueryHeaders(s->request,
				WINHTTP_QUERY_LOCATION,
				WINHTTP_HEADER_NAME_BY_INDEX,
				location,
				&location_length,
				WINHTTP_NO_HEADER_INDEX)) {
				giterr_set(GITERR_OS, "failed to read Location header");
				git__free(location);
				return -1;
			}

			/* Convert the Location header to UTF-8 */
			if (git__utf16_to_8_alloc(&location8, location) < 0) {
				giterr_set(GITERR_OS, "failed to convert Location header to UTF-8");
				git__free(location);
				return -1;
			}

			git__free(location);

			/* Replay the request */
			winhttp_stream_close(s);

			if (!git__prefixcmp_icase(location8, prefix_https)) {
				/* Upgrade to secure connection; disconnect and start over */
				if (gitno_connection_data_from_url(&t->connection_data, location8, s->service_url) < 0) {
					git__free(location8);
					return -1;
				}

				winhttp_close_connection(t);

				if (winhttp_connect(t) < 0)
					return -1;
			}

			git__free(location8);
			goto replay;
		}

		/* Handle proxy authentication failures */
		if (status_code == HTTP_STATUS_PROXY_AUTH_REQ) {
			int allowed_types;

			if (parse_unauthorized_response(s->request, &allowed_types, &t->auth_mechanisms) < 0)
				return -1;

			/* TODO: extract the username from the url, no payload? */
			if (t->owner->proxy.credentials) {
				int cred_error = 1;
				cred_error = t->owner->proxy.credentials(&t->proxy_cred, t->owner->proxy.url, NULL, allowed_types, t->owner->proxy.payload);

				if (cred_error < 0)
					return cred_error;
			}

			winhttp_stream_close(s);
			goto replay;
		}

		/* Handle authentication failures */
		if (HTTP_STATUS_DENIED == status_code && get_verb == s->verb) {
			int allowed_types;

			if (parse_unauthorized_response(s->request, &allowed_types, &t->auth_mechanisms) < 0)
				return -1;

			if (allowed_types) {
				int cred_error = 1;

				git_cred_free(t->cred);
				t->cred = NULL;
				/* Start with the user-supplied credential callback, if present */
				if (t->owner->cred_acquire_cb) {
					cred_error = t->owner->cred_acquire_cb(&t->cred, t->owner->url,
						t->connection_data.user, allowed_types,	t->owner->cred_acquire_payload);

					/* Treat GIT_PASSTHROUGH as though git_cred_acquire_cb isn't set */
					if (cred_error == GIT_PASSTHROUGH)
						cred_error = 1;
					else if (cred_error < 0)
						return cred_error;
				}

				/* Invoke the fallback credentials acquisition callback if necessary */
				if (cred_error > 0) {
					cred_error = fallback_cred_acquire_cb(&t->cred, t->owner->url,
						t->connection_data.user, allowed_types, NULL);

					if (cred_error < 0)
						return cred_error;
				}

				if (!cred_error) {
					assert(t->cred);

					winhttp_stream_close(s);

					/* Successfully acquired a credential */
					goto replay;
				}
			}
		}

		if (HTTP_STATUS_OK != status_code) {
			giterr_set(GITERR_NET, "request failed with status code: %d", status_code);
			return -1;
		}

		/* Verify that we got the correct content-type back */
		if (post_verb == s->verb)
			p_snprintf(expected_content_type_8, MAX_CONTENT_TYPE_LEN, "application/x-git-%s-result", s->service);
		else
			p_snprintf(expected_content_type_8, MAX_CONTENT_TYPE_LEN, "application/x-git-%s-advertisement", s->service);

		if (git__utf8_to_16(expected_content_type, MAX_CONTENT_TYPE_LEN, expected_content_type_8) < 0) {
			giterr_set(GITERR_OS, "failed to convert expected content-type to wide characters");
			return -1;
		}

		content_type_length = sizeof(content_type);

		if (!WinHttpQueryHeaders(s->request,
			WINHTTP_QUERY_CONTENT_TYPE,
			WINHTTP_HEADER_NAME_BY_INDEX,
			&content_type, &content_type_length,
			WINHTTP_NO_HEADER_INDEX)) {
				giterr_set(GITERR_OS, "failed to retrieve response content-type");
				return -1;
		}

		if (wcscmp(expected_content_type, content_type)) {
			giterr_set(GITERR_NET, "received unexpected content-type");
			return -1;
		}

		s->received_response = 1;
	}

	if (!WinHttpReadData(s->request,
		(LPVOID)buffer,
		(DWORD)buf_size,
		&dw_bytes_read))
	{
		giterr_set(GITERR_OS, "failed to read data");
		return -1;
	}

	*bytes_read = dw_bytes_read;

	return 0;
}

static int winhttp_stream_write_single(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	DWORD bytes_written;
	int error;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	/* This implementation of write permits only a single call. */
	if (s->sent_request) {
		giterr_set(GITERR_NET, "subtransport configured for only one write");
		return -1;
	}

	if ((error = send_request(s, len, 0)) < 0)
		return error;

	s->sent_request = 1;

	if (!WinHttpWriteData(s->request,
			(LPCVOID)buffer,
			(DWORD)len,
			&bytes_written)) {
		giterr_set(GITERR_OS, "failed to write data");
		return -1;
	}

	assert((DWORD)len == bytes_written);

	return 0;
}

static int put_uuid_string(LPWSTR buffer, size_t buffer_len_cch)
{
	UUID uuid;
	RPC_STATUS status = UuidCreate(&uuid);
	int result;

	if (RPC_S_OK != status &&
		RPC_S_UUID_LOCAL_ONLY != status &&
		RPC_S_UUID_NO_ADDRESS != status) {
		giterr_set(GITERR_NET, "unable to generate name for temp file");
		return -1;
	}

	if (buffer_len_cch < UUID_LENGTH_CCH + 1) {
		giterr_set(GITERR_NET, "buffer too small for name of temp file");
		return -1;
	}

#if !defined(__MINGW32__) || defined(MINGW_HAS_SECURE_API)
	result = swprintf_s(buffer, buffer_len_cch,
#else
	result = wsprintfW(buffer,
#endif
		L"%08x%04x%04x%02x%02x%02x%02x%02x%02x%02x%02x",
		uuid.Data1, uuid.Data2, uuid.Data3,
		uuid.Data4[0], uuid.Data4[1], uuid.Data4[2], uuid.Data4[3],
		uuid.Data4[4], uuid.Data4[5], uuid.Data4[6], uuid.Data4[7]);

	if (result < UUID_LENGTH_CCH) {
		giterr_set(GITERR_OS, "unable to generate name for temp file");
		return -1;
	}

	return 0;
}

static int get_temp_file(LPWSTR buffer, DWORD buffer_len_cch)
{
	size_t len;

	if (!GetTempPathW(buffer_len_cch, buffer)) {
		giterr_set(GITERR_OS, "failed to get temp path");
		return -1;
	}

	len = wcslen(buffer);

	if (buffer[len - 1] != '\\' && len < buffer_len_cch)
		buffer[len++] = '\\';

	if (put_uuid_string(&buffer[len], (size_t)buffer_len_cch - len) < 0)
		return -1;

	return 0;
}

static int winhttp_stream_write_buffered(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	DWORD bytes_written;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	/* Buffer the payload, using a temporary file so we delegate
	 * memory management of the data to the operating system. */
	if (!s->post_body) {
		wchar_t temp_path[MAX_PATH + 1];

		if (get_temp_file(temp_path, MAX_PATH + 1) < 0)
			return -1;

		s->post_body = CreateFileW(temp_path,
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_DELETE, NULL,
			CREATE_NEW,
			FILE_ATTRIBUTE_TEMPORARY | FILE_FLAG_DELETE_ON_CLOSE | FILE_FLAG_SEQUENTIAL_SCAN,
			NULL);

		if (INVALID_HANDLE_VALUE == s->post_body) {
			s->post_body = NULL;
			giterr_set(GITERR_OS, "failed to create temporary file");
			return -1;
		}
	}

	if (!WriteFile(s->post_body, buffer, (DWORD)len, &bytes_written, NULL)) {
		giterr_set(GITERR_OS, "failed to write to temporary file");
		return -1;
	}

	assert((DWORD)len == bytes_written);

	s->post_body_len += bytes_written;

	return 0;
}

static int winhttp_stream_write_chunked(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	winhttp_stream *s = (winhttp_stream *)stream;
	int error;

	if (!s->request && winhttp_stream_connect(s) < 0)
		return -1;

	if (!s->sent_request) {
		/* Send Transfer-Encoding: chunked header */
		if (!WinHttpAddRequestHeaders(s->request,
			transfer_encoding, (ULONG) -1L,
			WINHTTP_ADDREQ_FLAG_ADD)) {
			giterr_set(GITERR_OS, "failed to add a header to the request");
			return -1;
		}

		if ((error = send_request(s, 0, 1)) < 0)
			return error;

		s->sent_request = 1;
	}

	if (len > CACHED_POST_BODY_BUF_SIZE) {
		/* Flush, if necessary */
		if (s->chunk_buffer_len > 0) {
			if (write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;
		}

		/* Write chunk directly */
		if (write_chunk(s->request, buffer, len) < 0)
			return -1;
	}
	else {
		/* Append as much to the buffer as we can */
		int count = (int)min(CACHED_POST_BODY_BUF_SIZE - s->chunk_buffer_len, len);

		if (!s->chunk_buffer)
			s->chunk_buffer = git__malloc(CACHED_POST_BODY_BUF_SIZE);

		memcpy(s->chunk_buffer + s->chunk_buffer_len, buffer, count);
		s->chunk_buffer_len += count;
		buffer += count;
		len -= count;

		/* Is the buffer full? If so, then flush */
		if (CACHED_POST_BODY_BUF_SIZE == s->chunk_buffer_len) {
			if (write_chunk(s->request, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			/* Is there any remaining data from the source? */
			if (len > 0) {
				memcpy(s->chunk_buffer, buffer, len);
				s->chunk_buffer_len = (unsigned int)len;
			}
		}
	}

	return 0;
}

static void winhttp_stream_free(git_smart_subtransport_stream *stream)
{
	winhttp_stream *s = (winhttp_stream *)stream;

	winhttp_stream_close(s);
	git__free(s);
}

static int winhttp_stream_alloc(winhttp_subtransport *t, winhttp_stream **stream)
{
	winhttp_stream *s;

	if (!stream)
		return -1;

	s = git__calloc(1, sizeof(winhttp_stream));
	GITERR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = winhttp_stream_read;
	s->parent.write = winhttp_stream_write_single;
	s->parent.free = winhttp_stream_free;

	*stream = s;

	return 0;
}

static int winhttp_uploadpack_ls(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = upload_pack_service;
	s->service_url = upload_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int winhttp_uploadpack(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = upload_pack_service;
	s->service_url = upload_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int winhttp_receivepack_ls(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	s->service = receive_pack_service;
	s->service_url = receive_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int winhttp_receivepack(
	winhttp_subtransport *t,
	winhttp_stream *s)
{
	GIT_UNUSED(t);

	/* WinHTTP only supports Transfer-Encoding: chunked
	 * on Windows Vista (NT 6.0) and higher. */
	s->chunked = git_has_win32_version(6, 0, 0);

	if (s->chunked)
		s->parent.write = winhttp_stream_write_chunked;
	else
		s->parent.write = winhttp_stream_write_buffered;

	s->service = receive_pack_service;
	s->service_url = receive_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int winhttp_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;
	winhttp_stream *s;
	int ret = -1;

	if (!t->connection)
		if ((ret = gitno_connection_data_from_url(&t->connection_data, url, NULL)) < 0 ||
			 (ret = winhttp_connect(t)) < 0)
			return ret;

	if (winhttp_stream_alloc(t, &s) < 0)
		return -1;

	if (!stream)
		return -1;

	switch (action)
	{
		case GIT_SERVICE_UPLOADPACK_LS:
			ret = winhttp_uploadpack_ls(t, s);
			break;

		case GIT_SERVICE_UPLOADPACK:
			ret = winhttp_uploadpack(t, s);
			break;

		case GIT_SERVICE_RECEIVEPACK_LS:
			ret = winhttp_receivepack_ls(t, s);
			break;

		case GIT_SERVICE_RECEIVEPACK:
			ret = winhttp_receivepack(t, s);
			break;

		default:
			assert(0);
	}

	if (!ret)
		*stream = &s->parent;

	return ret;
}

static int winhttp_close(git_smart_subtransport *subtransport)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;

	gitno_connection_data_free_ptrs(&t->connection_data);
	memset(&t->connection_data, 0x0, sizeof(gitno_connection_data));
	gitno_connection_data_free_ptrs(&t->proxy_connection_data);
	memset(&t->proxy_connection_data, 0x0, sizeof(gitno_connection_data));

	if (t->cred) {
		t->cred->free(t->cred);
		t->cred = NULL;
	}

	if (t->proxy_cred) {
		t->proxy_cred->free(t->proxy_cred);
		t->proxy_cred = NULL;
	}

	if (t->url_cred) {
		t->url_cred->free(t->url_cred);
		t->url_cred = NULL;
	}

	return winhttp_close_connection(t);
}

static void winhttp_free(git_smart_subtransport *subtransport)
{
	winhttp_subtransport *t = (winhttp_subtransport *)subtransport;

	winhttp_close(subtransport);

	git__free(t);
}

int git_smart_subtransport_http(git_smart_subtransport **out, git_transport *owner, void *param)
{
	winhttp_subtransport *t;

	GIT_UNUSED(param);

	if (!out)
		return -1;

	t = git__calloc(1, sizeof(winhttp_subtransport));
	GITERR_CHECK_ALLOC(t);

	t->owner = (transport_smart *)owner;
	t->parent.action = winhttp_action;
	t->parent.close = winhttp_close;
	t->parent.free = winhttp_free;

	*out = (git_smart_subtransport *) t;
	return 0;
}

#endif /* GIT_WINHTTP */
