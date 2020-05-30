/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "w32_stack.h"

#if defined(GIT_MSVC_CRTDBG)
#include "Windows.h"
#include "Dbghelp.h"
#include "win32/posix.h"
#include "hash.h"

static bool   g_win32_stack_initialized = false;
static HANDLE g_win32_stack_process = INVALID_HANDLE_VALUE;
static git_win32__stack__aux_cb_alloc  g_aux_cb_alloc  = NULL;
static git_win32__stack__aux_cb_lookup g_aux_cb_lookup = NULL;

int git_win32__stack__set_aux_cb(
	git_win32__stack__aux_cb_alloc cb_alloc,
	git_win32__stack__aux_cb_lookup cb_lookup)
{
	g_aux_cb_alloc = cb_alloc;
	g_aux_cb_lookup = cb_lookup;

	return 0;
}

void git_win32__stack_init(void)
{
	if (!g_win32_stack_initialized) {
		g_win32_stack_process = GetCurrentProcess();
		SymSetOptions(SYMOPT_LOAD_LINES);
		SymInitialize(g_win32_stack_process, NULL, TRUE);
		g_win32_stack_initialized = true;
	}
}

void git_win32__stack_cleanup(void)
{
	if (g_win32_stack_initialized) {
		SymCleanup(g_win32_stack_process);
		g_win32_stack_process = INVALID_HANDLE_VALUE;
		g_win32_stack_initialized = false;
	}
}

int git_win32__stack_capture(git_win32__stack__raw_data *pdata, int skip)
{
	if (!g_win32_stack_initialized) {
		git_error_set(GIT_ERROR_INVALID, "git_win32_stack not initialized.");
		return GIT_ERROR;
	}

	memset(pdata, 0, sizeof(*pdata));
	pdata->nr_frames = RtlCaptureStackBackTrace(
		skip+1, GIT_WIN32__STACK__MAX_FRAMES, pdata->frames, NULL);

	/* If an "aux" data provider was registered, ask it to capture
	 * whatever data it needs and give us an "aux_id" to it so that
	 * we can refer to it later when reporting.
	 */
	if (g_aux_cb_alloc)
		(g_aux_cb_alloc)(&pdata->aux_id);

	return 0;
}

int git_win32__stack_compare(
	git_win32__stack__raw_data *d1,
	git_win32__stack__raw_data *d2)
{
	return memcmp(d1, d2, sizeof(*d1));
}

int git_win32__stack_format(
	char *pbuf, size_t buf_len,
	const git_win32__stack__raw_data *pdata,
	const char *prefix, const char *suffix)
{
#define MY_MAX_FILENAME 255

	/* SYMBOL_INFO has char FileName[1] at the end.  The docs say to
	 * to malloc it with extra space for your desired max filename.
	 */
	struct {
		SYMBOL_INFO symbol;
		char extra[MY_MAX_FILENAME + 1];
	} s;

	IMAGEHLP_LINE64 line;
	size_t buf_used = 0;
	unsigned int k;
	char detail[MY_MAX_FILENAME * 2]; /* filename plus space for function name and formatting */
	size_t detail_len;

	if (!g_win32_stack_initialized) {
		git_error_set(GIT_ERROR_INVALID, "git_win32_stack not initialized.");
		return GIT_ERROR;
	}

	if (!prefix)
		prefix = "\t";
	if (!suffix)
		suffix = "\n";

	memset(pbuf, 0, buf_len);

	memset(&s, 0, sizeof(s));
	s.symbol.MaxNameLen = MY_MAX_FILENAME;
	s.symbol.SizeOfStruct = sizeof(SYMBOL_INFO);

	memset(&line, 0, sizeof(line));
	line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);

	for (k=0; k < pdata->nr_frames; k++) {
		DWORD64 frame_k = (DWORD64)pdata->frames[k];
		DWORD dwUnused;

		if (SymFromAddr(g_win32_stack_process, frame_k, 0, &s.symbol) &&
			SymGetLineFromAddr64(g_win32_stack_process, frame_k, &dwUnused, &line)) {
			const char *pslash;
			const char *pfile;

			pslash = strrchr(line.FileName, '\\');
			pfile = ((pslash) ? (pslash+1) : line.FileName);
			p_snprintf(detail, sizeof(detail), "%s%s:%d> %s%s",
					   prefix, pfile, line.LineNumber, s.symbol.Name, suffix);
		} else {
			/* This happens when we cross into another module.
			 * For example, in CLAR tests, this is typically
			 * the CRT startup code.  Just print an unknown
			 * frame and continue.
			 */
			p_snprintf(detail, sizeof(detail), "%s??%s", prefix, suffix);
		}
		detail_len = strlen(detail);

		if (buf_len < (buf_used + detail_len + 1)) {
			/* we don't have room for this frame in the buffer, so just stop. */
			break;
		}

		memcpy(&pbuf[buf_used], detail, detail_len);
		buf_used += detail_len;
	}

	/* "aux_id" 0 is reserved to mean no aux data. This is needed to handle
	 * allocs that occur before the aux callbacks were registered.
	 */
	if (pdata->aux_id > 0) {
		p_snprintf(detail, sizeof(detail), "%saux_id: %d%s",
				   prefix, pdata->aux_id, suffix);
		detail_len = strlen(detail);
		if ((buf_used + detail_len + 1) < buf_len) {
			memcpy(&pbuf[buf_used], detail, detail_len);
			buf_used += detail_len;
		}

		/* If an "aux" data provider is still registered, ask it to append its detailed
		 * data to the end of ours using the "aux_id" it gave us when this de-duped
		 * item was created.
		 */
		if (g_aux_cb_lookup)
			(g_aux_cb_lookup)(pdata->aux_id, &pbuf[buf_used], (buf_len - buf_used - 1));
	}

	return GIT_OK;
}

int git_win32__stack(
	char * pbuf, size_t buf_len,
	int skip,
	const char *prefix, const char *suffix)
{
	git_win32__stack__raw_data data;
	int error;

	if ((error = git_win32__stack_capture(&data, skip)) < 0)
		return error;
	if ((error = git_win32__stack_format(pbuf, buf_len, &data, prefix, suffix)) < 0)
		return error;
	return 0;
}

#endif
