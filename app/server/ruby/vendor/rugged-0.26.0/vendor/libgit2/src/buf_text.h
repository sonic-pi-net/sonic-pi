/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_buf_text_h__
#define INCLUDE_buf_text_h__

#include "common.h"

#include "buffer.h"

typedef enum {
	GIT_BOM_NONE = 0,
	GIT_BOM_UTF8 = 1,
	GIT_BOM_UTF16_LE = 2,
	GIT_BOM_UTF16_BE = 3,
	GIT_BOM_UTF32_LE = 4,
	GIT_BOM_UTF32_BE = 5
} git_bom_t;

typedef struct {
	git_bom_t bom; /* BOM found at head of text */
	unsigned int nul, cr, lf, crlf; /* NUL, CR, LF and CRLF counts */
	unsigned int printable, nonprintable; /* These are just approximations! */
} git_buf_text_stats;

/**
 * Append string to buffer, prefixing each character from `esc_chars` with
 * `esc_with` string.
 *
 * @param buf Buffer to append data to
 * @param string String to escape and append
 * @param esc_chars Characters to be escaped
 * @param esc_with String to insert in from of each found character
 * @return 0 on success, <0 on failure (probably allocation problem)
 */
extern int git_buf_text_puts_escaped(
	git_buf *buf,
	const char *string,
	const char *esc_chars,
	const char *esc_with);

/**
 * Append string escaping characters that are regex special
 */
GIT_INLINE(int) git_buf_text_puts_escape_regex(git_buf *buf, const char *string)
{
	return git_buf_text_puts_escaped(buf, string, "^.[]$()|*+?{}\\", "\\");
}

/**
 * Unescape all characters in a buffer in place
 *
 * I.e. remove backslashes
 */
extern void git_buf_text_unescape(git_buf *buf);

/**
 * Replace all \r\n with \n.
 *
 * @return 0 on success, -1 on memory error
 */
extern int git_buf_text_crlf_to_lf(git_buf *tgt, const git_buf *src);

/**
 * Replace all \n with \r\n. Does not modify existing \r\n.
 *
 * @return 0 on success, -1 on memory error
 */
extern int git_buf_text_lf_to_crlf(git_buf *tgt, const git_buf *src);

/**
 * Fill buffer with the common prefix of a array of strings
 *
 * Buffer will be set to empty if there is no common prefix
 */
extern int git_buf_text_common_prefix(git_buf *buf, const git_strarray *strs);

/**
 * Check quickly if buffer looks like it contains binary data
 *
 * @param buf Buffer to check
 * @return true if buffer looks like non-text data
 */
extern bool git_buf_text_is_binary(const git_buf *buf);

/**
 * Check quickly if buffer contains a NUL byte
 *
 * @param buf Buffer to check
 * @return true if buffer contains a NUL byte
 */
extern bool git_buf_text_contains_nul(const git_buf *buf);

/**
 * Check if a buffer begins with a UTF BOM
 *
 * @param bom Set to the type of BOM detected or GIT_BOM_NONE
 * @param buf Buffer in which to check the first bytes for a BOM
 * @param offset Offset into buffer to look for BOM
 * @return Number of bytes of BOM data (or 0 if no BOM found)
 */
extern int git_buf_text_detect_bom(
	git_bom_t *bom, const git_buf *buf, size_t offset);

/**
 * Gather stats for a piece of text
 *
 * Fill the `stats` structure with counts of unreadable characters, carriage
 * returns, etc, so it can be used in heuristics.  This automatically skips
 * a trailing EOF (\032 character).  Also it will look for a BOM at the
 * start of the text and can be told to skip that as well.
 *
 * @param stats Structure to be filled in
 * @param buf Text to process
 * @param skip_bom Exclude leading BOM from stats if true
 * @return Does the buffer heuristically look like binary data
 */
extern bool git_buf_text_gather_stats(
	git_buf_text_stats *stats, const git_buf *buf, bool skip_bom);

#endif
