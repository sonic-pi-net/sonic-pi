/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "message.h"

static size_t line_length_without_trailing_spaces(const char *line, size_t len)
{
	while (len) {
		unsigned char c = line[len - 1];
		if (!git__isspace(c))
			break;
		len--;
	}

	return len;
}

/* Greatly inspired from git.git "stripspace" */
/* see https://github.com/git/git/blob/497215d8811ac7b8955693ceaad0899ecd894ed2/builtin/stripspace.c#L4-67 */
int git_message_prettify(git_buf *message_out, const char *message, int strip_comments, char comment_char)
{
	const size_t message_len = strlen(message);

	int consecutive_empty_lines = 0;
	size_t i, line_length, rtrimmed_line_length;
	char *next_newline;

	git_buf_sanitize(message_out);

	for (i = 0; i < strlen(message); i += line_length) {
		next_newline = memchr(message + i, '\n', message_len - i);

		if (next_newline != NULL) {
			line_length = next_newline - (message + i) + 1;
		} else {
			line_length = message_len - i;
		}

		if (strip_comments && line_length && message[i] == comment_char)
			continue;

		rtrimmed_line_length = line_length_without_trailing_spaces(message + i, line_length);

		if (!rtrimmed_line_length) {
			consecutive_empty_lines++;
			continue;
		}

		if (consecutive_empty_lines > 0 && message_out->size > 0)
			git_buf_putc(message_out, '\n');

		consecutive_empty_lines = 0;
		git_buf_put(message_out, message + i, rtrimmed_line_length);
		git_buf_putc(message_out, '\n');
	}

	return git_buf_oom(message_out) ? -1 : 0;
}
