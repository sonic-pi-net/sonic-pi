/*
 * The MIT License
 *
 * Copyright (c) 2014 GitHub, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include "rugged.h"

extern VALUE rb_cRuggedDiff;
VALUE rb_cRuggedDiffLine;

VALUE rugged_diff_line_new(const git_diff_line *line)
{
	VALUE rb_line = rb_class_new_instance(0, NULL, rb_cRuggedDiffLine), rb_line_origin;

	switch(line->origin) {
		case GIT_DIFF_LINE_CONTEXT:
			rb_line_origin = CSTR2SYM("context");
			break;
		case GIT_DIFF_LINE_ADDITION:
			rb_line_origin = CSTR2SYM("addition");
			break;
		case GIT_DIFF_LINE_DELETION:
			rb_line_origin = CSTR2SYM("deletion");
			break;
		case GIT_DIFF_LINE_CONTEXT_EOFNL: /* neither file has newline at the end */
			rb_line_origin = CSTR2SYM("eof_no_newline");
			break;
		case GIT_DIFF_LINE_ADD_EOFNL: /* added at end of old file */
			rb_line_origin = CSTR2SYM("eof_newline_added");
			break;
		case GIT_DIFF_LINE_DEL_EOFNL: /* removed at end of old file */
			rb_line_origin = CSTR2SYM("eof_newline_removed");
			break;
		case GIT_DIFF_LINE_FILE_HDR:
			rb_line_origin = CSTR2SYM("file_header");
			break;
		case GIT_DIFF_LINE_HUNK_HDR:
			rb_line_origin = CSTR2SYM("hunk_header");
			break;
		case GIT_DIFF_LINE_BINARY:
			rb_line_origin = CSTR2SYM("binary");
			break;			
		default:
			/* FIXME: raise here instead? */
			rb_line_origin = CSTR2SYM("unknown");
	}

	rb_iv_set(rb_line, "@line_origin", rb_line_origin);
	rb_iv_set(rb_line, "@content", rb_str_new(line->content, line->content_len));
	rb_iv_set(rb_line, "@old_lineno", INT2FIX(line->old_lineno));
	rb_iv_set(rb_line, "@new_lineno", INT2FIX(line->new_lineno));

	if (line->content_offset == -1)
		rb_iv_set(rb_line, "@content_offset", Qnil);
	else
		rb_iv_set(rb_line, "@content_offset", INT2FIX(line->content_offset));

	return rb_line;
}

void Init_rugged_diff_line(void)
{
	rb_cRuggedDiffLine = rb_define_class_under(rb_cRuggedDiff, "Line", rb_cObject);
}
