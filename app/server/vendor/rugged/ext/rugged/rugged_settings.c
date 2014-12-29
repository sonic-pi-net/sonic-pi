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

#if !defined(NUM2SIZET)
#  if SIZEOF_SIZE_T == SIZEOF_LONG
#    define NUM2SIZET(n) ((size_t)NUM2ULONG(n))
#    define SIZET2NUM(n) ((size_t)ULONG2NUM(n))
#  else
#    define NUM2SIZET(n) ((size_t)NUM2ULL(n))
#    define SIZET2NUM(n) ((size_t)ULL2NUM(n))
#  endif
#endif /* ! defined(NUM2SIZET) */

extern VALUE rb_mRugged;

static void set_search_path(int level, VALUE value)
{
	const char *path;

	Check_Type(value, T_STRING);
	path = StringValueCStr(value);

	rugged_exception_check(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, level, path));
}

static VALUE get_search_path(int level)
{
	git_buf buf = {NULL};
	VALUE ret;

	rugged_exception_check(git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH, level, &buf));

	ret = rb_str_new_utf8(buf.ptr);
	git_buf_free(&buf);

	return ret;
}

/*
 *  call-seq:
 *    Settings[option] = value
 *
 *  Sets a libgit2 library option.
 */
static VALUE rb_git_set_option(VALUE self, VALUE option, VALUE value)
{
	const char *opt;

	Check_Type(option, T_STRING);
	opt = StringValueCStr(option);

	if (strcmp(opt, "mwindow_size") == 0) {
		size_t val;
		Check_Type(value, T_FIXNUM);
		val = NUM2SIZET(value);
		git_libgit2_opts(GIT_OPT_SET_MWINDOW_SIZE, val);
	}
	
	else if (strcmp(opt, "mwindow_mapped_limit") == 0) {
		size_t val;
		Check_Type(value, T_FIXNUM);
		val = NUM2SIZET(value);
		git_libgit2_opts(GIT_OPT_SET_MWINDOW_MAPPED_LIMIT, val);
	}

	else if (strcmp(opt, "search_path_global") == 0) {
		set_search_path(GIT_CONFIG_LEVEL_GLOBAL, value);
	}

	else if (strcmp(opt, "search_path_xdg") == 0) {
		set_search_path(GIT_CONFIG_LEVEL_XDG, value);
	}

	else if (strcmp(opt, "search_path_system") == 0) {
		set_search_path(GIT_CONFIG_LEVEL_SYSTEM, value);
	}

	else {
		rb_raise(rb_eArgError, "Unknown option specified");
	}

	return Qnil;
}

/*
 *  call-seq:
 *    Settings[option] -> value
 *
 *  Gets the value of a libgit2 library option.
 */
static VALUE rb_git_get_option(VALUE self, VALUE option)
{
	const char *opt;

	Check_Type(option, T_STRING);
	opt = StringValueCStr(option);

	if (strcmp(opt, "mwindow_size") == 0) {
		size_t val;
		git_libgit2_opts(GIT_OPT_GET_MWINDOW_SIZE, &val);
		return SIZET2NUM(val);
	}
	
	else if (strcmp(opt, "mwindow_mapped_limit") == 0) {
		size_t val;
		git_libgit2_opts(GIT_OPT_GET_MWINDOW_MAPPED_LIMIT, &val);
		return SIZET2NUM(val);
	}
	
	else if (strcmp(opt, "search_path_global") == 0) {
		return get_search_path(GIT_CONFIG_LEVEL_GLOBAL);
	}

	else if (strcmp(opt, "search_path_xdg") == 0) {
		return get_search_path(GIT_CONFIG_LEVEL_XDG);
	}

	else if (strcmp(opt, "search_path_system") == 0) {
		return get_search_path(GIT_CONFIG_LEVEL_SYSTEM);
	}

	else {
		rb_raise(rb_eArgError, "Unknown option specified");
	}
}

void Init_rugged_settings(void)
{
	VALUE rb_cRuggedSettings = rb_define_class_under(rb_mRugged, "Settings", rb_cObject);
	rb_define_module_function(rb_cRuggedSettings, "[]=", rb_git_set_option, 2);
	rb_define_module_function(rb_cRuggedSettings, "[]", rb_git_get_option, 1);
}
