#include "clar_libgit2.h"
#include "buffer.h"
#include "message.h"

static void assert_message_prettifying(char *expected_output, char *input, int strip_comments)
{
	git_buf prettified_message = GIT_BUF_INIT;

	git_message_prettify(&prettified_message, input, strip_comments, '#');
	cl_assert_equal_s(expected_output, git_buf_cstr(&prettified_message));

	git_buf_free(&prettified_message);
}

#define t40 "A quick brown fox jumps over the lazy do"
#define s40 "                                        "
#define sss s40 s40 s40 s40 s40 s40 s40 s40 s40 s40 // # 400
#define ttt t40 t40 t40 t40 t40 t40 t40 t40 t40 t40 // # 400

/* Ported from git.git */
/* see https://github.com/git/git/blob/master/t/t0030-stripspace.sh */
void test_object_message__long_lines_without_spaces_should_be_unchanged(void)
{
	assert_message_prettifying(ttt "\n", ttt, 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt, 0);
	assert_message_prettifying(ttt ttt ttt "\n", ttt ttt ttt, 0);
	assert_message_prettifying(ttt ttt ttt ttt "\n", ttt ttt ttt ttt, 0);
}

void test_object_message__lines_with_spaces_at_the_beginning_should_be_unchanged(void)
{
	assert_message_prettifying(sss ttt "\n", sss ttt, 0);
	assert_message_prettifying(sss sss ttt "\n", sss sss ttt, 0);
	assert_message_prettifying(sss sss sss ttt "\n", sss sss sss ttt, 0);
}

void test_object_message__lines_with_intermediate_spaces_should_be_unchanged(void)
{
	assert_message_prettifying(ttt sss ttt "\n", ttt sss ttt, 0);
	assert_message_prettifying(ttt sss sss ttt "\n", ttt sss sss ttt, 0);
}

void test_object_message__consecutive_blank_lines_should_be_unified(void)
{
	assert_message_prettifying(ttt "\n\n" ttt "\n", ttt "\n\n\n\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt "\n\n" ttt "\n", ttt ttt "\n\n\n\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt ttt "\n\n" ttt "\n", ttt ttt ttt "\n\n\n\n\n" ttt "\n", 0);

	assert_message_prettifying(ttt "\n\n" ttt ttt "\n", ttt "\n\n\n\n\n" ttt ttt "\n", 0);
	assert_message_prettifying(ttt "\n\n" ttt ttt ttt "\n", ttt "\n\n\n\n\n" ttt ttt ttt "\n", 0);

	assert_message_prettifying(ttt "\n\n" ttt "\n", ttt "\n\t\n \n\n  \t\t\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt "\n\n" ttt "\n", ttt ttt "\n\t\n \n\n  \t\t\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt ttt "\n\n" ttt "\n", ttt ttt ttt "\n\t\n \n\n  \t\t\n" ttt "\n", 0);

	assert_message_prettifying(ttt "\n\n" ttt ttt "\n", ttt "\n\t\n \n\n  \t\t\n" ttt ttt "\n", 0);
	assert_message_prettifying(ttt "\n\n" ttt ttt ttt "\n", ttt "\n\t\n \n\n  \t\t\n" ttt ttt ttt "\n", 0);
}

void test_object_message__only_consecutive_blank_lines_should_be_completely_removed(void)
{
	assert_message_prettifying("", "\n", 0);
	assert_message_prettifying("", "\n\n\n", 0);
	assert_message_prettifying("", sss "\n" sss "\n" sss "\n", 0);
	assert_message_prettifying("", sss sss "\n" sss "\n\n", 0);
}

void test_object_message__consecutive_blank_lines_at_the_beginning_should_be_removed(void)
{
	assert_message_prettifying(ttt "\n", "\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", "\n\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt "\n", "\n\n\n" ttt ttt "\n", 0);
	assert_message_prettifying(ttt ttt ttt "\n", "\n\n\n" ttt ttt ttt "\n", 0);
	assert_message_prettifying(ttt ttt ttt ttt "\n", "\n\n\n" ttt ttt ttt ttt "\n", 0);
	assert_message_prettifying(ttt "\n", sss "\n" sss "\n" sss "\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", "\n" sss "\n" sss sss "\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", sss sss "\n" sss "\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", sss sss sss "\n\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", "\n" sss sss sss "\n\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n", "\n\n" sss sss sss "\n" ttt "\n", 0);
}

void test_object_message__consecutive_blank_lines_at_the_end_should_be_removed(void)
{
	assert_message_prettifying(ttt "\n", ttt "\n\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n\n\n\n", 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt "\n\n\n\n", 0);
	assert_message_prettifying(ttt ttt ttt "\n", ttt ttt ttt "\n\n\n\n", 0);
	assert_message_prettifying(ttt ttt ttt ttt "\n", ttt ttt ttt ttt "\n\n\n\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n" sss "\n" sss "\n" sss "\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n\n" sss "\n" sss sss "\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n" sss sss "\n" sss "\n\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n" sss sss sss "\n\n\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n\n" sss sss sss "\n\n", 0);
	assert_message_prettifying(ttt "\n", ttt "\n\n\n" sss sss sss "\n\n", 0);
}

void test_object_message__text_without_newline_at_end_should_end_with_newline(void)
{
	assert_message_prettifying(ttt "\n", ttt, 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt, 0);
	assert_message_prettifying(ttt ttt ttt "\n", ttt ttt ttt, 0);
	assert_message_prettifying(ttt ttt ttt ttt "\n", ttt ttt ttt ttt, 0);
}

void test_object_message__text_plus_spaces_without_newline_should_not_show_spaces_and_end_with_newline(void)
{
	assert_message_prettifying(ttt "\n", ttt sss, 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt sss, 0);
	assert_message_prettifying(ttt ttt ttt "\n", ttt ttt ttt sss, 0);
	assert_message_prettifying(ttt "\n", ttt sss sss, 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt sss sss, 0);
	assert_message_prettifying(ttt "\n", ttt sss sss sss, 0);
}

void test_object_message__text_plus_spaces_ending_with_newline_should_be_cleaned_and_newline_must_remain(void){
	assert_message_prettifying(ttt "\n", ttt sss "\n", 0);
	assert_message_prettifying(ttt "\n", ttt sss sss "\n", 0);
	assert_message_prettifying(ttt "\n", ttt sss sss sss "\n", 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt sss "\n", 0);
	assert_message_prettifying(ttt ttt "\n", ttt ttt sss sss "\n", 0);
	assert_message_prettifying(ttt ttt ttt "\n", ttt ttt ttt sss "\n", 0);
}

void test_object_message__spaces_with_newline_at_end_should_be_replaced_with_empty_string(void)
{
	assert_message_prettifying("", sss "\n", 0);
	assert_message_prettifying("", sss sss "\n", 0);
	assert_message_prettifying("", sss sss sss "\n", 0);
	assert_message_prettifying("", sss sss sss sss "\n", 0);
}

void test_object_message__spaces_without_newline_at_end_should_be_replaced_with_empty_string(void)
{
	assert_message_prettifying("", "", 0);
	assert_message_prettifying("", sss sss, 0);
	assert_message_prettifying("", sss sss sss, 0);
	assert_message_prettifying("", sss sss sss sss, 0);
}

void test_object_message__consecutive_text_lines_should_be_unchanged(void)
{
	assert_message_prettifying(ttt ttt "\n" ttt "\n", ttt ttt "\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n" ttt ttt "\n" ttt "\n", ttt "\n" ttt ttt "\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n" ttt "\n" ttt "\n" ttt ttt "\n", ttt "\n" ttt "\n" ttt "\n" ttt ttt "\n", 0);
	assert_message_prettifying(ttt "\n" ttt "\n\n" ttt ttt "\n" ttt "\n", ttt "\n" ttt "\n\n" ttt ttt "\n" ttt "\n", 0);
	assert_message_prettifying(ttt ttt "\n\n" ttt "\n" ttt ttt "\n", ttt ttt "\n\n" ttt "\n" ttt ttt "\n", 0);
	assert_message_prettifying(ttt "\n" ttt ttt "\n\n" ttt "\n", ttt "\n" ttt ttt "\n\n" ttt "\n", 0);
}

void test_object_message__strip_comments(void)
{
	assert_message_prettifying("", "# comment", 1);
	assert_message_prettifying("", "# comment\n", 1);
	assert_message_prettifying("", "# comment    \n", 1);

	assert_message_prettifying(ttt "\n", ttt "\n" "# comment\n", 1);
	assert_message_prettifying(ttt "\n", "# comment\n" ttt "\n", 1);
	assert_message_prettifying(ttt "\n" ttt "\n", ttt "\n" "# comment\n" ttt "\n", 1);
}

void test_object_message__keep_comments(void)
{
	assert_message_prettifying("# comment\n", "# comment", 0);
	assert_message_prettifying("# comment\n", "# comment\n", 0);
	assert_message_prettifying("# comment\n", "# comment    \n", 0);

	assert_message_prettifying(ttt "\n" "# comment\n", ttt "\n" "# comment\n", 0);
	assert_message_prettifying("# comment\n" ttt "\n", "# comment\n" ttt "\n", 0);
	assert_message_prettifying(ttt "\n" "# comment\n" ttt "\n", ttt "\n" "# comment\n" ttt "\n", 0);
}

void test_object_message__message_prettify(void)
{
	git_buf buffer;

	memset(&buffer, 0, sizeof(buffer));
	cl_git_pass(git_message_prettify(&buffer, "", 0, '#'));
	cl_assert_equal_s(buffer.ptr, "");
	git_buf_free(&buffer);
	cl_git_pass(git_message_prettify(&buffer, "", 1, '#'));
	cl_assert_equal_s(buffer.ptr, "");
	git_buf_free(&buffer);

	cl_git_pass(git_message_prettify(&buffer, "Short", 0, '#'));
	cl_assert_equal_s("Short\n", buffer.ptr);
	git_buf_free(&buffer);
	cl_git_pass(git_message_prettify(&buffer, "Short", 1, '#'));
	cl_assert_equal_s("Short\n", buffer.ptr);
	git_buf_free(&buffer);

	cl_git_pass(git_message_prettify(&buffer, "This is longer\nAnd multiline\n# with some comments still in\n", 0, '#'));
	cl_assert_equal_s(buffer.ptr, "This is longer\nAnd multiline\n# with some comments still in\n");
	git_buf_free(&buffer);

	cl_git_pass(git_message_prettify(&buffer, "This is longer\nAnd multiline\n# with some comments still in\n", 1, '#'));
	cl_assert_equal_s(buffer.ptr, "This is longer\nAnd multiline\n");
	git_buf_free(&buffer);
}
