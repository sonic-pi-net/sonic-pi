#include "clar_libgit2.h"
#include "buffer.h"
#include "zstream.h"

static const char *data = "This is a test test test of This is a test";

#define INFLATE_EXTRA 2

static void assert_zlib_equal_(
	const void *expected, size_t e_len,
	const void *compressed, size_t c_len,
	const char *msg, const char *file, int line)
{
	z_stream stream;
	char *expanded = git__calloc(1, e_len + INFLATE_EXTRA);
	cl_assert(expanded);

	memset(&stream, 0, sizeof(stream));
	stream.next_out  = (Bytef *)expanded;
	stream.avail_out = (uInt)(e_len + INFLATE_EXTRA);
	stream.next_in   = (Bytef *)compressed;
	stream.avail_in  = (uInt)c_len;

	cl_assert(inflateInit(&stream) == Z_OK);
	cl_assert(inflate(&stream, Z_FINISH));
	inflateEnd(&stream);

	clar__assert_equal(
		file, line, msg, 1,
		"%d", (int)stream.total_out, (int)e_len);
	clar__assert_equal(
		file, line, "Buffer len was not exact match", 1,
		"%d", (int)stream.avail_out, (int)INFLATE_EXTRA);

	clar__assert(
		memcmp(expanded, expected, e_len) == 0,
		file, line, "uncompressed data did not match", NULL, 1);

	git__free(expanded);
}

#define assert_zlib_equal(E,EL,C,CL) \
	assert_zlib_equal_(E, EL, C, CL, #EL " != " #CL, __FILE__, (int)__LINE__)

void test_core_zstream__basic(void)
{
	git_zstream z = GIT_ZSTREAM_INIT;
	char out[128];
	size_t outlen = sizeof(out);

	cl_git_pass(git_zstream_init(&z, GIT_ZSTREAM_DEFLATE));
	cl_git_pass(git_zstream_set_input(&z, data, strlen(data) + 1));
	cl_git_pass(git_zstream_get_output(out, &outlen, &z));
	cl_assert(git_zstream_done(&z));
	cl_assert(outlen > 0);
	git_zstream_free(&z);

	assert_zlib_equal(data, strlen(data) + 1, out, outlen);
}

void test_core_zstream__fails_on_trailing_garbage(void)
{
	git_buf deflated = GIT_BUF_INIT, inflated = GIT_BUF_INIT;
	size_t i = 0;

	/* compress a simple string */
	git_zstream_deflatebuf(&deflated, "foobar!!", 8);

	/* append some garbage */
	for (i = 0; i < 10; i++) {
		git_buf_putc(&deflated, i);
	}

	cl_git_fail(git_zstream_inflatebuf(&inflated, deflated.ptr, deflated.size));

	git_buf_free(&deflated);
	git_buf_free(&inflated);
}

void test_core_zstream__buffer(void)
{
	git_buf out = GIT_BUF_INIT;
	cl_git_pass(git_zstream_deflatebuf(&out, data, strlen(data) + 1));
	assert_zlib_equal(data, strlen(data) + 1, out.ptr, out.size);
	git_buf_free(&out);
}

#define BIG_STRING_PART "Big Data IS Big - Long Data IS Long - We need a buffer larger than 1024 x 1024 to make sure we trigger chunked compression - Big Big Data IS Bigger than Big - Long Long Data IS Longer than Long"

static void compress_and_decompress_input_various_ways(git_buf *input)
{
	git_buf out1 = GIT_BUF_INIT, out2 = GIT_BUF_INIT;
	git_buf inflated = GIT_BUF_INIT;
	size_t i, fixed_size = max(input->size / 2, 256);
	char *fixed = git__malloc(fixed_size);
	cl_assert(fixed);

	/* compress with deflatebuf */

	cl_git_pass(git_zstream_deflatebuf(&out1, input->ptr, input->size));
	assert_zlib_equal(input->ptr, input->size, out1.ptr, out1.size);

	/* compress with various fixed size buffer (accumulating the output) */

	for (i = 0; i < 3; ++i) {
		git_zstream zs = GIT_ZSTREAM_INIT;
		size_t use_fixed_size;

		switch (i) {
		case 0: use_fixed_size = 256; break;
		case 1: use_fixed_size = fixed_size / 2; break;
		case 2: use_fixed_size = fixed_size; break;
		}
		cl_assert(use_fixed_size <= fixed_size);

		cl_git_pass(git_zstream_init(&zs, GIT_ZSTREAM_DEFLATE));
		cl_git_pass(git_zstream_set_input(&zs, input->ptr, input->size));

		while (!git_zstream_done(&zs)) {
			size_t written = use_fixed_size;
			cl_git_pass(git_zstream_get_output(fixed, &written, &zs));
			cl_git_pass(git_buf_put(&out2, fixed, written));
		}

		git_zstream_free(&zs);
		assert_zlib_equal(input->ptr, input->size, out2.ptr, out2.size);

		/* did both approaches give the same data? */
		cl_assert_equal_sz(out1.size, out2.size);
		cl_assert(!memcmp(out1.ptr, out2.ptr, out1.size));

		git_buf_free(&out2);
	}

	cl_git_pass(git_zstream_inflatebuf(&inflated, out1.ptr, out1.size));
	cl_assert_equal_i(input->size, inflated.size);
	cl_assert(memcmp(input->ptr, inflated.ptr, inflated.size) == 0);

	git_buf_free(&out1);
	git_buf_free(&inflated);
	git__free(fixed);
}

void test_core_zstream__big_data(void)
{
	git_buf in = GIT_BUF_INIT;
	size_t scan, target;

	for (target = 1024; target <= 1024 * 1024 * 4; target *= 8) {

		/* make a big string that's easy to compress */
		git_buf_clear(&in);
		while (in.size < target)
			cl_git_pass(
				git_buf_put(&in, BIG_STRING_PART, strlen(BIG_STRING_PART)));

		compress_and_decompress_input_various_ways(&in);

		/* make a big string that's hard to compress */
		srand(0xabad1dea);
		for (scan = 0; scan < in.size; ++scan)
			in.ptr[scan] = (char)rand();

		compress_and_decompress_input_various_ways(&in);
	}

	git_buf_free(&in);
}
