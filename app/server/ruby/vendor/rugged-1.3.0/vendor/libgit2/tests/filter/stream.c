#include "clar_libgit2.h"
#include "posix.h"
#include "blob.h"
#include "filter.h"
#include "git2/sys/filter.h"
#include "git2/sys/repository.h"

static git_repository *g_repo = NULL;

static git_filter *create_compress_filter(void);
static git_filter *compress_filter;

void test_filter_stream__initialize(void)
{
	compress_filter = create_compress_filter();

	cl_git_pass(git_filter_register("compress", compress_filter, 50));
	g_repo = cl_git_sandbox_init("empty_standard_repo");
}

void test_filter_stream__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;

	git_filter_unregister("compress");
	git__free(compress_filter);
}

#define CHUNKSIZE 10240

struct compress_stream {
	git_writestream parent;
	git_writestream *next;
	git_filter_mode_t mode;
	char current;
	size_t current_chunk;
};

static int compress_stream_write__deflated(struct compress_stream *stream, const char *buffer, size_t len)
{
	size_t idx = 0;

	while (len > 0) {
		size_t chunkremain, chunksize;

		if (stream->current_chunk == 0)
			stream->current = buffer[idx];

		chunkremain = CHUNKSIZE - stream->current_chunk;
		chunksize = min(chunkremain, len);

		stream->current_chunk += chunksize;
		len -= chunksize;
		idx += chunksize;

		if (stream->current_chunk == CHUNKSIZE) {
			cl_git_pass(stream->next->write(stream->next, &stream->current, 1));
			stream->current_chunk = 0;
		}
	}

	return 0;
}

static int compress_stream_write__inflated(struct compress_stream *stream, const char *buffer, size_t len)
{
	char inflated[CHUNKSIZE];
	size_t i, j;

	for (i = 0; i < len; i++) {
		for (j = 0; j < CHUNKSIZE; j++)
			inflated[j] = buffer[i];

		cl_git_pass(stream->next->write(stream->next, inflated, CHUNKSIZE));
	}

	return 0;
}

static int compress_stream_write(git_writestream *s, const char *buffer, size_t len)
{
	struct compress_stream *stream = (struct compress_stream *)s;

	return (stream->mode == GIT_FILTER_TO_ODB) ?
		compress_stream_write__deflated(stream, buffer, len) :
		compress_stream_write__inflated(stream, buffer, len);
}

static int compress_stream_close(git_writestream *s)
{
	struct compress_stream *stream = (struct compress_stream *)s;
	cl_assert_equal_i(0, stream->current_chunk);
	stream->next->close(stream->next);
	return 0;
}

static void compress_stream_free(git_writestream *stream)
{
	git__free(stream);
}

static int compress_filter_stream_init(
	git_writestream **out,
	git_filter *self,
	void **payload,
	const git_filter_source *src,
	git_writestream *next)
{
	struct compress_stream *stream = git__calloc(1, sizeof(struct compress_stream));
	cl_assert(stream);

	GIT_UNUSED(self);
	GIT_UNUSED(payload);

	stream->parent.write = compress_stream_write;
	stream->parent.close = compress_stream_close;
	stream->parent.free = compress_stream_free;
	stream->next = next;
	stream->mode = git_filter_source_mode(src);

	*out = (git_writestream *)stream;
	return 0;
}

git_filter *create_compress_filter(void)
{
	git_filter *filter = git__calloc(1, sizeof(git_filter));
	cl_assert(filter);

	filter->version = GIT_FILTER_VERSION;
	filter->attributes = "+compress";
	filter->stream = compress_filter_stream_init;

	return filter;
}

static void writefile(const char *filename, size_t numchunks)
{
	git_buf path = GIT_BUF_INIT;
	char buf[CHUNKSIZE];
	size_t i = 0, j = 0;
	int fd;

	cl_git_pass(git_buf_joinpath(&path, "empty_standard_repo", filename));

	fd = p_open(path.ptr, O_RDWR|O_CREAT, 0666);
	cl_assert(fd >= 0);

	for (i = 0; i < numchunks; i++) {
		for (j = 0; j < CHUNKSIZE; j++) {
			buf[j] = i % 256;
		}

		cl_git_pass(p_write(fd, buf, CHUNKSIZE));
	}
	p_close(fd);

	git_buf_dispose(&path);
}

static void test_stream(size_t numchunks)
{
	git_index *index;
	const git_index_entry *entry;
	git_blob *blob;
	struct stat st;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_mkfile(
		"empty_standard_repo/.gitattributes",
		"* compress\n");

	/* write a file to disk */
	writefile("streamed_file", numchunks);

	/* place it in the index */
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "streamed_file"));
	cl_git_pass(git_index_write(index));

	/* ensure it was appropriately compressed */
	cl_assert(entry = git_index_get_bypath(index, "streamed_file", 0));

	cl_git_pass(git_blob_lookup(&blob, g_repo, &entry->id));
	cl_assert_equal_i(numchunks, git_blob_rawsize(blob));

	/* check the file back out */
	cl_must_pass(p_unlink("empty_standard_repo/streamed_file"));
	cl_git_pass(git_checkout_index(g_repo, index, &checkout_opts));

	/* ensure it was decompressed */
	cl_must_pass(p_stat("empty_standard_repo/streamed_file", &st));
	cl_assert_equal_sz((numchunks * CHUNKSIZE), st.st_size);

	git_index_free(index);
	git_blob_free(blob);
}

/* write a 50KB file through the "compression" stream */
void test_filter_stream__smallfile(void)
{
	test_stream(5);
}

/* optionally write a 500 MB file through the compression stream */
void test_filter_stream__bigfile(void)
{
	if (!cl_is_env_set("GITTEST_INVASIVE_FS_SIZE"))
		cl_skip();

	test_stream(51200);
}
