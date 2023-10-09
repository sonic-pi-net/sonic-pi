#include "common.h"

/*
 * This could be run in the main loop whilst the application waits for
 * the indexing to finish in a worker thread
 */
static int index_cb(const git_indexer_progress *stats, void *data)
{
	(void)data;
	printf("\rProcessing %u of %u", stats->indexed_objects, stats->total_objects);

	return 0;
}

int lg2_index_pack(git_repository *repo, int argc, char **argv)
{
	git_indexer *idx;
	git_indexer_progress stats = {0, 0};
	int error;
	int fd;
	ssize_t read_bytes;
	char buf[512];

	(void)repo;

	if (argc < 2) {
		fprintf(stderr, "usage: %s index-pack <packfile>\n", argv[-1]);
		return EXIT_FAILURE;
	}

#ifdef GIT_EXPERIMENTAL_SHA256
	error = git_indexer_new(&idx, ".", git_repository_oid_type(repo), NULL);
#else
	error = git_indexer_new(&idx, ".", 0, NULL, NULL);
#endif

	if (error < 0) {
		puts("bad idx");
		return -1;
	}


	if ((fd = open(argv[1], 0)) < 0) {
		perror("open");
		return -1;
	}

	do {
		read_bytes = read(fd, buf, sizeof(buf));
		if (read_bytes < 0)
			break;

		if ((error = git_indexer_append(idx, buf, read_bytes, &stats)) < 0)
			goto cleanup;

		index_cb(&stats, NULL);
	} while (read_bytes > 0);

	if (read_bytes < 0) {
		error = -1;
		perror("failed reading");
		goto cleanup;
	}

	if ((error = git_indexer_commit(idx, &stats)) < 0)
		goto cleanup;

	printf("\rIndexing %u of %u\n", stats.indexed_objects, stats.total_objects);

	puts(git_indexer_name(idx));

 cleanup:
	close(fd);
	git_indexer_free(idx);
	return error;
}
