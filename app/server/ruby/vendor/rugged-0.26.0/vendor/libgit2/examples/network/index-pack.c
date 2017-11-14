#include <git2.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef _WIN32
# include <io.h>
# include <Windows.h>

# define open _open
# define read _read
# define close _close

#define ssize_t unsigned int
#else
# include <unistd.h>
#endif
#include "common.h"

// This could be run in the main loop whilst the application waits for
// the indexing to finish in a worker thread
static int index_cb(const git_transfer_progress *stats, void *data)
{
	(void)data;
	printf("\rProcessing %d of %d", stats->indexed_objects, stats->total_objects);

	return 0;
}

int index_pack(git_repository *repo, int argc, char **argv)
{
	git_indexer *idx;
	git_transfer_progress stats = {0, 0};
	int error;
	char hash[GIT_OID_HEXSZ + 1] = {0};
	int fd;
	ssize_t read_bytes;
	char buf[512];

	(void)repo;

	if (argc < 2) {
		fprintf(stderr, "usage: %s index-pack <packfile>\n", argv[-1]);
		return EXIT_FAILURE;
	}

	if (git_indexer_new(&idx, ".", 0, NULL, NULL, NULL) < 0) {
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

	printf("\rIndexing %d of %d\n", stats.indexed_objects, stats.total_objects);

	git_oid_fmt(hash, git_indexer_hash(idx));
	puts(hash);

 cleanup:
	close(fd);
	git_indexer_free(idx);
	return error;
}
