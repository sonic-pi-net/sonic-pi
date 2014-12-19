#include "clar_libgit2.h"
#include "fileops.h"

typedef struct name_data {
	int count; /* return count */
	char *name; /* filename		*/
} name_data;

typedef struct walk_data {
	char *sub;		/* sub-directory name */
	name_data *names; /* name state data	*/
	git_buf path;
} walk_data;


static char *top_dir = "dir-walk";
static walk_data *state_loc;

static void setup(walk_data *d)
{
	name_data *n;

	cl_must_pass(p_mkdir(top_dir, 0777));

	cl_must_pass(p_chdir(top_dir));

	if (strcmp(d->sub, ".") != 0)
		cl_must_pass(p_mkdir(d->sub, 0777));

	cl_git_pass(git_buf_sets(&d->path, d->sub));

	state_loc = d;

	for (n = d->names; n->name; n++) {
		git_file fd = p_creat(n->name, 0666);
		cl_assert(fd >= 0);
		p_close(fd);
		n->count = 0;
	}
}

static void dirent_cleanup__cb(void *_d)
{
	walk_data *d = _d;
	name_data *n;

	for (n = d->names; n->name; n++) {
		cl_must_pass(p_unlink(n->name));
	}

	if (strcmp(d->sub, ".") != 0)
		cl_must_pass(p_rmdir(d->sub));

	cl_must_pass(p_chdir(".."));

	cl_must_pass(p_rmdir(top_dir));

	git_buf_free(&d->path);
}

static void check_counts(walk_data *d)
{
	name_data *n;

	for (n = d->names; n->name; n++) {
		cl_assert(n->count == 1);
	}
}

static int one_entry(void *state, git_buf *path)
{
	walk_data *d = (walk_data *) state;
	name_data *n;

	if (state != state_loc)
		return GIT_ERROR;

	if (path != &d->path)
		return GIT_ERROR;

	for (n = d->names; n->name; n++) {
		if (!strcmp(n->name, path->ptr)) {
			n->count++;
			return 0;
		}
	}

	return GIT_ERROR;
}


static name_data dot_names[] = {
	{ 0, "./a" },
	{ 0, "./asdf" },
	{ 0, "./pack-foo.pack" },
	{ 0, NULL }
};
static walk_data dot = {
	".",
	dot_names,
	GIT_BUF_INIT
};

/* make sure that the '.' folder is not traversed */
void test_core_dirent__dont_traverse_dot(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &dot);
	setup(&dot);

	cl_git_pass(git_path_direach(&dot.path, 0, one_entry, &dot));

	check_counts(&dot);
}


static name_data sub_names[] = {
	{ 0, "sub/a" },
	{ 0, "sub/asdf" },
	{ 0, "sub/pack-foo.pack" },
	{ 0, NULL }
};
static walk_data sub = {
	"sub",
	sub_names,
	GIT_BUF_INIT
};

/* traverse a subfolder */
void test_core_dirent__traverse_subfolder(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &sub);
	setup(&sub);

	cl_git_pass(git_path_direach(&sub.path, 0, one_entry, &sub));

	check_counts(&sub);
}


static walk_data sub_slash = {
	"sub/",
	sub_names,
	GIT_BUF_INIT
};

/* traverse a slash-terminated subfolder */
void test_core_dirent__traverse_slash_terminated_folder(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &sub_slash);
	setup(&sub_slash);

	cl_git_pass(git_path_direach(&sub_slash.path, 0, one_entry, &sub_slash));

	check_counts(&sub_slash);
}


static name_data empty_names[] = {
	{ 0, NULL }
};
static walk_data empty = {
	"empty",
	empty_names,
	GIT_BUF_INIT
};

/* make sure that empty folders are not traversed */
void test_core_dirent__dont_traverse_empty_folders(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &empty);
	setup(&empty);

	cl_git_pass(git_path_direach(&empty.path, 0, one_entry, &empty));

	check_counts(&empty);

	/* make sure callback not called */
	cl_assert(git_path_is_empty_dir(empty.path.ptr));
}

static name_data odd_names[] = {
	{ 0, "odd/.a" },
	{ 0, "odd/..c" },
	/* the following don't work on cygwin/win32 */
	/* { 0, "odd/.b." }, */
	/* { 0, "odd/..d.." }, */
	{ 0, NULL }
};
static walk_data odd = {
	"odd",
	odd_names,
	GIT_BUF_INIT
};

/* make sure that strange looking filenames ('..c') are traversed */
void test_core_dirent__traverse_weird_filenames(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &odd);
	setup(&odd);

	cl_git_pass(git_path_direach(&odd.path, 0, one_entry, &odd));

	check_counts(&odd);
}

/* test filename length limits */
void test_core_dirent__length_limits(void)
{
	char *big_filename = (char *)git__malloc(FILENAME_MAX + 1);
	memset(big_filename, 'a', FILENAME_MAX + 1);
	big_filename[FILENAME_MAX] = 0;

	cl_must_fail(p_creat(big_filename, 0666));

	git__free(big_filename);
}

void test_core_dirent__empty_dir(void)
{
	cl_must_pass(p_mkdir("empty_dir", 0777));
	cl_assert(git_path_is_empty_dir("empty_dir"));

	cl_git_mkfile("empty_dir/content", "whatever\n");
	cl_assert(!git_path_is_empty_dir("empty_dir"));
	cl_assert(!git_path_is_empty_dir("empty_dir/content"));

	cl_must_pass(p_unlink("empty_dir/content"));

	cl_must_pass(p_mkdir("empty_dir/content", 0777));
	cl_assert(!git_path_is_empty_dir("empty_dir"));
	cl_assert(git_path_is_empty_dir("empty_dir/content"));

	cl_must_pass(p_rmdir("empty_dir/content"));

	cl_must_pass(p_rmdir("empty_dir"));
}
