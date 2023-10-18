#include "clar_libgit2.h"
#include "futils.h"

typedef struct name_data {
	int count; /* return count */
	char *name; /* filename		*/
} name_data;

typedef struct walk_data {
	char *sub;		/* sub-directory name */
	name_data *names; /* name state data	*/
	git_str path;
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

	cl_git_pass(git_str_sets(&d->path, d->sub));

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

	git_str_dispose(&d->path);
}

static void check_counts(walk_data *d)
{
	name_data *n;

	for (n = d->names; n->name; n++) {
		cl_assert(n->count == 1);
	}
}

static int update_count(name_data *data, const char *name)
{
	name_data *n;

	for (n = data; n->name; n++) {
		if (!strcmp(n->name, name)) {
			n->count++;
			return 0;
		}
	}

	return GIT_ERROR;
}

static int one_entry(void *state, git_str *path)
{
	walk_data *d = (walk_data *) state;

	if (state != state_loc)
		return GIT_ERROR;

	if (path != &d->path)
		return GIT_ERROR;

	return update_count(d->names, path->ptr);
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
	GIT_STR_INIT
};

/* make sure that the '.' folder is not traversed */
void test_dirent__dont_traverse_dot(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &dot);
	setup(&dot);

	cl_git_pass(git_fs_path_direach(&dot.path, 0, one_entry, &dot));

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
	GIT_STR_INIT
};

/* traverse a subfolder */
void test_dirent__traverse_subfolder(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &sub);
	setup(&sub);

	cl_git_pass(git_fs_path_direach(&sub.path, 0, one_entry, &sub));

	check_counts(&sub);
}


static walk_data sub_slash = {
	"sub/",
	sub_names,
	GIT_STR_INIT
};

/* traverse a slash-terminated subfolder */
void test_dirent__traverse_slash_terminated_folder(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &sub_slash);
	setup(&sub_slash);

	cl_git_pass(git_fs_path_direach(&sub_slash.path, 0, one_entry, &sub_slash));

	check_counts(&sub_slash);
}


static name_data empty_names[] = {
	{ 0, NULL }
};
static walk_data empty = {
	"empty",
	empty_names,
	GIT_STR_INIT
};

/* make sure that empty folders are not traversed */
void test_dirent__dont_traverse_empty_folders(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &empty);
	setup(&empty);

	cl_git_pass(git_fs_path_direach(&empty.path, 0, one_entry, &empty));

	check_counts(&empty);

	/* make sure callback not called */
	cl_assert(git_fs_path_is_empty_dir(empty.path.ptr));
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
	GIT_STR_INIT
};

/* make sure that strange looking filenames ('..c') are traversed */
void test_dirent__traverse_weird_filenames(void)
{
	cl_set_cleanup(&dirent_cleanup__cb, &odd);
	setup(&odd);

	cl_git_pass(git_fs_path_direach(&odd.path, 0, one_entry, &odd));

	check_counts(&odd);
}

/* test filename length limits */
void test_dirent__length_limits(void)
{
	char *big_filename = (char *)git__malloc(FILENAME_MAX + 1);
	memset(big_filename, 'a', FILENAME_MAX + 1);
	big_filename[FILENAME_MAX] = 0;

	cl_must_fail(p_creat(big_filename, 0666));

	git__free(big_filename);
}

void test_dirent__empty_dir(void)
{
	cl_must_pass(p_mkdir("empty_dir", 0777));
	cl_assert(git_fs_path_is_empty_dir("empty_dir"));

	cl_git_mkfile("empty_dir/content", "whatever\n");
	cl_assert(!git_fs_path_is_empty_dir("empty_dir"));
	cl_assert(!git_fs_path_is_empty_dir("empty_dir/content"));

	cl_must_pass(p_unlink("empty_dir/content"));

	cl_must_pass(p_mkdir("empty_dir/content", 0777));
	cl_assert(!git_fs_path_is_empty_dir("empty_dir"));
	cl_assert(git_fs_path_is_empty_dir("empty_dir/content"));

	cl_must_pass(p_rmdir("empty_dir/content"));

	cl_must_pass(p_rmdir("empty_dir"));
}

static void handle_next(git_fs_path_diriter *diriter, walk_data *walk)
{
	const char *fullpath, *filename;
	size_t fullpath_len, filename_len;

	cl_git_pass(git_fs_path_diriter_fullpath(&fullpath, &fullpath_len, diriter));
	cl_git_pass(git_fs_path_diriter_filename(&filename, &filename_len, diriter));

	cl_assert_equal_strn(fullpath, "sub/", 4);
	cl_assert_equal_s(fullpath+4, filename);

	update_count(walk->names, fullpath);
}

/* test directory iterator */
void test_dirent__diriter_with_fullname(void)
{
	git_fs_path_diriter diriter = GIT_FS_PATH_DIRITER_INIT;
	int error;

	cl_set_cleanup(&dirent_cleanup__cb, &sub);
	setup(&sub);

	cl_git_pass(git_fs_path_diriter_init(&diriter, sub.path.ptr, 0));

	while ((error = git_fs_path_diriter_next(&diriter)) == 0)
		handle_next(&diriter, &sub);

	cl_assert_equal_i(error, GIT_ITEROVER);

	git_fs_path_diriter_free(&diriter);

	check_counts(&sub);
}

void test_dirent__diriter_at_directory_root(void)
{
	git_fs_path_diriter diriter = GIT_FS_PATH_DIRITER_INIT;
	const char *sandbox_path, *path;
	char *root_path;
	size_t path_len;
	int root_offset, error;

	sandbox_path = clar_sandbox_path();
	cl_assert((root_offset = git_fs_path_root(sandbox_path)) >= 0);

	cl_assert(root_path = git__calloc(1, root_offset + 2));
	strncpy(root_path, sandbox_path, root_offset + 1);

	cl_git_pass(git_fs_path_diriter_init(&diriter, root_path, 0));

	while ((error = git_fs_path_diriter_next(&diriter)) == 0) {
		cl_git_pass(git_fs_path_diriter_fullpath(&path, &path_len, &diriter));

		cl_assert(path_len > (size_t)(root_offset + 1));
		cl_assert(path[root_offset+1] != '/');
	}

	cl_assert_equal_i(error, GIT_ITEROVER);

	git_fs_path_diriter_free(&diriter);
	git__free(root_path);
}
