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

extern VALUE rb_mRugged;
extern VALUE rb_cRuggedObject;
extern VALUE rb_cRuggedRepo;
extern VALUE rb_cRuggedDiff;
extern VALUE rb_cRuggedIndex;
extern VALUE rb_cRuggedCommit;

VALUE rb_cRuggedTree;
VALUE rb_cRuggedTreeBuilder;

static VALUE rb_git_treeentry_fromC(const git_tree_entry *entry)
{
	VALUE rb_entry;
	VALUE type;

	if (!entry)
		return Qnil;

	rb_entry = rb_hash_new();

	rb_hash_aset(rb_entry, CSTR2SYM("name"), rb_str_new_utf8(git_tree_entry_name(entry)));
	rb_hash_aset(rb_entry, CSTR2SYM("oid"), rugged_create_oid(git_tree_entry_id(entry)));

	rb_hash_aset(rb_entry, CSTR2SYM("filemode"), INT2FIX(git_tree_entry_filemode(entry)));

	switch(git_tree_entry_type(entry)) {
		case GIT_OBJ_TREE:
			type = CSTR2SYM("tree");
			break;

		case GIT_OBJ_BLOB:
			type = CSTR2SYM("blob");
			break;

		case GIT_OBJ_COMMIT:
			type = CSTR2SYM("commit");
			break;

		default:
			type = Qnil;
			break;
	}
	rb_hash_aset(rb_entry, CSTR2SYM("type"), type);

	return rb_entry;
}

/*
 * Rugged Tree
 */

/*
 *  call-seq:
 *    tree.count -> count
 *    tree.length -> count
 *
 *  Return the number of entries contained in the tree.
 *
 *  Note that this only applies to entries in the root of the tree,
 *  not any other entries contained in sub-folders.
 */
static VALUE rb_git_tree_entrycount(VALUE self)
{
	git_tree *tree;
	Data_Get_Struct(self, git_tree, tree);

	return INT2FIX(git_tree_entrycount(tree));
}

/*
 *  call-seq:
 *    tree[e] -> entry
 *    tree.get_entry(e) -> entry
 *
 *  Return one of the entries from a tree as a +Hash+. If +e+ is a number, the +e+nth entry
 *  from the tree will be returned. If +e+ is a string, the entry with that name
 *  will be returned.
 *
 *  If the entry doesn't exist, +nil+ will be returned.
 *
 *    tree[3] #=> {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *    tree['bar.txt'] #=> {:name => "bar.txt", :type => :blob, :oid => "de5ba987198bcf2518885f0fc1350e5172cded78", :filemode => 0}
 *    tree['baz.txt'] #=> nil
 */
static VALUE rb_git_tree_get_entry(VALUE self, VALUE entry_id)
{
	git_tree *tree;
	Data_Get_Struct(self, git_tree, tree);

	if (TYPE(entry_id) == T_FIXNUM)
		return rb_git_treeentry_fromC(git_tree_entry_byindex(tree, FIX2INT(entry_id)));

	else if (TYPE(entry_id) == T_STRING)
		return rb_git_treeentry_fromC(git_tree_entry_byname(tree, StringValueCStr(entry_id)));

	else
		rb_raise(rb_eTypeError, "entry_id must be either an index or a filename");
}

/*
 *  call-seq:
 *    tree.get_entry_by_oid(rb_oid) -> entry
 *
 *  Return one of the entries from a tree as a +Hash+, based off the oid SHA.
 *
 *  If the entry doesn't exist, +nil+ will be returned.
 *
 *  This does a full traversal of the every element in the tree, so this method
 *  is not especially fast.
 *
 *    tree.get_entry_by_oid("d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f")
 *    #=> {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *
 */
static VALUE rb_git_tree_get_entry_by_oid(VALUE self, VALUE rb_oid)
{
	git_tree *tree;
	git_oid oid;
	Data_Get_Struct(self, git_tree, tree);

	Check_Type(rb_oid, T_STRING);
	rugged_exception_check(git_oid_fromstr(&oid, StringValueCStr(rb_oid)));

	return rb_git_treeentry_fromC(git_tree_entry_byid(tree, &oid));
}

/*
 *  call-seq:
 *    tree.each { |entry| block }
 *    tree.each -> enumerator
 *
 *  Call +block+ with each of the entries of the subtree as a +Hash+. If no +block+
 *  is given, an +enumerator+ is returned instead.
 *
 *  Note that only the entries in the root of the tree are yielded; if you need to
 *  list also entries in subfolders, use +tree.walk+ instead.
 *
 *    tree.each { |entry| puts entry.inspect }
 *
 *  generates:
 *
 *    {:name => "foo.txt", :type => :blob, :oid => "d8786bfc97485e8d7b19b21fb88c8ef1f199fc3f", :filemode => 0}
 *    {:name => "bar.txt", :type => :blob, :oid => "de5ba987198bcf2518885f0fc1350e5172cded78", :filemode => 0}
 *    ...
 */
static VALUE rb_git_tree_each(VALUE self)
{
	git_tree *tree;
	size_t i, count;
	Data_Get_Struct(self, git_tree, tree);

	if (!rb_block_given_p())
		return rb_funcall(self, rb_intern("to_enum"), 0);

	count = git_tree_entrycount(tree);

	for (i = 0; i < count; ++i) {
		const git_tree_entry *entry = git_tree_entry_byindex(tree, i);
		rb_yield(rb_git_treeentry_fromC(entry));
	}

	return Qnil;
}

static int rugged__treewalk_cb(const char *root, const git_tree_entry *entry, void *proc)
{
	rb_funcall((VALUE)proc, rb_intern("call"), 2,
		rb_str_new_utf8(root),
		rb_git_treeentry_fromC(entry)
	);

	return GIT_OK;
}

/*
 *  call-seq:
 *    tree.walk(mode) { |root, entry| block }
 *    tree.walk(mode) -> Iterator
 *
 *  Walk +tree+ with the given mode (either +:preorder+ or +:postorder+) and yield
 *  to +block+ every entry in +tree+ and all its subtrees, as a +Hash+. The +block+
 *  also takes a +root+, the relative path in the traversal, starting from the root
 *  of the original tree.
 *
 *  If no +block+ is given, an +Iterator+ is returned instead.
 *
 *    tree.walk(:postorder) { |root, entry| puts "#{root}#{entry[:name]} [#{entry[:oid]}]" }
 *
 *  generates:
 *
 *    USAGE.rb [02bae86c91f96b5fdb6b1cf06f5aa3612139e318]
 *    ext [23f135b3c576b6ac4785821888991d7089f35db1]
 *    ext/rugged [25c88faa9302e34e16664eb9c990deb2bcf77849]
 *    ext/rugged/extconf.rb [40c1aa8a8cec8ca444ed5758e3f00ecff093070a]
 *    ...
 */
static VALUE rb_git_tree_walk(VALUE self, VALUE rb_mode)
{
	git_tree *tree;
	int error, mode = 0;
	ID id_mode;

	Data_Get_Struct(self, git_tree, tree);

	if (!rb_block_given_p())
		return rb_funcall(self, rb_intern("to_enum"), 2, CSTR2SYM("walk"), rb_mode);

	Check_Type(rb_mode, T_SYMBOL);
	id_mode = SYM2ID(rb_mode);

	if (id_mode == rb_intern("preorder"))
		mode = GIT_TREEWALK_PRE;
	else if (id_mode == rb_intern("postorder"))
		mode = GIT_TREEWALK_POST;
	else
		rb_raise(rb_eTypeError,
				"Invalid iteration mode. Expected `:preorder` or `:postorder`");

	error = git_tree_walk(tree, mode, &rugged__treewalk_cb, (void *)rb_block_proc());
	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    tree.path(path) -> entry
 *
 *  Retrieve and return a tree entry by its relative path.
 */
static VALUE rb_git_tree_path(VALUE self, VALUE rb_path)
{
	int error;
	git_tree *tree;
	git_tree_entry *entry;
	VALUE rb_entry;
	Data_Get_Struct(self, git_tree, tree);
	Check_Type(rb_path, T_STRING);

	error = git_tree_entry_bypath(&entry, tree, StringValueCStr(rb_path));
	rugged_exception_check(error);

	rb_entry = rb_git_treeentry_fromC(entry);
	git_tree_entry_free(entry);

	return rb_entry;
}

/*
 *  call-seq:
 *    tree.diff(diffable[, options]) -> diff
 *
 *  Returns a diff between the tree and the diffable object that was given.
 *  +diffable+ can either be a +Rugged::Commit+, a +Rugged::Tree+, a +Rugged::Index+,
 *  or +nil+.
 *
 *  The +tree+ object will be used as the "old file" side of the diff, while the
 *  parent tree or the +diffable+ object will be used for the "new file" side.
 *
 *  If +diffable+ is nil, it will be treated as an empty tree.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :paths ::
 *    An array of paths / fnmatch patterns to constrain the diff to a specific
 *    set of files. Also see +:disable_pathspec_match+.
 *
 *  :max_size ::
 *    An integer specifying the maximum byte size of a file before a it will
 *    be treated as binary. The default value is 512MB.
 *
 *  :context_lines ::
 *    The number of unchanged lines that define the boundary of a hunk (and
 *    to display before and after the actual changes). The default is 3.
 *
 *  :interhunk_lines ::
 *    The maximum number of unchanged lines between hunk boundaries before the hunks
 *    will be merged into a one. The default is 0.
 *
 *  :old_prefix ::
 *    The virtual "directory" to prefix to old filenames in hunk headers.
 *    The default is "a".
 *
 *  :new_prefix ::
 *    The virtual "directory" to prefix to new filenames in hunk headers.
 *    The default is "b".
 *
 *  :reverse ::
 *    If true, the sides of the diff will be reversed.
 *
 *  :force_text ::
 *    If true, all files will be treated as text, disabling binary attributes & detection.
 *
 *  :ignore_whitespace ::
 *    If true, all whitespace will be ignored.
 *
 *  :ignore_whitespace_change ::
 *    If true, changes in amount of whitespace will be ignored.
 *
 *  :ignore_whitespace_eol ::
 *    If true, whitespace at end of line will be ignored.
 *
 *  :ignore_submodules ::
 *    if true, submodules will be excluded from the diff completely.
 *
 *  :patience ::
 *    If true, the "patience diff" algorithm will be used (currenlty unimplemented).
 *
 *  :include_ignored ::
 *    If true, ignored files will be included in the diff.
 *
 *  :include_untracked ::
 *   If true, untracked files will be included in the diff.
 *
 *  :include_unmodified ::
 *    If true, unmodified files will be included in the diff.
 *
 *  :recurse_untracked_dirs ::
 *    Even if +:include_untracked+ is true, untracked directories will only be
 *    marked with a single entry in the diff. If this flag is set to true,
 *    all files under ignored directories will be included in the diff, too.
 *
 *  :disable_pathspec_match ::
 *    If true, the given +:paths+ will be applied as exact matches, instead of
 *    as fnmatch patterns.
 *
 *  :deltas_are_icase ::
 *    If true, filename comparisons will be made with case-insensitivity.
 *
 *  :include_untracked_content ::
 *    if true, untracked content will be contained in the the diff patch text.
 *
 *  :skip_binary_check ::
 *    If true, diff deltas will be generated without spending time on binary
 *    detection. This is useful to improve performance in cases where the actual
 *    file content difference is not needed.
 *
 *  :include_typechange ::
 *    If true, type changes for files will not be interpreted as deletion of
 *    the "old file" and addition of the "new file", but will generate
 *    typechange records.
 *
 *  :include_typechange_trees ::
 *    Even if +:include_typechange+ is true, blob -> tree changes will still
 *    usually be handled as a deletion of the blob. If this flag is set to true,
 *    blob -> tree changes will be marked as typechanges.
 *
 *  :ignore_filemode ::
 *    If true, file mode changes will be ignored.
 *
 *  :recurse_ignored_dirs ::
 *    Even if +:include_ignored+ is true, ignored directories will only be
 *    marked with a single entry in the diff. If this flag is set to true,
 *    all files under ignored directories will be included in the diff, too.
 *
 *  Examples:
 *
 *    # Emulating `git diff <treeish>`
 *    tree = Rugged::Tree.lookup(repo, "d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
 *    diff = tree.diff(repo.index)
 *    diff.merge!(tree.diff)
 *
 *    # Tree-to-Tree Diff
 *    tree = Rugged::Tree.lookup(repo, "d70d245ed97ed2aa596dd1af6536e4bfdb047b69")
 *    other_tree = Rugged::Tree.lookup(repo, "7a9e0b02e63179929fed24f0a3e0f19168114d10")
 *    diff = tree.diff(other_tree)
 */
static VALUE rb_git_tree_diff(int argc, VALUE *argv, VALUE self)
{
	git_tree *tree;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo;
	git_diff *diff = NULL;
	VALUE owner, rb_other, rb_options;
	int error;

	rb_scan_args(argc, argv, "10:", &rb_other, &rb_options);
	rugged_parse_diff_options(&opts, rb_options);

	Data_Get_Struct(self, git_tree, tree);
	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	if (NIL_P(rb_other)) {
		error = git_diff_tree_to_tree(&diff, repo, tree, NULL, &opts);
	} else {
		if (TYPE(rb_other) == T_STRING) {
			rb_other = rugged_object_rev_parse(owner, rb_other, 1);
		}

		if (rb_obj_is_kind_of(rb_other, rb_cRuggedCommit)) {
			git_tree *other_tree;
			git_commit *commit;

			Data_Get_Struct(rb_other, git_commit, commit);
			error = git_commit_tree(&other_tree, commit);

			if (!error) {
				error = git_diff_tree_to_tree(&diff, repo, tree, other_tree, &opts);
				git_tree_free(other_tree);
			}
		} else if (rb_obj_is_kind_of(rb_other, rb_cRuggedTree)) {
			git_tree *other_tree;

			Data_Get_Struct(rb_other, git_tree, other_tree);
			error = git_diff_tree_to_tree(&diff, repo, tree, other_tree, &opts);
		} else if (rb_obj_is_kind_of(rb_other, rb_cRuggedIndex)) {
			git_index *index;
			Data_Get_Struct(rb_other, git_index, index);
			error = git_diff_tree_to_index(&diff, repo, tree, index, &opts);
		} else {
			xfree(opts.pathspec.strings);
			rb_raise(rb_eTypeError, "A Rugged::Commit, Rugged::Tree or Rugged::Index instance is required");
		}
	}

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, self, diff);
}

/*
 *  call-seq:
 *    tree.diff_workdir([options]) -> diff
 *
 *  Returns a diff between a tree and the current workdir.
 *
 *  The +tree+ object will be used as the "old file" side of the diff, while the
 *  content of the current workdir will be used for the "new file" side.
 *
 *  See Rugged::Tree#diff for a list of options that can be passed.
 */
static VALUE rb_git_tree_diff_workdir(int argc, VALUE *argv, VALUE self)
{
	git_tree *tree;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_repository *repo;
	git_diff *diff;
	VALUE owner, rb_options;
	int error;

	rb_scan_args(argc, argv, "00:", &rb_options);
	rugged_parse_diff_options(&opts, rb_options);

	Data_Get_Struct(self, git_tree, tree);
	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	error = git_diff_tree_to_workdir(&diff, repo, tree, &opts);

	xfree(opts.pathspec.strings);
	rugged_exception_check(error);

	return rugged_diff_new(rb_cRuggedDiff, self, diff);
}

void rugged_parse_merge_options(git_merge_options *opts, VALUE rb_options)
{
	if (!NIL_P(rb_options)) {
		VALUE rb_value;
		Check_Type(rb_options, T_HASH);

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("rename_threshold"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->rename_threshold = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("target_limit"));
		if (!NIL_P(rb_value)) {
			Check_Type(rb_value, T_FIXNUM);
			opts->target_limit = FIX2UINT(rb_value);
		}

		rb_value = rb_hash_aref(rb_options, CSTR2SYM("favor"));
		if (!NIL_P(rb_value)) {
			ID id_favor;

			Check_Type(rb_value, T_SYMBOL);
			id_favor = SYM2ID(rb_value);

			if (id_favor == rb_intern("normal")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_NORMAL;
			} else if (id_favor == rb_intern("ours")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_OURS;
			} else if (id_favor == rb_intern("theirs")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_THEIRS;
			} else if (id_favor == rb_intern("union")) {
				opts->file_favor = GIT_MERGE_FILE_FAVOR_UNION;
			} else {
				rb_raise(rb_eTypeError,
					"Invalid favor mode. Expected `:normal`, `:ours`, `:theirs` or `:union`");
			}
		}

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("renames")))) {
			opts->flags |= GIT_MERGE_TREE_FIND_RENAMES;
		}
	}
}

/*
 *  tree.merge(other_tree[, ancestor_tree[, options]]) -> Rugged::Index
 *  tree.merge(other_tree[, options]) -> Rugged::Index
 *
 *  Merges two trees and returns the a Rugged::Index object that reflects
 *  the result of the merge.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :renames ::
 *    If true, looking for renames will be enabled (`--find-renames`).
 *
 *  :rename_threshold ::
 *    An integer specifying the minimum similarity of a file to be
 *    seen as an eligible rename source (default 50).
 *
 *  :target_limit ::
 *    An integer specifying the maximum byte size of a file before a it will
 *    be treated as binary. The default value is 512MB.
 *
 *  :favor ::
 *    Specifies how and if conflicts are auto-resolved by favoring a specific
 *    file output. Can be one of `:normal`, `:ours`, `:theirs` or `:union`.
 *
 */
static VALUE rb_git_tree_merge(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_other_tree, rb_ancestor_tree, rb_options;
	VALUE rb_repo = rugged_owner(self);

	git_tree *tree, *other_tree, *ancestor_tree;
	git_repository *repo;
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	int error;

	if (rb_scan_args(argc, argv, "12", &rb_other_tree, &rb_ancestor_tree, &rb_options) == 2) {
		if (TYPE(rb_ancestor_tree) == T_HASH) {
			rb_options = rb_ancestor_tree;
			rb_ancestor_tree = Qnil;
		}
	}

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);
		rugged_parse_merge_options(&opts, rb_options);
	}

	if (!rb_obj_is_kind_of(rb_other_tree, rb_cRuggedTree))
		rb_raise(rb_eTypeError, "Expecting a Rugged::Tree instance");
	else if (!NIL_P(rb_ancestor_tree) && !rb_obj_is_kind_of(rb_ancestor_tree, rb_cRuggedTree))
		rb_raise(rb_eTypeError, "Expecting a Rugged::Tree instance");

	Data_Get_Struct(self, git_tree, tree);
	Data_Get_Struct(rb_repo, git_repository, repo);
	Data_Get_Struct(rb_other_tree, git_tree, other_tree);

	if (!NIL_P(rb_ancestor_tree))
		Data_Get_Struct(rb_ancestor_tree, git_tree, ancestor_tree);
	else
		ancestor_tree = NULL;

	error = git_merge_trees(&index, repo, ancestor_tree, tree, other_tree, &opts);
	rugged_exception_check(error);

	return rugged_index_new(rb_cRuggedIndex, rb_repo, index);
}

static void rb_git_treebuilder_free(git_treebuilder *bld)
{
	git_treebuilder_free(bld);
}

static VALUE rb_git_treebuilder_allocate(VALUE klass)
{
	return Data_Wrap_Struct(klass, NULL, &rb_git_treebuilder_free, NULL);
}

/*
 *  call-seq:
 *    TreeBuilder.new([tree])
 *
 *  Create a new Rugged::Trebuilder instance.
 *
 *  If an optional +tree+ is given, the returned TreeBuilder will be
 *  initialized with the entry of +tree+. Otherwise, the TreeBuilder
 *  will be empty and has to be filled manually.
 */
static VALUE rb_git_treebuilder_init(int argc, VALUE *argv, VALUE self)
{
	git_treebuilder *builder;
	git_tree *tree = NULL;
	VALUE rb_object;
	int error;

	if (rb_scan_args(argc, argv, "01", &rb_object) == 1) {
		if (!rb_obj_is_kind_of(rb_object, rb_cRuggedTree))
			rb_raise(rb_eTypeError, "A Rugged::Tree instance is required");

		Data_Get_Struct(rb_object, git_tree, tree);
	}

	error = git_treebuilder_create(&builder, tree);
	rugged_exception_check(error);

	DATA_PTR(self) = builder;
	return Qnil;
}

/*
 *  call-seq:
 *    builder.clear -> nil
 *
 *  Clear all entries in +builder+.
 */
static VALUE rb_git_treebuilder_clear(VALUE self)
{
	git_treebuilder *builder;
	Data_Get_Struct(self, git_treebuilder, builder);
	git_treebuilder_clear(builder);
	return Qnil;
}

/*
 *  call-seq:
 *    builder[path] -> entry
 *
 *  Return an entry from +builder+ based on its relative path.
 */
static VALUE rb_git_treebuilder_get(VALUE self, VALUE path)
{
	git_treebuilder *builder;
	Data_Get_Struct(self, git_treebuilder, builder);

	Check_Type(path, T_STRING);

	return rb_git_treeentry_fromC(git_treebuilder_get(builder, StringValueCStr(path)));
}

/*
 *  call-seq:
 *    builder << entry      -> nil
 *    builder.insert(entry) -> nil
 *
 *  Inser a new entry into +builder+.
 */
static VALUE rb_git_treebuilder_insert(VALUE self, VALUE rb_entry)
{
	git_treebuilder *builder;
	VALUE rb_path, rb_oid, rb_attr;
	git_oid oid;
	int error;

	Data_Get_Struct(self, git_treebuilder, builder);
	Check_Type(rb_entry, T_HASH);

	rb_path = rb_hash_aref(rb_entry, CSTR2SYM("name"));
	Check_Type(rb_path, T_STRING);

	rb_oid = rb_hash_aref(rb_entry, CSTR2SYM("oid"));
	Check_Type(rb_oid, T_STRING);
	rugged_exception_check(git_oid_fromstr(&oid, StringValueCStr(rb_oid)));

	rb_attr = rb_hash_aref(rb_entry, CSTR2SYM("filemode"));
	Check_Type(rb_attr, T_FIXNUM);

	error = git_treebuilder_insert(NULL,
		builder,
		StringValueCStr(rb_path),
		&oid,
		FIX2INT(rb_attr));

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    builder.remove(path) -> true or false
 *
 *  Remove an entry from +builder+ by its relative +path+.
 *
 *  Returns +true+ if the entry was successfully removed,
 *  or +false+ if the entry was not found.
 */
static VALUE rb_git_treebuilder_remove(VALUE self, VALUE path)
{
	git_treebuilder *builder;
	int error;

	Data_Get_Struct(self, git_treebuilder, builder);
	Check_Type(path, T_STRING);

	error = git_treebuilder_remove(builder, StringValueCStr(path));
	if (error == GIT_ENOTFOUND)
		return Qfalse;

	rugged_exception_check(error);
	return Qtrue;
}

/*
 *  call-seq:
 *    builder.write(repo) -> oid
 *
 *  Write +builder+'s content as a tree to the given +repo+
 *  and return the +oid+ for the newly created tree.
 */
static VALUE rb_git_treebuilder_write(VALUE self, VALUE rb_repo)
{
	git_treebuilder *builder;
	git_repository *repo;
	git_oid written_id;
	int error;

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Data_Get_Struct(self, git_treebuilder, builder);

	error = git_treebuilder_write(&written_id, repo, builder);
	rugged_exception_check(error);

	return rugged_create_oid(&written_id);
}

static int treebuilder_cb(const git_tree_entry *entry, void *opaque)
{
	VALUE proc = (VALUE)opaque;
	VALUE ret = rb_funcall(proc, rb_intern("call"), 1, rb_git_treeentry_fromC(entry));
	return rugged_parse_bool(ret);
}

/*
 *  call-seq:
 *    builder.reject! { |entry| block } -> nil
 *
 *  Deletes every tree +entry+ from +builder+ for which
 *  the given +block+ evaluates to true.
 */
static VALUE rb_git_treebuilder_filter(VALUE self)
{
	git_treebuilder *builder;

	rb_need_block();
	Data_Get_Struct(self, git_treebuilder, builder);

	git_treebuilder_filter(builder, &treebuilder_cb, (void *)rb_block_proc());
	return Qnil;
}

void Init_rugged_tree(void)
{
	/*
	 * Tree
	 */
	rb_cRuggedTree = rb_define_class_under(rb_mRugged, "Tree", rb_cRuggedObject);
	rb_define_method(rb_cRuggedTree, "count", rb_git_tree_entrycount, 0);
	rb_define_method(rb_cRuggedTree, "length", rb_git_tree_entrycount, 0);
	rb_define_method(rb_cRuggedTree, "get_entry", rb_git_tree_get_entry, 1);
	rb_define_method(rb_cRuggedTree, "get_entry_by_oid", rb_git_tree_get_entry_by_oid, 1);
	rb_define_method(rb_cRuggedTree, "path", rb_git_tree_path, 1);
	rb_define_method(rb_cRuggedTree, "diff", rb_git_tree_diff, -1);
	rb_define_method(rb_cRuggedTree, "diff_workdir", rb_git_tree_diff_workdir, -1);
	rb_define_method(rb_cRuggedTree, "[]", rb_git_tree_get_entry, 1);
	rb_define_method(rb_cRuggedTree, "each", rb_git_tree_each, 0);
	rb_define_method(rb_cRuggedTree, "walk", rb_git_tree_walk, 1);
	rb_define_method(rb_cRuggedTree, "merge", rb_git_tree_merge, -1);

	rb_cRuggedTreeBuilder = rb_define_class_under(rb_cRuggedTree, "Builder", rb_cObject);
	rb_define_alloc_func(rb_cRuggedTreeBuilder, rb_git_treebuilder_allocate);
	rb_define_method(rb_cRuggedTreeBuilder, "initialize", rb_git_treebuilder_init, -1);
	rb_define_method(rb_cRuggedTreeBuilder, "clear", rb_git_treebuilder_clear, 0);
	rb_define_method(rb_cRuggedTreeBuilder, "[]", rb_git_treebuilder_get, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "insert", rb_git_treebuilder_insert, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "<<", rb_git_treebuilder_insert, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "remove", rb_git_treebuilder_remove, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "write", rb_git_treebuilder_write, 1);
	rb_define_method(rb_cRuggedTreeBuilder, "reject!", rb_git_treebuilder_filter, 0);
}
