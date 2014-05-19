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
extern VALUE rb_cRuggedSignature;
VALUE rb_cRuggedCommit;

/*
 *  call-seq:
 *    commit.message -> msg
 *
 *  Return the message of this commit. This includes the full body of the
 *  message, with the short description, detailed descritpion, and any
 *  optional footers or signatures after it.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.message #=> "add a lot of RDoc docs\n\nthis includes docs for commit and blob"
 */
static VALUE rb_git_commit_message_GET(VALUE self)
{
	git_commit *commit;
	rb_encoding *encoding = rb_utf8_encoding();
	const char *encoding_name;
	const char *message;

	Data_Get_Struct(self, git_commit, commit);

	message = git_commit_message(commit);
	encoding_name = git_commit_message_encoding(commit);
	if (encoding_name != NULL)
		encoding = rb_enc_find(encoding_name);

	return rb_enc_str_new(message, strlen(message), encoding);
}

/*
 *  call-seq:
 *    commit.committer -> signature
 *
 *  Return the signature for the committer of this +commit+. The signature
 *  is returned as a +Hash+ containing +:name+, +:email+ of the author
 *  and +:time+ of the change.
 *
 *  The committer of a commit is the person who actually applied the changes
 *  of the commit; in most cases it's the same as the author.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.committer #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_commit_committer_GET(VALUE self)
{
	git_commit *commit;
	Data_Get_Struct(self, git_commit, commit);

	return rugged_signature_new(
		git_commit_committer(commit),
		git_commit_message_encoding(commit));
}

/*
 *  call-seq:
 *    commit.author -> signature
 *
 *  Return the signature for the author of this +commit+. The signature
 *  is returned as a +Hash+ containing +:name+, +:email+ of the author
 *  and +:time+ of the change.
 *
 *  The author of the commit is the person who intially created the changes.
 *
 *  In Ruby 1.9+, the returned string will be encoded with the encoding
 *  specified in the +Encoding+ header of the commit, if available.
 *
 *    commit.author #=> {:email=>"tanoku@gmail.com", :time=>Tue Jan 24 05:42:45 UTC 2012, :name=>"Vicent Mart\303\255"}
 */
static VALUE rb_git_commit_author_GET(VALUE self)
{
	git_commit *commit;
	Data_Get_Struct(self, git_commit, commit);

	return rugged_signature_new(
		git_commit_author(commit),
		git_commit_message_encoding(commit));
}

/*
 *  call-seq:
 *    commit.epoch_time -> int
 *
 *  Return the time when this commit was made effective. This is the same value
 *  as the +:time+ attribute for +commit.committer+, but represented as an +Integer+
 *  value in seconds since the Epoch.
 *
 *    commit.time #=> 1327383765
 */
static VALUE rb_git_commit_epoch_time_GET(VALUE self)
{
	git_commit *commit;
	Data_Get_Struct(self, git_commit, commit);

	return ULONG2NUM(git_commit_time(commit));
}

/*
 *  call-seq:
 *    commit.tree -> tree
 *
 *  Return the tree pointed at by this +commit+. The tree is
 *  returned as a +Rugged::Tree+ object.
 *
 *    commit.tree #=> #<Rugged::Tree:0x10882c680>
 */
static VALUE rb_git_commit_tree_GET(VALUE self)
{
	git_commit *commit;
	git_tree *tree;
	VALUE owner;
	int error;

	Data_Get_Struct(self, git_commit, commit);
	owner = rugged_owner(self);

	error = git_commit_tree(&tree, commit);
	rugged_exception_check(error);

	return rugged_object_new(owner, (git_object *)tree);
}

/*
 *  call-seq:
 *    commit.tree_id -> oid
 *
 *  Return the tree oid pointed at by this +commit+. The tree is
 *  returned as a String object.
 *
 *    commit.tree_id #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_tree_id_GET(VALUE self)
{
	git_commit *commit;
	const git_oid *tree_id;

	Data_Get_Struct(self, git_commit, commit);

	tree_id = git_commit_tree_id(commit);

	return rugged_create_oid(tree_id);
}

/*
 *  call-seq:
 *    commit.parents -> [commit, ...]
 *
 *  Return the parent(s) of this commit as an array of +Rugged::Commit+
 *  objects. An array is always returned even when the commit has only
 *  one or zero parents.
 *
 *    commit.parents #=> => [#<Rugged::Commit:0x108828918>]
 *    root.parents #=> []
 */
static VALUE rb_git_commit_parents_GET(VALUE self)
{
	git_commit *commit;
	git_commit *parent;
	unsigned int n, parent_count;
	VALUE ret_arr, owner;
	int error;

	Data_Get_Struct(self, git_commit, commit);
	owner = rugged_owner(self);

	parent_count = git_commit_parentcount(commit);
	ret_arr = rb_ary_new2((long)parent_count);

	for (n = 0; n < parent_count; n++) {
		error = git_commit_parent(&parent, commit, n);
		rugged_exception_check(error);
		rb_ary_push(ret_arr, rugged_object_new(owner, (git_object *)parent));
	}

	return ret_arr;
}

/*
 *  call-seq:
 *    commit.parent_ids -> [oid, ...]
 *
 *  Return the parent oid(s) of this commit as an array of oid String
 *  objects. An array is always returned even when the commit has only
 *  one or zero parents.
 *
 *    commit.parent_ids #=> => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1", ...]
 *    root.parent_ids #=> []
 */
static VALUE rb_git_commit_parent_ids_GET(VALUE self)
{
	git_commit *commit;
	const git_oid *parent_id;
	unsigned int n, parent_count;
	VALUE ret_arr;

	Data_Get_Struct(self, git_commit, commit);

	parent_count = git_commit_parentcount(commit);
	ret_arr = rb_ary_new2((long)parent_count);

	for (n = 0; n < parent_count; n++) {
		parent_id = git_commit_parent_id(commit, n);
		if (parent_id) {
			rb_ary_push(ret_arr, rugged_create_oid(parent_id));
		}
	}

	return ret_arr;
}

/*
 *  call-seq:
 *    commit.amend(data = {}) -> oid
 *
 *  Amend a commit object, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:message+: a string with the full text for the commit's message
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:tree+: the tree for this amended commit, represented as a <tt>Rugged::Tree</tt>
 *    instance or an OID +String+.
 *  - +:update_ref+ (optional): a +String+ with the name of a reference in the
 *  repository which should be updated to point to this amended commit (e.g. "HEAD")
 *
 *  When the amended commit is successfully written to disk, its +oid+ will be
 *  returned as a hex +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    commit.amend(
 *      :author => author,
 *      :message => "Updated Hello world\n\n",
 *      :committer => author,
 *      :tree => some_tree) #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_amend(VALUE self, VALUE rb_data)
{
	VALUE rb_message, rb_tree, rb_ref, owner;
	int error = 0;
	git_commit *commit_to_amend;
	char *message = NULL;
	git_tree *tree = NULL;
	git_signature *author = NULL, *committer = NULL;
	git_oid commit_oid;
	git_repository *repo;
	const char *update_ref = NULL;

	Check_Type(rb_data, T_HASH);

	Data_Get_Struct(self, git_commit, commit_to_amend);

	owner = rugged_owner(self);
	Data_Get_Struct(owner, git_repository, repo);

	rb_ref = rb_hash_aref(rb_data, CSTR2SYM("update_ref"));
	if (!NIL_P(rb_ref)) {
		Check_Type(rb_ref, T_STRING);
		update_ref = StringValueCStr(rb_ref);
	}

	rb_message = rb_hash_aref(rb_data, CSTR2SYM("message"));
	if (!NIL_P(rb_message)) {
		Check_Type(rb_message, T_STRING);
		message = StringValueCStr(rb_message);
	}

	rb_tree = rb_hash_aref(rb_data, CSTR2SYM("tree"));
	if (!NIL_P(rb_tree))
		tree = (git_tree *)rugged_object_get(repo, rb_tree, GIT_OBJ_TREE);

	if (!NIL_P(rb_hash_aref(rb_data, CSTR2SYM("committer")))) {
		committer = rugged_signature_get(
			rb_hash_aref(rb_data, CSTR2SYM("committer")), repo
		);
	}

	if (!NIL_P(rb_hash_aref(rb_data, CSTR2SYM("author")))) {
		author = rugged_signature_get(
			rb_hash_aref(rb_data, CSTR2SYM("author")), repo
		);
	}

	error = git_commit_amend(
		&commit_oid,
		commit_to_amend,
		update_ref,
		author,
		committer,
		NULL,
		message,
		tree);

	git_signature_free(author);
	git_signature_free(committer);

	git_object_free((git_object *)tree);

	rugged_exception_check(error);

	return rugged_create_oid(&commit_oid);
}

/*
 *  call-seq:
 *    Commit.create(repository, data = {}) -> oid
 *
 *  Write a new +Commit+ object to +repository+, with the given +data+
 *  arguments, passed as a +Hash+:
 *
 *  - +:message+: a string with the full text for the commit's message
 *  - +:committer+ (optional): a hash with the signature for the committer,
 *    defaults to the signature from the configuration
 *  - +:author+ (optional): a hash with the signature for the author,
 *    defaults to the signature from the configuration
 *  - +:parents+: an +Array+ with zero or more parents for this commit,
 *    represented as <tt>Rugged::Commit</tt> instances, or OID +String+.
 *  - +:tree+: the tree for this commit, represented as a <tt>Rugged::Tree</tt>
 *    instance or an OID +String+.
 *  - +:update_ref+ (optional): a +String+ with the name of a reference in the
 *  repository which should be updated to point to this commit (e.g. "HEAD")
 *
 *  When the commit is successfully written to disk, its +oid+ will be
 *  returned as a hex +String+.
 *
 *    author = {:email=>"tanoku@gmail.com", :time=>Time.now, :name=>"Vicent Mart\303\255"}
 *
 *    Rugged::Commit.create(r,
 *      :author => author,
 *      :message => "Hello world\n\n",
 *      :committer => author,
 *      :parents => ["2cb831a8aea28b2c1b9c63385585b864e4d3bad1"],
 *      :tree => some_tree) #=> "f148106ca58764adc93ad4e2d6b1d168422b9796"
 */
static VALUE rb_git_commit_create(VALUE self, VALUE rb_repo, VALUE rb_data)
{
	VALUE rb_message, rb_tree, rb_parents, rb_ref;
	VALUE rb_err_obj = Qnil;
	int parent_count, i, error = 0;
	const git_commit **parents = NULL;
	git_commit **free_list = NULL;
	git_tree *tree;
	git_signature *author, *committer;
	git_oid commit_oid;
	git_repository *repo;
	const char *update_ref = NULL;

	Check_Type(rb_data, T_HASH);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	rb_ref = rb_hash_aref(rb_data, CSTR2SYM("update_ref"));
	if (!NIL_P(rb_ref)) {
		Check_Type(rb_ref, T_STRING);
		update_ref = StringValueCStr(rb_ref);
	}

	rb_message = rb_hash_aref(rb_data, CSTR2SYM("message"));
	Check_Type(rb_message, T_STRING);

	committer = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("committer")), repo
	);

	author = rugged_signature_get(
		rb_hash_aref(rb_data, CSTR2SYM("author")), repo
	);

	rb_parents = rb_hash_aref(rb_data, CSTR2SYM("parents"));
	Check_Type(rb_parents, T_ARRAY);

	rb_tree = rb_hash_aref(rb_data, CSTR2SYM("tree"));
	tree = (git_tree *)rugged_object_get(repo, rb_tree, GIT_OBJ_TREE);

	parents = alloca(RARRAY_LEN(rb_parents) * sizeof(void *));
	free_list = alloca(RARRAY_LEN(rb_parents) * sizeof(void *));
	parent_count = 0;

	for (i = 0; i < (int)RARRAY_LEN(rb_parents); ++i) {
		VALUE p = rb_ary_entry(rb_parents, i);
		git_commit *parent = NULL;
		git_commit *free_ptr = NULL;

		if (NIL_P(p))
			continue;

		if (TYPE(p) == T_STRING) {
			git_oid oid;

			error = git_oid_fromstr(&oid, StringValueCStr(p));
			if (error < GIT_OK)
				goto cleanup;

			error = git_commit_lookup(&parent, repo, &oid);
			if (error < GIT_OK)
				goto cleanup;

			free_ptr = parent;

		} else if (rb_obj_is_kind_of(p, rb_cRuggedCommit)) {
			Data_Get_Struct(p, git_commit, parent);
		} else {
			rb_err_obj = rb_exc_new2(rb_eTypeError, "Invalid type for parent object");
			goto cleanup;
		}

		parents[parent_count] = parent;
		free_list[parent_count] = free_ptr;
		parent_count++;
	}

	error = git_commit_create(
		&commit_oid,
		repo,
		update_ref,
		author,
		committer,
		NULL,
		StringValueCStr(rb_message),
		tree,
		parent_count,
		parents);

cleanup:
	git_signature_free(author);
	git_signature_free(committer);

	git_object_free((git_object *)tree);

	for (i = 0; i < parent_count; ++i)
		git_object_free((git_object *)free_list[i]);

	if (!NIL_P(rb_err_obj))
		rb_exc_raise(rb_err_obj);

	rugged_exception_check(error);

	return rugged_create_oid(&commit_oid);
}

/*
 *  call-seq:
 *    commit.to_mbox(options = {}) -> str
 *
 *  Returns +commit+'s contents formatted to resemble UNIX mailbox format.
 *
 *  Does not (yet) support merge commits.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :patch_no ::
 *    Number for this patch in the series. Defaults to +1+.
 *
 *  :total_patches ::
 *    Total number of patches in the series. Defaults to +1+.
 *
 *  :exclude_subject_patch_marker ::
 *    If set to true, no "[PATCH]" marker will be
 *    added to the beginning of the subject line.
 *
 *  Additionally, you can also pass the same options as for Rugged::Tree#diff.
 */
static VALUE rb_git_commit_to_mbox(int argc, VALUE *argv, VALUE self)
{
	git_buf email_patch = { NULL };
	git_repository *repo;
	git_commit *commit;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff_format_email_flags_t flags = GIT_DIFF_FORMAT_EMAIL_NONE;

	VALUE rb_repo = rugged_owner(self), rb_email_patch = Qnil, rb_val, rb_options;

	int error;
	size_t patch_no = 1, total_patches = 1;

	rb_scan_args(argc, argv, ":", &rb_options);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	Data_Get_Struct(self, git_commit, commit);

	if (!NIL_P(rb_options)) {
		Check_Type(rb_options, T_HASH);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("patch_no"));
		if (!NIL_P(rb_val))
			patch_no = NUM2INT(rb_val);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("total_patches"));
		if (!NIL_P(rb_val))
			total_patches = NUM2INT(rb_val);

		if (RTEST(rb_hash_aref(rb_options, CSTR2SYM("exclude_subject_patch_marker"))))
			flags |= GIT_DIFF_FORMAT_EMAIL_EXCLUDE_SUBJECT_PATCH_MARKER;

		rugged_parse_diff_options(&opts, rb_options);
	}

	error = git_diff_commit_as_email(
		&email_patch,
		repo,
		commit,
		patch_no,
		total_patches,
		flags,
		&opts);

	if (error) goto cleanup;

	rb_email_patch = rb_enc_str_new(email_patch.ptr, email_patch.size, rb_utf8_encoding());

	cleanup:

	xfree(opts.pathspec.strings);
	git_buf_free(&email_patch);
	rugged_exception_check(error);

	return rb_email_patch;
}


void Init_rugged_commit(void)
{
	rb_cRuggedCommit = rb_define_class_under(rb_mRugged, "Commit", rb_cRuggedObject);

	rb_define_singleton_method(rb_cRuggedCommit, "create", rb_git_commit_create, 2);

	rb_define_method(rb_cRuggedCommit, "message", rb_git_commit_message_GET, 0);
	rb_define_method(rb_cRuggedCommit, "epoch_time", rb_git_commit_epoch_time_GET, 0);
	rb_define_method(rb_cRuggedCommit, "committer", rb_git_commit_committer_GET, 0);
	rb_define_method(rb_cRuggedCommit, "author", rb_git_commit_author_GET, 0);
	rb_define_method(rb_cRuggedCommit, "tree", rb_git_commit_tree_GET, 0);

	rb_define_method(rb_cRuggedCommit, "tree_id", rb_git_commit_tree_id_GET, 0);
	rb_define_method(rb_cRuggedCommit, "tree_oid", rb_git_commit_tree_id_GET, 0);

	rb_define_method(rb_cRuggedCommit, "parents", rb_git_commit_parents_GET, 0);
	rb_define_method(rb_cRuggedCommit, "parent_ids", rb_git_commit_parent_ids_GET, 0);
	rb_define_method(rb_cRuggedCommit, "parent_oids", rb_git_commit_parent_ids_GET, 0);

	rb_define_method(rb_cRuggedCommit, "amend", rb_git_commit_amend, 1);

	rb_define_method(rb_cRuggedCommit, "to_mbox", rb_git_commit_to_mbox, -1);
}
