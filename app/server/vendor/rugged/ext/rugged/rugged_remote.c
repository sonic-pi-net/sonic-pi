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
extern VALUE rb_cRuggedRepo;
extern VALUE rb_eRuggedError;
VALUE rb_cRuggedRemote;

#define RUGGED_REMOTE_CALLBACKS_INIT {1, progress_cb, NULL, credentials_cb, transfer_progress_cb, update_tips_cb, NULL}

static int progress_cb(const char *str, int len, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(2);

	if (NIL_P(payload->progress))
		return 0;

	rb_ary_push(args, payload->progress);
	rb_ary_push(args, rb_str_new(str, len));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

static int transfer_progress_cb(const git_transfer_progress *stats, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(5);

	if (NIL_P(payload->transfer_progress))
		return 0;

	rb_ary_push(args, payload->transfer_progress);
	rb_ary_push(args, UINT2NUM(stats->total_objects));
	rb_ary_push(args, UINT2NUM(stats->indexed_objects));
	rb_ary_push(args, UINT2NUM(stats->received_objects));
	rb_ary_push(args, UINT2NUM(stats->local_objects));
	rb_ary_push(args, UINT2NUM(stats->total_deltas));
	rb_ary_push(args, UINT2NUM(stats->indexed_deltas));
	rb_ary_push(args, INT2FIX(stats->received_bytes));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

static int update_tips_cb(const char *refname, const git_oid *src, const git_oid *dest, void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	VALUE args = rb_ary_new2(4);

	if (NIL_P(payload->update_tips))
		return 0;

	rb_ary_push(args, payload->update_tips);
	rb_ary_push(args, rb_str_new_utf8(refname));
	rb_ary_push(args, git_oid_iszero(src) ? Qnil : rugged_create_oid(src));
	rb_ary_push(args, git_oid_iszero(dest) ? Qnil : rugged_create_oid(dest));

	rb_protect(rugged__block_yield_splat, args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

struct extract_cred_args
{
	VALUE rb_callback;
	git_cred **cred;
	const char *url;
	const char *username_from_url;
	unsigned int allowed_types;
};

static VALUE allowed_types_to_rb_ary(int allowed_types) {
	VALUE rb_allowed_types = rb_ary_new();

	if (allowed_types & GIT_CREDTYPE_USERPASS_PLAINTEXT)
		rb_ary_push(rb_allowed_types, CSTR2SYM("plaintext"));

	if (allowed_types & GIT_CREDTYPE_SSH_KEY)
		rb_ary_push(rb_allowed_types, CSTR2SYM("ssh_key"));

	if (allowed_types & GIT_CREDTYPE_DEFAULT)
		rb_ary_push(rb_allowed_types, CSTR2SYM("default"));

	return rb_allowed_types;
}

static VALUE extract_cred(VALUE data) {
	struct extract_cred_args *args = (struct extract_cred_args*)data;
	VALUE rb_url, rb_username_from_url, rb_cred;

	rb_url = args->url ? rb_str_new2(args->url) : Qnil;
	rb_username_from_url = args->username_from_url ? rb_str_new2(args->username_from_url) : Qnil;

	rb_cred = rb_funcall(args->rb_callback, rb_intern("call"), 3,
		rb_url, rb_username_from_url, allowed_types_to_rb_ary(args->allowed_types));

	rugged_cred_extract(args->cred, args->allowed_types, rb_cred);

	return Qnil;
}

static int credentials_cb(
	git_cred **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *data)
{
	struct rugged_remote_cb_payload *payload = data;
	struct extract_cred_args args = {
		payload->credentials, cred, url, username_from_url, allowed_types
	};

	if (NIL_P(payload->credentials))
		return GIT_PASSTHROUGH;

	rb_protect(extract_cred, (VALUE)&args, &payload->exception);

	return payload->exception ? GIT_ERROR : GIT_OK;
}

#define CALLABLE_OR_RAISE(ret, rb_options, name) \
	do {							\
		ret = rb_hash_aref(rb_options, CSTR2SYM(name)); \
								\
		if (!NIL_P(ret) && !rb_respond_to(ret, rb_intern("call"))) \
			rb_raise(rb_eArgError, "Expected a Proc or an object that responds to #call (:" name " )."); \
	} while (0);

void rugged_remote_init_callbacks_and_payload_from_options(
	VALUE rb_options,
	git_remote_callbacks *callbacks,
	struct rugged_remote_cb_payload *payload)
{
	git_remote_callbacks prefilled = RUGGED_REMOTE_CALLBACKS_INIT;

	prefilled.payload = payload;
	memcpy(callbacks, &prefilled, sizeof(git_remote_callbacks));

	CALLABLE_OR_RAISE(payload->update_tips, rb_options, "update_tips");
	CALLABLE_OR_RAISE(payload->progress, rb_options, "progress");
	CALLABLE_OR_RAISE(payload->transfer_progress, rb_options, "transfer_progress");
	CALLABLE_OR_RAISE(payload->credentials, rb_options, "credentials");
}

static void rb_git_remote__free(git_remote *remote)
{
	git_remote_free(remote);
}

VALUE rugged_remote_new(VALUE klass, VALUE owner, git_remote *remote)
{
	VALUE rb_remote;

	rb_remote = Data_Wrap_Struct(klass, NULL, &rb_git_remote__free, remote);
	rugged_set_owner(rb_remote, owner);
	return rb_remote;
}

static inline void rugged_validate_remote_url(VALUE rb_url)
{
	Check_Type(rb_url, T_STRING);
	if (!git_remote_valid_url(StringValueCStr(rb_url)))
		rb_raise(rb_eArgError, "Invalid URL format");
}

/*
 *  call-seq:
 *    Remote.new(repository, url) -> remote
 *
 *  Return a new remote with +url+ in +repository+ , the remote is not persisted:
 *  - +url+: a valid remote url
 *
 *  Returns a new Rugged::Remote object
 *
 *    Rugged::Remote.new(@repo, 'git://github.com/libgit2/libgit2.git') #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_new(VALUE klass, VALUE rb_repo, VALUE rb_url)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	rugged_check_repo(rb_repo);
	rugged_validate_remote_url(rb_url);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_create_anonymous(
			&remote,
			repo,
			StringValueCStr(rb_url),
			NULL);

	rugged_exception_check(error);

	return rugged_remote_new(klass, rb_repo, remote);
}

/*
 *  call-seq:
 *     Remote.add(repository, name, url) -> remote
 *
 *  Add a new remote with +name+ and +url+ to +repository+
 *  - +url+: a valid remote url
 *  - +name+: a valid remote name
 *
 *  Returns a new Rugged::Remote object
 *
 *    Rugged::Remote.add(@repo, 'origin', 'git://github.com/libgit2/rugged.git') #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_add(VALUE klass, VALUE rb_repo,VALUE rb_name, VALUE rb_url)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	Check_Type(rb_name, T_STRING);
	rugged_validate_remote_url(rb_url);
	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_create(
			&remote,
			repo,
			StringValueCStr(rb_name),
			StringValueCStr(rb_url));

	rugged_exception_check(error);

	return rugged_remote_new(klass, rb_repo, remote);
}

/*
 *  call-seq:
 *    Remote.lookup(repository, name) -> remote or nil
 *
 *  Return an existing remote with +name+ in +repository+:
 *  - +name+: a valid remote name
 *
 *  Returns a new Rugged::Remote object or +nil+ if the
 *  remote doesn't exist
 *
 *    Rugged::Remote.lookup(@repo, 'origin') #=> #<Rugged::Remote:0x00000001fbfa80>
 */
static VALUE rb_git_remote_lookup(VALUE klass, VALUE rb_repo, VALUE rb_name)
{
	git_remote *remote;
	git_repository *repo;
	int error;

	Check_Type(rb_name, T_STRING);
	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_load(&remote, repo, StringValueCStr(rb_name));

	if (error == GIT_ENOTFOUND)
		return Qnil;

	rugged_exception_check(error);

	return rugged_remote_new(klass, rb_repo, remote);
}

static VALUE rugged_rhead_new(const git_remote_head *head)
{
	VALUE rb_head = rb_hash_new();

	rb_hash_aset(rb_head, CSTR2SYM("local?"), head->local ? Qtrue : Qfalse);
	rb_hash_aset(rb_head, CSTR2SYM("oid"), rugged_create_oid(&head->oid));
	rb_hash_aset(rb_head, CSTR2SYM("loid"),
			git_oid_iszero(&head->loid) ? Qnil : rugged_create_oid(&head->loid));
	rb_hash_aset(rb_head, CSTR2SYM("name"), rb_str_new_utf8(head->name));

	return rb_head;
}

/*
 *  call-seq:
 *    remote.ls(options = {}) -> an_enumerator
 *    remote.ls(options = {}) { |remote_head_hash| block }
 *
 *  List references available in a connected +remote+ repository along
 *  with the associated commit IDs.
 *
 *  Call the given block once for each remote head in the +remote+ as a
 *  +Hash+.
 *  If no block is given an Enumerator is returned.
 *
 *    r.ls.to_a #=> [{:local?=>false, :oid=>"b3ee97a91b02e91c35394950bda6ea622044baad", :loid=> nil, :name=>"refs/heads/development"}]
 *
 *  remote head hash includes:
 *  [:oid] oid of the remote head
 *  [:name] name of the remote head
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the ls operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 */
static VALUE rb_git_remote_ls(int argc, VALUE *argv, VALUE self)
{
	git_remote *remote;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	const git_remote_head **heads;

	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, 0 };

	VALUE rb_options;

	int error;
	size_t heads_len, i;

	Data_Get_Struct(self, git_remote, remote);

	rb_scan_args(argc, argv, ":", &rb_options);

	if (!rb_block_given_p())
		return rb_funcall(self, rb_intern("to_enum"), 2, CSTR2SYM("ls"), rb_options);

	if (!NIL_P(rb_options))
		rugged_remote_init_callbacks_and_payload_from_options(rb_options, &callbacks, &payload);

	if ((error = git_remote_set_callbacks(remote, &callbacks)) ||
	    (error = git_remote_connect(remote, GIT_DIRECTION_FETCH)) ||
	    (error = git_remote_ls(&heads, &heads_len, remote)))
		goto cleanup;

	for (i = 0; i < heads_len && !payload.exception; i++)
		rb_protect(rb_yield, rugged_rhead_new(heads[i]), &payload.exception);

	cleanup:

	git_remote_disconnect(remote);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    remote.name() -> string
 *
 *	Returns the remote's name
 *	  remote.name #=> "origin"
 */
static VALUE rb_git_remote_name(VALUE self)
{
	git_remote *remote;
	const char * name;
	Data_Get_Struct(self, git_remote, remote);

	name = git_remote_name(remote);

	return name ? rb_str_new_utf8(name) : Qnil;
}

/*
 *  call-seq:
 *    remote.url() -> string
 *
 *  Returns the remote's url
 *    remote.url #=> "git://github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_url(VALUE self)
{
	git_remote *remote;
	Data_Get_Struct(self, git_remote, remote);

	return rb_str_new_utf8(git_remote_url(remote));
}

/*
 *  call-seq:
 *    remote.url = url -> url
 *
 *  Sets the remote's url without persisting it in the config.
 *  Existing connections will not be updated.
 *    remote.url = 'git://github.com/libgit2/rugged.git' #=> "git://github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_set_url(VALUE self, VALUE rb_url)
{
	git_remote *remote;

	rugged_validate_remote_url(rb_url);
	Data_Get_Struct(self, git_remote, remote);

	rugged_exception_check(
		git_remote_set_url(remote, StringValueCStr(rb_url))
	);
	return rb_url;
}

/*
 *  call-seq:
 *    remote.push_url() -> string or nil
 *
 *  Returns the remote's url for pushing or nil if no special url for
 *  pushing is set.
 *    remote.push_url #=> "git://github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_push_url(VALUE self)
{
	git_remote *remote;
	const char * push_url;

	Data_Get_Struct(self, git_remote, remote);

	push_url = git_remote_pushurl(remote);
	return push_url ? rb_str_new_utf8(push_url) : Qnil;
}

/*
 *  call-seq:
 *    remote.push_url = url -> url
 *
 *  Sets the remote's url for pushing without persisting it in the config.
 *  Existing connections will not be updated.
 *    remote.push_url = 'git@github.com/libgit2/rugged.git' #=> "git@github.com/libgit2/rugged.git"
 */
static VALUE rb_git_remote_set_push_url(VALUE self, VALUE rb_url)
{
	git_remote *remote;

	rugged_validate_remote_url(rb_url);
	Data_Get_Struct(self, git_remote, remote);

	rugged_exception_check(
		git_remote_set_pushurl(remote, StringValueCStr(rb_url))
	);

	return rb_url;
}

static VALUE rb_git_remote_refspecs(VALUE self, git_direction direction)
{
	git_remote *remote;
	int error = 0;
	git_strarray refspecs;
	VALUE rb_refspec_array;

	Data_Get_Struct(self, git_remote, remote);

	if (direction == GIT_DIRECTION_FETCH)
		error = git_remote_get_fetch_refspecs(&refspecs, remote);
	else
		error = git_remote_get_push_refspecs(&refspecs, remote);

	rugged_exception_check(error);

	rb_refspec_array = rugged_strarray_to_rb_ary(&refspecs);
	git_strarray_free(&refspecs);
	return rb_refspec_array;
}

/*
 *  call-seq:
 *  remote.fetch_refspecs -> array
 *
 *  Get the remote's list of fetch refspecs as +array+
 */
static VALUE rb_git_remote_fetch_refspecs(VALUE self)
{
	return rb_git_remote_refspecs(self, GIT_DIRECTION_FETCH);
}

/*
 *  call-seq:
 *  remote.push_refspecs -> array
 *
 *  Get the remote's list of push refspecs as +array+
 */
static VALUE rb_git_remote_push_refspecs(VALUE self)
{
	return rb_git_remote_refspecs(self, GIT_DIRECTION_PUSH);
}

static VALUE rb_git_remote_add_refspec(VALUE self, VALUE rb_refspec, git_direction direction)
{
	git_remote *remote;
	int error = 0;

	Data_Get_Struct(self, git_remote, remote);

	Check_Type(rb_refspec, T_STRING);

	if (direction == GIT_DIRECTION_FETCH)
		error = git_remote_add_fetch(remote, StringValueCStr(rb_refspec));
	else
		error = git_remote_add_push(remote, StringValueCStr(rb_refspec));

	rugged_exception_check(error);

	return Qnil;
}

/*
 *  call-seq:
 *    remote.add_fetch(refspec) -> nil
 *
 *  Add a fetch refspec to the remote
 */
static VALUE rb_git_remote_add_fetch(VALUE self, VALUE rb_refspec)
{
	return rb_git_remote_add_refspec(self, rb_refspec, GIT_DIRECTION_FETCH);
}

/*
 *  call-seq:
 *    remote.add_push(refspec) -> nil
 *
 *  Add a push refspec to the remote
 */
static VALUE rb_git_remote_add_push(VALUE self, VALUE rb_refspec)
{
	return rb_git_remote_add_refspec(self, rb_refspec, GIT_DIRECTION_PUSH);
}

/*
 *  call-seq:
 *    remote.clear_refspecs -> nil
 *
 *  Remove all configured fetch and push refspecs from the remote.
 */
static VALUE rb_git_remote_clear_refspecs(VALUE self)
{
	git_remote *remote;

	Data_Get_Struct(self, git_remote, remote);

	git_remote_clear_refspecs(remote);

	return Qnil;
}

/*
 *  call-seq:
 *    Remote.names(repository) -> array
 *
 *  Returns the names of all remotes in +repository+
 *
 *    Rugged::Remote.names(@repo) #=> ['origin', 'upstream']
 */

static VALUE rb_git_remote_names(VALUE klass, VALUE rb_repo)
{
	git_repository *repo;
	git_strarray remotes;
	VALUE rb_remote_names;
	int error;

	rugged_check_repo(rb_repo);

	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_list(&remotes, repo);
	rugged_exception_check(error);

	rb_remote_names = rugged_strarray_to_rb_ary(&remotes);
	git_strarray_free(&remotes);
	return rb_remote_names;
}

/* :nodoc: */
static VALUE rb_git_remote_each(VALUE klass, VALUE rb_repo)
{
	git_repository *repo;
	git_strarray remotes;
	size_t i;
	int error = 0;
	int exception = 0;

	if (!rb_block_given_p())
		return rb_funcall(klass, rb_intern("to_enum"), 2, CSTR2SYM("each"), rb_repo);

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	error = git_remote_list(&remotes, repo);
	rugged_exception_check(error);

	for (i = 0; !exception && !error && i < remotes.count; ++i) {
		git_remote *remote;
		error = git_remote_load(&remote, repo, remotes.strings[i]);

		if (!error) {
			rb_protect(
				rb_yield, rugged_remote_new(klass, rb_repo, remote),
				&exception);
		}
	}

	git_strarray_free(&remotes);

	if (exception)
		rb_jump_tag(exception);

	rugged_exception_check(error);
	return Qnil;
}

/*
 *  call-seq:
 *    remote.save -> true
 *
 *  Saves the remote data ( url, fetchspecs, ...) to the config
 *
 *  One can't save a in-memory remote created with Remote.new.
 *  Doing so will result in an exception being raised.
*/
static VALUE rb_git_remote_save(VALUE self)
{
	git_remote *remote;

	Data_Get_Struct(self, git_remote, remote);

	rugged_exception_check(
		git_remote_save(remote)
	);
	return Qtrue;
}

static int cb_remote__rename_problem(const char* refspec_name, void *payload)
{
	rb_ary_push((VALUE) payload, rb_str_new_utf8(refspec_name));
	return 0;
}

/*
 *  call-seq:
 *    remote.rename!(new_name) -> array or nil
 *
 *  Renames a remote
 *
 *  All remote-tracking branches and configuration settings
 *  for the remote are updated.
 *
 *  Returns +nil+ if everything was updated or array of fetch refspecs
 *  that haven't been automatically updated and need potential manual
 *  tweaking.
 *
 *  A temporary in-memory remote, created with Remote.new
 *  cannot be given a name with this method.
 *    remote = Rugged::Remote.lookup(@repo, 'origin')
 *    remote.rename!('upstream') #=> nil
 *
*/
static VALUE rb_git_remote_rename(VALUE self, VALUE rb_new_name)
{
	git_remote *remote;
	int error = 0;
	VALUE rb_refspec_ary = rb_ary_new();

	Check_Type(rb_new_name, T_STRING);
	Data_Get_Struct(self, git_remote, remote);
	error = git_remote_rename(
			remote,
			StringValueCStr(rb_new_name),
			cb_remote__rename_problem, (void *)rb_refspec_ary);

	rugged_exception_check(error);

	return RARRAY_LEN(rb_refspec_ary) == 0 ? Qnil : rb_refspec_ary;
}

/*
 *  call-seq:
 *    remote.fetch(refspecs = nil, options = {}) -> hash
 *
 *  Downloads new data from the remote for the given +refspecs+ and updates tips.
 *
 *  You can optionally pass in an alternative list of +refspecs+ to use instead of the fetch
 *  refspecs already configured for +remote+.
 *
 *  Returns a hash containing statistics for the fetch operation.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the fetch operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :progress ::
 *    A callback that will be executed with the textual progress received from the remote.
 *    This is the text send over the progress side-band (ie. the "counting objects" output).
 *
 *  :transfer_progress ::
 *    A callback that will be executed to report clone progress information. It will be passed
 *    the amount of +total_objects+, +indexed_objects+, +received_objects+, +local_objects+,
 *    +total_deltas+, +indexed_deltas+ and +received_bytes+.
 *
 *  :update_tips ::
 *    A callback that will be executed each time a reference is updated locally. It will be
 *    passed the +refname+, +old_oid+ and +new_oid+.
 *
 *  :message ::
 *    The message to insert into the reflogs. Defaults to "fetch".
 *
 *  :signature ::
 *    The signature to be used for updating the reflogs.
 *
 *  Example:
 *
 *    remote = Rugged::Remote.lookup(@repo, 'origin')
 *    remote.fetch({
 *      transfer_progress: lambda { |total_objects, indexed_objects, received_objects, local_objects, total_deltas, indexed_deltas, received_bytes|
 *        # ...
 *      }
 *    })
 */
static VALUE rb_git_remote_fetch(int argc, VALUE *argv, VALUE self)
{
	git_remote *remote, *tmp_remote = NULL;
	git_repository *repo;
	git_signature *signature = NULL;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, Qnil, 0 };

	char *log_message = NULL;
	int error, i;

	VALUE rb_options, rb_refspecs, rb_result = Qnil, rb_repo = rugged_owner(self);

	rb_scan_args(argc, argv, "01:", &rb_refspecs, &rb_options);

	if (!NIL_P(rb_refspecs)) {
		Check_Type(rb_refspecs, T_ARRAY);
		for (i = 0; i < RARRAY_LEN(rb_refspecs); ++i) {
			VALUE rb_refspec = rb_ary_entry(rb_refspecs, i);
			Check_Type(rb_refspec, T_STRING);
		}
	}

	Data_Get_Struct(self, git_remote, remote);
	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);

	if (!NIL_P(rb_options)) {
		VALUE rb_val = rb_hash_aref(rb_options, CSTR2SYM("signature"));
		if (!NIL_P(rb_val))
			signature = rugged_signature_get(rb_val, repo);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));
		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);

		rugged_remote_init_callbacks_and_payload_from_options(rb_options, &callbacks, &payload);
	}

	if ((error = git_remote_dup(&tmp_remote, remote)) ||
		(error = git_remote_set_callbacks(tmp_remote, &callbacks)))
		goto cleanup;

	if (!NIL_P(rb_refspecs)) {
		git_remote_clear_refspecs(tmp_remote);
		for (i = 0; !error && i < RARRAY_LEN(rb_refspecs); ++i) {
			VALUE rb_refspec = rb_ary_entry(rb_refspecs, i);

			if ((error = git_remote_add_fetch(tmp_remote, StringValueCStr(rb_refspec))))
				goto cleanup;
		}
	}

	if ((error = git_remote_fetch(tmp_remote, signature, log_message)) == GIT_OK) {
		const git_transfer_progress *stats = git_remote_stats(tmp_remote);

		rb_result = rb_hash_new();
		rb_hash_aset(rb_result, CSTR2SYM("total_objects"),    UINT2NUM(stats->total_objects));
		rb_hash_aset(rb_result, CSTR2SYM("indexed_objects"),  UINT2NUM(stats->indexed_objects));
		rb_hash_aset(rb_result, CSTR2SYM("received_objects"), UINT2NUM(stats->received_objects));
		rb_hash_aset(rb_result, CSTR2SYM("local_objects"),    UINT2NUM(stats->local_objects));
		rb_hash_aset(rb_result, CSTR2SYM("total_deltas"),     UINT2NUM(stats->total_deltas));
		rb_hash_aset(rb_result, CSTR2SYM("indexed_deltas"),   UINT2NUM(stats->indexed_deltas));
		rb_hash_aset(rb_result, CSTR2SYM("received_bytes"),   INT2FIX(stats->received_bytes));
	}

	cleanup:

	git_signature_free(signature);
	git_remote_free(tmp_remote);

	if (payload.exception)
		rb_jump_tag(payload.exception);

	rugged_exception_check(error);

	return rb_result;
}

static int push_status_cb(const char *ref, const char *msg, void *payload)
{
	VALUE rb_result_hash = (VALUE)payload;
	if (msg != NULL)
		rb_hash_aset(rb_result_hash, rb_str_new_utf8(ref), rb_str_new_utf8(msg));

	return GIT_OK;
}

/*
 *  call-seq:
 *    remote.push(refspecs = nil, options = {}) -> hash
 *
 *  Pushes the given +refspecs+ to the given +remote+. Returns a hash that contains
 *  key-value pairs that reflect pushed refs and error messages, if applicable.
 *
 *  The following options can be passed in the +options+ Hash:
 *
 *  :credentials ::
 *    The credentials to use for the push operation. Can be either an instance of one
 *    of the Rugged::Credentials types, or a proc returning one of the former.
 *    The proc will be called with the +url+, the +username+ from the url (if applicable) and
 *    a list of applicable credential types.
 *
 *  :message ::
 *    A single line log message to be appended to the reflog of each local remote-tracking
 *    branch that gets updated. Defaults to: "fetch".
 *
 *  :signature ::
 *    The signature to be used for populating the reflog entries.
 *
 *  Example:
 *
 *    remote = Rugged::Remote.lookup(@repo, 'origin')
 *    remote.push(["refs/heads/master", ":refs/heads/to_be_deleted"])
 */
static VALUE rb_git_remote_push(int argc, VALUE *argv, VALUE self)
{
	VALUE rb_refspecs, rb_options, rb_val;
	VALUE rb_repo = rugged_owner(self);
	VALUE rb_exception = Qnil, rb_result = rb_hash_new();

	git_repository *repo;
	git_remote *remote, *tmp_remote = NULL;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	git_push *push = NULL;
	git_signature *signature = NULL;

	int error = 0, i = 0;
	char *log_message = NULL;

	struct rugged_remote_cb_payload payload = { Qnil, Qnil, Qnil, Qnil, 0 };

	rb_scan_args(argc, argv, "01:", &rb_refspecs, &rb_options);

	if (!NIL_P(rb_refspecs)) {
		Check_Type(rb_refspecs, T_ARRAY);
		for (i = 0; i < RARRAY_LEN(rb_refspecs); ++i) {
			VALUE rb_refspec = rb_ary_entry(rb_refspecs, i);
			Check_Type(rb_refspec, T_STRING);
		}
	}

	rugged_check_repo(rb_repo);
	Data_Get_Struct(rb_repo, git_repository, repo);
	Data_Get_Struct(self, git_remote, remote);

	if (!NIL_P(rb_options)) {
		rugged_remote_init_callbacks_and_payload_from_options(rb_options, &callbacks, &payload);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("message"));
		if (!NIL_P(rb_val))
			log_message = StringValueCStr(rb_val);

		rb_val = rb_hash_aref(rb_options, CSTR2SYM("signature"));
		if (!NIL_P(rb_val))
			signature = rugged_signature_get(rb_val, repo);
	}

	// Create a temporary remote that we use for pushing
	if ((error = git_remote_dup(&tmp_remote, remote)) ||
	    (error = git_remote_set_callbacks(tmp_remote, &callbacks)))
	    goto cleanup;

	if (!NIL_P(rb_refspecs)) {
		git_remote_clear_refspecs(tmp_remote);

		for (i = 0; !error && i < RARRAY_LEN(rb_refspecs); ++i) {
			VALUE rb_refspec = rb_ary_entry(rb_refspecs, i);

			if ((error = git_remote_add_push(tmp_remote, StringValueCStr(rb_refspec))))
				goto cleanup;
		}
	}

	if ((error = git_push_new(&push, tmp_remote)))
		goto cleanup;

	// TODO: Get rid of this once git_remote_push lands in libgit2.
	{
		git_strarray push_refspecs;
		size_t i;

		if ((error = git_remote_get_push_refspecs(&push_refspecs, tmp_remote)))
			goto cleanup;

		if (push_refspecs.count == 0) {
			rb_exception = rb_exc_new2(rb_eRuggedError, "no pushspecs are configured for the given remote");
			goto cleanup;
		}

		for (i = 0; !error && i < push_refspecs.count; ++i) {
			error = git_push_add_refspec(push, push_refspecs.strings[i]);
		}

		git_strarray_free(&push_refspecs);
		if (error) goto cleanup;
	}

	if ((error = git_push_finish(push)))
		goto cleanup;

	if (!git_push_unpack_ok(push)) {
		rb_exception = rb_exc_new2(rb_eRuggedError, "the remote side did not unpack successfully");
		goto cleanup;
	}

	if ((error = git_push_status_foreach(push, &push_status_cb, (void *)rb_result)) ||
	    (error = git_push_update_tips(push, signature, log_message)))
	    goto cleanup;

cleanup:
	git_push_free(push);
	git_remote_free(tmp_remote);
	git_signature_free(signature);

	if (!NIL_P(rb_exception))
		rb_exc_raise(rb_exception);

	rugged_exception_check(error);

	return rb_result;
}

void Init_rugged_remote(void)
{
	rb_cRuggedRemote = rb_define_class_under(rb_mRugged, "Remote", rb_cObject);

	rb_define_singleton_method(rb_cRuggedRemote, "new", rb_git_remote_new, 2);
	rb_define_singleton_method(rb_cRuggedRemote, "add", rb_git_remote_add, 3);
	rb_define_singleton_method(rb_cRuggedRemote, "lookup", rb_git_remote_lookup, 2);
	rb_define_singleton_method(rb_cRuggedRemote, "names", rb_git_remote_names, 1);
	rb_define_singleton_method(rb_cRuggedRemote, "each", rb_git_remote_each, 1);

	rb_define_method(rb_cRuggedRemote, "name", rb_git_remote_name, 0);
	rb_define_method(rb_cRuggedRemote, "url", rb_git_remote_url, 0);
	rb_define_method(rb_cRuggedRemote, "url=", rb_git_remote_set_url, 1);
	rb_define_method(rb_cRuggedRemote, "push_url", rb_git_remote_push_url, 0);
	rb_define_method(rb_cRuggedRemote, "push_url=", rb_git_remote_set_push_url, 1);
	rb_define_method(rb_cRuggedRemote, "fetch_refspecs", rb_git_remote_fetch_refspecs, 0);
	rb_define_method(rb_cRuggedRemote, "push_refspecs", rb_git_remote_push_refspecs, 0);
	rb_define_method(rb_cRuggedRemote, "add_fetch", rb_git_remote_add_fetch, 1);
	rb_define_method(rb_cRuggedRemote, "add_push", rb_git_remote_add_push, 1);
	rb_define_method(rb_cRuggedRemote, "ls", rb_git_remote_ls, -1);
	rb_define_method(rb_cRuggedRemote, "fetch", rb_git_remote_fetch, -1);
	rb_define_method(rb_cRuggedRemote, "push", rb_git_remote_push, -1);
	rb_define_method(rb_cRuggedRemote, "clear_refspecs", rb_git_remote_clear_refspecs, 0);
	rb_define_method(rb_cRuggedRemote, "save", rb_git_remote_save, 0);
	rb_define_method(rb_cRuggedRemote, "rename!", rb_git_remote_rename, 1);
}
