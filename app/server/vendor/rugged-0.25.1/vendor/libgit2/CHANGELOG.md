v0.25 + 1
-------

### Changes or improvements

### API additions

### API removals

### Breaking API changes

v0.25
-------

### Changes or improvements

* Fix repository discovery with `git_repository_discover` and
  `git_repository_open_ext` to match git's handling of a ceiling
  directory at the current directory. git only checks ceiling
  directories when its search ascends to a parent directory.  A ceiling
  directory matching the starting directory will not prevent git from
  finding a repository in the starting directory or a parent directory.

* Do not fail when deleting remotes in the presence of broken
  global configs which contain branches.

* Support for reading and writing git index v4 files

* Improve the performance of the revwalk and bring us closer to git's code.

* The reference db has improved support for concurrency and returns `GIT_ELOCKED`
  when an operation could not be performed due to locking.

* Nanosecond resolution is now activated by default, following git's change to
  do this.

* We now restrict the set of ciphers we let OpenSSL use by default.

* Users can now register their own merge drivers for use with `.gitattributes`.
  The library also gained built-in support for the union merge driver.

* The default for creating references is now to validate that the object does
  exist.

* Add `git_proxy_options` which is used by the different networking
  implementations to let the caller specify the proxy settings instead of
  relying on the environment variables.

### API additions

* You can now get the user-agent used by libgit2 using the
  `GIT_OPT_GET_USER_AGENT` option with `git_libgit2_opts()`.
  It is the counterpart to `GIT_OPT_SET_USER_AGENT`.

* The `GIT_OPT_SET_SSL_CIPHERS` option for `git_libgit2_opts()` lets you specify
  a custom list of ciphers to use for OpenSSL.

* `git_commit_create_buffer()` creates a commit and writes it into a
  user-provided buffer instead of writing it into the object db. Combine it with
  `git_commit_create_with_signature()` in order to create a commit with a
  cryptographic signature.

* `git_blob_create_fromstream()` and
  `git_blob_create_fromstream_commit()` allow you to create a blob by
  writing into a stream. Useful when you do not know the final size or
  want to copy the contents from another stream.

* New flags for `git_repository_open_ext`:

    * `GIT_REPOSITORY_OPEN_NO_DOTGIT` - Do not check for a repository by
      appending `/.git` to the `start_path`; only open the repository if
      `start_path` itself points to the git directory.
    * `GIT_REPOSITORY_OPEN_FROM_ENV` - Find and open a git repository,
      respecting the environment variables used by the git command-line
      tools. If set, `git_repository_open_ext` will ignore the other
      flags and the `ceiling_dirs` argument, and will allow a NULL
      `path` to use `GIT_DIR` or search from the current directory. The
      search for a repository will respect `$GIT_CEILING_DIRECTORIES`
      and `$GIT_DISCOVERY_ACROSS_FILESYSTEM`.  The opened repository
      will respect `$GIT_INDEX_FILE`, `$GIT_NAMESPACE`,
      `$GIT_OBJECT_DIRECTORY`, and `$GIT_ALTERNATE_OBJECT_DIRECTORIES`.
      In the future, this flag will also cause `git_repository_open_ext`
      to respect `$GIT_WORK_TREE` and `$GIT_COMMON_DIR`; currently,
      `git_repository_open_ext` with this flag will error out if either
      `$GIT_WORK_TREE` or `$GIT_COMMON_DIR` is set.

* `git_diff_from_buffer()` can create a `git_diff` object from the contents
  of a git-style patch file.

* `git_index_version()` and `git_index_set_version()` to get and set
  the index version

* `git_odb_expand_ids()` lets you check for the existence of multiple
  objects at once.

* The new `git_blob_dup()`, `git_commit_dup()`, `git_tag_dup()` and
  `git_tree_dup()` functions provide type-specific wrappers for
  `git_object_dup()` to reduce noise and increase type safety for callers.

* `git_reference_dup()` lets you duplicate a reference to aid in ownership
  management and cleanup.

* `git_signature_from_buffer()` lets you create a signature from a string in the
  format that appear in objects.

* `git_tree_create_updated()` lets you create a tree based on another one
  together with a list of updates. For the covered update cases, it's more
  efficient than the `git_index` route.

* `git_apply_patch()` applies hunks from a `git_patch` to a buffer.

* `git_diff_to_buf()` lets you print an entire diff directory to a buffer,
  similar to how `git_patch_to_buf()` works.

* `git_proxy_init_options()` is added to initialize a `git_proxy_options`
  structure at run-time.

* `git_merge_driver_register()`, `git_merge_driver_unregister()` let you
  register and unregister a custom merge driver to be used when `.gitattributes`
  specifies it.

* `git_merge_driver_lookup()` can be used to look up a merge driver by name.

* `git_merge_driver_source_repo()`, `git_merge_driver_source_ancestor()`,
  `git_merge_driver_source_ours()`, `git_merge_driver_source_theirs()`,
  `git_merge_driver_source_file_options()` added as accessors to
  `git_merge_driver_source`.

### API removals

* `git_blob_create_fromchunks()` has been removed in favour of
  `git_blob_create_fromstream()`.

### Breaking API changes

* `git_packbuilder_object_count` and `git_packbuilder_written` now
  return a `size_t` instead of a `uint32_t` for more thorough
  compatibility with the rest of the library.

* `git_packbuiler_progress` now provides explicitly sized `uint32_t`
  values instead of `unsigned int`.

* `git_diff_file` now includes an `id_abbrev` field that reflects the
  number of nibbles set in the `id` field.

* `git_odb_backend` now has a `freshen` function pointer.  This optional
  function pointer is similar to the `exists` function, but it will update
  a last-used marker.  For filesystem-based object databases, this updates
  the timestamp of the file containing the object, to indicate "freshness".
  If this is `NULL`, then it will not be called and the `exists` function
  will be used instead.

* `git_remote_connect()` now accepts `git_proxy_options` argument, and
  `git_fetch_options` and `git_push_options` each have a `proxy_opts` field.

* `git_merge_options` now provides a `default_driver` that can be used
  to provide the name of a merge driver to be used to handle files changed
  during a merge.

v0.24
-------

### Changes or improvements

* Custom merge drivers can now be registered, which allows callers to
  configure callbacks to honor `merge=driver` configuration in
  `.gitattributes`.

* Custom filters can now be registered with wildcard attributes, for
  example `filter=*`.  Consumers should examine the attributes parameter
  of the `check` function for details.

* Symlinks are now followed when locking a file, which can be
  necessary when multiple worktrees share a base repository.

* You can now set your own user-agent to be sent for HTTP requests by
  using the `GIT_OPT_SET_USER_AGENT` with `git_libgit2_opts()`.

* You can set custom HTTP header fields to be sent along with requests
  by passing them in the fetch and push options.

* Tree objects are now assumed to be sorted. If a tree is not
  correctly formed, it will give bad results. This is the git approach
  and cuts a significant amount of time when reading the trees.

* Filter registration is now protected against concurrent
  registration.

* Filenames which are not valid on Windows in an index no longer cause
  to fail to parse it on that OS.

* Rebases can now be performed purely in-memory, without touching the
  repository's workdir.

* When adding objects to the index, or when creating new tree or commit
  objects, the inputs are validated to ensure that the dependent objects
  exist and are of the correct type.  This object validation can be
  disabled with the GIT_OPT_ENABLE_STRICT_OBJECT_CREATION option.

* The WinHTTP transport's handling of bad credentials now behaves like
  the others, asking for credentials again.

### API additions

* `git_config_lock()` has been added, which allow for
  transactional/atomic complex updates to the configuration, removing
  the opportunity for concurrent operations and not committing any
  changes until the unlock.

* `git_diff_options` added a new callback `progress_cb` to report on the
  progress of the diff as files are being compared. The documentation of
  the existing callback `notify_cb` was updated to reflect that it only
  gets called when new deltas are added to the diff.

* `git_fetch_options` and `git_push_options` have gained a `custom_headers`
  field to set the extra HTTP header fields to send.

* `git_stream_register_tls()` lets you register a callback to be used
  as the constructor for a TLS stream instead of the libgit2 built-in
  one.

* `git_commit_header_field()` allows you to look up a specific header
  field in a commit.

* `git_commit_extract_signature()` extracts the signature from a
  commit and gives you both the signature and the signed data so you
  can verify it.

### API removals

* No APIs were removed in this version.

### Breaking API changes

* The `git_merge_tree_flag_t` is now `git_merge_flag_t`.  Subsequently,
  its members are no longer prefixed with `GIT_MERGE_TREE_FLAG` but are
  now prefixed with `GIT_MERGE_FLAG`, and the `tree_flags` field of the
  `git_merge_options` structure is now named `flags`.

* The `git_merge_file_flags_t` enum is now `git_merge_file_flag_t` for
  consistency with other enum type names.

* `git_cert` descendent types now have a proper `parent` member

* It is the responsibility of the refdb backend to decide what to do
  with the reflog on ref deletion. The file-based backend must delete
  it, a database-backed one may wish to archive it.

* `git_config_backend` has gained two entries. `lock` and `unlock`
  with which to implement the transactional/atomic semantics for the
  configuration backend.

* `git_index_add` and `git_index_conflict_add()` will now use the case
  as provided by the caller on case insensitive systems.  Previous
  versions would keep the case as it existed in the index.  This does
  not affect the higher-level `git_index_add_bypath` or
  `git_index_add_frombuffer` functions.

* The `notify_payload` field of `git_diff_options` was renamed to `payload`
  to reflect that it's also the payload for the new progress callback.

* The `git_config_level_t` enum has gained a higher-priority value
  `GIT_CONFIG_LEVEL_PROGRAMDATA` which represent a rough Windows equivalent
  to the system level configuration.

* `git_rebase_options` now has a `merge_options` field.

* The index no longer performs locking itself. This is not something
  users of the library should have been relying on as it's not part of
  the concurrency guarantees.

* `git_remote_connect()` now takes a `custom_headers` argument to set
  the extra HTTP header fields to send.

v0.23
------

### Changes or improvements

* Patience and minimal diff drivers can now be used for merges.

* Merges can now ignore whitespace changes.

* Updated binary identification in CRLF filtering to avoid false positives in
  UTF-8 files.

* Rename and copy detection is enabled for small files.

* Checkout can now handle an initial checkout of a repository, making
  `GIT_CHECKOUT_SAFE_CREATE` unnecessary for users of clone.

* The signature parameter in the ref-modifying functions has been
  removed. Use `git_repository_set_ident()` and
  `git_repository_ident()` to override the signature to be used.

* The local transport now auto-scales the number of threads to use
  when creating the packfile instead of sticking to one.

* Reference renaming now uses the right id for the old value.

* The annotated version of branch creation, HEAD detaching and reset
  allow for specifying the expression from the user to be put into the
  reflog.

* `git_rebase_commit` now returns `GIT_EUNMERGED` when you attempt to
  commit with unstaged changes.

* On Mac OS X, we now use SecureTransport to provide the cryptographic
  support for HTTPS connections insead of OpenSSL.

* Checkout can now accept an index for the baseline computations via the
  `baseline_index` member.

* The configuration for fetching is no longer stored inside the
  `git_remote` struct but has been moved to a `git_fetch_options`. The
  remote functions now take these options or the callbacks instead of
  setting them beforehand.

* `git_submodule` instances are no longer cached or shared across
  lookup. Each submodule represents the configuration at the time of
  loading.

* The index now uses diffs for `add_all()` and `update_all()` which
  gives it a speed boost and closer semantics to git.

* The ssh transport now reports the stderr output from the server as
  the error message, which allows you to get the "repository not
  found" messages.

* `git_index_conflict_add()` will remove staged entries that exist for
  conflicted paths.

* The flags for a `git_diff_file` will now have the `GIT_DIFF_FLAG_EXISTS`
  bit set when a file exists on that side of the diff.  This is useful
  for understanding whether a side of the diff exists in the presence of
  a conflict.

* The constructor for a write-stream into the odb now takes
  `git_off_t` instead of `size_t` for the size of the blob, which
  allows putting large files into the odb on 32-bit systems.

* The remote's push and pull URLs now honor the url.$URL.insteadOf
  configuration. This allows modifying URL prefixes to a custom
  value via gitconfig.

* `git_diff_foreach`, `git_diff_blobs`, `git_diff_blob_to_buffer`,
  and `git_diff_buffers` now accept a new binary callback of type
  `git_diff_binary_cb` that includes the binary diff information.

* The race condition mitigations described in `racy-git.txt` have been
  implemented.

* If libcurl is installed, we will use it to connect to HTTP(S)
  servers.

### API additions

* The `git_merge_options` gained a `file_flags` member.

* Parsing and retrieving a configuration value as a path is exposed
  via `git_config_parse_path()` and `git_config_get_path()`
  respectively.

* `git_repository_set_ident()` and `git_repository_ident()` serve to
  set and query which identity will be used when writing to the
  reflog.

* `git_config_entry_free()` frees a config entry.

* `git_config_get_string_buf()` provides a way to safely retrieve a
  string from a non-snapshot configuration.

* `git_annotated_commit_from_revspec()` allows to get an annotated
  commit from an extended sha synatx string.

* `git_repository_set_head_detached_from_annotated()`,
  `git_branch_create_from_annotated()` and
  `git_reset_from_annotated()` allow for the caller to provide an
  annotated commit through which they can control what expression is
  put into the reflog as the source/target.

* `git_index_add_frombuffer()` can now create a blob from memory
  buffer and add it to the index which is attached to a repository.

* The structure `git_fetch_options` has been added to determine the
  runtime configuration for fetching, such as callbacks, pruning and
  autotag behaviour. It has the runtime initializer
  `git_fetch_init_options()`.

* The enum `git_fetch_prune_t` has been added, letting you specify the
  pruning behaviour for a fetch.

* A push operation will notify the caller of what updates it indends
  to perform on the remote, which provides similar information to
  git's pre-push hook.

* `git_stash_apply()` can now apply a stashed state from the stash list,
  placing the data into the working directory and index.

* `git_stash_pop()` will apply a stashed state (like `git_stash_apply()`)
  but will remove the stashed state after a successful application.

* A new error code `GIT_EEOF` indicates an early EOF from the
  server. This typically indicates an error with the URL or
  configuration of the server, and tools can use this to show messages
  about failing to communicate with the server.

* A new error code `GIT_EINVALID` indicates that an argument to a
  function is invalid, or an invalid operation was requested.

* `git_diff_index_to_workdir()` and `git_diff_tree_to_index()` will now
  produce deltas of type `GIT_DELTA_CONFLICTED` to indicate that the index
  side of the delta is a conflict.

* The `git_status` family of functions will now produce status of type
  `GIT_STATUS_CONFLICTED` to indicate that a conflict exists for that file
  in the index.

* `git_index_entry_is_conflict()` is a utility function to determine if
  a given index entry has a non-zero stage entry, indicating that it is
  one side of a conflict.

* It is now possible to pass a keypair via a buffer instead of a
  path. For this, `GIT_CREDTYPE_SSH_MEMORY` and
  `git_cred_ssh_key_memory_new()` have been added.

* `git_filter_list_contains` will indicate whether a particular
  filter will be run in the given filter list.

* `git_commit_header_field()` has been added, which allows retrieving
  the contents of an arbitrary header field.

* `git_submodule_set_branch()` allows to set the configured branch for
  a submodule.

### API removals

* `git_remote_save()` and `git_remote_clear_refspecs()` have been
  removed. Remote's configuration is changed via the configuration
  directly or through a convenience function which performs changes to
  the configuration directly.

* `git_remote_set_callbacks()`, `git_remote_get_callbacks()` and
  `git_remote_set_transport()` have been removed and the remote no
  longer stores this configuration.

* `git_remote_set_fetch_refpecs()` and
  `git_remote_set_push_refspecs()` have been removed. There is no
  longer a way to set the base refspecs at run-time.

* `git_submodule_save()` has been removed. The submodules are no
  longer configured via the objects.

* `git_submodule_reload_all()` has been removed as we no longer cache
  submodules.

### Breaking API changes

* `git_smart_subtransport_cb` now has a `param` parameter.

* The `git_merge_options` structure member `flags` has been renamed
  to `tree_flags`.

* The `git_merge_file_options` structure member `flags` is now
  an unsigned int. It was previously a `git_merge_file_flags_t`.

* `GIT_CHECKOUT_SAFE_CREATE` has been removed.  Most users will generally
  be able to switch to `GIT_CHECKOUT_SAFE`, but if you require missing
  file handling during checkout, you may now use `GIT_CHECKOUT_SAFE |
  GIT_CHECKOUT_RECREATE_MISSING`.

* The `git_clone_options` and `git_submodule_update_options`
  structures no longer have a `signature` field.

* The following functions have removed the signature and/or log message
  parameters in favour of git-emulating ones.

    * `git_branch_create()`, `git_branch_move()`
    * `git_rebase_init()`, `git_rebase_abort()`
    * `git_reference_symbolic_create_matching()`,
      `git_reference_symbolic_create()`, `git_reference_create()`,
      `git_reference_create_matching()`,
      `git_reference_symbolic_set_target()`,
      `git_reference_set_target()`, `git_reference_rename()`
    * `git_remote_update_tips()`, `git_remote_fetch()`, `git_remote_push()`
    * `git_repository_set_head()`,
      `git_repository_set_head_detached()`,
      `git_repository_detach_head()`
    * `git_reset()`

* `git_config_get_entry()` now gives back a ref-counted
  `git_config_entry`. You must free it when you no longer need it.

* `git_config_get_string()` will return an error if used on a
  non-snapshot configuration, as there can be no guarantee that the
  returned pointer is valid.

* `git_note_default_ref()` now uses a `git_buf` to return the string,
  as the string is otherwise not guaranteed to stay allocated.

* `git_rebase_operation_current()` will return `GIT_REBASE_NO_OPERATION`
  if it is called immediately after creating a rebase session but before
  you have applied the first patch.

* `git_rebase_options` now contains a `git_checkout_options` struct
  that will be used for functions that modify the working directory,
  namely `git_rebase_init`, `git_rebase_next` and
  `git_rebase_abort`.  As a result, `git_rebase_open` now also takes
  a `git_rebase_options` and only the `git_rebase_init` and
  `git_rebase_open` functions take a `git_rebase_options`, where they
  will persist the options to subsequent `git_rebase` calls.

* The `git_clone_options` struct now has fetch options in a
  `fetch_opts` field instead of remote callbacks in
  `remote_callbacks`.

* The remote callbacks has gained a new member `push_negotiation`
  which gets called before sending the update commands to the server.

* The following functions no longer act on a remote instance but
  change the repository's configuration. Their signatures have changed
  accordingly:

    * `git_remote_set_url()`, `git_remote_seturl()`
    * `git_remote_add_fetch()`, `git_remote_add_push()` and
    * `git_remote_set_autotag()`

* `git_remote_connect()` and `git_remote_prune()` now take a pointer
  to the callbacks.

* `git_remote_fetch()` and `git_remote_download()` now take a pointer
  to fetch options which determine the runtime configuration.

* The `git_remote_autotag_option_t` values have been changed. It has
  gained a `_UNSPECIFIED` default value to specify no override for the
  configured setting.

* `git_remote_update_tips()` now takes a pointer to the callbacks as
  well as a boolean whether to write `FETCH_HEAD` and the autotag
  setting.

* `git_remote_create_anonymous()` no longer takes a fetch refspec as
  url-only remotes cannot have configured refspecs.

* The `git_submodule_update_options` struct now has fetch options in
  the `fetch_opts` field instead of callbacks in the
  `remote_callbacks` field.

* The following functions no longer act on a submodule instance but
  change the repository's configuration. Their signatures have changed
  accordingly:

    * `git_submodule_set_url()`, `git_submodule_set_ignore()`,
      `git_submodule_set_update()`,
      `git_submodule_set_fetch_recurse_submodules()`.

* `git_submodule_status()` no longer takes a submodule instance but a
  repsitory, a submodule name and an ignore setting.

* The `push` function in the `git_transport` interface now takes a
  pointer to the remote callbacks.

* The `git_index_entry` struct's fields' types have been changed to
  more accurately reflect what is in fact stored in the
  index. Specifically, time and file size are 32 bits intead of 64, as
  these values are truncated.

* `GIT_EMERGECONFLICT` is now `GIT_ECONFLICT`, which more accurately
  describes the nature of the error.

* It is no longer allowed to call `git_buf_grow()` on buffers
  borrowing the memory they point to.

v0.22
------

### Changes or improvements

* `git_signature_new()` now requires a non-empty email address.

* Use CommonCrypto libraries for SHA-1 calculation on Mac OS X.

* Disable SSL compression and SSLv2 and SSLv3 ciphers in favor of TLSv1
  in OpenSSL.

* The fetch behavior of remotes with autotag set to `GIT_REMOTE_DOWNLOAD_TAGS_ALL`
  has been changed to match git 1.9.0 and later. In this mode, libgit2 now
  fetches all tags in addition to whatever else needs to be fetched.

* `git_checkout()` now handles case-changing renames correctly on
  case-insensitive filesystems; for example renaming "readme" to "README".

* The search for libssh2 is now done via pkg-config instead of a
  custom search of a few directories.

* Add support for core.protectHFS and core.protectNTFS. Add more
  validation for filenames which we write such as references.

* The local transport now generates textual progress output like
  git-upload-pack does ("counting objects").

* `git_checkout_index()` can now check out an in-memory index that is not
  necessarily the repository's index, so you may check out an index
  that was produced by git_merge and friends while retaining the cached
  information.

* Remove the default timeout for receiving / sending data over HTTP using
  the WinHTTP transport layer.

* Add SPNEGO (Kerberos) authentication using GSSAPI on Unix systems.

* Provide built-in objects for the empty blob (e69de29) and empty
  tree (4b825dc) objects.

* The index' tree cache is now filled upon read-tree and write-tree
  and the cache is written to disk.

* LF -> CRLF filter refuses to handle mixed-EOL files

* LF -> CRLF filter now runs when * text = auto (with Git for Windows 1.9.4)

* File unlocks are atomic again via rename. Read-only files on Windows are
  made read-write if necessary.

* Share open packfiles across repositories to share descriptors and mmaps.

* Use a map for the treebuilder, making insertion O(1)

* The build system now accepts an option EMBED_SSH_PATH which when set
  tells it to include a copy of libssh2 at the given location. This is
  enabled for MSVC.

* Add support for refspecs with the asterisk in the middle of a
  pattern.

* Fetching now performs opportunistic updates. To achieve this, we
  introduce a difference between active and passive refspecs, which
  make `git_remote_download()` and `git_remote_fetch()` to take a list of
  resfpecs to be the active list, similarly to how git fetch accepts a
  list on the command-line.

* The THREADSAFE option to build libgit2 with threading support has
  been flipped to be on by default.

* The remote object has learnt to prune remote-tracking branches. If
  the remote is configured to do so, this will happen via
  `git_remote_fetch()`. You can also call `git_remote_prune()` after
  connecting or fetching to perform the prune.


### API additions

* Introduce `git_buf_text_is_binary()` and `git_buf_text_contains_nul()` for
  consumers to perform binary detection on a git_buf.

* `git_branch_upstream_remote()` has been introduced to provide the
  branch.<name>.remote configuration value.

* Introduce `git_describe_commit()` and `git_describe_workdir()` to provide
  a description of the current commit (and working tree, respectively)
  based on the nearest tag or reference

* Introduce `git_merge_bases()` and the `git_oidarray` type to expose all
  merge bases between two commits.

* Introduce `git_merge_bases_many()` to expose all merge bases between
  multiple commits.

* Introduce rebase functionality (using the merge algorithm only).
  Introduce `git_rebase_init()` to begin a new rebase session,
  `git_rebase_open()` to open an in-progress rebase session,
  `git_rebase_commit()` to commit the current rebase operation,
  `git_rebase_next()` to apply the next rebase operation,
  `git_rebase_abort()` to abort an in-progress rebase and `git_rebase_finish()`
  to complete a rebase operation.

* Introduce `git_note_author()` and `git_note_committer()` to get the author
  and committer information on a `git_note`, respectively.

* A factory function for ssh has been added which allows to change the
  path of the programs to execute for receive-pack and upload-pack on
  the server, `git_transport_ssh_with_paths()`.

* The ssh transport supports asking the remote host for accepted
  credential types as well as multiple challeges using a single
  connection. This requires to know which username you want to connect
  as, so this introduces the USERNAME credential type which the ssh
  transport will use to ask for the username.

* The `GIT_EPEEL` error code has been introduced when we cannot peel a tag
  to the requested object type; if the given object otherwise cannot be
  peeled, `GIT_EINVALIDSPEC` is returned.

* Introduce `GIT_REPOSITORY_INIT_RELATIVE_GITLINK` to use relative paths
  when writing gitlinks, as is used by git core for submodules.

* `git_remote_prune()` has been added. See above for description.


* Introduce reference transactions, which allow multiple references to
  be locked at the same time and updates be queued. This also allows
  us to safely update a reflog with arbitrary contents, as we need to
  do for stash.

### API removals

* `git_remote_supported_url()` and `git_remote_is_valid_url()` have been
  removed as they have become essentially useless with rsync-style ssh paths.

* `git_clone_into()` and `git_clone_local_into()` have been removed from the
  public API in favour of `git_clone callbacks`.

* The option to ignore certificate errors via `git_remote_cert_check()`
  is no longer present. Instead, `git_remote_callbacks` has gained a new
  entry which lets the user perform their own certificate checks.

### Breaking API changes

* `git_cherry_pick()` is now `git_cherrypick()`.

* The `git_submodule_update()` function was renamed to
  `git_submodule_update_strategy()`. `git_submodule_update()` is now used to
  provide functionalty similar to "git submodule update".

* `git_treebuilder_create()` was renamed to `git_treebuilder_new()` to better
  reflect it being a constructor rather than something which writes to
  disk.

* `git_treebuilder_new()` (was `git_treebuilder_create()`) now takes a
  repository so that it can query repository configuration.
  Subsequently, `git_treebuilder_write()` no longer takes a repository.

* `git_threads_init()` and `git_threads_shutdown()` have been renamed to
  `git_libgit2_init()` and `git_libgit2_shutdown()` to better explain what
  their purpose is, as it's grown to be more than just about threads.

* `git_libgit2_init()` and `git_libgit2_shutdown()` now return the number of
  initializations of the library, so consumers may schedule work on the
  first initialization.

* The `git_transport_register()` function no longer takes a priority and takes
  a URL scheme name (eg "http") instead of a prefix like "http://"

* `git_index_name_entrycount()` and `git_index_reuc_entrycount()` now
  return size_t instead of unsigned int.

* The `context_lines` and `interhunk_lines` fields in `git_diff`_options are
  now `uint32_t` instead of `uint16_t`. This allows to set them to `UINT_MAX`,
  in effect asking for "infinite" context e.g. to iterate over all the
  unmodified lines of a diff.

* `git_status_file()` now takes an exact path. Use `git_status_list_new()` if
  pathspec searching is needed.

* `git_note_create()` has changed the position of the notes reference
  name to match `git_note_remove()`.

* Rename `git_remote_load()` to `git_remote_lookup()` to bring it in line
  with the rest of the lookup functions.

* `git_remote_rename()` now takes the repository and the remote's
  current name. Accepting a remote indicates we want to change it,
  which we only did partially. It is much clearer if we accept a name
  and no loaded objects are changed.

* `git_remote_delete()` now accepts the repository and the remote's name
  instead of a loaded remote.

* `git_merge_head` is now `git_annotated_commit`, to better reflect its usage
  for multiple functions (including rebase)

* The `git_clone_options` struct no longer provides the `ignore_cert_errors` or
  `remote_name` members for remote customization.

  Instead, the `git_clone_options` struct has two new members, `remote_cb` and
  `remote_cb_payload`, which allow the caller to completely override the remote
  creation process. If needed, the caller can use this callback to give their
  remote a name other than the default (origin) or disable cert checking.

  The `remote_callbacks` member has been preserved for convenience, although it
  is not used when a remote creation callback is supplied.

* The `git_clone`_options struct now provides `repository_cb` and
  `repository_cb_payload` to allow the user to create a repository with
  custom options.

* The `git_push` struct to perform a push has been replaced with
  `git_remote_upload()`. The refspecs and options are passed as a
  function argument. `git_push_update_tips()` is now also
  `git_remote_update_tips()` and the callbacks are in the same struct as
  the rest.

* The `git_remote_set_transport()` function now sets a transport factory function,
  rather than a pre-existing transport instance.

* The `git_transport` structure definition has moved into the sys/transport.h
  file.

* libgit2 no longer automatically sets the OpenSSL locking
  functions. This is not something which we can know to do. A
  last-resort convenience function is provided in sys/openssl.h,
  `git_openssl_set_locking()` which can be used to set the locking.
