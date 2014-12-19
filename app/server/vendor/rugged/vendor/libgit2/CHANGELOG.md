v0.21 + 1
------

* File unlocks are atomic again via rename. Read-only files on Windows are
  made read-write if necessary.

* Share open packfiles across repositories to share descriptors and mmaps.

* Use a map for the treebuilder, making insertion O(1)

* Introduce reference transactions, which allow multiple references to
  be locked at the same time and updates be queued. This also allows
  us to safely update a reflog with arbitrary contents, as we need to
  do for stash.

* The index' tree cache is now filled upon read-tree and write-tree
  and the cache is written to disk.

* LF -> CRLF filter refuses to handle mixed-EOL files

* LF -> CRLF filter now runs when * text = auto (with Git for Windows 1.9.4)

* The git_transport structure definition has moved into the sys/transport.h
  file.

* The ssh transport supports asking the remote host for accepted
  credential types as well as multiple challeges using a single
  connection. This requires to know which username you want to connect
  as, so this introduces the USERNAME credential type which the ssh
  transport will use to ask for the username.

* The build system now accepts an option EMBED_SSH_PATH which when set
  tells it to include a copy of libssh2 at the given location. This is
  enabled for MSVC.

* The git_transport_register function no longer takes a priority and takes
  a URL scheme name (eg "http") instead of a prefix like "http://"

* The git_remote_set_transport function now sets a transport factory function,
  rather than a pre-existing transport instance.

* A factory function for ssh has been added which allows to change the
  path of the programs to execute for receive-pack and upload-pack on
  the server, git_transport_ssh_with_paths.

* git_remote_rename() now takes the repository and the remote's
  current name. Accepting a remote indicates we want to change it,
  which we only did partially. It is much clearer if we accept a name
  and no loaded objects are changed.

* git_remote_delete() now accepts the repository and the remote's name
  instead of a loaded remote.

* git_remote_supported_url() has been removed as it has become
  essentially useless with rsync-style ssh paths.

* The git_clone_options struct no longer provides the ignore_cert_errors or
  remote_name members for remote customization.

  Instead, the git_clone_options struct has two new members, remote_cb and
  remote_cb_payload, which allow the caller to completely override the remote
  creation process. If needed, the caller can use this callback to give their
  remote a name other than the default (origin) or disable cert checking.

  The remote_callbacks member has been preserved for convenience, although it
  is not used when a remote creation callback is supplied.

* The git_clone_options struct now provides repository_cb and
  repository_cb_payload to allow the user to create a repository with
  custom options.

* The option to ignore certificate errors via git_remote_cert_check()
  is no longer present. Instead, git_remote_callbacks has gained a new
  entry which lets the user perform their own certificate checks.

* git_clone_into and git_clone_local_into have been removed from the
  public API in favour of git_clone callbacks

* Add support for refspecs with the asterisk in the middle of a
  pattern.

* Fetching now performs opportunistic updates. To achieve this, we
  introduce a difference between active and passive refspecs, which
  make git_remote_download and git_remote_fetch to take a list of
  resfpecs to be the active list, similarly to how git fetch accepts a
  list on the command-line.

* Rename git_remote_load() to git_remote_lookup() to bring it in line
  with the rest of the lookup functions.

* git_push_unpack_ok() has been removed and git_push_finish() now
  returns an error if the unpacking failed.

* Introduce git_merge_bases() and the git_oidarray type to expose all
  merge bases between two commits.

* Introduce git_merge_bases_many() to expose all merge bases between
  multiple commits.

* git_merge_head is now git_annotated_commit, to better reflect its usage
  for multiple functions (including rebase)

* Introduce rebase functionality (using the merge algorithm only).
  Introduce git_rebase_init() to begin a new rebase session,
  git_rebase_open() to open an in-progress rebase session,
  git_rebase_commit() to commit the current rebase operation,
  git_rebase_next() to apply the next rebase operation,
  git_rebase_abort() to abort an in-progress rebase and git_rebase_finish()
  to complete a rebase operation.

* Introduce git_note_author() and git_note_committer() to get the author
  and committer information on a git_note, respectively.

* git_note_create() has changed the position of the notes reference
  name to match git_note_remove().

* The THREADSAFE option to build libgit2 with threading support has
  been flipped to be on by default.

* The context_lines and interhunk_lines fields in git_diff_options are
  now uint32_t instead of uint16_t. This allows to set them to UINT_MAX,
  in effect asking for "infinite" context e.g. to iterate over all the
  unmodified lines of a diff.

* git_status_file now takes an exact path. Use git_status_list_new if
  pathspec searching is needed.

* The fetch behavior of remotes with autotag set to GIT_REMOTE_DOWNLOAD_TAGS_ALL
  has been changed to match git 1.9.0 and later. In this mode, libgit2 now
  fetches all tags in addition to whatever else needs to be fetched.

* git_threads_init() and git_threads_shutdown() have been renamed to
  git_libgit2_init() and git_libgit2_shutdown() to better explain what
  their purpose is, as it's grown to be more than just about threads.

* git_libgit2_init() and git_libgit2_shutdown() now return the number of
  initializations of the library, so consumers may schedule work on the
  first initialization.

* git_treebuilder_create now takes a repository so that it can query
  repository configuration.  Subsequently, git_treebuilder_write no
  longer takes a repository.
