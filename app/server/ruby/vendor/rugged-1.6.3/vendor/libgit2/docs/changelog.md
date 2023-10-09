v1.6.3
------

## What's Changed

### Bug fixes

* odb: restore `git_odb_open` by @ethomson in https://github.com/libgit2/libgit2/pull/6520
* Ensure that `git_index_add_all` handles ignored directories by @ethomson in https://github.com/libgit2/libgit2/pull/6521
* pack: use 64 bits for the number of objects by @carlosmn in https://github.com/libgit2/libgit2/pull/6530

### Build and CI improvements

* Remove unused wditer variable by @georgthegreat in https://github.com/libgit2/libgit2/pull/6518
* fs_path: let root run the ownership tests by @ethomson in https://github.com/libgit2/libgit2/pull/6513
* sysdir: Do not declare win32 functions on non-win32 platforms by @Batchyx in https://github.com/libgit2/libgit2/pull/6527
* cmake: don't include `include/git2` by @ethomson in https://github.com/libgit2/libgit2/pull/6529

## New Contributors
* @georgthegreat made their first contribution in https://github.com/libgit2/libgit2/pull/6518

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.6.2...v1.6.3

v1.6.2
------

## What's Changed
### Bug fixes

* remote: always populate old id in update tips by @ethomson in https://github.com/libgit2/libgit2/pull/6506
  The update tips callback would not always be properly provided with an empty (`0000000...`) OID for new refs.

* Revert #6503 by @ethomson in https://github.com/libgit2/libgit2/pull/6511
  The certificate callback added port information for callbacks in #6503, but the format was ambiguous with IPv6 addresses. Revert this change temporarily.

* Add `git_odb_backend_loose` back by @ethomson in https://github.com/libgit2/libgit2/pull/6512
  During SHA256 refactoring, the `git_odb_backend_loose` API was accidentally removed. Add it back.

* meta: configure pkg-config .pc correctly by @ethomson in https://github.com/libgit2/libgit2/pull/6514
  During SHA256 refactoring, the pkg-config `.pc` file was erroneously renamed to `git2` instead of `libgit2`. Repair this.

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.6.1...v1.6.2

v1.6
----

This is release v1.6.1, "Hubbeliges Krokodil". This release adds experimental SHA256 support and includes many new features and bugfixes. This release replaces libgit2 v1.6.0, which did not correctly update its version number(s).

## What's Changed

### New features

* **Support for bare repositories with SHA256 support (experimental)** by @ethomson in https://github.com/libgit2/libgit2/pull/6191
   You can configure experimental SHA256 support in libgit2 with `cmake -DEXPERIMENTAL_SHA256=ON` during project setup. This is useful for considering future integrations, work on clients, and work on language bindings. At present, working with bare repositories should largely work, including remote operations. But many pieces of functionality - including working with the index - are not yet supported. As a result, **libgit2 with SHA256 support should not be used in production or released with package distribution.**

* **Support the notion of a home directory separately from global configuration directory** by @ethomson in https://github.com/libgit2/libgit2/pull/6455 and https://github.com/libgit2/libgit2/pull/6456
  Callers and language bindings can now configure the home directory that libgit2 uses for file lookups (eg, the `.ssh` directory). This configuration is separate from the git global configuration path.

* **stash: partial stash specific files** by @gitkraken-jacobw in https://github.com/libgit2/libgit2/pull/6330
  A stash can be created with only specific files, using a pathspec. This is similar to the `git stash push` command.

* **push: revparse refspec source, so you can push things that are not refs** by @sven-of-cord in https://github.com/libgit2/libgit2/pull/6362
  Pushes can be performed using refspecs instead of only references.

* **Support OpenSSL3** by @ethomson in https://github.com/libgit2/libgit2/pull/6464 and https://github.com/libgit2/libgit2/pull/6471
  OpenSSL 3 is now supported, both when compiled directly and dynamically loaded.

### Bug fixes
* winhttp: support long custom headers by @kcsaul in https://github.com/libgit2/libgit2/pull/6363
* Fix memory leak by @csware in https://github.com/libgit2/libgit2/pull/6382
* Don't fail the whole clone if you can't find a default branch by @torvalds in https://github.com/libgit2/libgit2/pull/6369
* #6366: When a worktree is missing, return `GIT_ENOTFOUND`. by @arroz in https://github.com/libgit2/libgit2/pull/6395
* commit-graph: only verify csum on `git_commit_graph_open()`. by @derrickstolee in https://github.com/libgit2/libgit2/pull/6420
* Ignore missing 'safe.directory' config during ownership checks by @kcsaul in https://github.com/libgit2/libgit2/pull/6408
* Fix leak in `git_tag_create_from_buffer` by @julianmesa-gitkraken in https://github.com/libgit2/libgit2/pull/6421
* http: Update httpclient options when reusing an existing connection. by @slackner in https://github.com/libgit2/libgit2/pull/6416
* Add support for `safe.directory *` by @csware in https://github.com/libgit2/libgit2/pull/6429
* URL parsing for google-compatible URLs by @ethomson in https://github.com/libgit2/libgit2/pull/6326
* Fixes #6433: `git_submodule_update` fails to update configured but missing submodule by @tagesuhu in https://github.com/libgit2/libgit2/pull/6434
* transport: fix capabilities calculation by @russell in https://github.com/libgit2/libgit2/pull/6435
* push: use resolved oid as the source by @ethomson in https://github.com/libgit2/libgit2/pull/6452
* Use `git_clone__submodule` to avoid file checks in workdir by @abizjak in https://github.com/libgit2/libgit2/pull/6444
* #6422: handle dangling symbolic refs gracefully by @arroz in https://github.com/libgit2/libgit2/pull/6423
* `diff_file`: Fix crash when freeing a patch representing an empty untracked file by @jorio in https://github.com/libgit2/libgit2/pull/6475
* clone: clean up options on failure by @ethomson in https://github.com/libgit2/libgit2/pull/6479
* stash: update strarray usage by @ethomson in https://github.com/libgit2/libgit2/pull/6487
* #6491: Sets `oid_type` on repos open with `git_repository_open_bare` by @arroz in https://github.com/libgit2/libgit2/pull/6492
* Handle Win32 shares by @ethomson in https://github.com/libgit2/libgit2/pull/6493
* Make failure to connect to ssh-agent non-fatal by @fxcoudert in https://github.com/libgit2/libgit2/pull/6497
* odb: don't unconditionally add `oid_type` to stream by @ethomson in https://github.com/libgit2/libgit2/pull/6499
* Pass hostkey & port to host verify callback by @fxcoudert in https://github.com/libgit2/libgit2/pull/6503

### Code cleanups
* meta: update version number to v1.6.0-alpha by @ethomson in https://github.com/libgit2/libgit2/pull/6352
* sha256: indirection for experimental functions by @ethomson in https://github.com/libgit2/libgit2/pull/6354
* Delete `create.c.bak` by @lrm29 in https://github.com/libgit2/libgit2/pull/6398
* Support non-cmake builds with an in-tree `experimental.h` by @ethomson in https://github.com/libgit2/libgit2/pull/6405

### Build and CI improvements
* tests: skip flaky-ass googlesource tests by @ethomson in https://github.com/libgit2/libgit2/pull/6353
* clar: remove ftrunacte from libgit2 tests by @boretrk in https://github.com/libgit2/libgit2/pull/6357
* CI Improvements by @ethomson in https://github.com/libgit2/libgit2/pull/6403
* fix compile on Windows with `-DWIN32_LEAN_AND_MEAN` by @christoph-cullmann in https://github.com/libgit2/libgit2/pull/6373
* Fixes #6365 : Uppercase windows.h include fails build in case-sensitive OS by @Vinz2008 in https://github.com/libgit2/libgit2/pull/6377
* ci: update version numbers of actions by @ethomson in https://github.com/libgit2/libgit2/pull/6448
* thread: avoid warnings when building without threads by @ethomson in https://github.com/libgit2/libgit2/pull/6432
* src: hide unused hmac() prototype by @0-wiz-0 in https://github.com/libgit2/libgit2/pull/6458
* tests: update clar test runner by @ethomson in https://github.com/libgit2/libgit2/pull/6459
* ci: always create test summaries, even on failure by @ethomson in https://github.com/libgit2/libgit2/pull/6460
* Fix build failure with `-DEMBED_SSH_PATH` by @vicr123 in https://github.com/libgit2/libgit2/pull/6374
* Define correct `off64_t` for AIX by @bzEq in https://github.com/libgit2/libgit2/pull/6376
* Fix some warnings in main by @ethomson in https://github.com/libgit2/libgit2/pull/6480
* strarray: remove deprecated declaration by @ethomson in https://github.com/libgit2/libgit2/pull/6486
* tests: always unset `HTTP_PROXY` before starting tests by @ethomson in https://github.com/libgit2/libgit2/pull/6498

### Documentation improvements
* add 2-clause BSD license to COPYING by @martinvonz in https://github.com/libgit2/libgit2/pull/6413
* Add new PHP bindings project to language bindings section of README.md by @RogerGee in https://github.com/libgit2/libgit2/pull/6473
* README: clarify the linking exception by @ethomson in https://github.com/libgit2/libgit2/pull/6494
* Correct the definition of "empty" in the docs for `git_repository_is_empty` by @timrogers in https://github.com/libgit2/libgit2/pull/6500

## New Contributors
* @christoph-cullmann made their first contribution in https://github.com/libgit2/libgit2/pull/6373
* @Vinz2008 made their first contribution in https://github.com/libgit2/libgit2/pull/6377
* @torvalds made their first contribution in https://github.com/libgit2/libgit2/pull/6369
* @derrickstolee made their first contribution in https://github.com/libgit2/libgit2/pull/6420
* @julianmesa-gitkraken made their first contribution in https://github.com/libgit2/libgit2/pull/6421
* @slackner made their first contribution in https://github.com/libgit2/libgit2/pull/6416
* @martinvonz made their first contribution in https://github.com/libgit2/libgit2/pull/6413
* @tagesuhu made their first contribution in https://github.com/libgit2/libgit2/pull/6434
* @russell made their first contribution in https://github.com/libgit2/libgit2/pull/6435
* @sven-of-cord made their first contribution in https://github.com/libgit2/libgit2/pull/6362
* @0-wiz-0 made their first contribution in https://github.com/libgit2/libgit2/pull/6458
* @abizjak made their first contribution in https://github.com/libgit2/libgit2/pull/6444
* @vicr123 made their first contribution in https://github.com/libgit2/libgit2/pull/6374
* @bzEq made their first contribution in https://github.com/libgit2/libgit2/pull/6376
* @gitkraken-jacobw made their first contribution in https://github.com/libgit2/libgit2/pull/6330
* @fxcoudert made their first contribution in https://github.com/libgit2/libgit2/pull/6497
* @timrogers made their first contribution in https://github.com/libgit2/libgit2/pull/6500

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.5.0...v1.6.0

v1.5
----

This is release v1.5.0, "Stubentiger". This release adds the basis for an experimental CLI, continues preparing for SHA256 support, adds a benchmarking utility, and has numerous new features and bugfixes.

## What's Changed
### New features
* The beginnings of a git-compatible CLI for testing and benchmarking by @ethomson in https://github.com/libgit2/libgit2/pull/6133
* Add `clone` support to the CLI @ethomson in https://github.com/libgit2/libgit2/pull/6274
* A benchmarking suite to compare libgit2 functionality against git by @ethomson in https://github.com/libgit2/libgit2/pull/6235
* SHA256: add a SHA256 implementation backend by @ethomson in https://github.com/libgit2/libgit2/pull/6144
* SHA256: support dynamically loaded openssl by @ethomson in https://github.com/libgit2/libgit2/pull/6258
* Transport: introduce `git_transport_smart_remote_connect_options` by @lhchavez in https://github.com/libgit2/libgit2/pull/6278
### Bug fixes
* Free parent and ref in lg2_commit before returning. by @apnadkarni in https://github.com/libgit2/libgit2/pull/6219
* xdiff: use xdl_free not free by @ethomson in https://github.com/libgit2/libgit2/pull/6223
* remote: do store the update_tips callback error value by @carlosmn in https://github.com/libgit2/libgit2/pull/6226
* win32: `find_system_dirs` does not return `GIT_ENOTFOUND` by @ethomson in https://github.com/libgit2/libgit2/pull/6228
* Some minor fixes for issues discovered by coverity by @ethomson in https://github.com/libgit2/libgit2/pull/6238
* Fix a string concatenation bug when validating extensions by @bierbaum in https://github.com/libgit2/libgit2/pull/6246
* fetch: support OID refspec without dst by @ethomson in https://github.com/libgit2/libgit2/pull/6251
* Fix crash when regenerating a patch with unquoted spaces in filename by @jorio in https://github.com/libgit2/libgit2/pull/6244
* midx: Fix an undefined behavior (left-shift signed overflow) by @lhchavez in https://github.com/libgit2/libgit2/pull/6260
* Validate repository directory ownership by @ethomson in https://github.com/libgit2/libgit2/pull/6266
* midx: fix large offset table check. by @ccstolley in https://github.com/libgit2/libgit2/pull/6309
* midx: do not verify the checksum on load by @carlosmn in https://github.com/libgit2/libgit2/pull/6291
* revparse: Remove error-prone, redundant test by @dongcarl in https://github.com/libgit2/libgit2/pull/6299
* refs: fix missing error message by @zawata in https://github.com/libgit2/libgit2/pull/6305
* CLI: progress updates by @ethomson in https://github.com/libgit2/libgit2/pull/6319
* A couple of simplications around mwindow by @carlosmn in https://github.com/libgit2/libgit2/pull/6288
* config: update config entry iteration lifecycle by @ethomson in https://github.com/libgit2/libgit2/pull/6320
* repo: allow administrator to own the configuration by @ethomson in https://github.com/libgit2/libgit2/pull/6321
* filter: Fix Segfault by @zawata in https://github.com/libgit2/libgit2/pull/6303
* ntlmclient: LibreSSL 3.5 removed HMAC_CTX_cleanup by @vishwin in https://github.com/libgit2/libgit2/pull/6340
* Fix internal git_sysdir_find* function usage within public git_config_find* functions by @kcsaul in https://github.com/libgit2/libgit2/pull/6335
* fix interactive rebase detect. by @i-tengfei in https://github.com/libgit2/libgit2/pull/6334
* cmake: drop posix dependency from pcre* detection by @jpalus in https://github.com/libgit2/libgit2/pull/6333
* Fix erroneously lax configuration ownership checks by @ethomson in https://github.com/libgit2/libgit2/pull/6341
* pack: don't pretend we support pack files v3 by @ethomson in https://github.com/libgit2/libgit2/pull/6347
* Fix creation of branches and tags with invalid names by @lya001 in https://github.com/libgit2/libgit2/pull/6348
### Security fixes
* Fixes for CVE 2022-29187 by @ethomson in https://github.com/libgit2/libgit2/pull/6349
* zlib: update bundled zlib to v1.2.12 by @ethomson in https://github.com/libgit2/libgit2/pull/6350
### Code cleanups
* sha256: refactoring in preparation for sha256 by @ethomson in https://github.com/libgit2/libgit2/pull/6265
* remote: Delete a now-inexistent API declaration by @lhchavez in https://github.com/libgit2/libgit2/pull/6276
* Fix missing include by @cschlack in https://github.com/libgit2/libgit2/pull/6277
### Build and CI improvements
* meta: show build status for v1.3 and v1.4 branches by @ethomson in https://github.com/libgit2/libgit2/pull/6216
* cmake: Fix package name for system http-parser by @mgorny in https://github.com/libgit2/libgit2/pull/6217
* meta: update version number to v1.5.0-alpha by @ethomson in https://github.com/libgit2/libgit2/pull/6220
* cmake: export libraries needed to compile against libgit2 by @ethomson in https://github.com/libgit2/libgit2/pull/6239
* clone: update bitbucket tests by @ethomson in https://github.com/libgit2/libgit2/pull/6252
* diff: don't stat empty file on arm32 (flaky test) by @ethomson in https://github.com/libgit2/libgit2/pull/6259
* tests: support flaky stat by @ethomson in https://github.com/libgit2/libgit2/pull/6262
* Include test results data in CI by @ethomson in https://github.com/libgit2/libgit2/pull/6306
* Add a .clang-format with our style by @ethomson in https://github.com/libgit2/libgit2/pull/6023
* CI: limits actions scheduled workflows to the main repo by @ethomson in https://github.com/libgit2/libgit2/pull/6342
* ci: update dockerfiles for mbedTLS new url by @ethomson in https://github.com/libgit2/libgit2/pull/6343
### Documentation improvements
* Add Pharo to language bindings by @theseion in https://github.com/libgit2/libgit2/pull/6310
* Add link to Tcl bindings for libgit2 by @apnadkarni in https://github.com/libgit2/libgit2/pull/6318
* fix couple of typos by @SkinnyMind in https://github.com/libgit2/libgit2/pull/6287
* update documentation for default status options by @ethomson in https://github.com/libgit2/libgit2/pull/6322

## New Contributors
* @bierbaum made their first contribution in https://github.com/libgit2/libgit2/pull/6246
* @dongcarl made their first contribution in https://github.com/libgit2/libgit2/pull/6299
* @SkinnyMind made their first contribution in https://github.com/libgit2/libgit2/pull/6287
* @zawata made their first contribution in https://github.com/libgit2/libgit2/pull/6305
* @vishwin made their first contribution in https://github.com/libgit2/libgit2/pull/6340
* @i-tengfei made their first contribution in https://github.com/libgit2/libgit2/pull/6334
* @jpalus made their first contribution in https://github.com/libgit2/libgit2/pull/6333
* @lya001 made their first contribution in https://github.com/libgit2/libgit2/pull/6348

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.4.0...v1.5.0

v1.4
----

This is release v1.4.0, "Fisematenten".  This release includes several new features and bugfixes, improves compatibility with git, and begins preparation for SHA256 support in a future release.

## What's Changed
### New features
* diff: update rename limit to 1000 to match git's behavior by @ethomson in https://github.com/libgit2/libgit2/pull/6092
* odb: support checking for object existence without refresh by @joshtriplett in https://github.com/libgit2/libgit2/pull/6107
* object: provide a low-level mechanism to validate whether a raw object is valid (`git_object_rawcontent_is_valid`) by @ethomson in https://github.com/libgit2/libgit2/pull/6128
* blob: provide a function to identify binary content by @ethomson in https://github.com/libgit2/libgit2/pull/6142
* status: add `rename_threshold` to `git_status_options`. by @arroz in https://github.com/libgit2/libgit2/pull/6158
* remote: support `http.followRedirects` (`false` and `initial`) and follow initial redirects by default by @ethomson in https://github.com/libgit2/libgit2/pull/6175
* remote: support scp style paths with ports (`[git@github.com:22]:libgit2/libgit2`) by @ethomson in https://github.com/libgit2/libgit2/pull/6167
* win32: update git for windows configuration file location compatibility by @csware in https://github.com/libgit2/libgit2/pull/6151 and @ethomson in https://github.com/libgit2/libgit2/pull/6180
* refs: speed up packed reference lookups when packed refs are sorted by @ccstolley in https://github.com/libgit2/libgit2/pull/6138
* merge: support zdiff3 conflict styles by @ethomson in https://github.com/libgit2/libgit2/pull/6195
* remote: support fetching by object id (using "+oid:ref" refspec syntax) by @ethomson in https://github.com/libgit2/libgit2/pull/6203
* merge: callers can specify virtual-base building behavior and to optionally accept conflict markers as a resolution by @boretrk in https://github.com/libgit2/libgit2/pull/6204

### Bug fixes
* Fix a gcc 11 warning in src/threadstate.c by @lhchavez in https://github.com/libgit2/libgit2/pull/6115
* Fix a gcc 11 warning in src/thread.h by @lhchavez in https://github.com/libgit2/libgit2/pull/6116
* cmake: re-enable WinHTTP by @ethomson in https://github.com/libgit2/libgit2/pull/6120
* Fix repo init when template dir is non-existent by @ammgws in https://github.com/libgit2/libgit2/pull/6106
* cmake: use project-specific root variable instead of CMAKE_SOURCE_DIR by @Qix- in https://github.com/libgit2/libgit2/pull/6146
* Better revparse compatibility for at time notation by @yoichi in https://github.com/libgit2/libgit2/pull/6095
* remotes: fix insteadOf/pushInsteadOf handling by @mkhl in https://github.com/libgit2/libgit2/pull/6101
* git_commit_summary: ignore lines with spaces by @stforek in https://github.com/libgit2/libgit2/pull/6125
* Config parsing by @csware in https://github.com/libgit2/libgit2/pull/6124
* config: handle empty conditional in includeIf by @ethomson in https://github.com/libgit2/libgit2/pull/6165
* #6154 git_status_list_new case insensitive fix by @arroz in https://github.com/libgit2/libgit2/pull/6159
* futils_mktmp: don't use umask by @boretrk in https://github.com/libgit2/libgit2/pull/6178
* revparse: support bare '@' by @ethomson in https://github.com/libgit2/libgit2/pull/6196
* odb: check for write failures by @ethomson in https://github.com/libgit2/libgit2/pull/6206
* push: Prepare pack before sending pack header. by @ccstolley in https://github.com/libgit2/libgit2/pull/6205
* mktmp: improve our temp file creation by @ethomson in https://github.com/libgit2/libgit2/pull/6207
* diff_file: fix crash if size of diffed file changes in workdir by @jorio in https://github.com/libgit2/libgit2/pull/6208
* merge: comment conflicts lines in MERGE_MSG by @ethomson in https://github.com/libgit2/libgit2/pull/6197
* Fix crashes in example programs on Windows (sprintf_s not compatible with snprintf) by @apnadkarni in https://github.com/libgit2/libgit2/pull/6212

### Code cleanups
* Introduce `git_remote_connect_options` by @ethomson in https://github.com/libgit2/libgit2/pull/6161
* hash: separate hashes and git_oid by @ethomson in https://github.com/libgit2/libgit2/pull/6082
* `git_buf`: now a public-only API (`git_str` is our internal API) by @ethomson in https://github.com/libgit2/libgit2/pull/6078
* cmake: cleanups and consistency by @ethomson in https://github.com/libgit2/libgit2/pull/6084
* path: refactor utility path functions by @ethomson in https://github.com/libgit2/libgit2/pull/6104
* str: git_str_free is never a function by @ethomson in https://github.com/libgit2/libgit2/pull/6111
* cmake refactorings by @ethomson in https://github.com/libgit2/libgit2/pull/6112
* Add missing-declarations warning globally by @ethomson in https://github.com/libgit2/libgit2/pull/6113
* cmake: further refactorings by @ethomson in https://github.com/libgit2/libgit2/pull/6114
* tag: set validity to 0 by default by @ethomson in https://github.com/libgit2/libgit2/pull/6119
* util: minor cleanup and refactoring to the date class by @ethomson in https://github.com/libgit2/libgit2/pull/6121
* Minor code cleanups by @ethomson in https://github.com/libgit2/libgit2/pull/6122
* Fix a long long that crept past by @NattyNarwhal in https://github.com/libgit2/libgit2/pull/6094
* remote: refactor insteadof application by @ethomson in https://github.com/libgit2/libgit2/pull/6147
* ntmlclient: fix linking with libressl by @boretrk in https://github.com/libgit2/libgit2/pull/6157
* c99: change single bit flags to unsigned by @boretrk in https://github.com/libgit2/libgit2/pull/6179
* Fix typos by @rex4539 in https://github.com/libgit2/libgit2/pull/6164
* diff_driver: split global_drivers array into separate elements by @boretrk in https://github.com/libgit2/libgit2/pull/6184
* cmake: disable some gnu extensions by @boretrk in https://github.com/libgit2/libgit2/pull/6185
* Disabling setting `CMAKE_FIND_LIBRARY_SUFFIXES` on Apple platforms. by @arroz in https://github.com/libgit2/libgit2/pull/6153
* C90: add inline macro to xdiff and mbedtls by @boretrk in https://github.com/libgit2/libgit2/pull/6200
* SHA256: early preparation by @ethomson in https://github.com/libgit2/libgit2/pull/6192

### CI improvements
* tests: rename test runner to `libgit2_tests`, build option to `BUILD_TESTS`. by @ethomson in https://github.com/libgit2/libgit2/pull/6083
* ci: only update docs on push by @ethomson in https://github.com/libgit2/libgit2/pull/6108
* Pedantic header test by @boretrk in https://github.com/libgit2/libgit2/pull/6086
* ci: build with ssh on nightly by @ethomson in https://github.com/libgit2/libgit2/pull/6148
* ci: improve the name in CI runs by @ethomson in https://github.com/libgit2/libgit2/pull/6198

### Documentation improvements
* Document that `git_odb` is thread-safe by @joshtriplett in https://github.com/libgit2/libgit2/pull/6109
* Improve documentation by @punkymaniac in https://github.com/libgit2/libgit2/pull/6168

### Other changes
* libgit2_clar is now libgit2_tests by @mkhl in https://github.com/libgit2/libgit2/pull/6100
* Remove PSGit from Language Bindings section of README by @cestrand in https://github.com/libgit2/libgit2/pull/6150
* COPYING: remove regex copyright, add PCRE copyright by @ethomson in https://github.com/libgit2/libgit2/pull/6187
* meta: add a release configuration file by @ethomson in https://github.com/libgit2/libgit2/pull/6211

## New Contributors
* @mkhl made their first contribution in https://github.com/libgit2/libgit2/pull/6100
* @ammgws made their first contribution in https://github.com/libgit2/libgit2/pull/6106
* @yoichi made their first contribution in https://github.com/libgit2/libgit2/pull/6095
* @stforek made their first contribution in https://github.com/libgit2/libgit2/pull/6125
* @cestrand made their first contribution in https://github.com/libgit2/libgit2/pull/6150
* @rex4539 made their first contribution in https://github.com/libgit2/libgit2/pull/6164
* @jorio made their first contribution in https://github.com/libgit2/libgit2/pull/6208

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.3.0...v1.4.0

v1.3
----

This is release v1.3.0, "Zugunruhe".  This release includes only minor new features that will be helpful for users to have an orderly transition to the v2.0 lineage.

## New Features
* Support custom git extensions by @ethomson in https://github.com/libgit2/libgit2/pull/6031
* Introduce `git_email_create`; deprecate `git_diff_format_email` by @ethomson in https://github.com/libgit2/libgit2/pull/6061

## Deprecated APIs
* `git_oidarray_free` is deprecated; callers should use `git_oidarray_dispose`

## Bug fixes
* #6028: Check if `threadstate->error_t.message` is not `git_buf__initbuf` before freeing. by @arroz in https://github.com/libgit2/libgit2/pull/6029
* remote: Mark `git_remote_name_is_valid` as `GIT_EXTERN` by @lhchavez in https://github.com/libgit2/libgit2/pull/6032
* Fix config parsing for multiline with multiple quoted comment chars by @basile-henry in https://github.com/libgit2/libgit2/pull/6043
* indexer: Avoid one `mmap(2)`/`munmap(2)` pair per `git_indexer_append` call by @lhchavez in https://github.com/libgit2/libgit2/pull/6039
* merge: Check file mode when resolving renames by @ccstolley in https://github.com/libgit2/libgit2/pull/6060
* Allow proxy options when connecting with a detached remote. by @lrm29 in https://github.com/libgit2/libgit2/pull/6058
* win32: allow empty environment variables by @ethomson in https://github.com/libgit2/libgit2/pull/6063
* Fixes for deprecated APIs by @ethomson in https://github.com/libgit2/libgit2/pull/6066
* filter: use a `git_oid` in filter options, not a pointer by @ethomson in https://github.com/libgit2/libgit2/pull/6067
* diff: update `GIT_DIFF_IGNORE_BLANK_LINES` by @ethomson in https://github.com/libgit2/libgit2/pull/6068 
* Attribute lookups are always on relative paths by @ethomson in https://github.com/libgit2/libgit2/pull/6073
* Handle long paths when querying attributes by @ethomson in https://github.com/libgit2/libgit2/pull/6075

## Code cleanups
* notes: use a buffer internally by @ethomson in https://github.com/libgit2/libgit2/pull/6047
* Fix coding style for pointer by @punkymaniac in https://github.com/libgit2/libgit2/pull/6045
* Use __typeof__ GNUC keyword for ISO C compatibility by @duncanthomson in https://github.com/libgit2/libgit2/pull/6041
* Discover libssh2 without pkg-config by @stac47 in https://github.com/libgit2/libgit2/pull/6053
* Longpath filter bug by @lrm29 in https://github.com/libgit2/libgit2/pull/6055
* Add test to ensure empty proxy env behaves like unset env by @sathieu in https://github.com/libgit2/libgit2/pull/6052
* Stdint header condition has been reverted. by @lolgear in https://github.com/libgit2/libgit2/pull/6020
* buf: `common_prefix` takes a string array by @ethomson in https://github.com/libgit2/libgit2/pull/6077
* oidarray: introduce `git_oidarray_dispose` by @ethomson in https://github.com/libgit2/libgit2/pull/6076
* examples: Free the git_config and git_config_entry after use by @257 in https://github.com/libgit2/libgit2/pull/6071

## CI Improvements
* ci: pull libssh2 from www.libssh2.org by @ethomson in https://github.com/libgit2/libgit2/pull/6064

## Documentation changes
* Update README.md by @shijinglu in https://github.com/libgit2/libgit2/pull/6050

## New Contributors
* @basile-henry made their first contribution in https://github.com/libgit2/libgit2/pull/6043
* @duncanthomson made their first contribution in https://github.com/libgit2/libgit2/pull/6041
* @stac47 made their first contribution in https://github.com/libgit2/libgit2/pull/6053
* @shijinglu made their first contribution in https://github.com/libgit2/libgit2/pull/6050
* @ccstolley made their first contribution in https://github.com/libgit2/libgit2/pull/6060
* @sathieu made their first contribution in https://github.com/libgit2/libgit2/pull/6052
* @257 made their first contribution in https://github.com/libgit2/libgit2/pull/6071

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.2.0...v1.3.0

---------------------------------------------------------------------

v1.2
-----

This is release v1.2.0, "Absacker".  This release includes many new features: in particular, support for commit graphs, multi-pack indexes, and `core.longpaths` support.

This is meant to be the final minor release in the v1 lineage.  v2.0 will be the next major release and will remove deprecated APIs and may include breaking changes.

## Deprecated APIs

* revspec: rename git_revparse_mode_t to git_revspec_t by @ethomson in https://github.com/libgit2/libgit2/pull/5786
* tree: deprecate `git_treebuilder_write_with_buffer` by @ethomson in https://github.com/libgit2/libgit2/pull/5815
* Deprecate `is_valid_name` functions; replace with `name_is_valid` functions by @ethomson in https://github.com/libgit2/libgit2/pull/5659
* filter: stop taking git_buf as user input by @ethomson in https://github.com/libgit2/libgit2/pull/5859
* remote: introduce remote_ready_cb, deprecate resolve_url callback by @ethomson in https://github.com/libgit2/libgit2/pull/6012
* Introduce `create_commit_cb`, deprecate `signing_cb` by @ethomson in https://github.com/libgit2/libgit2/pull/6016
* filter: filter drivers stop taking git_buf as user input by @ethomson in https://github.com/libgit2/libgit2/pull/6011
* buf: deprecate public git_buf writing functions by @ethomson in https://github.com/libgit2/libgit2/pull/6017

## New features

* winhttp: support optional client cert by @ianhattendorf in https://github.com/libgit2/libgit2/pull/5384
* Add support for additional SSH hostkey types. by @arroz in https://github.com/libgit2/libgit2/pull/5750
* Handle ipv6 addresses by @ethomson in https://github.com/libgit2/libgit2/pull/5741
* zlib: Add support for building with Chromium's zlib implementation by @lhchavez in https://github.com/libgit2/libgit2/pull/5748
* commit-graph: Introduce a parser for commit-graph files by @lhchavez in https://github.com/libgit2/libgit2/pull/5762
* patch: add owner accessor by @KOLANICH in https://github.com/libgit2/libgit2/pull/5731
* commit-graph: Support lookups of entries in a commit-graph by @lhchavez in https://github.com/libgit2/libgit2/pull/5763
* commit-graph: Introduce `git_commit_graph_needs_refresh()` by @lhchavez in https://github.com/libgit2/libgit2/pull/5764
* Working directory path validation by @ethomson in https://github.com/libgit2/libgit2/pull/5823
* Support `core.longpaths` on Windows by @ethomson in https://github.com/libgit2/libgit2/pull/5857
* git_reference_create_matching: Treat all-zero OID as "must be absent" by @novalis in https://github.com/libgit2/libgit2/pull/5842
* diff:add option to ignore blank line changes by @yuuri in https://github.com/libgit2/libgit2/pull/5853
* [Submodule] Git submodule dup by @lolgear in https://github.com/libgit2/libgit2/pull/5890
* commit-graph: Use the commit-graph in revwalks by @lhchavez in https://github.com/libgit2/libgit2/pull/5765
* commit-graph: Introduce `git_commit_list_generation_cmp` by @lhchavez in https://github.com/libgit2/libgit2/pull/5766
* graph: Create `git_graph_reachable_from_any()` by @lhchavez in https://github.com/libgit2/libgit2/pull/5767
* Support reading attributes from a specific commit by @ethomson in https://github.com/libgit2/libgit2/pull/5952
* [Branch] Branch upstream with format by @lolgear in https://github.com/libgit2/libgit2/pull/5861
* Dynamically load OpenSSL (optionally) by @ethomson in https://github.com/libgit2/libgit2/pull/5974
* Set refs/remotes/origin/HEAD to default branch when branch is specified by @A-Ovchinnikov-mx in https://github.com/libgit2/libgit2/pull/6010
* midx: Add a way to write multi-pack-index files by @lhchavez in https://github.com/libgit2/libgit2/pull/5404
* Use error code GIT_EAUTH for authentication failures by @josharian in https://github.com/libgit2/libgit2/pull/5395
* midx: Introduce git_odb_write_multi_pack_index() by @lhchavez in https://github.com/libgit2/libgit2/pull/5405
* Checkout dry-run by @J0Nes90 in https://github.com/libgit2/libgit2/pull/5841
* mbedTLS: Fix setting certificate directory by @mikezackles in https://github.com/libgit2/libgit2/pull/6004
* remote: introduce remote_ready_cb, deprecate resolve_url callback by @ethomson in https://github.com/libgit2/libgit2/pull/6012
* Introduce `create_commit_cb`, deprecate `signing_cb` by @ethomson in https://github.com/libgit2/libgit2/pull/6016
* commit-graph: Add a way to write commit-graph files by @lhchavez in https://github.com/libgit2/libgit2/pull/5778

## Bug fixes

* Define `git___load` when building with `-DTHREADSAFE=OFF` by @lhchavez in https://github.com/libgit2/libgit2/pull/5664
* Make the Windows leak detection more robust by @lhchavez in https://github.com/libgit2/libgit2/pull/5661
* Refactor "global" state by @ethomson in https://github.com/libgit2/libgit2/pull/5546
* threadstate: rename tlsdata when building w/o threads by @ethomson in https://github.com/libgit2/libgit2/pull/5668
* Include `${MBEDTLS_INCLUDE_DIR}` when compiling `crypt_mbedtls.c` by @staticfloat in https://github.com/libgit2/libgit2/pull/5685
* Fix the `-DTHREADSAFE=OFF` build by @lhchavez in https://github.com/libgit2/libgit2/pull/5690
* Add missing worktree_dir check and test case by @rbmclean in https://github.com/libgit2/libgit2/pull/5692
* msvc crtdbg -> win32 leakcheck by @ethomson in https://github.com/libgit2/libgit2/pull/5580
* Introduce GIT_ASSERT macros by @ethomson in https://github.com/libgit2/libgit2/pull/5327
* Also add the raw hostkey to `git_cert_hostkey` by @lhchavez in https://github.com/libgit2/libgit2/pull/5704
* Make the odb race-free by @lhchavez in https://github.com/libgit2/libgit2/pull/5595
* Make the pack and mwindow implementations data-race-free by @lhchavez in https://github.com/libgit2/libgit2/pull/5593
* Thread-free implementation by @ethomson in https://github.com/libgit2/libgit2/pull/5719
* Thread-local storage: a generic internal library (with no allocations) by @ethomson in https://github.com/libgit2/libgit2/pull/5720
* Friendlier getting started in the lack of git_libgit2_init by @ethomson in https://github.com/libgit2/libgit2/pull/5578
* Make git__strntol64() ~70%* faster by @lhchavez in https://github.com/libgit2/libgit2/pull/5735
* Cache the parsed submodule config when diffing by @lhchavez in https://github.com/libgit2/libgit2/pull/5727
* pack: continue zlib while we can make progress by @ethomson in https://github.com/libgit2/libgit2/pull/5740
* Avoid using `__builtin_mul_overflow` with the clang+32-bit combo by @lhchavez in https://github.com/libgit2/libgit2/pull/5742
* repository: use intptr_t's in the config map cache by @ethomson in https://github.com/libgit2/libgit2/pull/5746
* Build with NO_MMAP by @0xdky in https://github.com/libgit2/libgit2/pull/5583
* Add documentation for git_blob_filter_options.version by @JoshuaS3 in https://github.com/libgit2/libgit2/pull/5759
* blob: fix name of `GIT_BLOB_FILTER_ATTRIBUTES_FROM_HEAD` by @ethomson in https://github.com/libgit2/libgit2/pull/5760
* Cope with empty default branch by @ethomson in https://github.com/libgit2/libgit2/pull/5770
* README: instructions for using libgit2 without compiling by @ethomson in https://github.com/libgit2/libgit2/pull/5772
* Use `p_pwrite`/`p_pread` consistently throughout the codebase by @lhchavez in https://github.com/libgit2/libgit2/pull/5769
* midx: Fix a bug in `git_midx_needs_refresh()` by @lhchavez in https://github.com/libgit2/libgit2/pull/5768
* mwindow: Fix a bug in the LRU window finding code by @lhchavez in https://github.com/libgit2/libgit2/pull/5783
* refdb_fs: Check git_sortedcache wlock/rlock errors by @mamapanda in https://github.com/libgit2/libgit2/pull/5800
* index: Check git_vector_dup error in write_entries by @mamapanda in https://github.com/libgit2/libgit2/pull/5801
* Fix documentation formating on repository.h by @punkymaniac in https://github.com/libgit2/libgit2/pull/5806
* include: fix typos in comments by @tniessen in https://github.com/libgit2/libgit2/pull/5805
* Fix some typos by @aaronfranke in https://github.com/libgit2/libgit2/pull/5797
* Check git_signature_dup failure by @mamapanda in https://github.com/libgit2/libgit2/pull/5817
* merge: Check insert_head_ids error in create_virtual_base by @mamapanda in https://github.com/libgit2/libgit2/pull/5818
* winhttp: skip certificate check if unable to send request by @ianhattendorf in https://github.com/libgit2/libgit2/pull/5814
* Default to GIT_BRANCH_DEFAULT if init.defaultBranch is empty string by @ianhattendorf in https://github.com/libgit2/libgit2/pull/5832
* Fix diff_entrycount -> diff_num_deltas doc typo by @mjsir911 in https://github.com/libgit2/libgit2/pull/5838
* repo: specify init.defaultbranch is meant to be a branch name by @carlosmn in https://github.com/libgit2/libgit2/pull/5835
* repo: remove an inappropriate use of PASSTHROUGH by @carlosmn in https://github.com/libgit2/libgit2/pull/5834
* src: fix typos in header files by @tniessen in https://github.com/libgit2/libgit2/pull/5843
* test: clean up memory leaks by @ethomson in https://github.com/libgit2/libgit2/pull/5858
* buf: remove unnecessary buf_text namespace by @ethomson in https://github.com/libgit2/libgit2/pull/5860
* Fix bug in git_diff_find_similar. by @staktrace in https://github.com/libgit2/libgit2/pull/5839
* Fix issues with Proxy Authentication after httpclient refactor by @implausible in https://github.com/libgit2/libgit2/pull/5852
* tests: clean up memory leak, fail on leak for win32 by @ethomson in https://github.com/libgit2/libgit2/pull/5892
* Tolerate readlink size less than st_size by @dtolnay in https://github.com/libgit2/libgit2/pull/5900
* Define WINHTTP_NO_CLIENT_CERT_CONTEXT if needed by @jacquesg in https://github.com/libgit2/libgit2/pull/5929
* Update from regex to pcre licensing information in docs/contributing.md by @boretrk in https://github.com/libgit2/libgit2/pull/5916
* Consider files executable only if the user can execute them by @novalis in https://github.com/libgit2/libgit2/pull/5915
* git__timer: Limit ITimer usage to AmigaOS4 by @boretrk in https://github.com/libgit2/libgit2/pull/5936
* Fix memory leak in git_smart__connect by @punkymaniac in https://github.com/libgit2/libgit2/pull/5908
* config: fix included configs not refreshed more than once by @Batchyx in https://github.com/libgit2/libgit2/pull/5926
* Fix wrong time_t used in function by @NattyNarwhal in https://github.com/libgit2/libgit2/pull/5938
* fix check for ignoring of negate rules by @palmin in https://github.com/libgit2/libgit2/pull/5824
* Make `FIND_PACKAGE(PythonInterp)` prefer `python3` by @lhchavez in https://github.com/libgit2/libgit2/pull/5913
* git__timer: Allow compilation on systems without CLOCK_MONOTONIC by @boretrk in https://github.com/libgit2/libgit2/pull/5945
* stdintification: use int64_t and INT64_C instead of long long by @NattyNarwhal in https://github.com/libgit2/libgit2/pull/5941
* Optional stricter allocation checking (for `malloc(0)` cases) by @ethomson in https://github.com/libgit2/libgit2/pull/5951
* Variadic arguments aren't in C89 by @NattyNarwhal in https://github.com/libgit2/libgit2/pull/5948
* Fix typo in general.c by @Crayon2000 in https://github.com/libgit2/libgit2/pull/5954
* common.h: use inline when compiling for C99 and later by @boretrk in https://github.com/libgit2/libgit2/pull/5953
* Fix one memory leak in master by @lhchavez in https://github.com/libgit2/libgit2/pull/5957
* tests: reset odb backend priority by @ethomson in https://github.com/libgit2/libgit2/pull/5961
* cmake: extended futimens checking on macOS by @ethomson in https://github.com/libgit2/libgit2/pull/5962
* amiga: use ';' as path list separator on AmigaOS by @boretrk in https://github.com/libgit2/libgit2/pull/5978
* Respect the force flag on refspecs in git_remote_fetch by @alexjg in https://github.com/libgit2/libgit2/pull/5854
* Fix LIBGIT2_FILENAME not being passed to the resource compiler by @jairbubbles in https://github.com/libgit2/libgit2/pull/5994
* sha1dc: remove conditional for <sys/types.h> by @boretrk in https://github.com/libgit2/libgit2/pull/5997
* openssl: don't fail when we can't customize allocators by @ethomson in https://github.com/libgit2/libgit2/pull/5999
* C11 warnings by @boretrk in https://github.com/libgit2/libgit2/pull/6005
* open: input validation for empty segments in path by @boretrk in https://github.com/libgit2/libgit2/pull/5950
* Introduce GIT_WARN_UNUSED_RESULT by @lhchavez in https://github.com/libgit2/libgit2/pull/5802
* GCC C11 warnings by @boretrk in https://github.com/libgit2/libgit2/pull/6006
* array: check dereference from void * type by @boretrk in https://github.com/libgit2/libgit2/pull/6007
* Homogenize semantics for atomic-related functions by @lhchavez in https://github.com/libgit2/libgit2/pull/5747
* git_array_alloc: return objects of correct type by @boretrk in https://github.com/libgit2/libgit2/pull/6008
* CMake. hash sha1 header has been added. by @lolgear in https://github.com/libgit2/libgit2/pull/6013
* tests: change comments to c89 style by @boretrk in https://github.com/libgit2/libgit2/pull/6015
* Set Host Header to match CONNECT authority target by @lollipopman in https://github.com/libgit2/libgit2/pull/6022
* Fix worktree iteration when repository has no common directory by @kcsaul in https://github.com/libgit2/libgit2/pull/5943

## Documentation improvements

* Update README.md for additional Delphi bindings by @todaysoftware in https://github.com/libgit2/libgit2/pull/5831
* Fix documentation formatting by @punkymaniac in https://github.com/libgit2/libgit2/pull/5850
* docs: fix incorrect comment marker by @tiennou in https://github.com/libgit2/libgit2/pull/5897
* Patch documentation by @punkymaniac in https://github.com/libgit2/libgit2/pull/5903
* Fix misleading doc for `git_index_find` by @arxanas in https://github.com/libgit2/libgit2/pull/5910
* docs: stop mentioning libgit2's "master" branch by @Batchyx in https://github.com/libgit2/libgit2/pull/5925
* docs: fix some missing includes that cause Docurium to error out by @tiennou in https://github.com/libgit2/libgit2/pull/5917
* Patch documentation by @punkymaniac in https://github.com/libgit2/libgit2/pull/5940

## Development improvements

* WIP: .devcontainer: settings for a codespace workflow by @ethomson in https://github.com/libgit2/libgit2/pull/5508

## CI Improvements

* Add a ThreadSanitizer build by @lhchavez in https://github.com/libgit2/libgit2/pull/5597
* ci: more GitHub Actions by @ethomson in https://github.com/libgit2/libgit2/pull/5706
* ci: run coverity in the nightly builds by @ethomson in https://github.com/libgit2/libgit2/pull/5707
* ci: only report main branch in README status by @ethomson in https://github.com/libgit2/libgit2/pull/5708
* Fix the `ENABLE_WERROR=ON` build in Groovy Gorilla (gcc 10.2) by @lhchavez in https://github.com/libgit2/libgit2/pull/5715
* Re-enable the RC4 test by @carlosmn in https://github.com/libgit2/libgit2/pull/4418
* ci: run codeql by @ethomson in https://github.com/libgit2/libgit2/pull/5709
* github-actions: Also rename the main branch here by @lhchavez in https://github.com/libgit2/libgit2/pull/5771
* ci: don't use ninja on macOS by @ethomson in https://github.com/libgit2/libgit2/pull/5780
* ci: use GitHub for storing mingw-w64 build dependency by @ethomson in https://github.com/libgit2/libgit2/pull/5855
* docker: remove the entrypoint by @ethomson in https://github.com/libgit2/libgit2/pull/5980
* http: don't require a password by @ethomson in https://github.com/libgit2/libgit2/pull/5972
* ci: update nightly to use source path by @ethomson in https://github.com/libgit2/libgit2/pull/5989
* ci: add centos 7 and centos 8 by @ethomson in https://github.com/libgit2/libgit2/pull/5992
* ci: update centos builds by @ethomson in https://github.com/libgit2/libgit2/pull/5995
* ci: tag new containers with the latest tag by @ethomson in https://github.com/libgit2/libgit2/pull/6000

## Dependency updates

* ntlm: [ntlmclient](https://github.com/ethomson/ntlmclient) is now v0.9.1

**Full Changelog**: https://github.com/libgit2/libgit2/compare/v1.1.0...v1.2.0

---------------------------------------------------------------------

v1.1
----

This is release v1.1, "Fernweh".

### Changes or improvements

* Our bundled PCRE dependency has been updated to 8.44.

* The `refs/remotes/origin/HEAD` file will be created at clone time to
  point to the origin's default branch.

* libgit2 now uses the `__atomic_` intrinsics instead of `__sync_`
  intrinsics on supported gcc and clang versions.

* The `init.defaultBranch` setting is now respected and `master` is
  no longer the hardcoded as the default branch name.

* Patch files that do not contain an `index` line can now be parsed.

* Configuration files with multi-line values can now contain quotes
  split across multiple lines.

* Windows clients now attempt to use TLS1.3 when available.

* Servers that request an upgrade to a newer HTTP version are
  silently ignored instead of erroneously failing.

* Users can pass `NULL` to the options argument to
  `git_describe_commit`.

* Clones and fetches of very large packfiles now succeeds on 32-bit
  platforms.

* Custom reference database backends can now handle the repository's
  `HEAD` correctly.

* Repositories with a large number of packfiles no longer exhaust the
  number of file descriptors.

* The test framework now supports TAP output when the `-t` flag is
  specified.

* The test framework can now specify an exact match to a test
  function using a trailing `$`.

* All checkout types support `GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH`.

* `git_blame` now can ignore whitespace changes using the option
  `GIT_BLAME_IGNORE_WHITESPACE`.

* Several new examples have been created, including an examples for
  commit, add and push.

* Mode changes during rename are now supported in patch application.

* `git_checkout_head` now correctly removes untracked files in a
  subdirectory when the `FORCE | REMOVE_UNTRACKED` options are specified.

v1.0.1
------

This is a bugfix release with the following changes:

- Calculating information about renamed files during merges is more
  efficient because dissimilarity about files is now being cached and
  no longer needs to be recomputed.
  
- The `git_worktree_prune_init_options` has been correctly restored for
  backward compatibility.  In v1.0 it was incorrectly deprecated with a
  typo.

- The optional ntlmclient dependency now supports NetBSD.

- A bug where attempting to stash on a bare repository may have failed
  has been fixed.

- Configuration files that are unreadable due to permissions are now
  silently ignored, and treated as if they do not exist.  This matches
  git's behavior; previously this case would have been an error.

- v4 index files are now correctly written; previously we would read
  them correctly but would not write the prefix-compression accurately,
  causing corruption.

- A bug where the smart HTTP transport could not read large data packets
  has been fixed.  Previously, fetching from servers like Gerrit, that
  sent large data packets, would error.

---------------------------------------------------------------------

v1.0
----

This is release v1.0 "Luftschloss", which is the first stabe release of
libgit2. The API will stay compatible across all releases of the same major
version. This release includes bugfixes only and supersedes v0.99, which will
stop being maintained. Both v0.27 and v0.28 stay supported in accordance with
our release policy.

### Changes or improvements

- CMake was converted to make use of the GNUInstallDirs module for both our
  pkgconfig and install targets in favor of our custom build options
  `BIN_INSTALL_DIR`, `LIB_INSTALL_DIR` and `INCLUDE_INSTALL_DIR`. Instead, you
  can now use CMakes standard variables `CMAKE_INSTALL_BINDIR`,
  `CMAKE_INSTALL_LIBDIR` and `CMAKE_INSTALL_INCLUDEDIR`.

- Some CMake build options accepted either a specific value or a boolean value
  to disable the option altogether or use automatic detection. We only accepted
  "ON" or "OFF", but none of the other values CMake recognizes as boolean. This
  was aligned with CMake's understanding of booleans.

- The installed pkgconfig file contained incorrect values for both `libdir` and
  `includedir` variables.

- If using pcre2 for regular expressions, then we incorrectly added "pcre2"
  instead of "pcre2-8" to our pkgconfig dependencies, which was corrected.

- Fixed building the bundled ntlmclient dependency on FreeBSD, OpenBSD and
  SunOS.

- When writing symlinks on Windows, we incorrectly handled relative symlink
  targets, which was corrected.

- When using the HTTP protocol via macOS' SecureTransport implementation, reads
  could stall at the end of the session and only continue after a timeout of 60
  seconds was reached.

- The filesystem-based reference callback didn't corectly initialize the backend
  version.

- A segmentation fault was fixed when calling `git_blame_buffer()` for files
  that were modified and added to the index.

- A backwards-incompatible change was introduced when we moved some structures
  from "git2/credentials.h" into "git2/sys/credentials.h". This was fixed in the
  case where you do not use hard deprecation.

- Improved error handling in various places.


v0.99
-----

This is v0.99 "Torschlusspanik".  This will be the last minor release
before libgit2 v1.0.  We expect to only respond to bugs in this release,
to stabilize it for next major release.

It contains significant refactorings, but is expected to be API-compatible
with v0.28.0.

### Changes or improvements

* When fetching from an anonymous remote using a URL with authentication
  information provided in the URL (eg `https://foo:bar@example.com/repo`),
  we would erroneously include the literal URL in the FETCH_HEAD file.
  We now remove that to match git's behavior.

* Some credential structures, enums and values have been renamed:
  `git_cred` is now `git_credential`.  `git_credtype_t` is now
  `git_credential_t`.  Functions and types beginning with
  `git_cred_` now begin with `git_credential`, and constants beginning
  with `GIT_CREDTYPE` now begin with `GIT_CREDENTIAL`.  The former names
  are deprecated.

* Several function signatures have been changed to return an `int` to
  indicate error conditions.  We encourage you to check them for errors
  in the standard way. 

  * `git_attr_cache_flush`
  * `git_error_set_str`
  * `git_index_name_clear`
  * `git_index_reuc_clear`
  * `git_libgit2_version`
  * `git_mempack_reset`
  * `git_oid_cpy`
  * `git_oid_fmt`
  * `git_oid_fromraw`
  * `git_oid_nfmt`
  * `git_oid_pathfmt`
  * `git_remote_stop`
  * `git_remote_disconnect`
  * `git_repository__cleanup`
  * `git_repository_set_config`
  * `git_repository_set_index`
  * `git_repository_set_odb`
  * `git_repository_set_refdb`
  * `git_revwalk_reset`
  * `git_revwalk_simplify_first_parent`
  * `git_revwalk_sorting`
  * `git_treebuilder_clear`
  * `git_treebuilder_filter`

* The NTLM and Negotiate authentication mechanisms are now supported when
  talking to git implementations hosted on Apache or nginx servers.

* The `HEAD` symbolic reference can no longer be deleted.

* `git_merge_driver_source_repo` no longer returns a `const git_repository *`,
  it now returns a non-`const` `git_repository *`.

* Relative symbolic links are now supported on Windows when `core.symlinks`
  is enabled.

* Servers that provide query parameters with a redirect are now supported.

* `git_submodule_sync` will now resolve relative URLs.

* When creating git endpoint URLs, double-slashes are no longer used when
  the given git URL has a trailing slash.

* On Windows, a `DllMain` function is no longer included and thread-local
  storage has moved to fiber-local storage in order to prevent race
  conditions during shutdown.

* The tracing mechanism (`GIT_TRACE`) is now enabled by default and does
  not need to be explicitly enabled in CMake.

* The size of Git objects is now represented by `git_object_size_t`
  instead of `off_t`.

* Binary patches without data can now be parsed.

* A configuration snapshot can now be created from another configuration
  snapshot, not just a "true" configuration object.

* The `git_commit_with_signature` API will now ensure that referenced
  objects exist in the object database.

* Stash messages containing newlines will now be replaced with spaces;
  they will no longer be (erroneously) written to the repository.

* `git_commit_create_with_signature` now verifies the commit information
  to ensure that it points to a valid tree and valid parents.

* `git_apply` has an option `GIT_APPLY_CHECK` that will only do a dry-run.
  The index and working directory will remain unmodified, and application
  will report if it would have worked.

* Patches produced by Mercurial (those that lack some git extended headers)
  can now be parsed and applied.

* Reference locks are obeyed correctly on POSIX platforms, instead of
  being removed.

* Patches with empty new files can now be read and applied.

* `git_apply_to_tree` can now correctly apply patches that add new files.

* The program data configuration on Windows (`C:\ProgramData\Git\config`)
  must be owned by an administrator, a system account or the current user
  to be read.

* `git_blob_filtered_content` is now deprecated in favor of `git_blob_filter`.

* Configuration files can now be included conditionally using the
  `onbranch` conditional.

* Checkout can now properly create and remove symbolic links to directories
  on Windows.

* Stash no longer recomputes trees when committing a worktree, for
  improved performance.

* Repository templates can now include a `HEAD` file to default the
  initial default branch.

* Some configuration structures, enums and values have been renamed:
  `git_cvar_map` is now `git_configmap`, `git_cvar_t` is now
  `git_configmap_t`, `GIT_CVAR_FALSE` is now `GIT_CONFIGMAP_FALSE`,
  `GIT_CVAR_TRUE` is now `GIT_CONFIGMAP_TRUE`, `GIT_CVAR_INT32` is now
  `GIT_CONFIGMAP_INT32`, and `GIT_CVAR_STRING` is now `GIT_CONFIGMAP_STRING`.
  The former names are deprecated.

* Repositories can now be created at the root of a Windows drive.

* Configuration lookups are now more efficiently cached.

* `git_commit_create_with_signature` now supports a `NULL` signature,
  which will create a commit without adding a signature.

* When a repository lacks an `info` "common directory", we will no
  longer erroneously return `GIT_ENOTFOUND` for all attribute lookups.

* Several attribute macros have been renamed: `GIT_ATTR_TRUE` is now
  `GIT_ATTR_IS_TRUE`, `GIT_ATTR_FALSE` is now `GIT_ATTR_IS_FALSE`,
  `GIT_ATTR_UNSPECIFIED` is now `GIT_ATTR_IS_UNSPECIFIED`.  The 
  attribute enum `git_attr_t` is now `git_attr_value_t` and its
  values have been renamed: `GIT_ATTR_UNSPECIFIED_T` is now
  `GIT_ATTR_VALUE_UNSPECIFIED`, `GIT_ATTR_TRUE_T` is now
  `GIT_ATTR_VALUE_TRUE`, `GIT_ATTR_FALSE_T` is now `GIT_ATTR_VALUE_FALSE`,
  and `GIT_ATTR_VALUE_T` is now `GIT_ATTR_VALUE_STRING`.  The
  former names are deprecated.

* `git_object__size` is now `git_object_size`.  The former name is
  deprecated.

* `git_tag_create_frombuffer` is now `git_tag_create_from_buffer`.  The
  former name is deprecated.

* Several blob creation functions have been renamed:
  `git_blob_create_frombuffer` is now named `git_blob_create_from_buffer`,
  `git_blob_create_fromdisk` is now named `git_blob_create_from_disk`,
  `git_blob_create_fromworkdir` is now named `git_blob_create_from_workdir`,
  `git_blob_create_fromstream` is now named `git_blob_create_from_stream`,
  and `git_blob_create_fromstream_commit` is now named
  `git_blob_create_from_stream_commit`.  The former names are deprecated.

* The function `git_oid_iszero` is now named `git_oid_is_zero`.  The
  former name is deprecated.

* Pattern matching is now done using `wildmatch` instead of `fnmatch`
  for compatibility with git.

* The option initialization functions suffixed by `init_options` are now
  suffixed with `options_init`.  (For example, `git_checkout_init_options`
  is now `git_checkout_options_init`.)  The former names are deprecated.

* NTLM2 authentication is now supported on non-Windows platforms.

* The `git_cred_sign_callback` callback is now named `git_cred_sign_cb`.
  The `git_cred_ssh_interactive_callback` callback is now named
  `git_cred_ssh_interactive_cb`.

* Ignore files now:

  * honor escaped trailing whitespace.
  * do not incorrectly negate sibling paths of a negated pattern.
  * honor rules that stop ignoring files after a wildcard

* Attribute files now:

  * honor leading and trailing whitespace.
  * treat paths beginning with `\` as absolute only on Windows.
  * properly handle escaped characters.
  * stop reading macros defined in subdirectories

* The C locale is now correctly used when parsing regular expressions.

* The system PCRE2 or PCRE regular expression libraries are now used
  when `regcomp_l` is not available on the system.  If none of these
  are available on the system, an included version of PCRE is used.

* Wildcards in reference specifications are now supported beyond simply
  a bare wildcard (`*`) for compatibility with git.

* When `git_ignore_path_is_ignored` is provided a path with a trailing
  slash (eg, `dir/`), it will now treat it as a directory for the
  purposes of ignore matching.

* Patches that add or remove a file with a space in the path can now
  be correctly parsed.

* The `git_remote_completion_type` type is now `git_remote_completion_t`.
  The former name is deprecated.

* The `git_odb_backend_malloc` is now `git_odb_backend_data_alloc`.  The
  former name is deprecated.

* The `git_transfer_progress_cb` callback is now `git_indexer_progress_cb`
  and the `git_transfer_progress` structure is now `git_indexer_progress`.
  The former names are deprecated.

* The example projects are now contained in a single `lg2` executable
  for ease of use.

* libgit2 now correctly handles more URLs, such as
  `http://example.com:/repo.git` (colon but no port),
  `http://example.com` (no path),
  and `http://example.com:8080/` (path is /, nonstandard port).

* A carefully constructed commit object with a very large number
  of parents may lead to potential out-of-bounds writes or
  potential denial of service.

* The ProgramData configuration file is always read for compatibility
  with Git for Windows and Portable Git installations.  The ProgramData
  location is not necessarily writable only by administrators, so we
  now ensure that the configuration file is owned by the administrator
  or the current user.

### API additions

* The SSH host key now supports SHA-256 when `GIT_CERT_SSH_SHA256` is set.

* The diff format option `GIT_DIFF_FORMAT_PATCH_ID` can now be used to
  emit an output like `git patch-id`.

* The `git_apply_options_init` function will initialize a
  `git_apply_options` structure.

* The remote callbacks structure adds a `git_url_resolve_cb` callback
  that is invoked when connecting to a server, so that applications
  may edit or replace the URL before connection.

* The information about the original `HEAD` in a rebase operation is
  available with `git_rebase_orig_head_name`.  Its ID is available with
  `git_rebase_orig_head_id`.  The `onto` reference name is available with
  `git_rebase_onto_name` and its ID is available with `git_rebase_onto_id`.

* ODB backends can now free backend data when an error occurs during its
  backend data creation using `git_odb_backend_data_free`.

* Options may be specified to `git_repository_foreach_head` to control
  its behavior: `GIT_REPOSITORY_FOREACH_HEAD_SKIP_REPO` will not skip
  the main repository's HEAD reference, while
  `GIT_REPOSITORY_FOREACH_HEAD_SKIP_WORKTREES` will now skip the
  worktree HEAD references.

* The `GIT_OPT_DISABLE_PACK_KEEP_FILE_CHECKS` option can be specified to
  `git_libgit2_opts()` to avoid looking for `.keep` files that correspond
  to packfiles.  This setting can improve performance when packfiles are
  stored on high-latency filesystems like network filesystems.

* Blobs can now be filtered with `git_blob_filter`, which allows for
  options to be set with `git_blob_filter_options`, including
  `GIT_FILTER_NO_SYSTEM_ATTRIBUTES` to disable filtering with system-level
  attributes in `/etc/gitattributes` and `GIT_ATTR_CHECK_INCLUDE_HEAD` to
  enable filtering with `.gitattributes` files in the HEAD revision.

### API removals

* The unused `git_headlist_cb` function declaration was removed.

* The unused `git_time_monotonic` API is removed.

* The erroneously exported `inttypes.h` header was removed.

# Security Fixes

- CVE-2019-1348: the fast-import stream command "feature
  export-marks=path" allows writing to arbitrary file paths. As
  libgit2 does not offer any interface for fast-import, it is not
  susceptible to this vulnerability.

- CVE-2019-1349: by using NTFS 8.3 short names, backslashes or
  alternate filesystreams, it is possible to cause submodules to
  be written into pre-existing directories during a recursive
  clone using git. As libgit2 rejects cloning into non-empty
  directories by default, it is not susceptible to this
  vulnerability.

- CVE-2019-1350: recursive clones may lead to arbitrary remote
  code executing due to improper quoting of command line
  arguments. As libgit2 uses libssh2, which does not require us
  to perform command line parsing, it is not susceptible to this
  vulnerability.

- CVE-2019-1351: Windows provides the ability to substitute
  drive letters with arbitrary letters, including multi-byte
  Unicode letters. To fix any potential issues arising from
  interpreting such paths as relative paths, we have extended
  detection of DOS drive prefixes to accomodate for such cases.

- CVE-2019-1352: by using NTFS-style alternative file streams for
  the ".git" directory, it is possible to overwrite parts of the
  repository. While this has been fixed in the past for Windows,
  the same vulnerability may also exist on other systems that
  write to NTFS filesystems. We now reject any paths starting
  with ".git:" on all systems.

- CVE-2019-1353: by using NTFS-style 8.3 short names, it was
  possible to write to the ".git" directory and thus overwrite
  parts of the repository, leading to possible remote code
  execution. While this problem was already fixed in the past for
  Windows, other systems accessing NTFS filesystems are
  vulnerable to this issue too. We now enable NTFS protecions by
  default on all systems to fix this attack vector.

- CVE-2019-1354: on Windows, backslashes are not a valid part of
  a filename but are instead interpreted as directory separators.
  As other platforms allowed to use such paths, it was possible
  to write such invalid entries into a Git repository and was
  thus an attack vector to write into the ".git" dierctory. We
  now reject any entries starting with ".git\" on all systems.

- CVE-2019-1387: it is possible to let a submodule's git
  directory point into a sibling's submodule directory, which may
  result in overwriting parts of the Git repository and thus lead
  to arbitrary command execution. As libgit2 doesn't provide any
  way to do submodule clones natively, it is not susceptible to
  this vulnerability. Users of libgit2 that have implemented
  recursive submodule clones manually are encouraged to review
  their implementation for this vulnerability.

### Breaking API changes

* The "private" implementation details of the `git_cred` structure have been
  moved to a dedicated `git2/sys/cred.h` header, to clarify that the underlying
  structures are only provided for custom transport implementers.
  The breaking change is that the `username` member of the underlying struct
  is now hidden, and a new `git_cred_get_username` function has been provided.

* Some errors of class `GIT_ERROR_NET` now have class `GIT_ERROR_HTTP`.
  Most authentication failures now have error code `GIT_EAUTH` instead of `GIT_ERROR`.

### Breaking CMake configuration changes

* The CMake option to use a system http-parser library, instead of the
  bundled dependency, has changed.  This is due to a deficiency in
  http-parser that we have fixed in our implementation.  The bundled
  library is now the default, but if you wish to force the use of the
  system http-parser implementation despite incompatibilities, you can
  specify `-DUSE_HTTP_PARSER=system` to CMake.

* The interactions between `USE_HTTPS` and `SHA1_BACKEND` have been
  streamlined. The detection was moved to a new `USE_SHA1`, modeled after
  `USE_HTTPS`, which takes the values "CollisionDetection/Backend/Generic", to
  better match how the "hashing backend" is selected, the default (ON) being
  "CollisionDetection". If you were using `SHA1_BACKEND` previously, you'll
  need to check the value you've used, or switch to the autodetection.

### Authors

The following individuals provided changes that were included in this
release:

* Aaron Patterson
* Alberto Fanjul
* Anders Borum
* Augie Fackler
* Augustin Fabre
* Ayush Shridhar
* brian m. carlson
* buddyspike
* Carlos Martín Nieto
* cheese1
* Dan Skorupski
* Daniel Cohen Gindi
* Dave Lee
* David Brooks
* David Turner
* Denis Laxalde
* Dhruva Krishnamurthy
* Dominik Ritter
* Drew DeVault
* Edward Thomson
* Eric Huss
* Erik Aigner
* Etienne Samson
* Gregory Herrero
* Heiko Voigt
* Ian Hattendorf
* Jacques Germishuys
* Janardhan Pulivarthi
* Jason Haslam
* Johannes Schindelin
* Jordan Wallet
* Josh Bleecher Snyder
* kas
* kdj0c
* Laurence McGlashan
* lhchavez
* Lukas Berk
* Max Kostyukevich
* Patrick Steinhardt
* pcpthm
* Remy Suen
* Robert Coup
* romkatv
* Scott Furry
* Sebastian Henke
* Stefan Widgren
* Steve King Jr
* Sven Strickroth
* Tobias Nießen
* Tyler Ang-Wanek
* Tyler Wanek

---------------------------------------------------------------------

v0.28
-----

### Changes or improvements

* The library is now always built with cdecl calling conventions on
  Windows; the ability to build a stdcall library has been removed.

* Reference log creation now honors `core.logallrefupdates=always`.

* Fix some issues with the error-reporting in the OpenSSL backend.

* HTTP proxy support is now builtin; libcurl is no longer used to support
  proxies and is removed as a dependency.

* Certificate and credential callbacks can now return `GIT_PASSTHROUGH`
  to decline to act; libgit2 will behave as if there was no callback set
  in the first place.

* The line-ending filtering logic - when checking out files - has been
  updated to match newer git (>= git 2.9) for proper interoperability.

* Symbolic links are now supported on Windows when `core.symlinks` is set
  to `true`.

* Submodules with names which attempt to perform path traversal now have their
  configuration ignored. Such names were blindly appended to the
  `$GIT_DIR/modules` and a malicious name could lead to an attacker writing to
  an arbitrary location. This matches git's handling of CVE-2018-11235.

* Object validation is now performed during tree creation in the
  `git_index_write_tree_to` API.

* Configuration variable may now be specified on the same line as a section
  header; previously this was erroneously a parser error.

* When an HTTP server supports both NTLM and Negotiate authentication
  mechanisms, we would previously fail to authenticate with any mechanism.

* The `GIT_OPT_SET_PACK_MAX_OBJECTS` option can now set the maximum
  number of objects allowed in a packfile being downloaded; this can help
  limit the maximum memory used when fetching from an untrusted remote.

* Line numbers in diffs loaded from patch files were not being populated;
  they are now included in the results.

* The repository's index is reloaded from disk at the beginning of
  `git_merge` operations to ensure that it is up-to-date.

* Mailmap handling APIs have been introduced, and the new commit APIs
  `git_commit_committer_with_mailmap` and `git_commit_author_with_mailmap`
  will use the mailmap to resolve the committer and author information.
  In addition, blame will use the mailmap given when the
  `GIT_BLAME_USE_MAILMAP` option.

* Ignore handling for files in ignored folders would be ignored.

* Worktrees can now be backed by bare repositories.

* Trailing spaces are supported in `.gitignore` files, these spaces were
  previously (and erroneously) treated as part of the pattern.

* The library can now be built with mbedTLS support for HTTPS.

* The diff status character 'T' will now be presented by the
  `git_diff_status_char` API for diff entries that change type.

* Revision walks previously would sometimes include commits that should
  have been ignored; this is corrected.

* Revision walks are now more efficient when the output is unsorted;
  we now avoid walking all the way to the beginning of history unnecessarily.

* Error-handling around index extension loading has been fixed. We were
  previously always misreporting a truncated index (#4858).

### API additions

* The index may now be iterated atomically using `git_index_iterator`.

* Remote objects can now be created with extended options using the
  `git_remote_create_with_opts` API.

* Diff objects can now be applied as changes to the working directory,
  index or both, emulating the `git apply` command.  Additionally,
  `git_apply_to_tree` can apply those changes to a tree object as a
  fully in-memory operation.

* You can now swap out memory allocators via the
  `GIT_OPT_SET_ALLOCATOR` option with `git_libgit2_opts()`.

* You can now ensure that functions do not discard unwritten changes to the
  index via the `GIT_OPT_ENABLE_UNSAVED_INDEX_SAFETY` option to
  `git_libgit2_opts()`.  This will cause functions that implicitly re-read
  the index (eg, `git_checkout`) to fail if you have staged changes to the
  index but you have not written the index to disk.  (Unless the checkout
  has the FORCE flag specified.)

  At present, this defaults to off, but we intend to enable this more
  broadly in the future, as a warning or error.  We encourage you to
  examine your code to ensure that you are not relying on the current
  behavior that implicitly removes staged changes.

* Reference specifications can be parsed from an arbitrary string with
  the `git_refspec_parse` API.

* You can now get the name and path of worktrees using the
  `git_worktree_name` and `git_worktree_path` APIs, respectively.

* The `ref` field has been added to `git_worktree_add_options` to enable
  the creation of a worktree from a pre-existing branch.

* It's now possible to analyze merge relationships between any two
  references, not just against `HEAD`, using `git_merge_analysis_for_ref`.

### API removals

* The `git_buf_free` API is deprecated; it has been renamed to
  `git_buf_dispose` for consistency.  The `git_buf_free` API will be
  retained for backward compatibility for the foreseeable future.

* The `git_otype` enumeration and its members are deprecated and have
  been renamed for consistency.  The `GIT_OBJ_` enumeration values are
  now prefixed with `GIT_OBJECT_`.  The old enumerations and macros
  will be retained for backward compatibility for the foreseeable future.

* Several index-related APIs have been renamed for consistency.  The
  `GIT_IDXENTRY_` enumeration values and macros have been renamed to
  be prefixed with `GIT_INDEX_ENTRY_`.  The `GIT_INDEXCAP` enumeration
  values are now prefixed with `GIT_INDEX_CAPABILITY_`.  The old
  enumerations and macros will be retained for backward compatibility
  for the foreseeable future.

* The error functions and enumeration values have been renamed for
  consistency.  The `giterr_` functions and values prefix have been
  renamed to be prefixed with `git_error_`; similarly, the `GITERR_`
  constants have been renamed to be prefixed with `GIT_ERROR_`.
  The old enumerations and macros will be retained for backward
  compatibility for the foreseeable future.

### Breaking API changes

* The default checkout strategy changed from `DRY_RUN` to `SAFE` (#4531).

* Adding a symlink as .gitmodules into the index from the workdir or checking
  out such files is not allowed as this can make a Git implementation write
  outside of the repository and bypass the fsck checks for CVE-2018-11235.

---------------------------------------------------------------------

v0.27
---------

### Changes or improvements

* Improved `p_unlink` in `posix_w32.c` to try and make a file writable
  before sleeping in the retry loop to prevent unnecessary calls to sleep.

* The CMake build infrastructure has been improved to speed up building time.

* A new CMake option "-DUSE_HTTPS=<backend>" makes it possible to explicitly
  choose an HTTP backend.

* A new CMake option "-DSHA1_BACKEND=<backend>" makes it possible to explicitly
  choose an SHA1 backend. The collision-detecting backend is now the default.

* A new CMake option "-DUSE_BUNDLED_ZLIB" makes it possible to explicitly use
  the bundled zlib library.

* A new CMake option "-DENABLE_REPRODUCIBLE_BUILDS" makes it possible to
  generate a reproducible static archive. This requires support from your
  toolchain.

* The minimum required CMake version has been bumped to 2.8.11.

* Writing to a configuration file now preserves the case of the key given by the
  caller for the case-insensitive portions of the key (existing sections are
  used even if they don't match).

* We now support conditional includes in configuration files.

* Fix for handling re-reading of configuration files with includes.

* Fix for reading patches which contain exact renames only.

* Fix for reading patches with whitespace in the compared files' paths.

* We will now fill `FETCH_HEAD` from all passed refspecs instead of overwriting
  with the last one.

* There is a new diff option, `GIT_DIFF_INDENT_HEURISTIC` which activates a
  heuristic which takes into account whitespace and indentation in order to
  produce better diffs when dealing with ambiguous diff hunks.

* Fix for pattern-based ignore rules where files ignored by a rule cannot be
  un-ignored by another rule.

* Sockets opened by libgit2 are now being closed on exec(3) if the platform
  supports it.

* Fix for peeling annotated tags from packed-refs files.

* Fix reading huge loose objects from the object database.

* Fix files not being treated as modified when only the file mode has changed.

* We now explicitly reject adding submodules to the index via
  `git_index_add_frombuffer`.

* Fix handling of `GIT_DIFF_FIND_RENAMES_FROM_REWRITES` raising `SIGABRT` when
  one file has been deleted and another file has been rewritten.

* Fix for WinHTTP not properly handling NTLM and Negotiate challenges.

* When using SSH-based transports, we now repeatedly ask for the passphrase to
  decrypt the private key in case a wrong passphrase is being provided.

* When generating conflict markers, they will now use the same line endings as
  the rest of the file.

### API additions

* The `git_merge_file_options` structure now contains a new setting,
  `marker_size`.  This allows users to set the size of markers that
  delineate the sides of merged files in the output conflict file.
  By default this is 7 (`GIT_MERGE_CONFLICT_MARKER_SIZE`), which
  produces output markers like `<<<<<<<` and `>>>>>>>`.

* `git_remote_create_detached()` creates a remote that is not associated
  to any repository (and does not apply configuration like 'insteadof' rules).
  This is mostly useful for e.g. emulating `git ls-remote` behavior.

* `git_diff_patchid()` lets you generate patch IDs for diffs.

* `git_status_options` now has an additional field `baseline` to allow creating
  status lists against different trees.

* New family of functions to allow creating notes for a specific notes commit
  instead of for a notes reference.

* New family of functions to allow parsing message trailers. This API is still
  experimental and may change in future releases.

### API removals

### Breaking API changes

* Signatures now distinguish between +0000 and -0000 UTC offsets.

* The certificate check callback in the WinHTTP transport will now receive the
  `message_cb_payload` instead of the `cred_acquire_payload`.

* We are now reading symlinked directories under .git/refs.

* We now refuse creating branches named "HEAD".

* We now refuse reading and writing all-zero object IDs into the
  object database.

* We now read the effective user's configuration file instead of the real user's
  configuration in case libgit2 runs as part of a setuid binary.

* The `git_odb_open_rstream` function and its `readstream` callback in the
  `git_odb_backend` interface have changed their signatures to allow providing
  the object's size and type to the caller.

---------------------------------------------------------------------
    
v0.26
-----

### Changes or improvements

* Support for opening, creating and modifying worktrees.

* We can now detect SHA1 collisions resulting from the SHAttered attack. These
  checks can be enabled at build time via `-DUSE_SHA1DC`.

* Fix for missing implementation of `git_merge_driver_source` getters.

* Fix for installed pkg-config file being broken when the prefix contains
  spaces.

* We now detect when the hashsum of on-disk objects does not match their
  expected hashsum.

* We now support open-ended ranges (e.g. "master..", "...master") in our
  revision range parsing code.

* We now correctly compute ignores with leading "/" in subdirectories.

* We now optionally call `fsync` on loose objects, packfiles and their indexes,
  loose references and packed reference files.

* We can now build against OpenSSL v1.1 and against LibreSSL.

* `GIT_MERGE_OPTIONS_INIT` now includes a setting to perform rename detection.
  This aligns this structure with the default by `git_merge` and
  `git_merge_trees` when `NULL` was provided for the options.

* Improvements for reading index v4 files.

* Perform additional retries for filesystem operations on Windows when files
  are temporarily locked by other processes.

### API additions

* New family of functions to handle worktrees:

    * `git_worktree_list()` lets you look up worktrees for a repository.
    * `git_worktree_lookup()` lets you get a specific worktree.
    * `git_worktree_open_from_repository()` lets you get the associated worktree
      of a repository.
      a worktree.
    * `git_worktree_add` lets you create new worktrees.
    * `git_worktree_prune` lets you remove worktrees from disk.
    * `git_worktree_lock()` and `git_worktree_unlock()` let you lock
      respectively unlock a worktree.
    * `git_repository_open_from_worktree()` lets you open a repository via
    * `git_repository_head_for_worktree()` lets you get the current `HEAD` for a
      linked worktree.
    * `git_repository_head_detached_for_worktree()` lets you check whether a
      linked worktree is in detached HEAD mode.

* `git_repository_item_path()` lets you retrieve paths for various repository
  files.

* `git_repository_commondir()` lets you retrieve the common directory of a
  repository.

* `git_branch_is_checked_out()` allows you to check whether a branch is checked
  out in a repository or any of its worktrees.

* `git_repository_submodule_cache_all()` and
  `git_repository_submodule_cache_clear()` functions allow you to prime or clear
  the submodule cache of a repository.

* You can disable strict hash verifications via the
  `GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION` option with `git_libgit2_opts()`.

* You can enable us calling `fsync` for various files inside the ".git"
  directory by setting the `GIT_OPT_ENABLE_FSYNC_GITDIR` option with
  `git_libgit2_opts()`.

* You can now enable "offset deltas" when creating packfiles and negotiating
  packfiles with a remote server by setting `GIT_OPT_ENABLE_OFS_DELTA` option
  with `GIT_libgit2_opts()`.

* You can now set the default share mode on Windows for opening files using
  `GIT_OPT_SET_WINDOWS_SHAREMODE` option with `git_libgit2_opts()`.
  You can query the current share mode with `GIT_OPT_GET_WINDOWS_SHAREMODE`.

* `git_transport_smart_proxy_options()' enables you to get the proxy options for
  smart transports.

* The `GIT_FILTER_INIT` macro and the `git_filter_init` function are provided
  to initialize a `git_filter` structure.

### Breaking API changes

* `clone_checkout_strategy` has been removed from
  `git_submodule_update_option`. The checkout strategy used to clone will
  be the same strategy specified in `checkout_opts`.

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

---------------------------------------------------------------------

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

---------------------------------------------------------------------

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

---------------------------------------------------------------------

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

* `git_reference_*()` functions use mmap() + binary search for packed
  refs lookups when using the fs backend. Previously all entries were
  read into a hashtable, which could be slow for repositories with a
  large number of refs.
