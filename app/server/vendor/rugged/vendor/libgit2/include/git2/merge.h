/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_merge_h__
#define INCLUDE_git_merge_h__

#include "common.h"
#include "types.h"
#include "oid.h"
#include "checkout.h"
#include "index.h"

/**
 * @file git2/merge.h
 * @brief Git merge routines
 * @defgroup git_merge Git merge routines
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/**
 * The file inputs to `git_merge_file`.  Callers should populate the
 * `git_merge_file_input` structure with descriptions of the files in
 * each side of the conflict for use in producing the merge file.
 */
typedef struct {
	unsigned int version;

	/** Pointer to the contents of the file. */
	const char *ptr;

	/** Size of the contents pointed to in `ptr`. */
	size_t size;

	/** File name of the conflicted file, or `NULL` to not merge the path. */
	const char *path;

	/** File mode of the conflicted file, or `0` to not merge the mode. */
	unsigned int mode;
} git_merge_file_input;

#define GIT_MERGE_FILE_INPUT_VERSION 1
#define GIT_MERGE_FILE_INPUT_INIT {GIT_MERGE_FILE_INPUT_VERSION}

/**
 * Initializes a `git_merge_file_input` with default values. Equivalent to
 * creating an instance with GIT_MERGE_FILE_INPUT_INIT.
 *
 * @param opts the `git_merge_file_input` instance to initialize.
 * @param version the version of the struct; you should pass
 *        `GIT_MERGE_FILE_INPUT_VERSION` here.
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_merge_file_init_input(
	git_merge_file_input *opts,
	unsigned int version);

/**
 * Flags for `git_merge_tree` options.  A combination of these flags can be
 * passed in via the `flags` value in the `git_merge_options`.
 */
typedef enum {
	/**
	 * Detect renames that occur between the common ancestor and the "ours"
	 * side or the common ancestor and the "theirs" side.  This will enable
	 * the ability to merge between a modified and renamed file.
	 */
	GIT_MERGE_TREE_FIND_RENAMES = (1 << 0),
} git_merge_tree_flag_t;

/**
 * Merge file favor options for `git_merge_options` instruct the file-level
 * merging functionality how to deal with conflicting regions of the files.
 */
typedef enum {
	/**
	 * When a region of a file is changed in both branches, a conflict
	 * will be recorded in the index so that `git_checkout` can produce
	 * a merge file with conflict markers in the working directory.
	 * This is the default.
	 */
	GIT_MERGE_FILE_FAVOR_NORMAL = 0,

	/**
	 * When a region of a file is changed in both branches, the file
	 * created in the index will contain the "ours" side of any conflicting
	 * region.  The index will not record a conflict.
	 */
	GIT_MERGE_FILE_FAVOR_OURS = 1,

	/**
	 * When a region of a file is changed in both branches, the file
	 * created in the index will contain the "theirs" side of any conflicting
	 * region.  The index will not record a conflict.
	 */
	GIT_MERGE_FILE_FAVOR_THEIRS = 2,

	/**
	 * When a region of a file is changed in both branches, the file
	 * created in the index will contain each unique line from each side,
	 * which has the result of combining both files.  The index will not
	 * record a conflict.
	 */
	GIT_MERGE_FILE_FAVOR_UNION = 3,
} git_merge_file_favor_t;

typedef enum {
	/* Defaults */
	GIT_MERGE_FILE_DEFAULT = 0,

	/* Create standard conflicted merge files */
	GIT_MERGE_FILE_STYLE_MERGE = (1 << 0),

	/* Create diff3-style files */
	GIT_MERGE_FILE_STYLE_DIFF3 = (1 << 1),

	/* Condense non-alphanumeric regions for simplified diff file */
	GIT_MERGE_FILE_SIMPLIFY_ALNUM = (1 << 2),
} git_merge_file_flags_t;

typedef struct {
	unsigned int version;

	/**
	 * Label for the ancestor file side of the conflict which will be prepended
	 * to labels in diff3-format merge files.
	 */
	const char *ancestor_label;

	/**
	 * Label for our file side of the conflict which will be prepended
	 * to labels in merge files.
	 */
	const char *our_label;

	/**
	 * Label for their file side of the conflict which will be prepended
	 * to labels in merge files.
	 */
	const char *their_label;

	/** The file to favor in region conflicts. */
	git_merge_file_favor_t favor;

	/** Merge file flags. */
	git_merge_file_flags_t flags;
} git_merge_file_options;

#define GIT_MERGE_FILE_OPTIONS_VERSION 1
#define GIT_MERGE_FILE_OPTIONS_INIT {GIT_MERGE_FILE_OPTIONS_VERSION}

/**
 * Initializes a `git_merge_file_options` with default values. Equivalent to
 * creating an instance with GIT_MERGE_FILE_OPTIONS_INIT.
 *
 * @param opts the `git_merge_file_options` instance to initialize.
 * @param version the version of the struct; you should pass
 *        `GIT_MERGE_FILE_OPTIONS_VERSION` here.
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_merge_file_init_options(
	git_merge_file_options *opts,
	unsigned int version);

typedef struct {
	/**
	 * True if the output was automerged, false if the output contains
	 * conflict markers.
	 */
	unsigned int automergeable;

	/**
	 * The path that the resultant merge file should use, or NULL if a
	 * filename conflict would occur.
	 */
	const char *path;

	/** The mode that the resultant merge file should use.  */
	unsigned int mode;

	/** The contents of the merge. */
	const char *ptr;

	/** The length of the merge contents. */
	size_t len;
} git_merge_file_result;

typedef struct {
	unsigned int version;
	git_merge_tree_flag_t flags;

	/**
	 * Similarity to consider a file renamed (default 50).  If
	 * `GIT_MERGE_TREE_FIND_RENAMES` is enabled, added files will be compared
	 * with deleted files to determine their similarity.  Files that are
	 * more similar than the rename threshold (percentage-wise) will be
	 * treated as a rename.
	 */
	unsigned int rename_threshold;

	/**
	 * Maximum similarity sources to examine for renames (default 200).
	 * If the number of rename candidates (add / delete pairs) is greater
	 * than this value, inexact rename detection is aborted.
	 *
	 * This setting overrides the `merge.renameLimit` configuration value.
	 */
	unsigned int target_limit;

	/** Pluggable similarity metric; pass NULL to use internal metric */
	git_diff_similarity_metric *metric;

	/** Flags for handling conflicting content. */
	git_merge_file_favor_t file_favor;
} git_merge_options;

#define GIT_MERGE_OPTIONS_VERSION 1
#define GIT_MERGE_OPTIONS_INIT {GIT_MERGE_OPTIONS_VERSION}

/**
 * Initializes a `git_merge_options` with default values. Equivalent to
 * creating an instance with GIT_MERGE_OPTIONS_INIT.
 *
 * @param opts the `git_merge_options` instance to initialize.
 * @param version the version of the struct; you should pass
 *        `GIT_MERGE_OPTIONS_VERSION` here.
 * @return Zero on success; -1 on failure.
 */
GIT_EXTERN(int) git_merge_init_options(
	git_merge_options *opts,
	unsigned int version);

/**
 * The results of `git_merge_analysis` indicate the merge opportunities.
 */
typedef enum {
	/** No merge is possible.  (Unused.) */
	GIT_MERGE_ANALYSIS_NONE = 0,

	/**
	 * A "normal" merge; both HEAD and the given merge input have diverged
	 * from their common ancestor.  The divergent commits must be merged.
	 */
	GIT_MERGE_ANALYSIS_NORMAL = (1 << 0),

	/**
	 * All given merge inputs are reachable from HEAD, meaning the
	 * repository is up-to-date and no merge needs to be performed.
	 */
	GIT_MERGE_ANALYSIS_UP_TO_DATE = (1 << 1),

	/**
	 * The given merge input is a fast-forward from HEAD and no merge
	 * needs to be performed.  Instead, the client can check out the
	 * given merge input.
	 */
	GIT_MERGE_ANALYSIS_FASTFORWARD = (1 << 2),

	/**
	 * The HEAD of the current repository is "unborn" and does not point to
	 * a valid commit.  No merge can be performed, but the caller may wish
	 * to simply set HEAD to the target commit(s).
	 */
	GIT_MERGE_ANALYSIS_UNBORN = (1 << 3),
} git_merge_analysis_t;

/**
 * Analyzes the given branch(es) and determines the opportunities for
 * merging them into the HEAD of the repository.
 *
 * @param analysis_out analysis enumeration that the result is written into
 * @param repo the repository to merge
 * @param their_heads the heads to merge into
 * @param their_heads_len the number of heads to merge
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_analysis(
	git_merge_analysis_t *analysis_out,
	git_repository *repo,
	const git_merge_head **their_heads,
	size_t their_heads_len);

/**
 * Find a merge base between two commits
 *
 * @param out the OID of a merge base between 'one' and 'two'
 * @param repo the repository where the commits exist
 * @param one one of the commits
 * @param two the other commit
 * @return 0 on success, GIT_ENOTFOUND if not found or error code
 */
GIT_EXTERN(int) git_merge_base(
	git_oid *out,
	git_repository *repo,
	const git_oid *one,
	const git_oid *two);

/**
 * Find a merge base given a list of commits
 *
 * @param out the OID of a merge base considering all the commits
 * @param repo the repository where the commits exist
 * @param length The number of commits in the provided `input_array`
 * @param input_array oids of the commits
 * @return Zero on success; GIT_ENOTFOUND or -1 on failure.
 */
GIT_EXTERN(int) git_merge_base_many(
	git_oid *out,
	git_repository *repo,
	size_t length,
	const git_oid input_array[]);

/**
 * Find a merge base in preparation for an octopus merge
 *
 * @param out the OID of a merge base considering all the commits
 * @param repo the repository where the commits exist
 * @param length The number of commits in the provided `input_array`
 * @param input_array oids of the commits
 * @return Zero on success; GIT_ENOTFOUND or -1 on failure.
 */
GIT_EXTERN(int) git_merge_base_octopus(
	git_oid *out,
	git_repository *repo,
	size_t length,
	const git_oid input_array[]);

/**
 * Creates a `git_merge_head` from the given reference.  The resulting
 * git_merge_head must be freed with `git_merge_head_free`.
 *
 * @param out pointer to store the git_merge_head result in
 * @param repo repository that contains the given reference
 * @param ref reference to use as a merge input
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_head_from_ref(
	git_merge_head **out,
	git_repository *repo,
	const git_reference *ref);

/**
 * Creates a `git_merge_head` from the given fetch head data.  The resulting
 * git_merge_head must be freed with `git_merge_head_free`.
 *
 * @param out pointer to store the git_merge_head result in
 * @param repo repository that contains the given commit
 * @param branch_name name of the (remote) branch
 * @param remote_url url of the remote
 * @param oid the commit object id to use as a merge input
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_head_from_fetchhead(
	git_merge_head **out,
	git_repository *repo,
	const char *branch_name,
	const char *remote_url,
	const git_oid *oid);

/**
 * Creates a `git_merge_head` from the given commit id.  The resulting
 * git_merge_head must be freed with `git_merge_head_free`.
 *
 * @param out pointer to store the git_merge_head result in
 * @param repo repository that contains the given commit
 * @param id the commit object id to use as a merge input
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_head_from_id(
	git_merge_head **out,
	git_repository *repo,
	const git_oid *id);

/**
 * Gets the commit ID that the given `git_merge_head` refers to.
 *
 * @param id pointer to commit id to be filled in
 * @param head the given merge head
 */
GIT_EXTERN(const git_oid *) git_merge_head_id(
	const git_merge_head *head);

/**
 * Frees a `git_merge_head`.
 *
 * @param head merge head to free
 */
GIT_EXTERN(void) git_merge_head_free(
	git_merge_head *head);

/**
 * Merge two files as they exist in the in-memory data structures, using
 * the given common ancestor as the baseline, producing a
 * `git_merge_file_result` that reflects the merge result.  The
 * `git_merge_file_result` must be freed with `git_merge_file_result_free`.
 *
 * Note that this function does not reference a repository and any
 * configuration must be passed as `git_merge_file_options`.
 *
 * @param out The git_merge_file_result to be filled in
 * @param ancestor The contents of the ancestor file
 * @param ours The contents of the file in "our" side
 * @param theirs The contents of the file in "their" side
 * @param opts The merge file options or `NULL` for defaults
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_file(
	git_merge_file_result *out,
	const git_merge_file_input *ancestor,
	const git_merge_file_input *ours,
	const git_merge_file_input *theirs,
	const git_merge_file_options *opts);

/**
 * Merge two files as they exist in the index, using the given common
 * ancestor as the baseline, producing a `git_merge_file_result` that
 * reflects the merge result.  The `git_merge_file_result` must be freed with
 * `git_merge_file_result_free`.
 *
 * @param out The git_merge_file_result to be filled in
 * @param repo The repository
 * @param ancestor The index entry for the ancestor file (stage level 1)
 * @param our_path The index entry for our file (stage level 2)
 * @param their_path The index entry for their file (stage level 3)
 * @param opts The merge file options or NULL
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_file_from_index(
	git_merge_file_result *out,
	git_repository *repo,
	const git_index_entry *ancestor,
	const git_index_entry *ours,
	const git_index_entry *theirs,
	const git_merge_file_options *opts);

/**
 * Frees a `git_merge_file_result`.
 *
 * @param result The result to free or `NULL`
 */
GIT_EXTERN(void) git_merge_file_result_free(git_merge_file_result *result);

/**
 * Merge two trees, producing a `git_index` that reflects the result of
 * the merge.  The index may be written as-is to the working directory
 * or checked out.  If the index is to be converted to a tree, the caller
 * should resolve any conflicts that arose as part of the merge.
 *
 * The returned index must be freed explicitly with `git_index_free`.
 *
 * @param out pointer to store the index result in
 * @param repo repository that contains the given trees
 * @param ancestor_tree the common ancestor between the trees (or null if none)
 * @param our_tree the tree that reflects the destination tree
 * @param their_tree the tree to merge in to `our_tree`
 * @param opts the merge tree options (or null for defaults)
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_trees(
	git_index **out,
	git_repository *repo,
	const git_tree *ancestor_tree,
	const git_tree *our_tree,
	const git_tree *their_tree,
	const git_merge_options *opts);

/**
 * Merge two commits, producing a `git_index` that reflects the result of
 * the merge.  The index may be written as-is to the working directory
 * or checked out.  If the index is to be converted to a tree, the caller
 * should resolve any conflicts that arose as part of the merge.
 *
 * The returned index must be freed explicitly with `git_index_free`.
 *
 * @param out pointer to store the index result in
 * @param repo repository that contains the given trees
 * @param our_commit the commit that reflects the destination tree
 * @param their_commit the commit to merge in to `our_commit`
 * @param opts the merge tree options (or null for defaults)
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge_commits(
	git_index **out,
	git_repository *repo,
	const git_commit *our_commit,
	const git_commit *their_commit,
	const git_merge_options *opts);

/**
 * Merges the given commit(s) into HEAD, writing the results into the working
 * directory.  Any changes are staged for commit and any conflicts are written
 * to the index.  Callers should inspect the repository's index after this
 * completes, resolve any conflicts and prepare a commit.
 *
 * @param repo the repository to merge
 * @param merge_heads the heads to merge into
 * @param merge_heads_len the number of heads to merge
 * @param merge_opts merge options
 * @param checkout_opts checkout options
 * @return 0 on success or error code
 */
GIT_EXTERN(int) git_merge(
	git_repository *repo,
	const git_merge_head **their_heads,
	size_t their_heads_len,
	const git_merge_options *merge_opts,
	const git_checkout_options *checkout_opts);

/** @} */
GIT_END_DECL
#endif
