Diff is broken into four phases:

1. Building a list of things that have changed.  These changes are called
   deltas (git_diff_delta objects) and are grouped into a git_diff_list.
2. Applying file similarity measurement for rename and copy detection (and
   to potentially split files that have changed radically).  This step is
   optional.
3. Computing the textual diff for each delta.  Not all deltas have a
   meaningful textual diff.  For those that do, the textual diff can
   either be generated on the fly and passed to output callbacks or can be
   turned into a git_diff_patch object.
4. Formatting the diff and/or patch into standard text formats (such as
   patches, raw lists, etc).

In the source code, step 1 is implemented in `src/diff.c`, step 2 in
`src/diff_tform.c`, step 3 in `src/diff_patch.c`, and step 4 in
`src/diff_print.c`.  Additionally, when it comes to accessing file
content, everything goes through diff drivers that are implemented in
`src/diff_driver.c`.

External Objects
----------------

* `git_diff_options` represents user choices about how a diff should be
  performed and is passed to most diff generating functions.
* `git_diff_file` represents an item on one side of a possible delta
* `git_diff_delta` represents a pair of items that have changed in some
  way - it contains two `git_diff_file` plus a status and other stuff.
* `git_diff_list` is a list of deltas along with information about how
  those particular deltas were found.
* `git_diff_patch` represents the actual diff between a pair of items.  In
  some cases, a delta may not have a corresponding patch, if the objects
  are binary, for example.  The content of a patch will be a set of hunks
  and lines.
* A `hunk` is range of lines described by a `git_diff_range` (i.e.  "lines
  10-20 in the old file became lines 12-23 in the new").  It will have a
  header that compactly represents that information, and it will have a
  number of lines of context surrounding added and deleted lines.
* A `line` is simple a line of data along with a `git_diff_line_t` value
  that tells how the data should be interpreted (e.g. context or added).

Internal Objects
----------------

* `git_diff_file_content` is an internal structure that represents the
  data on one side of an item to be diffed; it is an augmented
  `git_diff_file` with more flags and the actual file data.

    * it is created from a repository plus a) a git_diff_file, b) a git_blob,
   or c) raw data and size
    * there are three main operations on git_diff_file_content:
    
        * _initialization_ sets up the data structure and does what it can up to,
          but not including loading and looking at the actual data
        * _loading_ loads the data, preprocesses it (i.e. applies filters) and
          potentially analyzes it (to decide if binary)
        * _free_ releases loaded data and frees any allocated memory

* The internal structure of a `git_diff_patch` stores the actual diff
  between a pair of `git_diff_file_content` items

    * it may be "unset" if the items are not diffable
    * "empty" if the items are the same
    * otherwise it will consist of a set of hunks each of which covers some
      number of lines of context, additions and deletions
    * a patch is created from two git_diff_file_content items
    * a patch is fully instantiated in three phases:
    
        * initial creation and initialization
        * loading of data and preliminary data examination
        * diffing of data and optional storage of diffs
    * (TBD) if a patch is asked to store the diffs and the size of the diff
      is significantly smaller than the raw data of the two sides, then the
      patch may be flattened using a pool of string data

* `git_diff_output` is an internal structure that represents an output
  target for a `git_diff_patch`
    * It consists of file, hunk, and line callbacks, plus a payload
    * There is a standard flattened output that can be used for plain text output
    * Typically we use a `git_xdiff_output` which drives the callbacks via the
      xdiff code taken from core Git.

* `git_diff_driver` is an internal structure that encapsulates the logic
  for a given type of file
    * a driver is looked up based on the name and mode of a file.
    * the driver can then be used to:
        * determine if a file is binary (by attributes, by git_diff_options
          settings, or by examining the content)
        * give you a function pointer that is used to evaluate function context
          for hunk headers
    * At some point, the logic for getting a filtered version of file content
      or calculating the OID of a file may be moved into the driver.
