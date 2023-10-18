Memory allocation and ownership
-------------------------------

Any library needs to _take_ data from users, and then _return_ data to
users.  With some types this is simple - integer parameters and return
types are trivial.  But with more complex data types, things are more
complicated.  Even something seemingly simple, like a C string, requires
discipline: we cannot simple return an allocated hunk of memory for
callers to `free`, since some systems have multiple allocators and users
cannot necessarily reason about the allocator used and which corresponding
deallocation function to call to free the memory.

## Objects

Most types in libgit2 are "opaque" types, which we treat as "objects" (even
though C is "not an object oriented language").  You may create an object -
for example, with `git_odb_new`, or libgit2 may return you an object as an
"out" parameter - for example, with `git_repository_open`.  With any of
these objects, you should call their corresponding `_free` function (for
example, `git_odb_free` or `git_repository_free`) when you are done using
them.

## Structures

libgit2 will often take _input_ as structures (for example, options
structures like `git_merge_options`).  Rarely, libgit2 will return data in
a structure.  This is typically used for simpler data types, like `git_buf`
and `git_strarray`.  Users should allocate the structure themselves (either
on the stack or the heap) and pass a pointer to it.  Since libgit2 does not
allocate the structure itself, only the data inside of it, the deallocation
functions are suffixed with `_dispose` instead of `_free`, since they only
free the data _inside_ the structure.

## Strings or continuous memory buffers (`git_buf`)

libgit2 typically _takes_ NUL-terminated strings ("C strings") with a
`const char *`, and typically _takes_ raw data with a `const char *` and a
corresponding `size_t` for its size.  libgit2 typically _returns_ strings
or raw data in a `git_buf` structure.  The given data buffer will always be
NUL terminated (even if it contains binary data) and the `size` member will
always contain the size (in bytes) of the contents of the pointer (excluding
the NUL terminator).

In other words, if a `git_buf` contains the string `foo` then the memory
buffer will be { `f`, `o`, `o`, `\0` } and the size will be `3`.

Callers _must_ initialize the buffer with `GIT_BUF_INIT` (or by setting
all the members to `0`) when it is created, before passing a pointer
to the buffer to libgit2 for the first time.

Subsequent calls to libgit2 APIs that take a buffer can re-use a
buffer that was previously used.  The buffer will be cleared and grown
to accommodate the new contents (if necessary).  The new data will
written into the buffer, overwriting the previous contents.  This
allows callers to reduce the number of allocations performed by the
library.

Callers must call `git_buf_dispose` when they have finished.

Note that the deprecated `git_diff_format_email` API does not follow
this behavior; subsequent calls will concatenate data to the buffer
instead of rewriting it.  Users should move to the new `git_email`
APIs that follow the `git_buf` standards.
