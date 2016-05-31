/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#nodef GITERR_CHECK_ALLOC(ptr) if (ptr == NULL) { __coverity_panic__(); }
#nodef GITERR_CHECK_ALLOC_BUF(buf) if (buf == NULL || git_buf_oom(buf)) { __coverity_panic__(); }

#nodef GITERR_CHECK_ALLOC_ADD(out, one, two) \
	if (GIT_ADD_SIZET_OVERFLOW(out, one, two)) { __coverity_panic__(); }

#nodef GITERR_CHECK_ALLOC_ADD3(out, one, two, three) \
	if (GIT_ADD_SIZET_OVERFLOW(out, one, two) || \
		GIT_ADD_SIZET_OVERFLOW(out, *(out), three)) { __coverity_panic__(); }

#nodef GITERR_CHECK_ALLOC_ADD4(out, one, two, three, four) \
	if (GIT_ADD_SIZET_OVERFLOW(out, one, two) || \
		GIT_ADD_SIZET_OVERFLOW(out, *(out), three) || \
		GIT_ADD_SIZET_OVERFLOW(out, *(out), four)) { __coverity_panic__(); }

#nodef GITERR_CHECK_ALLOC_MULTIPLY(out, nelem, elsize) \
	if (GIT_MULTIPLY_SIZET_OVERFLOW(out, nelem, elsize)) { __coverity_panic__(); }

#nodef GITERR_CHECK_VERSION(S,V,N) if (giterr__check_version(S,V,N) < 0)  { __coverity_panic__(); }

#nodef LOOKS_LIKE_DRIVE_PREFIX(S) (strlen(S) >= 2 && git__isalpha((S)[0]) && (S)[1] == ':')

#nodef git_vector_foreach(v, iter, elem)	\
	for ((iter) = 0; (v)->contents != NULL && (iter) < (v)->length && ((elem) = (v)->contents[(iter)], 1); (iter)++ )

#nodef git_vector_rforeach(v, iter, elem)	\
	for ((iter) = (v)->length - 1; (v)->contents != NULL && (iter) < SIZE_MAX && ((elem) = (v)->contents[(iter)], 1); (iter)-- )
