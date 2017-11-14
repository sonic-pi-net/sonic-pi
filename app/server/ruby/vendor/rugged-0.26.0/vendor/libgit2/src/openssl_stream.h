/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_openssl_stream_h__
#define INCLUDE_openssl_stream_h__

#include "common.h"

#include "git2/sys/stream.h"

extern int git_openssl_stream_global_init(void);

extern int git_openssl_stream_new(git_stream **out, const char *host, const char *port);

/*
 * OpenSSL 1.1 made BIO opaque so we have to use functions to interact with it
 * which do not exist in previous versions. We define these inline functions so
 * we can program against the interface instead of littering the implementation
 * with ifdefs.
 */
#ifdef GIT_OPENSSL
# include <openssl/ssl.h>
# include <openssl/err.h>
# include <openssl/x509v3.h>
# include <openssl/bio.h>



# if OPENSSL_VERSION_NUMBER < 0x10100000L || defined(LIBRESSL_VERSION_NUMBER)

GIT_INLINE(BIO_METHOD*) BIO_meth_new(int type, const char *name)
{
	BIO_METHOD *meth = git__calloc(1, sizeof(BIO_METHOD));
	if (!meth) {
		return NULL;
	}

	meth->type = type;
	meth->name = name;

	return meth;
}

GIT_INLINE(void) BIO_meth_free(BIO_METHOD *biom)
{
	git__free(biom);
}

GIT_INLINE(int) BIO_meth_set_write(BIO_METHOD *biom, int (*write) (BIO *, const char *, int))
{
	biom->bwrite = write;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_read(BIO_METHOD *biom, int (*read) (BIO *, char *, int))
{
	biom->bread = read;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_puts(BIO_METHOD *biom, int (*puts) (BIO *, const char *))
{
	biom->bputs = puts;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_gets(BIO_METHOD *biom, int (*gets) (BIO *, char *, int))

{
	biom->bgets = gets;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_ctrl(BIO_METHOD *biom, long (*ctrl) (BIO *, int, long, void *))
{
	biom->ctrl = ctrl;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_create(BIO_METHOD *biom, int (*create) (BIO *))
{
	biom->create = create;
	return 1;
}

GIT_INLINE(int) BIO_meth_set_destroy(BIO_METHOD *biom, int (*destroy) (BIO *))
{
	biom->destroy = destroy;
	return 1;
}

GIT_INLINE(int) BIO_get_new_index(void)
{
	/* This exists as of 1.1 so before we'd just have 0 */
	return 0;
}

GIT_INLINE(void) BIO_set_init(BIO *b, int init)
{
	b->init = init;
}

GIT_INLINE(void) BIO_set_data(BIO *a, void *ptr)
{
	a->ptr = ptr;
}

GIT_INLINE(void*) BIO_get_data(BIO *a)
{
	return a->ptr;
}

GIT_INLINE(const unsigned char *) ASN1_STRING_get0_data(const ASN1_STRING *x)
{
	return ASN1_STRING_data((ASN1_STRING *)x);
}

# endif // OpenSSL < 1.1
#endif // GIT_OPENSSL

#endif
