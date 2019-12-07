/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_win32_h__
#define INCLUDE_hash_win32_h__

#include "common.h"
#include "hash.h"

#include <wincrypt.h>
#include <strsafe.h>

enum hash_win32_prov_type {
	INVALID = 0,
	CRYPTOAPI,
	CNG
};

/*
 * CryptoAPI is available for hashing on Windows XP and newer.
 */

struct hash_cryptoapi_prov {
	HCRYPTPROV handle;
};

/*
 * CNG (bcrypt.dll) is significantly more performant than CryptoAPI and is
 * preferred, however it is only available on Windows 2008 and newer and
 * must therefore be dynamically loaded, and we must inline constants that
 * would not exist when building in pre-Windows 2008 environments.
 */

#define GIT_HASH_CNG_DLL_NAME           "bcrypt.dll"

/* BCRYPT_SHA1_ALGORITHM */
#define GIT_HASH_CNG_HASH_TYPE          L"SHA1"

/* BCRYPT_OBJECT_LENGTH */
#define GIT_HASH_CNG_HASH_OBJECT_LEN    L"ObjectLength"

/* BCRYPT_HASH_REUSEABLE_FLAGS */
#define GIT_HASH_CNG_HASH_REUSABLE      0x00000020

/* Function declarations for CNG */
typedef NTSTATUS (WINAPI *hash_win32_cng_open_algorithm_provider_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ *phAlgorithm,
	LPCWSTR pszAlgId,
	LPCWSTR pszImplementation,
	DWORD dwFlags);

typedef NTSTATUS (WINAPI *hash_win32_cng_get_property_fn)(
	HANDLE /* BCRYPT_HANDLE */ hObject,
	LPCWSTR pszProperty,
	PUCHAR pbOutput,
	ULONG cbOutput,
	ULONG *pcbResult,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *hash_win32_cng_create_hash_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ hAlgorithm,
	HANDLE /* BCRYPT_HASH_HANDLE */ *phHash,
	PUCHAR pbHashObject, ULONG cbHashObject,
	PUCHAR pbSecret,
	ULONG cbSecret,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *hash_win32_cng_finish_hash_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash,
	PUCHAR pbOutput,
	ULONG cbOutput,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *hash_win32_cng_hash_data_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash,
	PUCHAR pbInput,
	ULONG cbInput,
	ULONG dwFlags);

typedef NTSTATUS (WINAPI *hash_win32_cng_destroy_hash_fn)(
	HANDLE /* BCRYPT_HASH_HANDLE */ hHash);

typedef NTSTATUS (WINAPI *hash_win32_cng_close_algorithm_provider_fn)(
	HANDLE /* BCRYPT_ALG_HANDLE */ hAlgorithm,
	ULONG dwFlags);

struct hash_cng_prov {
	/* DLL for CNG */
	HINSTANCE dll;

	/* Function pointers for CNG */
	hash_win32_cng_open_algorithm_provider_fn open_algorithm_provider;
	hash_win32_cng_get_property_fn get_property;
	hash_win32_cng_create_hash_fn create_hash;
	hash_win32_cng_finish_hash_fn finish_hash;
	hash_win32_cng_hash_data_fn hash_data;
	hash_win32_cng_destroy_hash_fn destroy_hash;
	hash_win32_cng_close_algorithm_provider_fn close_algorithm_provider;

	HANDLE /* BCRYPT_ALG_HANDLE */ handle;
	DWORD hash_object_size;
};

struct git_hash_prov {
	enum hash_win32_prov_type type;

	union {
		struct hash_cryptoapi_prov cryptoapi;
		struct hash_cng_prov cng;
	} prov;
};

/* Hash contexts */

struct hash_cryptoapi_ctx {
	bool valid;
	HCRYPTHASH hash_handle;
};

struct hash_cng_ctx {
	bool updated;
	HANDLE /* BCRYPT_HASH_HANDLE */ hash_handle;
	PBYTE hash_object;
};

struct git_hash_ctx {
	enum hash_win32_prov_type type;
	git_hash_prov *prov;

	union {
		struct hash_cryptoapi_ctx cryptoapi;
		struct hash_cng_ctx cng;
	} ctx;
};

#endif /* INCLUDE_hash_openssl_h__ */
