/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oid.h"

#include "git2/oid.h"
#include "repository.h"
#include "threadstate.h"
#include <string.h>
#include <limits.h>

static char to_hex[] = "0123456789abcdef";

static int oid_error_invalid(const char *msg)
{
	git_error_set(GIT_ERROR_INVALID, "unable to parse OID - %s", msg);
	return -1;
}

int git_oid_fromstrn(git_oid *out, const char *str, size_t length)
{
	size_t p;
	int v;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(str);

	if (!length)
		return oid_error_invalid("too short");

	if (length > GIT_OID_HEXSZ)
		return oid_error_invalid("too long");

	memset(out->id, 0, GIT_OID_RAWSZ);

	for (p = 0; p < length; p++) {
		v = git__fromhex(str[p]);
		if (v < 0)
			return oid_error_invalid("contains invalid characters");

		out->id[p / 2] |= (unsigned char)(v << (p % 2 ? 0 : 4));
	}

	return 0;
}

int git_oid_fromstrp(git_oid *out, const char *str)
{
	return git_oid_fromstrn(out, str, strlen(str));
}

int git_oid_fromstr(git_oid *out, const char *str)
{
	return git_oid_fromstrn(out, str, GIT_OID_HEXSZ);
}

GIT_INLINE(char) *fmt_one(char *str, unsigned int val)
{
	*str++ = to_hex[val >> 4];
	*str++ = to_hex[val & 0xf];
	return str;
}

int git_oid_nfmt(char *str, size_t n, const git_oid *oid)
{
	size_t i, max_i;

	if (!oid) {
		memset(str, 0, n);
		return 0;
	}
	if (n > GIT_OID_HEXSZ) {
		memset(&str[GIT_OID_HEXSZ], 0, n - GIT_OID_HEXSZ);
		n = GIT_OID_HEXSZ;
	}

	max_i = n / 2;

	for (i = 0; i < max_i; i++)
		str = fmt_one(str, oid->id[i]);

	if (n & 1)
		*str++ = to_hex[oid->id[i] >> 4];

	return 0;
}

int git_oid_fmt(char *str, const git_oid *oid)
{
	return git_oid_nfmt(str, GIT_OID_HEXSZ, oid);
}

int git_oid_pathfmt(char *str, const git_oid *oid)
{
	size_t i;

	str = fmt_one(str, oid->id[0]);
	*str++ = '/';
	for (i = 1; i < sizeof(oid->id); i++)
		str = fmt_one(str, oid->id[i]);

	return 0;
}

char *git_oid_tostr_s(const git_oid *oid)
{
	char *str = GIT_THREADSTATE->oid_fmt;
	git_oid_nfmt(str, GIT_OID_HEXSZ + 1, oid);
	return str;
}

char *git_oid_allocfmt(const git_oid *oid)
{
	char *str = git__malloc(GIT_OID_HEXSZ + 1);
	if (!str)
		return NULL;
	git_oid_nfmt(str, GIT_OID_HEXSZ + 1, oid);
	return str;
}

char *git_oid_tostr(char *out, size_t n, const git_oid *oid)
{
	if (!out || n == 0)
		return "";

	if (n > GIT_OID_HEXSZ + 1)
		n = GIT_OID_HEXSZ + 1;

	git_oid_nfmt(out, n - 1, oid); /* allow room for terminating NUL */
	out[n - 1] = '\0';

	return out;
}

int git_oid__parse(
	git_oid *oid, const char **buffer_out,
	const char *buffer_end, const char *header)
{
	const size_t sha_len = GIT_OID_HEXSZ;
	const size_t header_len = strlen(header);

	const char *buffer = *buffer_out;

	if (buffer + (header_len + sha_len + 1) > buffer_end)
		return -1;

	if (memcmp(buffer, header, header_len) != 0)
		return -1;

	if (buffer[header_len + sha_len] != '\n')
		return -1;

	if (git_oid_fromstr(oid, buffer + header_len) < 0)
		return -1;

	*buffer_out = buffer + (header_len + sha_len + 1);

	return 0;
}

void git_oid__writebuf(git_buf *buf, const char *header, const git_oid *oid)
{
	char hex_oid[GIT_OID_HEXSZ];

	git_oid_fmt(hex_oid, oid);
	git_buf_puts(buf, header);
	git_buf_put(buf, hex_oid, GIT_OID_HEXSZ);
	git_buf_putc(buf, '\n');
}

int git_oid_fromraw(git_oid *out, const unsigned char *raw)
{
	memcpy(out->id, raw, sizeof(out->id));
	return 0;
}

int git_oid_cpy(git_oid *out, const git_oid *src)
{
	memcpy(out->id, src->id, sizeof(out->id));
	return 0;
}

int git_oid_cmp(const git_oid *a, const git_oid *b)
{
	return git_oid__cmp(a, b);
}

int git_oid_equal(const git_oid *a, const git_oid *b)
{
	return (git_oid__cmp(a, b) == 0);
}

int git_oid_ncmp(const git_oid *oid_a, const git_oid *oid_b, size_t len)
{
	const unsigned char *a = oid_a->id;
	const unsigned char *b = oid_b->id;

	if (len > GIT_OID_HEXSZ)
		len = GIT_OID_HEXSZ;

	while (len > 1) {
		if (*a != *b)
			return 1;
		a++;
		b++;
		len -= 2;
	};

	if (len)
		if ((*a ^ *b) & 0xf0)
			return 1;

	return 0;
}

int git_oid_strcmp(const git_oid *oid_a, const char *str)
{
	const unsigned char *a;
	unsigned char strval;
	int hexval;

	for (a = oid_a->id; *str && (a - oid_a->id) < GIT_OID_RAWSZ; ++a) {
		if ((hexval = git__fromhex(*str++)) < 0)
			return -1;
		strval = (unsigned char)(hexval << 4);
		if (*str) {
			if ((hexval = git__fromhex(*str++)) < 0)
				return -1;
			strval |= hexval;
		}
		if (*a != strval)
			return (*a - strval);
	}

	return 0;
}

int git_oid_streq(const git_oid *oid_a, const char *str)
{
	return git_oid_strcmp(oid_a, str) == 0 ? 0 : -1;
}

int git_oid_is_zero(const git_oid *oid_a)
{
	const unsigned char *a = oid_a->id;
	unsigned int i;
	for (i = 0; i < GIT_OID_RAWSZ; ++i, ++a)
		if (*a != 0)
			return 0;
	return 1;
}

#ifndef GIT_DEPRECATE_HARD
int git_oid_iszero(const git_oid *oid_a)
{
	return git_oid_is_zero(oid_a);
}
#endif

typedef short node_index;

typedef union {
	const char *tail;
	node_index children[16];
} trie_node;

struct git_oid_shorten {
	trie_node *nodes;
	size_t node_count, size;
	int min_length, full;
};

static int resize_trie(git_oid_shorten *self, size_t new_size)
{
	self->nodes = git__reallocarray(self->nodes, new_size, sizeof(trie_node));
	GIT_ERROR_CHECK_ALLOC(self->nodes);

	if (new_size > self->size) {
		memset(&self->nodes[self->size], 0x0, (new_size - self->size) * sizeof(trie_node));
	}

	self->size = new_size;
	return 0;
}

static trie_node *push_leaf(git_oid_shorten *os, node_index idx, int push_at, const char *oid)
{
	trie_node *node, *leaf;
	node_index idx_leaf;

	if (os->node_count >= os->size) {
		if (resize_trie(os, os->size * 2) < 0)
			return NULL;
	}

	idx_leaf = (node_index)os->node_count++;

	if (os->node_count == SHRT_MAX) {
		os->full = 1;
        return NULL;
    }

	node = &os->nodes[idx];
	node->children[push_at] = -idx_leaf;

	leaf = &os->nodes[idx_leaf];
	leaf->tail = oid;

	return node;
}

git_oid_shorten *git_oid_shorten_new(size_t min_length)
{
	git_oid_shorten *os;

	GIT_ASSERT_ARG_WITH_RETVAL((size_t)((int)min_length) == min_length, NULL);

	os = git__calloc(1, sizeof(git_oid_shorten));
	if (os == NULL)
		return NULL;

	if (resize_trie(os, 16) < 0) {
		git__free(os);
		return NULL;
	}

	os->node_count = 1;
	os->min_length = (int)min_length;

	return os;
}

void git_oid_shorten_free(git_oid_shorten *os)
{
	if (os == NULL)
		return;

	git__free(os->nodes);
	git__free(os);
}


/*
 * What wizardry is this?
 *
 * This is just a memory-optimized trie: basically a very fancy
 * 16-ary tree, which is used to store the prefixes of the OID
 * strings.
 *
 * Read more: http://en.wikipedia.org/wiki/Trie
 *
 * Magic that happens in this method:
 *
 *	- Each node in the trie is an union, so it can work both as
 *	a normal node, or as a leaf.
 *
 *	- Each normal node points to 16 children (one for each possible
 *	character in the oid). This is *not* stored in an array of
 *	pointers, because in a 64-bit arch this would be sucking
 *	16*sizeof(void*) = 128 bytes of memory per node, which is
 *	insane. What we do is store Node Indexes, and use these indexes
 *	to look up each node in the om->index array. These indexes are
 *	signed shorts, so this limits the amount of unique OIDs that
 *	fit in the structure to about 20000 (assuming a more or less uniform
 *	distribution).
 *
 *	- All the nodes in om->index array are stored contiguously in
 *	memory, and each of them is 32 bytes, so we fit 2x nodes per
 *	cache line. Convenient for speed.
 *
 *	- To differentiate the leafs from the normal nodes, we store all
 *	the indexes towards a leaf as a negative index (indexes to normal
 *	nodes are positives). When we find that one of the children for
 *	a node has a negative value, that means it's going to be a leaf.
 *	This reduces the amount of indexes we have by two, but also reduces
 *	the size of each node by 1-4 bytes (the amount we would need to
 *	add a `is_leaf` field): this is good because it allows the nodes
 *	to fit cleanly in cache lines.
 *
 *	- Once we reach an empty children, instead of continuing to insert
 *	new nodes for each remaining character of the OID, we store a pointer
 *	to the tail in the leaf; if the leaf is reached again, we turn it
 *	into a normal node and use the tail to create a new leaf.
 *
 *	This is a pretty good balance between performance and memory usage.
 */
int git_oid_shorten_add(git_oid_shorten *os, const char *text_oid)
{
	int i;
	bool is_leaf;
	node_index idx;

	if (os->full) {
		git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
		return -1;
	}

	if (text_oid == NULL)
		return os->min_length;

	idx = 0;
	is_leaf = false;

	for (i = 0; i < GIT_OID_HEXSZ; ++i) {
		int c = git__fromhex(text_oid[i]);
		trie_node *node;

		if (c == -1) {
			git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - invalid hex value");
			return -1;
		}

		node = &os->nodes[idx];

		if (is_leaf) {
			const char *tail;

			tail = node->tail;
			node->tail = NULL;

			node = push_leaf(os, idx, git__fromhex(tail[0]), &tail[1]);
			if (node == NULL) {
				if (os->full)
					git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
				return -1;
			}
		}

		if (node->children[c] == 0) {
			if (push_leaf(os, idx, c, &text_oid[i + 1]) == NULL) {
				if (os->full)
					git_error_set(GIT_ERROR_INVALID, "unable to shorten OID - OID set full");
				return -1;
			}
			break;
		}

		idx = node->children[c];
		is_leaf = false;

		if (idx < 0) {
			node->children[c] = idx = -idx;
			is_leaf = true;
		}
	}

	if (++i > os->min_length)
		os->min_length = i;

	return os->min_length;
}

