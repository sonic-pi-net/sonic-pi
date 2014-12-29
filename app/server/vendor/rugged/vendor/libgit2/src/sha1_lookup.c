/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>

#include "sha1_lookup.h"
#include "common.h"
#include "oid.h"

/*
 * Conventional binary search loop looks like this:
 *
 *	unsigned lo, hi;
 *		do {
 *				unsigned mi = (lo + hi) / 2;
 *				int cmp = "entry pointed at by mi" minus "target";
 *				if (!cmp)
 *						return (mi is the wanted one)
 *				if (cmp > 0)
 *						hi = mi; "mi is larger than target"
 *				else
 *						lo = mi+1; "mi is smaller than target"
 *		} while (lo < hi);
 *
 * The invariants are:
 *
 * - When entering the loop, lo points at a slot that is never
 *	above the target (it could be at the target), hi points at a
 *	slot that is guaranteed to be above the target (it can never
 *	be at the target).
 *
 * - We find a point 'mi' between lo and hi (mi could be the same
 *	as lo, but never can be as same as hi), and check if it hits
 *	the target. There are three cases:
 *
 *	- if it is a hit, we are happy.
 *
 *	- if it is strictly higher than the target, we set it to hi,
 *		and repeat the search.
 *
 *	- if it is strictly lower than the target, we update lo to
 *		one slot after it, because we allow lo to be at the target.
 *
 *	If the loop exits, there is no matching entry.
 *
 * When choosing 'mi', we do not have to take the "middle" but
 * anywhere in between lo and hi, as long as lo <= mi < hi is
 * satisfied. When we somehow know that the distance between the
 * target and lo is much shorter than the target and hi, we could
 * pick mi that is much closer to lo than the midway.
 *
 * Now, we can take advantage of the fact that SHA-1 is a good hash
 * function, and as long as there are enough entries in the table, we
 * can expect uniform distribution. An entry that begins with for
 * example "deadbeef..." is much likely to appear much later than in
 * the midway of the table. It can reasonably be expected to be near
 * 87% (222/256) from the top of the table.
 *
 * However, we do not want to pick "mi" too precisely. If the entry at
 * the 87% in the above example turns out to be higher than the target
 * we are looking for, we would end up narrowing the search space down
 * only by 13%, instead of 50% we would get if we did a simple binary
 * search. So we would want to hedge our bets by being less aggressive.
 *
 * The table at "table" holds at least "nr" entries of "elem_size"
 * bytes each. Each entry has the SHA-1 key at "key_offset". The
 * table is sorted by the SHA-1 key of the entries. The caller wants
 * to find the entry with "key", and knows that the entry at "lo" is
 * not higher than the entry it is looking for, and that the entry at
 * "hi" is higher than the entry it is looking for.
 */
int sha1_entry_pos(const void *table,
			size_t elem_size,
			size_t key_offset,
			unsigned lo, unsigned hi, unsigned nr,
			const unsigned char *key)
{
	const unsigned char *base = (const unsigned char*)table;
	const unsigned char *hi_key, *lo_key;
	unsigned ofs_0;

	if (!nr || lo >= hi)
		return -1;

	if (nr == hi)
		hi_key = NULL;
	else
		hi_key = base + elem_size * hi + key_offset;
	lo_key = base + elem_size * lo + key_offset;

	ofs_0 = 0;
	do {
		int cmp;
		unsigned ofs, mi, range;
		unsigned lov, hiv, kyv;
		const unsigned char *mi_key;

		range = hi - lo;
		if (hi_key) {
			for (ofs = ofs_0; ofs < 20; ofs++)
				if (lo_key[ofs] != hi_key[ofs])
					break;
			ofs_0 = ofs;
			/*
			 * byte 0 thru (ofs-1) are the same between
			 * lo and hi; ofs is the first byte that is
			 * different.
			 *
			 * If ofs==20, then no bytes are different,
			 * meaning we have entries with duplicate
			 * keys. We know that we are in a solid run
			 * of this entry (because the entries are
			 * sorted, and our lo and hi are the same,
			 * there can be nothing but this single key
			 * in between). So we can stop the search.
			 * Either one of these entries is it (and
			 * we do not care which), or we do not have
			 * it.
			 *
			 * Furthermore, we know that one of our
			 * endpoints must be the edge of the run of
			 * duplicates. For example, given this
			 * sequence:
			 *
			 *     idx 0 1 2 3 4 5
			 *     key A C C C C D
			 *
			 * If we are searching for "B", we might
			 * hit the duplicate run at lo=1, hi=3
			 * (e.g., by first mi=3, then mi=0). But we
			 * can never have lo > 1, because B < C.
			 * That is, if our key is less than the
			 * run, we know that "lo" is the edge, but
			 * we can say nothing of "hi". Similarly,
			 * if our key is greater than the run, we
			 * know that "hi" is the edge, but we can
			 * say nothing of "lo".
			 *
			 * Therefore if we do not find it, we also
			 * know where it would go if it did exist:
			 * just on the far side of the edge that we
			 * know about.
			 */
			if (ofs == 20) {
				mi = lo;
				mi_key = base + elem_size * mi + key_offset;
				cmp = memcmp(mi_key, key, 20);
				if (!cmp)
					return mi;
				if (cmp < 0)
					return -1 - hi;
				else
					return -1 - lo;
			}

			hiv = hi_key[ofs_0];
			if (ofs_0 < 19)
				hiv = (hiv << 8) | hi_key[ofs_0+1];
		} else {
			hiv = 256;
			if (ofs_0 < 19)
				hiv <<= 8;
		}
		lov = lo_key[ofs_0];
		kyv = key[ofs_0];
		if (ofs_0 < 19) {
			lov = (lov << 8) | lo_key[ofs_0+1];
			kyv = (kyv << 8) | key[ofs_0+1];
		}
		assert(lov < hiv);

		if (kyv < lov)
			return -1 - lo;
		if (hiv < kyv)
			return -1 - hi;

		/*
		 * Even if we know the target is much closer to 'hi'
		 * than 'lo', if we pick too precisely and overshoot
		 * (e.g. when we know 'mi' is closer to 'hi' than to
		 * 'lo', pick 'mi' that is higher than the target), we
		 * end up narrowing the search space by a smaller
		 * amount (i.e. the distance between 'mi' and 'hi')
		 * than what we would have (i.e. about half of 'lo'
		 * and 'hi'). Hedge our bets to pick 'mi' less
		 * aggressively, i.e. make 'mi' a bit closer to the
		 * middle than we would otherwise pick.
		 */
		kyv = (kyv * 6 + lov + hiv) / 8;
		if (lov < hiv - 1) {
			if (kyv == lov)
				kyv++;
			else if (kyv == hiv)
				kyv--;
		}
		mi = (range - 1) * (kyv - lov) / (hiv - lov) + lo;

#ifdef INDEX_DEBUG_LOOKUP
		printf("lo %u hi %u rg %u mi %u ", lo, hi, range, mi);
		printf("ofs %u lov %x, hiv %x, kyv %x\n",
				ofs_0, lov, hiv, kyv);
#endif

		if (!(lo <= mi && mi < hi)) {
			giterr_set(GITERR_INVALID, "Assertion failure. Binary search invariant is false");
			return -1;
		}

		mi_key = base + elem_size * mi + key_offset;
		cmp = memcmp(mi_key + ofs_0, key + ofs_0, 20 - ofs_0);
		if (!cmp)
			return mi;
		if (cmp > 0) {
			hi = mi;
			hi_key = mi_key;
		} else {
			lo = mi + 1;
			lo_key = mi_key + elem_size;
		}
	} while (lo < hi);
	return -((int)lo)-1;
}

int sha1_position(const void *table,
			size_t stride,
			unsigned lo, unsigned hi,
			const unsigned char *key)
{
	const unsigned char *base = table;

	do {
		unsigned mi = (lo + hi) / 2;
		int cmp = git_oid__hashcmp(base + mi * stride, key);

		if (!cmp)
			return mi;

		if (cmp > 0)
			hi = mi;
		else
			lo = mi+1;
	} while (lo < hi);

	return -((int)lo)-1;
}
