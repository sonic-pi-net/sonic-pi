#include "git2/sys/filter.h"

extern git_filter *create_bitflip_filter(void);
extern git_filter *create_reverse_filter(const char *attr);

extern int bitflip_filter_apply(
	git_filter     *self,
	void          **payload,
	git_buf        *to,
	const git_buf  *from,
	const git_filter_source *source);

extern int reverse_filter_apply(
	git_filter     *self,
	void          **payload,
	git_buf        *to,
	const git_buf  *from,
	const git_filter_source *source);
