#ifndef __CLAR_LIBGIT2_TIMER__
#define __CLAR_LIBGIT2_TIMER__

struct cl_perf_timer
{
	/* cumulative running time across all start..stop intervals */
	uint64_t sum;

	/* value of last start..stop interval */
	uint64_t last;

	/* clock value at start */
	uint64_t time_started;
};

#define CL_PERF_TIMER_INIT {0}

typedef struct cl_perf_timer cl_perf_timer;

void cl_perf_timer__init(cl_perf_timer *t);
void cl_perf_timer__start(cl_perf_timer *t);
void cl_perf_timer__stop(cl_perf_timer *t);

/**
 * return value of last start..stop interval in seconds.
 */
uint64_t cl_perf_timer__last(const cl_perf_timer *t);

/**
 * return cumulative running time across all start..stop
 * intervals in seconds.
 */
uint64_t cl_perf_timer__sum(const cl_perf_timer *t);

#endif /* __CLAR_LIBGIT2_TIMER__ */
