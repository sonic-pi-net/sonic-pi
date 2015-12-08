#include "clar_libgit2.h"
#include "clar_libgit2_timer.h"
#include "buffer.h"

void cl_perf_timer__init(cl_perf_timer *t)
{
	memset(t, 0, sizeof(cl_perf_timer));
}

void cl_perf_timer__start(cl_perf_timer *t)
{
	t->time_started = git__timer();
}

void cl_perf_timer__stop(cl_perf_timer *t)
{
	double time_now = git__timer();

	t->last = time_now - t->time_started;
	t->sum += t->last;
}

double cl_perf_timer__last(const cl_perf_timer *t)
{
	return t->last;
}

double cl_perf_timer__sum(const cl_perf_timer *t)
{
	return t->sum;
}
