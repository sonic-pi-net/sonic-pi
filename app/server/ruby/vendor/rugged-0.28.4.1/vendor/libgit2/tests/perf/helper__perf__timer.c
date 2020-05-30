#include "clar_libgit2.h"
#include "helper__perf__timer.h"

#if defined(GIT_WIN32)

void perf__timer__start(perf_timer *t)
{
	QueryPerformanceCounter(&t->time_started);
}

void perf__timer__stop(perf_timer *t)
{
	LARGE_INTEGER time_now;
	QueryPerformanceCounter(&time_now);

	t->sum.QuadPart += (time_now.QuadPart - t->time_started.QuadPart);
}

void perf__timer__report(perf_timer *t, const char *fmt, ...)
{
	va_list arglist;
	LARGE_INTEGER freq;
	double fraction;

	QueryPerformanceFrequency(&freq);

	fraction = ((double)t->sum.QuadPart) / ((double)freq.QuadPart);

	printf("%10.3f: ", fraction);

	va_start(arglist, fmt);
	vprintf(fmt, arglist);
	va_end(arglist);

	printf("\n");
}

#else

#include <sys/time.h>

static uint32_t now_in_ms(void)
{
	struct timeval now;
	gettimeofday(&now, NULL);
	return (uint32_t)((now.tv_sec * 1000) + (now.tv_usec / 1000));
}

void perf__timer__start(perf_timer *t)
{
	t->time_started = now_in_ms();
}

void perf__timer__stop(perf_timer *t)
{
	uint32_t now = now_in_ms();
	t->sum += (now - t->time_started);
}

void perf__timer__report(perf_timer *t, const char *fmt, ...)
{
	va_list arglist;

	printf("%10.3f: ", ((double)t->sum) / 1000);

	va_start(arglist, fmt);
	vprintf(fmt, arglist);
	va_end(arglist);

	printf("\n");
}

#endif
