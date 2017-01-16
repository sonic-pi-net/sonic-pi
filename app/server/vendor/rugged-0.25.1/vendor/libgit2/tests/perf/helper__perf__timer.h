#if defined(GIT_WIN32)

struct perf__timer
{
	LARGE_INTEGER sum;
	LARGE_INTEGER time_started;
};

#define PERF_TIMER_INIT {0}

#else

struct perf__timer
{
	uint32_t sum;
	uint32_t time_started;
};

#define PERF_TIMER_INIT {0}

#endif

typedef struct perf__timer perf_timer;

void perf__timer__start(perf_timer *t);
void perf__timer__stop(perf_timer *t);
void perf__timer__report(perf_timer *t, const char *fmt, ...);
