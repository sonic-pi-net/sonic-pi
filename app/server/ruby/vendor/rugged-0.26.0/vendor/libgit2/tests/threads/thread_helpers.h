#include "thread-utils.h"

void run_in_parallel(
	int repeats,
	int threads,
	void *(*func)(void *),
	void (*before_test)(void),
	void (*after_test)(void));
