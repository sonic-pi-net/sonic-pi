#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <limits.h>
#include <string.h>
#include "tlsf.h"

#define NUM_MALLOC      (10000)
#define SIZE_MALLOC     (10000)
#define POOL_SIZE	(NUM_MALLOC*SIZE_MALLOC)

#define	FILL_MEM	(0)

typedef struct
{
  const char *name;
  double min;
  double max;
  double n;
  double sum;
  double sumquad;
} minmax;

static double
getcurtime (void)
{
  struct timeval tv;
  struct timezone tz;

  gettimeofday (&tv, &tz);
  return ((tv.tv_usec / 1000000.0) + (1.0 * tv.tv_sec));
}

static void
init (minmax * data)
{
  data->name = "";
  data->min = 1e37;
  data->max = 0.0;
  data->n = 0.0;
  data->sum = 0.0;
  data->sumquad = 0.0;
}

static void
update (int print, const char *n, double s, double e, minmax * data)
{
  double d = e - s;
  double mean, sigma;

  if (n) {
    data->name = n;
    data->n++;
    data->sum += d;
    data->sumquad += d * d;
    mean = data->sum / data->n;
    sigma = sqrt ((data->sumquad - ((data->sum * data->sum) / data->n)) /
		  (data->n));
    if (d < data->min) {
      data->min = d;
    }
    if (d > data->max) {
      data->max = d;
    }
  }
  else {
    mean = data->sum / data->n;
    sigma = sqrt ((data->sumquad - ((data->sum * data->sum) / data->n)) /
		  (data->n));
  }
  if (print) {
    printf ("%s min=%.9f, max=%.9f, mean=%.9f, sigma=%.9f\n",
	    n ? n : data->name, data->min, data->max, mean, sigma);
    if (n == NULL) {
      data->min = 1e37;
      data->max = 0.0;
    }
  }
}

static char pool[POOL_SIZE];

int
main (void)
{
  int i, free_mem;
  int j;
  size_t t;
  int n = 0;
  double s, first, last_h;
  void **m;
  minmax maldata, raldata, freedata;

  free_mem = init_memory_pool (POOL_SIZE, pool);
  printf ("Total free memory = %d\n", free_mem);
  init (&maldata);
  init (&raldata);
  init (&freedata);
  m = (void **) tlsf_malloc (NUM_MALLOC * sizeof (void *));
  for (i = 0; i < NUM_MALLOC; i++) {
    m[i] = NULL;
  }
  first = last_h = getcurtime ();
  for (i = 0; i < NUM_MALLOC; i++) {
    t = (size_t) (1 + drand48 () * SIZE_MALLOC);
    m[i] = tlsf_calloc (t, 1);
    if (((unsigned long) m[i] & (sizeof(void *) * 2 - 1)) != 0) {
      fprintf(stderr,"Alignment error %p\n", m[i]);
    }
#if FILL_MEM
    memset (m[i], -1, t);
#endif
  }
  for (j = 0; j < 1000; j++) {
    for (i = 0; i < NUM_MALLOC; i++) {
      if (m[i]) {
	t = (size_t) (1 + drand48 () * SIZE_MALLOC);
        s = getcurtime ();
	m[i] = tlsf_realloc (m[i], t);
        update (0, "realloc", s, getcurtime (), &raldata);
	if (((unsigned long) m[i] & (sizeof(void *) * 2 - 1)) != 0) {
	  fprintf(stderr,"Alignment error %p\n", m[i]);
	}
#if FILL_MEM
        memset (m[i], -1, t);
#endif
      }
      if (m[i]) {
	s = getcurtime ();
	tlsf_free (m[i]);
	update (0, "free   ", s, getcurtime (), &freedata);
      }
      t = (size_t) (1 + drand48 () * SIZE_MALLOC);
      s = getcurtime ();
      m[i] = tlsf_malloc (t);
      update (0, "malloc ", s, getcurtime (), &maldata);
      if (((unsigned long) m[i] & (sizeof(void *) * 2 - 1)) != 0) {
        fprintf(stderr,"Alignment error %p\n", m[i]);
      }
#if FILL_MEM
      memset (m[i], -1, t);
#endif
    }
    n++;
    s = getcurtime ();
    if ((s - last_h) > 10) {
      last_h = s;
      printf ("Count = %d %f\n",
	      n * NUM_MALLOC, last_h - first);
      update (1, NULL, 0.0, getcurtime (), &maldata);
      update (1, NULL, 0.0, getcurtime (), &raldata);
      update (1, NULL, 0.0, getcurtime (), &freedata);
    }
  }
  for (i = 0; i < NUM_MALLOC; i++) {
    tlsf_free (m[i]);
  }
  tlsf_free (m);
  return 0;
}
