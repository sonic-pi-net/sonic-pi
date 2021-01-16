#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include <limits.h>
#include <string.h>
#include <malloc.h>

#define NUM_MALLOC      (10000)
#define SIZE_MALLOC     (10000)

#define FILL_MEM        (0)

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

int
main (void)
{
  int i;
  int j;
  size_t t;
  int n = 0;
  int max_size = 0;
  double s, first, last_h;
  void **m;
  minmax maldata, raldata, freedata;

  init (&maldata);
  init (&raldata);
  init (&freedata);
  m = (void **) malloc (NUM_MALLOC * sizeof (void *));
  for (i = 0; i < NUM_MALLOC; i++) {
    m[i] = NULL;
  }
  first = last_h = getcurtime ();
  for (i = 0; i < NUM_MALLOC; i++) {
    t = (size_t) (1 + drand48 () * SIZE_MALLOC);
    m[i] = calloc (t, 1);
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
	m[i] = realloc (m[i], t);
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
	free (m[i]);
	update (0, "free   ", s, getcurtime (), &freedata);
      }
      t = (size_t) (1 + drand48 () * SIZE_MALLOC);
      s = getcurtime ();
      m[i] = malloc (t);
      update (0, "malloc ", s, getcurtime (), &maldata);
      if (((unsigned long) m[i] & (sizeof(void *) * 2 - 1)) != 0) {
        fprintf(stderr,"Alignment error %p\n", m[i]);
      }
#if FILL_MEM
      memset (m[i], -1, t);
#endif
    }
    if (mallinfo().uordblks > max_size) {
      max_size = mallinfo().uordblks;
    }
    n++;
    s = getcurtime ();
    if ((s - last_h) > 10) {
      last_h = s;
      printf ("Count = %d %f, max memory %d\n",
	      n * NUM_MALLOC, last_h - first, mallinfo().uordblks);
      update (1, NULL, 0.0, getcurtime (), &maldata);
      update (1, NULL, 0.0, getcurtime (), &raldata);
      update (1, NULL, 0.0, getcurtime (), &freedata);
    }
  }
  for (i = 0; i < NUM_MALLOC; i++) {
    free (m[i]);
  }
  free (m);
  printf ("Total used memory = %d, max memory %d\n",
          mallinfo().uordblks, max_size);
  return 0;
}
