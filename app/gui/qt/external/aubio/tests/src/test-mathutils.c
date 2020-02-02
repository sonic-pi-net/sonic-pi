#define AUBIO_UNSTABLE 1
#include "aubio.h"
#include "utils_tests.h"

int test_next_power_of_two (void);
int test_miditofreq (void);
int test_freqtomidi (void);
int test_aubio_window (void);

int test_next_power_of_two (void)
{
  uint_t a, b;
  a = 15; b = aubio_next_power_of_two(a); assert(b == 16);
  fprintf(stdout, "aubio_next_power_of_two(%d) = %d\n", a, b);

  a = 17; b = aubio_next_power_of_two(a); assert(b == 32);
  fprintf(stdout, "aubio_next_power_of_two(%d) = %d\n", a, b);

  a = 31; b = aubio_next_power_of_two(a); assert(b == 32);
  fprintf(stdout, "aubio_next_power_of_two(%d) = %d\n", a, b);

  a = 32; b = aubio_next_power_of_two(a); assert(b == 32);
  fprintf(stdout, "aubio_next_power_of_two(%d) = %d\n", a, b);

  a = 33; b = aubio_next_power_of_two(a); assert(b == 64);
  fprintf(stdout, "aubio_next_power_of_two(%d) = %d\n", a, b);

  return 0;
}

int test_miditofreq (void)
{
  smpl_t a, b;
  fprintf(stdout, "b = aubio_miditofreq(a): [");
  for ( a = -123.; a < 400.; a += 20. ) {
    b = aubio_miditofreq(a);
    fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  }
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = -69.5;
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = -169.5;
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = 140.;
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = 0;
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = 8.2e10;
  b = aubio_miditofreq(a);
  fprintf(stdout, "(%.2f,  %.2f), ", a, b);
  a = -5.e10;
  fprintf(stdout, "(%.2f,  %.2f)", a, b);
  fprintf(stdout, "]\n");
  return 0;
}

int test_freqtomidi (void)
{
  smpl_t midi, freq;
  fprintf(stdout, "b = aubio_freqtomidi(a): [");
  for ( freq = 0.; freq < 30000.; freq += 440. ) {
    midi = aubio_freqtomidi(freq);
    fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  }
  freq = 69.5;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = -69.5;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = -169.5;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = 140.;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = 0;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = 8.2e10;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f), ", freq, midi);
  freq = -5.;
  midi = aubio_freqtomidi(freq);
  fprintf(stdout, "(%.2f,  %.2f)]\n", freq, midi);
  return 0;
}

int test_aubio_window (void)
{
  uint_t window_size = 16;
  fvec_t * window = new_aubio_window("default", window_size);
  del_fvec(window);

  window = new_fvec(window_size);
  fvec_set_window(window, "rectangle");
  fvec_print(window);
  del_fvec(window);

  window_size /= 2.;
  window = new_aubio_window("parzen", window_size);
  fvec_print(window);
  del_fvec(window);

  window = new_aubio_window("rectangle", 16);
  del_fvec (window);
  return 0;
}

int main (void)
{
  test_next_power_of_two();
  test_miditofreq();
  test_freqtomidi();
  test_aubio_window();
  return 0;
}
