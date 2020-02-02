#include <aubio.h>

int main (void)
{
  
  aubio_filter_t * f;

  uint_t rates[] = { 8000, 16000, 22050, 44100, 96000, 192000};
  uint_t nrates = 6;
  uint_t samplerate, i = 0;

  for ( samplerate = rates[i]; i < nrates ; i++ ) {
    f = new_aubio_filter_a_weighting (samplerate);
    del_aubio_filter (f);

    f = new_aubio_filter (7);
    aubio_filter_set_a_weighting (f, samplerate);
    del_aubio_filter (f);
  }

  // samplerate unknown
  f = new_aubio_filter_a_weighting (4200);
  if (!f) {
    //PRINT_MSG ("failed creating A-weighting filter with samplerate=4200Hz\n");
  }

  // order to small
  f = new_aubio_filter (2);
  if (aubio_filter_set_a_weighting (f, samplerate) != 0) {
    //PRINT_MSG ("failed setting filter to A-weighting\n");
  }
  del_aubio_filter (f);

  // order to big
  f = new_aubio_filter (12);
  if (aubio_filter_set_a_weighting (f, samplerate) != 0) {
    //PRINT_MSG ("failed setting filter to A-weighting\n");
  }
  del_aubio_filter (f);

  return 0;
}

