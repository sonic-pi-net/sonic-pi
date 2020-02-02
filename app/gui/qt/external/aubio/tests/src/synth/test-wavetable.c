#include <aubio.h>
#include "utils_tests.h"

int main (int argc, char **argv)
{
  sint_t err = 0;

  if (argc < 2) {
    PRINT_ERR("not enough arguments, running tests\n");
    err = run_on_default_sink(main);
    PRINT_MSG("usage: %s <output_path> [freq] [samplerate]\n", argv[0]);
    return err;
  }

  uint_t samplerate = 44100; // default is the samplerate of input_path
  uint_t hop_size = 256;
  smpl_t freq = 440.;

  char_t *sink_path = argv[1];
  if ( argc >= 4 ) samplerate = atoi(argv[3]);
  if ( argc >= 3 ) freq = atof(argv[2]);

  fvec_t *vec = new_fvec(hop_size);
  aubio_sink_t *sink = new_aubio_sink(sink_path, samplerate);

  aubio_wavetable_t * wavetable = new_aubio_wavetable (samplerate, hop_size);

  // 2 seconds duration, in samples
  uint_t duration = 2 * samplerate;

  uint_t n_frames = 0;
  uint_t write = 0;

  aubio_wavetable_play (wavetable);
  aubio_wavetable_set_freq ( wavetable, freq );

  uint_t region = 0;

  do {
    if ( n_frames > duration / 3 && region < 1) {
      aubio_wavetable_stop (wavetable);
      region++;
    }
    if ( n_frames > 2 * duration / 3 && region < 2) {
      aubio_wavetable_play (wavetable);
      aubio_wavetable_set_freq ( wavetable, freq  / 2.);
      region++;
    }
    if (duration - n_frames < hop_size * 2 ) {
      aubio_wavetable_stop( wavetable );
    }
    if (duration - n_frames < hop_size ) {
      write = duration - n_frames;
    } else {
      write = hop_size;
    }
    aubio_wavetable_do (wavetable, vec, vec);
    aubio_sink_do(sink, vec, write);
    n_frames += hop_size;
  } while ( n_frames <= duration );

  del_aubio_wavetable (wavetable);
  del_aubio_sink(sink);
  del_fvec(vec);
  aubio_cleanup();

  return 0;
}
