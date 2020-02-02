#include <string.h> // strncpy
#include <limits.h> // PATH_MAX
#include <aubio.h>
#include "utils_tests.h"

int main (int argc, char **argv)
{
  sint_t err = 0;

  if (argc < 3) {
    PRINT_ERR("not enough arguments, running tests\n");
    err = run_on_default_source_and_sink(main);
    PRINT_MSG("usage: %s <input_path> <output_path> <sample_path> [samplerate]\n", argv[0]);
    return err;
  }

  uint_t samplerate = 0; // default is the samplerate of input_path
  uint_t hop_size = 256;
  uint_t n_frames = 0, read = 0;

  char_t *source_path = argv[1];
  char_t *sink_path = argv[2];
  char_t sample_path[PATH_MAX];
  if ( argc >= 4 ) {
    strncpy(sample_path, argv[3], PATH_MAX - 1);
  } else {
    // use input_path as sample
    strncpy(sample_path, source_path, PATH_MAX - 1);
  }
  sample_path[PATH_MAX - 1] = '\0';
  if ( argc >= 5 ) samplerate = atoi(argv[4]);

  fvec_t *vec = new_fvec(hop_size);
  aubio_source_t *source = new_aubio_source(source_path, samplerate, hop_size);
  if (samplerate == 0 ) samplerate = aubio_source_get_samplerate(source);
  aubio_sink_t *sink = new_aubio_sink(sink_path, samplerate);

  aubio_sampler_t * sampler = new_aubio_sampler (samplerate, hop_size);

  aubio_sampler_load (sampler, sample_path);

  do {
    aubio_source_do(source, vec, &read);
    if (n_frames / hop_size == 10) {
      aubio_sampler_play ( sampler );
    }
    if (n_frames / hop_size == 40) {
      aubio_sampler_play ( sampler );
    }
    if (n_frames / hop_size == 70) {
      aubio_sampler_play ( sampler );
    }
    if (n_frames > 10.0 * samplerate) {
      aubio_sampler_stop ( sampler );
    }
    aubio_sampler_do (sampler, vec, vec);
    aubio_sink_do(sink, vec, read);
    n_frames += read;
  } while ( read == hop_size );

  del_aubio_sampler(sampler);
  del_aubio_source(source);
  del_aubio_sink(sink);
  del_fvec(vec);
  aubio_cleanup();

  return 0;
}
