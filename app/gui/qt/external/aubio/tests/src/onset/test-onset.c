#include <aubio.h>
#include "utils_tests.h"

int test_wrong_params(void);

int main (int argc, char **argv)
{
  uint_t err = 0;
  if (argc < 2) {
    err = 2;
    PRINT_WRN("no arguments, running tests\n");
    err = test_wrong_params();
    PRINT_MSG("usage: %s <source_path> [samplerate] [hop_size]\n", argv[0]);
    return err;
  }
  uint_t samplerate = 0;
  uint_t win_s = 1024; // window size
  uint_t hop_size = win_s / 4;
  uint_t n_frames = 0, read = 0;
  if ( argc >= 3 ) samplerate = atoi(argv[2]);
  if ( argc >= 4 ) hop_size = atoi(argv[3]);

  char_t *source_path = argv[1];
  aubio_source_t * source = new_aubio_source(source_path, samplerate, hop_size);
  if (!source) { err = 1; goto beach; }

  if (samplerate == 0 ) samplerate = aubio_source_get_samplerate(source);

  // create some vectors
  fvec_t * in = new_fvec (hop_size); // input audio buffer
  fvec_t * out = new_fvec (2); // output position

  // create onset object
  aubio_onset_t * o = new_aubio_onset("default", win_s, hop_size, samplerate);

  do {
    // put some fresh data in input vector
    aubio_source_do(source, in, &read);
    // execute onset
    aubio_onset_do(o,in,out);
    // do something with the onsets
    if (out->data[0] != 0) {
      PRINT_MSG("onset at %.3fms, %.3fs, frame %d\n",
          aubio_onset_get_last_ms(o), aubio_onset_get_last_s(o),
          aubio_onset_get_last(o));
    }
    n_frames += read;
  } while ( read == hop_size );

  PRINT_MSG("read %.2fs, %d frames at %dHz (%d blocks) from %s\n",
      n_frames * 1. / samplerate,
      n_frames, samplerate,
      n_frames / hop_size, source_path);

  // clean up memory
  del_aubio_source(source);
  del_aubio_onset(o);
  del_fvec(in);
  del_fvec(out);
beach:
  aubio_cleanup();

  return err;
}

int test_wrong_params(void)
{
  uint_t win_size = 1024;
  uint_t hop_size = win_size / 2;
  uint_t samplerate = 44100;
  // hop_size < 1
  if (new_aubio_onset("default", 5, 0, samplerate)) return 1;

  // buf_size < 2
  if (new_aubio_onset("default", 1, 1, samplerate)) return 1;

  // buf_size < hop_size
  if (new_aubio_onset("default", hop_size, win_size, samplerate)) return 1;

  // samplerate < 1
  if (new_aubio_onset("default", 1024, 512, 0)) return 1;

  // specdesc creation failed
  if (new_aubio_onset("abcd", win_size, win_size/2, samplerate)) return 1;

  aubio_onset_t *o;

  // pv creation might fail
  o = new_aubio_onset("default", 5, 2, samplerate);
  if (o) del_aubio_onset(o);

  o = new_aubio_onset("default", win_size, hop_size, samplerate);
  if (!aubio_onset_set_default_parameters(o, "wrong_type")) return 1;
  del_aubio_onset(o);

  return run_on_default_source(main);
}
