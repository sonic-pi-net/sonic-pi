#include <aubio.h>
#include "utils_tests.h"

int test_wrong_params(void);

int main (int argc, char **argv)
{
  sint_t err = 0;

  if (argc < 3) {
    err = 2;
    PRINT_WRN("no arguments, running tests\n");
    err = test_wrong_params();
    PRINT_MSG("usage: %s <input_path> <output_path> [samplerate] [hop_size]\n", argv[0]);
    return err;
  }

  uint_t samplerate = 0;
  uint_t win_size = 1024;
  uint_t hop_size = 512;
  uint_t n_frames = 0, read = 0;

  char_t *source_path = argv[1];
  char_t *sink_path = argv[2];

  if ( argc >= 4 ) samplerate = atoi(argv[3]);
  if ( argc >= 5 ) hop_size = atoi(argv[4]);

  fvec_t *vec = new_fvec(hop_size);
  fvec_t *out = new_fvec(hop_size); // output buffer
  fvec_t *scale = new_fvec(hop_size);
  cvec_t *fftgrain = new_cvec(win_size); // fft norm and phase
  if (!vec) { err = 1; goto beach_fvec; }

  aubio_source_t *i = new_aubio_source(source_path, samplerate, hop_size);
  if (!i) { err = 1; goto beach_source; }

  if (samplerate == 0 ) samplerate = aubio_source_get_samplerate(i);

  aubio_sink_t *o = new_aubio_sink(sink_path, samplerate);
  if (!o) { err = 1; goto beach_sink; }

  aubio_pvoc_t *pv = new_aubio_pvoc(win_size, hop_size);
  if (!pv) { err = 1; goto beach_pvoc; }

  aubio_spectral_whitening_t *awhitening =
    new_aubio_spectral_whitening (win_size, hop_size, samplerate);
  if (!awhitening) { err = 1; goto beach_awhitening; }

  aubio_spectral_whitening_set_relax_time(awhitening, 20.);
  fvec_set_all(scale, 3.);

  PRINT_MSG("spectral whitening relaxation time is %f\n",
      aubio_spectral_whitening_get_relax_time(awhitening));

  do {
    aubio_source_do(i, vec, &read);
    aubio_pvoc_do(pv, vec, fftgrain);
    // apply spectral whitening
    aubio_spectral_whitening_do(awhitening, fftgrain);
    // rebuild the signal
    aubio_pvoc_rdo(pv, fftgrain, out);
    // make louder
    fvec_weight(out, scale);
    // make sure we dont saturate
    fvec_clamp(out, 1.);
    // write output
    aubio_sink_do(o, out, read);
    n_frames += read;
  } while ( read == hop_size );

  PRINT_MSG("read %d frames at %dHz (%d blocks) from %s written to %s\n",
      n_frames, samplerate, n_frames / hop_size,
      source_path, sink_path);

  del_aubio_spectral_whitening(awhitening);
beach_awhitening:
  del_aubio_pvoc(pv);
beach_pvoc:
  del_aubio_sink(o);
beach_sink:
  del_aubio_source(i);
beach_source:
  del_fvec(vec);
  del_fvec(out);
  del_fvec(scale);
  del_cvec(fftgrain);
beach_fvec:
  return err;
}

int test_wrong_params(void)
{
  uint_t buf_size = 512;
  uint_t hop_size = 256;
  uint_t samplerate = 44100;
  aubio_spectral_whitening_t *o;

  if (new_aubio_spectral_whitening(       0, hop_size, samplerate)) return 1;
  if (new_aubio_spectral_whitening(buf_size,        0, samplerate)) return 1;
  if (new_aubio_spectral_whitening(buf_size, hop_size,          0)) return 1;

  o = new_aubio_spectral_whitening(buf_size, hop_size, samplerate);

  aubio_spectral_whitening_get_relax_time(o);
  aubio_spectral_whitening_get_floor(o);

  del_aubio_spectral_whitening(o);

  return run_on_default_source_and_sink(main);
}
