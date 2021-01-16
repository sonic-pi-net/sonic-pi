#include <aubio.h>
#include "utils_tests.h"

int test_wrong_params(void);

int main (int argc, char** argv)
{
  sint_t err = 0;

  if (argc < 2) {
    err = 2;
    PRINT_WRN("no arguments, running tests\n");
    err = test_wrong_params();
    PRINT_MSG("usage: %s <input_path> [samplerate] [hop_size]\n", argv[0]);
    return err;
  }

  uint_t win_s; // fft size
  uint_t hop_s = 256; // block size
  uint_t samplerate = 0; // samplerate
  uint_t n_filters = 40; // number of filters
  uint_t n_coeffs = 13; // number of coefficients
  uint_t read = 0;

  char_t *source_path = argv[1];

  if ( argc >= 3 ) samplerate = atoi(argv[2]);
  if ( argc >= 4 ) hop_s = atoi(argv[3]);

  win_s = 2 * hop_s;

  aubio_source_t *source = 0;
  aubio_pvoc_t *pv = 0;
  aubio_mfcc_t *mfcc = 0;

  fvec_t *in = new_fvec (hop_s);       // phase vocoder input
  cvec_t *fftgrain = new_cvec (win_s); // pvoc output / mfcc input
  fvec_t *out = new_fvec (n_coeffs);   // mfcc output

  if (!in || !fftgrain || !out) { err = 1; goto failure; }

  // source
  source = new_aubio_source(source_path, samplerate, hop_s);
  if (!source) { err = 1; goto failure; }
  if (samplerate == 0) samplerate = aubio_source_get_samplerate(source);

  // phase vocoder
  pv = new_aubio_pvoc(win_s, hop_s);
  if (!pv) { err = 1; goto failure; }

  // mfcc object
  mfcc = new_aubio_mfcc (win_s, n_filters, n_coeffs, samplerate);
  if (!mfcc) { err = 1; goto failure; }

  // processing loop
  do {
    aubio_source_do(source, in, &read);
    aubio_pvoc_do(pv, in, fftgrain);
    aubio_mfcc_do(mfcc, fftgrain, out);
    fvec_print(out);
  } while (read == hop_s);

failure:

  if (mfcc)
    del_aubio_mfcc(mfcc);
  if (pv)
    del_aubio_pvoc(pv);
  if (source)
    del_aubio_source(source);
  if (in)
    del_fvec(in);
  if (fftgrain)
    del_cvec(fftgrain);
  if (out)
    del_fvec(out);
  aubio_cleanup();
  return err;
}

int test_wrong_params()
{
  uint_t win_s = 512; // fft size
  uint_t n_filters = 40; // number of filters
  uint_t n_coeffs = 13; // number of coefficients
  smpl_t samplerate = 16000.; // samplerate

  if (new_aubio_mfcc(    0, n_filters, n_coeffs, samplerate)) return 1;
  if (new_aubio_mfcc(win_s,         0, n_coeffs, samplerate)) return 1;
  if (new_aubio_mfcc(win_s, n_filters,        0, samplerate)) return 1;
  if (new_aubio_mfcc(win_s, n_filters, n_coeffs,          0)) return 1;

  return run_on_default_source(main);
}
