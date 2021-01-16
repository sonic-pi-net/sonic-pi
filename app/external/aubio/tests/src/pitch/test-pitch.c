#include <aubio.h>

int main (void)
{
  // 1. allocate some memory
  uint_t n = 0; // frame counter
  uint_t win_s = 1024; // window size
  uint_t hop_s = win_s / 4; // hop size
  uint_t samplerate = 44100; // samplerate
  // create some vectors
  fvec_t *input = new_fvec (hop_s); // input buffer
  fvec_t *out = new_fvec (1); // output candidates
  // create pitch object
  aubio_pitch_t *o = new_aubio_pitch ("default", win_s, hop_s, samplerate);

  // 2. do something with it
  while (n < 100) {
    // get `hop_s` new samples into `input`
    // ...
    // exectute pitch
    aubio_pitch_do (o, input, out);
    // do something with output candidates
    // ...
    n++;
  };

  // 3. clean up memory
  del_aubio_pitch (o);
  del_fvec (out);
  del_fvec (input);
  aubio_cleanup ();

  if (new_aubio_pitch(0, win_s, hop_s, samplerate)) return 1;
  if (new_aubio_pitch("unknown", win_s, hop_s, samplerate)) return 1;
  if (new_aubio_pitch("default", win_s,     0, samplerate)) return 1;
  if (new_aubio_pitch("default",     0, hop_s, samplerate)) return 1;
  if (new_aubio_pitch("default", hop_s, win_s, samplerate)) return 1;
  if (new_aubio_pitch("default", win_s, hop_s,          0)) return 1;

  o = new_aubio_pitch("default", win_s, hop_s, samplerate);

  if (aubio_pitch_set_unit(o, "freq")) return 1;
  if (aubio_pitch_set_unit(o, "hertz")) return 1;
  if (aubio_pitch_set_unit(o, "Hertz")) return 1;
  if (aubio_pitch_set_unit(o, "Hz")) return 1;
  if (aubio_pitch_set_unit(o, "f0")) return 1;
  if (aubio_pitch_set_unit(o, "midi")) return 1;
  if (aubio_pitch_set_unit(o, "cent")) return 1;
  if (aubio_pitch_set_unit(o, "bin")) return 1;
  if (!aubio_pitch_set_unit(o, "unknown")) return 1;

  if (aubio_pitch_set_tolerance(o, 0.3)) return 1;
  if (aubio_pitch_set_silence(o, 0)) return 1;
  if (aubio_pitch_set_silence(o, -200)) return 1;
  if (!aubio_pitch_set_silence(o, -300)) return 1;
  del_aubio_pitch(o);

  // fft based might fail with non power of 2
  o = new_aubio_pitch("yinfft", win_s + 1, hop_s, samplerate);
  if (o) del_aubio_pitch(o);
  o = new_aubio_pitch("yinfast", win_s + 1, hop_s, samplerate);
  if (o) del_aubio_pitch(o);
  o = new_aubio_pitch("fcomb", win_s + 1, hop_s, samplerate);
  if (o) del_aubio_pitch(o);
  o = new_aubio_pitch("mcomb", win_s + 1, hop_s, samplerate);
  if (o) del_aubio_pitch(o);
  o = new_aubio_pitch("specacf", win_s + 1, hop_s, samplerate);
  if (o) del_aubio_pitch(o);

  return 0;
}
