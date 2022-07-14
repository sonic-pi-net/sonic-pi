#include <aubio.h>

int main (void)
{
  uint_t buf_size = 2048;
  uint_t hop_size = 512;
  uint_t samplerate = 44100;
  smpl_t silence, minioi_ms, release_drop;
  aubio_notes_t *o = new_aubio_notes("default",
      buf_size, hop_size, samplerate);
  silence = aubio_notes_get_silence(o);
  minioi_ms = aubio_notes_get_minioi_ms(o);
  release_drop = aubio_notes_get_release_drop(o);
  if (aubio_notes_set_silence(o, silence)) return 1;
  if (aubio_notes_set_minioi_ms(o, minioi_ms)) return 1;
  if (aubio_notes_set_release_drop(o, release_drop)) return 1;
  del_aubio_notes(o);
  // test wrong arguments
  if (new_aubio_notes("unknown", buf_size, hop_size, samplerate)) return 1;
  if (new_aubio_notes("default",        0, hop_size, samplerate)) return 1;
  if (new_aubio_notes("default", buf_size,        0, samplerate)) return 1;
  if (new_aubio_notes("default", buf_size, hop_size,          0)) return 1;
  return 0;
}
