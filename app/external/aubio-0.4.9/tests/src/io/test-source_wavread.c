#define AUBIO_UNSTABLE 1
#include <aubio.h>
#include "utils_tests.h"

#define aubio_source_custom "wavread"

#ifdef HAVE_WAVREAD
#define HAVE_AUBIO_SOURCE_CUSTOM
#define aubio_source_custom_t aubio_source_wavread_t
#define new_aubio_source_custom new_aubio_source_wavread
#define del_aubio_source_custom del_aubio_source_wavread
#define aubio_source_custom_get_samplerate aubio_source_wavread_get_samplerate
#define aubio_source_custom_get_duration aubio_source_wavread_get_duration
#define aubio_source_custom_do aubio_source_wavread_do
#define aubio_source_custom_do_multi aubio_source_wavread_do_multi
#define aubio_source_custom_seek aubio_source_wavread_seek
#define aubio_source_custom_close aubio_source_wavread_close
#define aubio_source_custom_get_channels aubio_source_wavread_get_channels
#define aubio_source_custom_get_samplerate aubio_source_wavread_get_samplerate
#endif /* HAVE_WAVREAD */

#include "base-source_custom.h"

// this file uses the unstable aubio api, please use aubio_source instead
// see src/io/source.h and tests/src/source/test-source.c

int main (int argc, char **argv)
{
  return base_main(argc, argv);
}
