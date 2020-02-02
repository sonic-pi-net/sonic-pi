#define AUBIO_UNSTABLE 1
#include <aubio.h>
#include "utils_tests.h"

#define aubio_sink_custom "sndfile"

#ifdef HAVE_SNDFILE
#define HAVE_AUBIO_SINK_CUSTOM
#define aubio_sink_custom_t aubio_sink_sndfile_t
#define new_aubio_sink_custom new_aubio_sink_sndfile
#define del_aubio_sink_custom del_aubio_sink_sndfile
#define aubio_sink_custom_do aubio_sink_sndfile_do
#define aubio_sink_custom_do_multi aubio_sink_sndfile_do_multi
#define aubio_sink_custom_close aubio_sink_sndfile_close
#define aubio_sink_custom_preset_samplerate aubio_sink_sndfile_preset_samplerate
#define aubio_sink_custom_preset_channels aubio_sink_sndfile_preset_channels
#define aubio_sink_custom_get_samplerate aubio_sink_sndfile_get_samplerate
#define aubio_sink_custom_get_channels aubio_sink_sndfile_get_channels
#endif /* HAVE_SNDFILE */

#include "base-sink_custom.h"

// this file uses the unstable aubio api, please use aubio_sink instead
// see src/io/sink.h and tests/src/sink/test-sink.c

int main (int argc, char **argv)
{
  return base_main(argc, argv);
}
