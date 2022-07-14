// this should be included *after* custom functions have been defined

#ifndef aubio_sink_custom
#define aubio_sink_custom "undefined"
#endif /* aubio_sink_custom */

#ifdef HAVE_AUBIO_SINK_CUSTOM
int test_wrong_params(void);

int base_main(int argc, char **argv)
{
  uint_t err = 0;
  if (argc < 3 || argc >= 6) {
    PRINT_ERR("wrong number of arguments, running tests\n");
    err = test_wrong_params();
    PRINT_MSG("usage: %s <input_path> <output_path> [samplerate] [hop_size]\n",
        argv[0]);
    return err;
  }

  uint_t samplerate = 0;
  uint_t hop_size = 512;
  uint_t n_frames = 0, read = 0;

  char_t *source_path = argv[1];
  char_t *sink_path = argv[2];

  aubio_source_t *src = NULL;
  aubio_sink_custom_t *snk = NULL;

  if ( argc >= 4 ) samplerate = atoi(argv[3]);
  if ( argc >= 5 ) hop_size = atoi(argv[4]);

  fvec_t *vec = new_fvec(hop_size);
  if (!vec) { err = 1; goto failure; }

  src = new_aubio_source(source_path, samplerate, hop_size);
  if (!src) { err = 1; goto failure; }
  if (samplerate == 0 ) samplerate = aubio_source_get_samplerate(src);

  snk = new_aubio_sink_custom(sink_path, samplerate);
  if (!snk) { err = 1; goto failure; }

  do {
    aubio_source_do(src, vec, &read);
    aubio_sink_custom_do(snk, vec, read);
    n_frames += read;
  } while ( read == hop_size );

  PRINT_MSG("%d frames at %dHz (%d blocks) read from %s, wrote to %s\n",
      n_frames, samplerate, n_frames / hop_size,
      source_path, sink_path);

  // close sink now (optional)
  aubio_sink_custom_close(snk);

failure:
  if (snk)
    del_aubio_sink_custom(snk);
  if (src)
    del_aubio_source(src);
  if (vec)
    del_fvec(vec);

  return err;
}

int test_wrong_params(void)
{
  fvec_t *vec;
  fmat_t *mat;
  aubio_sink_custom_t *s;
  char_t sink_path[PATH_MAX] = "tmp_aubio_XXXXXX";
  uint_t samplerate = 44100;
  uint_t hop_size = 256;
  uint_t oversized_hop_size = 4097;
  uint_t oversized_samplerate = 192000 * 8 + 1;
  uint_t channels = 3;
  uint_t oversized_channels = 1025;
  // create temp file
  int fd = create_temp_sink(sink_path);

  if (!fd) return 1;

  if (new_aubio_sink_custom(   0,   samplerate)) return 1;
  if (new_aubio_sink_custom("\0",   samplerate)) return 1;
  if (new_aubio_sink_custom(sink_path,      -1)) return 1;

  s = new_aubio_sink_custom(sink_path, 0);

  // check setting wrong parameters fails
  if (!aubio_sink_custom_preset_samplerate(s, oversized_samplerate)) return 1;
  if (!aubio_sink_custom_preset_channels(s, oversized_channels)) return 1;
  if (!aubio_sink_custom_preset_channels(s, -1)) return 1;

  // check setting valid parameters passes
  if (aubio_sink_custom_preset_samplerate(s, samplerate)) return 1;
  if (aubio_sink_custom_preset_channels(s, 1)) return 1;

  // check writing a vector with valid length
  vec = new_fvec(hop_size);
  aubio_sink_custom_do(s, vec, hop_size);
  // check writing more than in the input
  aubio_sink_custom_do(s, vec, hop_size+1);
  // check write 0 frames
  aubio_sink_custom_do(s, vec, 0);
  del_fvec(vec);

  // check writing an oversized vector
  vec = new_fvec(oversized_hop_size);
  aubio_sink_custom_do(s, vec, oversized_hop_size);
  del_fvec(vec);

  // test delete without closing
  del_aubio_sink_custom(s);

  s = new_aubio_sink_custom(sink_path, 0);

  // preset channels first
  if (aubio_sink_custom_preset_channels(s, channels)) return 1;
  if (aubio_sink_custom_preset_samplerate(s, samplerate)) return 1;

  if (aubio_sink_custom_get_samplerate(s) != samplerate) return 1;
  if (aubio_sink_custom_get_channels(s) != channels) return 1;

  mat = new_fmat(channels, hop_size);
  // check writing a vector with valid length
  aubio_sink_custom_do_multi(s, mat, hop_size);
  // check writing 0 frames
  aubio_sink_custom_do_multi(s, mat, 0);
  // check writing more than in the input
  aubio_sink_custom_do_multi(s, mat, hop_size+1);
  del_fmat(mat);

  // check writing oversized input
  mat = new_fmat(channels, oversized_hop_size);
  aubio_sink_custom_do_multi(s, mat, oversized_hop_size);
  del_fmat(mat);

  // check writing undersized input
  mat = new_fmat(channels - 1, hop_size);
  aubio_sink_custom_do_multi(s, mat, hop_size);
  del_fmat(mat);

  aubio_sink_custom_close(s);
  // test closing twice
  aubio_sink_custom_close(s);

  del_aubio_sink_custom(s);

  // delete temp file
  close_temp_sink(sink_path, fd);

  // shouldn't crash on null (bypassed, only check del_aubio_sink)
  // del_aubio_sink_custom(NULL);

  return run_on_default_source_and_sink(base_main);
}

#else /* HAVE_AUBIO_SINK_CUSTOM */

int base_main(int argc, char** argv)
{
  PRINT_ERR("aubio was not compiled with aubio_sink_"
          aubio_sink_custom ", failed running %s with %d args\n",
          argv[0], argc);
  return 0;
}

#endif /* HAVE_AUBIO_SINK_CUSTOM */
