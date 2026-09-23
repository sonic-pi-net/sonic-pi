# an absolute path to a sound file
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample SAMPLES_DIR + "/loop_amen.flac"
sample SAMPLES_DIR + "/loop_amen.flac"
sample SAMPLES_DIR + "/loop_amen.flac", rate: 2
