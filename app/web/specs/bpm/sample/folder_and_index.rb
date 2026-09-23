# a folder and an index picks a sample from that folder alphabetically
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
use_bpm 120   # at twice the tempo: specs/sample/folder_and_index.rb
load_samples SAMPLES_DIR
sample SAMPLES_DIR, 0
sample SAMPLES_DIR, 1
sample SAMPLES_DIR, 205
sample SAMPLES_DIR, 206
