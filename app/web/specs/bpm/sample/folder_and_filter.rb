# a folder and a name filter; a regexp filter; several filters are all required
use_bpm 120   # at twice the tempo: specs/sample/folder_and_filter.rb
sample SAMPLES_DIR, "bd_haus"
sample SAMPLES_DIR, /^loop_amen\.flac/
sample SAMPLES_DIR, "loop", "amen"
sample SAMPLES_DIR, "loop", 2
