# onset indexes wrap around the number of onsets, negative from the end
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, onset: 8
sample :loop_amen, onset: -1
sample :loop_amen, onset: 100
