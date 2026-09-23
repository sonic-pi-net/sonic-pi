# onsets are proportions of the sample, so they combine with rate and stretch
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, onset: 1, rate: 2
sample :loop_amen, onset: 1, beat_stretch: 2
sample :loop_amen, onset: 1, rate: -1
