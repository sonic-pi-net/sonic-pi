# start and finish pick out a portion, via the full player
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, start: 0.5
sample :loop_amen, finish: 0.25
sample :loop_amen, start: 0.25, finish: 0.75
