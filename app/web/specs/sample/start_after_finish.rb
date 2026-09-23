# start greater than finish plays that portion backwards
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, start: 0.75, finish: 0.25
sample :loop_amen, start: 1, finish: 0, rate: 1
