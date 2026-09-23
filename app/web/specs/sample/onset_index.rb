# onset: n plays from the nth detected onset to the next
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, onset: 0
sample :loop_amen, onset: 1
sample :loop_amen, onset: 3
