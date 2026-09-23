# slice: n plays the nth of num_slices equal portions
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, slice: 0
sample :loop_amen, slice: 1
sample :loop_amen, slice: 3, num_slices: 8
