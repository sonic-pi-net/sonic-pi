# opts from a hash
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
o = { rate: 0.5, amp: 0.25 }
sample :loop_amen, o
sample :loop_amen, o.merge(pan: 1)
