# onset: can be a lambda over the onsets ring
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, onset: ->(o) { o[2] }
sample :loop_amen, onset: ->(o) { o.choose }
