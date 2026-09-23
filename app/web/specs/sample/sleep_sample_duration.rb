# the idiom: sleep for the sample's length
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen
sleep sample_duration(:loop_amen)
sample :loop_amen, rate: 2
sleep sample_duration(:loop_amen, rate: 2)
play 60
