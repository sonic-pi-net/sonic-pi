# cutoff, lpf, hpf and res
# (the sample is loaded first: a sample's first trigger runs in a helper thread, see README)
load_sample :loop_amen
sample :loop_amen, cutoff: 80
sample :loop_amen, lpf: 90, hpf: 20
sample :loop_amen, cutoff: 100, res: 0.5
