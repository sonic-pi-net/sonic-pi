# sample returns a node; its args carry the resolved buffer and opts
n = sample :loop_amen, amp: 0.5
puts n.args["amp"]
puts n.name
