# play returns a node whose args are the resolved opts
n = play 60, amp: 0.5
puts n.args["note"]
puts n.args["amp"]
