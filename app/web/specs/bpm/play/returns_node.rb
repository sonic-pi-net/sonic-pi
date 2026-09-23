# play returns a node whose args are the resolved opts
use_bpm 120   # at twice the tempo: specs/play/returns_node.rb
n = play 60, amp: 0.5
puts n.args["note"]
puts n.args["amp"]
