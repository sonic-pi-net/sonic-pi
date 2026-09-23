# sample returns a node; its args carry the resolved buffer and opts
use_bpm 120   # at twice the tempo: specs/sample/returns_node.rb
n = sample :loop_amen, amp: 0.5
puts n.args["amp"]
puts n.name
