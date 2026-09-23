# opts can come from a hash held in a variable, and merged
o = { amp: 0.7, pan: -0.5 }
play 60, o
play 60, o.merge(release: 0.1)
play 60, **o, amp: 0.2
