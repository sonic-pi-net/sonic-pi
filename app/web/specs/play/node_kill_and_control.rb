# the node play returns can be controlled and killed through its own methods, as the language's control and kill
s = play 60, release: 4, cutoff: 80
sleep 0.5
s.control note: 67, cutoff: 100
sleep 0.5
s.ctl note: 72
sleep 0.5
s.kill
