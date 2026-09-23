# playing nil is silent, but the run carries on
use_bpm 120   # at twice the tempo: specs/play/nil_note.rb
play nil
sleep 0.25
play nil, amp: 2
sleep 0.25
play 60
