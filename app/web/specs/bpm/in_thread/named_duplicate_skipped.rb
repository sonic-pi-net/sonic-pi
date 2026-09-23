# a second thread with a live name is not started
use_bpm 120   # at twice the tempo: specs/in_thread/named_duplicate_skipped.rb
in_thread(name: :one) do
  sleep 0.5
  play 60
end
in_thread(name: :one) do
  play 99
end
sleep 0.25
in_thread(name: :one) do
  play 98
end
