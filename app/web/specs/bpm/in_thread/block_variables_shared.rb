# blocks close over the parent's variables
use_bpm 120   # at twice the tempo: specs/in_thread/block_variables_shared.rb
count = 0
in_thread do
  count += 1
end
sleep 0.2
puts count
