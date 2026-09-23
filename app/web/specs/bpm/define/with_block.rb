# a function may take a block
use_bpm 120   # at twice the tempo: specs/define/with_block.rb
define :twice do |&blk|
  2.times { blk.call }
end
twice do
  play 60
  sleep 0.25
end
