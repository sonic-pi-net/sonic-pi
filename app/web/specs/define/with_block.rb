# a function may take a block
define :twice do |&blk|
  2.times { blk.call }
end
twice do
  play 60
  sleep 0.25
end
