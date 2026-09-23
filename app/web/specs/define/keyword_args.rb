# keyword parameters
define :hits do |len:, times: 2|
  times.times do
    play 36
    sleep len
  end
end
hits len: 0.25
hits len: 0.5, times: 1
