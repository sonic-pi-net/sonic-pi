# Coded by Sam Aaron

with_fx(:rlpf, cutoff: 10, cutoff_slide: 4) do |c|
  loop do
    sample :bass_dnb_f, amp: 5
    sample :loop_amen, amp: 5
    sleep sample_duration :loop_amen
    control c, cutoff: rrand(40, 120), cutoff_slide: rrand(1, 4)
  end
end
