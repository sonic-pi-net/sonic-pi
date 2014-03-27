with_fx(:rlpf, cutoff: 10, cutoff_slide: 20) do |c|
  loop do
    sample :bass_dnb_f, amp: 5
    sample :loop_amen, amp: 5
    sleep sample_duration :loop_amen
    c.ctl cutoff: rrand(40, 120), cutoff_slide: rrand(1, 20)
  end
end
