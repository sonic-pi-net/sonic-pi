# Coded by Sam Aaron

live_loop :foo do
  sample :bd_haus, amp: 5, cutoff: 50, amp: 5, release: 0.1
  sleep 0.5
end

live_loop :mel do
  with_fx :wobble, phase: 1, invert_wave: 1, wave: 0, cutoff_max: 80, cutoff_min: 60 do
    synth :blade, note: :cs1, release: 4, cutoff: 110, amp: 1, pitch_shift: 0
  end
  with_fx :reverb, room: 1 do
    with_fx :bitcrusher, mix: 0.4 do
      sample :bass_trance_c, rate: 0.5, pitch: 0, window_size: 0.125, time_dis: 0.125, amp: 8, release: 0.2
    end
  end
  sleep 4
end
