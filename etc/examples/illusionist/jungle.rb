# Coded by Sam Aaron

with_fx :lpf, cutoff: 90 do
  with_fx :reverb, mix: 0.5 do
    with_fx :compressor, pre_amp: 40 do
      with_fx :distortion, distort: 0.4 do
        loop do
          use_random_seed 667
          4.times do
            sample :loop_amen, rate: [1, 1, 1, -1].choose * 1.35 / 2, finish: 0.5, amp: 0.5
            sample :loop_amen, rate: 1.35
            sleep sample_duration :loop_amen, rate: 1.35
          end
        end
      end
    end
  end
end
