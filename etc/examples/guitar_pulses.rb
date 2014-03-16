size = 0.5

in_thread do
  loop do
    s_name = :guit_e_slide
    s = rand(1 - size)
    e = s + size
    if rand < 0.25
      r = -1
    else
      r = 1
    end
    sample s_name, :start, s, :end, e, :attack, 0.1, :release, 0.1, :rate, r * [1, 0.5].choose
    sleep sample_duration(s_name) * size
  end
end

in_thread do
  loop do
    n_size = size / 0.5
    s_name = :guit_harmonics
    s = rand(1 - n_size)
    e = s + n_size
    if rand < 0.25
      r = -1
    else
      r = 1
    end
    sample s_name, :start, s, :end, e, :attack, 0.1, :release, 0.1, :rate, r * 0.5
    sleep sample_duration(s_name) * n_size
  end
end


loop do
  n_size = size / 8
  s_name = :guit_e_fifths
  s = 0.2 + rand(0.3)
  e = s + n_size
  sample s_name, :start, s, :end, e, :attack, 0.1, :release, 0.1
  sleep sample_duration(s_name) * n_size
end
