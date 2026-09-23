# a sample sounds for its buffer at its rate
load_sample :elec_blip
sleep 0.25
with_fx :level, kill_delay: 0.25 do
  sample :elec_blip, rate: 0.5
end
