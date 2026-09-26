# slicer, panslicer and wobble are given the studio's random stream as rand_buf, which probability: tosses its coins with; an fx that tosses none is not
with_fx :slicer, probability: 0.5, seed: 3 do
  play 60, release: 0.25
end
with_fx :panslicer, probability: 0.25 do
  play 62, release: 0.25
end
with_fx :wobble, probability: 0.75, phase: 0.25 do
  play 64, release: 0.25
end
with_fx :echo do
  play 65, release: 0.25
end
