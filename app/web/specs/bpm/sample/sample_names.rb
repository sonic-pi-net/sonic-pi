# sample_names lists the built-ins in a group
use_bpm 120   # at twice the tempo: specs/sample/sample_names.rb
puts sample_names(:bd).length
puts sample_names(:bd)[0]
puts sample_names(:loop).length
