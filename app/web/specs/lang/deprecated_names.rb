# native's retired names say what replaced them
[-> { with_tempo 60 }, -> { use_fx :reverb }, -> { pitch_ratio 1 }, -> { use_sample_pack :x }, -> { with_sample_pack :x }, -> { use_sample_pack_as :x }, -> { with_sample_pack_as :x }, -> { current_sample_pack_aliases }].each do |f|
  begin
    f.call
  rescue => e
    puts e.class
    puts e.message
  end
end
