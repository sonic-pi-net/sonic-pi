# a chord plays one synth per note, all at the same time
play chord(:e3, :minor)
sleep 0.5
play chord(:c4, :major7), amp: 0.5
