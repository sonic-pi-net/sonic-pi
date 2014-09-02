# Welcome to Sonic Pi v2.0

pent = [#:B1, :Cs2, :Ds2,
        :Fs2, :Gs2, :As2,
        :B2,
        :Cs3, :Ds3, :Fs3, :Gs3, :As3,
        :B3,
        :Cs4, :Ds4, :Fs4, :Gs4, :As4,
        :B4,
        :Cs5, :Ds5, :Fs5, :Gs5, :As5,
        :B5,
        :Cs6, :Ds6, :Fs6, :Gs6, :As6]

use_synth :tri
with_fx :reverb, rate: 0.2 do
  i = 15
  t = 0.28
  stack = []
  100.times do
    mode = rrand_i(0,2)
    lene = rrand_i(0,4)*2
    if mode==2 then
      direction = rrand_i(0,1)*2 - 1
      tp = (t*4)/lene #(t/2 + (t)*rrand_i(0,1))
      local_stack = []
      direction = -1 if (lene + i >= pent.length)
      direction = 1 if (i - lene <= 0)
      lene.to_int.times do
        notes = [i]
        notes.push(i+4) if rrand_i(0,3) == 1 && (i+4 < pent.length)
        notes.push(i-4) if rrand_i(0,3) == 1 && (i-4 >= 0)
        notes.push(i+2) if rrand_i(0,6) == 3 && (i+2 < pent.length)
        notes.push(i-2) if rrand_i(0,6) == 3 && (i-2 >= 0)
        notes.each do |note|
          play pent[note]
        end
        sleep tp
        i = (i + direction).abs
        local_stack.push([notes,tp])
      end
      stack.push(local_stack)
    else
      print "repeat mode"
      transp = rrand_i(0,4)-2
      if stack != [] then
        stack.last.each do |notes, tp|
          notes.each do |note|
            if ((note + transp >= 0) && (note + transp < pent.length)) then
              play pent[note + transp]
            end
          end
          sleep tp
        end
      end
    end
  end
end
