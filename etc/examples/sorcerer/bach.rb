# Bach Minuet in G
#
# Coded by Robin Newman

use_bpm 60
use_synth_defaults release: 0.5, amp: 0.7, cutoff: 90
use_synth :beep

## Each section of the minuet is repeated
2.times do

  ## First start a thread for the first 8 bars of the bass left hand part
  in_thread do
    play_chord [55,59]#b1
    sleep 1
    play_pattern_timed [57],[0.5]
    play_pattern_timed [59],[1.5] #b2
    play_pattern_timed [60],[1.5] #b3
    play_pattern_timed [59],[1.5] #b4
    play_pattern_timed [57],[1.5] #b5
    play_pattern_timed [55],[1.5] #b6
    play_pattern_timed [62,59,55],[0.5] #b7
    play_pattern_timed [62],[0.5] #b8
    play_pattern_timed [50,60,59,57],[0.25]
  end

  ## Play concurrently the first 8 bars of the right hand part
  play_pattern_timed [74],[0.5]#b1
  play_pattern_timed [67,69,71,72],[0.25]
  play_pattern_timed [74,67,67],[0.5]#b2
  play_pattern_timed [76],[0.5]#b3
  play_pattern_timed [72,74,76,78],[0.25]
  play_pattern_timed [79,67,67],[0.5]#b4
  play_pattern_timed [72],[0.5] #b5
  play_pattern_timed [74,72,71,69],[0.25]
  play_pattern_timed [71],[0.5] #b6
  play_pattern_timed [72,71,69,67],[0.25]
  play_pattern_timed [66],[0.5] #b7
  play_pattern_timed [67,69,71,67],[0.25]
  play_pattern_timed [71,69],[0.5,1] #b8

  ## Start a new thread for bars 9-16 of the left hand part
  in_thread do
    play_chord [55,59]#b9=b1
    sleep 1
    play 57
    sleep 0.5
    play_pattern_timed [55,59,55],[0.5] #b10
    play_pattern_timed [60],[1.5] #b11=b3
    play_pattern_timed [59,60,59,57,5],[0.5,0.25,0.25,0.25,0.25] #b12=b4]
    play_pattern_timed [57,54],[1,0.5] #b13
    play_pattern_timed [55,59],[1,0.5] #b14
    play_pattern_timed [60,62,50],[0.5] #b15
    play_pattern_timed [55,43],[1,0.5] #b16
  end

  ## Play concurrently bars 9-16 of the right hand part the first six
  ## bars repeat bars 1-6
  play_pattern_timed [74],[0.5]#b9 = b1
  play_pattern_timed [67,69,71,72],[0.25]
  play_pattern_timed [74,67,67],[0.5]#b10=b2
  play_pattern_timed [76],[0.5]#b11=b3
  play_pattern_timed [72,74,76,78],[0.25]
  play_pattern_timed [79,67,67],[0.5]#b12=b4
  play_pattern_timed [72],[0.5] #b13=b5
  play_pattern_timed [74,72,71,69],[0.25]
  play_pattern_timed [71],[0.5] #b14=b6
  play_pattern_timed [72,71,69,67],[0.25]
  play_pattern_timed [69],[0.5] #b15
  play_pattern_timed [71,69,67,66],[0.25]
  play_pattern_timed [67],[1.5] #b16
end


## ==========second section starts here======
## The second section is also repeated
2.times do

  ## Start a thread for bars 17-24 of the left hand part
  in_thread do
    play_pattern_timed [55],[1.5] #b17
    play_pattern_timed [54],[1.5] #b18
    play_pattern_timed [52,54,52],[0.5] #b19
    play_pattern_timed [57,45],[1,0.5] #b20
    play_pattern_timed [57],[1.5] #b21
    play_pattern_timed [59,62,61],[0.5] #b22
    play_pattern_timed [62,54,57],[0.5] #b23
    play_pattern_timed [62,50,60],[0.5] #b24
  end

  ## Play bars 17 to 24 of the right hand concurrently with the left
  ## hand thread
  play_pattern_timed [83],[0.5] #b17
  play_pattern_timed [79,81,83,79],[0.25]
  play_pattern_timed [81],[0.5] #b18
  play_pattern_timed [74,76,78,74],[0.25]
  play_pattern_timed [79],[0.5] #b19
  play_pattern_timed [76,78,79,74],[0.25]
  play_pattern_timed [73,71,73,69],[0.5,0.25,0.25,0.5] #b20
  play_pattern_timed [69,71,73,74,76,78],[0.25] #b21
  play_pattern_timed [79,78,76],[0.5] #b22
  play_pattern_timed [78,69,73],[0.5] #b23
  play 74 #b24
  sleep 1.5

  ## Start a new thread for bars 25-32 of the left hand part
  in_thread do
    play_pattern_timed [59,62,59],[0.5] #b25
    play_pattern_timed [60,64,60],[0.5] #b26
    play_pattern_timed [59,57,55],[0.5] #b27
    play 62 #b28
    sleep 1.5 #includes a rest
    play_pattern_timed [50,54],[1,0.5] #b29
    play_pattern_timed [52,55,54],[0.5] #b30
    play_pattern_timed [55,47,50],[0.5] #b31
    play_pattern_timed [55,50,43],[0.5] #b32
  end

  ## Play bars 25-32 of the right hand part concurrently with the left
  ## hand thread
  play_pattern_timed [74,67,66,67],[0.5,0.25,0.25,0.5] #b25
  play_pattern_timed [76,67,66,67],[0.5,0.25,0.25,0.5] #b26
  play_pattern_timed [74,72,71],[0.5] #b27
  play_pattern_timed [69,67,66,67,69],[0.25,0.25,0.25,0.25,0.5] #b28
  play_pattern_timed [62,64,66,67,69,71],[0.25] #b29
  play_pattern_timed [72,71,69],[0.5] #b30
  play_pattern_timed [71,74,67,66],[0.25,0.25,0.5,0.5] #b31
  play_chord [67,59] #b32
  sleep 1.5
end
