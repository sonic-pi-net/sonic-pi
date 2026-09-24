# horizon: 4.02
# a live_loop run again inside the with_fx it is already in carries on there with its new body
# The gaps are a beat and two, not less: native only waits in real time once a thread is more than 0.4 s ahead of
# the clock (lang/core.rb sleep), so a redefinition sooner than that after the loop starts happens at the same real
# moment as the loop's first time round, and which body that runs is the scheduler's coin toss. At twice the tempo
# (specs/bpm) half a beat was 0.25 s, inside that window, and a quarter of the oracle's runs played the new body
# first. A beat and two clear it at either tempo, by 0.3 s and more.
use_bpm 120   # at twice the tempo: specs/with_fx/live_loop_run_again_inside.rb
with_fx :level, kill_delay: 0.25 do
  live_loop :a do
    play 60, release: 0.1
    sleep 2
  end
  sleep 1
  live_loop :a do
    play 62, release: 0.1
    sleep 2
  end
end
