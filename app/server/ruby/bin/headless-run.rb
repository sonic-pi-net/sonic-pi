## Headless Sonic Pi runner for automated timing/behaviour tests.
##
## Usage: ruby headless-run.rb /path/to/script.rb [duration_seconds]
##
## Boots the daemon + engine, waits until ready, runs the given script,
## streams every server log/error line to stdout (prefixed + timestamped
## relative to script start) for `duration` seconds, then shuts down cleanly.
## Unlike repl.rb there is no Readline loop, so it runs unattended.
##
## The mixer is muted, so a run makes no sound. Use headless-record.rb when the
## audio itself is what you're after.

require_relative "headless_boot"

script   = ARGV[0]
duration = (ARGV[1] || "20").to_f
abort "File not found: #{script}" unless script && File.exist?(script)

boot = SonicPi::HeadlessBoot.new
boot.boot! do |osc|
  osc.add_method("/flash") { |m| boot.say "FLASH job #{m[0]} #{m[1]} line #{m[2]}" }

  osc.add_method("/live_loop/scope") { |m| boot.say "LOOP-SCOPE job #{m[0]} #{m[1]} #{m[2]} line #{m[3]} slot #{m[4]}" }
  osc.add_method("/live_loop/scope-ended") { |m| boot.say "LOOP-SCOPE-ENDED job #{m[0]} #{m[1]}" }
end

boot.mute!  # silent: volume fader 0
boot.say "READY — running script for #{duration}s"
boot.run(File.read(script))

sleep duration
boot.say "done — stopping"
boot.stop_all
sleep 0.5
exit 0
