## Headless Sonic Pi recorder: code in, wav out.
##
## Usage:
##   headless-record.rb -o OUT.wav -d SECONDS [-e CODE | -f FILE.rb] [-s SEED]
##
## With neither -e nor -f, the code is read from stdin, so this works:
##   echo 'live_loop :x do; sample :bd_haus; sleep 0.5; end' | \
##     headless-record.rb -o beat.wav -d 8
##
## Boots the daemon + engine like headless-run.rb, starts SuperSonic's record
## tap, runs the code for `duration` seconds, then stops and shuts down. Unlike
## headless-run.rb the mixer is left at full amp — muting it would record
## silence, because the tap sits after the main mixer.
##
## Recording is realtime: a 60s take takes 60s of wall clock. The tap is
## pre-device, so the machine's output volume does not affect what is written
## (it will still be audible while rendering).
##
## -s SEED seeds the run via use_random_seed, which makes a take reproducible.
## Most of the bundled examples are built on rrand/choose, so without a seed
## every render differs; with one, a take you like can be re-rendered exactly.

require 'fileutils'
require_relative "headless_boot"

USAGE = "Usage: headless-record.rb -o OUT.wav -d SECONDS [-e CODE | -f FILE.rb] [-s SEED]"

FLUSH_WAIT = 2  # let the writer thread finish the file before we check it

out_path = nil
duration = nil
code     = nil
seed     = nil

args = ARGV.dup
value = lambda do |flag|
  v = args.shift
  abort "#{flag} needs a value\n#{USAGE}" if v.nil?
  v
end

until args.empty?
  case (flag = args.shift)
  when "-o" then out_path = value.call(flag)
  when "-d" then duration = value.call(flag)
  when "-e" then code     = value.call(flag)
  when "-f" then
    path = value.call(flag)
    abort "File not found: #{path}" unless File.exist?(path)
    code = File.read(path)
  when "-s" then seed = value.call(flag)
  when "-h", "--help" then puts USAGE; exit 0
  else abort "Unknown argument: #{flag}\n#{USAGE}"
  end
end

abort USAGE unless out_path && duration
abort "Duration must be a positive number of seconds\n#{USAGE}" unless duration.to_f > 0
code = STDIN.read if code.nil?
abort "No code supplied (use -e, -f, or pipe to stdin)" if code.strip.empty?

out_path = File.expand_path(out_path)
duration = duration.to_f
FileUtils.mkdir_p(File.dirname(out_path))

# Recording is driven by talking to SuperSonic directly rather than by wrapping
# the code in recording_start / recording_save. Wrapping would change the
# program's meaning (`with_fx ... do live_loop ... end` returns as soon as the
# loop is started, so anything appended after it runs with the FX torn down),
# and the spider's recording_* only adds a temp file and a move on top of the
# same engine calls. The engine's record tap writes straight to the
# destination. Only the seed is prepended to the code, which is inert.
script = +""
script << "use_random_seed #{seed}\n" if seed
script << code

boot = SonicPi::HeadlessBoot.new.boot!
boot.say "READY — recording #{duration}s to #{out_path}"

boot.engine_client.send("/clockwork/record/start", out_path, "wav", 24)
boot.run(script)

sleep duration
boot.engine_client.send("/clockwork/record/stop")
sleep FLUSH_WAIT
boot.stop_all
sleep 1

boot.say "ERRORS: #{boot.errors.join(' | ')}" unless boot.errors.empty?

if File.exist?(out_path) && File.size(out_path) > 0
  boot.say "WROTE #{out_path} (#{File.size(out_path)} bytes)"
  exit(boot.errors.empty? ? 0 : 2)
else
  boot.say "FAILED — no output written"
  exit 1
end
