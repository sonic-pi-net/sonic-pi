# Testing and Profiling

This is a short document to help out anyone who wants to
contribute to development of Sonic Pi. To do that you should
try to use (and add to) the test suites.

## Running the tests

Sonic Pi doesn't have a database, so the tests are quite a lot
simpler than your average web app.

```
$ cd app/server/sonicpi/test
$ rake test
```

This should run all the tests for you. 

If you have an error like this:

```
Warning: you should require 'minitest/autorun' instead.
Warning: or add 'gem "minitest"' before 'require "minitest/autorun"'
```

You may find you need to run the following.

```
$ gem install test-unit
```

## Integration tests

Some experimental integration tests have been included in `app/server/sonicpi/test/integration`.
which involve some manual intervention to run. The idea is that a script is run against an open
instance of Sonic Pi and each of the examples are recorded to a wav file in the `/tmp` directory.

This is then run on another platform and the resulting wavs are copied over to the original test machine.
There is then another script which trims the silence from both wavs using [sox](http://sox.sourceforge.net/),
and then the trimmed wavs are "fingerprinted" using [chromaprint](https://acoustid.org/chromaprint). This
process is the same as the one [described here by Sergey Potapov](http://greyblake.com/blog/2013/12/19/how-to-compare-audio-in-ruby/).
Thanks Sergey!

It gives a similarity score to each wav (magic!) and gives us some idea of how Sonic Pi is performing
across the different platforms. At least that's the idea...

If you want to run these yourself you'll need to install the following packages and gems

```
# (on a mac)
$ brew install sox chromaprint
$ gem install sox-ruby
$ gem install chromaprint
```

Then open your copy of Sonic Pi and run

```
$ ruby ./app/server/sonicpi/test/integration/export_example_wavs.rb
```

This will take a while as it runs through every example for about 30 seconds! Feel free to turn the volume down and do
something else for a while...

To run the fingerprinting test, change the file paths to point to your original files and whichever folder you want to compare it with.
Then run

```
$ ruby ./app/server/sonicpi/test/integration/fingerprint_test.rb
```

## Profiling

Sonic Pi is a truly multi-threaded system with communications via OSC
(open sound control) being sent between the Qt GUI, the Ruby server and
the scsynth synthesis server. As a result, profiling has proved to be tricky.

### Profiling the Ruby server

The following is a suggestion for how one might go about profiling the Ruby
elements of Sonic Pi. This assumes you are using Ruby 2.1 or greater.

1. `gem install ruby-prof`
2. (optional) Clone [this repo](https://github.com/xavriley/tinyosc), run `build.sh` followed by `./tinyosc 4557`
 This is just to provide a receiver for the OSC message that are sent to the GUI
on port 4557. If you don't do this you'll see some error messages to do with connections.
3. `gem install sonic-pi-cli` - for version 2.7 dev and above you may need to install from this repo: https://github.com/xavriley/sonic-pi-cli
4. Run this command from the root dir of Sonic Pi

```
$ ruby-prof -p multi ./app/server/bin/sonic-pi-server.rb
```

5. Run this command (again from the root dir) to send some code to Sonic Pi

```
$ cat etc/examples/wizard/tilburg.rb | sonic_pi
```

6. After letting it run for a while, run `sonic_pi stop` in a fresh terminal
and then Ctrl+C the sonic-pi-server.rb process. This should create several
files in the root folder containing profiling information.

### Profiling the Qt GUI

TODO

### Profiling `scsynth`

TODO
