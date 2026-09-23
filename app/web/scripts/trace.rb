#!/usr/bin/env ruby
# SPDX-License-Identifier: AGPL-3.0-or-later
# Trace runner: runs one spec program through an adapter and prints its trace
# as JSON. The adapter is the runtime under test; "oracle" is Sonic Pi's own
# Ruby runtime (oracle/), the reference every other adapter is measured against.
#
#   scripts/trace.rb specs/play/midi_number.rb                # oracle
#   ADAPTER="$PWD/build/runtime/sp-trace" scripts/trace.rb spec.rb # another runtime
#
# An adapter is any command that takes a program path and prints a trace JSON
# document on stdout (see specs/README.md for the shape).
require "json"
ROOT = File.expand_path("..", __dir__)
path = ARGV[0] or abort "usage: trace.rb <spec.rb>"
adapter = ENV["ADAPTER"] || "ruby #{File.join(ROOT, 'oracle/harness/oracle.rb')}"
out = IO.popen("#{adapter} #{path}", err: [:child, :out], &:read)
abort "adapter failed for #{path}:\n#{out}" unless $?.success?
puts out
