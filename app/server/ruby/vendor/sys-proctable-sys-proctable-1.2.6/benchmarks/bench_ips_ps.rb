#!/usr/bin/env ruby

require 'json'
require 'optparse'
require 'rbconfig'
require 'benchmark/ips'

@source, @gem = false

OptionParser.new do |opt|
  opt.banner = "Usage: #{File.basename $0} [--gem|--source]"

  opt.separator ""
  opt.separator "Benchmark IPS (iterations per second) changes between"
  opt.separator "source version of sys-proctable and previous gem"
  opt.separator "version."
  opt.separator ""
  opt.separator "Requires that the both the source version of the gem"
  opt.separator "be installed (run `rake install` to do this after"
  opt.separator "each of your changes) and version installed via"
  opt.separator "rubygems."
  opt.separator ""
  opt.separator "To run, call with `--gem` two times, and then with "
  opt.separator "`--source` two times again."
  opt.separator ""

  opt.on("--source") { @source = true }
  opt.on("--gem")    { @gem = true }
end.parse!

if @source && !@gem
  # Being paranoid here and making sure we get the version installed to
  # sitelibdir
  require File.join(RbConfig::CONFIG["sitelibdir"], "sys", "proctable")
else
  @gem = true
  require 'sys-proctable'
end

Benchmark.ips do |bench|
  bench.report("Block form - pre patch") do
    (puts "ERR:  Please run with --gem"; exit 1) unless @gem
    Sys::ProcTable.ps {}
  end

  bench.report("Non-block form - pre patch") do
    (puts "ERR:  Please run with --gem"; exit 1) unless @gem
    Sys::ProcTable.ps
  end

  bench.report("Block form - post patch") do
    (puts "ERR:  Please run with --source"; exit 1) unless @source
    Sys::ProcTable.ps {}
  end

  bench.report("Non-block form - post patch") do
    (puts "ERR:  Please run with --source"; exit 1) unless @source
    Sys::ProcTable.ps
  end

  bench.hold! "bench_ips_ps.results"
  bench.compare!
end
