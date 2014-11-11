# encoding: utf-8

# These methods are here for backwards compatability with previous RubyProf releases
module RubyProf
  # Measurements
  def self.cpu_frequency
    Measure::CpuTime.frequency
  end

  def self.cpu_frequency=(value)
    Measure::CpuTime.frequency = value
  end

  def self.measure_allocations
    Measure::Allocations.measure
  end

  def self.measure_cpu_time
    Measure::CpuTime.measure
  end

  def self.measure_gc_runs
    Measure::GcRuns.measure
  end

  def self.measure_gc_time
    Measure::GcTime.measure
  end

  def self.measure_memory
    Measure::Memory.measure
  end

  def self.measure_process_time
    Measure::ProcessTime.measure
  end

  def self.measure_wall_time
    Measure::WallTime.measure
  end

  # call-seq:
  # measure_mode -> measure_mode
  #
  # Returns what ruby-prof is measuring.  Valid values include:
  #
  # *RubyProf::PROCESS_TIME - Measure process time.  This is default.  It is implemented using the clock functions in the C Runtime library.
  # *RubyProf::WALL_TIME - Measure wall time using gettimeofday on Linx and GetLocalTime on Windows
  # *RubyProf::CPU_TIME - Measure time using the CPU clock counter.  This mode is only supported on Pentium or PowerPC platforms.
  # *RubyProf::ALLOCATIONS - Measure object allocations.  This requires a patched Ruby interpreter.
  # *RubyProf::MEMORY - Measure memory size.  This requires a patched Ruby interpreter.
  # *RubyProf::GC_RUNS - Measure number of garbage collections.  This requires a patched Ruby interpreter.
  # *RubyProf::GC_TIME - Measure time spent doing garbage collection.  This requires a patched Ruby interpreter.*/

  def self.measure_mode
    @measure_mode ||= RubyProf::WALL_TIME
  end

  # call-seq:
  # measure_mode=value -> void
  #
  # Specifies what ruby-prof should measure.  Valid values include:
  #
  # *RubyProf::PROCESS_TIME - Measure process time.  This is default.  It is implemented using the clock functions in the C Runtime library.
  # *RubyProf::WALL_TIME - Measure wall time using gettimeofday on Linx and GetLocalTime on Windows
  # *RubyProf::CPU_TIME - Measure time using the CPU clock counter.  This mode is only supported on Pentium or PowerPC platforms.
  # *RubyProf::ALLOCATIONS - Measure object allocations.  This requires a patched Ruby interpreter.
  # *RubyProf::MEMORY - Measure memory size.  This requires a patched Ruby interpreter.
  # *RubyProf::GC_RUNS - Measure number of garbage collections.  This requires a patched Ruby interpreter.
  # *RubyProf::GC_TIME - Measure time spent doing garbage collection.  This requires a patched Ruby interpreter.*/
  def self.measure_mode=(value)
    @measure_mode = value
  end

  # call-seq:
  # exclude_threads -> exclude_threads
  #
  # Returns threads ruby-prof should exclude from profiling

  def self.exclude_threads
    @exclude_threads ||= Array.new
  end

  # call-seq:
  # exclude_threads= -> void
  #
  # Specifies what threads ruby-prof should exclude from profiling

  def self.exclude_threads=(value)
    @exclude_threads = value
  end

  # Profiling
  def self.start_script(script)
    start
    load script
  end

  def self.start
    ensure_not_running!
    @profile = Profile.new(self.measure_mode, self.exclude_threads)
    enable_gc_stats_if_needed
    @profile.start
  end

  def self.pause
    ensure_running!
    disable_gc_stats_if_needed
    @profile.pause
  end

  def self.running?
    if defined?(@profile) and @profile
      @profile.running?
    else
      false
    end
  end

  def self.resume
    ensure_running!
    enable_gc_stats_if_needed
    @profile.resume
  end

  def self.stop
    ensure_running!
    result = @profile.stop
    disable_gc_stats_if_needed
    @profile = nil
    result
  end

  # Profile a block
  def self.profile(&block)
    ensure_not_running!
    gc_stat_was_enabled = enable_gc_stats_if_needed
    res = Profile.profile(self.measure_mode, self.exclude_threads, &block)
    disable_gc_stats_if_needed(gc_stat_was_enabled)
    res
  end


private
  def self.ensure_running!
    raise(RuntimeError, "RubyProf.start was not yet called") unless running?
  end

  def self.ensure_not_running!
    raise(RuntimeError, "RubyProf is already running") if running?
  end

  # for GC.allocated_size to work GC statistics should be enabled
  def self.enable_gc_stats_if_needed
    if measure_mode_requires_gc_stats_enabled?
      @gc_stat_was_enabled = GC.enable_stats
    end
  end

  def self.disable_gc_stats_if_needed(was_enabled=nil)
    was_enabled ||= defined?(@gc_stat_was_enabled) && @gc_stat_was_enabled
    GC.disable_stats if measure_mode_requires_gc_stats_enabled? && !was_enabled
  end

  def self.measure_mode_requires_gc_stats_enabled?
    GC.respond_to?(:enable_stats) &&
      [RubyProf::MEMORY, RubyProf::GC_TIME, RubyProf::GC_RUNS].include?(measure_mode)
  end
end
