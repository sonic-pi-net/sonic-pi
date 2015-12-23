require "mkmf"

if RUBY_VERSION < "1.9.3"
  STDERR.puts("Ruby version #{RUBY_VERSION} is no longer supported. Please upgrade to 1.9.3 or higher")
  exit(1)
end

# standard ruby methods
have_func("rb_gc_stat")
have_func("rb_gc_count")

# Alexander Dymo GC patch
have_func("rb_os_allocated_objects")
have_func("rb_gc_allocated_size")

# Stefan Kaes GC patches
have_func("rb_gc_collections")
have_func("rb_gc_time")
# for ruby 2.1
have_func("rb_gc_total_time")
have_func("rb_gc_total_mallocs")
have_func("rb_gc_total_malloced_bytes")

# Lloyd Hilaiel's heap info patch
have_func("rb_heap_total_mem")
have_func("rb_gc_heap_info")

def add_define(name, value = nil)
  if value
    $defs.push("-D#{name}=#{value}")
  else
    $defs.push("-D#{name}")
  end
end

if !Gem.win_platform? && RUBY_PLATFORM !~ /(darwin|openbsd)/
  $LDFLAGS += " -lrt" # for clock_gettime
end
add_define("RUBY_VERSION", RUBY_VERSION.gsub('.', ''))

# for ruby 1.9, determine whether threads inherit trace flags (latest 1.9.2 and later should work correctly)
if RUBY_VERSION > "1.9"
  require 'set'
  threads = Set.new
  set_trace_func lambda { |*args| threads << Thread.current.object_id }
  Thread.new{1}.join
  set_trace_func nil
  if threads.size < 2
    # if we end up here, ruby does not automatically active tracing in spawned threads
    STDERR.puts("Ruby #{RUBY_VERSION} does not activate tracing in spawned threads. Consider upgrading.")
    exit(1)
  end
end

create_makefile("ruby_prof")
