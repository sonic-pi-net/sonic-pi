module FileMap

  GIT_FILES      = `git ls-files`.split("\n")
  ALL_LIB_FILES  = Dir['lib/concurrent/**/*.rb'] & GIT_FILES
  EDGE_LIB_FILES = Dir['lib/concurrent/actor.rb',
                       'lib/concurrent/actor/**/*.rb',
                       'lib/concurrent/channel.rb',
                       'lib/concurrent/channel/**/*.rb',
                       'lib/concurrent/agent.rb',
                       'lib/concurrent/edge/**/*.rb'] & GIT_FILES
  CORE_LIB_FILES = ALL_LIB_FILES - EDGE_LIB_FILES

  MAP = {
    core: CORE_LIB_FILES + %w(lib/concurrent.rb lib/concurrent_ruby.rb),
    edge: EDGE_LIB_FILES + %w(lib/concurrent-edge.rb)
  }
end
