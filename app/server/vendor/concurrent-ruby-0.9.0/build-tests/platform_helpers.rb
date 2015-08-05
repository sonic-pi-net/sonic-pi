require 'rbconfig'

def windows?
  host_os = RbConfig::CONFIG['host_os']
  host_os =~ /mswin32/i || host_os =~ /mingw32/i
end

def mri?(engine = RUBY_ENGINE)
  engine == 'ruby'
end

def jruby?(engine = RUBY_ENGINE)
  engine == 'jruby'
end

def rbx?(engine = RUBY_ENGINE)
  engine == 'rbx'
end
