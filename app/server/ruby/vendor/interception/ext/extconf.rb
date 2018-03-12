require 'rbconfig'


if RbConfig::CONFIG['ruby_install_name'] == 'jruby'

  File.open("Makefile", "w") do |f|
    f.write "install:\n\tjrubyc --javac org/pryrepl/InterceptionEventHook.java\n"
  end

elsif RbConfig::CONFIG['ruby_install_name'] =~ /^ruby/ && RUBY_VERSION.to_f < 2.0

  require 'mkmf'
  $CFLAGS += " -DRUBY_18" if RUBY_VERSION =~ /^(1.8)/
  $CFLAGS += " -DRUBY_19" if RUBY_VERSION =~ /^(1.9)/
  extension_name = "interception"
  dir_config(extension_name)
  create_makefile(extension_name)

else

  File.open("Makefile", "w") do |f|
    f.write "install:\n\t:\n"
  end

end
