case RUBY_ENGINE
when 'ruby'
  require 'mkmf'
  create_makefile 'did_you_mean/method_missing'
else
  File.open("Makefile", "w") {|file| file.write "install:\n\t:\n" }
end
