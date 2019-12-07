libpath = File.expand_path(File.dirname(__FILE__))+"/../lib"
$LOAD_PATH.unshift libpath
libpath = File.expand_path(File.dirname(__FILE__))+"/../"
$LOAD_PATH.unshift libpath

begin
  require "rubygems"
rescue
end

require "narray"
