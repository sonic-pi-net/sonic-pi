$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), "..", ".."))
require "lib/simplecov"
SimpleCov.start { command_name "Test" }

dir = File.expand_path(File.dirname(__FILE__))
file = File.join(dir, "generated_buddha.rb")
code = %{
  def kill_the_buddha(z)
    z**z
  end
}
File.open(file, "w") { |f| f.print code }
load file
File.unlink file
raise unless kill_the_buddha(3) == 27
