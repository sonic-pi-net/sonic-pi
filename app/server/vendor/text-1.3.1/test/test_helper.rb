require 'test/unit'

lib = File.expand_path("../../lib", __FILE__)
$:.unshift lib unless $:.include?(lib)

class Test::Unit::TestCase
  def data_file_path(*path)
    File.join(File.dirname(__FILE__), "data", *path)
  end

  def data_file(*path)
    File.read(data_file_path(*path))
  end
end
