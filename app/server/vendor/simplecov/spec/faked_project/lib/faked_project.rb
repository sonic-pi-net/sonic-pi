class FakedProject
  def self.foo
    "bar"
  end
end

Dir[File.join(File.dirname(__FILE__), "faked_project/*.rb")].reject { |f| /untested/.match(f) }.each do |file|
  require file # Require all source files in project dynamically so we can inject some stuff depending on test situation
end

FakedProject.send :include, MetaMagic
