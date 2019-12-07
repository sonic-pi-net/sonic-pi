
require 'case'
require 'text/highlight'

require 'document'
require 'example'

class ExampleRunner
  def run(args)
    Dir[File.join(args.last, '*.textile')].each do |name|
      puts name.white
      Document.new(name).process
    end
  end
end

if $0 == __FILE__
  ExampleRunner.new.run(ARGV)
end