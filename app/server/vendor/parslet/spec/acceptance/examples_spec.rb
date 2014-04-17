require 'spec_helper'
require 'open3'

describe "Regression on" do
  Dir["example/*.rb"].each do |example|
    context example do
      # Generates a product path for a given example file. 
      def product_path(str, ext)
        str.
          gsub('.rb', ".#{ext}").
          gsub('example/','example/output/')
      end
      
      it "runs successfully" do
        stdin, stdout, stderr = Open3.popen3("ruby #{example}")
        
        handle_map = {
          stdout => :out, 
          stderr => :err
        }
        expectation_found = handle_map.any? do |io, ext|
          name = product_path(example, ext)
          
          if File.exists?(name)
            io.read.strip.should == File.read(name).strip
            true
          end
        end
        
        unless expectation_found
          fail "Example doesn't have either an .err or an .out file. "+
            "Please create in examples/output!"
        end
      end
    end
  end
end
