require 'spec_helper'

describe Parslet::Parser do
  include Parslet
  class FooParser < Parslet::Parser
    rule(:foo) { str('foo') }
    root(:foo)
  end
  
  describe "<- .root" do
    parser = Class.new(Parslet::Parser)
    parser.root :root_parslet
    
    it "should have defined a 'root' method, returning the root" do
      parser_instance = parser.new
      flexmock(parser_instance).should_receive(:root_parslet => :answer)
      
      parser_instance.root.should == :answer
    end 
  end
  it "should parse 'foo'" do
    FooParser.new.parse('foo').should == 'foo'
  end 
  context "composition" do
    let(:parser) { FooParser.new }
    it "should allow concatenation" do
      composite = parser >> str('bar')
      composite.should parse('foobar')
    end
  end
end