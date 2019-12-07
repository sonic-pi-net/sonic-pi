require 'spec_helper'

describe Parslet::Atoms::Capture do
  include Parslet
  
  let(:context) { Parslet::Atoms::Context.new(nil) }
  
  def inject string, parser
    source = Parslet::Source.new(string)
    parser.apply(source, context, true)
  end
  
  it "should capture simple results" do
    inject 'a', str('a').capture(:a)
    context.captures[:a].should == 'a'
  end 
  it "should capture complex results" do
    inject 'a', str('a').as(:b).capture(:a)
    context.captures[:a].should == {:b => 'a'}
  end 
end