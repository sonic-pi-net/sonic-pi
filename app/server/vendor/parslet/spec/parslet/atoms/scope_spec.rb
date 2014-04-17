require 'spec_helper'

describe Parslet::Atoms::Scope do
  include Parslet
  include Parslet::Atoms::DSL
  
  
  let(:context) { Parslet::Atoms::Context.new(nil) }
  let(:captures) { context.captures }
  
  def inject string, parser
    source = Parslet::Source.new(string)
    parser.apply(source, context, true)
  end
  
  let(:aabb) { 
    scope {
      match['ab'].capture(:f) >> dynamic { |s,c| str(c.captures[:f]) }
    }
  }
  it "keeps values of captures outside" do
    captures[:f] = 'old_value'
    inject 'aa', aabb
    captures[:f].should == 'old_value'
  end 
end