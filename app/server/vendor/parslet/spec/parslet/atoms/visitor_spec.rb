require 'spec_helper'

describe Parslet::Atoms do
  include Parslet
  let(:visitor) { flexmock(:visitor) }
  
  describe Parslet::Atoms::Str do
    let(:parslet) { str('foo') }
    it "should call back visitor" do
      visitor.should_receive(:visit_str).with('foo').once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Re do
    let(:parslet) { match['abc'] }
    it "should call back visitor" do
      visitor.should_receive(:visit_re).with('[abc]').once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Sequence do
    let(:parslet) { str('a') >> str('b') }
    it "should call back visitor" do
      visitor.should_receive(:visit_sequence).with(Array).once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Repetition do
    let(:parslet) { str('a').repeat(1,2) }
    it "should call back visitor" do
      visitor.should_receive(:visit_repetition).with(:repetition, 1, 2, Parslet::Atoms::Base).once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Alternative do
    let(:parslet) { str('a') | str('b') }
    it "should call back visitor" do
      visitor.should_receive(:visit_alternative).with(Array).once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Named do
    let(:parslet) { str('a').as(:a) }
    it "should call back visitor" do
      visitor.should_receive(:visit_named).with(:a, Parslet::Atoms::Base).once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Entity do
    let(:parslet) { Parslet::Atoms::Entity.new('foo', &lambda {}) }
    it "should call back visitor" do
      visitor.should_receive(:visit_entity).with('foo', Proc).once
      
      parslet.accept(visitor)
    end 
  end
  describe Parslet::Atoms::Lookahead do
    let(:parslet) { str('a').absent? }
    it "should call back visitor" do
      visitor.should_receive(:visit_lookahead).with(false, Parslet::Atoms::Base).once
      
      parslet.accept(visitor)
    end 
  end
  describe "< Parslet::Parser" do
    let(:parslet) { Parslet::Parser.new }
    it "calls back to visitor" do
      visitor.should_receive(:visit_parser).with(:root).once
      
      flexmock(parslet, :root => :root)
      parslet.accept(visitor)
    end 
  end
end