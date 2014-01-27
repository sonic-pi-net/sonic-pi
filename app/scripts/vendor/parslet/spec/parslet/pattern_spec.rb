require 'spec_helper'

require 'parslet'

describe Parslet::Pattern do
  include Parslet
  
  # These two factory methods help make the specs more robust to interface
  # changes. They also help to label trees (t) and patterns (p).
  def p(pattern)
    Parslet::Pattern.new(pattern)
  end
  def t(obj)
    obj
  end
  
  # Tries to match pattern to the tree, and verifies the bindings hash. Don't
  # use this for new examples.
  #
  RSpec::Matchers.define :match_with_bind do |pattern, exp_bindings|
    failure_message_for_should do |tree|
      "expected #{pattern.inspect} to match #{tree.inspect}, but didn't. (block wasn't called or not correctly)"
    end
    match do |tree|
      bindings = Parslet::Pattern.new(pattern).match(tree)
      bindings && bindings == exp_bindings
    end
  end

  # This is the more modern version of verifying a match: (uses 'exp'
  # implicitly). Checks for a match of pattern in +exp+ and yields the
  # matched variables.
  #
  def with_match_locals(pattern, &block) 
    bindings = p(pattern).match(exp)
    bindings.should_not be_nil
    
    block.call(bindings) if block
  end
  
  # Can't use #match here, so I went to the Thesaurus.
  #
  RSpec::Matchers.define :detect do |pattern|
    match do |tree|
      bindings = Parslet::Pattern.new(pattern).match(tree)

      bindings ? true : false
    end
  end
  
  describe "<- #match" do
    context "injecting bindings" do
      let(:pattern) { p(simple(:x)) }
      
      it "should not modify the original bindings hash" do
        h = {}
        b=pattern.match('a', h)
        h.size.should == 0
        b.size.should == 1
      end
      it "should return nil when no match succeeds" do
        pattern.match([], :foo => :bar).should be_nil
      end
      context "when matching simple(:x) against 'a'" do
        let(:bindings) { pattern.match(t('a'), :foo => :bar) }
        
        before(:each) { bindings.should_not be_nil }
        it "should return the injected bindings" do
          bindings[:foo].should == :bar
        end
        it "should return the new bindings" do  
          bindings[:x].should == 'a'
        end
      end
    end
    context "simple strings" do
      let(:exp) { 'aaaa' }

      it "should match simple strings" do
        exp.should match_with_bind(simple(:x), :x => 'aaaa')
      end 
    end
    context "simple hash {:a => 'b'}" do
      attr_reader :exp
      before(:each) do
        @exp = t(:a => 'b')
      end

      it "should not match {:a => simple(:x), :b => simple(:y)}" do
        exp.should_not detect(:a => simple(:x), :b => simple(:y))
      end
      it "should match {:a => simple(:x)}, binding 'x' to the first argument" do
        exp.should match_with_bind({:a => simple(:x)}, :x => 'b')
      end 
      it "should match {:a => 'b'} with no binds" do
        exp.should match_with_bind({:a => 'b'}, {})
      end 
    end
    context "a more complex hash {:a => {:b => 'c'}}" do
      attr_reader :exp
      before(:each) do
        @exp = t(:a => {:b => 'c'})
      end
      
      it "should match wholly with {:a => {:b => simple(:x)}}" do
        exp.should match_with_bind({:a => {:b => simple(:x)}}, :x => 'c')
      end
      it "should match wholly with {:a => subtree(:t)}" do
        with_match_locals(:a => subtree(:t)) do |dict|
          dict[:t].should == {:b => 'c'}
        end
      end
      it "should not bind subtrees to variables in {:a => simple(:x)}" do
        p(:a => simple(:x)).should_not detect(exp)
      end
    end
    context "a more complex hash {:a => 'a', :b => 'b'}" do
      attr_reader :exp
      before(:each) do
        @exp = t({:a => 'a', :b => 'b'})
      end

      it "should not match partially" do
        Parslet::Pattern.new(:a => simple(:x)).match(exp).should be_nil
      end 
      it "should match completely" do
        exp.should match_with_bind({:a => simple(:x), :b => simple(:y)}, 
          :x => 'a', 
          :y => 'b')
      end 
    end
    context "an array of 'a', 'b', 'c'" do
      let(:exp) { ['a', 'b', 'c'] }

      it "should match all elements at once" do
        exp.should match_with_bind(
          [simple(:x), simple(:y), simple(:z)], 
          :x => 'a', :y => 'b', :z => 'c')
      end 
    end
    context "{:a => 'a', :b => 'b'}" do
      attr_reader :exp
      before(:each) do
        @exp = t(:a => 'a', :b => 'b')
      end

      it "should match both elements simple(:x), simple(:y)" do
        exp.should match_with_bind(
          {:a => simple(:x), :b => simple(:y)}, 
          :x => 'a', :y => 'b')
      end
      it "should not match a constrained match (simple(:x) != simple(:y))"  do
        exp.should_not detect({:a => simple(:x), :b => simple(:x)})
      end
    end
    context "{:a => 'a', :b => 'a'}" do
      attr_reader :exp
      before(:each) do
        @exp = t(:a => 'a', :b => 'a')
      end

      it "should match constrained pattern" do
        exp.should match_with_bind(
          {:a => simple(:x), :b => simple(:x)}, 
          :x => 'a')
      end
    end
    context "{:sub1 => {:a => 'a'}, :sub2 => {:a => 'a'}}" do
      attr_reader :exp
      before(:each) do
        @exp = t({
          :sub1 => {:a => 'a'}, 
          :sub2 => {:a => 'a'} 
        })
      end

      it "should verify constraints over several subtrees" do
        exp.should match_with_bind({
          :sub1 => {:a => simple(:x)}, 
          :sub2 => {:a => simple(:x)} 
        }, :x => 'a')
      end
      it "should return both bind variables simple(:x), simple(:y)" do
        exp.should match_with_bind({
          :sub1 => {:a => simple(:x)}, 
          :sub2 => {:a => simple(:y)} 
        }, :x => 'a', :y => 'a')
      end  
    end
    context "{:sub1 => {:a => 'a'}, :sub2 => {:a => 'b'}}" do
      attr_reader :exp
      before(:each) do
        @exp = t({
          :sub1 => {:a => 'a'}, 
          :sub2 => {:a => 'b'} 
        })
      end

      it "should verify constraints over several subtrees" do
        exp.should_not match_with_bind({
          :sub1 => {:a => simple(:x)}, 
          :sub2 => {:a => simple(:x)} 
        }, :x => 'a')
      end
      it "should return both bind variables simple(:x), simple(:y)" do
        exp.should match_with_bind({
          :sub1 => {:a => simple(:x)}, 
          :sub2 => {:a => simple(:y)} 
        }, :x => 'a', :y => 'b')
      end  
    end
    context "[{:a => 'x'}, {:a => 'y'}]" do
      attr_reader :exp  
      before(:each) do
        @exp = t([{:a => 'x'}, {:a => 'y'}])
      end
      
      it "should not match sequence(:x) (as a whole)" do
        exp.should_not detect(sequence(:x))
      end
    end
    context "['x', 'y', 'z']" do
      attr_reader :exp  
      before(:each) do
        @exp = t(['x', 'y', 'z'])
      end

      it "should match [simple(:x), simple(:y), simple(:z)]" do
        with_match_locals([simple(:x), simple(:y), simple(:z)]) do |dict|
          dict[:x].should == 'x'
          dict[:y].should == 'y'
          dict[:z].should == 'z'
        end
      end
      it "should match %w(x y z)" do
        exp.should match_with_bind(%w(x y z), { })
      end 
      it "should not match [simple(:x), simple(:y), simple(:x)]" do
        exp.should_not detect([simple(:x), simple(:y), simple(:x)])
      end
      it "should not match [simple(:x), simple(:y)]" do
        exp.should_not detect([simple(:x), simple(:y), simple(:x)])
      end
      it "should match sequence(:x) (as array)" do
        exp.should match_with_bind(sequence(:x), :x => ['x', 'y', 'z'])
      end
    end
    context "{:a => [1,2,3]}" do
      attr_reader :exp  
      before(:each) do
        @exp = t(:a => [1,2,3])
      end

      it "should match :a => sequence(:x) (binding x to the whole array)" do
        exp.should match_with_bind({:a => sequence(:x)}, {:x => [1,2,3]})
      end
    end
    context "with differently ordered hashes" do
      it "should still match" do
        t(:a => 'a', :b => 'b').should detect(:a => 'a', :b => 'b')
        t(:a => 'a', :b => 'b').should detect(:b => 'b', :a => 'a')

        t(:b => 'b', :a => 'a').should detect(:b => 'b', :a => 'a')
        t(:b => 'b', :a => 'a').should detect(:a => 'a', :b => 'b')
      end 
    end
  end
end