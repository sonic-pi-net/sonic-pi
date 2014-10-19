require "spec_helper"
require "hamster/list"

describe Hamster::List do
  describe "#eql?" do
    context "on a really big list" do
      it "doesn't run out of stack" do
        -> { Hamster.interval(0, STACK_OVERFLOW_DEPTH).eql?(Hamster.interval(0, STACK_OVERFLOW_DEPTH)) }.should_not raise_error
      end
    end
  end

  shared_examples 'equal using eql?' do |a, b|
    specify "#{a.inspect} should eql? #{b.inspect}" do
      expect(a).to eql b
    end

    specify "#{a.inspect} should == #{b.inspect}" do
      expect(a).to eq b
    end
  end

  shared_examples 'not equal using eql?' do |a, b|
    specify "#{a.inspect} should not eql? #{b.inspect}" do
      expect(a).to_not eql b
    end
  end

  shared_examples 'equal using ==' do |a, b|
    specify "#{a.inspect} should == #{b.inspect}" do
      expect(a).to eq b
    end
  end

  shared_examples 'not equal using ==' do |a, b|
    specify "#{a.inspect} should not == #{b.inspect}" do
      expect(a).to_not eq b
    end
  end

  include_examples 'equal using =='       , Hamster.list("A", "B", "C"), %w[A B C]
  include_examples 'not equal using eql?' , Hamster.list("A", "B", "C"), %w[A B C]
  include_examples 'not equal using =='   , Hamster.list("A", "B", "C"), Object.new
  include_examples 'not equal using eql?' , Hamster.list("A", "B", "C"), Object.new
  include_examples 'equal using =='       , Hamster.list, []
  include_examples 'not equal using eql?' , Hamster.list, []

  include_examples 'equal using eql?'     , Hamster.list, Hamster.list
  include_examples 'not equal using eql?' , Hamster.list, Hamster.list(nil)
  include_examples 'not equal using eql?' , Hamster.list("A"), Hamster.list
  include_examples 'equal using eql?'     , Hamster.list("A"), Hamster.list("A")
  include_examples 'not equal using eql?' , Hamster.list("A"), Hamster.list("B")
  include_examples 'not equal using eql?' , Hamster.list(%w[A B]), Hamster.list("A")
  include_examples 'equal using eql?'     , Hamster.list(*%w[A B C]), Hamster.list(*%w[A B C])
  include_examples 'not equal using eql?' , Hamster.list(*%w[C A B]), Hamster.list(*%w[A B C])

  include_examples 'equal using =='       , Hamster.list('A'), ['A']
  include_examples 'equal using =='       , ['A'], Hamster.list('A')

  include_examples 'not equal using eql?' , Hamster.list('A'), ['A']
  include_examples 'not equal using eql?' , ['A'], Hamster.list('A')
end