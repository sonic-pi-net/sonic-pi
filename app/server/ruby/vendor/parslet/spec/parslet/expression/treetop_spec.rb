require 'spec_helper'

require 'parslet'

describe Parslet::Expression::Treetop do
  include Parslet

  describe "positive samples" do
    [ # pattern             # input
      "'abc'",              'abc', 
      "...",                'abc', 
      "[1-4]",              '3',
      
      "'abc'?",             'abc', 
      "'abc'?",             '', 
      
      "('abc')",            'abc', 
      
      "'a' 'b'",            'ab', 
      "'a' ('b')",          'ab', 
      
      "'a' / 'b'",          'a', 
      "'a' / 'b'",          'b', 
      
      "'a'*",               'aaa', 
      "'a'*",               '', 

      "'a'+",               'aa', 
      "'a'+",               'a', 
      
      "'a'{1,2}",           'a',
      "'a'{1,2}",           'aa',

      "'a'{1,}",            'a',
      "'a'{1,}",            'aa',

      "'a'{,2}",            '',
      "'a'{,2}",            'a',
      "'a'{,2}",            'aa',
    ].each_slice(2) do |pattern, input|
      context "exp(#{pattern.inspect})" do
        let(:parslet) { exp(pattern) }
        subject { parslet }
        it { should parse(input) }
        context "string representation" do
          subject { exp(parslet.to_s) }
          it { should parse(input, :trace => true) }
        end
      end
    end
  end
  describe "negative samples" do
    [ # pattern             # input
      "'abc'",              'cba', 
      "[1-4]",              '5',
      
      "'a' / 'b'",          'c', 
      
      "'a'+",               '',
      
      "'a'{1,2}",           '',
      "'a'{1,2}",           'aaa',
      
      "'a'{1,}",            '',

      "'a'{,2}",            'aaa',
    ].each_slice(2) do |pattern, input|
      context "exp(#{pattern.inspect})" do
        subject { exp(pattern) }
        it { should_not parse(input) }
      end
    end
  end
end