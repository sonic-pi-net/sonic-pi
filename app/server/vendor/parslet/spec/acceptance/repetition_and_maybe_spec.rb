require 'spec_helper'

describe "Tree output" do
  extend Parslet

  def self.hash_examples(h)
    h.each do |atom, expected|
      it "should convert #{atom} to #{expected.inspect}" do
        atom.parse(input).should == expected
      end
    end
  end

  context "when parsing the empty string" do
    let(:input) { '' }
    hash_examples( 
      # No naming 
      str('a').maybe              => '', 
      str('a').repeat             => '',
      
      # Named contents: maybe yields nil
      str('a').maybe.as(:f)       => {:f => nil},
      str('a').repeat.as(:f)      => {:f => []}, 
      
      # Contents that aren't simple strings
      (str('a') >> str('b')).maybe.as(:f)     => {:f => nil},
      (str('a') >> str('b')).repeat.as(:f)    => {:f => []}, 
      
      # The other way around: Contents would be tagged, but nil result isn't
      (str('a') >> str('b')).as(:f).maybe     => '',
      (str('a') >> str('b')).as(:f).repeat    => ''
    )
  end
  
  context "when parsing 'aa'" do
    let(:input) { 'aa' }
    hash_examples(
      # since they're not named, repetitions get merged together.
      str('a').as(:a).repeat >> str('a').as(:a).repeat => [{:a=>'a'},{:a=>'a'}]
    )
  end
end