require 'spec_helper'

describe 'Result of a Parslet#parse' do
  include Parslet; extend Parslet
  
  describe "regression" do
    [
      # Behaviour with maybe-nil
      [str('foo').maybe >> str('bar'), "bar", "bar"],
      [str('bar') >> str('foo').maybe, "bar", 'bar'], 
      
      # These might be hard to understand; look at the result of 
      #   str.maybe >> str
      # and 
      #   str.maybe >> str first. 
      [(str('f').maybe >> str('b')).repeat, "bb", "bb"],
      [(str('b') >> str('f').maybe).repeat, "bb", 'bb'], 
      
      [str('a').as(:a) >> (str('b') >> str('c').as(:a)).repeat, 'abc', 
        [{:a=>'a'}, {:a=>'c'}]], 
      
      [str('a').as(:a).repeat >> str('b').as(:b).repeat, 'ab', [{:a=>'a'}, {:b=>'b'}]], 
      
      # Repetition behaviour / named vs. unnamed
      [str('f').repeat, '', ''], 
      [str('f').repeat.as(:f), '', {:f => []}],

      # Maybe behaviour / named vs. unnamed
      [str('f').maybe, '', ''], 
      [str('f').maybe.as(:f), '', {:f => nil}],
    ].each do |parslet, input, result|
      context "#{parslet.inspect}" do
        it "should parse \"#{input}\" into \"#{result}\"" do
          parslet.parse(input).should == result
        end
      end
    end
  end
end