require 'spec_helper'

describe 'parslet/convenience' do
  require 'parslet/convenience'
  include Parslet
  
  class FooParser < Parslet::Parser
    rule(:foo) { str('foo') }
    root(:foo)
  end
  
  describe 'parse_with_debug' do
    let(:parser) { flexmock FooParser.new }
    context 'internal' do
      before(:each) do
        # Suppress output.
        #
        parser.should_receive(:puts)
      end
      it 'should exist' do
        lambda { parser.parse_with_debug('anything') }.should_not raise_error
      end
      it 'should catch ParseFailed exceptions' do
        lambda { parser.parse_with_debug('bar') }.should_not raise_error
      end
      it 'should parse correct input like #parse' do
        lambda { parser.parse_with_debug('foo') }.should_not raise_error
      end
    end
    context 'output' do
      it 'should puts once for tree output' do
        parser.should_receive(:puts).once
        
        parser.parse_with_debug('incorrect')
      end
      it "should puts once for the error on unconsumed input" do
        parser.should_receive(:puts).once

        parser.parse_with_debug('foobar')
      end 
    end
    
    it "should work for all parslets" do
      str('foo').parse_with_debug('foo')
      match['bar'].parse_with_debug('a')
    end 
  end
end
