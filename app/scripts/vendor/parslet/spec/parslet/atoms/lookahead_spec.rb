require 'spec_helper'

describe Parslet::Atoms::Lookahead do
  include Parslet
  
  describe 'negative lookahead' do
    it "influences the error tree" do
      parser = str('f').absent? >> str('b')
      cause = catch_failed_parse { parser.parse('f') }
      
      cause.ascii_tree.should == "Failed to match sequence (!'f' 'b') at line 1 char 1.\n`- Input should not start with 'f' at line 1 char 1.\n"
    end 
  end
  describe 'positive lookahead' do
    it "influences the error tree" do
      parser = str('f').present? >> str('b')
      cause = catch_failed_parse { parser.parse('b') }
      
      cause.ascii_tree.should == "Failed to match sequence (&'f' 'b') at line 1 char 1.\n`- Input should start with 'f' at line 1 char 1.\n"
    end 
  end
end