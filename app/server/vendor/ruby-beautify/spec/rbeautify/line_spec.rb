require File.dirname(__FILE__) + '/../spec_helper.rb'

describe RBeautify::Line do

  describe '#format' do

    before(:each) do
      @language = mock(RBeautify::Language)
    end

    it 'should just strip with empty stack' do
      RBeautify::BlockMatcher.stub!(:parse => nil)
      RBeautify::Line.new(@language, ' a = 3 ', 0).format.should == "a = 3"
    end

    it 'should indent with existing indent' do
      current_block = mock('block_start', :total_indent_size => 2, :format_content? => true, :strict_ancestor_of? => false)
      RBeautify::BlockStart.stub(:first_common_ancestor => current_block)
      RBeautify::BlockMatcher.stub!(:parse => current_block)
      RBeautify::Line.new(@language, ' a = 3 ', 0, current_block).format.should == '  a = 3'
    end

    it 'leave empty lines blank' do
      current_block = mock('block_start', :format_content? => true)
      RBeautify::BlockMatcher.stub!(:parse => current_block)
      RBeautify::Line.new(@language, '    ', 0, current_block).format.should == ''
    end

    it 'should remove indent with match to end of block' do
      current_block = mock('block_start', :format_content? => true, :indent_end_line? => false)
      RBeautify::BlockStart.stub(:first_common_ancestor => nil)
      RBeautify::BlockMatcher.stub!(:parse => nil)
      RBeautify::Line.new(@language, '  end ', 0, current_block).format.should == 'end'
    end

    it 'should not remove indent with match to end of block if indent_end_line? is true' do
      current_block = mock('block_start', :total_indent_size => 2, :format_content? => true, :indent_end_line? => true)
      RBeautify::BlockMatcher.stub!(:parse => nil)
      RBeautify::Line.new(@language, '  end ', 0, current_block).format.should == '  end'
    end

    it 'should leave indent at old stack level with match of new block' do
      current_block = mock('current_block_start', :total_indent_size => 2, :format_content? => true)
      new_block = mock('new_block_start', :format_content? => true, :strict_ancestor_of? => false)
      RBeautify::BlockStart.stub(:first_common_ancestor => current_block)
      RBeautify::BlockMatcher.stub!(:parse => new_block)
      RBeautify::Line.new(@language, 'class Foo', 0, current_block).format.should == '  class Foo'
    end

    it 'should remove indent if a block ends and starts' do
      current_block = mock('current_block_start', :format_content? => true)
      new_block = mock('new_block_start', :format_content? => true, :strict_ancestor_of? => false)
      RBeautify::BlockStart.stub(:first_common_ancestor => nil)
      RBeautify::BlockMatcher.stub!(:parse => new_block)
      RBeautify::Line.new(@language, '   else   ', 0, current_block).format.should == 'else'
    end

    it 'should not change when format is false' do
      current_block = mock('block_start', :format_content? => false)
      RBeautify::BlockMatcher.stub!(:parse => current_block)
      RBeautify::Line.new(@language, ' some content after program has finished. ', 0, current_block).format.should ==
        " some content after program has finished. "
    end

    it 'should leave indent with match to end of block (but no format)' do
      current_block = mock('block_start', :format_content? => false)
      RBeautify::BlockMatcher.stub!(:parse => nil)
      RBeautify::Line.new(@language, '  "', 0, current_block).format.should == '  "'
    end

  end

end
