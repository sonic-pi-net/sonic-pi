require File.join( File.dirname(__FILE__) , '..', 'spec_helper' )


describe OSC::NetworkPacket do
  before :each do
    @empty = OSC::NetworkPacket.new( "" )
    @simple = OSC::NetworkPacket.new( "abc" )
  end
  
  it "should know if it's at the end of the stream" do
    @empty.eof?.should be_true
  end
  
  it "should know the remainder in the stream" do
    @simple.rem.should == 3
  end
  
  it "should be able to skip positions" do
    @simple.skip( 1 )
    @simple.rem.should == 2
  end
  
  it "should be able to get a character from the stream" do
    @simple.getc.should == ?a
    @simple.getc.should == ?b
    @simple.getc.should == ?c
    @simple.eof?.should be_true
  end
  
  it "should be able to get a number of characters from the stream" do
    @simple.getn(3).should == "abc"
  end

  it "outputs characters with ASCII/BINARY encoding" do
    @simple.getc.encoding.to_s.should == "ASCII-8BIT"
  end
end
