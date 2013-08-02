# some test data for when i unit test this
# @simple_packet = "/hi\000,\000\000\000"
# @simple_with_integer_arg = "/hi\000,i\000\000\000\000\000*"
# @simple_with_two_integer_args = "/hi\000,ii\000\000\000\000*\000\000\000!"
# @simple_with_float_arg = "/hi\000,f\000\000B(\n="
# @simple_with_two_float_args = "/hi\000,ff\000B(\n=B\004\n="
# @simple_with_string_arg = "/hi\000,s\000\000greetings\000\000\000"
# @simple_with_two_string_args = "/hi\000,ss\000greetings\000\000\000how are you?\000\000\000\000"
# @simple_with_int_float_string = "/hi\000,ifs\000\000\000\000\000\000\000*B\004\n=greetings\000\000\000"

require File.join( File.dirname(__FILE__) , '..', 'spec_helper' )


describe OSC::Message do
  describe "basic traits" do
    it "should have no arguments if you define none" do
      m = OSC::Message.new( "/hi" )
      m.to_a.should == []
    end

    it "should accept int arguments" do
      m = OSC::Message.new( "/hi", 42 )
      m.to_a.should == [42]
      m.tags.should == "i"
    end

    it "should accept string arguments" do
      m = OSC::Message.new( "/hi", "42" )
      m.to_a.should == ["42"]
      m.tags.should == "s"
    end
    
    it "should accept float arguments" do
      m = OSC::Message.new( "/hi", 42.001 )
      m.to_a.should == [42.001]
      m.tags.should == "f"
    end
  end
  
  describe "message output encoding" do
    it "integer arguments output binary/ascii string" do
      m = OSC::Message.new( "/hi", 42 ).encode
      m.encoding.to_s.should == "ASCII-8BIT"
    end

    it "string arguments output binary/ascii string" do
      m = OSC::Message.new( "/hi", "42" ).encode
      m.encoding.to_s.should == "ASCII-8BIT"
    end

    it "float arguments output binary/ascii string" do
      m = OSC::Message.new( "/hi", 3.14159 ).encode
      m.encoding.to_s.should == "ASCII-8BIT"
    end
  end

  describe "more interesting traits" do
    before :each do
      @builder = MessageBuilder.new
      @builder.with_int( 42 ).
               with_int( 33 )
             
      @message = @builder.build
    end
  
    it "should know equality" do
      @message2 = @builder.build
    
      @message.object_id.should_not == @message2.object_id
      @message.should == @message2
    end
  end
end