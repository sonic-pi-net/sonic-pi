class MessageBuilder
  
  def initialize
    @address = ""
    @tags = []
    @values = []
    @time = nil
  end
  
  def with_address( addr )
    @address = addr
    self
  end
  
  def with_float( float )
    with_arg( "f", float )
    self
  end
  
  def with_int( int )
    with_arg( "i", int )
    self
  end
  
  def with_string( string )
    with_arg( "s", string )
    self
  end
  
  def with_blob( blob )
    with_arg( "b", blob )
    self
  end
  
  def with_time( time )
    @time = time
  end
  
  def build
    message = OSC::Message.new( @address , *@values)
    message.time = @time
    message
  end
  
private

  def with_arg( tag, value )
    @tags << tag 
    @values << value 
  end
end