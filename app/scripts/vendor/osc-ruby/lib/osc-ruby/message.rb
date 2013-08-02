module OSC
  class Message
    attr_accessor :address
    attr_accessor :time
    attr_accessor :ip_address
    attr_accessor :ip_port

    def self.new_with_time( address, time, tags=nil, *args )
      message = new( address, tags, *args )
      message.time = time
      message
    end

    def initialize( address, *args )
      @address = address
      @args = []

      args.each do |arg|
        case arg
          when Integer;     @args << OSCInt32.new(arg)
          when Float;       @args << OSCFloat32.new(arg)
          when String;      @args << OSCString.new(arg)
          when OSCArgument; @args << arg
        end
      end
    end

    def tags() @args.collect{|x| x.tag}.join end

    def encode
      s = OSCString.new( @address ).encode
      s << OSCString.new( ',' + tags ).encode
      s << @args.collect{|x| x.encode}.join
    end

    def to_a() @args.collect{|x| x.val} end

    def ==( other )
      @address == other.address &&
      to_a == other.to_a
    end
  end
end