module OSC
  class OSCArgument
    def initialize(val) @val = val end

    attr_accessor :val

    def to_i() @val.to_i end
    def to_f() @val.to_f end
    def to_s() @val.to_s end

  private
    def padding(s)
      s + ("\000" * ((4 - (s.size % 4)) % 4))
    end
  end
end