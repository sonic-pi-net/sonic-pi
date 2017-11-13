module Parslet
  # Represents a cause why a parse did fail. A lot of these objects are
  # constructed - not all of the causes turn out to be failures for the whole
  # parse. 
  #
  class Cause
    def initialize(message, source, pos, children)
      @message, @source, @pos, @children = 
        message, source, pos, children
    end
    
    # @return [String, Array] A string or an array of message pieces that 
    #   provide failure information. Use #to_s to get a formatted string.
    attr_reader :message
    
    # @return [Parslet::Source] Source that was parsed when this error 
    #   happend. Mainly used for line number information.
    attr_reader :source
    
    # Location of the error. 
    #
    # @return [Fixnum] Position where the error happened. (character offset)
    attr_reader :pos 
    
    # When this cause is part of a tree of error causes: child nodes for this
    # node. Very often carries the reasons for this cause. 
    #
    # @return [Array<Parslet::Cause>] A list of reasons for this cause. 
    def children
      @children ||= []
    end
    
    # Appends 'at line LINE char CHAR' to the string given. Use +pos+ to
    # override the position of the +source+. This method returns an object
    # that can be turned into a string using #to_s.
    #
    # @param source [Parslet::Source] source that was parsed when this error
    #   happened 
    # @param pos [Fixnum] position of error
    # @param str [String, Array<String>] message parts
    # @param children [Array<Parslet::Cause>] child nodes for this error tree
    # @return [Parslet::Cause] a new instance of {Parslet::Cause}
    #
    def self.format(source, pos, str, children=[])
      self.new(str, source, pos, children)
    end
    
    def to_s
      line, column = source.line_and_column(pos)
      # Allow message to be a list of objects. Join them here, since we now
      # really need it. 
      Array(message).map { |o| 
        o.respond_to?(:to_slice) ? 
          o.str.inspect : 
          o.to_s }.join + " at line #{line} char #{column}."
    end
    
    # Signals to the outside that the parse has failed. Use this in
    # conjunction with .format for nice error messages. 
    #
    def raise(exception_klass=Parslet::ParseFailed)
      exception = exception_klass.new(self.to_s, self)
      Kernel.raise exception
    end

    # Returns an ascii tree representation of the causes of this node and its
    # children. 
    #
    def ascii_tree
      StringIO.new.tap { |io| 
        recursive_ascii_tree(self, io, [true]) }.
        string
    end

  private
    def recursive_ascii_tree(node, stream, curved)
      append_prefix(stream, curved)
      stream.puts node.to_s

      node.children.each do |child|
        last_child = (node.children.last == child)

        recursive_ascii_tree(child, stream, curved + [last_child])
      end
    end
    def append_prefix(stream, curved)
      return if curved.size < 2
      curved[1..-2].each do |c|
        stream.print c ? "   " : "|  "
      end
      stream.print curved.last ? "`- " : "|- "
    end
  end
end