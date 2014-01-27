module Parslet
  module ErrorReporter
    # An error reporter has two central methods, one for reporting errors at
    # the current parse position (#err) and one for reporting errors at a
    # given parse position (#err_at). The reporter can return an object (a
    # 'cause') that will be returned to the caller along with the information
    # that the parse failed. 
    # 
    # When reporting errors on the outer levels of your parser, these methods
    # get passed a list of error objects ('causes') from the inner levels. In
    # this default implementation, the inner levels are considered error
    # subtrees and are appended to the generated tree node at each level,
    # thereby constructing an error tree. 
    #
    # This error tree will report in parallel with the grammar structure that
    # failed. A one-to-one correspondence exists between each error in the 
    # tree and the parslet atom that produced that error. 
    #
    # The implementor is really free to use these return values as he sees
    # fit. One example would be to return an error state object from these
    # methods that is then updated as errors cascade up the parse derivation
    # tree. 
    #
    class Tree
      # Produces an error cause that combines the message at the current level
      # with the errors that happened at a level below (children).
      #
      # @param atom [Parslet::Atoms::Base] parslet that failed
      # @param source [Source] Source that we're using for this parse. (line 
      #   number information...)
      # @param message [String, Array] Error message at this level.
      # @param children [Array] A list of errors from a deeper level (or nil).
      # @return [Cause] An error tree combining children with message.
      #
      def err(atom, source, message, children=nil)
        position = source.pos
        Cause.format(source, position, message, children)
      end

      # Produces an error cause that combines the message at the current level
      # with the errors that happened at a level below (children).
      #
      # @param atom [Parslet::Atoms::Base] parslet that failed
      # @param source [Source] Source that we're using for this parse. (line 
      #   number information...)
      # @param message [String, Array] Error message at this level.
      # @param pos [Fixnum] The real position of the error.
      # @param children [Array] A list of errors from a deeper level (or nil).
      # @return [Cause] An error tree combining children with message.
      #
      def err_at(atom, source, message, pos, children=nil)
        position = pos
        Cause.format(source, position, message, children)
      end
    end
  end
end