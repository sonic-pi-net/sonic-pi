class Parslet::Atoms::Infix < Parslet::Atoms::Base
  attr_reader :element, :operations

  def initialize(element, operations)
    super()

    @element = element
    @operations = operations
  end
  
  def try(source, context, consume_all)
    return catch_error {
      return succ(
        produce_tree(
          precedence_climb(source, context, consume_all)))
    }
  end

  # Turns an array of the form ['1', '+', ['2', '*', '3']] into a hash that
  # reflects the same structure.
  #
  def produce_tree(ary)
    return ary unless ary.kind_of? Array

    left = ary.shift

    until ary.empty?
      op, right = ary.shift(2)

      # p [left, op, right]

      if right.kind_of? Array
        # Subexpression -> Subhash
        left = {l: left, o: op, r: produce_tree(right)}
      else
        left = {l: left, o: op, r: right}
      end
    end

    left
  end

  # A precedence climbing algorithm married to parslet, as described here
  #   http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing/
  # 
  # @note Error handling in this routine is done by throwing :error and 
  #       as a value the error to return to parslet. This avoids cluttering
  #       the recursion logic here with parslet error handling. 
  #
  def precedence_climb(source, context, consume_all, current_prec=1, needs_element=false)
    result = []

    # To even begin parsing an arithmetic expression, there needs to be 
    # at least one @element. 
    success, value = @element.apply(source, context, false)
    
    unless success
      abort context.err(self, source, "#{@element.inspect} was expected", [value])
    end

    result << flatten(value, true)

    # Loop until we fail on operator matching or until input runs out.
    loop do
      op_pos = source.pos
      op_match, prec, assoc = match_operation(source, context, false)

      # If no operator could be matched here, one of several cases 
      # applies: 
      #
      # - end of file
      # - end of expression
      # - syntax error
      # 
      # We abort matching the expression here. 
      break unless op_match

      if prec >= current_prec
        next_prec = (assoc == :left) ? prec+1 : prec

        result << op_match
        result << precedence_climb(
          source, context, consume_all, next_prec, true)
      else
        source.pos = op_pos
        return unwrap(result)
      end
    end

    return unwrap(result)
  end

  def unwrap expr
    expr.size == 1 ? expr.first : expr
  end

  def match_operation(source, context, consume_all)
    errors = []
    @operations.each do |op_atom, prec, assoc|
      success, value = op_atom.apply(source, context, consume_all)
      return flatten(value, true), prec, assoc if success

      # assert: this was in fact an error, accumulate
      errors << value
    end

    return nil
  end

  def abort(error)
    throw :error, error
  end
  def catch_error
    catch(:error) { yield }
  end

  def to_s_inner(prec)
    ops = @operations.map { |o, _, _| o.inspect }.join(', ')
    "infix_expression(#{@element.inspect}, [#{ops}])"
  end
end