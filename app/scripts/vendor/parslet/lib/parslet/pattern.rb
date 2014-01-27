# Matches trees against expressions. Trees are formed by arrays and hashes
# for expressing membership and sequence. The leafs of the tree are other
# classes. 
#
# A tree issued by the parslet library might look like this: 
#
#   { 
#     :function_call => {
#       :name => 'foobar', 
#       :args => [1, 2, 3]
#     }
#   }
#
# A pattern that would match against this tree would be: 
#
#   { :function_call => { :name => simple(:name), :args => sequence(:args) }}
#
# Note that Parslet::Pattern only matches at a given subtree; it wont try 
# to match recursively. To do that, please use Parslet::Transform. 
#
class Parslet::Pattern  
  def initialize(pattern)
    @pattern = pattern
  end

  # Decides if the given subtree matches this pattern. Returns the bindings
  # made on a successful match or nil if the match fails. If you specify 
  # bindings to be a hash, the mappings in it will be treated like bindings
  # made during an attempted match. 
  #
  #   Pattern.new('a').match('a', :foo => 'bar') # => { :foo => 'bar' }
  #
  # @param subtree [String, Hash, Array] poro subtree returned by a parse
  # @param bindings [Hash] variable bindings to be verified
  # @return [Hash, nil] On success: variable bindings that allow a match. On 
  #   failure: nil
  #
  def match(subtree, bindings=nil)
    bindings = bindings && bindings.dup || Hash.new
    return bindings if element_match(subtree, @pattern, bindings)
  end
  
  # Returns true if the tree element given by +tree+ matches the expression
  # given by +exp+. This match must respect bindings already made in
  # +bindings+. Note that bindings is carried along and modified. 
  #
  # @api private
  #
  def element_match(tree, exp, bindings) 
    # p [:elm, tree, exp]
    case [tree, exp].map { |e| e.class }
      when [Hash,Hash]
        return element_match_hash(tree, exp, bindings)
      when [Array,Array]
        return element_match_ary_single(tree, exp, bindings)
    else
      # If elements match exactly, then that is good enough in all cases
      return true if exp === tree
      
      # If exp is a bind variable: Check if the binding matches
      if exp.respond_to?(:can_bind?) && exp.can_bind?(tree)
        return element_match_binding(tree, exp, bindings)
      end
                  
      # Otherwise: No match (we don't know anything about the element
      # combination)
      return false
    end
  end
  
  # @api private
  #
  def element_match_binding(tree, exp, bindings)
    var_name = exp.variable_name

    # TODO test for the hidden :_ feature.
    if var_name && bound_value = bindings[var_name]
      return bound_value == tree
    end
    
    # New binding: 
    bindings.store var_name, tree
    
    return true
  end
  
  # @api private
  #
  def element_match_ary_single(sequence, exp, bindings)
    return false if sequence.size != exp.size
    
    return sequence.zip(exp).all? { |elt, subexp|
      element_match(elt, subexp, bindings) }
  end
  
  # @api private
  #
  def element_match_hash(tree, exp, bindings)
    # Early failure when one hash is bigger than the other
    return false unless exp.size == tree.size
    
    # We iterate over expected pattern, since we demand that the keys that
    # are there should be in tree as well.
    exp.each do |expected_key, expected_value|
      return false unless tree.has_key? expected_key
      
      # Recurse into the value and stop early on failure
      value = tree[expected_key]
      return false unless element_match(value, expected_value, bindings)
    end
    
    return true
  end  
end