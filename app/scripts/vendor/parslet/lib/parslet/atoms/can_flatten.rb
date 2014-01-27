
module Parslet::Atoms
  # A series of helper functions that have the common topic of flattening 
  # result values into the intermediary tree that consists of Ruby Hashes and 
  # Arrays. 
  #
  # This module has one main function, #flatten, that takes an annotated 
  # structure as input and returns the reduced form that users expect from 
  # Atom#parse. 
  #
  # NOTE: Since all of these functions are just that, functions without 
  # side effects, they are in a module and not in a class. Its hard to draw 
  # the line sometimes, but this is beyond. 
  #
  module CanFlatten
    # Takes a mixed value coming out of a parslet and converts it to a return
    # value for the user by dropping things and merging hashes. 
    #
    # Named is set to true if this result will be embedded in a Hash result from 
    # naming something using <code>.as(...)</code>. It changes the folding 
    # semantics of repetition.
    #
    def flatten(value, named=false)
      # Passes through everything that isn't an array of things
      return value unless value.instance_of? Array

      # Extracts the s-expression tag
      tag, *tail = value

      # Merges arrays:
      result = tail.
        map { |e| flatten(e) }            # first flatten each element

      case tag
        when :sequence
          return flatten_sequence(result)
        when :maybe
          return named ? result.first : result.first || ''
        when :repetition
          return flatten_repetition(result, named)
      end

      fail "BUG: Unknown tag #{tag.inspect}."
    end

    # Lisp style fold left where the first element builds the basis for 
    # an inject. 
    #
    def foldl(list, &block)
      return '' if list.empty?
      list[1..-1].inject(list.first, &block)
    end

    # Flatten results from a sequence of parslets. 
    #
    # @api private
    #
    def flatten_sequence(list)
      foldl(list.compact) { |r, e|        # and then merge flat elements
        merge_fold(r, e)
      }
    end
    # @api private 
    def merge_fold(l, r)
      # equal pairs: merge. ----------------------------------------------------
      if l.class == r.class
        if l.is_a?(Hash)
          warn_about_duplicate_keys(l, r)
          return l.merge(r)
        else
          return l + r
        end
      end

      # unequal pairs: hoist to same level. ------------------------------------

      # Maybe classes are not equal, but both are stringlike?
      if l.respond_to?(:to_str) && r.respond_to?(:to_str)
        # if we're merging a String with a Slice, the slice wins. 
        return r if r.respond_to? :to_slice
        return l if l.respond_to? :to_slice

        fail "NOTREACHED: What other stringlike classes are there?"
      end

      # special case: If one of them is a string/slice, the other is more important 
      return l if r.respond_to? :to_str
      return r if l.respond_to? :to_str

      # otherwise just create an array for one of them to live in 
      return l + [r] if r.class == Hash
      return [l] + r if l.class == Hash

      fail "Unhandled case when foldr'ing sequence."
    end

    # Flatten results from a repetition of a single parslet. named indicates
    # whether the user has named the result or not. If the user has named
    # the results, we want to leave an empty list alone - otherwise it is 
    # turned into an empty string. 
    #
    # @api private
    #
    def flatten_repetition(list, named)
      if list.any? { |e| e.instance_of?(Hash) }
        # If keyed subtrees are in the array, we'll want to discard all 
        # strings inbetween. To keep them, name them. 
        return list.select { |e| e.instance_of?(Hash) }
      end

      if list.any? { |e| e.instance_of?(Array) }
        # If any arrays are nested in this array, flatten all arrays to this
        # level. 
        return list.
          select { |e| e.instance_of?(Array) }.
          flatten(1)
      end

      # Consistent handling of empty lists, when we act on a named result        
      return [] if named && list.empty?

      # If there are only strings, concatenate them and return that. 
      foldl(list) { |s,e| s+e }
    end

    # That annoying warning 'Duplicate subtrees while merging result' comes 
    # from here. You should add more '.as(...)' names to your intermediary tree.
    #
    def warn_about_duplicate_keys(h1, h2)
      d = h1.keys & h2.keys
      unless d.empty?
        warn "Duplicate subtrees while merging result of \n  #{self.inspect}\nonly the values"+
             " of the latter will be kept. (keys: #{d.inspect})"
      end
    end
  end
end