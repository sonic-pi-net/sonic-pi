
# A mixin module that defines operations that can be called on any subclass
# of Parslet::Atoms::Base. These operations make parslets atoms chainable and 
# allow combination of parslet atoms to form bigger parsers.
#
# Example: 
#
#   str('foo') >> str('bar')
#   str('f').repeat
#   any.absent?               # also called The Epsilon
#
module Parslet::Atoms::DSL
  # Construct a new atom that repeats the current atom min times at least and
  # at most max times. max can be nil to indicate that no maximum is present. 
  #
  # Example: 
  #   # match any number of 'a's
  #   str('a').repeat     
  #
  #   # match between 1 and 3 'a's
  #   str('a').repeat(1,3)
  #
  def repeat(min=0, max=nil)
    Parslet::Atoms::Repetition.new(self, min, max)
  end
  
  # Returns a new parslet atom that is only maybe present in the input. This
  # is synonymous to calling #repeat(0,1). Generated tree value will be 
  # either nil (if atom is not present in the input) or the matched subtree. 
  #
  # Example: 
  #   str('foo').maybe
  #
  def maybe
    Parslet::Atoms::Repetition.new(self, 0, 1, :maybe)
  end

  # Chains two parslet atoms together as a sequence. 
  #
  # Example: 
  #   str('a') >> str('b')
  #
  def >>(parslet)
    Parslet::Atoms::Sequence.new(self, parslet)
  end

  # Chains two parslet atoms together to express alternation. A match will
  # always be attempted with the parslet on the left side first. If it doesn't
  # match, the right side will be tried. 
  #
  # Example:
  #   # matches either 'a' OR 'b'
  #   str('a') | str('b')
  #
  def |(parslet)
    Parslet::Atoms::Alternative.new(self, parslet)
  end
  
  # Tests for absence of a parslet atom in the input stream without consuming
  # it. 
  # 
  # Example: 
  #   # Only proceed the parse if 'a' is absent.
  #   str('a').absent?
  #
  def absent?
    Parslet::Atoms::Lookahead.new(self, false)
  end

  # Tests for presence of a parslet atom in the input stream without consuming
  # it. 
  # 
  # Example: 
  #   # Only proceed the parse if 'a' is present.
  #   str('a').present?
  #
  def present?
    Parslet::Atoms::Lookahead.new(self, true)
  end
  
  # Alias for present? that will disappear in 2.0 (deprecated)
  #
  alias prsnt? present?

  # Alias for absent? that will disappear in 2.0 (deprecated)
  #
  alias absnt? absent?

  # Marks a parslet atom as important for the tree output. This must be used 
  # to achieve meaningful output from the #parse method. 
  #
  # Example:
  #   str('a').as(:b) # will produce {:b => 'a'}
  #
  def as(name)
    Parslet::Atoms::Named.new(self, name)
  end

  # Captures a part of the input and stores it under the name given. This 
  # is very useful to create self-referential parses. A capture stores
  # the result of its parse (may be complex) on a successful parse action.
  # 
  # Example: 
  #   str('a').capture(:b)  # will store captures[:b] == 'a'
  # 
  def capture(name)
    Parslet::Atoms::Capture.new(self, name)
  end
end