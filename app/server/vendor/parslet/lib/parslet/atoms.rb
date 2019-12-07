
# This is where parslets name comes from: Small parser atoms.
#
module Parslet::Atoms
  # The precedence module controls parenthesis during the #inspect printing
  # of parslets. It is not relevant to other aspects of the parsing. 
  #
  module Precedence
    prec = 0
    BASE       = (prec+=1)    # everything else
    LOOKAHEAD  = (prec+=1)    # &SOMETHING
    REPETITION = (prec+=1)    # 'a'+, 'a'?
    SEQUENCE   = (prec+=1)    # 'a' 'b'
    ALTERNATE  = (prec+=1)    # 'a' | 'b'
    OUTER      = (prec+=1)    # printing is done here.
  end
  
  require 'parslet/atoms/can_flatten'
  require 'parslet/atoms/context'
  require 'parslet/atoms/dsl'
  require 'parslet/atoms/base'
  require 'parslet/atoms/named'
  require 'parslet/atoms/lookahead'
  require 'parslet/atoms/alternative'
  require 'parslet/atoms/sequence'
  require 'parslet/atoms/repetition'
  require 'parslet/atoms/re'
  require 'parslet/atoms/str'
  require 'parslet/atoms/entity'
  require 'parslet/atoms/capture'
  require 'parslet/atoms/dynamic'
  require 'parslet/atoms/scope'
  require 'parslet/atoms/infix'
end

