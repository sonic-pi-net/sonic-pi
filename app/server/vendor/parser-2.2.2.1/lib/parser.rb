require 'set'
require 'racc/parser'

require 'ast'

if RUBY_VERSION < '1.9'
  require 'parser/compatibility/ruby1_8'
end

if RUBY_VERSION < '2.0'
  require 'parser/compatibility/ruby1_9'
end

##
# @api public
#
module Parser
  require 'parser/version'
  require 'parser/messages'

  module AST
    require 'parser/ast/node'
    require 'parser/ast/processor'
    require 'parser/meta'
  end

  module Source
    require 'parser/source/buffer'
    require 'parser/source/range'

    require 'parser/source/comment'
    require 'parser/source/comment/associator'

    require 'parser/source/rewriter'
    require 'parser/source/rewriter/action'

    require 'parser/source/map'
    require 'parser/source/map/operator'
    require 'parser/source/map/collection'
    require 'parser/source/map/constant'
    require 'parser/source/map/variable'
    require 'parser/source/map/keyword'
    require 'parser/source/map/definition'
    require 'parser/source/map/send'
    require 'parser/source/map/condition'
    require 'parser/source/map/ternary'
    require 'parser/source/map/for'
    require 'parser/source/map/rescue_body'
    require 'parser/source/map/heredoc'
  end

  require 'parser/syntax_error'
  require 'parser/clobbering_error'
  require 'parser/diagnostic'
  require 'parser/diagnostic/engine'

  require 'parser/static_environment'

  require 'parser/lexer'
  require 'parser/lexer/literal'
  require 'parser/lexer/stack_state'

  module Builders
    require 'parser/builders/default'
  end

  require 'parser/base'

  require 'parser/rewriter'

  ##
  # Verify that the current Ruby implementation supports Encoding.
  # @raise [RuntimeError]
  def self.check_for_encoding_support
    unless defined?(Encoding)
      raise RuntimeError, 'Parsing 1.9 and later versions of Ruby is not supported on 1.8 due to the lack of Encoding support'
    end
  end
end
