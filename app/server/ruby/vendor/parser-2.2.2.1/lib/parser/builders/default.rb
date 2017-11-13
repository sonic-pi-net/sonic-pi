module Parser

  ##
  # Default AST builder. Uses {AST::Node}s.
  #
  class Builders::Default
    ##
    # @api private
    attr_accessor :parser

    ##
    # If set to true, `__FILE__` and `__LINE__` are transformed to
    # literal nodes. For example, `s(:str, "lib/foo.rb")` and `s(:int, 10)`.
    #
    # If set to false, `__FILE__` and `__LINE__` are emitted as-is,
    # i.e. as `s(:__FILE__)` and `s(:__LINE__)` nodes.
    #
    # Source maps are identical in both cases.
    #
    # @return [Boolean]
    attr_accessor :emit_file_line_as_literals

    ##
    # Initializes attributes:
    #
    #   * `emit_file_line_as_literals`: `true`
    def initialize
      @emit_file_line_as_literals = true
    end

    # @!parse private

    #
    # Literals
    #

    # Singletons

    def nil(nil_t)
      n0(:nil,
        token_map(nil_t))
    end

    def true(true_t)
      n0(:true,
        token_map(true_t))
    end

    def false(false_t)
      n0(:false,
        token_map(false_t))
    end

    # Numerics

    def integer(integer_t)
      numeric(:int, integer_t)
    end

    def float(float_t)
      numeric(:float, float_t)
    end

    def rational(rational_t)
      numeric(:rational, rational_t)
    end

    def complex(complex_t)
      numeric(:complex, complex_t)
    end

    def numeric(kind, token)
      n(kind, [ value(token) ],
        Source::Map::Operator.new(nil, loc(token)))
    end
    private :numeric

    def negate(uminus_t, numeric)
      value, = *numeric
      operator_loc = loc(uminus_t)

      numeric.updated(nil, [ -value ],
        :location =>
          Source::Map::Operator.new(
            operator_loc,
            operator_loc.join(numeric.loc.expression)))
    end

    def __LINE__(__LINE__t)
      n0(:__LINE__,
        token_map(__LINE__t))
    end

    # Strings

    def string(string_t)
      n(:str, [ value(string_t) ],
        delimited_string_map(string_t))
    end

    def string_internal(string_t)
      n(:str, [ value(string_t) ],
        unquoted_map(string_t))
    end

    def string_compose(begin_t, parts, end_t)
      if collapse_string_parts?(parts)
        if begin_t.nil? && end_t.nil?
          parts.first
        else
          n(:str, parts.first.children,
            string_map(begin_t, parts, end_t))
        end
      else
        n(:dstr, [ *parts ],
          string_map(begin_t, parts, end_t))
      end
    end

    def character(char_t)
      n(:str, [ value(char_t) ],
        prefix_string_map(char_t))
    end

    def __FILE__(__FILE__t)
      n0(:__FILE__,
        token_map(__FILE__t))
    end

    # Symbols

    def symbol(symbol_t)
      n(:sym, [ value(symbol_t).to_sym ],
        prefix_string_map(symbol_t))
    end

    def symbol_internal(symbol_t)
      n(:sym, [ value(symbol_t).to_sym ],
        unquoted_map(symbol_t))
    end

    def symbol_compose(begin_t, parts, end_t)
      if collapse_string_parts?(parts)
        str = parts.first

        n(:sym, [ str.children.first.to_sym ],
          collection_map(begin_t, str.loc.expression, end_t))
      elsif @parser.version == 18 && parts.empty?
        diagnostic :error, :empty_symbol, nil, loc(begin_t).join(loc(end_t))
      else
        n(:dsym, [ *parts ],
          collection_map(begin_t, parts, end_t))
      end
    end

    # Executable strings

    def xstring_compose(begin_t, parts, end_t)
      n(:xstr, [ *parts ],
        string_map(begin_t, parts, end_t))
    end

    # Regular expressions

    def regexp_options(regopt_t)
      options = value(regopt_t).
        each_char.sort.uniq.
        map(&:to_sym)

      n(:regopt, options,
        token_map(regopt_t))
    end

    def regexp_compose(begin_t, parts, end_t, options)
      begin
        static_regexp(parts, options)
      rescue RegexpError => e
        diagnostic :error, :invalid_regexp, { :message => e.message },
                   loc(begin_t).join(loc(end_t))
      end

      n(:regexp, (parts << options),
        regexp_map(begin_t, end_t, options))
    end

    # Arrays

    def array(begin_t, elements, end_t)
      n(:array, elements,
        collection_map(begin_t, elements, end_t))
    end

    def splat(star_t, arg=nil)
      if arg.nil?
        n0(:splat,
          unary_op_map(star_t))
      else
        n(:splat, [ arg ],
          unary_op_map(star_t, arg))
      end
    end

    def word(parts)
      if collapse_string_parts?(parts)
        parts.first
      else
        n(:dstr, [ *parts ],
          collection_map(nil, parts, nil))
      end
    end

    def words_compose(begin_t, parts, end_t)
      n(:array, [ *parts ],
        collection_map(begin_t, parts, end_t))
    end

    def symbols_compose(begin_t, parts, end_t)
      parts = parts.map do |part|
        case part.type
        when :str
          value, = *part
          part.updated(:sym, [ value.to_sym ])
        when :dstr
          part.updated(:dsym)
        else
          part
        end
      end

      n(:array, [ *parts ],
        collection_map(begin_t, parts, end_t))
    end

    # Hashes

    def pair(key, assoc_t, value)
      n(:pair, [ key, value ],
        binary_op_map(key, assoc_t, value))
    end

    def pair_list_18(list)
      if list.size % 2 != 0
        diagnostic :error, :odd_hash, nil, list.last.loc.expression
      else
        list.
          each_slice(2).map do |key, value|
            n(:pair, [ key, value ],
              binary_op_map(key, nil, value))
          end
      end
    end

    def pair_keyword(key_t, value)
      key_map, pair_map = pair_keyword_map(key_t, value)

      key = n(:sym, [ value(key_t).to_sym ], key_map)

      n(:pair, [ key, value ], pair_map)
    end

    def pair_quoted(begin_t, parts, end_t, value)
      end_t, pair_map = pair_quoted_map(begin_t, end_t, value)

      key = symbol_compose(begin_t, parts, end_t)

      n(:pair, [ key, value ], pair_map)
    end

    def kwsplat(dstar_t, arg)
      n(:kwsplat, [ arg ],
        unary_op_map(dstar_t, arg))
    end

    def associate(begin_t, pairs, end_t)
      n(:hash, [ *pairs ],
        collection_map(begin_t, pairs, end_t))
    end

    # Ranges

    def range_inclusive(lhs, dot2_t, rhs)
      n(:irange, [ lhs, rhs ],
        binary_op_map(lhs, dot2_t, rhs))
    end

    def range_exclusive(lhs, dot3_t, rhs)
      n(:erange, [ lhs, rhs ],
        binary_op_map(lhs, dot3_t, rhs))
    end

    #
    # Access
    #

    def self(token)
      n0(:self,
        token_map(token))
    end

    def ident(token)
      n(:ident, [ value(token).to_sym ],
        variable_map(token))
    end

    def ivar(token)
      n(:ivar, [ value(token).to_sym ],
        variable_map(token))
    end

    def gvar(token)
      n(:gvar, [ value(token).to_sym ],
        variable_map(token))
    end

    def cvar(token)
      n(:cvar, [ value(token).to_sym ],
        variable_map(token))
    end

    def back_ref(token)
      n(:back_ref, [ value(token).to_sym ],
        token_map(token))
    end

    def nth_ref(token)
      n(:nth_ref, [ value(token) ],
        token_map(token))
    end

    def accessible(node)
      case node.type
      when :__FILE__
        if @emit_file_line_as_literals
          n(:str, [ node.loc.expression.source_buffer.name ],
            node.loc.dup)
        else
          node
        end

      when :__LINE__
        if @emit_file_line_as_literals
          n(:int, [ node.loc.expression.line ],
            node.loc.dup)
        else
          node
        end

      when :__ENCODING__
        n(:const, [ n(:const, [ nil, :Encoding], nil), :UTF_8 ],
          node.loc.dup)

      when :ident
        name, = *node

        if @parser.static_env.declared?(name)
          node.updated(:lvar)
        else
          name, = *node
          n(:send, [ nil, name ],
            var_send_map(node))
        end

      else
        node
      end
    end

    def const(name_t)
      n(:const, [ nil, value(name_t).to_sym ],
        constant_map(nil, nil, name_t))
    end

    def const_global(t_colon3, name_t)
      cbase = n0(:cbase, token_map(t_colon3))

      n(:const, [ cbase, value(name_t).to_sym ],
        constant_map(cbase, t_colon3, name_t))
    end

    def const_fetch(scope, t_colon2, name_t)
      n(:const, [ scope, value(name_t).to_sym ],
        constant_map(scope, t_colon2, name_t))
    end

    def __ENCODING__(__ENCODING__t)
      n0(:__ENCODING__,
        token_map(__ENCODING__t))
    end

    #
    # Assignment
    #

    def assignable(node)
      case node.type
      when :cvar
        node.updated(:cvasgn)

      when :ivar
        node.updated(:ivasgn)

      when :gvar
        node.updated(:gvasgn)

      when :const
        if @parser.in_def?
          diagnostic :error, :dynamic_const, nil, node.loc.expression
        end

        node.updated(:casgn)

      when :ident
        name, = *node
        @parser.static_env.declare(name)

        node.updated(:lvasgn)

      when :nil, :self, :true, :false,
           :__FILE__, :__LINE__, :__ENCODING__
        diagnostic :error, :invalid_assignment, nil, node.loc.expression

      when :back_ref, :nth_ref
        diagnostic :error, :backref_assignment, nil, node.loc.expression
      end
    end

    def const_op_assignable(node)
      node.updated(:casgn)
    end

    def assign(lhs, eql_t, rhs)
      (lhs << rhs).updated(nil, nil,
        :location => lhs.loc.
          with_operator(loc(eql_t)).
          with_expression(join_exprs(lhs, rhs)))
    end

    def op_assign(lhs, op_t, rhs)
      case lhs.type
      when :gvasgn, :ivasgn, :lvasgn, :cvasgn, :casgn, :send
        operator   = value(op_t)[0..-1].to_sym
        source_map = lhs.loc.
                        with_operator(loc(op_t)).
                        with_expression(join_exprs(lhs, rhs))

        case operator
        when :'&&'
          n(:and_asgn, [ lhs, rhs ], source_map)
        when :'||'
          n(:or_asgn, [ lhs, rhs ], source_map)
        else
          n(:op_asgn, [ lhs, operator, rhs ], source_map)
        end

      when :back_ref, :nth_ref
        diagnostic :error, :backref_assignment, nil, lhs.loc.expression
      end
    end

    def multi_lhs(begin_t, items, end_t)
      n(:mlhs, [ *items ],
        collection_map(begin_t, items, end_t))
    end

    def multi_assign(lhs, eql_t, rhs)
      n(:masgn, [ lhs, rhs ],
        binary_op_map(lhs, eql_t, rhs))
    end

    #
    # Class and module definition
    #

    def def_class(class_t, name,
                  lt_t, superclass,
                  body, end_t)
      n(:class, [ name, superclass, body ],
        module_definition_map(class_t, name, lt_t, end_t))
    end

    def def_sclass(class_t, lshft_t, expr,
                   body, end_t)
      n(:sclass, [ expr, body ],
        module_definition_map(class_t, nil, lshft_t, end_t))
    end

    def def_module(module_t, name,
                   body, end_t)
      n(:module, [ name, body ],
        module_definition_map(module_t, name, nil, end_t))
    end

    #
    # Method (un)definition
    #

    def def_method(def_t, name_t, args,
                   body, end_t)
      n(:def, [ value(name_t).to_sym, args, body ],
        definition_map(def_t, nil, name_t, end_t))
    end

    def def_singleton(def_t, definee, dot_t,
                      name_t, args,
                      body, end_t)
      case definee.type
      when :int, :str, :dstr, :sym, :dsym,
           :regexp, :array, :hash

        diagnostic :error, :singleton_literal, nil, definee.loc.expression

      else
        n(:defs, [ definee, value(name_t).to_sym, args, body ],
          definition_map(def_t, dot_t, name_t, end_t))
      end
    end

    def undef_method(undef_t, names)
      n(:undef, [ *names ],
        keyword_map(undef_t, nil, names, nil))
    end

    def alias(alias_t, to, from)
      n(:alias, [ to, from ],
        keyword_map(alias_t, nil, [to, from], nil))
    end

    #
    # Formal arguments
    #

    def args(begin_t, args, end_t, check_args=true)
      args = check_duplicate_args(args) if check_args
      n(:args, args,
        collection_map(begin_t, args, end_t))
    end

    def arg(name_t)
      n(:arg, [ value(name_t).to_sym ],
        variable_map(name_t))
    end

    def optarg(name_t, eql_t, value)
      n(:optarg, [ value(name_t).to_sym, value ],
        variable_map(name_t).
          with_operator(loc(eql_t)).
          with_expression(loc(name_t).join(value.loc.expression)))
    end

    def restarg(star_t, name_t=nil)
      if name_t
        n(:restarg, [ value(name_t).to_sym ],
          arg_prefix_map(star_t, name_t))
      else
        n0(:restarg,
          arg_prefix_map(star_t))
      end
    end

    def kwarg(name_t)
      n(:kwarg, [ value(name_t).to_sym ],
        kwarg_map(name_t))
    end

    def kwoptarg(name_t, value)
      n(:kwoptarg, [ value(name_t).to_sym, value ],
        kwarg_map(name_t, value))
    end

    def kwrestarg(dstar_t, name_t=nil)
      if name_t
        n(:kwrestarg, [ value(name_t).to_sym ],
          arg_prefix_map(dstar_t, name_t))
      else
        n0(:kwrestarg,
          arg_prefix_map(dstar_t))
      end
    end

    def shadowarg(name_t)
      n(:shadowarg, [ value(name_t).to_sym ],
        variable_map(name_t))
    end

    def blockarg(amper_t, name_t)
      n(:blockarg, [ value(name_t).to_sym ],
        arg_prefix_map(amper_t, name_t))
    end

    # Ruby 1.8 block arguments

    def arg_expr(expr)
      if expr.type == :lvasgn
        expr.updated(:arg)
      else
        n(:arg_expr, [ expr ],
          expr.loc.dup)
      end
    end

    def restarg_expr(star_t, expr=nil)
      if expr.nil?
        n0(:restarg, token_map(star_t))
      elsif expr.type == :lvasgn
        expr.updated(:restarg)
      else
        n(:restarg_expr, [ expr ],
          expr.loc.dup)
      end
    end

    def blockarg_expr(amper_t, expr)
      if expr.type == :lvasgn
        expr.updated(:blockarg)
      else
        n(:blockarg_expr, [ expr ],
          expr.loc.dup)
      end
    end

    #
    # Method calls
    #

    def call_method(receiver, dot_t, selector_t,
                    lparen_t=nil, args=[], rparen_t=nil)
      if selector_t.nil?
        n(:send, [ receiver, :call, *args ],
          send_map(receiver, dot_t, nil, lparen_t, args, rparen_t))
      else
        n(:send, [ receiver, value(selector_t).to_sym, *args ],
          send_map(receiver, dot_t, selector_t, lparen_t, args, rparen_t))
      end
    end

    def call_lambda(lambda_t)
      n(:send, [ nil, :lambda ],
        send_map(nil, nil, lambda_t))
    end

    def block(method_call, begin_t, args, body, end_t)
      _receiver, _selector, *call_args = *method_call

      if method_call.type == :yield
        diagnostic :error, :block_given_to_yield, nil, method_call.loc.keyword, [loc(begin_t)]
      end

      last_arg = call_args.last
      if last_arg && last_arg.type == :block_pass
        diagnostic :error, :block_and_blockarg, nil, last_arg.loc.expression, [loc(begin_t)]
      end

      if [:send, :super, :zsuper].include?(method_call.type)
        n(:block, [ method_call, args, body ],
          block_map(method_call.loc.expression, begin_t, end_t))
      else
        # Code like "return foo 1 do end" is reduced in a weird sequence.
        # Here, method_call is actually (return).
        actual_send, = *method_call
        block =
          n(:block, [ actual_send, args, body ],
            block_map(actual_send.loc.expression, begin_t, end_t))

        n(method_call.type, [ block ],
          method_call.loc.with_expression(join_exprs(method_call, block)))
      end
    end

    def block_pass(amper_t, arg)
      n(:block_pass, [ arg ],
        unary_op_map(amper_t, arg))
    end

    def attr_asgn(receiver, dot_t, selector_t)
      method_name = (value(selector_t) + '=').to_sym

      # Incomplete method call.
      n(:send, [ receiver, method_name ],
        send_map(receiver, dot_t, selector_t))
    end

    def index(receiver, lbrack_t, indexes, rbrack_t)
      n(:send, [ receiver, :[], *indexes ],
        send_index_map(receiver, lbrack_t, rbrack_t))
    end

    def index_asgn(receiver, lbrack_t, indexes, rbrack_t)
      # Incomplete method call.
      n(:send, [ receiver, :[]=, *indexes ],
        send_index_map(receiver, lbrack_t, rbrack_t))
    end

    def binary_op(receiver, operator_t, arg)
      source_map = send_binary_op_map(receiver, operator_t, arg)

      if @parser.version == 18
        operator = value(operator_t)

        if operator == '!='
          method_call = n(:send, [ receiver, :==, arg ], source_map)
        elsif operator == '!~'
          method_call = n(:send, [ receiver, :=~, arg ], source_map)
        end

        if %w(!= !~).include?(operator)
          return n(:not, [ method_call ],
                   expr_map(source_map.expression))
        end
      end

      n(:send, [ receiver, value(operator_t).to_sym, arg ],
        source_map)
    end

    def match_op(receiver, match_t, arg)
      source_map = send_binary_op_map(receiver, match_t, arg)

      if (regexp = static_regexp_node(receiver))
        regexp.names.each do |name|
          @parser.static_env.declare(name)
        end

        n(:match_with_lvasgn, [ receiver, arg ],
          source_map)
      else
        n(:send, [ receiver, :=~, arg ],
          source_map)
      end
    end

    def unary_op(op_t, receiver)
      case value(op_t)
      when '+', '-'
        method = value(op_t) + '@'
      else
        method = value(op_t)
      end

      n(:send, [ receiver, method.to_sym ],
        send_unary_op_map(op_t, receiver))
    end

    def not_op(not_t, begin_t=nil, receiver=nil, end_t=nil)
      if @parser.version == 18
        n(:not, [ receiver ],
          unary_op_map(not_t, receiver))
      else
        if receiver.nil?
          nil_node = n0(:begin, collection_map(begin_t, nil, end_t))

          n(:send, [
            nil_node, :'!'
          ], send_unary_op_map(not_t, nil_node))
        else
          n(:send, [ receiver, :'!' ],
            send_map(nil, nil, not_t, begin_t, [receiver], end_t))
        end
      end
    end

    #
    # Control flow
    #

    # Logical operations: and, or

    def logical_op(type, lhs, op_t, rhs)
      n(type, [ lhs, rhs ],
        binary_op_map(lhs, op_t, rhs))
    end

    # Conditionals

    def condition(cond_t, cond, then_t,
                  if_true, else_t, if_false, end_t)
      n(:if, [ check_condition(cond), if_true, if_false ],
        condition_map(cond_t, cond, then_t, if_true, else_t, if_false, end_t))
    end

    def condition_mod(if_true, if_false, cond_t, cond)
      n(:if, [ check_condition(cond), if_true, if_false ],
        keyword_mod_map(if_true || if_false, cond_t, cond))
    end

    def ternary(cond, question_t, if_true, colon_t, if_false)
      n(:if, [ check_condition(cond), if_true, if_false ],
        ternary_map(cond, question_t, if_true, colon_t, if_false))
    end

    # Case matching

    def when(when_t, patterns, then_t, body)
      children = patterns << body
      n(:when, children,
        keyword_map(when_t, then_t, children, nil))
    end

    def case(case_t, expr, when_bodies, else_t, else_body, end_t)
      n(:case, [ expr, *(when_bodies << else_body)],
        condition_map(case_t, expr, nil, nil, else_t, else_body, end_t))
    end

    # Loops

    def loop(type, keyword_t, cond, do_t, body, end_t)
      n(type, [ check_condition(cond), body ],
        keyword_map(keyword_t, do_t, nil, end_t))
    end

    def loop_mod(type, body, keyword_t, cond)
      if body.type == :kwbegin
        type = :"#{type}_post"
      end

      n(type, [ check_condition(cond), body ],
        keyword_mod_map(body, keyword_t, cond))
    end

    def for(for_t, iterator, in_t, iteratee,
            do_t, body, end_t)
      n(:for, [ iterator, iteratee, body ],
        for_map(for_t, in_t, do_t, end_t))
    end

    # Keywords

    def keyword_cmd(type, keyword_t, lparen_t=nil, args=[], rparen_t=nil)
      if type == :yield && args.count > 0
        last_arg = args.last
        if last_arg.type == :block_pass
          diagnostic :error, :block_given_to_yield, nil, loc(keyword_t), [last_arg.loc.expression]
        end
      end

      n(type, args,
        keyword_map(keyword_t, lparen_t, args, rparen_t))
    end

    # BEGIN, END

    def preexe(preexe_t, lbrace_t, compstmt, rbrace_t)
      n(:preexe, [ compstmt ],
        keyword_map(preexe_t, lbrace_t, [], rbrace_t))
    end

    def postexe(postexe_t, lbrace_t, compstmt, rbrace_t)
      n(:postexe, [ compstmt ],
        keyword_map(postexe_t, lbrace_t, [], rbrace_t))
    end

    # Exception handling

    def rescue_body(rescue_t,
                    exc_list, assoc_t, exc_var,
                    then_t, compound_stmt)
      n(:resbody, [ exc_list, exc_var, compound_stmt ],
        rescue_body_map(rescue_t, exc_list, assoc_t,
                        exc_var, then_t, compound_stmt))
    end

    def begin_body(compound_stmt, rescue_bodies=[],
                   else_t=nil,    else_=nil,
                   ensure_t=nil,  ensure_=nil)
      if rescue_bodies.any?
        if else_t
          compound_stmt =
            n(:rescue,
              [ compound_stmt, *(rescue_bodies + [ else_ ]) ],
              eh_keyword_map(compound_stmt, nil, rescue_bodies, else_t, else_))
        else
          compound_stmt =
            n(:rescue,
              [ compound_stmt, *(rescue_bodies + [ nil ]) ],
              eh_keyword_map(compound_stmt, nil, rescue_bodies, nil, nil))
        end
      end

      if ensure_t
        compound_stmt =
          n(:ensure,
            [ compound_stmt, ensure_ ],
            eh_keyword_map(compound_stmt, ensure_t, [ ensure_ ], nil, nil))
      end

      compound_stmt
    end

    #
    # Expression grouping
    #

    def compstmt(statements)
      case
      when statements.none?
        nil
      when statements.one?
        statements.first
      else
        n(:begin, statements,
          collection_map(nil, statements, nil))
      end
    end

    def begin(begin_t, body, end_t)
      if body.nil?
        # A nil expression: `()'.
        n0(:begin,
          collection_map(begin_t, nil, end_t))
      elsif body.type == :mlhs  ||
           (body.type == :begin &&
            body.loc.begin.nil? && body.loc.end.nil?)
        # Synthesized (begin) from compstmt "a; b" or (mlhs)
        # from multi_lhs "(a, b) = *foo".
        n(body.type, body.children,
          collection_map(begin_t, body.children, end_t))
      else
        n(:begin, [ body ],
          collection_map(begin_t, [ body ], end_t))
      end
    end

    def begin_keyword(begin_t, body, end_t)
      if body.nil?
        # A nil expression: `begin end'.
        n0(:kwbegin,
          collection_map(begin_t, nil, end_t))
      elsif (body.type == :begin &&
             body.loc.begin.nil? && body.loc.end.nil?)
        # Synthesized (begin) from compstmt "a; b".
        n(:kwbegin, body.children,
          collection_map(begin_t, body.children, end_t))
      else
        n(:kwbegin, [ body ],
          collection_map(begin_t, [ body ], end_t))
      end
    end

    private

    #
    # VERIFICATION
    #

    def check_condition(cond)
      case cond.type
      when :masgn
        diagnostic :error, :masgn_as_condition, nil, cond.loc.expression

      when :begin
        if cond.children.count == 1
          cond.updated(nil, [
            check_condition(cond.children.last)
          ])
        else
          cond
        end

      when :and, :or, :irange, :erange
        lhs, rhs = *cond

        type = case cond.type
        when :irange then :iflipflop
        when :erange then :eflipflop
        end

        if [:and, :or].include?(cond.type) &&
               @parser.version == 18
          cond
        else
          cond.updated(type, [
            check_condition(lhs),
            check_condition(rhs)
          ])
        end

      when :regexp
        n(:match_current_line, [ cond ], expr_map(cond.loc.expression))

      else
        cond
      end
    end

    def check_duplicate_args(args, map={})
      args.each do |this_arg|
        case this_arg.type
        when :arg, :optarg, :restarg, :blockarg,
             :kwarg, :kwoptarg, :kwrestarg,
             :shadowarg

          this_name, = *this_arg

          that_arg   = map[this_name]
          that_name, = *that_arg

          if that_arg.nil?
            map[this_name] = this_arg
          elsif arg_name_collides?(this_name, that_name)
            diagnostic :error, :duplicate_argument, nil,
                       this_arg.loc.name, [ that_arg.loc.name ]
          end

        when :mlhs
          check_duplicate_args(this_arg.children, map)
        end
      end
    end

    def arg_name_collides?(this_name, that_name)
      case @parser.version
      when 18
        this_name == that_name
      when 19
        # Ignore underscore.
        this_name != :_ &&
          this_name == that_name
      else
        # Ignore everything beginning with underscore.
        this_name && this_name[0] != '_' &&
          this_name == that_name
      end
    end

    #
    # SOURCE MAPS
    #

    def n(type, children, source_map)
      AST::Node.new(type, children, :location => source_map)
    end

    def n0(type, source_map)
      n(type, [], source_map)
    end

    def join_exprs(left_expr, right_expr)
      left_expr.loc.expression.
        join(right_expr.loc.expression)
    end

    def token_map(token)
      Source::Map.new(loc(token))
    end

    def delimited_string_map(string_t)
      str_range = loc(string_t)

      begin_l = Source::Range.new(str_range.source_buffer,
                                  str_range.begin_pos,
                                  str_range.begin_pos + 1)

      end_l   = Source::Range.new(str_range.source_buffer,
                                  str_range.end_pos - 1,
                                  str_range.end_pos)

      Source::Map::Collection.new(begin_l, end_l,
                                  loc(string_t))
    end

    def prefix_string_map(symbol)
      str_range = loc(symbol)

      begin_l = Source::Range.new(str_range.source_buffer,
                                  str_range.begin_pos,
                                  str_range.begin_pos + 1)

      Source::Map::Collection.new(begin_l, nil,
                                  loc(symbol))
    end

    def unquoted_map(token)
      Source::Map::Collection.new(nil, nil,
                                  loc(token))
    end

    def pair_keyword_map(key_t, value_e)
      key_range = loc(key_t)

      key_l   = Source::Range.new(key_range.source_buffer,
                                  key_range.begin_pos,
                                  key_range.end_pos - 1)

      colon_l = Source::Range.new(key_range.source_buffer,
                                  key_range.end_pos - 1,
                                  key_range.end_pos)

      [ # key map
        Source::Map::Collection.new(nil, nil,
                                    key_l),
        # pair map
        Source::Map::Operator.new(colon_l,
                                  key_range.join(value_e.loc.expression)) ]
    end

    def pair_quoted_map(begin_t, end_t, value_e)
      end_l = loc(end_t)

      quote_l = Source::Range.new(end_l.source_buffer,
                                  end_l.end_pos - 2,
                                  end_l.end_pos - 1)

      colon_l = Source::Range.new(end_l.source_buffer,
                                  end_l.end_pos - 1,
                                  end_l.end_pos)

      [ # modified end token
        [ value(end_t), quote_l ],
        # pair map
        Source::Map::Operator.new(colon_l,
                                  loc(begin_t).join(value_e.loc.expression)) ]
    end

    def expr_map(loc)
      Source::Map.new(loc)
    end

    def collection_map(begin_t, parts, end_t)
      if begin_t.nil? || end_t.nil?
        if parts.any?
          expr_l = join_exprs(parts.first, parts.last)
        end
      else
        expr_l = loc(begin_t).join(loc(end_t))
      end

      Source::Map::Collection.new(loc(begin_t), loc(end_t), expr_l)
    end

    def string_map(begin_t, parts, end_t)
      if begin_t && value(begin_t).start_with?('<<')
        if parts.any?
          expr_l = join_exprs(parts.first, parts.last)
        else
          expr_l = loc(end_t).begin
        end

        Source::Map::Heredoc.new(loc(begin_t), expr_l, loc(end_t))
      else
        collection_map(begin_t, parts, end_t)
      end
    end

    def regexp_map(begin_t, end_t, options_e)
      Source::Map::Collection.new(loc(begin_t), loc(end_t),
                                  loc(begin_t).join(options_e.loc.expression))
    end

    def constant_map(scope, colon2_t, name_t)
      if scope.nil?
        expr_l = loc(name_t)
      else
        expr_l = scope.loc.expression.join(loc(name_t))
      end

      Source::Map::Constant.new(loc(colon2_t), loc(name_t), expr_l)
    end

    def variable_map(name_t)
      Source::Map::Variable.new(loc(name_t))
    end

    def binary_op_map(left_e, op_t, right_e)
      Source::Map::Operator.new(loc(op_t), join_exprs(left_e, right_e))
    end

    def unary_op_map(op_t, arg_e=nil)
      if arg_e.nil?
        expr_l = loc(op_t)
      else
        expr_l = loc(op_t).join(arg_e.loc.expression)
      end

      Source::Map::Operator.new(loc(op_t), expr_l)
    end

    def arg_prefix_map(op_t, name_t=nil)
      if name_t.nil?
        expr_l = loc(op_t)
      else
        expr_l = loc(op_t).join(loc(name_t))
      end

      Source::Map::Variable.new(loc(name_t), expr_l)
    end

    def kwarg_map(name_t, value_e=nil)
      label_range = loc(name_t)
      name_range  = Source::Range.new(label_range.source_buffer,
                                      label_range.begin_pos,
                                      label_range.end_pos - 1)

      if value_e
        expr_l = loc(name_t).join(value_e.loc.expression)
      else
        expr_l = loc(name_t)
      end

      Source::Map::Variable.new(name_range, expr_l)
    end

    def module_definition_map(keyword_t, name_e, operator_t, end_t)
      if name_e
        name_l = name_e.loc.expression
      end

      Source::Map::Definition.new(loc(keyword_t),
                                  loc(operator_t), name_l,
                                  loc(end_t))
    end

    def definition_map(keyword_t, operator_t, name_t, end_t)
      Source::Map::Definition.new(loc(keyword_t),
                                  loc(operator_t), loc(name_t),
                                  loc(end_t))
    end

    def send_map(receiver_e, dot_t, selector_t, begin_t=nil, args=[], end_t=nil)
      if receiver_e
        begin_l = receiver_e.loc.expression
      elsif selector_t
        begin_l = loc(selector_t)
      end

      if end_t
        end_l   = loc(end_t)
      elsif args.any?
        end_l   = args.last.loc.expression
      elsif selector_t
        end_l   = loc(selector_t)
      end

      Source::Map::Send.new(loc(dot_t),   loc(selector_t),
                            loc(begin_t), loc(end_t),
                            begin_l.join(end_l))
    end

    def var_send_map(variable_e)
      Source::Map::Send.new(nil, variable_e.loc.expression,
                            nil, nil,
                            variable_e.loc.expression)
    end

    def send_binary_op_map(lhs_e, selector_t, rhs_e)
      Source::Map::Send.new(nil, loc(selector_t),
                            nil, nil,
                            join_exprs(lhs_e, rhs_e))
    end

    def send_unary_op_map(selector_t, arg_e)
      if arg_e.nil?
        expr_l = loc(selector_t)
      else
        expr_l = loc(selector_t).join(arg_e.loc.expression)
      end

      Source::Map::Send.new(nil, loc(selector_t),
                            nil, nil,
                            expr_l)
    end

    def send_index_map(receiver_e, lbrack_t, rbrack_t)
      Source::Map::Send.new(nil, loc(lbrack_t).join(loc(rbrack_t)),
                            nil, nil,
                            receiver_e.loc.expression.join(loc(rbrack_t)))
    end

    def block_map(receiver_l, begin_t, end_t)
      Source::Map::Collection.new(loc(begin_t), loc(end_t),
                                  receiver_l.join(loc(end_t)))
    end

    def keyword_map(keyword_t, begin_t, args, end_t)
      args ||= []

      if end_t
        end_l = loc(end_t)
      elsif args.any? && !args.last.nil?
        end_l = args.last.loc.expression
      elsif args.any? && args.count > 1
        end_l = args[-2].loc.expression
      else
        end_l = loc(keyword_t)
      end

      Source::Map::Keyword.new(loc(keyword_t), loc(begin_t), loc(end_t),
                               loc(keyword_t).join(end_l))
    end

    def keyword_mod_map(pre_e, keyword_t, post_e)
      Source::Map::Keyword.new(loc(keyword_t), nil, nil,
                               join_exprs(pre_e, post_e))
    end

    def condition_map(keyword_t, cond_e, begin_t, body_e, else_t, else_e, end_t)
      if end_t
        end_l = loc(end_t)
      elsif else_e && else_e.loc.expression
        end_l = else_e.loc.expression
      elsif loc(else_t)
        end_l = loc(else_t)
      elsif body_e && body_e.loc.expression
        end_l = body_e.loc.expression
      elsif loc(begin_t)
        end_l = loc(begin_t)
      else
        end_l = cond_e.loc.expression
      end

      Source::Map::Condition.new(loc(keyword_t),
                                 loc(begin_t), loc(else_t), loc(end_t),
                                 loc(keyword_t).join(end_l))
    end

    def ternary_map(begin_e, question_t, mid_e, colon_t, end_e)
      Source::Map::Ternary.new(loc(question_t), loc(colon_t),
                               join_exprs(begin_e, end_e))
    end

    def for_map(keyword_t, in_t, begin_t, end_t)
      Source::Map::For.new(loc(keyword_t), loc(in_t),
                           loc(begin_t), loc(end_t),
                           loc(keyword_t).join(loc(end_t)))
    end

    def rescue_body_map(keyword_t, exc_list_e, assoc_t,
                        exc_var_e, then_t,
                        compstmt_e)
      end_l = compstmt_e.loc.expression if compstmt_e
      end_l = loc(then_t)               if end_l.nil? && then_t
      end_l = exc_var_e.loc.expression  if end_l.nil? && exc_var_e
      end_l = exc_list_e.loc.expression if end_l.nil? && exc_list_e
      end_l = loc(keyword_t)            if end_l.nil?

      Source::Map::RescueBody.new(loc(keyword_t), loc(assoc_t), loc(then_t),
                                  loc(keyword_t).join(end_l))
    end

    def eh_keyword_map(compstmt_e, keyword_t, body_es,
                       else_t, else_e)
      if compstmt_e.nil?
        if keyword_t.nil?
          begin_l = body_es.first.loc.expression
        else
          begin_l = loc(keyword_t)
        end
      else
        begin_l = compstmt_e.loc.expression
      end

      if else_t
        if else_e.nil?
          end_l = loc(else_t)
        else
          end_l = else_e.loc.expression
        end
      elsif !body_es.last.nil?
        end_l = body_es.last.loc.expression
      else
        end_l = loc(keyword_t)
      end

      Source::Map::Condition.new(loc(keyword_t), nil, loc(else_t), nil,
                                 begin_l.join(end_l))
    end

    #
    # HELPERS
    #

    # Extract a static string from e.g. a regular expression,
    # honoring the fact that MRI expands interpolations like #{""}
    # at parse time.
    def static_string(nodes)
      nodes.map do |node|
        case node.type
        when :str
          node.children[0]
        when :begin
          if (string = static_string(node.children))
            string
          else
            return nil
          end
        else
          return nil
        end
      end.join
    end

    def static_regexp(parts, options)
      source = static_string(parts)
      return nil if source.nil?

      if defined?(Encoding)
        source = case
        when options.children.include?(:u)
          source.encode(Encoding::UTF_8)
        when options.children.include?(:e)
          source.encode(Encoding::EUC_JP)
        when options.children.include?(:s)
          source.encode(Encoding::WINDOWS_31J)
        when options.children.include?(:n)
          source.encode(Encoding::BINARY)
        else
          source
        end
      end

      Regexp.new(source, (Regexp::EXTENDED if options.children.include?(:x)))
    end

    def static_regexp_node(node)
      if node.type == :regexp
        parts, options = node.children[0..-2], node.children[-1]
        static_regexp(parts, options)
      end
    end

    def collapse_string_parts?(parts)
      parts.one? &&
          [:str, :dstr].include?(parts.first.type)
    end

    def value(token)
      token[0]
    end

    def loc(token)
      # Pass through `nil`s and return nil for tNL.
      token[1] if token && token[0]
    end

    def diagnostic(type, reason, arguments, location, highlights=[])
      @parser.diagnostics.process(
          Diagnostic.new(type, reason, arguments, location, highlights))

      if type == :error
        @parser.send :yyerror
      end
    end
  end

end
