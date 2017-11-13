module Parser
  module Source

    ##
    # A processor which associates AST nodes with comments based on their
    # location in source code. It may be used, for example, to implement
    # rdoc-style processing.
    #
    # @example
    #   require 'parser/current'
    #
    #   ast, comments = Parser::CurrentRuby.parse_with_comments(<<-CODE)
    #   # Class stuff
    #   class Foo
    #     # Attr stuff
    #     # @see bar
    #     attr_accessor :foo
    #   end
    #   CODE
    #
    #   p Parser::Source::Comment.associate(ast, comments)
    #   # => {
    #   #   (class (const nil :Foo) ...) =>
    #   #     [#<Parser::Source::Comment (string):1:1 "# Class stuff">],
    #   #   (send nil :attr_accessor (sym :foo)) =>
    #   #     [#<Parser::Source::Comment (string):3:3 "# Attr stuff">,
    #   #      #<Parser::Source::Comment (string):4:3 "# @see bar">]
    #   # }
    #
    # @see {associate}
    #
    # @!attribute skip_directives
    #  Skip file processing directives disguised as comments.
    #  Namely:
    #
    #    * Shebang line,
    #    * Magic encoding comment.
    #
    #  @return [Boolean]
    #
    # @api public
    #
    class Comment::Associator
      attr_accessor :skip_directives

      ##
      # @param [Parser::AST::Node] ast
      # @param [Array(Parser::Source::Comment)] comments
      def initialize(ast, comments)
        @ast         = ast
        @comments    = comments

        @skip_directives = true
      end

      ##
      # Compute a mapping between AST nodes and comments.
      #
      # A comment belongs to a certain node if it begins after end
      # of the previous node (if one exists) and ends before beginning of
      # the current node.
      #
      # This rule is unambiguous and produces the result
      # one could reasonably expect; for example, this code
      #
      #     # foo
      #     hoge # bar
      #       + fuga
      #
      # will result in the following association:
      #
      #     {
      #       (send (lvar :hoge) :+ (lvar :fuga)) =>
      #         [#<Parser::Source::Comment (string):2:1 "# foo">],
      #       (lvar :fuga) =>
      #         [#<Parser::Source::Comment (string):3:8 "# bar">]
      #     }
      #
      # Note that {associate} produces unexpected result for nodes which are
      # equal but have distinct locations; comments for these nodes are merged.
      #
      # @return [Hash(Parser::AST::Node, Array(Parser::Source::Comment))]
      # @deprecated Use {associate_locations}.
      #
      def associate
        @map_using_locations = false
        do_associate
      end

      ##
      # Same as {associate}, but uses `node.loc` instead of `node` as
      # the hash key, thus producing an unambiguous result even in presence
      # of equal nodes.
      #
      # @return [Hash(Parser::Source::Map, Array(Parser::Source::Comment))]
      #
      def associate_locations
        @map_using_locations = true
        do_associate
      end

      private

      def do_associate
        @mapping     = Hash.new { |h, k| h[k] = [] }
        @comment_num = -1
        advance_comment

        advance_through_directives if @skip_directives

        @prev_node = nil
        visit(@ast)

        @mapping
      end

      def visit(node)
        process_node(node)

        if node.children.length > 0
          node.children.each do |child|
            next unless child.is_a?(AST::Node) && child.loc && child.loc.expression
            visit(child)
          end
          process_trailing_comments(node)
          @prev_node = node
        end
      end

      def process_node(node)
        return unless node.type != :begin
        while current_comment_between?(@prev_node, node)
          associate_and_advance_comment(@prev_node, node)
        end
        @prev_node = node
      end

      def process_trailing_comments(parent)
        while current_comment_decorates?(@prev_node)
          associate_and_advance_comment(@prev_node, nil)
        end
        while current_comment_before_end?(parent)
          associate_and_advance_comment(@prev_node, nil)
        end
      end

      def advance_comment
        @comment_num += 1
        @current_comment = @comments[@comment_num]
      end

      def current_comment_between?(prev_node, next_node)
        return false if !@current_comment
        comment_loc = @current_comment.location.expression

        if next_node
          next_loc = next_node.location.expression
          return false if comment_loc.end_pos > next_loc.begin_pos
        end
        if prev_node
          prev_loc = prev_node.location.expression
          return false if comment_loc.begin_pos < prev_loc.begin_pos
        end
        true
      end

      def current_comment_decorates?(prev_node)
        return false if !@current_comment
        @current_comment.location.line == prev_node.location.line
      end

      def current_comment_before_end?(parent)
        return false if !@current_comment
        comment_loc = @current_comment.location.expression
        parent_loc = parent.location.expression
        comment_loc.end_pos <= parent_loc.end_pos
      end

      def associate_and_advance_comment(prev_node, node)
        if prev_node && node
          owner_node = (@current_comment.location.line == prev_node.location.line) ? prev_node : node
        else
          owner_node = prev_node ? prev_node : node
        end
        key = @map_using_locations ? owner_node.location : owner_node
        @mapping[key] << @current_comment
        advance_comment
      end

      def advance_through_directives
        # Skip shebang.
        if @current_comment && @current_comment.text =~ /^#!/
          advance_comment
        end

        # Skip encoding line.
        if @current_comment && @current_comment.text =~ Buffer::ENCODING_RE
          advance_comment
        end
      end
    end

  end
end
