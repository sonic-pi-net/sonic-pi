require 'helper'

describe AST::Node do
  extend AST::Sexp

  class MetaNode < AST::Node
    attr_reader :meta
  end

  before do
    @node = AST::Node.new(:node, [ 0, 1 ])
    @metanode = MetaNode.new(:node, [ 0, 1 ], :meta => 'value')
  end

  it 'should have accessors for type and children' do
    @node.type.should.equal :node
    @node.children.should.equal [0, 1]
  end

  it 'should set metadata' do
    @metanode.meta.should.equal 'value'
  end

  it 'should be frozen' do
    @node.frozen?.should.be.true
    @node.children.frozen?.should.be.true
  end

  it 'should return self when duping' do
    @node.dup.should.equal? @node
  end

  it 'should return an updated node, but only if needed' do
    @node.updated().should.be.identical_to @node
    @node.updated(:node).should.be.identical_to @node
    @node.updated(nil, [0, 1]).should.be.identical_to @node

    updated = @node.updated(:other_node)
    updated.should.not.be.identical_to @node
    updated.type.should.equal :other_node
    updated.children.should.equal @node.children

    updated.frozen?.should.be.true

    updated = @node.updated(nil, [1, 1])
    updated.should.not.be.identical_to @node
    updated.type.should.equal @node.type
    updated.children.should.equal [1, 1]

    updated = @metanode.updated(nil, nil, :meta => 'other_value')
    updated.meta.should.equal 'other_value'
  end

  it 'should use fancy type in to_s' do
    node = AST::Node.new(:ast_node)
    node.to_s.should.equal '(ast-node ...)'
  end

  it 'should format to_sexp correctly' do
    AST::Node.new(:a, [ :sym, [ 1, 2 ] ]).to_sexp.should.equal '(a :sym [1, 2])'
    AST::Node.new(:a, [ :sym, @node ]).to_sexp.should.equal "(a :sym\n  (node 0 1))"
    AST::Node.new(:a, [ :sym,
      AST::Node.new(:b, [ @node, @node ])
    ]).to_sexp.should.equal "(a :sym\n  (b\n    (node 0 1)\n    (node 0 1)))"
  end

  it 'should return self in to_ast' do
    @node.to_ast.should.be.identical_to @node
  end

  it 'should only use type and children to compute #hash' do
    @node.hash.should.equal([@node.type, @node.children, @node.class].hash)
  end

  it 'should only use type and children in #eql? comparisons' do
    # Not identical but equivalent
    @node.eql?(AST::Node.new(:node, [0, 1])).should.be.true
    # Not identical and not equivalent
    @node.eql?(AST::Node.new(:other, [0, 1])).should.be.false
    # Not identical and not equivalent because of differend class
    @node.eql?(@metanode).should.be.false
  end

  it 'should only use type and children in #== comparisons' do
    @node.should.equal @node
    @node.should.equal @metanode
    @node.should.not.equal :foo

    mock_node = Object.new.tap do |obj|
      def obj.to_ast
        self
      end

      def obj.type
        :node
      end

      def obj.children
        [ 0, 1 ]
      end
    end
    @node.should.equal mock_node
  end

  it 'should allow to decompose nodes with a, b = *node' do
    node = s(:gasgn, :$foo, s(:integer, 1))

    var_name, value = *node
    var_name.should.equal :$foo
    value.should.equal s(:integer, 1)
  end

  it 'should concatenate with arrays' do
    node = s(:gasgn, :$foo)
    (node + [s(:integer, 1)]).
        should.equal s(:gasgn, :$foo, s(:integer, 1))
  end

  it 'should append elements' do
    node = s(:array)
    (node << s(:integer, 1) << s(:string, "foo")).
        should.equal s(:array, s(:integer, 1), s(:string, "foo"))
  end

  begin
    eval <<-CODE
    it 'should not trigger a rubinius bug' do
      bar = [ s(:bar, 1) ]
      baz = s(:baz, 2)
      s(:foo, *bar, baz).should.equal s(:foo, s(:bar, 1), s(:baz, 2))
    end
    CODE
  rescue SyntaxError
    # Running on 1.8, ignore.
  end
end

describe AST::Processor do
  extend AST::Sexp

  def have_sexp(text)
    text = text.lines.map { |line| line.sub /^ +\|(.+)/, '\1' }.join.rstrip
    lambda { |ast| ast.to_sexp == text }
  end

  class MockProcessor < AST::Processor
    attr_reader :counts

    def initialize
      @counts = Hash.new(0)
    end

    def on_root(node)
      count_node(node)
      node.updated(nil, process_all(node.children))
    end
    alias on_body on_root

    def on_def(node)
      count_node(node)
      name, arglist, body = node.children
      node.updated(:def, [ name, process(arglist), process(body) ])
    end

    def handler_missing(node)
      count_node(node)
    end

    def count_node(node)
      @counts[node.type] += 1; nil
    end
  end

  before do
    @ast = AST::Node.new(:root, [
      AST::Node.new(:def, [ :func,
        AST::Node.new(:arglist, [ :foo, :bar ]),
        AST::Node.new(:body, [
          AST::Node.new(:invoke, [ :puts, "Hello world" ])
        ])
      ]),
      AST::Node.new(:invoke, [ :func ])
    ])

    @processor = MockProcessor.new
  end

  it 'should visit every node' do
    @processor.process(@ast).should.equal @ast
    @processor.counts.should.equal({
      :root    => 1,
      :def     => 1,
      :arglist => 1,
      :body    => 1,
      :invoke  => 2,
    })
  end

  it 'should be able to replace inner nodes' do
    def @processor.on_arglist(node)
      node.updated(:new_fancy_arglist)
    end

    @processor.process(@ast).should have_sexp(<<-SEXP)
    |(root
    |  (def :func
    |    (new-fancy-arglist :foo :bar)
    |    (body
    |      (invoke :puts "Hello world")))
    |  (invoke :func))
    SEXP
  end

  it 'should build sexps' do
    s(:add,
      s(:integer, 1),
      s(:multiply,
        s(:integer, 2),
        s(:integer, 3))).should have_sexp(<<-SEXP)
    |(add
    |  (integer 1)
    |  (multiply
    |    (integer 2)
    |    (integer 3)))
    SEXP
  end

  it 'should return nil if passed nil' do
    @processor.process(nil).should == nil
  end

  it 'should refuse to process non-nodes' do
    lambda { @processor.process([]) }.should.raise NoMethodError, %r|to_ast|
  end

  it 'should allow to visit nodes with process_all(node)' do
    @processor.process_all s(:foo, s(:bar), s(:integer, 1))
    @processor.counts.should.equal({
      :bar =>     1,
      :integer => 1,
    })
  end
end
