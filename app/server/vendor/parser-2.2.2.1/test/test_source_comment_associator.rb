require 'helper'
require 'parser/ruby18'

class TestSourceCommentAssociator < Minitest::Test
  def parse_with_comments(code)
    parser = Parser::Ruby18.new

    buffer = Parser::Source::Buffer.new('(comments)')
    buffer.source = code

    parser.parse_with_comments(buffer)
  end

  def associate(code)
    ast, comments = parse_with_comments(code)
    associations  = Parser::Source::Comment.associate(ast, comments)

    [ ast, associations ]
  end

  def associate_locations(code)
    ast, comments = parse_with_comments(code)
    associations  = Parser::Source::Comment.associate_locations(ast, comments)

    [ ast, associations ]
  end

  def test_associate
    ast, associations = associate(<<-END)
#!/usr/bin/env ruby
# coding: utf-8
# Class comment
# another class comment
class Foo
  # attr_accessor comment
  attr_accessor :foo

  # method comment
  def bar
    # expr comment
    1 + # intermediate comment
      2
    # stray comment
  end
end
    END

    klass_node         = ast
    attr_accessor_node = ast.children[2].children[0]
    method_node        = ast.children[2].children[1] # def bar
    expr_node          = method_node.children[2] # 1 + 2
    intermediate_node  = expr_node.children[0] # 1

    assert_equal 5, associations.size
    assert_equal ['# Class comment', '# another class comment'],
                 associations[klass_node].map(&:text)
    assert_equal ['# attr_accessor comment'],
                 associations[attr_accessor_node].map(&:text)
    assert_equal ['# method comment'],
                 associations[method_node].map(&:text)
    assert_equal ['# expr comment', "# stray comment"],
                 associations[expr_node].map(&:text)
    assert_equal ['# intermediate comment'],
                 associations[intermediate_node].map(&:text)
  end

  # The bug below is fixed by using associate_locations
  def test_associate_dupe_statement
    ast, associations = associate(<<-END)
class Foo
  def bar
    f1 # comment on 1st call to f1
    f2
    f1 # comment on 2nd call to f1
  end
end
    END

    klass_node         = ast
    method_node        = ast.children[2]
    body               = method_node.children[2]
    f1_1_node          = body.children[0]
    f1_2_node          = body.children[2]

    assert_equal 1, associations.size
    assert_equal ['# comment on 1st call to f1', '# comment on 2nd call to f1'],
                 associations[f1_1_node].map(&:text)
    assert_equal ['# comment on 1st call to f1', '# comment on 2nd call to f1'],
                 associations[f1_2_node].map(&:text)
  end

  def test_associate_locations
    ast, associations = associate_locations(<<-END)
#!/usr/bin/env ruby
# coding: utf-8
# Class comment
# another class comment
class Foo
  # attr_accessor comment
  attr_accessor :foo

  # method comment
  def bar
    # expr comment
    1 + # intermediate comment
      2
    # stray comment
  end
end
    END

    klass_node         = ast
    attr_accessor_node = ast.children[2].children[0]
    method_node        = ast.children[2].children[1]
    expr_node          = method_node.children[2]
    intermediate_node  = expr_node.children[0]

    assert_equal 5, associations.size
    assert_equal ['# Class comment', '# another class comment'],
                 associations[klass_node.loc].map(&:text)
    assert_equal ['# attr_accessor comment'],
                 associations[attr_accessor_node.loc].map(&:text)
    assert_equal ['# method comment'],
                 associations[method_node.loc].map(&:text)
    assert_equal ['# expr comment', '# stray comment'],
                 associations[expr_node.loc].map(&:text)
    assert_equal ['# intermediate comment'],
                 associations[intermediate_node.loc].map(&:text)
  end

  def test_associate_locations_dupe_statement
    ast, associations = associate_locations(<<-END)
class Foo
  def bar
    f1 # comment on 1st call to f1
    f2
    f1 # comment on 2nd call to f1
  end
end
    END

    klass_node         = ast
    method_node        = ast.children[2]
    body               = method_node.children[2]
    f1_1_node          = body.children[0]
    f1_2_node          = body.children[2]

    assert_equal 2, associations.size
    assert_equal ['# comment on 1st call to f1'],
                 associations[f1_1_node.loc].map(&:text)
    assert_equal ['# comment on 2nd call to f1'],
                 associations[f1_2_node.loc].map(&:text)
  end

  def test_associate_no_body
    ast, associations = associate(<<-END)
# foo
class Foo
end
    END

    assert_equal 1, associations.size
    assert_equal ['# foo'],
                 associations[ast].map(&:text)
  end

  def test_associate_shebang_only
    ast, associations = associate(<<-END)
#!ruby
class Foo
end
    END

    assert_equal 0, associations.size
  end

  def test_associate_no_comments
    ast, associations = associate(<<-END)
class Foo
end
    END

    assert_equal 0, associations.size
  end

  def test_associate_stray_comment
    ast, associations = associate(<<-END)
def foo
  # foo
end
    END

    assert_equal 1, associations.size
    assert_equal ['# foo'],
                 associations[ast].map(&:text)
  end

  def test_associate___ENCODING__
    ast, associations = associate(<<-END)
# foo
__ENCODING__
    END

    assert_equal 1, associations.size
    assert_equal ['# foo'],
                 associations[ast].map(&:text)
  end
end
