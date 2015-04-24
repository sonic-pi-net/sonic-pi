# Customizing Parsers

While the default setup of the parsers provided by this Gem should be suitable
for most some developers might want to change parts of it. An example would be
the use of a custom class for nodes instead of `Parser::AST::Node`.

Customizing the AST is done by creating a custom builder class and passing it
to the constructor method of a parser. The default setup comes down to the
following:

    builder = Parser::Builders::Default.new
    parser  = Parser::Ruby19.new(builder)

When creating your own builder class it's best to subclass the default one so
that you don't have to redefine every used method again:

    class MyBuilder < Parser::Builders::Default

    end

    builder = MyBuilder.new
    parser  = Parser::Ruby19.new(builder)

## Custom Node Classes

To use a custom node class you have to override the method
`Parser::Builders::Default#n`:

    class MyBuilder < Parser::Builders::Default
      def n(type, children, location)
        return MyNodeClass.new(type, children, :location => location)
      end
    end

Note that the used class (and corresponding instance) must be compatible with
`Parser::AST::Node` so it's best to subclass it and override/add code where
needed.
