class Rewriter < Parser::Rewriter
  def on_if(node)
    # Crude, totally-not-usable-in-the-real-world code to remove optional
    # parens from control keywords.
    #
    # In a perfect test scenario we'd simply make this a no-op, to demonstrate
    # that the bug happens when any rewriter is loaded regardless of whether it
    # actually changes anything but that makes assertions much harder to get
    # right.  It's much easier to just show that the file did, or did not
    # get changed.
    if node.children[0].type == :begin
      replace node.children[0].loc.begin, ' '
      remove node.children[0].loc.end
    end

    super
  end
end
