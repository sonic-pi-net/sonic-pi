# -*- coding: utf-8 -*-

require 'minitest/autorun'
require 'kramdown'

Encoding.default_external = 'utf-8' if RUBY_VERSION >= '1.9'

describe 'location' do

  # checks that +element+'s :location option corresponds to the location stored
  # in the element.attr['class']
  def check_element_for_location(element)
    if (match = /^line-(\d+)/.match(element.attr['class'] || ''))
      expected_line = match[1].to_i
      element.options[:location].must_equal(expected_line)
    end
    element.children.each do |child|
      check_element_for_location(child)
    end
  end

  # Test cases consist of a kramdown string that uses IALs to specify the expected
  # line numbers for a given element.
  test_cases = {
    'autolink' => %(testing autolinks\n\n<http://kramdown.org>{:.line-3}),
    'blockquote' => %(
      > block quote1
      >
      > * {:.line-3} list item in block quote
      > * {:.line-4} list item in block quote
      > {:.line-3}
      {:.line-1}

      > block quote2
      {:.line-8}
    ),
    'codeblock' => %(\na para\n\n~~~~\ntest code 1\n~~~~\n{:.line-3}\n\n    test code 2\n{:.line-8}\n),
    'codespan' => %(a para\n\nanother para `<code>`{:.line-3} with code\n),
    'emphasis' => %(
      para *span*{:.line-1}
      {:.line-1}

      ## header *span*{:.line-4}
      {:.line-4}

      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
      consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
      cillum *short span on single line*{:.line-11}
      dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
      *long span over multiple lines - proident, sunt in culpa qui officia deserunt
      mollit anim id est laborum.*{:.line-13}
      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
      `code span`{:.line-18}
      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
      {:.line-7}
    ),
    'header' => %(
      # header1
      {:.line-1}

      ## header2
      {:.line-4}

      ## header3
      {:.line-7}

      header4
      =======
      {:.line-10}

      ^

      header5
      -------
      {:.line-16}
    ),
    'horizontal_rule' => %(\na para\n\n----\n{:.line-3}\n),
    'html_entity' => "a para\n\nanother para with &amp;{:.line-3} html entity.\n",
    'link' => %(
      a para

      This is [a link](http://rubyforge.org){:.line-3} to a page.

      Here comes a ![smiley](../images/smiley.png){:.line-5}
    ),
    'list' => %(
      * {:.line-1} list item
      * {:.line-2} list item
      * {:.line-3} list item
      {:.line-1}

      {:.line-7}
      1. {:.line-7} list item
      2. {:.line-8} list item
      3. {:.line-9} list item

      {:.line-12}
      definition term 1
      : {:.line-13} definition definition 1
      definition term 2
      : {:.line-15} definition definition 2
    ),
    'math_block' => %(\na para\n\n$$5+5$$\n{:.line-3}\n),
    'math_inline' => %(\na para\n\nanother para with inline math $$5+5$${:.line-3}\n),
    'paragraph' => %(
      para1
      {:.line-1}

      para2
      {:.line-4}

      Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
      consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
      {:.line-7}

      {:.line-14}
      para with leading IAL
    ),
    'table' => %(
      a para

      |first|second|third|
      |-----|------|-----|
      |a    |b     |c    |
      {:.line-3}
    ),
    'typographic_symbol' => %(
      a para

      another para ---{:.line-3}

      another para ...{:.line-5}
    ),
    'gh issue 129' => %(
      `|`
      {:.line-1}
    ),
    'gh issue 131' => %(
      * {:.line-1} test
        line 2
        * {:.line-3} second
        * {:.line-4} third
      * {:.line-5} * {:.line-5} one
        * {:.line-6} two
    ),
  }
  test_cases.each do |name, test_string|
    it "Handles #{ name }" do
      doc = Kramdown::Document.new(test_string.gsub(/^      /, '').strip)
      check_element_for_location(doc.root)
    end
  end

  it 'adds location info to duplicate abbreviation definition warnings' do
    test_string = %(This snippet contains a duplicate abbreviation definition

*[duplicate]: The first definition
*[duplicate]: The second definition
    )
    doc = Kramdown::Document.new(test_string.strip)
    doc.warnings.must_equal ["Duplicate abbreviation ID 'duplicate' on line 4 - overwriting"]
  end

end
