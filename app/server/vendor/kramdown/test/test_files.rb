# -*- coding: utf-8 -*-
#
#--
# Copyright (C) 2009-2014 Thomas Leitner <t_leitner@gmx.at>
#
# This file is part of kramdown which is licensed under the MIT.
#++
#

require 'minitest/autorun'
require 'kramdown'
require 'yaml'
require 'tmpdir'

Encoding.default_external = 'utf-8' if RUBY_VERSION >= '1.9'

class TestFiles < Minitest::Test

  EXCLUDE_KD_FILES = [('test/testcases/block/04_header/with_auto_ids.text' if RUBY_VERSION <= '1.8.6'), # bc of dep stringex not working
                     ].compact

  # Generate test methods for kramdown-to-xxx conversion
  Dir[File.dirname(__FILE__) + '/testcases/**/*.text'].each do |text_file|
    next if EXCLUDE_KD_FILES.any? {|f| text_file =~ /#{f}$/}
    basename = text_file.sub(/\.text$/, '')
    opts_file = text_file.sub(/\.text$/, '.options')
    (Dir[basename + ".*"] - [text_file, opts_file]).each do |output_file|
      next if (RUBY_VERSION >= '1.9' && File.exist?(output_file + '.19')) ||
        (RUBY_VERSION < '1.9' && output_file =~ /\.19$/)
      output_format = File.extname(output_file.sub(/\.19$/, ''))[1..-1]
      next if !Kramdown::Converter.const_defined?(output_format[0..0].upcase + output_format[1..-1])
      define_method('test_' + text_file.tr('.', '_') + "_to_#{output_format}") do
        opts_file = File.join(File.dirname(text_file), 'options') if !File.exist?(opts_file)
        options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
        doc = Kramdown::Document.new(File.read(text_file), options)
        assert_equal(File.read(output_file), doc.send("to_#{output_format}"))
      end
    end
  end

  # Generate test methods for html-to-html conversion
  `tidy -v 2>&1`
  if $?.exitstatus != 0
    warn("Skipping html-to-html tests because tidy executable is missing")
  else
    EXCLUDE_HTML_FILES = ['test/testcases/block/06_codeblock/whitespace.html', # bc of span inside pre
                          'test/testcases/block/09_html/simple.html', # bc of xml elements
                          'test/testcases/span/03_codespan/highlighting.html', # bc of span elements inside code element
                          'test/testcases/block/04_header/with_auto_ids.html', # bc of auto_ids=true option
                          'test/testcases/block/04_header/header_type_offset.html', # bc of header_offset option
                         ]
    Dir[File.dirname(__FILE__) + '/testcases/**/*.{html,html.19,htmlinput,htmlinput.19}'].each do |html_file|
      next if EXCLUDE_HTML_FILES.any? {|f| html_file =~ /#{f}(\.19)?$/}
      next if (RUBY_VERSION >= '1.9' && File.exist?(html_file + '.19')) ||
        (RUBY_VERSION < '1.9' && html_file =~ /\.19$/)
      out_file = (html_file =~ /\.htmlinput(\.19)?$/ ? html_file.sub(/input(\.19)?$/, '') : html_file)
      define_method('test_' + html_file.tr('.', '_') + "_to_html") do
        opts_file = html_file.sub(/\.html(input)?(\.19)?$/, '.options')
        opts_file = File.join(File.dirname(html_file), 'options') if !File.exist?(opts_file)
        options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
        doc = Kramdown::Document.new(File.read(html_file), options.merge(:input => 'html'))
        assert_equal(tidy_output(File.read(out_file)), tidy_output(doc.to_html))
      end
    end
  end

  def tidy_output(out)
    cmd = "tidy -q --doctype omit #{RUBY_VERSION >= '1.9' ? '-utf8' : '-raw'} 2>/dev/null"
    result = IO.popen(cmd, 'r+') do |io|
      io.write(out)
      io.close_write
      io.read
    end
    if $?.exitstatus == 2
      raise "Problem using tidy"
    end
    result
  end

  # Generate test methods for text-to-latex conversion and compilation
  `latex -v 2>&1`
  if $?.exitstatus != 0
    warn("Skipping latex compilation tests because latex executable is missing")
  else
    EXCLUDE_LATEX_FILES = ['test/testcases/span/01_link/image_in_a.text', # bc of image link
                           'test/testcases/span/01_link/imagelinks.text', # bc of image links
                           'test/testcases/span/04_footnote/markers.text', # bc of footnote in header
                          ]
    Dir[File.dirname(__FILE__) + '/testcases/**/*.text'].each do |text_file|
      next if EXCLUDE_LATEX_FILES.any? {|f| text_file =~ /#{f}$/}
      define_method('test_' + text_file.tr('.', '_') + "_to_latex_compilation") do
        latex =  Kramdown::Document.new(File.read(text_file),
                                        :auto_ids => false, :footnote_nr => 1,
                                        :template => 'document').to_latex
        Dir.mktmpdir do |tmpdir|
          result = IO.popen("latex -output-directory='#{tmpdir}' 2>/dev/null", 'r+') do |io|
            io.write(latex)
            io.close_write
            io.read
          end
          assert($?.exitstatus == 0, result.scan(/^!(.*\n.*)/).join("\n"))
        end
      end
    end
  end

  # Generate test methods for text->kramdown->html conversion
  `tidy -v 2>&1`
  if $?.exitstatus != 0
    warn("Skipping text->kramdown->html tests because tidy executable is missing")
  else
    EXCLUDE_TEXT_FILES = ['test/testcases/span/05_html/markdown_attr.text',  # bc of markdown attr
                          'test/testcases/block/09_html/markdown_attr.text', # bc of markdown attr
                          'test/testcases/span/extension/options.text',      # bc of parse_span_html option
                          'test/testcases/block/12_extension/options.text',  # bc of options option
                          'test/testcases/block/12_extension/options3.text', # bc of options option
                          'test/testcases/block/09_html/content_model/tables.text',  # bc of parse_block_html option
                          'test/testcases/block/09_html/html_to_native/header.text', # bc of auto_ids option that interferes
                          'test/testcases/block/09_html/html_to_native/table_simple.text', # bc of tr style attr getting removed
                          'test/testcases/block/09_html/simple.text',        # bc of webgen:block elements
                          'test/testcases/block/11_ial/simple.text',         # bc of change of ordering of attributes in header
                          'test/testcases/span/extension/comment.text',      # bc of comment text modifications (can this be avoided?)
                          'test/testcases/block/04_header/header_type_offset.text', # bc of header_offset being applied twice
                          ('test/testcases/block/04_header/with_auto_ids.text' if RUBY_VERSION <= '1.8.6'), # bc of dep stringex not working
                         ].compact
    Dir[File.dirname(__FILE__) + '/testcases/**/*.text'].each do |text_file|
      next if EXCLUDE_TEXT_FILES.any? {|f| text_file =~ /#{f}$/}
      define_method('test_' + text_file.tr('.', '_') + "_to_kramdown_to_html") do
        html_file = text_file.sub(/\.text$/, '.html')
        html_file += '.19' if RUBY_VERSION >= '1.9' && File.exist?(html_file + '.19')
        opts_file = text_file.sub(/\.text$/, '.options')
        opts_file = File.join(File.dirname(text_file), 'options') if !File.exist?(opts_file)
        options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
        kdtext = Kramdown::Document.new(File.read(text_file), options).to_kramdown
        html = Kramdown::Document.new(kdtext, options).to_html
        assert_equal(tidy_output(File.read(html_file)), tidy_output(html))
      end
    end
  end

  # Generate test methods for html-to-kramdown-to-html conversion
  `tidy -v 2>&1`
  if $?.exitstatus != 0
    warn("Skipping html-to-kramdown-to-html tests because tidy executable is missing")
  else
    EXCLUDE_HTML_KD_FILES = ['test/testcases/span/extension/options.html',        # bc of parse_span_html option
                             'test/testcases/span/05_html/normal.html',           # bc of br tag before closing p tag
                             'test/testcases/block/12_extension/nomarkdown.html', # bc of nomarkdown extension
                             'test/testcases/block/09_html/simple.html',          # bc of webgen:block elements
                             'test/testcases/block/09_html/markdown_attr.html',   # bc of markdown attr
                             'test/testcases/block/09_html/html_to_native/table_simple.html', # bc of invalidly converted simple table
                             'test/testcases/block/06_codeblock/whitespace.html', # bc of entity to char conversion
                             'test/testcases/block/11_ial/simple.html',           # bc of change of ordering of attributes in header
                             'test/testcases/span/03_codespan/highlighting.html', # bc of span elements inside code element
                             'test/testcases/block/04_header/with_auto_ids.html', # bc of auto_ids=true option
                             'test/testcases/block/04_header/header_type_offset.html', # bc of header_offset option
                             'test/testcases/block/16_toc/toc_exclude.html',      # bc of different attribute ordering
                             'test/testcases/span/autolinks/url_links.html',      # bc of quot entity being converted to char
                            ]
    Dir[File.dirname(__FILE__) + '/testcases/**/*.{html,html.19}'].each do |html_file|
      next if EXCLUDE_HTML_KD_FILES.any? {|f| html_file =~ /#{f}(\.19)?$/}
      next if (RUBY_VERSION >= '1.9' && File.exist?(html_file + '.19')) ||
        (RUBY_VERSION < '1.9' && html_file =~ /\.19$/)
      define_method('test_' + html_file.tr('.', '_') + "_to_kramdown_to_html") do
        kd = Kramdown::Document.new(File.read(html_file), :input => 'html', :auto_ids => false, :footnote_nr => 1).to_kramdown
        opts_file = html_file.sub(/\.html(\.19)?$/, '.options')
        opts_file = File.join(File.dirname(html_file), 'options') if !File.exist?(opts_file)
        options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
        doc = Kramdown::Document.new(kd, options)
        assert_equal(tidy_output(File.read(html_file)), tidy_output(doc.to_html))
      end
    end
  end

  EXCLUDE_GFM_FILES = [
    'test/testcases/block/03_paragraph/no_newline_at_end.text',
    'test/testcases/block/03_paragraph/indented.text',
    'test/testcases/block/03_paragraph/two_para.text',
    'test/testcases/block/04_header/atx_header.text',
    'test/testcases/block/04_header/setext_header.text',
    'test/testcases/block/05_blockquote/indented.text',
    'test/testcases/block/05_blockquote/lazy.text',
    'test/testcases/block/05_blockquote/nested.text',
    'test/testcases/block/05_blockquote/no_newline_at_end.text',
    'test/testcases/block/06_codeblock/error.text',
    'test/testcases/block/07_horizontal_rule/error.text',
    'test/testcases/block/08_list/escaping.text',
    'test/testcases/block/08_list/item_ial.text',
    'test/testcases/block/08_list/lazy.text',
    'test/testcases/block/08_list/list_and_others.text',
    'test/testcases/block/08_list/other_first_element.text',
    'test/testcases/block/08_list/simple_ul.text',
    'test/testcases/block/08_list/special_cases.text',
    'test/testcases/block/09_html/comment.text',
    'test/testcases/block/09_html/html_to_native/code.text',
    'test/testcases/block/09_html/html_to_native/emphasis.text',
    'test/testcases/block/09_html/html_to_native/typography.text',
    'test/testcases/block/09_html/parse_as_raw.text',
    'test/testcases/block/09_html/simple.text',
    'test/testcases/block/12_extension/comment.text',
    'test/testcases/block/12_extension/ignored.text',
    'test/testcases/block/12_extension/nomarkdown.text',
    'test/testcases/block/13_definition_list/item_ial.text',
    'test/testcases/block/13_definition_list/multiple_terms.text',
    'test/testcases/block/13_definition_list/no_def_list.text',
    'test/testcases/block/13_definition_list/simple.text',
    'test/testcases/block/13_definition_list/with_blocks.text',
    'test/testcases/block/14_table/errors.text',
    'test/testcases/block/14_table/escaping.text',
    'test/testcases/block/14_table/simple.text',
    'test/testcases/block/15_math/normal.text',
    'test/testcases/encoding.text',
    'test/testcases/span/01_link/inline.text',
    'test/testcases/span/01_link/link_defs.text',
    'test/testcases/span/01_link/reference.text',
    'test/testcases/span/02_emphasis/normal.text',
    'test/testcases/span/03_codespan/normal.text',
    'test/testcases/span/04_footnote/definitions.text',
    'test/testcases/span/04_footnote/markers.text',
    'test/testcases/span/05_html/across_lines.text',
    'test/testcases/span/05_html/markdown_attr.text',
    'test/testcases/span/05_html/normal.text',
    'test/testcases/span/autolinks/url_links.text',
    'test/testcases/span/extension/comment.text',
    'test/testcases/span/ial/simple.text',
    'test/testcases/span/line_breaks/normal.text',
    'test/testcases/span/text_substitutions/entities_as_char.text',
    'test/testcases/span/text_substitutions/entities.text',
    'test/testcases/span/text_substitutions/typography.text'
  ]

  # Generate test methods for gfm-to-html conversion
  Dir[File.dirname(__FILE__) + '/{testcases,testcases_gfm}/**/*.text'].each do |text_file|
    next if EXCLUDE_GFM_FILES.any? {|f| text_file =~ /#{f}$/}
    basename = text_file.sub(/\.text$/, '')

    html_file = [(".html.19" if RUBY_VERSION >= '1.9'), ".html"].compact.
      map {|ext| basename + ext }.
      detect {|file| File.exist?(file) }

    define_method('test_gfm_' + text_file.tr('.', '_') + "_to_html") do
      opts_file = basename + '.options'
      opts_file = File.join(File.dirname(html_file), 'options') if !File.exist?(opts_file)
      options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
      doc = Kramdown::Document.new(File.read(text_file), options.merge(:input => 'GFM'))
      assert_equal(File.read(html_file), doc.to_html)
    end
  end


  # Generate test methods for asserting that converters don't modify the document tree.
  Dir[File.dirname(__FILE__) + '/testcases/**/*.text'].each do |text_file|
    opts_file = text_file.sub(/\.text$/, '.options')
    options = File.exist?(opts_file) ? YAML::load(File.read(opts_file)) : {:auto_ids => false, :footnote_nr => 1}
    (Kramdown::Converter.constants.map {|c| c.to_sym} - [:Base, :RemoveHtmlTags]).each do |conv_class|
      next if conv_class == :Pdf && RUBY_VERSION < '1.9'
      define_method("test_whether_#{conv_class}_modifies_tree_with_file_#{text_file.tr('.', '_')}") do
        doc = Kramdown::Document.new(File.read(text_file), options)
        options_before = Marshal.load(Marshal.dump(doc.options))
        tree_before = Marshal.load(Marshal.dump(doc.root))
        Kramdown::Converter.const_get(conv_class).convert(doc.root, doc.options)
        assert_equal(options_before, doc.options)
        assert_tree_not_changed(tree_before, doc.root)
      end
    end
  end

  def assert_tree_not_changed(old, new)
    assert_equal(old.type, new.type, "type mismatch")
    if old.value.kind_of?(Kramdown::Element)
      assert_tree_not_changed(old.value, new.value)
    else
      assert_equal(old.value, new.value, "value mismatch")
    end
    assert_equal(old.attr, new.attr, "attr mismatch")
    assert_equal(old.options, new.options, "options mismatch")
    assert_equal(old.children.length, new.children.length, "children count mismatch")

    old.children.each_with_index do |child, index|
      assert_tree_not_changed(child, new.children[index])
    end
  end

end
