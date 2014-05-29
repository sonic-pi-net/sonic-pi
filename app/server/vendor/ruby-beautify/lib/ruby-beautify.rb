require "ruby-beautify/version"
require 'ruby-beautify/block_start'
require 'ruby-beautify/block_end'
require 'ruby-beautify/block_matcher'
require 'ruby-beautify/language'
require 'ruby-beautify/line'
require 'ruby-beautify/config/ruby'

module RBeautify
  def self.beautify_string(language, source, use_tabs=false)
    dest = ""
    block = nil

    unless language.is_a? RBeautify::Language
      language = RBeautify::Language.language(language)
    end

    source.lines.each_with_index do |line_content, line_number|
      line = RBeautify::Line.new(language, line_content, line_number, block, use_tabs)
      dest += line.format + "\n"
      block = line.block
    end

    return dest
  end
end # module RBeautify
