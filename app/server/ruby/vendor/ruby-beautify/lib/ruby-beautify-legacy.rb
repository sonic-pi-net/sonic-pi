require_relative "ruby-beautify-legacy/version"
require_relative 'ruby-beautify-legacy/block_start'
require_relative 'ruby-beautify-legacy/block_end'
require_relative 'ruby-beautify-legacy/block_matcher'
require_relative 'ruby-beautify-legacy/language'
require_relative 'ruby-beautify-legacy/line'
require_relative 'ruby-beautify-legacy/config/ruby'

module RBeautifyLegacy
  def self.beautify_string(language, source, use_tabs=false)
    dest = ""
    block = nil

    unless language.is_a? RBeautifyLegacy::Language
      language = RBeautifyLegacy::Language.language(language)
    end

    source.lines.each_with_index do |line_content, line_number|
      line = RBeautifyLegacy::Line.new(language, line_content, line_number, block, use_tabs)
      dest += line.format
      block = line.block
    end

    return dest
  end
end # module RBeautifyLegacy
