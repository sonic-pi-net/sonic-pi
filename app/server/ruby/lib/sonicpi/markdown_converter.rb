require 'kramdown'

module SonicPi
  class MarkdownConverter
    def self.convert(contents)
      contents = contents.to_s
      # GitHub markdown syntax uses ```` to mark code blocks Kramdown uses ~~~~
      # Therefore, let's fix-point on GitHub syntax, and fudge it
      # into Kramdown syntax where necessary
      contents.gsub!(/\`\`\`\`*/, '~~~~')

      contents_html = Kramdown::Document.new(contents).to_html
      massage!(contents_html)
    end

    def self.massage!(html)
      # remove unneeded newlines before </pre>
      html.gsub!(/\n(<\/code>)?<\/pre>/, '\1</pre>')
      # add Unicode and body tags
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n<body>\n" + html + "</body>\n"
    end
  end
end
