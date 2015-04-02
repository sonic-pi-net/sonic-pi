require 'kramdown'

module SonicPi
  class MarkdownConverter
    def self.convert(contents)
      # GitHub markdown syntax uses ```` to mark code blocks Kramdown uses ~~~~
      # Therefore, let's fix-point on GitHub syntax, and fudge it
      # into Kramdown syntax where necessary
      contents.gsub!(/\`\`\`\`*/, '~~~~')

      # todo: CSS
      contents_html = Kramdown::Document.new(contents).to_html
      massage!(contents_html)
    end

    def self.massage!(html)
      html.gsub!(/<h1.*?>/, '<p> <span style="font-size:25px; color:white;background-color:deeppink;">')
      html.gsub!(/<h2.*?>/, '<br><p><span style="font-size:20px; color:white; background-color:dodgerblue;">')
      html.gsub!(/<\/h1>/, '</span></p>')
      html.gsub!(/<\/h2>/, '</span></p>')
      html.gsub!(/<p>/, '<p style="font-size:15px;color:#5e5e5e;">')
      html.gsub!(/<em>/, '<em style="font-size:15px;color:darkorange;">')
      html.gsub!(/<ol>/, '<ol style="font-size:15px;color:#5e5e5e;">')
      html.gsub!(/<ul>/, '<ul style="font-size:15px;color:#5e5e5e;">')

      html.gsub!(/<code>/, '<code style="font-size:15px; color:deeppink; background-color:white">')
      html.gsub!(/<a href/, '<a style="text-decoration: none; color:dodgerblue;" href')
      "<font face=\"HelveticaNeue-Light,Helvetica Neue Light,Helvetica Neue\">\n\n" + html + "</font>"
    end
  end
end
