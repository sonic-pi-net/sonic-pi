#!/usr/bin/env ruby
=begin
  hello.cgi - Sample script for CGI

  Set UTF-8 forcely as output charset.

  Recommanded to set UTF-8 forcely because some web browser
  doesn't send HTTP_ACCEPT_CHARSET correctly.

  Copyright (C) 2005  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end
$:.insert(0, "../../lib")

# gettext/cgi support CGI.
begin
  require 'rubygems'
rescue LoadError
end

require 'gettext/cgi'

include GetText
set_cgi(CGI.new)

print "Content-type:text/html; charset=UTF-8\n\n"

# Configure GetText first.
set_output_charset("UTF-8")
bindtextdomain("main", "locale")

langs = ["en"] + Dir.glob("locale/*").collect{|item| File.basename(item)}
langs.sort!

urls = [
  ["helloerb1.cgi", N_("an ERB/CGI sample (UTF-8).")],
  ["helloerb1.cgi?other=true", N_("an ERB/CGI sample (UTF-8). This sample uses the same container as sample 1 but has a different rhtml file.")],
  ["helloerb2.cgi", N_("an ERB/CGI sample (Auto-Detect charset).")]
]


#
# CGI part
#

puts %Q[<html><head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link rel="stylesheet" type="text/css" href="gettext.css" media="all">
  <title>]
puts _("Sample script for CGI/ERB and gettext")
puts  "</title>
</head>
<body>
<h1>"
puts _("Ruby-GetText CGI sample scripts")
puts "</h1>"
puts "<p>" + _("Supported Locales:") + "[#{langs.join(", ")}]</p>"
puts "<h2>" + _("Auto-Detect a locale from the WWW browser") + "</h2>"
puts "<ol>"

urls.each do |url, desc|
  puts "<li><a href=\"/#{url}\">#{url}</a><br/>" + _(desc) + "</a></li>"
end
puts "</ol>"

puts "<h2>" + _('Set locale as a "lang" parameter') + "</h2>"
  
langs.each do |lang|
  puts "<h3>[#{lang}]</h3>"
  puts "<ol>"
  urls.each do |url, desc|
    if /\?other/ =~ url
      url += "&lang=" + lang
    else
      url += "?lang=" + lang
    end
    puts "<li><a href=\"#{url}\">#{CGI.escapeHTML(url)}</a><br/>" + _(desc) + "</li>"
  end
  puts "</ol>"
end

puts "<h2>" + _('Set "lang" to cookie.') + "</h2>"
puts "<p>" + _('Click one of the link below, and then click "Auto-Detect a locale from the WWW browser" samples.') + "</p>"
puts "<ol>"

langs.each do |lang|
  url = "cookie.cgi?lang=" + lang 
  puts "<li><a href=\"#{url}\">#{CGI.escapeHTML(url)}</a> [#{lang}]</li>"
end

puts "</ol>"
puts "<h2>" + _("Source codes") + "</h2>"
puts "<ol>"

Dir.glob("*cgi\0*rb\0*rhtml)").sort.each do |src|
  unless /http.rb|makemo.rb/ =~ src
    puts %Q[<li><a href="/src/?#{src}">#{src}</a></li>]
  end
end
puts %Q[</ol>
<hr/>
  <div class="copyright">
    <p>] +  _("index.cgi is also a Ruby-GetText sample script using CGI(not ERB).")+ %Q[</p>
    <p>Copyright (C) 2005-2008 Masao Mutoh</p>
  </div>
</body>
</html>
]

