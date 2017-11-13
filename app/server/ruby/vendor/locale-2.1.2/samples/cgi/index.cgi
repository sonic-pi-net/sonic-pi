#!/usr/bin/env ruby
=begin
  index.cgi - Sample script for CGI

  Copyright (C) 2008  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end
$:<< "../../lib"

begin
  require 'rubygems'
rescue LoadError
end

require 'locale'
require 'cgi'

# Initialize Locale library.
Locale.init(:driver => :cgi)
cgi = CGI.new
Locale.set_cgi(cgi)


url_base = "http://#{cgi.server_name}:#{cgi.server_port}/"

sample_locales = ["en-US", "fr-FR", "ja-JP"]

langs = Locale.candidates(:type => :common)  #Try :rfc, :simple and others.


#
# CGI part
#

print "Content-type:text/html; charset=UTF-8\n\n"

puts %Q[<html><head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link rel="stylesheet" type="text/css" href="locale.css" media="all">
  <title>Sample script for CGI/ERB and Ruby/Locale</title>
</head>
<body>
<h1>Ruby/Locale CGI sample scripts</h1>
<p>Requested Locales order by the priority:</p>
<div id="result">#{CGI.escapeHTML(langs.inspect)}</div>

<h2>Auto-Detect the locale from the WWW browser</h2>
<p><a href="./">#{url_base}</a></p>

<h2>Set locale as the "lang" parameter</h2>
<ol>
]
sample_locales.each do |lang|
  url = url_base + "?lang=" + lang
  puts %Q!<li> <a href="#{url}">#{CGI.escapeHTML(url)}</a> [#{lang}]</li>!
end
url = url_base + "?lang=zh_CN;lang=ko_KR"
puts %Q!<li> <a href="#{url}">#{CGI.escapeHTML(url)}</a> [Plural locales]</li>!

puts "</ol>"
puts %Q[<h2>Set "lang" as the cookie value.</h2>
<p>Click one of the link below, and then click "Auto-Detect the locale from the WWW browser".</p>
<ol>]

sample_locales.each do |lang|
  url = url_base + "cookie.cgi?lang=" + lang 
  puts %Q!<li><a href="#{url}">#{CGI.escapeHTML(url)}</a> [#{lang}]</li>!
end
puts %Q!<li><a href="cookie.cgi">Clear the cookie value</a></li>!

puts "</ol>"
puts "<h2>Source codes</h2>"
puts "<ol>"

Dir.glob("*cgi\0*rb)").sort.each do |src|
  unless /http.rb|makemo.rb/ =~ src
    puts %Q[<li><a href="/src/?#{src}">#{src}</a></li>]
  end
end
puts %Q[</ol>
<hr/>
  <div class="copyright">
    <p>Copyright (C) 2008 Masao Mutoh</p>
  </div>
</body>
</html>
]

