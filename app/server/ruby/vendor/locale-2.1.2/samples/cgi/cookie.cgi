#!/usr/bin/env ruby
=begin
  cookie.cgi - Set a selected locale to the cookie of WWW Browser.

  Set UTF-8 forcely as output charset.

  Recommanded to set UTF-8 forcely because some web browser
  doesn't send HTTP_ACCEPT_CHARSET correctly.

  Copyright (C) 2008  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end

$:<< "../../lib"

require 'locale'
require 'cgi'

# Initialization of Locale library.
Locale.init(:driver => :cgi)
cgi = CGI.new
Locale.set_cgi(cgi)

# Get "lang" from the query string.
lang = cgi["lang"] or ""

#
# CGI part
#
print "Set-Cookie:lang=#{lang}\n"
print "Content-type:text/html; charset=UTF-8\n\n"

puts %Q[
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link rel="stylesheet" type="text/css" href="locale.css" media="all">
  <title>Sample script for CGI and Ruby/Locale</title>
</head>
<body>

<div style="text-align:center; margin:5em;">
]

if lang.size > 0
  puts %Q!<p><div id="result">Set [#{lang}] as the cookie of your WWW browser.</div></p>!
else
  puts %Q!<p><div id="result">Clear "lang" value from your WWW browser.</div></p>!
end

puts %Q!
<p style="margin:2em;"><a href="/">Back</a></p>
</div>
<div style="text-align:right">
  <p>Copyright (C) 2008 Masao Mutoh</p>
</div>
</body>
</html>
!

