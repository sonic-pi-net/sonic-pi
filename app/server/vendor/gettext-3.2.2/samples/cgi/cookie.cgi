#!/usr/bin/env ruby
=begin
  cookie.cgi - Set a selected locale to the cookie of WWW Browser.

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

# Configure GetText first.
set_output_charset("UTF-8")
set_cgi(CGI.new)
bindtextdomain("main", "locale")

lang = GetText.cgi['lang']
lang = "en" unless lang.size > 0

#
# CGI part
#
print "Set-Cookie:lang=#{lang}\n"
print "Content-type:text/html; charset=UTF-8\n\n"

puts %Q[
<head>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <link rel="stylesheet" type="text/css" href="gettext.css" media="all">
  <title>]
puts _("Sample script for CGI/ERB and gettext")
puts %Q[</title>
</head>
<body>
]

puts "<h1>" + _("Set [%s] as the cookie of your WWW Browser.") % lang + "</h1>"

puts %Q[</h1>
  <p><a href="/">]
puts _("Back")
puts %Q[</a></p>
  <div style="text-align:right">
  <p>Copyright (C) 2005 Masao Mutoh</p>
  </div>
  </body>
</html>
]

