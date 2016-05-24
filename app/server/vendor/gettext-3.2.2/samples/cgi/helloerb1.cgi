#!/usr/bin/env ruby
=begin
  helloerb1.cgi - Sample script for CGI/ERB

  Set UTF-8 forcely as output charset.

  Recommanded to set UTF-8 forcely because some web browser
  doesn't send HTTP_ACCEPT_CHARSET correctly.

  Copyright (C) 2005-2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end
$:.insert(0, "../../lib")

begin
  require 'rubygems'
rescue LoadError
end

require 'erb'
require 'gettext/cgi'

class SimpleContainer1
  include GetText

  
  def initialize(domainname, domainpath, cgi)
    set_cgi(cgi)
    bindtextdomain(domainname, :path => domainpath)
    @domainname = domainname
  end

  def description
    _("Sample script for CGI/ERB (UTF-8).")
  end

  def to_html(path)
    erb = ERB.new(IO.read(path)).src
    eval(erb, binding)
  end
end


GetText.output_charset = "UTF-8"

print "Content-type:text/html; charset=UTF-8\n\n"

cgi = CGI.new

con = SimpleContainer1.new("helloerb1", "locale", cgi)

if GetText.cgi["other"] == "true"
  print con.to_html("other.rhtml")
else
  print con.to_html("helloerb.rhtml")
end
