#!/usr/bin/env ruby
=begin
  helloerb2.cgi - Sample script for CGI/ERB

  Set locale/charset automaticaly.
  (from HTTP_ACCEPT_LANGUAGE, HTTP_ACCEPT_CHARSET sended by web browser).

  Recommanded to set UTF-8 forcely because some web browser
  doesn't send HTTP_ACCEPT_CHARSET correctly.(See helloerb.cgi)

  Copyright (C) 2005  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby.
=end
$:.insert(0, "../../lib")

begin
  require 'rubygems'
rescue LoadError
end

require 'gettext/cgi'
require 'erb'

class SimpleContainer2
  include GetText

  def initialize(domainname, domainpath)
    bindtextdomain(domainname, :path => domainpath)
    @domainname = domainname
  end

  def description
    _("Sample script for CGI/ERB (Auto-Detect charset).")
  end

  def to_html(path)
    erb = ERB.new(IO.read(path)).src
    eval(erb, binding)
  end
end

cgi = CGI.new
GetText.set_cgi(cgi)

print "Content-type:text/html; charset=#{Locale.charset}\n\n"

con = SimpleContainer2.new("helloerb2", "locale")

print con.to_html("helloerb.rhtml")
