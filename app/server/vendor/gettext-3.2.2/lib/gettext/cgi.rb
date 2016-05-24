# -*- coding: utf-8 -*-

=begin
  gettext/cgi.rb - GetText for CGI

  Copyright (C) 2005-2009  Masao Mutoh

  You may redistribute it and/or modify it under the same
  license terms as Ruby or LGPL.
=end

require 'cgi'
require 'gettext'

Locale.init(:driver => :cgi)

module GetText

  # Sets a CGI object. This methods is appeared when requiring "gettext/cgi".
  # * cgi_: CGI object
  # * Returns: self
  def set_cgi(cgi_)
    Locale.set_cgi(cgi_)
  end

  # Same as GetText.set_cgi. This methods is appeared when requiring "gettext/cgi".
  # * cgi_: CGI object
  # * Returns: cgi_
  def cgi=(cgi_)
    set_cgi(cgi_)
    cgi_
  end

  # Gets the CGI object. If it is nil, returns new CGI object. This methods is appeared when requiring "gettext/cgi".
  # * Returns: the CGI object
  def cgi
    Locale.cgi
  end
end
