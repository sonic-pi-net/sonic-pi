# -*- coding: utf-8 -*-
#
#--
# Copyright (C) 2009-2014 Thomas Leitner <t_leitner@gmx.at>
#
# This file is part of kramdown which is licensed under the MIT.
#++
#

module Kramdown

  # This module contains all available converters, i.e. classes that take a root Element and convert
  # it to a specific output format. The result is normally a string. For example, the
  # Converter::Html module converts an element tree into valid HTML.
  #
  # Converters use the Base class for common functionality (like applying a template to the output)
  # \- see its API documentation for how to create a custom converter class.
  module Converter

    autoload :Base, 'kramdown/converter/base'
    autoload :Html, 'kramdown/converter/html'
    autoload :Latex, 'kramdown/converter/latex'
    autoload :Kramdown, 'kramdown/converter/kramdown'
    autoload :Toc, 'kramdown/converter/toc'
    autoload :RemoveHtmlTags, 'kramdown/converter/remove_html_tags'
    autoload :Pdf, 'kramdown/converter/pdf'

  end

end
