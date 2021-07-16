#!/usr/bin/env ruby
#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require 'cgi'
require 'optparse'
require 'fileutils'

require_relative "../../core.rb"
require_relative "../../lib/sonicpi/synths/synthinfo"
require_relative "../../lib/sonicpi/util"
require_relative "../../lib/sonicpi/runtime"
require_relative "../../lib/sonicpi/lang/core"
require_relative "../../lib/sonicpi/lang/sound"
require_relative "../../lib/sonicpi/lang/minecraftpi"
require_relative "../../lib/sonicpi/lang/midi"
require_relative "../../lib/sonicpi/version"

require_relative "./lang-names"

require 'active_support/inflector'
require 'json'

include SonicPi::Util

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: generate-book.rb [options]"
  opts.on('-l', '--lang LANG', 'Language to generate (if empty, defaults to all languages)') { |v| options[:language] = v }
end.parse!

languages = []
if options[:language] == nil
  # this will sort locale code names by reverse length
  # to make sure that a more specific locale is handled
  # before the generic language code,
  # e.g., "de_CH" should be handled before "de"
  languages =
    Dir["#{etc_path}/doc/translations/reference/sonic-pi-reference-*.po"].
    map { |p| File.basename(p).gsub(/sonic-pi-reference-(.*?).po/, '\1') }.
    sort_by {|n| -n.length}

  languages << "en"
else
  languages = [options[:language]]
end

def change_img_links_for_book(input)
  output = input.gsub(":/images", "../../images").gsub("../../../etc/doc/images", "../../images")
  return output
end

def generate_book(lang)
  json_toc = ""
  File.open("#{etc_path}/doc/generated_html/#{lang}/toc.json", 'r:UTF-8') do |f|
    json_toc = f.read()
  end
  toc = JSON.parse(json_toc)

  toc.each do |section, sec_list|
    content = ""
    html_toc = "<ul>"
    sec_list["pages"].each do |page_info|
      page_content, page_toc = generate_page_and_subpages(page_info, section)
      content << page_content
      html_toc << page_toc
    end

    title = "Sonic Pi - #{section.capitalize}" + (lang != "en" ? " (#{lang})" : "")

    FileUtils.mkdir_p("#{qt_gui_path}/book/#{lang}/")

    File.open("#{qt_gui_path}/book/#{lang}/#{title}.html", 'w') do |f|
      f << %{
        <!doctype html>
        <html lang="#{lang}">
          <head>
            <title>#{title}</title>
            <link rel="stylesheet" href="../../theme/light/doc-styles.css" type="text/css"/>
            <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
          </head>

          <body class="manual">
            <header>
              <h1>#{title}</h1>
            </header>

            <nav>
              #{html_toc}
              <hr>
            </nav>

            <main>
              #{content}
            </main>
          </body>

        </html>
      }
    end

  end
end

def generate_page_and_subpages(page_info, section)
  puts page_info["path"]

  id = File.basename(page_info["path"], ".html").gsub("?","%3F")

  content = "<section id=\"#{id}\">\n"
  File.open("#{etc_path}/doc/generated_html/#{page_info["path"]}", 'r:UTF-8') do |f|
    content << change_img_links_for_book(f.read()) << "\n"
  end
  content << "<hr>\n"

  toc = "<li>"
  toc << "<a href=\"##{id}\">#{page_info["name"].gsub(/"/, '&quot;')}</a>"
  toc << "</li>"

  if page_info.has_key?("subpages")
    if (page_info["subpages"].length() > 0)
      toc << "<ul class=\"toc_list\">\n"

      page_info["subpages"].each do |subpage_info|
        subpage_content, subpage_toc = generate_page_and_subpages(subpage_info, section)
        content << subpage_content
        toc << subpage_toc
      end

      toc << "</ul>\n"
    end
  end

  content << "</section>\n"

  return content, toc
end

###
# Generate doc HTML pages and tables of contents
###
puts "Generating doc HTML pages..."
languages.each do |lang|
  generate_book(lang)
end
