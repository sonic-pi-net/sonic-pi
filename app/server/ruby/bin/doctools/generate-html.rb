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
  opts.banner = "Usage: qt-doc.rb [options]"
  opts.on('-o', '--output NAME', 'Output filename') { |v| options[:output_name] = v }
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

info_sources = ["CHANGELOG.md", "CONTRIBUTORS.md", "COMMUNITY.md", "CORETEAM.html", "LICENSE.md"]

def change_img_links_for_website(input)
  output = input.gsub(":/images", "/assets/images").gsub("../../../etc/doc/images", "/assets/images")
  return output
end

def make_reference_html_section(section, lang, json_file)
  res = {}

  puts "Parsing #{json_file}"
  basename = File.basename(json_file)

  content = IO.read(json_file, :encoding => 'utf-8')
  content = content.to_s
  reference_hash = JSON.parse(content)

  html = {}
  titles = {}
  #toc = "<ul class=\"section_toc_list\">\n"
  #toc_level = 0
  toc_data = { :pages => [] }

  reference_hash.each do |k, v|
    doc = ""
    title = ""

    case basename
    when "synths.json"
      #puts v
      title = ActiveSupport::Inflector.titleize(k)

      doc << "<h1>" << v["name"] << "</h1>\n\n"

      doc << "<p><table class=\"arguments\"><tr>\n"
      cnt = 0
      v["opts"].each do |opt_name, opt_info|
        doc << "</tr><tr>" if (cnt > 0) and cnt % 4 == 0
        doc << "<td class=\"even\"><a href=\"#opt-#{opt_name}\">#{opt_name}:</a></td>\n"
        doc << "<td class=\"odd\">#{opt_info["default"]}</td>\n"
        cnt += 1
      end
      doc << "</tr></table></p>\n\n"

      doc << "<p class=\"usage\"><code><pre>"
      doc << "use_synth <span class=\"symbol\">:#{k}</span>"
      doc << "</pre></code></p>\n"

      doc << Kramdown::Document.new(v["description"]).to_html << "\n"

      doc << "<p class=\"introduced\">"
      doc << "Introduced in " << v["introduced"] << "</p>\n\n"

      doc << "<section id=\"options\">\n"
      doc << "<h2>Options</h2>\n"

      doc << "<p><table class=\"details\">\n"

      cnt = 0
      v["opts"].each do |ak, av|
        td_class = cnt.even? ? "even" : "odd"
        doc << "<tr id=\"opt-#{ak}\">\n"
        doc << " <td class=\"#{td_class} key\">#{ak}:</td>\n"
        doc << " <td class=\"#{td_class}\">\n"
        docstring = av["description"] || 'write me'
        doc <<  Kramdown::Document.new(docstring).to_html
        doc << "  <p class=\"properties\">\n"
        doc << "   Default: #{av["default"]}\n"
        doc << "   <br/>#{av["constraints"].join(",").capitalize}\n" unless av["constraints"].empty?
        doc << "   <br/>#{av["modulatable"] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
        doc << "   <br/><a href=\"#slide\">Has slide options to shape changes</a>\n" if av["slidable"]
        doc << "   <br/>Scaled with current BPM value\n" if av["bpm_scale"]
        doc << "  </p>\n"
        doc << " </td>\n"
        doc << "</tr>\n"
        cnt += 1
      end
      doc << "</table></p>\n"

      if (v["slide"]) then
        # table for slide parameters
        doc << "<section id=\"slide\">\n"
        doc << "<p>#{v["slide"]["slide_doc"]}</p>\n"
        doc << "<p><table class=\"details\">\n"

        cnt = 0
        v["slide"]["slide_args"].each do |ak, av|
          td_class = cnt.even? ? "even" : "odd"
          doc << "<tr>\n"
          doc << " <td class=\"#{td_class} key\">#{ak}:</td>\n"
          doc << " <td class=\"#{td_class}\">\n"
          doc << "  <p>#{av["doc"] || 'write me'}</p>\n"
          doc << "  <p class=\"properties\">\n"
          doc << "   Default: #{av["default"]}\n"
          doc << "  </p>\n"
          doc << " </td>\n"
          doc << "</tr>\n"
          cnt += 1
        end
        doc << "</table></p>\n"
        doc << "</section>\n"
      end

      doc << "</section>\n\n"
      doc << "</body>\n"

    when "fx.json"
      title = v["name"]

      doc << "<h1>" << v["name"] << "</h1>\n\n"

      doc << "<p><table class=\"arguments\"><tr>\n"
      cnt = 0
      v["opts"].each do |opt_name, opt_info|
        doc << "</tr><tr>" if (cnt > 0) and cnt % 4 == 0
        doc << "<td class=\"even\"><a href=\"##{opt_name}\">#{opt_name}:</a></td>\n"
        doc << "<td class=\"odd\">#{opt_info["default"]}</td>\n"
        cnt += 1
      end
      doc << "</tr></table></p>\n\n"

      doc << "<p class=\"usage\"><code><pre>"
      doc << "with_fx <span class=\"symbol\">:#{k}</span>"
      doc << "</pre></code></p>\n"

      doc << Kramdown::Document.new(v["description"]).to_html << "\n"

      doc << "<p class=\"introduced\">"
      doc << "Introduced in " << v["introduced"] << "</p>\n\n"

      doc << "<h2>Options</h2>\n"

      doc << "<p><table class=\"details\">\n"

      cnt = 0
      any_slidable = false
      v["opts"].each do |ak, av|
        td_class = cnt.even? ? "even" : "odd"
        doc << "<a name=\"#{ak}\"></a>\n"
        doc << "<tr>\n"
        doc << " <td class=\"#{td_class} key\">#{ak}:</td>\n"
        doc << " <td class=\"#{td_class}\">\n"
        docstring = av["description"] || 'write me'
        doc <<  Kramdown::Document.new(docstring).to_html
        doc << "  <p class=\"properties\">\n"
        doc << "   Default: #{av["default"]}\n"
        doc << "   <br/>#{av["constraints"].join(",").capitalize}\n" unless av["constraints"].empty?
        doc << "   <br/>#{av["modulatable"] ? "May be changed whilst playing" : "Can not be changed once set"}\n"
        doc << "   <br/><a href=\"#slide\">Has slide options to shape changes</a>\n" if av["slidable"]
        doc << "   <br/>Scaled with current BPM value\n" if av["bpm_scale"]
        doc << "  </p>\n"
        doc << " </td>\n"
        doc << "</tr>\n"
        any_slidable = true if av["slidable"]
        cnt += 1
      end
      doc << "</table></p>\n"

      #if any_slidable then
      #  doc << SonicPi::Synths::SynthInfo.slide_doc_html(k.to_sym)
      #end # any_slidable

    when "samples.json"
      title = v["description"]
      doc << "<h1>" << v["description"] << "</h1>\n\n"
      doc << "<ul>\n"
      v["samples"].each do |sample_name|
        doc << "<li>\n<code><pre>\n<span class=\"symbol\">\n:#{sample_name}\n</span>\n</pre></code>\n</li>\n"
      end
      doc << "</ul>\n"

    when "lang.json"
      title = k

      summary = v["summary"]
      summary[0] = summary[0].capitalize
      doc << "<h1>" << summary << "</h1>\n"
      doc << "<p class=\"usage\"><code><pre><span class=\"symbol\">#{v["usage"]["function"]}</span> "
      req_args = []
      if v["usage"]["args"]
        v["usage"]["args"].each do |arg_name, arg_type|
          req_args << "#{arg_name} <span class=\"info\">(#{arg_type})</span>"
        end
      end
      doc << " #{req_args.join(', ')}</pre></code></p>\n"

      doc << Kramdown::Document.new(v["description"]).to_html << "\n"

      doc << "<p class=\"introduced\">"
      doc << "Introduced in " << v["introduced"] << "</p>\n\n"

      if v[:opts] && !v[:opts].empty?

        doc << "<h2>Options</h2>"
        doc << "<p><table class=\"details\">\n"

        cnt = 0
        v["opts"].each do |opt_name, opt_info|
          td_class = cnt.even? ? "even" : "odd"
          doc << "<tr>"
          doc << " <td class=\"#{td_class} key\">#{opt_name}:</td>\n"
          doc << " <td class=\"#{td_class}\">\n"
          doc << Kramdown::Document.new(opt_info["description"]).to_html << "\n"
          doc << " </td>\n"
          doc << "</tr>\n"
          cnt += 1
        end
        doc << "</table></p>"
      end

      if v["examples"] && !v["examples"].empty?
        doc << "<h2>Example#{"s" if v["examples"].count > 1}</h2>\n"
        doc << "<p><table class=\"examples\">\n"

        v["examples"].each_with_index do |e, idx|

          td_class = idx.even? ? "even" : "odd"

          doc << " <tr>\n"
          doc << "  <td colspan=\"2\" class=\"#{td_class} head\"># Example #{idx+1}</td>\n"
          doc << " </tr><tr>\n"
          doc << "  <td class=\"#{td_class}\">\n"
          doc << "   <p><code><pre>\n#{e["code"] << "\n\n\n"}</pre></code></p>\n"
          doc << "  </td>\n"
          doc << "  <td class=\"#{td_class}\">\n"
          doc << "   <p><code><pre>\n#{e["comments"] << "\n\n\n"}</pre></code></p>\n"
          doc << "  </td>\n"
          doc << " </tr>\n"
        end
        doc << "</table></p>\n"
      end
    end

    #if titleize == :titleize then
    #  title = ActiveSupport::Inflector.titleize(title)
    #  # HPF et al get capitalized
    #  if name == 'fx' and title =~ /pf$/ then
    #    title = title.upcase
    #  end
    #end

    if title.start_with?("   ") then
      if parent == nil then
        parent = toc_data[section][:pages].count - 1
      end
    else
      if parent != nil then
        parent = nil
      end
    end

    if (parent != nil)
      toc_data[:pages][parent][:subpages].append({
          :name => title.gsub(/"/, '&quot;'),
          :url =>"/#{lang}/reference/#{section}/#{k.gsub("?","%3F")}.html"
      })
    else
      toc_data[:pages].append({
          :name => title.gsub(/"/, '&quot;'),
          :url =>"/#{lang}/reference/#{section}/#{k.gsub("?","%3F")}.html",
          :subpages => []
      })
    end

    html[k] = doc
    titles[k] = title
  end

  return [html, titles, toc_data]
end

example_html_map = {}
example_dirs = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]
example_dirs.each do |ex_dir|
  Dir["#{examples_path}/#{ex_dir.downcase}/*.rb"].each do |path|
    bname = File.basename(path, ".rb")
    bname = ActiveSupport::Inflector.titleize(bname)
    name = "[#{ex_dir}] #{bname}"
    lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}
    html = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
    html << "<body class=\"example\">\n"
    html << '<h1>'
    html << "# #{bname}"
    html << '</h1>'
    html << "<p><pre><code>\n"

    html << "#{lines.join("\n")}\n\n</code></pre></p>\n"
    html << "</body>\n"
    example_html_map[name] = html
  end
end

puts "Updating list of languages..."
FileUtils.rm_r("#{etc_path}/doc/html/generated_src/_data") if Dir.exists?("#{etc_path}/doc/html/generated_src/_data")
FileUtils.mkdir_p("#{etc_path}/doc/html/generated_src/_data/toc")
File.open("#{etc_path}/doc/html/generated_src/_data/languages.json", 'w') do |f|
  lang_list = {}
  languages.sort.each do |lang|
    lang_list[lang] = @lang_names[lang]
  end
  f << JSON.pretty_generate(lang_list)
end

#File.open("#{etc_path}/doc/html/generated_src/_includes/lang_switcher.html", "w") do |f|
#  f << "<div class=\"language-combobox\">\n"
#  f << "<label for=\"language-listbox\">Language</label>\n"
#  f << "<select id=\"language-listbox\" name=\"language\">\n"
#  languages.sort.each do |lang|
#    f << "<option id=\"language-option-#{lang}\" role=\"option\">#{@lang_names[lang]}</li>\n"
#  end
#  f << "</select>\n"
#  f << "</div>\n"
#end

#make_tab.call("examples", example_html_map, false, false, false, true)
#make_tab.call("synths", SonicPi::Synths::SynthInfo.synth_doc_html_map, :titleize, true, true, true)
#make_tab.call("fx", SonicPi::Synths::SynthInfo.fx_doc_html_map, :titleize, true, true, true)
#make_tab.call("samples", SonicPi::Synths::SynthInfo.samples_doc_html_map, false, true, false, true)
puts "Copying shared assets..."
FileUtils.cp_r("#{etc_path}/doc/html/src/.", "#{etc_path}/doc/html/generated_src")

puts "Generating doc HTML pages..."
languages.each do |lang|
  toc_list = {}
  generated_toc = ""

  ["tutorial", "synths", "fx", "samples", "lang"].each do |section|
    qt_output_folder = ""
    html_output_folder = ""
    relative_css_path = ""
    html = {}
    titles = {}
    toc_data = {
      :pages => [
      ]
    }

    if (section == "tutorial")
      qt_output_folder = "#{root_path}/app/gui/qt/help/#{lang}/tutorial"
      html_output_folder = "#{etc_path}/doc/html/generated_src/#{lang}/tutorial"
      #relative_css_path = "../../_theme"

      md_path = "#{etc_path}/doc/generated/#{lang}/tutorial"
      md_path = "#{etc_path}/doc/tutorial" if (lang == "en")

      parent = nil

      Dir["#{md_path}/*.md"].sort.each do |path|
        f = File.open(path, 'r:UTF-8')
        # read first line (title) of the markdown, use as title
        name = f.readline.strip
        # indent subchapters
        name = "   #{name}" if name.match(/\A[A-Z0-9]+\.[ ]*[0-9]+ /)
        id = File.basename(path).match(/(.*)\.(.*)/)[1]

        # read remaining content of markdown
        markdown = f.read
        html[id] = SonicPi::MarkdownConverter.convert markdown
        titles[id] = name

        if name.start_with?("   ") then
          if parent == nil then
            parent = toc_data[:pages].count - 1
          end
        else
          if parent != nil then
            parent = nil
          end
        end

        if (parent != nil)
          toc_data[:pages][parent][:subpages].append({
              :name => name.gsub(/"/, '&quot;'),
              :url =>"/#{lang}/tutorial/#{id.gsub("?","%3F")}.html"
          })
        else
          toc_data[:pages].append({
              :name => name.gsub(/"/, '&quot;'),
              :url =>"/#{lang}/tutorial/#{id.gsub("?","%3F")}.html",
              :subpages => []
          })
        end

      end

    else
      qt_output_folder = "#{root_path}/app/gui/qt/help/#{lang}/reference/#{section}"
      html_output_folder = "#{etc_path}/doc/html/generated_src/#{lang}/reference/#{section}"
      relative_css_path = "../../../_theme"

      json_path = (lang == "en") ? "#{etc_path}/doc/reference/#{section}.json" : "#{etc_path}/doc/generated/#{lang}/reference/#{section}.json"

      html, titles, toc_data = make_reference_html_section(section, lang, json_path)
    end


    FileUtils.rm_r qt_output_folder if Dir.exists?(qt_output_folder)
    FileUtils.mkdir_p qt_output_folder

    FileUtils.rm_r html_output_folder if Dir.exists?(html_output_folder)
    FileUtils.mkdir_p html_output_folder

    # Qt GUI output
    html.each do |item_name, doc|
      output_path = "#{qt_output_folder}/#{item_name}.html"
      File.open(output_path, 'w') do |f|
        f << "<!doctype html>\n<html>\n"
        f << "<head>\n"
        f << "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n"
        f << "</head>\n"
        f << "<body class=\"manual\">\n"
        f << doc << "\n"
        f << "</body>\n"
        f << "</html>\n"
      end

      output_path = "#{html_output_folder}/#{item_name}.html"
      File.open(output_path, 'w') do |f|
        f << "---\n"
        f << "layout: doc_manual\n"
        f << "title: \"#{titles[item_name].gsub(/"/, '\\"')}\"\n"
        f << "lang: #{lang}\n"
        f << "section: #{section}\n"
        f << "---\n"
        f << change_img_links_for_website(doc) << "\n"
      end
    end
    toc_list[section] = toc_data
  end

  info_toc = {
    :pages => [
    ]
  }
  info_sources.each do |src|
    input_path = "#{root_path}/#{src}"
    base = File.basename(input_path)
    m = base.match /(.*)\.(.*)/
    bn = m[1]
    ext = m[2]
    info_toc[:pages].append({
      :name => bn.capitalize.gsub(/"/, '&quot;'),
      :url => "/#{lang}/info/#{bn.gsub("?","%3F")}.html",
    })
  end
  toc_list["info"] = info_toc

  File.open("#{etc_path}/doc/html/generated_src/_data/toc/#{lang}.json", "w") do |f|
    f << JSON.pretty_generate(toc_list)
  end
end

# Copy images
FileUtils.rm_r("#{etc_path}/doc/html/generated_src/assets/images") if Dir.exists?("#{etc_path}/doc/html/generated_src/assets/images")
FileUtils.cp_r("#{etc_path}/doc/images", "#{etc_path}/doc/html/generated_src/assets/images")

###
# Generate info pages
###
puts "Generating info html pages..."
FileUtils.rm_r("#{etc_path}/doc/html/generated_src/info/") if Dir.exists?("#{etc_path}/doc/html/generated_src/info/")
FileUtils.mkdir_p("#{etc_path}/doc/html/generated_src/info/")

outputdir = "#{etc_path}/doc/html/generated_src"
info_toc = ""

info_sources.each do |src|
  input_path = "#{root_path}/#{src}"
  base = File.basename(input_path)
  m = base.match /(.*)\.(.*)/
  bn = m[1]
  ext = m[2]

  input = IO.read(input_path, :encoding => 'utf-8')
  output =  "---\n"
  output << "layout: info_page\n"
  output << "section: info\n"
  output << "title: #{bn.capitalize.gsub(/"/, '\\"')}\n"
  output << "---\n"

  # Edit image links to correct places for the website
  input = change_img_links_for_website(input)

  if ext == "md"
    output << input << "\n"
  else
    output << SonicPi::MarkdownConverter.massage!(input)
  end

  output_path = "#{outputdir}/info/#{bn}.#{ext}"

  File.open(output_path, 'w') do |f|
    f << output
  end

end

File.open("#{etc_path}/doc/html/generated_src/_data/version.yml", "w") do |f|
  f << "- version: \"3.3.1\""
end

# info_sources.each do |src|
#
#   input_path = "#{root_path}/#{src}"
#   base = File.basename(input_path)
#   m = base.match /(.*)\.(.*)/
#   bn = m[1]
#   ext = m[2]
#
#   input = IO.read(input_path, :encoding => 'utf-8')
#   if ext == "md"
#     html = SonicPi::MarkdownConverter.convert(input)
#   else
#     html = SonicPi::MarkdownConverter.massage!(input)
#   end
#
#   output_path = "#{outputdir}/#{bn}.html"
#
#   File.open(output_path, 'w') do |f|
#     f << html
#   end
#
# end
