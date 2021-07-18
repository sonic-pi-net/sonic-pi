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
require 'json'

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

include SonicPi::Util

options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: create-html.rb [options]"
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

# Remove any invalid file name characters (mostly for Windows)
def make_valid_file_name(basename)
  return basename.gsub(/[\/:*?"<>|]/, "_")
end

def make_reference_html_section(section, lang, json_file)
  res = {}

  puts "Parsing #{json_file}"
  basename = File.basename(json_file)

  content = IO.read(json_file, :encoding => 'utf-8')
  content = content.to_s
  reference_hash = JSON.parse(content)

  html = {}
  #toc = "<ul class=\"section_toc_list\">\n"
  #toc_level = 0
  toc_data = { :pages => [] }

  reference_hash.each do |k, v|
    doc = ""
    title = ""
    keyword = true

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

      doc << "<section id=\"#{k}_options\">\n"
      doc << "<h2>Options</h2>\n"

      doc << "<p><table class=\"details\">\n"

      cnt = 0
      v["opts"].each do |ak, av|
        td_class = cnt.even? ? "even" : "odd"
        doc << "<tr id=\"#{k}_opt-#{ak}\">\n"
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
        doc << "<section id=\"#{k}_slide\">\n"
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
      keyword = false
      doc << "<h1>" << v["description"] << "</h1>\n\n"
      doc << "<ul>\n"

      doc << "<table>\n"
      v["samples"].each do |sample_name|
        doc << "<tr style=\"vertical-align: middle;padding:5px\">\n"
        doc << "<td><a href=\"sonicpi://play-sample/#{sample_name}\"><img src=\":/images/play.png\" width=\"15\" height=\"16\"></a></td>\n"
        doc << "<td><p class=\"usage\"><code><pre> sample <span class=\"symbol\">:#{sample_name}</span> </pre></code></p></td>\n"
        doc << "</tr>\n"
      end
      doc << "</table>\n"

      # TODO: Re-add sample args to documentation
      #
      # stereo_player = StereoPlayer.new
      # stereo_player.arg_info.each do |ak, av|
      #   doc << "</tr><tr>" if (cnt > 0) and cnt % 4 == 0
      #   doc << "<td class=\"even\"><a href=\"##{ak}\">#{ak}:</a></td>\n"
      #   doc << "<td class=\"odd\">#{av[:default]}</td>\n"
      #   cnt += 1
      # end
      # doc << "</tr></table></p>\n"
      #
      # doc << "<p><table class=\"details\">\n"
      #
      # cnt = 0
      # any_slidable = false
      # stereo_player.arg_info.each do |ak, av|
      #   doc << "<a name=\"#{ak}\"></a>\n"
      #   doc << "<tr>\n"
      #   doc << " <td class=\"even key\">#{ak}:</td>\n"
      #   doc << " <td class=\"odd\">\n"
      #   doc << "  <p>#{av[:doc] || 'write me'}</p>\n"
      #   doc << "  <p class=\"properties\">\n"
      #   doc << "   Default: #{av[:default]}\n"
      #   doc << "   <br/>#{av[:constraints].join(",")}\n" unless av[:constraints].empty?
      #   if av[:slidable]
      #     doc << "   <br/>May be changed whilst playing\n"
      #     doc << "   <br/><a href=\"#slide\">Has slide options to shape changes</a>\n"
      #     any_slidable = true
      #   end
      #   doc << "   <br/>Scaled with current BPM value\n" if av[:bpm_scale]
      #   doc << "  </p>\n"
      #   doc << " </td>\n"
      #   doc << "</tr>\n"
      #   cnt += 1
      # end
      # doc << "</table></p>\n"
      # doc << slide_doc_html(stereo_player) if any_slidable

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

    file_name = make_valid_file_name(k)

    page_info = {
        :name => title,
        :path => "#{lang}/reference/#{section}/#{file_name}.html"
    }

    page_info[:keyword] = k if keyword

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
      toc_data[:pages][parent][:subpages].append(page_info)
    else
      page_info[:subpages] = []
      toc_data[:pages].append(page_info)
    end

    html[file_name] = doc
  end

  return [html, toc_data]
end

# Generate example HTML pages
example_toc = []
example_dirs = ["Apprentice", "Illusionist", "Magician", "Sorcerer", "Wizard", "Algomancer"]
example_dirs.each do |ex_dir|

  html_output_folder = "#{etc_path}/doc/generated_html/examples/#{ex_dir.downcase}"
  FileUtils.rm_r html_output_folder if Dir.exists?(html_output_folder)
  FileUtils.mkdir_p html_output_folder

  puts "Generating #{html_output_folder}"

  Dir["#{examples_path}/#{ex_dir.downcase}/*.rb"].each do |path|
    bname = File.basename(path, ".rb")
    bname = ActiveSupport::Inflector.titleize(bname)
    name = "[#{ex_dir}] #{bname}"
    lines = IO.readlines(path).map(&:chop).map{|s| CGI.escapeHTML(s)}

    html = '<h1>'
    html << "# #{bname}"
    html << '</h1>'
    html << "<p><pre><code>\n"
    html << "#{lines.join("\n")}\n\n</code></pre></p>\n"

    File.open("#{html_output_folder}/#{bname}.html", "w") do |f|
      f.write(html)
    end

    example_toc.append({
        :name => "[#{ex_dir}] #{bname}",
        :path => "examples/#{ex_dir.downcase}/#{bname}.html"
    })

  end
end

File.open("#{etc_path}/doc/generated_html/examples/toc.json", "w") do |f|
  f << JSON.pretty_generate({
      :examples => example_toc
    })
end

#make_tab.call("examples", example_html_map, false, false, false, true)
#make_tab.call("synths", SonicPi::Synths::SynthInfo.synth_doc_html_map, :titleize, true, true, true)
#make_tab.call("fx", SonicPi::Synths::SynthInfo.fx_doc_html_map, :titleize, true, true, true)
#make_tab.call("samples", SonicPi::Synths::SynthInfo.samples_doc_html_map, false, true, false, true)

puts "Generating doc HTML pages..."
languages.each do |lang|
  toc_list = {}
  generated_toc = ""

  ["tutorial", "synths", "fx", "samples", "lang"].each do |section|
    html_output_folder = ""
    html = {}
    toc_data = {
      :pages => [
      ]
    }

    if (section == "tutorial")
      html_output_folder = "#{etc_path}/doc/generated_html/#{lang}/tutorial"
      md_path = (lang == "en") ? "#{etc_path}/doc/tutorial" : "#{etc_path}/doc/generated/#{lang}/tutorial"

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
        # Remove the first 3 lines and the last line - we don't need the meta or head tags yet!
        html[id] = SonicPi::MarkdownConverter.convert(markdown).lines[3..-2].join()
        title = name

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
              :name => title,
              :path =>"#{lang}/tutorial/#{id}.html"
          })
        else
          toc_data[:pages].append({
              :name => title,
              :path =>"#{lang}/tutorial/#{id}.html",
              :subpages => []
          })
        end

      end

    else
      html_output_folder = "#{etc_path}/doc/generated_html/#{lang}/reference/#{section}"
      json_path = (lang == "en") ? "#{etc_path}/doc/reference/#{section}.json" : "#{etc_path}/doc/generated/#{lang}/reference/#{section}.json"

      html, toc_data = make_reference_html_section(section, lang, json_path)
    end

    FileUtils.rm_r html_output_folder if Dir.exists?(html_output_folder)
    FileUtils.mkdir_p html_output_folder



    html.each do |item_name, doc|
      # if (section == "tutorial")
      #   puts "================================ #{item_name} ==================================="
      #   puts doc
      # end
      output_path = "#{html_output_folder}/#{item_name}.html"
      File.open(output_path, 'w') do |f|
        f << doc << "\n"
      end
    end

    toc_list[section] = toc_data
  end

  File.open("#{etc_path}/doc/generated_html/#{lang}/toc.json", "w") do |f|
    f << JSON.pretty_generate(toc_list)
  end
end
