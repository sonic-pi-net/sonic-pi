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
require_relative '../../util'
require_relative '../../markdown_converter'


module SonicPi
  module Lang
    module Support
      module DocSystem

        def self.included(base)

          class << base
            include SonicPi::Util

            @@docs ||= {}

            def docs
              @@docs
            end

            def memoizable_fns
              res = []
              @@docs.each do |k, v|
                res << k if v[:memoize]
              end
              res
            end

            # Generates HTML for Lang part of help system
            def docs_html_map
              res = {}

              extract_comments = lambda do |s|
                code = ""
                comments = ""
                s.each_line do |l|
                  m = l.match(/(.*?)[^&]?(#.*)/)
                  if m

                    code << CGI.escapeHTML(m[1]) << "\n"
                    comments << CGI.escapeHTML(m[2]) << "\n"
                  else
                    code << CGI.escapeHTML(l)
                    comments << " \n"
                  end
                end
                [code, comments]
              end
              @@docs.each do |k, v|
                unless(v[:hide])
                  html = "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>\n\n"
                  html << "<body class=\"manual\">\n"

                  summary = (v[:summary] || v[:name]).to_s
                  summary[0] = summary[0].capitalize
                  html << "<h1>" << summary << "</h1>\n"
                  html << "<p class=\"usage\"><code><pre><span class=\"symbol\">#{v[:name]}</span> "
                  req_args = []
                  raise "no args defined for #{v[:name]}" unless v[:args]
                  v[:args].each do |arg|
                    n, t = *arg
                    req_args << "#{n} <span class=\"info\">(#{t})</span>"
                  end
                  html << " #{req_args.join(', ')}</pre></code></p>\n"

                  html << Kramdown::Document.new(v[:doc]).to_html << "\n"

                  html << "<p class=\"introduced\">"
                  html << "Introduced in " << v[:introduced].to_s << "</p>\n\n"

                  if v[:opts] && !v[:opts].empty?

                    html << "<h2>Options</h2>"
                    html << "<p><table class=\"details\">\n"

                    cnt = 0
                    v[:opts].each do |opt_name, opt_doc|
                      td_class = cnt.even? ? "even" : "odd"
                      html << "<tr>"
                      html << " <td class=\"#{td_class} key\">#{opt_name.to_s}:</td>\n"
                      html << " <td class=\"#{td_class}\">\n"
                      html << Kramdown::Document.new(opt_doc.to_s).to_html << "\n"
                      html << " </td>\n"
                      html << "</tr>\n"
                      cnt += 1
                    end
                    html << "</table></p>"
                  end

                  if v[:examples] && !v[:examples].empty?
                    html << "<h2>Example#{"s" if v[:examples].count > 1}</h2>\n"
                    html << "<p><table class=\"examples\">\n"

                    v[:examples].each_with_index do |e, idx|

                      td_class = idx.even? ? "even" : "odd"

                      html << " <tr>\n"
                      html << "  <td colspan=\"2\" class=\"#{td_class} head\"># Example #{idx+1}</td>\n"
                      html << " </tr><tr>\n"
                      code, comments = *extract_comments.call(e.strip)
                      html << "  <td class=\"#{td_class}\">\n"
                      html << "   <p><code><pre>\n#{code << "\n\n\n"}</pre></code></p>\n"
                      html << "  </td>\n"
                      html << "  <td class=\"#{td_class}\">\n"
                      html << "   <p><code><pre>\n#{comments << "\n\n\n"}</pre></code></p>\n"
                      html << "  </td>\n"
                      html << " </tr>\n"
                    end
                    html << "</table></p>\n"
                  end

                  html << "</body>\n"
                  res[k.to_s] = html
                end
              end
              res
            end

            def doc(*info)
              args_h = resolve_synth_opts_hash_or_array(info)
              @@docs[args_h[:name]] = args_h
            end

            def vec_fns
              @@docs.values.select do |info|
                info[:returns] == :ring || info[:returns] == :vector || info[:returns] == :ramp
              end
            end
          end
        end
      end
    end
  end
end
