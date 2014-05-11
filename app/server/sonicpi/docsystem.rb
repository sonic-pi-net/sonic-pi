#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

require 'cgi'

module SonicPi
  module DocSystem

    def self.included(base)

      class << base
        include SonicPi::Util

        @@docs ||= {}

        def docs
          @@docs
        end

        def docs_html_map
          res = {}
          @@docs.each do |k, v|
            unless(v[:hide])
              html = ""
              html << "<h2>#{v[:summary]}</h2>\n" if v[:summary]
              html << "<h2><pre>#{v[:name]}<pre></h2>\n"
              req_args = []
              v[:args].each do |arg|
                n, t = *arg
                req_args << "#{n} (#{t})"
              end
              html << "<h2><pre>[#{req_args.join(', ')}]</pre></h2>\n"
              html << "<h3>#{v[:doc]}</h3>\n"
              v[:examples].each_with_index do |e, idx|
                html << "<h3>Example #{idx + 1}</h3>\n"
                lines = e.split("\n").map{|l| CGI.escapeHTML(l)}
                html << "<pre>\n\n#{lines.join('<br/>')}\n\n</pre>\n"
              end
              res[k.to_s] = html
            end
          end
          res
        end

        def doc(*info)
          args_h = resolve_synth_opts_hash_or_array(info)
          @@docs[args_h[:name]] = args_h
        end
      end
    end
  end
end
