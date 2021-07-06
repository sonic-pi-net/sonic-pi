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
require "rake"
require "json"

require_relative "../core.rb"
require_relative "../lib/sonicpi/synths/synthinfo"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/sound"
require_relative "../lib/sonicpi/lang/minecraftpi"
require_relative "../lib/sonicpi/lang/midi"

include SonicPi::Util

namespace :docs do

end
def extract_comments(s)
  #puts("Example:")
  #print(s)
  #puts("End example")
  code = ""
  comments = ""
  s.each_line(chomp: true) do |l|
    m = l.match(/(.*?)[^&]?(#.*)/)
    if m
      code << m[1] << "\n"
      comments << m[2] << "\n"
    else

      #code << CGI.escapeHTML(l)
      if (l != "" && l != nil)
        code << l << "\n"
        comments << " \n"
      end
    end
  end
  #raise()
  return [code, comments]
end

def extract_synth_docs()
  synths = {}
  SonicPi::Synths::SynthInfo.get_all().sort_by { |k, v| v.synth_name() }.each do |k, v|
    next unless v.is_a? SonicPi::Synths::SynthInfo
    next if k.to_s.include? 'replace_'
    next if v.user_facing? == false

    name = v.name.gsub("Synth", "") # Remove prefix from some synth names
    id = v.synth_name()
    usage = {
      :function => "use_synth",
      :args => {}
    }
    usage[:args][":#{id}"] = "symbol"

    opts = {}
    v.arg_info.each do |arg, arg_info|
      opts[arg] = {
        :description => arg_info[:doc],
        :default => arg_info[:default],
        :bpm_scale => arg_info[:bpm_scale],
        :constraints => arg_info[:constraints],
        :modulatable => arg_info[:modulatable],
        :slidable => arg_info[:slidable]
      }
    end

    synths[id] = {
      :name => name,
      :description => v.doc,
      :introduced => v.introduced.to_s,
      :usage => usage,
      :opts => opts
    }
  end
  return synths
end

def extract_fx_docs()
  fx = {}
  SonicPi::Synths::SynthInfo.get_all().sort_by { |k, v| v.synth_name().gsub("fx_","") }.to_h.each do |k, v|
    next unless v.is_a? SonicPi::Synths::FXInfo
    next if k.to_s.include? 'replace_'
    next if v.user_facing? == false

    id = v.synth_name().gsub("fx_","")
    usage = {
      :function => "use_synth",
      :args => {}
    }
    usage[:args][":#{id}"] = "symbol"

    opts = {}
    v.arg_info.each do |arg, arg_info|
      opts[arg] = {
        :description => arg_info[:doc],
        :default => arg_info[:default],
        :bpm_scale => arg_info[:bpm_scale],
        :constraints => arg_info[:constraints],
        :modulatable => arg_info[:modulatable],
        :slidable => arg_info[:slidable]
      }
    end

    fx[id] = {
      :name => v.name,
      :description => v.doc,
      :introduced => v.introduced.to_s,
      :opts => opts
    }
  end
  return fx
end

def extract_samples()
  samples = {}
  SonicPi::Synths::SynthInfo.grouped_samples.sort_by { |key| key }.to_h.each do |k, v|
    group_id = k
    info = v[:desc]

    samples[group_id] = {
      :description => info,
      :samples => v[:samples].sort_by { |s| s }
    }
  end
  return samples
end

def extract_lang_docs()
  lang = {}
  SonicPi::Lang::Core.docs().sort.map do |k, v|
    unless(v[:hide])
      id = v[:name]
      summary = v[:summary] || v[:name]
      introduced = v[:introduced].to_s

      usage = {
        :function => id,
        :args => {}
      }
      args = {}
      v[:args].each do |arg|
        name, type = *arg
        if (name.to_s != "" && type != nil)
          args[name.to_s] = type.to_s
        end
        #print args
      end
      usage[:args] = args

      opts = {}
      if v[:opts]
        v[:opts].each do |opt_name, opt_doc|
          opts[opt_name] = {
            :description => opt_doc.to_s
          }
        end
      end

      examples = []
      if v[:examples] && !v[:examples].empty?
        v[:examples].each_with_index do |e, idx|
          code, comments = *extract_comments(e.strip)

          # Make sure the code and the comments are the same no. of lines
          #code_lines = code.split("\n")
          #comment_lines = comments.split("\n")
          #diff = comment_lines.length - code_lines.length
          #if (diff > 0)
          #  # Remove extra lines from the comments
          #  # comments = comment_lines[0..-diff].join("\n")
          #  # Add empty lines to the code
          #  for i in 0..diff
          #    code_lines << ""
          #  end
          #  code = code_lines.join("\n")
          #elsif (diff < 0)
          #  # Add empty lines to the comments
          #  for i in 0..-diff
          #    comment_lines << ""
          #  end
          #  comments = comment_lines.join("\n")
          #end

          examples << {
            :code => code,
            :comments => comments
          }
        end
      end

      lang[id] = {
        :summary => summary,
        :usage => usage,
        :description => v[:doc],
        :introduced => v[:introduced],
        :opts => opts,
        :examples => examples
      }
    end
  end
  return lang
end

# JSON
data = get_json_friendly_docs()
File.open( "#{etc_path}/doc/reference/synths.json", "w") do |f|
  f << JSON.pretty_generate(extract_synth_docs())
end

File.open( "#{etc_path}/doc/reference/fx.json", "w") do |f|
  f << JSON.pretty_generate(extract_fx_docs())
end

File.open( "#{etc_path}/doc/reference/samples.json", "w") do |f|
  f << JSON.pretty_generate(extract_samples())
end

File.open( "#{etc_path}/doc/reference/lang.json", "w") do |f|
  f << JSON.pretty_generate(extract_lang_docs())
end

# Cheatsheets
File.open( "#{cheatsheets_path}/synths.md", 'w' ) do |f|
 f << SonicPi::Synths::SynthInfo.synth_doc_markdown
end

File.open( "#{cheatsheets_path}/fx.md", 'w') do |f|
 f << SonicPi::Synths::SynthInfo.fx_doc_markdown
end

File.open( "#{cheatsheets_path}/samples.md", 'w') do |f|
 f << SonicPi::Synths::SynthInfo.samples_doc_markdown
end
