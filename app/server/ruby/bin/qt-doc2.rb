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

require_relative "../core.rb"
require_relative "../lib/sonicpi/lang/support/docsystem"
require_relative "../lib/sonicpi/synths/synthinfo"
require_relative "../lib/sonicpi/util"
require_relative "../lib/sonicpi/runtime"
require_relative "../lib/sonicpi/lang/core"
require_relative "../lib/sonicpi/lang/sound"
require_relative "../lib/sonicpi/lang/minecraftpi"
require_relative "../lib/sonicpi/lang/midi"
require 'active_support/inflector'
require 'erb'
require 'gettext'
require "gettext/tools/xgettext"


class QtDocs
  include SonicPi::Lang::Support::DocSystem
  include SonicPi::Util
  include GetText

  def doc_collections
    synths = SonicPi::Synths::SynthInfo.get_all.select do |k, v|
      v.is_a?(SonicPi::Synths::SynthInfo) && v.user_facing?
    end
    [
      {
        items: synths,
        template_path: synth_and_fx_template_path,
        interpolated_path: synths_interpolated_path,
        output_path: synths_toml_path,
        klass: SonicPi::Synths::SynthInfo
      },
      {
        items: @@docs,
        template_path: lang_template_path,
        interpolated_path: lang_interpolated_path,
        output_path: lang_toml_path
      }
    ]
  end

  def run
    _generate_docs
  end

  def t_(arg)
    "<%= _(\"#{arg}\") %>"
  end

  private
  def _generate_docs
    interpolated_file_paths = doc_collections.each_with_object([]) do |collection, paths|
      original_template = File.read(collection[:template_path])
      collection[:items].to_a.take(2).each do |key, item|
        interpolated_file_path = "#{collection[:interpolated_path]}/#{key}.toml.erb"
        template = ERB.new(original_template).result(binding)
        File.open(interpolated_file_path, 'w') do |f|
          f.write template
        end
        paths << interpolated_file_path
      end
    end
    _generate_pot_file(interpolated_file_paths)
    _generate_toml_files

  end


  def _generate_pot_file(interpolated_file_paths)
    GetText::Tools::XGetText.run(
      *interpolated_file_paths,
      "-o ../../../../../../etc/doc/generated/toml/synths/test.pot"
    )
  end

  def _generate_toml_files
    doc_collections.each do |collection|
      collection[:items].to_a.take(2).each do |key, item|
        interpolated_file = File.read("#{collection[:interpolated_path]}/#{key}.toml.erb")
        output = ERB.new(interpolated_file).result(binding)
        File.open("#{collection[:output_path]}/#{key}.toml", 'w') do |f|
          f.write output
        end
      end
    end
  end
end

QtDocs.new.run
