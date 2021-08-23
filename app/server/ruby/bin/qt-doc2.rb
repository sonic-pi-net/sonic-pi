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

  def run
    generate_docs
    sync_to_documentation_app
  end

  def t_(arg)
    # this didn't work on docstrings containing "blah"
    arg = arg.tr('"', "\"") if arg.is_a?(String)
    "<%= _(\"#{arg}\") %>"
  end

  private
  def generate_docs
    puts 'Rendering the interpolated TOML erb files...'
    interpolated_file_paths = [synths, fx, samples, lang].each_with_object([]) do |collection, paths|
      original_template = File.read(collection[:template_path])
      collection[:items].to_a.take(2).each do |key, item|
        # this really needs a more elegant way of reading arg/slide data from the object for
        # all types (synths/fx/samples)
        data_object = collection[:data_object] || item
        key = key.to_s[3..-1] if collection[:klass] == SonicPi::Synths::FXInfo
        interpolated_file_path = "#{collection[:interpolated_path]}/#{key}.toml.erb"
        template = ERB.new(original_template, nil, '-').result(binding)
        FileUtils.mkdir_p(collection[:interpolated_path]) unless File.exist?(collection[:interpolated_path])
        File.open(interpolated_file_path, 'w') do |f|
          f.write template
        end
        paths << interpolated_file_path
      end
    end
    generate_pot_file(interpolated_file_paths)
    generate_toml_files
  end

  def generate_pot_file(interpolated_file_paths)
    puts 'Extracting the translatable text into pot file...'
    FileUtils.mkdir_p(interpolated_template_path) unless File.exist?(interpolated_template_path)
    cwd = Dir.pwd
    Dir.chdir(generated_path)
    GetText::Tools::XGetText.run(
      *interpolated_file_paths,
      '-otest.pot'
    )
    Dir.chdir(cwd)
  end

  def generate_toml_files
    puts 'Rendering the final TOML output files...'
    [synths, fx, samples, lang].each do |collection|
      collection[:items].to_a.take(2).each do |key, item|
        key = key.to_s[3..-1] if collection[:klass] == SonicPi::Synths::FXInfo
        interpolated_file = File.read("#{collection[:interpolated_path]}/#{key}.toml.erb")
        output = ERB.new(interpolated_file, nil, '-').result(binding)
        File.open("#{collection[:output_path]}/#{key}.toml", 'w') do |f|
          f.write output
        end
      end
    end
  end

  def sync_to_documentation_app
    puts 'Syncing the TOML files to the documentation web app...'
    FileUtils.cp_r(doc_toml_path, documentation_app_priv_path, remove_destination: true)
  end

  def synths
    items = SonicPi::Synths::SynthInfo.get_all.select do |k, v|
      v.is_a?(SonicPi::Synths::SynthInfo) && v.user_facing?
    end
    {
      items: items,
      template_path: synth_and_fx_template_path,
      interpolated_path: synths_interpolated_path,
      output_path: synths_toml_path,
      klass: SonicPi::Synths::SynthInfo
    }
  end

  def fx
    items = SonicPi::Synths::SynthInfo.get_all.select do |k, v|
      v.is_a?(SonicPi::Synths::FXInfo) && v.user_facing? && !(k.to_s.include? 'replace_')
    end
    {
      items: items,
      template_path: synth_and_fx_template_path,
      interpolated_path: fx_interpolated_path,
      output_path: fx_toml_path,
      klass: SonicPi::Synths::FXInfo
    }
  end

  def samples
    {
      items: SonicPi::Synths::SynthInfo.grouped_samples,
      template_path: samples_template_path,
      interpolated_path: samples_interpolated_path,
      output_path: samples_toml_path,
      data_object: SonicPi::Synths::StereoPlayer.new
    }
  end

  def lang
    {
      items: @@docs,
      template_path: lang_template_path,
      interpolated_path: lang_interpolated_path,
      output_path: lang_toml_path
    }
  end
end

QtDocs.new.run
