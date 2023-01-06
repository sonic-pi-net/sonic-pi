#!/usr/bin/env ruby
# frozen_string_literal: true

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

require_relative '../core'
require_relative '../lib/sonicpi/lang/support/docsystem'
require_relative '../lib/sonicpi/synths/synthinfo'
require_relative '../lib/sonicpi/util'
require_relative '../lib/sonicpi/runtime'
require_relative '../lib/sonicpi/lang/core'
require_relative '../lib/sonicpi/lang/sound'
require_relative '../lib/sonicpi/lang/minecraftpi'
require_relative '../lib/sonicpi/lang/midi'
require_relative '../paths'
require 'active_support/inflector'
require 'erb'
require 'gettext'
require 'gettext/tools/xgettext'
require 'gettext/tools/msgmerge'
require 'gettext/tools/msgfmt'

class I18nTool
  include SonicPi::Lang::Support::DocSystem
  include SonicPi::Util
  include GetText

  POT_FILE_PATH = "#{SonicPi::Paths.lang_path}/sonic-pi-reference.pot"

  def initialize
    @pot_file_path = POT_FILE_PATH
  end

  def run(args)
      OptionParser.new do |opts|
      opts.banner = "Usage: i18n-tool-reference.rb [options]\nextracts and updates reference documentation via translatable toml files."
      opts.on('-x[POT_FILE_PATH]', '--extract[=POT_FILE_PATH]', 'creates .pot file from original documentation sources (similar to xgettext)') do |path|
        @pot_file_path = if path
                           File.expand_path(path)
                         else
                           POT_FILE_PATH
                         end
        interpolated_file_paths = interpolate_templates
        generate_pot_file(interpolated_file_paths)
      end
      opts.on('-u[POT_FILE_PATH]', '--update=[POT_FILE_PATH]', 'update translation files with new source strings (similar to msgmerge)') do |path|
        @pot_file_path = if path
                           File.expand_path(path)
                         else
                           POT_FILE_PATH
                         end
        update_translation_files
      end
      opts.on('-t', '--translate', 'translate English reference sources into all languages') do
        translate_reference_documents
      end
    end.parse!(args)
  end

  # TODO: when a function has no args, there is an orphaned comment in the TOML about the args.
  # Maybe instead of just 'displaying nothing' where the args would have been, there should be an empty args array?
  # Also, tests!
  def t_(arg)
    # Where documentation strings in the ruby source code contain values incompatible with the translation process,
    # (such as nested double quotes, or empty function argument lists, ([[]]),
    # These need to be ignored and replaced in any generated files for the translations.
    arg = arg.tr('\"', "'") if arg.is_a?(String)
    "<%= _(\"#{arg}\") %>" unless arg.nil? || arg == ''
  end

  # Replace any invalid filename characters with underscores
  def make_valid_filename(name)
    name.to_s.gsub(/[\/\\?%:*<>]/, '_')
  end

  # Reads the files containing the initial erb templates for each documentation category,
  # and copies their contents into new erb files, with any variables replaced (interpolated) by their actual values.
  # For example: <%= _(item.name) %> might become <%= _("Pretty Bell") %>
  # This allows gettext to handle marked strings in the interpolated files, as it cannot interpret ruby expressions,
  # Only plain ruby strings.
  def interpolate_templates
    [synths, fx, samples, lang].each_with_object([]) do |collection, paths|
      original_template = File.read(collection[:template_path])
      collection[:items].to_a.each do |key, item|
        # this really needs a more elegant way of reading arg/slide data from the object for
        # all types (synths/fx/samples)
        data_object = collection[:data_object] || item
        key = key.to_s[3..-1] if collection[:klass] == SonicPi::Synths::FXInfo
        interpolated_file_path = "#{collection[:interpolated_path]}/#{make_valid_filename(key)}.toml.erb"
        template = ERB.new(original_template, trim_mode: '-').result(binding)
        FileUtils.mkdir_p(collection[:interpolated_path]) unless File.exist?(collection[:interpolated_path])
        File.open(interpolated_file_path, 'w') do |f|
          f.write template
        end
        paths << interpolated_file_path
      end
    end
  end

  def generate_pot_file(interpolated_file_paths)
    puts 'Copying the translatable text into a pot file...'
    unless File.exist?(SonicPi::Paths.docs_interpolated_template_path)
      FileUtils.mkdir_p(SonicPi::Paths.docs_interpolated_template_path)
    end
    cwd = Dir.pwd
    Dir.chdir(File.dirname(@pot_file_path))
    GetText::Tools::XGetText.run(*interpolated_file_paths, "-o#{File.basename(@pot_file_path)}")
    Dir.chdir(cwd)
  end

  def update_translation_files
    puts 'Updating the translation files with new source strings...'
    raise "no .pot file, run 'i18n-tool-reference.rb --extract' first" unless File.exist?(@pot_file_path)
    (locales - ['en']).each do |l|
      po_file_path = "#{SonicPi::Paths.lang_path}/sonic-pi-reference-#{l}.po"
      puts po_file_path
      FileUtils.touch(po_file_path)
      cmdline = ['--update', '--no-obsolete-entries', po_file_path, @pot_file_path]
      GetText::Tools::MsgMerge.run(*cmdline)
    end
  end

  def translate_reference_documents
    puts 'Creating the final TOML output files...'
    old = Locale.current
    locales.each do |l|
      puts "Creating TOML files for locale #{l}..."
      create_and_bind_binary_translations(l) unless l == 'en'
      GetText.set_current_locale(l)
      output_final_toml_files(l)
    end
    GetText.set_current_locale(old)
  end

  def create_and_bind_binary_translations(locale)
    po_file_path = "#{SonicPi::Paths.lang_path}/sonic-pi-reference-#{locale}.po"
    mo_file_base_path = "#{SonicPi::Paths.docs_generated_path}/#{locale}/LC_MESSAGES"
    mo_file_path = "#{mo_file_base_path}/reference-#{locale}.mo"
    FileUtils.mkdir_p(mo_file_base_path) unless File.exist?(mo_file_base_path)
    GetText::Tools::MsgFmt.run(po_file_path, "-o#{mo_file_path}")
    bindtextdomain("reference-#{locale}", path: SonicPi::Paths.docs_generated_path)
  end

  def output_final_toml_files(locale)
    [synths, fx, samples, lang].each do |collection|
      collection[:items].to_a.each do |key, item|
        key = key.to_s[3..-1] if collection[:klass] == SonicPi::Synths::FXInfo
        filename = make_valid_filename(key)
        interpolated_file = File.read("#{collection[:interpolated_path]}/#{filename}.toml.erb")
        output = ERB.new(interpolated_file, trim_mode: '-').result(binding)
        output_path = collection[:output_path].call(locale)
        FileUtils.mkdir_p(output_path) unless File.exist?(output_path)
        File.open("#{output_path}/#{filename}.toml", 'w') do |f|
          f.write output
        end
      end
    end
  end

  def synths
    items = SonicPi::Synths::SynthInfo.get_all.select do |_k, v|
      v.is_a?(SonicPi::Synths::SynthInfo) && v.user_facing?
    end
    {
      items: items,
      template_path: SonicPi::Paths.docs_synth_and_fx_template_path,
      interpolated_path: SonicPi::Paths.docs_synths_interpolated_path,
      output_path: ->(lang) { SonicPi::Paths.docs_synths_toml_path(lang) },
      klass: SonicPi::Synths::SynthInfo
    }
  end

  def fx
    items = SonicPi::Synths::SynthInfo.get_all.select do |k, v|
      v.is_a?(SonicPi::Synths::FXInfo) && v.user_facing? && !(k.to_s.include? 'replace_')
    end
    {
      items: items,
      template_path: SonicPi::Paths.docs_synth_and_fx_template_path,
      interpolated_path: SonicPi::Paths.docs_fx_interpolated_path,
      output_path: ->(lang) { SonicPi::Paths.docs_fx_toml_path(lang) },
      klass: SonicPi::Synths::FXInfo
    }
  end

  def samples
    {
      items: SonicPi::Synths::SynthInfo.grouped_samples,
      template_path: SonicPi::Paths.docs_samples_template_path,
      interpolated_path: SonicPi::Paths.docs_samples_interpolated_path,
      output_path: ->(lang) { SonicPi::Paths.docs_samples_toml_path(lang) },
      data_object: SonicPi::Synths::StereoPlayer.new
    }
  end

  def lang
    {
      items: self.class.docs.reject { |_k, v| (v.keys.include?(:hide) && v[:hide] == true) || !v.keys.include?(:doc) },
      template_path: SonicPi::Paths.docs_lang_template_path,
      interpolated_path: SonicPi::Paths.docs_lang_interpolated_path,
      output_path: ->(lang) { SonicPi::Paths.docs_lang_toml_path(lang) }
    }
  end

  def locales
    @locales ||= begin
      (Dir["#{SonicPi::Paths.lang_path}/sonic-pi-tutorial-*.po"]
      .map { |p| File.basename(p).gsub(/sonic-pi-tutorial-(.*?).po/, '\1') }
      .sort << 'en')
    end
  end
end

I18nTool.new.run(ARGV)
