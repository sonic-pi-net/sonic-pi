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
    # _generate_lang_docs
    _generate_synth_docs
  end

  def t_(arg)
    "<%= _(\"#{arg}\") %>"
  end

  private
  def _generate_lang_docs
    _generate_docs(@@docs, lang_template_path, lang_toml_path)
  end

  def _generate_synth_docs
    collection = SonicPi::Synths::SynthInfo.get_all.select do |k, v|
      v.is_a?(SonicPi::Synths::SynthInfo) && v.user_facing?
    end
    _generate_docs(
      collection,
      synth_and_fx_template_path,
      synth_toml_path,
      SonicPi::Synths::SynthInfo
    )
  end

  def _generate_docs(collection, template_path, output_path, klass = nil)
    original_template = File.read(template_path)
    collection.to_a.take(2).map do |key, item|
      template = ERB.new(original_template).result(binding)
      interpolated_path = "#{interpolated_template_path}/#{key}.toml.erb"
      File.open(interpolated_path, 'w') do |f|
        f.write template
      end

      GetText::Tools::XGetText.run(
        interpolated_path,
        "-o ../../../../../../etc/doc/generated/toml/synths/test.pot"
      )
    end
  end
end

QtDocs.new.run
