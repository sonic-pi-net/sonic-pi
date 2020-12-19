#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2020 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++
require 'tomlrb'
require 'shellwords'

module SonicPi
  module Config
    class ScsynthSettings

      attr_reader :toml_opts_hash, :scsynth_opts, :scsynth_opts_override

      def initialize(settings_path, opts={})
        if opts[:dummy]
          @toml_opts_hash = {}.freeze
        else
          @toml_opts_hash = Tomlrb.load_file(settings_path, symbolize_keys: true).freeze
        end

        key_conversion = {
          sound_card_name: "-H",
          sound_card_sample_rate: "-S",
          sound_card_buffer_size: "-Z",
          num_inputs: "-i",
          num_outputs: "-o",
          block_size: "-z",
          enable_inputs: "-I",
          enable_outputs: "-O",
          num_control_bus_channels: "-c",
          num_audio_bus_channels: "-a",
          num_sample_buffers: "-b",
          max_num_nodes: "-n",
          max_num_synthdefs: "-d",
          real_time_memory_size: "-m",
          num_wire_buffers: "-w",
          num_random_seeds: "-r",
          # scsynth_opts: "",
          # scsynth_opts_override: ""
        }

        opts = {}

        @toml_opts_hash.each do |k, v|
          v = case v
              when TrueClass
                1
              when FalseClass
                0
              when String
                v.strip
              else
                v
              end

          command_line_key = key_conversion[k.to_sym]
          val = v.to_s

          #raise "Unknown SuperCollider scsynth arg: #{k}. Expected one of #{key_conversion.keys.inspect}" unless command_line_key
          next unless command_line_key

          opts[command_line_key] = val
        end

        begin
          clobber_opts_a = Shellwords.split(@toml_opts_hash.fetch(:scsynth_opts_override, ""))
          @scsynth_opts_override = clobber_opts_a.each_slice(2).to_h
        rescue
          @scsynth_opts_override = {}
        end

        begin
          scsynth_opts_a = Shellwords.split(@toml_opts_hash.fetch(:scsynth_opts, ""))
          scsynth_opts = clobber_opts_a.each_slice(2).to_h
        rescue
          scsynth_opts = {}
        end

        @scsynth_opts = opts.merge(scsynth_opts)
      end
    end
  end
end
