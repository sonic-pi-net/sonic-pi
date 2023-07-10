# encoding: UTF-8
# frozen_string_literal: true

# This file contains data derived from the IANA Time Zone Database
# (https://www.iana.org/time-zones).

module TZInfo
  module Data
    module Definitions
      module Etc
        module GMT__m__1
          include Format2::TimezoneDefinition

          timezone 'Etc/GMT-1' do |tz|
            tz.offset :o0, 3600, 0, '+01'

          end
        end
      end
    end
  end
end
