# encoding: UTF-8
# frozen_string_literal: true

# This file contains data derived from the IANA Time Zone Database
# (https://www.iana.org/time-zones).

module TZInfo
  module Data
    module Definitions
      module Etc
        module UTC
          include Format2::TimezoneDefinition

          timezone 'Etc/UTC' do |tz|
            tz.offset :o0, 0, 0, 'UTC'

          end
        end
      end
    end
  end
end
