# encoding: UTF-8
# frozen_string_literal: true

# This file contains data derived from the IANA Time Zone Database
# (https://www.iana.org/time-zones).

module TZInfo
  module Data
    module Definitions
      module UTC
        include Format2::TimezoneDefinition

        linked_timezone 'UTC', 'Etc/UTC'
      end
    end
  end
end
