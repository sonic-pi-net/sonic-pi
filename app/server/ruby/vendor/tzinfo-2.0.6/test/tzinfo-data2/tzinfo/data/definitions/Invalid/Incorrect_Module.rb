# encoding: UTF-8
# frozen_string_literal: true

# This file was created manually. It purposefully contains module names that
# don't match the zone identifier to test load errors.

module TZInfo
  module Data
    module Definitions
      module InvalidX
        module Incorrect_ModuleX
          include Format2::TimezoneDefinition

          timezone 'Invalid/Incorrect_Module' do |tz|
            tz.offset :o0, 0, 0, 'UTC'
          end
        end
      end
    end
  end
end
