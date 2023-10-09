# encoding: UTF-8
# frozen_string_literal: true

TZINFO_TEST_DATA_FORMAT = 2
COVERAGE_TYPE = 'ruby_format_2'

require_relative 'test_utils'

TZInfo::DataSource.set(:ruby)

require_relative 'ts_all'
