# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCTZInfo < Minitest::Test
  include TZInfo

  def test_eager_load
    test_data_source = Minitest::Mock.new
    test_data_source.expect(:kind_of?, true, [DataSource])
    test_data_source.expect(:eager_load!, nil)

    orig_data_source = DataSource.get
    DataSource.set(test_data_source)
    begin
      assert_nil(TZInfo.eager_load!)
      test_data_source.verify
    ensure
      DataSource.set(orig_data_source)
    end
  end
end
