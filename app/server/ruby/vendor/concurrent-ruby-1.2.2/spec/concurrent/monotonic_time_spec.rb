require 'concurrent/utility/monotonic_time'

module Concurrent

  RSpec.describe :monotonic_time do
    context 'behavior' do

      it 'returns seconds as float' do
        expect(Concurrent.monotonic_time).to be_a(Float)
      end

      [:float_second, :float_millisecond, :float_microsecond].each do |unit|
        it "returns a Float when unit = #{unit.inspect}" do
          expect(Concurrent.monotonic_time(unit)).to be_a(Float)
        end
      end

      [:second, :millisecond, :microsecond, :nanosecond].each do |unit|
        it "returns an Integer when unit = #{unit.inspect}" do
          expect(Concurrent.monotonic_time(unit)).to be_an(Integer)
        end
      end

      it 'raises ArgumentError on unknown units' do
        expect {
          Concurrent.monotonic_time(:foo)
        }.to raise_error(ArgumentError)
      end

    end
  end
end
