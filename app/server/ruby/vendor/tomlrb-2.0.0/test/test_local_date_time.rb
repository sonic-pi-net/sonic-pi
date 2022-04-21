require 'minitest_helper'
require 'tomlrb/local_date_time'
require 'tomlrb/local_date'
require 'tomlrb/local_time'

describe Tomlrb::LocalDateTime do
  subject { Tomlrb::LocalDateTime }

  it 'keeps fractional part' do
    dt = subject.new('1979', '05', '27', '00', '32', 0.999999)
    _(dt.to_s).must_equal '1979-05-27T00:32:00.999999'
  end

  describe '#to_time' do
    subject { Tomlrb::LocalDateTime.new('1979', '05', '27', '00', '32', 0.999999) }

    it 'returns Time object with zero offset' do
      _(subject.to_time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, '+00:00')
    end

    it 'can change time zone by offset' do
      time = subject.to_time('+09:00')
      _(time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, '+09:00')
    end

    if RUBY_VERSION >= '2.7.0'
      it 'can change time zone by name' do
        time = subject.to_time('UTC')
        _(time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, 'UTC')
      end
    end
  end
end

describe Tomlrb::LocalDate do
  subject { Tomlrb::LocalDate }

  it 'can initialize with year, month and day' do
    d = subject.new('1979', '05', '27')
    _(d.to_s).must_equal '1979-05-27'
  end

  describe '#to_time' do
    subject { Tomlrb::LocalDate.new('1979', '05', '27') }

    it 'has 00:00:00 as time part' do
      time = subject.to_time
      _(time).must_equal Time.new(1979, 5, 27, 0, 0, 0, '+00:00')
    end

    it 'can change time zone by offset' do
      time = subject.to_time('+09:00')
      _(time).must_equal Time.new(1979, 5, 27, 0, 0, 0, '+09:00')
    end

    if RUBY_VERSION >= '2.7.0'
      it 'can change time zone by name' do
        time = subject.to_time('UTC')
        _(time).must_equal Time.new(1979, 5, 27, 0, 0, 0, 'UTC')
      end
    end
  end
end

describe Tomlrb::LocalTime do
  subject { Tomlrb::LocalTime }

  it 'keeps fractional part' do
    t = subject.new('00', '32', 0.999999)
    _(t.to_s).must_equal '00:32:00.999999'
  end

  describe '#to_time' do
    subject { Tomlrb::LocalTime.new('00', '32', 0.999999) }

    it 'requires date part' do
      time = subject.to_time(1979, 5, 27)
      _(time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, '+00:00')
    end

    it 'can change time zone with offset' do
      time = subject.to_time(1979, 5, 27, '+09:00')
      _(time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, '+09:00')
    end

    if RUBY_VERSION >= '2.7.0'
      it 'can change time zone with name' do
        time = subject.to_time(1979, 5, 27, 'UTC')
        _(time).must_equal Time.new(1979, 5, 27, 0, 32, 0.999999, 'UTC')
      end
    end
  end
end
