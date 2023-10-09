# encoding: UTF-8
# frozen_string_literal: true

require_relative 'test_utils'

class TCCountryTimezone < Minitest::Test
  include TZInfo

  def test_identifier
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_equal('Europe/London', ct.identifier)
    assert(ct.identifier.frozen?)
  end

  def test_latitude
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_equal(Rational(2059, 40), ct.latitude)
  end

  def test_longitude
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_equal(Rational(-5, 16), ct.longitude)
  end

  def test_default_description
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_nil(ct.description)
  end

  def test_nil_description
    nil_frozen = nil.frozen?
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16), nil)
    assert_nil(ct.description)

    # nil is frozen by default from Ruby 2.2.0. Check that CountryTimezone.new
    # didn't freeze nil on earlier Ruby versions.
    assert_equal(nil_frozen, nil.frozen?)
  end

  def test_description
    ct = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time')
    assert_equal('Eastern Time', ct.description)
    assert(ct.description.frozen?)
  end

  def test_timezone
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_kind_of(TimezoneProxy, ct.timezone)
    assert_equal('Europe/London', ct.timezone.identifier)
  end

  def test_description_or_friendly_idenfier_no_description
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    assert_equal('London', ct.description_or_friendly_identifier)
  end

  def test_description_or_friendly_idenfier_description
    ct = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time')
    assert_equal('Eastern Time', ct.description_or_friendly_identifier)
  end

  def test_equality_1
    ct1 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16))
    ct2 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16))
    ct3 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16), 'Description')
    ct4 = CountryTimezone.new('Europe/LondonB', Rational(2059, 40), Rational(-5, 16))
    ct5 = CountryTimezone.new('Europe/London',  Rational(2060, 40), Rational(-5, 16))
    ct6 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-6, 16))

    assert_equal(true, ct1 == ct1)
    assert_equal(true, ct1 == ct2)
    assert_equal(false, ct1 == ct3)
    assert_equal(false, ct1 == ct4)
    assert_equal(false, ct1 == ct5)
    assert_equal(false, ct1 == ct6)
  end

  def test_equality_2
    ct1 = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time')
    ct2 = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time2')

    assert_equal(true, ct1 == ct1)
    assert_equal(false, ct1 == ct2)
  end

  def test_equality_non_country_timezone
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))

    assert_equal(false, ct == Object.new)
  end

  def test_eql_1
    ct1 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16))
    ct2 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16))
    ct3 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-5, 16), 'Description')
    ct4 = CountryTimezone.new('Europe/LondonB', Rational(2059, 40), Rational(-5, 16))
    ct5 = CountryTimezone.new('Europe/London',  Rational(2060, 40), Rational(-5, 16))
    ct6 = CountryTimezone.new('Europe/London',  Rational(2059, 40), Rational(-6, 16))

    assert_equal(true, ct1.eql?(ct1))
    assert_equal(true, ct1.eql?(ct2))
    assert_equal(false, ct1.eql?(ct3))
    assert_equal(false, ct1.eql?(ct4))
    assert_equal(false, ct1.eql?(ct5))
    assert_equal(false, ct1.eql?(ct6))
  end

  def test_eql_2
    ct1 = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time')
    ct2 = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time2')

    assert_equal(true, ct1.eql?(ct1))
    assert_equal(false, ct1.eql?(ct2))
  end

  def test_eql_non_country_timezone
    ct = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))

    assert_equal(false, ct.eql?(Object.new))
  end

  def test_hash_new
    ct1 = CountryTimezone.new('Europe/London', Rational(2059, 40), Rational(-5, 16))
    ct2 = CountryTimezone.new('America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time')

    assert_equal(['Europe/London', Rational(2059, 40), Rational(-5, 16), nil].hash, ct1.hash)
    assert_equal(['America/New_York', Rational(48857, 1200), Rational(-266423, 3600), 'Eastern Time'].hash, ct2.hash)
  end
end
