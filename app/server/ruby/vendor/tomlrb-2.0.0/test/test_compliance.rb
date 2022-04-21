require 'minitest_helper'
require 'pathname'
require 'json'
require 'yaml'

EXPONENTIAL_NOTATIONS = %w[
  values/spec-float-4.yaml
  values/spec-float-5.yaml
  values/spec-float-6.yaml
  values/spec-float-9.yaml
]

UNDERSCORE_NUMBERS = %w[
  values/spec-float-8.yaml
]

RATIONAL_TIME = %w[
  values/spec-date-time-3.yaml
  values/spec-date-time-5.yaml
  values/spec-date-time-6.yaml
]

INT_KEY = %w[
  values/spec-key-value-pair-9.yaml
]

describe Tomlrb::Parser do
  tests_dir = File.join(__dir__, '../toml-spec-tests')
  Pathname.glob("#{tests_dir}/values/*.toml").each do |toml_path|
    toml_path = toml_path.expand_path
    local_path = toml_path.parent.basename/toml_path.basename

    it "parses #{local_path}" do
      actual = Tomlrb.load_file(toml_path.to_path)
      json_path = toml_path.sub_ext('.json')
      yaml_path = toml_path.sub_ext('.yaml')
      expected =
        if json_path.exist?
          load_json(json_path)
        elsif yaml_path.exist?
          load_yaml(yaml_path)
        end
      _(actual).must_equal expected
    end
  end

  Pathname.glob("#{tests_dir}/errors/*.toml").each do |toml_path|
    toml_path = toml_path.expand_path
    local_path = toml_path.parent.basename/toml_path.basename

    it "raises an error on parsing #{local_path}" do
      _{ Tomlrb.load_file(toml_path.to_path) }.must_raise Tomlrb::ParseError, RangeError
    end
  end
end

def load_json(path)
  json = JSON.load(path.open)
  json.each_with_object({}) {|(key, value), table|
    v = value['value']
    table[key] =
      case value['type']
      when 'integer'
        v.to_i
      when 'datetime-local'
        date, time = v.split(/[t ]/i)
        year, month, day = date.split('-')
        hour, min, sec = time.split(':')
        Tomlrb::LocalDateTime.new(year, month, day, hour, min, sec.to_f)
      when 'date'
        Tomlrb::LocalDate.new(*v.split('-'))
      when 'time'
        hour, min, sec = v.split(':')
        Tomlrb::LocalTime.new(hour, min, sec.to_f)
      end
  }
end

def load_yaml(path)
  data = YAML.load_file(path.to_path)
  local_path = path.parent.basename/path.basename
  if EXPONENTIAL_NOTATIONS.include? local_path.to_path
    data = data.each_with_object({}) {|(key, value), table|
      table[key] = value.to_f
    }
  end
  if UNDERSCORE_NUMBERS.include? local_path.to_path
    data = data.each_with_object({}) {|(key, value), table|
      table[key] = value.to_f
    }
  end
  if RATIONAL_TIME.include? local_path.to_path
    data = data.each_with_object({}) {|(key, value), table|
      sec_frac = value.to_f.to_s.split('.')[1]
      table[key] = Time.new(value.year, value.month, value.day, value.hour, value.min, "#{value.sec}.#{sec_frac}".to_f, value.utc_offset || value.zone)
    }
  end
  if INT_KEY.include? local_path.to_path
    data = data.each_with_object({}) {|(key, value), table|
      table[key.to_s] = value.each_with_object({}) {|(k, v), t| t[k.to_s] = v}
    }
  end

  data
end
