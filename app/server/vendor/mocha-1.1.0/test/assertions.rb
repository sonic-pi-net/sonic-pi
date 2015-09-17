module Assertions
  def assert_method_visiblity(object, method_name, visiblity)
    method_key = RUBY_VERSION < '1.9' ? method_name.to_s : method_name.to_sym
    assert object.send("#{visiblity}_methods", false).include?(method_key), "#{method_name} is not #{visiblity}"
  end
end