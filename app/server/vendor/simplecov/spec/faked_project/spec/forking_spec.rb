require "spec_helper"

describe "forking" do
  it do
    # TODO: The defined?(RUBY_ENGINE) check can be dropped for simplecov 1.0.0
    Process.waitpid(Kernel.fork {}) unless defined?(RUBY_ENGINE) && RUBY_ENGINE == "jruby"
  end
end
