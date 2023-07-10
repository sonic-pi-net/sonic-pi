source "https://rubygems.org"

gemspec

group :test do
  gem 'rake', ['>= 12.2.1', '< 14']
  gem 'minitest', '~> 5.0'
  gem 'simplecov', '~> 0.15.1', require: false

  # json is a dependency of simplecov. Version 2.3.0 is declared as compatible
  # with Ruby >= 1.9, but actually fails with a syntax error:
  # https://travis-ci.org/tzinfo/tzinfo/jobs/625092293#L605
  #
  # 2.5.1 is declared as compatible with Ruby >= 2.0, but actually fails to
  # compile with an undefined reference to `rb_funcallv` on Windows:
  # https://github.com/tzinfo/tzinfo/runs/1664656059#step:3:757
  #
  # 2.3.0 also fails to build the native extension with Rubinius:
  # https://travis-ci.org/tzinfo/tzinfo/jobs/625092305#L1310
  #
  # Limit to earlier compatible versions.
  if RUBY_VERSION < '2.0' || RUBY_ENGINE == 'rbx'
    gem 'json', '< 2.3.0'
  elsif RUBY_VERSION < '2.1' && RUBY_PLATFORM =~ /mingw/
    gem 'json', '< 2.5.0'
  end
end
