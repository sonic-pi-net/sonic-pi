# -*- ruby -*-
#
# Rakefile for gettext
#
# This file maintains gettext.
#
# Use setup.rb or gem for installation.
# You don't need to use this file directly.
#
# Copyright(c) 2005-2009 Masao Mutoh
# Copyright(c) 2012-2013 Kouhei Sutou <kou@clear-code.com>
# Copyright(c) 2012 Haruka Yoshihara <yoshihara@clear-code.com>
# This program is licenced under the same licence as Ruby.

base_dir = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(base_dir, 'lib'))

require "tempfile"
require "rake"
require "rubygems"
require "yard"
require "gettext/version"
require "gettext/tools/task"
require "bundler/gem_helper"

class Bundler::GemHelper
  undef_method :version_tag
  def version_tag
    version
  end
end

helper = Bundler::GemHelper.new(base_dir)
helper.install
spec = helper.gemspec

task :default => :test

############################################################
# GetText tasks for developing
############################################################
po_parser_rb_path = "lib/gettext/po_parser.rb"
desc "Create #{po_parser_rb_path}"
task :po_parser => po_parser_rb_path

def fix_racc_output_indent(racc_output)
  racc_output.gsub(/^  (end\s*\# module GetText)$/, '\1')
end

po_parser_ry_path = "src/po_parser.ry"
file po_parser_rb_path => po_parser_ry_path do
  racc = File.join(Gem.bindir, "racc")
  tempfile = Tempfile.new("gettext-po-parser")
  ruby(racc, "-g", po_parser_ry_path, "-o", tempfile.path)

  File.open(po_parser_rb_path, "w") do |po_parser_rb|
    po_parser_rb.puts(<<-EOH)
# -*- coding: utf-8 -*-
#
# po_parser.rb - Generate a .mo
#
# Copyright (C) 2003-2009 Masao Mutoh <mutomasa at gmail.com>
# Copyright (C) 2012 Kouhei Sutou <kou@clear-code.com>
#
# You may redistribute it and/or modify it under the same
# license terms as Ruby or LGPL.

EOH

    po_parser_rb.puts(fix_racc_output_indent(tempfile.read))
  end
end

desc "Run all tests"
task :test => "test:prepare" do
  options = ARGV - Rake.application.top_level_tasks
  ruby "test/run-test.rb", *options
end

namespace :test do
  desc "Prepare test environment"
  task :prepare => [:po_parser, "test:gettext", "samples:gettext"]
end

xgettext_options = ["--add-comments=TRANSLATORS:"]
GetText::Tools::Task.define do |task|
  task.spec = spec
  task.xgettext_options.concat(xgettext_options)
end

Dir.glob("samples/*.rb") do |target|
  domain = File.basename(target, ".*")
  GetText::Tools::Task.define do |task|
    task.package_name = domain
    task.package_version = spec.version.to_s
    task.xgettext_options.concat(xgettext_options)
    task.domain = domain
    task.namespace_prefix = "samples:#{domain}"
    task.po_base_directory = "samples/po"
    task.mo_base_directory = "samples/locale"
    task.files = Dir.glob(target.gsub(/\..*\z/, ".*"))
  end
  task "samples:gettext" => "samples:#{domain}:gettext"
end
desc "Update *.mo for samples"
task "samples:gettext"

[
  ["main", Dir.glob("samples/cgi/{index.cgi,cookie.cgi}")],
  ["helloerb1", Dir.glob("samples/cgi/helloerb1.cgi")],
  ["helloerb2", Dir.glob("samples/cgi/helloerb2.cgi")],
  ["hellolib", Dir.glob("samples/cgi/hellolib.rb")],
].each do |domain, files|
  GetText::Tools::Task.define do |task|
    task.package_name = domain
    task.package_version = spec.version.to_s
    task.xgettext_options.concat(xgettext_options)
    task.domain = domain
    task.namespace_prefix = "samples:cgi:#{domain}"
    task.po_base_directory = "samples/cgi/po"
    task.mo_base_directory = "samples/cgi/locale"
    task.files = files
  end
  task "samples:cgi:gettext" => "samples:cgi:#{domain}:gettext"
end
desc "Updates *.mo for CGI samples"
task "samples:cgi:gettext"

task "samples:gettext" => "samples:cgi:gettext"

[
  "untranslated",
  "backslash",
  "non_ascii",
  "np_",
  "p_",
  "hello",
].each do |domain|
  GetText::Tools::Task.define do |task|
    task.package_name = domain
    task.package_version = spec.version.to_s
    task.xgettext_options.concat(xgettext_options)
    task.domain = domain
    task.namespace_prefix = "test:#{domain}"
    task.po_base_directory = "test/po"
    task.mo_base_directory = "test/locale"
    task.files = ["test/fixtures/#{domain}.rb"]
    task.locales = ["ja"]
  end
  task "test:gettext" => "test:#{domain}:gettext"
end

["_", "s_", "ns_"].each do |domain|
  GetText::Tools::Task.define do |task|
    task.package_name = domain
    task.package_version = spec.version.to_s
    task.xgettext_options.concat(xgettext_options)
    task.domain = domain
    task.namespace_prefix = "test:#{domain}"
    task.po_base_directory = "test/po"
    task.mo_base_directory = "test/locale"
    task.files = ["test/fixtures/#{domain}.rb"]
    task.files += Dir.glob("test/fixtures/#{domain}/*.rb")
    task.locales = ["ja"]
  end
  task "test:gettext" => "test:#{domain}:gettext"
end

po_only_domains = [
  "plural", "plural_error", "rubyparser", "test1", "test2", "test3"
]
po_only_domains.each do |domain|
  GetText::Tools::Task.define do |task|
    task.package_name = domain
    task.package_version = spec.version.to_s
    task.xgettext_options.concat(xgettext_options)
    task.domain = domain
    task.namespace_prefix = "test:#{domain}"
    task.po_base_directory = "test/po"
    task.mo_base_directory = "test/locale"
    task.enable_po = false
    task.locales = Dir.glob("test/po/*/#{domain}.po").collect do |po|
      File.basename(File.dirname(po))
    end
  end
  task "test:gettext" => "test:#{domain}:gettext"
end
desc "Update *.mo for test"
task "test:gettext"


task :package => [:gettext]

task :build => [:gettext]

YARD::Rake::YardocTask.new do |t|
end
