# -*- coding: utf-8 -*-
#
# Copyright (C) 2012-2014  Kouhei Sutou <kou@clear-code.com>
#
# License: Ruby's or LGPL
#
# This library is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require "pathname"
require "rake"
require "rake/clean"

require "gettext/tools"

module GetText
  module Tools
    class Task
      class Error < StandardError
      end

      class ValidationError < Error
        attr_reader :reasons
        def initialize(reasons)
          @reasons = reasons
          lines = []
          lines << "invalid configurations:"
          @reasons.each do |variable, reason|
            lines << "#{variable}: #{reason}"
          end
          super(lines.join("\n"))
        end
      end

      include GetText
      include Rake::DSL

      class << self
        # Define gettext related Rake tasks. Normally, use this method
        # to define tasks because this method is a convenient API.
        #
        # See accessor APIs how to configure this task.
        #
        # See {#define} for what task is defined.
        #
        # @example Recommended usage
        #   require "gettext/tools/task"
        #   # Recommended usage
        #   GetText::Tools::Task.define do |task|
        #     task.spec = spec
        #     # ...
        #   end
        #   # Low level API
        #   task = GetText::Tools::Task.new
        #   task.spec = spec
        #   # ...
        #   task.define
        #
        # @yield [task] Gives the newely created task to the block.
        # @yieldparam [GetText::Tools::Task] task The task that should be
        #   configured.
        # @see #define
        # @return [void]
        def define
          task = new
          yield(task)
          task.define
        end
      end

      # @return [Gem::Specification, nil] Package information associated
      #   with the task.
      attr_reader :spec

      # @return [String, nil] Package name for messages.
      attr_accessor :package_name

      # @return [String, nil] Package version for messages.
      attr_accessor :package_version

      # It is a required parameter.
      #
      # @return [Array<String>] Supported locales. It is filled from
      #   {#po_base_directory} by default.
      attr_accessor :locales
      attr_accessor :po_base_directory
      # @return [String] Base directory that has generated MOs. MOs
      #   are generated into
      #   `#{mo_base_directory}/#{locale}/LC_MESSAGES/#{domain}.mo`.
      attr_accessor :mo_base_directory
      # It is a required parameter.
      #
      # @return [Array<String>] Files that have messages.
      attr_accessor :files
      # It is a required parameter.
      #
      # @return [String] Text domain
      attr_accessor :domain

      # It is useful when you have multiple domains. You can define tasks
      # for each domains by using different namespace prefix.
      #
      # It is `nil` by default. It means that tasks are defined at top
      # level.
      #
      # TODO: example
      #
      # @return [String] Namespace prefix for tasks defined by this class.
      attr_accessor :namespace_prefix

      # @return [Array<String>] Command line options for extracting messages
      #   from sources.
      # @see GetText::Tools::XGetText
      # @see `rxgettext --help`
      attr_accessor :xgettext_options

      # @return [Array<String>] Command line options for creating PO from POT.
      # @see GetText::Tools::MsgInit
      # @see `rmsginit --help`
      attr_accessor :msginit_options

      # @return [Array<String>] Command line options for merging PO with the
      #   latest POT.
      # @see GetText::Tools::MsgMerge
      # @see `rmsgmerge --help`
      attr_accessor :msgmerge_options

      # @return [Array<String>] Command line options for filtering PO.
      # @see GetText::Tools::MsgCat
      # @see `rmsgcat --help`
      # @since 3.1.3
      attr_accessor :msgcat_options

      # @return [Bool]
      # @see #enable_description? For details.
      attr_writer :enable_description

      # @return [Bool]
      # @see #enable_po? For details.
      attr_writer :enable_po

      # It is used to custom how to create POT file. The object must have
      # `call` method. The method must accept one argument. The argument
      #  is a `Pathname` object that represents POT file path.
      #
      # @example
      #
      #   GetText::Tools::Task.define do |task|
      #     task.pot_creator = lambda do |pot_file_path|
      #       pot_file_path.open("w") do |pot_file|
      #         pot_file << <<-POT
      #   msgid "Hello"
      #   msgstr ""
      #         POT
      #       end
      #     end
      #   end
      #
      #
      # @return [Proc]
      attr_accessor :pot_creator

      # @param [Gem::Specification, nil] spec Package information associated
      #   with the task. Some information are extracted from the spec.
      # @see #spec= What information are extracted from the spec.
      def initialize(spec=nil)
        initialize_variables
        self.spec = spec
        if spec
          yield(self) if block_given?
          warn("Use #{self.class.name}.define instead of #{self.class.name}.new(spec).")
          define
        end
      end

      # Sets package infromation by Gem::Specification. Here is a list
      # for information extracted from the spec:
      #
      #   * {#package_name}
      #   * {#package_version}
      #   * {#domain}
      #   * {#files}
      #
      # @param [Gem::Specification] spec package information for the
      #   i18n application.
      def spec=(spec)
        @spec = spec
        return if @spec.nil?

        @package_name = spec.name
        @package_version = spec.version.to_s
        @domain ||= spec.name
        @files += target_files
      end

      # Define tasks from configured parameters.
      #
      # TODO: List defined Rake tasks.
      def define
        ensure_variables
        validate

        define_file_tasks
        if namespace_prefix
          namespace_recursive namespace_prefix do
            define_named_tasks
          end
        else
          define_named_tasks
        end
      end

      # If it is true, each task has description. Otherwise, all tasks
      # doesn't have description.
      #
      # @return [Bool]
      # @since 3.0.1
      def enable_description?
        @enable_description
      end

      # If it is true, PO related tasks are defined. Otherwise, they
      # are not defined.
      #
      # This parameter is useful to manage PO written by hand.
      #
      # @return [Bool]
      # @since 3.0.1
      def enable_po?
        @enable_po
      end

      private
      def initialize_variables
        @spec = nil
        @package_name = nil
        @package_version = nil
        @locales = []
        @po_base_directory = "po"
        @mo_base_directory = "locale"
        @files = []
        @domain = nil
        @namespace_prefix = nil
        @xgettext_options = []
        @msginit_options = []
        @msgmerge_options = []
        @msgcat_options = []
        @enable_description = true
        @enable_po = true
        @pot_creator = nil
      end

      def ensure_variables
        @locales = detect_locales if @locales.empty?
      end

      def validate
        reasons = {}
        if @locales.empty?
          reasons["locales"] = "must set one or more locales"
        end
        if @enable_po and @files.empty?
          reasons["files"] = "must set one or more files"
        end
        if @domain.nil?
          reasons["domain"] = "must set domain"
        end
        raise ValidationError.new(reasons) unless reasons.empty?
      end

      def desc(*args)
        return unless @enable_description
        super
      end

      def define_file_tasks
        define_pot_file_task

        locales.each do |locale|
          define_edit_po_file_task(locale)
          define_po_file_task(locale)
          define_mo_file_task(locale)
        end
      end

      def define_pot_file_task
        return unless @enable_po

        path = create_path
        pot_dependencies = files.dup
        unless path.po_base_directory.exist?
          directory path.po_base_directory.to_s
          pot_dependencies << path.po_base_directory.to_s
        end
        file path.pot_file.to_s => pot_dependencies do
          create_pot(path.pot_file)
        end
      end

      def create_pot(pot_file_path)
        if @pot_creator
          @pot_creator.call(pot_file_path)
        else
          xgettext(pot_file_path)
        end
      end

      def xgettext(pot_file_path)
        command_line = [
          "--output", pot_file_path.to_s,
        ]
        if package_name
          command_line.concat(["--package-name", package_name])
        end
        if package_version
          command_line.concat(["--package-version", package_version])
        end
        command_line.concat(@xgettext_options)
        command_line.concat(files)
        XGetText.run(*command_line)
      end

      def define_edit_po_file_task(locale)
        return unless @enable_po

        path = create_path(locale)
        directory path.edit_po_directory.to_s
        dependencies = [
          path.pot_file.to_s,
          path.edit_po_directory.to_s,
        ]
        if path.po_file_is_updated?
          dependencies << internal_force_task_name
        end
        file path.edit_po_file.to_s => dependencies do
          if path.po_file_is_updated?
            rm_f(path.edit_po_file.to_s)
          end
          unless path.edit_po_file.exist?
            if path.po_file.exist?
              cp(path.po_file.to_s, path.edit_po_file.to_s)
            else
              MsgInit.run("--input", path.pot_file.to_s,
                          "--output", path.edit_po_file.to_s,
                          "--locale", path.locale,
                          "--no-translator",
                          *@msginit_options)
            end
          end

          edit_po_file_mtime = path.edit_po_file.mtime
          MsgMerge.run("--update",
                       "--sort-by-file",
                       "--no-wrap",
                       *@msgmerge_options,
                       path.edit_po_file.to_s,
                       path.pot_file.to_s)
          if path.po_file.exist? and path.po_file.mtime > edit_po_file_mtime
            MsgMerge.run("--output", path.edit_po_file.to_s,
                         "--sort-by-file",
                         "--no-wrap",
                         "--no-obsolete-entries",
                         *@msgmerge_options,
                         path.po_file.to_s,
                         path.edit_po_file.to_s)
          end
        end
      end

      def define_po_file_task(locale)
        return unless @enable_po

        path = create_path(locale)
        directory path.po_directory.to_s
        dependencies = [
          path.edit_po_file.to_s,
          path.po_directory.to_s,
        ]
        CLEAN << path.po_time_stamp_file.to_s
        file path.po_file.to_s => dependencies do
          MsgCat.run("--output", path.po_file.to_s,
                     "--sort-by-file",
                     "--no-location",
                     "--no-report-warning",
                     "--no-obsolete-entries",
                     "--remove-header-field=POT-Creation-Date",
                     *@msgcat_options,
                     path.edit_po_file.to_s)
          touch(path.po_time_stamp_file.to_s)
        end
      end

      def define_mo_file_task(locale)
        path = create_path(locale)
        directory path.mo_directory.to_s
        mo_dependencies = [
          path.po_file.to_s,
          path.mo_directory.to_s,
        ]
        file path.mo_file.to_s => mo_dependencies do
          MsgFmt.run(path.po_file.to_s, "--output", path.mo_file.to_s)
        end
      end

      def define_named_tasks
        namespace :gettext do
          define_internal_tasks
          if @enable_po
            define_pot_tasks
            define_po_tasks
          end

          define_mo_tasks
        end

        desc "Update *.mo"
        task :gettext => (current_scope + ["gettext", "mo", "update"]).join(":")
      end

      def define_internal_tasks
        namespace :internal do
          task :force
        end
      end

      def internal_force_task_name
        [namespace_prefix, "gettext", "internal", "force"].compact.join(":")
      end

      def define_pot_tasks
        namespace :pot do
          path = create_path
          desc "Create #{path.pot_file}"
          task :create => path.pot_file.to_s
        end
      end

      def define_po_tasks
        namespace :po do
          desc "Add a new locale"
          task :add, [:locale] do |_task, args|
            locale = args.locale || ENV["LOCALE"]
            if locale.nil?
              raise "specify locale name by " +
                "'rake #{_task.name}[${LOCALE}]' or " +
                "rake #{_task.name} LOCALE=${LOCALE}'"
            end
            define_edit_po_file_task(locale)
            define_po_file_task(locale)
            path = create_path(locale)
            Rake::Task[path.po_file].invoke
          end

          update_tasks = []
          @locales.each do |locale|
            namespace locale do
              path = create_path(locale)
              desc "Update #{path.po_file}"
              task :update => path.po_file.to_s
              update_tasks << (current_scope + ["update"]).join(":")
            end
          end

          desc "Update *.po"
          task :update => update_tasks
        end
      end

      def define_mo_tasks
        namespace :mo do
          update_tasks = []
          @locales.each do |locale|
            namespace locale do
              path = create_path(locale)
              desc "Update #{path.mo_file}"
              task :update => path.mo_file.to_s
              update_tasks << (current_scope + ["update"]).join(":")
            end
          end

          desc "Update *.mo"
          task :update => update_tasks
        end
      end

      def create_path(locale=nil)
        locale = locale.to_s if locale.is_a?(Symbol)
        Path.new(Pathname.new(@po_base_directory),
                 Pathname.new(@mo_base_directory),
                 @domain,
                 locale)
      end

      def target_files
        files = @spec.files.find_all do |file|
          /\A\.(?:rb|erb|glade)\z/i =~ File.extname(file)
        end
        files += @spec.executables.collect do |executable|
          "bin/#{executable}"
        end
        files
      end

      def detect_locales
        locales = []
        return locales unless File.exist?(po_base_directory)

        Dir.open(po_base_directory) do |dir|
          dir.each do |entry|
            next unless /\A[a-z]{2,3}(?:_[A-Z]{2})?\z/ =~ entry
            next unless File.directory?(File.join(dir.path, entry))
            locales << entry
          end
        end
        locales
      end

      def current_scope
        scope = Rake.application.current_scope
        if scope.is_a?(Array)
          scope
        else
          if scope.empty?
            []
          else
            [scope.path]
          end
        end
      end

      def namespace_recursive(namespace_spec, &block)
        first, rest = namespace_spec.split(/:/, 2)
        namespace first do
          if rest.nil?
            block.call
          else
            namespace_recursive(rest, &block)
          end
        end
      end

      class Path
        attr_reader :po_base_directory
        attr_reader :mo_base_directory
        attr_reader :domain
        attr_reader :locale
        def initialize(po_base_directory, mo_base_directory, domain, locale=nil)
          @po_base_directory = po_base_directory
          @mo_base_directory = mo_base_directory
          @domain = domain
          @locale = locale
        end

        def pot_file
          @po_base_directory + "#{@domain}.pot"
        end

        def po_directory
          @po_base_directory + @locale
        end

        def po_file
          po_directory + "#{@domain}.po"
        end

        def po_time_stamp_file
          po_directory + "#{@domain}.po.time_stamp"
        end

        def po_file_is_updated?
          return false unless po_file.exist?
          return true unless po_time_stamp_file.exist?
          po_file.mtime > po_time_stamp_file.mtime
        end

        def edit_po_directory
          po_directory
        end

        def edit_po_file
          edit_po_directory + "#{@domain}.edit.po"
        end

        def mo_directory
          @mo_base_directory + @locale + "LC_MESSAGES"
        end

        def mo_file
          mo_directory + "#{@domain}.mo"
        end
      end
    end
  end
end
