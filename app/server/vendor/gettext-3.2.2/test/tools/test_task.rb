# -*- coding: utf-8 -*-
#
# Copyright (C) 2013-2014  Kouhei Sutou <kou@clear-code.com>
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

require "gettext/tools/task"

class TestToolsTask < Test::Unit::TestCase
  setup :before => :append
  def setup_task
    @task = GetText::Tools::Task.new
  end

  setup
  def setup_application
    @application = Rake::Application.new
    @original_application = Rake.application
    Rake.application = @application
  end

  teardown
  def teardown
    Rake.application = @original_application
  end

  setup
  def setup_record_task_metadata
    @original_record_task_metadata = Rake::TaskManager.record_task_metadata
    Rake::TaskManager.record_task_metadata = true
  end

  teardown
  def teardown_record_task_metadata
    Rake::TaskManager.record_task_metadata = @original_record_task_metadata
  end

  class TestPackageName < self
    def test_default
      assert_nil(@task.package_name)
    end

    def test_accessor
      package_name = "great application"
      @task.package_name = package_name
      assert_equal(package_name, @task.package_name)
    end

    def test_spec
      spec = Gem::Specification.new
      spec.name = "great-application"
      @task.spec = spec
      assert_equal(spec.name, @task.package_name)
    end
  end

  class TestPackageVersion < self
    def test_default
      assert_nil(@task.package_version)
    end

    def test_accessor
      package_version = "1.0"
      @task.package_version = package_version
      assert_equal(package_version, @task.package_version)
    end

    def test_spec
      version = "1.0"
      spec = Gem::Specification.new
      spec.version = version
      @task.spec = spec
      assert_equal(version, @task.package_version)
    end
  end

  class TestDomain < self
    def test_default
      assert_nil(@task.domain)
    end

    def test_accessor
      domain = "hello"
      @task.domain = domain
      assert_equal(domain, @task.domain)
    end

    class TestSpec < self
      def setup
        @spec = Gem::Specification.new
        @spec.name = "hello"
      end

      def test_not_set
        @task.spec = @spec
        assert_equal(@spec.name, @task.domain)
      end

      def test_already_set
        domain = "#{@spec.name}-world"
        @task.domain = domain
        @task.spec = @spec
        assert_equal(domain, @task.domain)
      end
    end
  end

  class TestFiles < self
    def test_default
      assert_equal([], @task.files)
    end

    def test_accessor
      files = [
        "lib/hellor.rb",
        "lib/world.rb",
      ]
      @task.files = files
      assert_equal(files, @task.files)
    end

    class TestSpec < self
      def setup
        @spec = Gem::Specification.new
        @spec.files = [
          "lib/hellor.rb",
          "lib/world.rb",
        ]
      end

      def test_not_set
        @task.spec = @spec
        assert_equal(@spec.files, @task.files)
      end

      def test_already_set
        files = [
          "lib/hello/world.rb",
        ]
        @task.files = files
        @task.spec = @spec
        assert_equal(files + @spec.files, @task.files)
      end
    end

    class TestTask < self
      setup :before => :append
      def setup_domain
        @task.domain = "hello"
      end

      def test_empty
        @task.files = []
        error = assert_raise(GetText::Tools::Task::ValidationError) do
          @task.define
        end
        assert_equal({"files" => "must set one or more files"},
                     error.reasons)
      end

      class TestPOT < self
        def setup
          @path = @task.send(:create_path)
        end

        def test_prerequisites
          @task.files = [__FILE__]
          @task.define
          assert_equal([__FILE__],
                       Rake::Task[@path.pot_file.to_s].prerequisites)
        end
      end

      class TestPO < self
        def setup
          @locale = "ja"
          @task.locales = [@locale]
          @path = @task.send(:create_path, @locale)
        end

        def test_prerequisites
          @task.files = [__FILE__]
          @task.define
          assert_equal([@path.edit_po_file.to_s, @path.po_directory.to_s],
                       Rake::Task[@path.po_file.to_s].prerequisites)
        end
      end

      class TestMO < self
        def setup
          @locale = "ja"
          @task.locales = [@locale]
          @path = @task.send(:create_path, @locale)
        end

        def test_prerequisites
          @task.files = [__FILE__]
          @task.define
          assert_equal([@path.po_file.to_s, @path.mo_directory.to_s],
                       Rake::Task[@path.mo_file.to_s].prerequisites)
        end
      end
    end
  end

  class TestEnableDescription < self
    def test_default
      assert_true(@task.enable_description?)
    end

    def test_accessor
      @task.enable_description = false
      assert_false(@task.enable_description?)
    end

    class TestTask < self
      def setup
        @task.domain = "hello"
        @task.files = [__FILE__]
      end

      def test_true
        @task.enable_description = true
        @task.define
        assert_not_nil(task.comment)
      end

      def test_false
        @task.enable_description = false
        @task.define
        assert_nil(task.comment)
      end

      private
      def task
        Rake::Task["gettext:po:update"]
      end
    end
  end

  class TestEnablePO < self
    def test_default
      assert_true(@task.enable_po?)
    end

    def test_accessor
      @task.enable_po = false
      assert_false(@task.enable_po?)
    end

    class TestTask < self
      def setup
        @task.domain = "hello"
      end

      def test_true
        @task.enable_po = true
        error = assert_raise(GetText::Tools::Task::ValidationError) do
          @task.define
        end
        assert_equal({"files" => "must set one or more files"},
                     error.reasons)
      end

      def test_false
        @task.enable_po = false
        @task.define
        task_names = @application.tasks.collect(&:name)
        assert_equal([], task_names.grep(/\Agettext:po/))
      end
    end
  end
end
