#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"
require_relative "../lib/sonicpi/promise"

module SonicPi
  module Threading
    class PromiseTester < Minitest::Test

      def test_get
        p = Promise.new
        t = Thread.new do
          assert_equal p.get, 3
        end

        p.deliver! 3
        t.join
      end

      def test_multi_get
        p = Promise.new
        t = Thread.new do
          assert_equal p.get, 3
        end

        t2 = Thread.new do
          assert_equal p.get, 3
        end
        Kernel.sleep 0.1
        p.deliver! 3
        t.join
        t2.join
      end

      def test_timeout
        p = Promise.new

        t = Thread.new do
          assert_raises PromiseTimeoutError do
            p.get(0.01)
          end
        end

        t.join

      end

      def test_multi_timeout
        p = Promise.new

        t = Thread.new do
          assert_raises PromiseTimeoutError do
            p.get(0.01)
          end
        end

        t2 = Thread.new do
          assert_raises PromiseTimeoutError do
            p.get(0.02)
          end
        end

        t.join
        t2.join
      end

      def test_multi_deliver_exception
        p = Promise.new
        p.deliver! 3

        assert_raises PromiseAlreadyDeliveredError do
          p.deliver! 4
        end
      end

      def test_blocking
        p = Promise.new
        t = Thread.new do
          p.get
        end

        Kernel.sleep 0.5
        assert_equal "sleep", t.status
        t.kill
      end
    end
  end
end
