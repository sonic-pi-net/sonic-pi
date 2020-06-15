#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, and
# distribution of modified versions of this work as long as this
# notice is included.
#++

require_relative "./setup_test"

module SonicPi
  class AllocatorTester < Minitest::Test
    def test_basic_allocation
      a = Allocator.new(5)
      assert_equal(1, a.allocate)
      assert_equal(2, a.allocate)
      assert_equal(3, a.allocate)
      assert_equal(4, a.allocate)
      assert_equal(0, a.allocate)
      assert_raises AllocationError do
        a.allocate
      end
    end

    def test_basic_deallocation
      a = Allocator.new(5)
      assert_equal(1, a.allocate)
      assert_equal(2, a.allocate)
      assert_equal(3, a.allocate)
      assert_equal(4, a.allocate)
      assert_equal(0, a.allocate)
      a.release!(1)
      assert_equal(1, a.allocate)
      a.release!(3)
      assert_equal(3, a.allocate)
      assert_raises AllocationError do
        a.allocate
      end
    end

    def test_allocation_count
      a = Allocator.new(5)
      last = nil
      3.times { last = a.allocate }
      assert_equal(3, a.num_allocations)
      a.release!(last)
      assert_equal(2, a.num_allocations)
    end

    def test_reset
      a = Allocator.new(5)
      last = nil
      3.times { last = a.allocate }
      assert_equal(3, a.num_allocations)
      a.reset!
      assert_equal(0, a.num_allocations)
    end
  end
end
