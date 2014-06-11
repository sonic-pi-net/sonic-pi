require 'thread_safe'
require 'thread'
require File.join(File.dirname(__FILE__), "test_helper")

Thread.abort_on_exception = true

class TestCache < Minitest::Test
  def setup
    @cache = ThreadSafe::Cache.new
  end

  def test_concurrency
    cache = @cache
    (1..100).map do |i|
      Thread.new do
        1000.times do |j|
          key = i*1000+j
          cache[key] = i
          cache[key]
          cache.delete(key)
        end
      end
    end.map(&:join)
  end

  def test_retrieval
    assert_size_change 1 do
      assert_equal nil, @cache[:a]
      assert_equal nil, @cache.get(:a)
      @cache[:a] = 1
      assert_equal 1,   @cache[:a]
      assert_equal 1,   @cache.get(:a)
    end
  end

  def test_put_if_absent
    with_or_without_default_proc do
      assert_size_change 1 do
        assert_equal nil, @cache.put_if_absent(:a, 1)
        assert_equal 1,   @cache.put_if_absent(:a, 1)
        assert_equal 1,   @cache.put_if_absent(:a, 2)
        assert_equal 1,   @cache[:a]
      end
    end
  end

  def test_compute_if_absent
    with_or_without_default_proc do
      assert_size_change 3 do
        assert_equal(1,   (@cache.compute_if_absent(:a) {1}))
        assert_equal(1,   (@cache.compute_if_absent(:a) {2}))
        assert_equal 1,    @cache[:a]
        @cache[:b] = nil
        assert_equal(nil, (@cache.compute_if_absent(:b) {1}))
        assert_equal(nil, (@cache.compute_if_absent(:c) {}))
        assert_equal nil,  @cache[:c]
        assert_equal true, @cache.key?(:c)
      end
    end
  end

  def test_compute_if_absent_with_return
    with_or_without_default_proc { assert_handles_return_lambda(:compute_if_absent, :a) }
  end

  def test_compute_if_absent_exception
    with_or_without_default_proc { assert_handles_exception(:compute_if_absent, :a) }
  end

  def test_compute_if_absent_atomicity
    late_compute_threads_count       = 10
    late_put_if_absent_threads_count = 10
    getter_threads_count             = 5
    compute_started = ThreadSafe::Test::Latch.new(1)
    compute_proceed = ThreadSafe::Test::Latch.new(late_compute_threads_count + late_put_if_absent_threads_count + getter_threads_count)
    block_until_compute_started = lambda do |name|
      if (v = @cache[:a]) != nil
        assert_equal nil, v
      end
      compute_proceed.release
      compute_started.await
    end

    assert_size_change 1 do
      late_compute_threads = Array.new(late_compute_threads_count) do
        Thread.new do
          block_until_compute_started.call('compute_if_absent')
          assert_equal(1, (@cache.compute_if_absent(:a) { flunk }))
        end
      end

      late_put_if_absent_threads = Array.new(late_put_if_absent_threads_count) do
        Thread.new do
          block_until_compute_started.call('put_if_absent')
          assert_equal(1, @cache.put_if_absent(:a, 2))
        end
      end

      getter_threads = Array.new(getter_threads_count) do
        Thread.new do
          block_until_compute_started.call('getter')
          Thread.pass while @cache[:a].nil?
          assert_equal 1, @cache[:a]
        end
      end

      Thread.new do
        @cache.compute_if_absent(:a) do
          compute_started.release
          compute_proceed.await
          sleep(0.2)
          1
        end
      end.join
      (late_compute_threads + late_put_if_absent_threads + getter_threads).each(&:join)
    end
  end

  def test_compute_if_present
    with_or_without_default_proc do
      assert_no_size_change do
        assert_equal(nil,   @cache.compute_if_present(:a) {})
        assert_equal(nil,   @cache.compute_if_present(:a) {1})
        assert_equal(nil,   @cache.compute_if_present(:a) {flunk})
        assert_equal false, @cache.key?(:a)
      end

      @cache[:a] = 1
      assert_no_size_change do
        assert_equal(1,     @cache.compute_if_present(:a) {1})
        assert_equal(1,     @cache[:a])
        assert_equal(2,     @cache.compute_if_present(:a) {2})
        assert_equal(2,     @cache[:a])
        assert_equal(false, @cache.compute_if_present(:a) {false})
        assert_equal(false, @cache[:a])

        @cache[:a] = 1
        yielded    = false
        @cache.compute_if_present(:a) do |old_value|
          yielded = true
          assert_equal 1, old_value
          2
        end
        assert yielded
      end

      assert_size_change -1 do
        assert_equal(nil,   @cache.compute_if_present(:a) {})
        assert_equal(false, @cache.key?(:a))
        assert_equal(nil,   @cache.compute_if_present(:a) {1})
        assert_equal(false, @cache.key?(:a))
      end
    end
  end

  def test_compute_if_present_with_return
    with_or_without_default_proc do
      @cache[:a] = 1
      assert_handles_return_lambda(:compute_if_present, :a)
    end
  end

  def test_compute_if_present_exception
    with_or_without_default_proc do
      @cache[:a] = 1
      assert_handles_exception(:compute_if_present, :a)
    end
  end

  def test_compute
    with_or_without_default_proc do
      assert_no_size_change do
        assert_compute(:a, nil, nil) {}
      end

      assert_size_change 1 do
        assert_compute(:a, nil, 1)   {1}
        assert_compute(:a, 1,   2)   {2}
        assert_compute(:a, 2, false) {false}
        assert_equal false, @cache[:a]
      end

      assert_size_change -1 do
        assert_compute(:a, false, nil) {}
      end
    end
  end

  def test_compute_with_return
    with_or_without_default_proc do
      assert_handles_return_lambda(:compute, :a)
      @cache[:a] = 1
      assert_handles_return_lambda(:compute, :a)
    end
  end

  def test_compute_exception
    with_or_without_default_proc do
      assert_handles_exception(:compute, :a)
      @cache[:a] = 1
      assert_handles_exception(:compute, :a)
    end
  end

  def test_merge_pair
    with_or_without_default_proc do
      assert_size_change 1 do
        assert_equal(nil,  @cache.merge_pair(:a, nil) {flunk})
        assert_equal true, @cache.key?(:a)
        assert_equal nil,  @cache[:a]
      end

      assert_no_size_change do
        assert_merge_pair(:a, nil, nil,   false) {false}
        assert_merge_pair(:a, nil, false, 1)     {1}
        assert_merge_pair(:a, nil, 1,     2)     {2}
      end

      assert_size_change -1 do
        assert_merge_pair(:a, nil, 2, nil) {}
        assert_equal false, @cache.key?(:a)
      end
    end
  end

  def test_merge_pair_with_return
    with_or_without_default_proc do
      @cache[:a] = 1
      assert_handles_return_lambda(:merge_pair, :a, 2)
    end
  end

  def test_merge_pair_exception
    with_or_without_default_proc do
      @cache[:a] = 1
      assert_handles_exception(:merge_pair, :a, 2)
    end
  end

  def test_updates_dont_block_reads
    getters_count = 20
    key_klass     = ThreadSafe::Test::HashCollisionKey
    keys          = [key_klass.new(1, 100), key_klass.new(2, 100), key_klass.new(3, 100)] # hash colliding keys
    inserted_keys = []

    keys.each do |key, i|
      compute_started  = ThreadSafe::Test::Latch.new(1)
      compute_finished = ThreadSafe::Test::Latch.new(1)
      getters_started  = ThreadSafe::Test::Latch.new(getters_count)
      getters_finished = ThreadSafe::Test::Latch.new(getters_count)

      computer_thread = Thread.new do
        getters_started.await
        @cache.compute_if_absent(key) do
          compute_started.release
          getters_finished.await
          1
        end
        compute_finished.release
      end

      getter_threads = (1..getters_count).map do
        Thread.new do
          getters_started.release
          inserted_keys.each do |inserted_key|
            assert_equal true, @cache.key?(inserted_key)
            assert_equal 1,    @cache[inserted_key]
          end
          assert_equal false, @cache.key?(key)
          compute_started.await
          inserted_keys.each do |inserted_key|
            assert_equal true, @cache.key?(inserted_key)
            assert_equal 1,    @cache[inserted_key]
          end
          assert_equal false, @cache.key?(key)
          assert_equal nil,   @cache[key]
          getters_finished.release
          compute_finished.await
          assert_equal true,  @cache.key?(key)
          assert_equal 1,     @cache[key]
        end
      end

      (getter_threads << computer_thread).map {|t| assert(t.join(2))} # asserting no deadlocks
      inserted_keys << key
    end
  end

  def test_collision_resistance
    assert_collision_resistance((0..1000).map {|i| ThreadSafe::Test::HashCollisionKey(i, 1)})
  end

  def test_collision_resistance_with_arrays
    special_array_class = Class.new(Array) do
      def key # assert_collision_resistance expects to be able to call .key to get the "real" key
        first.key
      end
    end
    # Test collision resistance with a keys that say they responds_to <=>, but then raise exceptions
    # when actually called (ie: an Array filled with non-comparable keys).
    # See https://github.com/headius/thread_safe/issues/19 for more info.
    assert_collision_resistance((0..100).map do |i|
      special_array_class.new([ThreadSafe::Test::HashCollisionKeyNonComparable.new(i, 1)])
    end)
  end

  def test_replace_pair
    with_or_without_default_proc do
      assert_no_size_change do
        assert_equal false, @cache.replace_pair(:a, 1, 2)
        assert_equal false, @cache.replace_pair(:a, nil, nil)
        assert_equal false, @cache.key?(:a)
      end

      @cache[:a] = 1
      assert_no_size_change do
        assert_equal true,  @cache.replace_pair(:a, 1, 2)
        assert_equal false, @cache.replace_pair(:a, 1, 2)
        assert_equal 2,     @cache[:a]
        assert_equal true,  @cache.replace_pair(:a, 2, 2)
        assert_equal 2,     @cache[:a]
        assert_equal true,  @cache.replace_pair(:a, 2, nil)
        assert_equal false, @cache.replace_pair(:a, 2, nil)
        assert_equal nil,   @cache[:a]
        assert_equal true,  @cache.key?(:a)
        assert_equal true,  @cache.replace_pair(:a, nil, nil)
        assert_equal true,  @cache.key?(:a)
        assert_equal true,  @cache.replace_pair(:a, nil, 1)
        assert_equal 1,     @cache[:a]
      end
    end
  end

  def test_replace_if_exists
    with_or_without_default_proc do
      assert_no_size_change do
        assert_equal nil,   @cache.replace_if_exists(:a, 1)
        assert_equal false, @cache.key?(:a)
      end

      @cache[:a] = 1
      assert_no_size_change do
        assert_equal 1,     @cache.replace_if_exists(:a, 2)
        assert_equal 2,     @cache[:a]
        assert_equal 2,     @cache.replace_if_exists(:a, nil)
        assert_equal nil,   @cache[:a]
        assert_equal true,  @cache.key?(:a)
        assert_equal nil,   @cache.replace_if_exists(:a, 1)
        assert_equal 1,     @cache[:a]
      end
    end
  end

  def test_get_and_set
    with_or_without_default_proc do
      assert_size_change 1 do
        assert_equal nil,   @cache.get_and_set(:a, 1)
        assert_equal true,  @cache.key?(:a)
        assert_equal 1,     @cache[:a]
        assert_equal 1,     @cache.get_and_set(:a, 2)
        assert_equal 2,     @cache.get_and_set(:a, nil)
        assert_equal nil,   @cache[:a]
        assert_equal true,  @cache.key?(:a)
        assert_equal nil,   @cache.get_and_set(:a, 1)
        assert_equal 1,     @cache[:a]
      end
    end
  end

  def test_key
    with_or_without_default_proc do
      assert_equal nil, @cache.key(1)
      @cache[:a] = 1
      assert_equal :a,  @cache.key(1)
      assert_equal nil,  @cache.key(0)
      assert_equal :a,  @cache.index(1) if RUBY_VERSION =~ /1\.8/
    end
  end

  def test_key?
    with_or_without_default_proc do
      assert_equal false, @cache.key?(:a)
      @cache[:a] = 1
      assert_equal true,  @cache.key?(:a)
    end
  end

  def test_value?
    with_or_without_default_proc do
      assert_equal false, @cache.value?(1)
      @cache[:a] = 1
      assert_equal true,  @cache.value?(1)
    end
  end

  def test_delete
    with_or_without_default_proc do |default_proc_set|
      assert_no_size_change do
        assert_equal nil,   @cache.delete(:a)
      end
      @cache[:a] = 1
      assert_size_change -1 do
        assert_equal 1,     @cache.delete(:a)
      end
      assert_no_size_change do
        assert_equal nil, @cache[:a] unless default_proc_set

        assert_equal false, @cache.key?(:a)
        assert_equal nil,   @cache.delete(:a)
      end
    end
  end

  def test_delete_pair
    with_or_without_default_proc do
      assert_no_size_change do
        assert_equal false, @cache.delete_pair(:a, 2)
        assert_equal false, @cache.delete_pair(:a, nil)
      end
      @cache[:a] = 1
      assert_no_size_change do
        assert_equal false, @cache.delete_pair(:a, 2)
      end
      assert_size_change -1 do
        assert_equal 1,     @cache[:a]
        assert_equal true,  @cache.delete_pair(:a, 1)
        assert_equal false, @cache.delete_pair(:a, 1)
        assert_equal false, @cache.key?(:a)
      end
    end
  end

  def test_default_proc
    @cache = cache_with_default_proc(1)
    assert_no_size_change do
      assert_equal false, @cache.key?(:a)
    end
    assert_size_change 1 do
      assert_equal 1,     @cache[:a]
      assert_equal true,  @cache.key?(:a)
    end
  end

  def test_falsy_default_proc
    @cache = cache_with_default_proc(nil)
    assert_no_size_change do
      assert_equal false, @cache.key?(:a)
    end
    assert_size_change 1 do
      assert_equal nil,   @cache[:a]
      assert_equal true,  @cache.key?(:a)
    end
  end

  def test_fetch
    with_or_without_default_proc do |default_proc_set|
      assert_no_size_change do
        assert_equal 1,      @cache.fetch(:a, 1)
        assert_equal(1,     (@cache.fetch(:a) {1}))
        assert_equal false,  @cache.key?(:a)

        assert_equal nil, @cache[:a] unless default_proc_set
      end

      @cache[:a] = 1
      assert_no_size_change do
        assert_equal(1, (@cache.fetch(:a) {flunk}))
      end

      assert_raises(ThreadSafe::Cache::KEY_ERROR) do
        @cache.fetch(:b)
      end

      assert_no_size_change do
        assert_equal 1,     (@cache.fetch(:b, :c) {1}) # assert block supersedes default value argument
        assert_equal false,  @cache.key?(:b)
      end
    end
  end

  def test_falsy_fetch
    with_or_without_default_proc do
      assert_equal false, @cache.key?(:a)

      assert_no_size_change do
        assert_equal(nil,    @cache.fetch(:a, nil))
        assert_equal(false,  @cache.fetch(:a, false))
        assert_equal(nil,   (@cache.fetch(:a) {}))
        assert_equal(false, (@cache.fetch(:a) {false}))
      end

      @cache[:a] = nil
      assert_no_size_change do
        assert_equal true, @cache.key?(:a)
        assert_equal(nil, (@cache.fetch(:a) {flunk}))
      end
    end
  end

  def test_fetch_with_return
    with_or_without_default_proc do
      r = lambda do
        @cache.fetch(:a) { return 10 }
      end.call

      assert_no_size_change do
        assert_equal 10,    r
        assert_equal false, @cache.key?(:a)
      end
    end
  end

  def test_fetch_or_store
    with_or_without_default_proc do |default_proc_set|
      assert_size_change 1 do
        assert_equal 1, @cache.fetch_or_store(:a, 1)
        assert_equal 1, @cache[:a]
      end

      @cache.delete(:a)

      assert_size_change 1 do
        assert_equal 1, (@cache.fetch_or_store(:a) {1})
        assert_equal 1,  @cache[:a]
      end

      assert_no_size_change do
        assert_equal(1, (@cache.fetch_or_store(:a) {flunk}))
      end

      assert_raises(ThreadSafe::Cache::KEY_ERROR) do
        @cache.fetch_or_store(:b)
      end

      assert_size_change 1 do
        assert_equal 1, (@cache.fetch_or_store(:b, :c) {1}) # assert block supersedes default value argument
        assert_equal 1,  @cache[:b]
      end
    end
  end

  def test_falsy_fetch_or_store
    with_or_without_default_proc do
      assert_equal false, @cache.key?(:a)

      assert_size_change 1 do
        assert_equal(nil,  @cache.fetch_or_store(:a, nil))
        assert_equal nil,  @cache[:a]
        assert_equal true, @cache.key?(:a)
      end
      @cache.delete(:a)

      assert_size_change 1 do
        assert_equal(false, @cache.fetch_or_store(:a, false))
        assert_equal false, @cache[:a]
        assert_equal true,  @cache.key?(:a)
      end
      @cache.delete(:a)

      assert_size_change 1 do
        assert_equal(nil, (@cache.fetch_or_store(:a) {}))
        assert_equal nil,  @cache[:a]
        assert_equal true, @cache.key?(:a)
      end
      @cache.delete(:a)

      assert_size_change 1 do
        assert_equal(false, (@cache.fetch_or_store(:a) {false}))
        assert_equal false,  @cache[:a]
        assert_equal true,   @cache.key?(:a)
      end

      @cache[:a] = nil
      assert_no_size_change do
        assert_equal(nil, (@cache.fetch_or_store(:a) {flunk}))
      end
    end
  end

  def test_fetch_or_store_with_return
    with_or_without_default_proc do
      r = lambda do
        @cache.fetch_or_store(:a) { return 10 }
      end.call

      assert_no_size_change do
        assert_equal 10,    r
        assert_equal false, @cache.key?(:a)
      end
    end
  end

  def test_clear
    @cache[:a] = 1
    assert_size_change -1 do
      assert_equal @cache, @cache.clear
      assert_equal false,  @cache.key?(:a)
      assert_equal nil,    @cache[:a]
    end
  end

  def test_each_pair
    @cache.each_pair {|k, v| flunk}
    assert_equal(@cache, (@cache.each_pair {}))
    @cache[:a] = 1

    h = {}
    @cache.each_pair {|k, v| h[k] = v}
    assert_equal({:a => 1}, h)

    @cache[:b] = 2
    h = {}
    @cache.each_pair {|k, v| h[k] = v}
    assert_equal({:a => 1, :b => 2}, h)
  end

  def test_each_pair_iterator
    @cache[:a] = 1
    @cache[:b] = 2
    i = 0
    r = @cache.each_pair do |k, v|
      if i == 0
        i += 1
        next
        flunk
      elsif i == 1
        break :breaked
      end
    end

    assert_equal :breaked, r
  end

  def test_each_pair_allows_modification
    @cache[:a] = 1
    @cache[:b] = 1
    @cache[:c] = 1

    assert_size_change 1 do
      @cache.each_pair do |k, v|
        @cache[:z] = 1
      end
    end
  end

  def test_keys
    assert_equal [], @cache.keys

    @cache[1] = 1
    assert_equal [1], @cache.keys

    @cache[2] = 2
    assert_equal [1, 2], @cache.keys.sort
  end

  def test_values
    assert_equal [], @cache.values

    @cache[1] = 1
    assert_equal [1], @cache.values

    @cache[2] = 2
    assert_equal [1, 2], @cache.values.sort
  end

  def test_each_key
    assert_equal(@cache, (@cache.each_key {flunk}))

    @cache[1] = 1
    arr = []
    @cache.each_key {|k| arr << k}
    assert_equal [1], arr

    @cache[2] = 2
    arr = []
    @cache.each_key {|k| arr << k}
    assert_equal [1, 2], arr.sort
  end

  def test_each_value
    assert_equal(@cache, (@cache.each_value {flunk}))

    @cache[1] = 1
    arr = []
    @cache.each_value {|k| arr << k}
    assert_equal [1], arr

    @cache[2] = 2
    arr = []
    @cache.each_value {|k| arr << k}
    assert_equal [1, 2], arr.sort
  end

  def test_empty
    assert_equal true,  @cache.empty?
    @cache[:a] = 1
    assert_equal false, @cache.empty?
  end

  def test_options_validation
    assert_valid_options(nil)
    assert_valid_options({})
    assert_valid_options(:foo => :bar)
  end

  def test_initial_capacity_options_validation
    assert_valid_option(:initial_capacity, nil)
    assert_valid_option(:initial_capacity, 1)
    assert_invalid_option(:initial_capacity, '')
    assert_invalid_option(:initial_capacity, 1.0)
    assert_invalid_option(:initial_capacity, -1)
  end

  def test_load_factor_options_validation
    assert_valid_option(:load_factor, nil)
    assert_valid_option(:load_factor, 0.01)
    assert_valid_option(:load_factor, 0.75)
    assert_valid_option(:load_factor, 1)
    assert_invalid_option(:load_factor, '')
    assert_invalid_option(:load_factor, 0)
    assert_invalid_option(:load_factor, 1.1)
    assert_invalid_option(:load_factor, 2)
    assert_invalid_option(:load_factor, -1)
  end

  def test_size
    assert_equal 0, @cache.size
    @cache[:a] = 1
    assert_equal 1, @cache.size
    @cache[:b] = 1
    assert_equal 2, @cache.size
    @cache.delete(:a)
    assert_equal 1, @cache.size
    @cache.delete(:b)
    assert_equal 0, @cache.size
  end

  def test_get_or_default
    with_or_without_default_proc do
      assert_equal 1,     @cache.get_or_default(:a, 1)
      assert_equal nil,   @cache.get_or_default(:a, nil)
      assert_equal false, @cache.get_or_default(:a, false)
      assert_equal false, @cache.key?(:a)

      @cache[:a] = 1
      assert_equal 1, @cache.get_or_default(:a, 2)
    end
  end

  def test_dup_clone
    [:dup, :clone].each do |meth|
      cache = cache_with_default_proc(:default_value)
      cache[:a] = 1
      dupped = cache.send(meth)
      assert_equal 1, dupped[:a]
      assert_equal 1, dupped.size
      assert_size_change 1, cache do
        assert_no_size_change dupped do
          cache[:b] = 1
        end
      end
      assert_equal false, dupped.key?(:b)
      assert_no_size_change cache do
        assert_size_change -1, dupped do
          dupped.delete(:a)
        end
      end
      assert_equal false, dupped.key?(:a)
      assert_equal true,  cache.key?(:a)
      # test default proc
      assert_size_change 1, cache do
        assert_no_size_change dupped do
          assert_equal :default_value, cache[:c]
          assert_equal false,          dupped.key?(:c)
        end
      end
      assert_no_size_change cache do
        assert_size_change 1, dupped do
          assert_equal :default_value, dupped[:d]
          assert_equal false,          cache.key?(:d)
        end
      end
    end
  end

  def test_is_unfreezable
    assert_raises(NoMethodError) { @cache.freeze }
  end

  def test_marshal_dump_load
    new_cache = Marshal.load(Marshal.dump(@cache))
    assert_instance_of ThreadSafe::Cache, new_cache
    assert_equal 0, new_cache.size
    @cache[:a] = 1
    new_cache = Marshal.load(Marshal.dump(@cache))
    assert_equal 1, @cache[:a]
    assert_equal 1, new_cache.size
  end

  def test_marshal_dump_doesnt_work_with_default_proc
    assert_raises(TypeError) do
      Marshal.dump(ThreadSafe::Cache.new {})
    end
  end

  private
  def with_or_without_default_proc
    yield false
    @cache = ThreadSafe::Cache.new {|h, k| h[k] = :default_value}
    yield true
  end

  def cache_with_default_proc(default_value = 1)
    ThreadSafe::Cache.new {|cache, k| cache[k] = default_value}
  end

  def assert_valid_option(option_name, value)
    assert_valid_options(option_name => value)
  end

  def assert_valid_options(options)
    c = ThreadSafe::Cache.new(options)
    assert_instance_of ThreadSafe::Cache, c
  end

  def assert_invalid_option(option_name, value)
    assert_invalid_options(option_name => value)
  end

  def assert_invalid_options(options)
    assert_raises(ArgumentError) { ThreadSafe::Cache.new(options) }
  end

  def assert_size_change(change, cache = @cache)
    start = cache.size
    yield
    assert_equal change, cache.size - start
  end

  def assert_no_size_change(cache = @cache, &block)
    assert_size_change(0, cache, &block)
  end

  def assert_handles_return_lambda(method, key, *args)
    before_had_key   = @cache.key?(key)
    before_had_value = before_had_key ? @cache[key] : nil

    returning_lambda = lambda do
      @cache.send(method, key, *args) { return :direct_return }
    end

    assert_no_size_change do
      assert_equal(:direct_return,   returning_lambda.call)
      assert_equal before_had_key,   @cache.key?(key)
      assert_equal before_had_value, @cache[key] if before_had_value
    end
  end

  class TestException < Exception; end
  def assert_handles_exception(method, key, *args)
    before_had_key   = @cache.key?(key)
    before_had_value = before_had_key ? @cache[key] : nil

    assert_no_size_change do
      assert_raises(TestException) do
        @cache.send(method, key, *args) { raise TestException, '' }
      end
      assert_equal before_had_key,   @cache.key?(key)
      assert_equal before_had_value, @cache[key] if before_had_value
    end
  end

  def assert_compute(key, expected_old_value, expected_result)
    result = @cache.compute(:a) do |old_value|
      assert_equal expected_old_value, old_value
      yield
    end
    assert_equal expected_result, result
  end

  def assert_merge_pair(key, value, expected_old_value, expected_result)
    result = @cache.merge_pair(key, value) do |old_value|
      assert_equal expected_old_value, old_value
      yield
    end
    assert_equal expected_result, result
  end

  def assert_collision_resistance(keys)
    keys.each {|k| @cache[k] = k.key}
    10.times do |i|
      size = keys.size
      while i < size
        k = keys[i]
        assert(k.key == @cache.delete(k) && !@cache.key?(k) && (@cache[k] = k.key; @cache[k] == k.key))
        i += 10
      end
    end
    assert(keys.all? {|k| @cache[k] == k.key})
  end
end
