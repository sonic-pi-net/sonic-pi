#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

# tests for bugs reported by users
class BugsTest < TestCase
  def setup
    RubyProf::measure_mode = RubyProf::WALL_TIME
    define_methods
  end

  def test_array_push_unoptimized
    a = nil
    result = RubyProf.profile do
      a = self.array_push_unoptimized
    end
    assert_equal 2, a.length
    assert_equal ["BugsTest#test_array_push_unoptimized", "BugsTest#array_push_unoptimized", 'Array#<<', "Array#push"], result.threads.first.methods.map(&:full_name)
  end

  def test_array_push_optimized
    a = nil
    result = RubyProf.profile do
      a = self.array_push_optimized
    end
    assert_equal 2, a.length
    assert_equal ["BugsTest#test_array_push_optimized", "BugsTest#array_push_optimized", "Array#push"], result.threads.first.methods.map(&:full_name)
  end

  private
  def define_methods
    return if respond_to?(:array_push_optimized)
    old_compile_option = RubyVM::InstructionSequence.compile_option
    RubyVM::InstructionSequence.compile_option = {
      :trace_instruction => true,
      :specialized_instruction => false
    }
    self.class.class_eval <<-"EOM"
      def array_push_unoptimized
        a = []
        a << 1
        a.push 2
      end
    EOM
    RubyVM::InstructionSequence.compile_option = old_compile_option
    self.class.class_eval <<-"EOM"
      def array_push_optimized
        a = []
        a << 1
        a.push 2
      end
    EOM
  end
end
