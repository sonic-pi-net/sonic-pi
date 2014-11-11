#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class PauseResumeTest < Test::Unit::TestCase
  def setup
    # Need to use wall time for this test due to the sleep calls
    RubyProf::measure_mode = RubyProf::WALL_TIME
  end

  def test_pause_resume
    # Measured
    RubyProf.start
    RubyProf::C1.hello

    # Not measured
    RubyProf.pause
    sleep 1
    RubyProf::C1.hello

    # Measured
    RubyProf.resume
    RubyProf::C1.hello

    result = RubyProf.stop

    # Length should be 3:
    #   PauseResumeTest#test_pause_resume
    #   <Class::RubyProf::C1>#hello
    #   Kernel#sleep

    methods = result.threads.first.methods.sort_by {|method_info| method_info.full_name}
    # remove methods called by pause/resume
    called_methods = ['Array#include?', 'Fixnum#==', 'Kernel#respond_to?', 'Kernel#respond_to_missing?']
    methods.reject!{|m| called_methods.include?(m.full_name) }
    # TODO: fix pause/resume to not include those methods in the first place
    assert_equal(3, methods.length)

    # Check the names
    assert_equal('<Class::RubyProf::C1>#hello', methods[0].full_name)
    assert_equal('Kernel#sleep', methods[1].full_name)
    assert_equal('PauseResumeTest#test_pause_resume', methods[2].full_name)

    # Check times
    assert_in_delta(0.2, methods[0].total_time, 0.01)
    assert_in_delta(0, methods[0].wait_time, 0.01)
    assert_in_delta(0, methods[0].self_time, 0.01)

    assert_in_delta(0.2, methods[1].total_time, 0.01)
    assert_in_delta(0, methods[1].wait_time, 0.01)
    assert_in_delta(0.2, methods[1].self_time, 0.01)

    assert_in_delta(0.2, methods[2].total_time, 0.01)
    assert_in_delta(0, methods[2].wait_time, 0.01)
    assert_in_delta(0, methods[2].self_time, 0.01)
  end

  # pause/resume in the same frame
  def test_pause_resume_1
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])

    profile.start
    method_1a

    profile.pause
    method_1b

    profile.resume
    method_1c

    result = profile.stop
    assert_in_delta(0.6, result.threads[0].methods.select{|m| m.full_name =~ /test_pause_resume_1$/}[0].total_time, 0.05)
  end
  def method_1a; sleep 0.2 end
  def method_1b; sleep 1   end
  def method_1c; sleep 0.4 end

  # pause in parent frame, resume in child
  def test_pause_resume_2
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])

    profile.start
    method_2a

    profile.pause
    sleep 0.5
    method_2b(profile)

    result = profile.stop
    assert_in_delta(0.6, result.threads[0].methods.select{|m| m.full_name =~ /test_pause_resume_2$/}[0].total_time, 0.05)
  end
  def method_2a; sleep 0.2 end
  def method_2b(profile); sleep 0.5; profile.resume; sleep 0.4 end

  # pause in child frame, resume in parent
  def test_pause_resume_3
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])

    profile.start
    method_3a(profile)

    sleep 0.5
    profile.resume
    method_3b

    result = profile.stop
    assert_in_delta(0.6, result.threads[0].methods.select{|m| m.full_name =~ /test_pause_resume_3$/}[0].total_time, 0.05)
  end
  def method_3a(profile); sleep 0.2; profile.pause; sleep 0.5 end
  def method_3b; sleep 0.4 end

  def test_pause_seq
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])
    profile.start ; assert !profile.paused?
    profile.pause ; assert profile.paused?
    profile.resume; assert !profile.paused?
    profile.pause ; assert profile.paused?
    profile.pause ; assert profile.paused?
    profile.resume; assert !profile.paused?
    profile.resume; assert !profile.paused?
    profile.stop  ; assert !profile.paused?
  end

  def test_pause_block
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])
    profile.start
    profile.pause
    assert profile.paused?

    times_block_invoked = 0
    retval= profile.resume{
      times_block_invoked += 1
      120 + times_block_invoked
    }
    assert_equal 1, times_block_invoked
    assert profile.paused?

    assert_equal 121, retval, "resume() should return the result of the given block."

    profile.stop
  end

  def test_pause_block_with_error
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])
    profile.start
    profile.pause
    assert profile.paused?

    begin
      profile.resume{ raise }
      flunk 'Exception expected.'
    rescue
      assert profile.paused?
    end

    profile.stop
  end

  def test_resume_when_not_paused
    profile = RubyProf::Profile.new(RubyProf::WALL_TIME,[])
    profile.start ; assert !profile.paused?
    profile.resume; assert !profile.paused?
    profile.stop  ; assert !profile.paused?
  end
end
