#!/usr/bin/env ruby
# encoding: UTF-8

require File.expand_path('../test_helper', __FILE__)

class UniqueCallPath
  def method_a(i)
    if i==1
      method_b
    else
      method_c
    end
  end

  def method_b
    method_c
  end

  def method_c
  end

  def method_k(i)
    method_a(i)
  end
end


# --  Tests ----
class UniqueCallPathTest < TestCase
  def test_root_method
    unique_call_path = UniqueCallPath.new

    result = RubyProf.profile do
      unique_call_path.method_a(1)
    end

    root_methods = Array.new
    result.threads.each do |thread|
      thread.methods.each do | m |
        if m.root?
          root_methods.push(m)
        end
      end
    end

    assert_equal(1, root_methods.length)
    assert_equal("UniqueCallPathTest#test_root_method", root_methods[0].full_name)
  end

  def test_root_children
    unique_call_path = UniqueCallPath.new

    result = RubyProf.profile do
      unique_call_path.method_a(1)
      unique_call_path.method_k(2)
    end

    root_methods = Array.new
    result.threads.each do |thread|
      thread.methods.each do | m |
        if m.root?
          root_methods.push(m)
        end
      end
    end

    assert_equal(1, root_methods.length)

    root_children = Array.new
    root_methods[0].children.each do | c |
      if c.parent.target.eql?(root_methods[0])
        root_children.push(c)
      end
    end

    children = root_children.sort do |c1, c2|
      c1.target.full_name <=> c2.target.full_name
    end

    assert_equal(2, children.length)
    assert_equal("UniqueCallPath#method_a", children[0].target.full_name)
    assert_equal("UniqueCallPath#method_k", children[1].target.full_name)
  end

  def test_children_of
    unique_call_path = UniqueCallPath.new

    result = RubyProf.profile do
      unique_call_path.method_a(1)
      unique_call_path.method_k(2)
    end

    root_methods = Array.new
    result.threads.each do |thread|
      thread.methods.each do | m |
        if m.root?
          root_methods.push(m)
        end
      end
    end

    assert_equal(1, root_methods.length)
    method = root_methods[0]
    assert_equal('UniqueCallPathTest#test_children_of', method.full_name)

    call_info_a = nil
    root_methods[0].children.each do | c |
      if c.target.full_name == "UniqueCallPath#method_a"
        call_info_a = c
        break
      end
    end

    assert !call_info_a.nil?

    children_of_a = Array.new

    call_info_a.children.each do | c |
      if c.parent.eql?(call_info_a)
        children_of_a.push(c)
      end
    end

    assert_equal(2, call_info_a.target.children.length)

    children_of_a = children_of_a.sort do |c1, c2|
      c1.target.full_name <=> c2.target.full_name
    end

    assert_equal(1, children_of_a.length)
    assert_equal("UniqueCallPath#method_b", children_of_a[0].target.full_name)
  end

  def test_id2ref
    unique_call_path = UniqueCallPath.new

    result = RubyProf.profile do
      unique_call_path.method_a(1)
    end

    root_methods = Array.new
    result.threads.each do |thread|
      thread.methods.each do | m |
        if m.root?
          root_methods.push(m)
        end
      end
    end

    child = root_methods[0].children[0]

    refute_equal(0, child.object_id)
    #assert_equal(RubyProf::CallInfo.id2ref(child.id).target.full_name, child.target.full_name)
  end

  def test_unique_path
    unique_call_path = UniqueCallPath.new

    result = RubyProf.profile do
      unique_call_path.method_a(1)
      unique_call_path.method_k(1)
    end

    root_methods = Array.new
    result.threads.each do |thread|
      thread.methods.each do | m |
        if m.root?
          root_methods.push(m)
        end
      end
    end

    assert_equal(1, root_methods.length)

    call_info_a = nil
    root_methods[0].children.each do | c |
      if c.target.full_name == "UniqueCallPath#method_a"
        call_info_a = c
        break
      end
    end

    assert !call_info_a.nil?

    children_of_a = Array.new
    call_info_a.children.each do |c|
      if c.parent.eql?(call_info_a)
        children_of_a.push(c)
      end
    end

    assert_equal(2, call_info_a.target.children.length)

    children_of_a = children_of_a.sort do |c1, c2|
      c1.target.full_name <=> c2.target.full_name
    end

    assert_equal(1, children_of_a.length)
    assert_equal(1, children_of_a[0].called)
    assert_equal("UniqueCallPath#method_b", children_of_a[0].target.full_name)
  end
end
