#!/usr/bin/env ruby

$: << File.expand_path('../../../lib', __FILE__)
require 'concurrent-edge'
Channel = Concurrent::Channel

## A Tour of Go: Equivalent Binary Trees
# https://tour.golang.org/concurrency/8

Tree = Struct.new(:value, :left, :right)

def new_tree(n, size = 10)
  values = [*1..size].collect{|i| i * n }.sample(size)
  root = Tree.new(values.shift)

  inserter = ->(current, new) do
    if new.value <= current.value
      if current.left.nil?
        current.left = new
      else
        inserter.call(current.left, new)
      end
    else
      if current.right.nil?
        current.right = new
      else
        inserter.call(current.right, new)
      end
    end
  end

  while value = values.shift do
    inserter.call(root, Tree.new(value))
  end

  root
end

def walk(tree, channel)
  _walk = ->(t, ch) do
    return unless t
    _walk.call(t.left, ch)
    ch << t.value
    _walk.call(t.right, ch)
  end

  _walk.call(tree, channel)
  channel.close
end

def same(t1, t2)
  ch1 = Channel.new
  ch2 = Channel.new

  Channel.go { walk(t1, ch1) }
  Channel.go { walk(t2, ch2) }

  ch1.each do |v|
    return false unless v == ~ch2
  end

  return true
end

puts same(new_tree(1), new_tree(1))
puts same(new_tree(1), new_tree(2))

__END__
true
false
