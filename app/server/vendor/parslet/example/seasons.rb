$:.unshift File.dirname(__FILE__) + "/../lib"

require 'parslet'
require 'pp'

tree = {:bud => {:stem => []}}

class Spring < Parslet::Transform
  rule(:stem => sequence(:branches)) {
    {:stem => (branches + [{:branch => :leaf}])}
  }
end
class Summer < Parslet::Transform
  rule(:stem => subtree(:branches)) {
    new_branches = branches.map { |b| {:branch => [:leaf, :flower]} }
    {:stem => new_branches}
  }
end
class Fall < Parslet::Transform
  rule(:branch => sequence(:x)) {
    x.each { |e| puts "Fruit!" if e==:flower }
    x.each { |e| puts "Falling Leaves!" if e==:leaf }
    {:branch => []}
  }
end
class Winter < Parslet::Transform
  rule(:stem => subtree(:x)) {
    {:stem => []}
  }
end

def do_seasons(tree)
  [Spring, Summer, Fall, Winter].each do |season|
    p "And when #{season} comes"
    tree = season.new.apply(tree)
    pp tree
    puts
  end
  tree
end

# What marvel of life!
tree = do_seasons(tree)
tree = do_seasons(tree)


