module Here
class There
def why?(argument = nil)
@array = [
1,
2,
3
]
hash = {
one:1,
two:2,
three:3
}
end

# a comment
def why_not?(argument: {})
@array = [4,5,6]
hash = {four:4, five:5, six:6}
s = "a #{"complex"} string."
end

def complex_method(one, two, three, four)
regex = /regex/
end

def with_block
run = Proc.new { |argument| puts arugment }
run do |arugment|
puts argument
end
end
end
end
