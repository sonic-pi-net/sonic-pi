require 'ripper'

h = Here::There.new

h.why?({
here: 'there',
there: 'here'
})

h.with_block {
puts 'yahooie'
}


h.complex_method('asdkjasflkjdglksjglksfgjlfkgjdf',
'alfkjsdlkfjsflgkjfglk',
'alfkjsdlkfjsflgkjfglk',
'alfkjsdlkfjsflgkjfglk'
)

h.complex_method('asdkjasflkjdglksjglksfgjlfkgjdf',
'alfkjsdlkfjsflgkjfglk',
'alfkjsdlkfjsflgkjfglk',
'alfkjsdlkfjsflgkjfglk'
).map do |i|
i
end.map! { |i| i }

if 1 > 0
puts 'something'
elsif 1 < 0
puts 'never!'
else
puts 'not likely.'
end

# Flattened formatting
if 1 > 0 then puts 'something'
else puts 'nothing' end
puts "This line should stay the same"
