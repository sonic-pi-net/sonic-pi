# Test for multiline string
def m (x)
	puts "This is multi-line string. It's line1 \
 It's line 2\
              And this is\n line3
     This line should not be mutated"
	puts "This is multi line interpolated string #{
		x
	}"
end
