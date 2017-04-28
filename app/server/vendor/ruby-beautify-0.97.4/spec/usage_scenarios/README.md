## This Directory

The examples in this directory are not direct examples but file pairs we want to consume/test to ensure that what we want to see is what we get.

For example, the most basic test would be something like:

```ruby
def this_is_ugly(nowhere)
if true
puts true
else
puts false
end
end
```

We would expect this to look like:

```ruby
def this_is_ugly(nowhere)
	if true
		puts true
	else
		puts false
	end
end
```

Notice we are testing that we get the *actual* output we want.  Not the  configuratbility of the binary or our output but rather the representation of ugly vs beauty.

We can expect a high level of redundancy in these specific examples, and that is ok.  We want to ensure that as we make changes to the binary, we don't break the possible ways people use Ruby.  I don't care if one file tests something another file does, as long as our ideas of what we want prettied up are throughly tested.

We should avoid testing the binary.  Therefor, the tests will always be run with the binary in *default* mode.  Testing the binary's options can be handled elsewhere.

## File names

Each usage pair of files will need to be in a specific format of:

```
test_or_whatever.rb
test_or_whatever_pretty.rb
```

With the pretty file being what we want it to look like by default.  The spec framework will loop through, consume the pairs, execute the parsing method and print out the results.  This will allow for dynamic testing and migrating from one framework to another if need be.
