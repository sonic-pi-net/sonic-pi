#! /usr/bin/moon

-- A heap of sample Moonscript syntax

util = require "my.module"

import SomeClass, member from util
import other from require "my.other_module"

a_table = {
  foo: 'bar'
  interpolated: "foo-#{other.stuff 2 + 3}"
  "string-key": 2
  do: 'keyword'
}

short_table_def = foo: 'bar', interpolated: "foo-#{other.stuff 2 + 3}"
scoped_table = :util, :a_table

multiline_string = "line 1
  for the alliance!
line2"

other_multiline_string = [[ for
the
win
]]

local x
export y

x or= 1
x += 1
y and= x

empty_function ->
args_function (arg1, arg2) -> arg1 + arg2
var_args_function (...) -> table.concat {...}, '|'

while cond == true do empty_function!

comprehension = [item * 2 for i, item in ipairs items when item != 3]

for i = 1,10
  continue unless i != 2

SomeClass(0xdeadbeef)\method 'foo'

with a_table
  .foobar = {}

switch i
  when 2
    "not first"

class MyClass extends SomeClass
  new: (@init, arg2 = 'default') =>
    @derived = @init + 2
    super!

  other: =>
    @@foo + 2
    @
