require 'rubygems'
require 'locale'

puts "Get the current locale."

p Locale.current  #=> Returns a language in a TagList.
p Locale.charset
p Locale.current.language
p Locale.current[0].language  #=> same result.

ENV["LANGUAGE"] = "ja_JP.eucJP:fr_FR"
puts "Set LANGUAGE." + ENV["LANGUAGE"]

# Clear locale because the values are cached.
Locale.clear

p Locale.current   #=> Return 2 languages in a TagList.
p Locale.current[0].language
p Locale.current[1].language
p Locale.current.language   #=> Same with Locale.current[0].language.
p Locale.charset

p "Locale.candidates"
p Locale.current
p Locale.candidates
