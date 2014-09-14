# taken from jimtcl:tests/parse.test

source [file dirname [info script]]/testing.tcl

test parse-1.1 "Quoted closing bracket" {
	set x [string length "]"]
} {1}

test parse-1.2 "Quoted opening bracket" {
	set x [string length "\["]
} {1}

test parse-1.3 "Quoted open brace" {
	set x [string length "\{"]
} {1}

test parse-1.4 "Quoted open brace via var" {
	set lb \{
	set x [string length "$lb"]
} {1}

test parse-1.5 "Braced bracket" {
	set x [string length {]}]
} {1}

test parse-1.6 "Dict sugar" -body {
    unset -nocomplain a
    array set a {a 1 b 2 c 3}
    set x $a(
} -returnCodes error -match glob -result "*"

test parse-1.8 "Dict sugar" {
    unset -nocomplain a
    array set a {a 1 b 2 c 3}
    set x $a([set y b])
} 2

test parse-1.9 "Backslash newline" {
	set x 123;\
	set y 456
	list $x $y
} {123 456}

test parse-1.10 "Backslash newline in quotes" {
	set x "abc\
def"
} "abc def"

test parse-1.11 "Backslash newline in quotes after var" {
	set y 1
	set x "abc$y\
def"
} "abc1 def"

test parse-1.12 "Backslash newline in quotes after var" {
	set y 1
	set x "abc$y\
def"
} "abc1 def"

test parse-1.13 "Newline in quotes" {
	set y 1
	set x "abc
def"
} "abc\ndef"

test parse-1.14 "Newline in quotes after var" {
	set y 1
	set x "abc$y
def"
} "abc1\ndef"

test parse-1.15 "Space in quotes" {
	set y 1
	set x "abc def"
} "abc def"

test parse-1.16 "Space in quotes after var" {
	set y 1
	set x "abc${y} def"
} "abc1 def"

test parse-1.17 "Command and var in quotes" {
	set y 1
	set x "[set z 2][set y]"
} 21

test parse-1.18 "Command and var in bare context" {
	set y 1
	set x [set z 2][set y]
} 21

test parse-1.19 "Lone dollar sign in quotes" {
	set y 1
	set x "6$[set y]"
} 6\$1

test parse-1.20 "Command and var in bare context" {
	set y 1
	set x 6$[set y]
} 6\$1

test parse-1.21 "Comment" {
	set y 1
# A comment one a line
	set x [set y] ;# comment after semicolon
} 1

test parse-1.22 "# char" {
	set y 1
	append y #
	set x "[set y]#"
} {1##}

test parse-1.23 "newline in command" {
	set y 1
	set z 2
	set x [incr y
	incr z]
	list $x $y $z
} {3 2 3}

test parse-1.24 "semicolon in command" {
	set x [list a; list b c; list d e f]
} {d e f}

# Note that Tcl complains about the missing brace here
# while Jim ignores it
test parse-1.25 "missing brace in var" jim {
	unset -nocomplain a
	set a 3
	set brace \{
	set x [subst \$${brace}a]
} 3

test parse-1.26 "newline in braced var" {
	set "a\nb" var1
	set x ${a
b}
} var1

test parse-1.27 "backslash escape in dict sugar" {
	unset -nocomplain a
	set a(b\x55d) 5
	set x $a(b\x55d)
} 5

test parse-1.28 "nested dict sugar" {
	unset -nocomplain a b
	set a(V) 5
	set b(5) five
	set x $b($a(V))
} five

set dq {"}
set script "set x ${dq}hello"

test parse-1.29 "missing quote" jim {
	eval $script
} hello

test parse-1.30 "missing quote" {
	info complete $script
} 0

test parse-1.31 "backslash newline in bare context" {
	list abc\
	123
} {abc 123}

test parse-1.32 "comment as last line of script" {
	set script {set x 3; # this is a comment}
	eval $script
} 3

test parse-1.33 "upper case hex escapes" {
	list \x4A \x4F \x3C
} {J O <}

test parse-1.34 "octal escapes" {
	list \112 \117 \074
} {J O <}

test parse-1.35 "invalid hex escape" {
	list \xZZ
} xZZ

test parse-1.36 "unicode escape" jim {
	list \u00b5
} \xc2\xb5

test parse-1.37 "invalid unicode escape after unicode" jim {
	list \ub5x
} \xc2\xb5x

test parse-1.38 "invalid unicode escape" {
	list \ux
} ux

test parse-1.39 "octal escape followed by invalid" {
	list \76x
} >x

test parse-1.40 "list containing quoted trailing backslash" jim {
	set x "abc \"def\\"
	lindex $x 1
} def\\

test parse-1.41 "list containing quoted newline" {
	set x {abc "def
ghi"}
	lindex $x 1
} def\nghi

test parse-1.42 "list containing missing quote" jim {
	set x {abc "def}
	lindex $x 1
} def

test parse-1.43 "list containing trailing backslash" {
	set x "abc def\\"
	lindex $x 1
} def\\

test parse-1.44 "list creation" {
	list "a{ }d"
} {{a{ }d}}

test parse-1.45 "spaces before expr function args" {
	expr {round  (3.2)}
} 3

test parse-1.46 "expr function missing paren" {
	catch {expr {round 3.2}}
} 1

test parse-1.47 "backslash newline in quotes" {
	# spaces
	set x "abc\
      def"
} "abc def"

test parse-1.48 "backslash newline in quotes" {
	# tabs
	set x "abc\
		def"
} "abc def"

test parse-1.49 "backslash newline in quotes" {
	# tabs plus newline
	set x "abc\

def"
} "abc \ndef"

test parse-1.50 "backslash newline in quotes" {
	# tabs plus newline
	set x "abc\
def"
} "abc def"

test parse-1.51 "special chars in dict sugar" {
	unset -nocomplain a
	set a(x$) 5
	array names a
} {{x$}}

test parse-1.52 "special chars in dict sugar" {
	set x $a(x$)
} 5

test parse-1.53 "special chars in dict sugar" {
	unset -nocomplain a
	set a(x\[) 5
	array names a
} {{x[}}

test parse-1.54 "special chars in dict sugar" {
	set x $a(x\[)
} 5

test parse-1.55 "special chars in dict sugar" {
	unset -nocomplain a
	set a(x\() 5
	array names a
} {x(}

test parse-1.56 "special chars in dict sugar" {
	set x $a(x\()
} 5

test parse-1.57 "special chars in dict sugar" {
	unset -nocomplain a
	set a(x() 5
	array names a
} {x(}

test parse-1.58 "special chars in dict sugar" {
	set x $a(x()
} 5

test parse-1.59 "special chars in dict sugar" {
	unset -nocomplain a
	set a(x") 5
	lindex [array names a] 0
} {x"}

test parse-1.60 "special chars in dict sugar" {
	set x $a(x")
} 5

test parse-1.61 "quote in command" {
	set x [list \\" x]
	lindex $x end
} x

test parse-1.62 "quoted orphan dollar sign" {
	set x "x$"
} {x$}

test parse-1.63 "unquoted dollar sign" {
	set x x$
} {x$}

testreport
