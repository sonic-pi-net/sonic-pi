// A line comment.

/*
A general comment.
*/

// Keywords

	break        default      func         interface    select
	case         defer        go           map          struct
	chan         else         goto         package      switch
	const        fallthrough  if           range        type
	continue     for          import       return       var

// Operators, delimiters and special tokens

	+    &     +=    &=     &&    ==    !=    (    )
	-    |     -=    |=     ||    <     <=    [    ]
	*    ^     *=    ^=     <-    >     >=    {    }
	/    <<    /=    <<=    ++    =     :=    ,    ;
	%    >>    %=    >>=    --    !     ...   .    :
	     &^          &^=

// Integer literals

	42
	0600
	0xBadFace
	170141183460469231731687303715884105727

// Floating-point literals

	0.
	72.40
	072.40
	2.71828
	1.e+0
	6.67428e-11
	1E6
	.25
	.12345E+5

// Imaginary literals

	0i
	011i
	0.i
	2.71828i
	1.e+0i
	6.67428e-11i
	1E6i
	.25i
	.12345E+5i

// Character literals

	'a'
	'ä'
	'本'
	'\t'
	'\000'
	'\007'
	'\377'
	'\x07'
	'\xff'
	'\u12e4'
	'\U00101234'

// String literals

	`abc`
	`\n
	\n`
	"\n"
	""
	"Hello, world!\n"
	"日本語"
	"\u65e5本\U00008a9e"
	"\xff\u00FF"
	"\uD800"       // illegal: surrogate half
	"\U00110000"   // illegal: invalid Unicode code point
	"\z"           // illegal

// Predeclared identifiers

	// Types:
	bool byte complex64 complex128 error float32 float64
	int int8 int16 int32 int64 rune string
	uint uint8 uint16 uint32 uint64 uintptr

	// Constants:
	true false iota

	// Zero value:
	nil

	// Functions:
	append cap close complex copy delete imag len
	make new panic print println real recover

// Types

	type T1 string
	type T2 T1
	type T3 []T1
	type T4 T3

// Array types

	[32]byte
	[2*N] struct { x, y int32 }
	[1000]*float64
	[3][5]int
	[2][2][2]float64

// Struct types

	struct {}

	struct {
		x, y int
		u float32
		_ float32
		A *[]int
		F func()
	}

	struct {
		T1
		*T2
		P.T3
		*P.T4
		x, y int
	}

	struct {
		T
		*T
		*P.T
	}

	struct {
		microsec  uint64 "field 1"
		serverIP6 uint64 "field 2"
		process   string "field 3"
	}

// Function types

	func()
	func(x int) int
	func(a, _ int, z float32) bool
	func(a, b int, z float32) (bool)
	func(prefix string, values ...int)
	func(a, b int, z float64, opt ...interface{}) (success bool)
	func(int, int, float64) (float64, *[]int)
	func(n int) func(p *T)

// Interface types

	interface {
		Read(b Buffer) bool
		Write(b Buffer) bool
		Close()
	}

	type Lock interface {
		Lock()
		Unlock()
	}

// Channel types

	chan T
	chan<- float64
	<-chan int

	chan<- chan int
	chan<- <-chan int
	<-chan <-chan int
	chan (<-chan int)
