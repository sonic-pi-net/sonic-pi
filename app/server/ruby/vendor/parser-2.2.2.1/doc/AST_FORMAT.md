AST and Source Location
=======================

## Literals

### Singletons

Format:

~~~
(true)
"true"
 ~~~~ expression

(false)
"false"
 ~~~~~ expression

(nil)
"nil"
 ~~~ expression
~~~

### Integer

Format:

~~~
(int 123)
"123"
 ~~~ expression

(int -123)
"-123"
 ^ operator
 ~~~ expression

(int 1)
"__LINE__"
 ~~~~~~~~ expression
~~~

### Float

Format:

~~~
(float 1.0)
"1.0"
 ~~~ expression

(float -1.0)
"-1.0"
 ^ operator
 ~~~~ expression
~~~

### Complex

Format:

~~~
(complex (0+1i))
"1i"
 ~~ expression

(complex (0+(1/1)*i))
"1ri"
 ~~~ expression
~~~

### Rational

Format:

~~~
(rational (2/1))
"2.0r"
 ~~~~ expression
~~~

### String

#### Plain

Format:

~~~
(str "foo")
"'foo'"
 ^ begin
     ^ end
 ~~~~~ expresion

(string "foo.rb")
"__FILE__"
 ~~~~~~~~ expression
~~~

#### With interpolation

Format:

~~~
(dstr (str "foo") (lvar bar) (str "baz"))
'"foo#{bar}baz"'
 ^ begin      ^ end
 ~~~~~~~~~~~~~~ expression
~~~

#### Here document

Format:

~~~
(str "foo\nbar\n")
'<<HERE␊foo␊bar␊HERE'
 ~~~~~~ expression
        ~~~~~~~~ heredoc_body
                ~~~~ heredoc_end
~~~

### Symbol

#### Plain

Format:

~~~
(sym :foo)
":foo"
 ~~~~ expresion

":'foo'"
  ^ begin
      ^ end
 ~~~~~~ expression
~~~

#### With interpolation

Format:

~~~
(dsym (str "foo") (lvar bar) (str "baz"))
':"foo#{bar}baz"'
  ^ begin      ^ end
 ~~~~~~~~~~~~~~~ expression
~~~

### Execute-string

#### Plain

Format:

~~~
(xstr (str "foo") (lvar bar))
"`foo#{bar}`"
 ^ begin   ^ end
 ~~~~~~~~~~~ expression
~~~

#### Here document

Format:

~~~
(xstr (str "foo\nbar\n"))
"<<`HERE`␊foo␊bar␊HERE"
 ~~~~~~~~ expression
          ~~~~~~~~ heredoc_body
                  ~~~~ heredoc_end
~~~

### Regexp

#### Options

Format:

~~~
(regopt :i :m)
"im"
 ~~ expression
~~~

#### Regexp

Format:

~~~
(regexp (str "foo") (lvar :bar) (regopt :i))
"/foo#{bar}/i"
 ^ begin   ^ end
 ~~~~~~~~~~~ expression
~~~

### Array

#### Plain

Format:

~~~
(array (int 1) (int 2))

"[1, 2]"
 ^ begin
      ^ end
 ~~~~~~ expression
~~~

#### Splat

Can also be used in argument lists: `foo(bar, *baz)`

Format:

~~~
(splat (lvar :foo))
"*foo"
 ^ operator
 ~~~~ expression
~~~

#### With interpolation

Format:

~~~
(array (int 1) (splat (lvar :foo)) (int 2))

"[1, *foo, 2]"
 ^ begin    ^ end
 ~~~~~~~~~~~~ expression
~~~

### Hash

#### Pair

##### With hashrocket

Format:

~~~
(pair (int 1) (int 2))
"1 => 2"
   ~~ operator
 ~~~~~~ expression
~~~

##### With label (1.9)

Format:

~~~
(pair (sym :answer) (int 42))
"answer: 42"
       ^ operator (pair)
 ~~~~~~ expression (sym)
 ~~~~~~~~~~ expression (pair)
~~~

#### Plain

Format:

~~~
(hash (pair (int 1) (int 2)) (pair (int 3) (int 4)))
"{1 => 2, 3 => 4}"
 ^ begin        ^ end
 ~~~~~~~~~~~~~~~~ expression
~~~

#### Keyword splat (2.0)

Can also be used in argument lists: `foo(bar, **baz)`

Format:

~~~
(kwsplat (lvar :foo))
"**foo"
 ~~ operator
 ~~~~~ expression
~~~

#### With interpolation (2.0)

Format:

~~~
(hash (pair (sym :foo) (int 2)) (kwsplat (lvar :bar)))
"{ foo: 2, **bar }"
 ^ begin         ^ end
 ~~~~~~~~~~~~~~~~~ expression
~~~

### Range

#### Inclusive

Format:

~~~
(irange (int 1) (int 2))
"1..2"
  ~~ operator
 ~~~~ expression
~~~

#### Exclusive

Format:

~~~
(erange (int 1) (int 2))
"1...2"
  ~~~ operator
 ~~~~~ expression
~~~

## Access

### Self

Format:

~~~
(self)
"self"
 ~~~~ expression
~~~

### Local variable

Format:

~~~
(lvar :foo)
"foo"
 ~~~ expression
~~~

### Instance variable

Format:

~~~
(ivar :@foo)
"@foo"
 ~~~~ expression
~~~

### Class variable

Format:

~~~
(cvar :@@foo)
"@@foo"
 ~~~~~ expression
~~~

### Global variable

#### Regular global variable

Format:

~~~
(gvar :$foo)
"$foo"
 ~~~~ expression
~~~

#### Regular expression capture groups

Format:

~~~
(nth-ref 1)
"$1"
 ~~ expression
~~~

#### Regular expression back-references

Format:

~~~
(back-ref :$&)
"$&"
 ~~ expression
(back-ref :$`)
"$`"
(back-ref :$')
"$'"
(back-ref :$+)
"$+"
~~~

### Constant

#### Top-level constant

Format:

~~~
(const (cbase) :Foo)
"::Foo"
   ~~~ name
 ~~ double_colon
 ~~~~~ expression
~~~

#### Scoped constant

Format:

~~~
(const (lvar :a) :Foo)
"a::Foo"
    ~~~ name
  ~~ double_colon
 ~~~~~~ expression
~~~

#### Unscoped constant

Format:

~~~
(const nil :Foo)
"Foo"
 ~~~ name
 ~~~ expression
~~~

### defined?

Format:

~~~
(defined? (lvar :a))
"defined? a"
 ~~~~~~~~ keyword
 ~~~~~~~~~~ expression

"defined?(a)"
 ~~~~~~~~ keyword
         ^ begin
           ^ end
 ~~~~~~~~~~~ expression
~~~

## Assignment

### To local variable

Format:

~~~
(lvasgn :foo (lvar :bar))
"foo = bar"
     ^ operator
 ~~~~~~~~~ expression
~~~

### To instance variable

Format:

~~~
(ivasgn :@foo (lvar :bar))
"@foo = bar"
      ^ operator
 ~~~~~~~~~~ expression
~~~

### To class variable

Format:

~~~
(cvasgn :@@foo (lvar :bar))
"@@foo = bar"
       ^ operator
 ~~~~~~~~~~~ expression
~~~

### To global variable

Format:

~~~
(gvasgn :$foo (lvar :bar))
"$foo = bar"
      ^ operator
 ~~~~~~~~~~ expression
~~~

### To constant

#### Top-level constant

Format:

~~~
(casgn (cbase) :Foo (int 1))
"::Foo = 1"
   ~~~ name
       ~ operator
 ~~~~~~~ expression
~~~

#### Scoped constant

Format:

~~~
(casgn (lvar :a) :Foo (int 1))
"a::Foo = 1"
    ~~~ name
        ~ operator
 ~~~~~~~~ expression
~~~

#### Unscoped constant

Format:

~~~
(casgn nil :Foo (int 1))
"Foo = 1"
 ~~~ name
     ~ operator
 ~~~~~~~ expression
~~~


### Multiple assignment

#### Multiple left hand side

Format:

~~~
(mlhs (lvasgn :a) (lvasgn :b))
"a, b"
 ~~~~ expression
"(a, b)"
 ^ begin
      ^ end
 ~~~~~~ expression
~~~

#### Assignment

Rule of thumb: every node inside `(mlhs)` is "incomplete"; to make
it "complete", one could imagine that a corresponding node from the
mrhs is "appended" to the node in question. This applies both to
side-effect free assignments (`lvasgn`, etc) and side-effectful
assignments (`send`).

Format:

~~~
(masgn (mlhs (lvasgn :foo) (lvasgn :bar)) (array (int 1) (int 2)))
"foo, bar = 1, 2"
          ^ operator
 ~~~~~~~~~~~~~~~ expression

(masgn (mlhs (ivasgn :@a) (cvasgn :@@b)) (array (splat (lvar :c))))
"@a, @@b = *c"

(masgn (mlhs (mlhs (lvasgn :a) (lvasgn :b)) (lvasgn :c)) (lvar :d))
"a, (b, c) = d"

(masgn (mlhs (send (self) :a=) (send (self) :[]= (int 1))) (lvar :a))
"self.a, self[1] = a"
~~~

### Binary operator-assignment

Binary operator-assignment features the same "incomplete assignments" and "incomplete calls" as [multiple assignment](#assignment-1).

#### Variable binary operator-assignment

Format:

~~~
(op-asgn (lvasgn :a) :+ (int 1))
"a += 1"
   ~~ operator
 ~~~~~~ expression

(op-asgn (ivasgn :a) :+ (int 1))
"@a += 1"
~~~

Ruby_parser output for reference:
~~~
"a += 1"
s(:lasgn, :a, s(:call, s(:lvar, :a), :+, s(:int, 1)))

"@a += 1"
s(:iasgn, :@a, s(:call, s(:ivar, :@a), :+, s(:int, 1)))
~~~

#### Method binary operator-assignment

Format:

~~~
(op-asgn (send (ivar :@a) :b) :+ (int 1))
"@a.b += 1"
    ~ selector (send)
 ~~~~ expression (send)
      ~~ operator (op-asgn)
 ~~~~~~~~~ expression (op-asgn)

(op-asgn (send (ivar :@a) :[] (int 0) (int 1))) :+ (int 1))
"@a[0, 1] += 1"
   ~~~~~~ selector (send)
 ~~~~~~~~ expression (send)
          ~~ operator (op-asgn)
 ~~~~~~~~~~~~~ expression (op-asgn)
~~~

Ruby_parser output for reference:
~~~
"@a.b += 1"
s(:op_asgn2, s(:ivar, :@a), :b=, :+, s(:int, 1))

"@a[0, 1] += 1"
s(:op_asgn1, s(:ivar, :@a), s(:arglist, s(:int, 0), s(:int, 1)), :+, s(:int, 1))
~~~

### Logical operator-assignment

Logical operator-assignment features the same "incomplete assignments" and "incomplete calls" as [multiple assignment](#assignment-1).

#### Variable logical operator-assignment

Format:

~~~
(or-asgn (ivasgn :@a) (int 1))
"@a ||= 1"
    ~~~ operator
 ~~~~~~~~ expression

(and-asgn (lvasgn :a) (int 1))
"a &&= 1"
   ~~~ operator
 ~~~~~~~ expression
~~~

Ruby_parser output for reference:
~~~
"@a ||= 1"
s(:op_asgn_or, s(:ivar, :@a), s(:ivasgn, :@a, s(:int, 1)))

"a &&= 1"
s(:op_asgn_and, s(:lvar, :a), s(:lvasgn, :a, s(:int, 1)))
~~~

#### Method logical operator-assignment

Format:

~~~
(or-asgn (send (ivar :@foo) :bar) (int 1))
"@foo.bar ||= 1"
      ~~~ selector (send)
 ~~~~~~~~ expr (send)
          ~~~ operator (or-asgn)
 ~~~~~~~~~~~~~~ expression (or-asgn)

(and-asgn (send (lvar :@foo) :bar) (int 1))
"foo.bar &&= 1"
     ~~~ selector (send)
 ~~~~~~~ expr (send)
         ~~~ operator (and-asgn)
 ~~~~~~~~~~~~~ expression (and-asgn)

(or-asgn (send (ivar :@foo) :[] (int 1) (int 2)) (int 1))
"@foo[1, 2] ||= 1"
     ~~~~~~ selector (send)
 ~~~~~~~~~~ expr (send)
            ~~~ operator (or-asgn)
 ~~~~~~~~~~~~~~~~ expression (or-asgn)

~~~

Ruby_parser output for reference:
~~~
"@foo.bar &&= 1"
s(:op_asgn2, s(:ivar, :@foo), :bar=, :"&&", s(:int, 1))

"@foo[0] ||= 1"
s(:op_asgn1, s(:ivar, :@foo), s(:arglist, s(:int, 0)), :"||", s(:int, 1))

~~~

## Class and module definition

### Module

Format:

~~~
(module (const nil :Foo) (nil))
"module Foo; end"
 ~~~~~~ keyword
             ~~~ end
~~~

### Class

Format:

~~~
(class (const nil :Foo) (const nil :Bar) (nil))
"class Foo < Bar; end"
 ~~~~~ keyword    ~~~ end
           ~ operator
 ~~~~~~~~~~~~~~~~~~~~ expression

(class (const nil :Foo) nil (nil))
"class Foo; end"
 ~~~~~ keyword
            ~~~ end
 ~~~~~~~~~~~~~~ expression
~~~

### Singleton class

Format:

~~~
(sclass (lvar :a) (nil))
"class << a; end"
 ~~~~~ keyword
       ~~ operator
             ~~~ end
 ~~~~~~~~~~~~~~~ expression
~~~

## Method (un)definition

### Instance methods

Format:

~~~
(def :foo (args) nil)
"def foo; end"
 ~~~ keyword
     ~~~ name
          ~~~ end
 ~~~~~~~~~~~~ expression
~~~

### Singleton methods

Format:

~~~
(defs (self) (args) nil)
"def self.foo; end"
 ~~~ keyword
          ~~~ name
               ~~~ end
 ~~~~~~~~~~~~~~~~~ expression
~~~

### Undefinition

Format:

~~~
(undef (sym :foo) (sym :bar) (dsym (str "foo") (int 1)))
"undef foo :bar :"foo#{1}""
 ~~~~~ keyword
 ~~~~~~~~~~~~~~~~~~~~~~~~~ expression
~~~

## Aliasing

### Method aliasing

Format:

~~~
(alias (sym :foo) (dsym (str "foo") (int 1)))
"alias foo :"foo#{1}""
 ~~~~~ keyword
 ~~~~~~~~~~~~~~~~~~~~ expression
~~~

### Global variable aliasing

Format:

~~~
(alias (gvar :$foo) (gvar :$bar))
"alias $foo $bar"
 ~~~~~ keyword
 ~~~~~~~~~~~~~~~ expression

(alias (gvar :$foo) (back-ref :$&))
"alias $foo $&"
 ~~~~~ keyword
 ~~~~~~~~~~~~~~~ expression
~~~

## Formal arguments

Format:

~~~
(args (arg :foo))
"(foo)"
 ~~~~~ expression
~~~

### Required argument

Format:

~~~
(arg :foo)
"foo"
 ~~~ expression
 ~~~ name
~~~

### Optional argument

Format:

~~~
(optarg :foo (int 1))
"foo = 1"
 ~~~~~~~ expression
     ^ operator
 ~~~ name
~~~

### Named splat argument

Format:

~~~
(restarg :foo)
"*foo"
 ~~~~ expression
  ~~~ name
~~~

Begin of the `expression` points to `*`.

### Unnamed splat argument

Format:

~~~
(restarg)
"*"
 ^ expression
~~~

### Block argument

Format:

~~~
(blockarg :foo)
"&foo"
  ~~~ name
 ~~~~ expression
~~~

Begin of the `expression` points to `&`.

### Expression arguments

Ruby 1.8 allows to use arbitrary expressions as block arguments,
such as `@var` or `foo.bar`. Such expressions should be treated as
if they were on the lhs of a multiple assignment.

Format:

~~~
(args (arg_expr (ivasgn :@bar)))
"|@bar|"

(args (arg_expr (send (send nil :foo) :a=)))
"|foo.a|"

(args (restarg_expr (ivasgn :@bar)))
"|*@bar|"

(args (blockarg_expr (ivasgn :@bar)))
"|&@bar|"
~~~

### Block shadow arguments

Format:

~~~
(args (shadowarg :foo) (shadowarg :bar))
"|; foo, bar|"
~~~

### Decomposition

Format:

~~~
(def :f (args (arg :a) (mlhs (arg :foo) (restarg :bar))))
"def f(a, (foo, *bar)); end"
          ^ begin   ^ end
          ~~~~~~~~~~~ expression
~~~

### Required keyword argument

Format:

~~~
(kwarg :foo (int 1))
"foo:"
 ~~~~ expression
 ~~~~ name
~~~

### Optional keyword argument

Format:

~~~
(kwoptarg :foo (int 1))
"foo: 1"
 ~~~~~~ expression
 ~~~~ name
~~~

### Named keyword splat argument

Format:

~~~
(kwrestarg :foo)
"**foo"
 ~~~~~ expression
   ~~~ name
~~~

### Unnamed keyword splat argument

Format:

~~~
(kwrestarg)
"**"
 ~~ expression
~~~

## Send

### To self

Format:

~~~
(send nil :foo (lvar :bar))
"foo(bar)"
 ~~~ selector
    ^ begin
        ^ end
 ~~~~~~~~ expression
~~~

### To receiver

Format:

~~~
(send (lvar :foo) :bar (int 1))
"foo.bar(1)"
     ~~~ selector
        ^ begin
          ^ end
 ~~~~~~~~~~ expression

(send (lvar :foo) :+ (int 1))
"foo + 1"
     ^ selector
 ~~~~~~~ expression

(send (lvar :foo) :-@)
"-foo"
 ^ selector
 ~~~~ expression

(send (lvar :foo) :a= (int 1))
"foo.a = 1"
     ~ selector
       ^ operator
 ~~~~~~~~~ expression

(send (lvar :foo) :[] (int 1))
"foo[i]"
    ~~~ selector
 ~~~~~~ expression

(send (lvar :bar) :[]= (int 1) (int 2) (lvar :baz))
"bar[1, 2] = baz"
    ~~~~~~ selector
           ^ operator
 ~~~~~~~~~~~~~~~ expression

~~~

### To superclass

Format of super with arguments:
~~~
(super (lvar :a))
"super a"
 ~~~~~ keyword
 ~~~~~~~ expression

(super)
"super()"
      ^ begin
       ^ end
 ~~~~~ keyword
 ~~~~~~~ expression
~~~

Format of super without arguments (**z**ero-arity):
~~~
(zsuper)
"super"
 ~~~~~ keyword
 ~~~~~ expression
~~~

### To block argument

Format:

~~~
(yield (lvar :foo))
"yield(foo)"
 ~~~~~ keyword
      ^ begin
          ^ end
 ~~~~~~~~~~ expression
~~~

### Passing a literal block

~~~
(block (send nil :foo) (args (arg :bar)) (begin ...))
"foo do |bar|; end"
     ~~ begin
               ~~~ end
     ~~~~~~~~~~~~~ expression
~~~

### Passing expression as block

Used when passing expression as block `foo(&bar)`

~~~
(send nil :foo (int 1) (block-pass (lvar :foo)))
"foo(1, &foo)"
        ^ operator
        ~~~~ expression
~~~

## Control flow

### Logical operators

#### Binary (and or && ||)

Format:

~~~
(and (lvar :foo) (lvar :bar))
"foo and bar"
     ~~~ operator
 ~~~~~~~~~~~ expression
~~~

~~~
(or (lvar :foo) (lvar :bar))
"foo or bar"
     ~~ operator
 ~~~~~~~~~~ expression
~~~

#### Unary (! not) (1.8)

Format:

~~~
(not (lvar :foo))
"!foo"
 ^ operator
"not foo"
 ~~~ operator
~~~

### Branching

#### Without else

Format:

~~~
(if (lvar :cond) (lvar :iftrue) nil)
"if cond then iftrue; end"
 ~~ keyword
         ~~~~ begin
                      ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~ expression

"if cond; iftrue; end"
 ~~ keyword
                  ~~~ end
 ~~~~~~~~~~~~~~~~~~~~ expression

"iftrue if cond"
        ~~ keyword
 ~~~~~~~~~~~~~~ expression

(if (lvar :cond) nil (lvar :iftrue))
"unless cond then iftrue; end"
 ~~~~~~ keyword
             ~~~~ begin
                          ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"unless cond; iftrue; end"
 ~~~~~~ keyword
                      ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~ expression

"iftrue unless cond"
        ~~~~~~ keyword
 ~~~~~~~~~~~~~~~~~~ expression
~~~

#### With else

Format:

~~~
(if (lvar :cond) (lvar :iftrue) (lvar :iffalse))
"if cond then iftrue; else; iffalse; end"
 ~~ keyword
         ~~~~ begin
                      ~~~~ else
                                 ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"if cond; iftrue; else; iffalse; end"
 ~~ keyword
                  ~~~~ else
                                 ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

(if (lvar :cond) (lvar :iffalse) (lvar :iftrue))
"unless cond then iftrue; else; iffalse; end"
 ~~~~~~ keyword
             ~~~~ begin
                          ~~~~ else
                                     ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"unless cond; iftrue; else; iffalse; end"
 ~~~~~~ keyword
                      ~~~~ else
                                     ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression
~~~

#### With elsif

Format:

~~~
(if (lvar :cond1) (int 1) (if (lvar :cond2 (int 2) (int 3))))
"if cond1; 1; elsif cond2; 2; else 3; end"
 ~~ keyword (left)
              ~~~~~ else (left)
                                      ~~~ end (left)
              ~~~~~ keyword (right)
                              ~~~~ else (right)
                                      ~~~ end (right)
~~~

#### Ternary

Format:

~~~
(if (lvar :cond) (lvar :iftrue) (lvar :iffalse))
"cond ? iftrue : iffalse"
      ^ question
               ^ colon
 ~~~~~~~~~~~~~~~~~~~~~~~ expression
~~~

### Case matching

#### When clause

Format:

~~~
(when (regexp "foo" (regopt)) (begin (lvar :bar)))
"when /foo/ then bar"
 ~~~~ keyword
            ~~~~ begin
 ~~~~~~~~~~~~~~~~~~~ expression

(when (int 1) (int 2) (send nil :meth))
"when 1, 2; meth"

(when (int 1) (splat (lvar :foo)) (send nil :meth))
"when 1, *foo; meth"

(when (splat (lvar :foo)) (send nil :meth))
"when *foo; meth"
~~~

#### Case-expression clause

##### Without else

Format:

~~~
(case (lvar :foo) (when (str "bar") (lvar :bar)) nil)
"case foo; when "bar"; bar; end"
 ~~~~ keyword               ~~~ end
~~~

##### With else

Format:

~~~
(case (lvar :foo) (when (str "bar") (lvar :bar)) (lvar :baz))
"case foo; when "bar"; bar; else baz; end"
 ~~~~ keyword               ~~~~ else ~~~ end
~~~

#### Case-conditions clause

##### Without else

Format:

~~~
(case nil (when (lvar :bar) (lvar :bar)) nil)
"case; when bar; bar; end"
 ~~~~ keyword         ~~~ end
~~~

##### With else

Format:

~~~
(case nil (when (lvar :bar) (lvar :bar)) (lvar :baz))
"case; when bar; bar; else baz; end"
 ~~~~ keyword         ~~~~ else ~~~ end

(case nil (lvar :baz))
"case; else baz; end"
 ~~~~ keyword
       ~~~~ else
                 ~~~ end
~~~

### Looping

#### With precondition

Format:

~~~
(while (lvar :condition) (send nil :foo))
"while condition do foo; end"
 ~~~~~ keyword
                 ~~ begin
                         ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"while condition; foo; end"
 ~~~~~ keyword
                       ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"foo while condition"
     ~~~~~ keyword
 ~~~~~~~~~~~~~~~~~~~ expression

(until (lvar :condition) (send nil :foo))
"until condition do foo; end"
 ~~~~~ keyword
                 ~~ begin
                         ~~~ end
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

(until (lvar :condition) (send nil :foo))
"until condition; foo; end"
 ~~~~~ keyword
                       ~~~ end
~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"foo until condition"
     ~~~~~ keyword
 ~~~~~~~~~~~~~~~~~~~ expression
~~~

#### With postcondition

Format:

~~~
(while-post (lvar :condition) (kwbegin (send nil :foo)))
"begin; foo; end while condition"
 ~~~~~ begin (begin)
             ~~~ end (begin)
                 ~~~~~ keyword (while-post)

(until-post (lvar :condition) (kwbegin (send nil :foo)))
"begin; foo; end until condition"
 ~~~~~ begin (begin)
             ~~~ end (begin)
                 ~~~~~ keyword (until-post)
~~~

#### For-in

Format:

~~~
(for (lvasgn :a) (lvar :array) (send nil :p (lvar :a)))
"for a in array do p a; end"
 ~~~ keyword
       ~~ in
                ~~ begin
                        ~~~ end

"for a in array; p a; end"
 ~~~ keyword
       ~~ in
                      ~~~ end

(for
  (mlhs (lvasgn :a) (lvasgn :b)) (lvar :array)
  (send nil :p (lvar :a) (lvar :b)))
"for a, b in array; p a, b; end"
~~~

#### Break

Format:

~~~
(break (int 1))
"break 1"
 ~~~~~ keyword
 ~~~~~~~ expression
~~~

#### Next

Format:

~~~
(next (int 1))
"next 1"
 ~~~~ keyword
 ~~~~~~ expression
~~~

#### Redo

Format:

~~~
(redo)
"redo"
 ~~~~ keyword
 ~~~~ expression
~~~

### Return

Format:

~~~
(return (lvar :foo))
"return(foo)"
 ~~~~~~ keyword
 ~~~~~~~~~~~ expression
~~~

### Exception handling

#### Rescue body

Format:

~~~
(resbody (array (const nil :Exception) (const nil :A)) (lvasgn :bar) (int 1))
"rescue Exception, A => bar; 1"
 ~~~~~~ keyword      ~~ assoc
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression

"rescue Exception, A => bar then 1"
 ~~~~~~ keyword      ~~ assoc
                            ~~~~ begin

(resbody (array (const nil :Exception)) (ivasgn :bar) (int 1))
"rescue Exception => @bar; 1"
 ~~~~~~ keyword   ~~ assoc

(resbody nil (lvasgn :bar) (int 1))
"rescue => bar; 1"
 ~~~~~~ keyword
        ~~ assoc

(resbody nil nil (int 1))
"rescue; 1"
 ~~~~~~ keyword
~~~

#### Rescue statement

##### Without else

Format:

~~~
(begin
  (rescue (send nil :foo) (resbody ...) (resbody ...) nil))
"begin; foo; rescue Exception; rescue; end"
 ~~~~~ begin                           ~~~ end
             ~~~~~~~~~~~~~~~~~ expression (rescue.resbody/1)
                               ~~~~~~~ expression (rescue.resbody/2)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (rescue)
~~~

##### With else

Format:

~~~
(begin
  (rescue (send nil :foo) (resbody ...) (resbody ...) (true)))
"begin; foo; rescue Exception; rescue; else true end"
 ~~~~~ begin                           ~~~~ else (rescue)
                                                 ~~~ end
~~~

#### Ensure statement

Format:

~~~
(begin
  (ensure (send nil :foo) (send nil :bar))
"begin; foo; ensure; bar; end"
 ~~~~~ begin ~~~~~~ keyword (ensure)
                          ~~~ end
~~~

#### Rescue with ensure

Format:

~~~
(begin
  (ensure
    (rescue (send nil :foo) (resbody ...) (int 1))
    (send nil :bar))
"begin; foo; rescue; nil; else; 1; ensure; bar; end"
 ~~~~~ begin
                          ~~~~ else (ensure.rescue)
             ~~~~~~~~~~~~~~~~~~~~~ expression (rescue)
                                   ~~~~~~ keyword (ensure)
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expression (ensure)
                                                ~~~ end
~~~

#### Retry

Format:

~~~
(retry)
"retry"
 ~~~~~ keyword
 ~~~~~ expression
~~~

### BEGIN and END

Format:

~~~
(preexe (send nil :puts (str "foo")))
"BEGIN { puts "foo" }"
 ~~~~~ keyword
       ^ begin      ^ end
 ~~~~~~~~~~~~~~~~~~~~ expression

(postexe (send nil :puts (str "bar")))
"END { puts "bar" }"
 ~~~ keyword
     ^ begin      ^ end
 ~~~~~~~~~~~~~~~~~~ expression
~~~

## Miscellanea

### Flip-flops

Format:

~~~
(iflipflop (lvar :a) (lvar :b))
"if a..b; end"
     ~~ operator
    ~~~~ expression

(eflipflop (lvar :a) (lvar :b))
"if a...b; end"
     ~~~ operator
    ~~~~~ expression
~~~

### Implicit matches

Format:

~~~
(match-current-line (regexp (str "a") (regopt)))
"if /a/; end"
    ~~~ expression
~~~

### Local variable injecting matches

Format:

~~~
(match-with-lvasgn (regexp (str "(?<match>bar)") (regopt)) (lvar :baz))
"/(?<match>bar)/ =~ baz"
                 ~~ selector
 ~~~~~~~~~~~~~~~~~~~~~~ expression
~~~
