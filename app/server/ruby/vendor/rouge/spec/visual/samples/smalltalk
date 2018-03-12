"======================================================================
|
|   Python-like Generators
|
|
 ======================================================================"


"======================================================================
|
| Copyright 2003 Free Software Foundation, Inc.
| Written by Paolo Bonzini.
|
| This file is part of GNU Smalltalk.
|
| GNU Smalltalk is free software; you can redistribute it and/or modify it
| under the terms of the GNU General Public License as published by the Free
| Software Foundation; either version 2, or (at your option) any later version.
| 
| GNU Smalltalk is distributed in the hope that it will be useful, but WITHOUT
| ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
| FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
| details.
| 
| You should have received a copy of the GNU General Public License along with
| GNU Smalltalk; see the file COPYING.  If not, write to the Free Software
| Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  
|
 ======================================================================"

Stream subclass: #Generator
	  instanceVariableNames: 'next topContext bottomContext contexts suspendedContext atEnd'
	  classVariableNames: ''
	  poolDictionaries: ''
	  category: 'Streams-Generators'
!

Generator comment:
'A Generator object provides a way to define a Generator method: such
a method does not return a single value, instead it returns an object
(the Generator) that uses a Stream interface to access multiple
return values.  The return values are computed one at a time, as
needed, and hence need not even be finite.

A generator methods starts by creating a Generator object with
"Generator new" and saving it into a temporary variable.  As soon as
this is executed, even though it is not apparent, the method exits
returning the newly created Generator.  As soon as a message like
#next, #peek, #atEnd or #peekFor: is sent to the generator, execution
of the method that created it resumes and goes on until the
generator''s #yield: method is called: then the argument of #yield:
will be the Generator''s next element.  If the generator method goes
on to the end without calling #yield:, the Generator will produce no
more elements and #atEnd will return true.

Alternatively, a generator block can be converted to a Generator with
"Generator on: [...]".  The Generator itself is passed to the block
and, again, the block starts its execution when a message like
#next, #peek, #atEnd or #peekFor: is sent to the generator.  Again,
the block''s execution is temporarily suspended when the
generator''s #yield: method is called.

Returning a value from the generator method makes no sense at least
after "Generator new" is invoked.  Before, you can use it to return a
different kind of Stream, or nil, or whatever else; after, the value
returned will not matter and the return will put an end to the
Generator''s production of elements.

You could achieve the effect of generators manually by writing your
own class and storing all the local variables of the generator as
instance variables.  For example, returning a list of integers could
be done by setting a variable to 0, and having the #next method
increment it and return it.  However, for a moderately complicated
generator, writing a corresponding class would be much messier (and
might lead to code duplication or inefficiency if you want to support
#peek, #peekFor: and/or #atEnd): in general, providing a #do:-like
interface is easy, but not providing a Stream-like one (think binary
trees).

The idea of generators comes from other programming languages, in
particular this interface looks much like Scheme streams and Python
generators.  But Python in turn mutuated the idea for example from
Icon, where the idea of generators is central.  In Icon, every
expression and function call behaves like a generator, and if a
statement manages scalars, it automatically uses up all the results
that the corresponding generator provides; on the other hand, Icon
does not represent generators as first-class objects like Python and
Smalltalk do.'!

!Generator class methodsFor: 'instance creation'!

new
    "Return a generator, and also suspend the execution of the
     sender by returning the new generator to the method that
     invoked the sender.  More easily seen by looking at an
     example:

     Integer>>evenNumbersUpTo: n
         | gen |
         gen := Generator new.
         self to: n do: [ :each |
             each even ifTrue: [ gen yield: each ]
         ]

     Although there is no return statement in the method, evaluating
     it returns a Generator for the even numbers between the receiver
     and the argument."
    ^super new
	context: thisContext parentContext
!

on: aBlock
    | gen |
    gen := self new.
    aBlock value: gen.
! !

!Generator methodsFor: 'stream protocol'!

atEnd
    "Answer whether more data can be generated."
    atEnd isNil ifTrue: [ self generateNext ].
    ^atEnd
!

next
    "Evaluate the generator until it generates the next value or
     decides that nothing else can be generated."
    | result |
    self atEnd ifTrue: [ ^self pastEnd ].
    atEnd := nil.
    result := next.
    next := nil.
    ^result
!

peek
    "Evaluate the generator until it generates the next value or
     decides that nothing else can be generated, and save the value
     so that #peek or #next will return it again."
    self atEnd ifTrue: [ ^nil ].
    ^next
!

peekFor: anObject
    "Evaluate the generator until it generates the next value or
     decides that nothing else can be generated, and if it is not equal
     to anObject, save the value so that #peek or #next will return it
     again."
    self atEnd ifTrue: [ self pastEnd. ^false ].
    ^next = anObject
        ifTrue: [ next := nil. atEnd := nil. true ]
        ifFalse: [ false ]
! !

!Generator methodsFor: 'private - continuations'!

context: aContext
    "Initialize the state of the generator.  Its execution will
     resume from the context, aContext.  Then return the generator
     itself to the sender of aContext: this method is called by
     Generator class>>#new, and this has the side effect of
     returning the Generator from the sender."
    contexts := OrderedCollection new.
    topContext := bottomContext := aContext.
    bottomContext parentContext continue: self
!

yield: anObject
    "Save the object returned by the continuation in the next
     instance variable, then save the execution state of the
     continuation in topContext, bottomContext and contexts.
     This is because resuming execution in #invokeGenerator
     will cause returned blocks to be marked as non-returnable,
     and we want to preserve the chain."
    next := anObject.
    atEnd := false.
    topContext := bottomContext := thisContext parentContext.
    [
	contexts addLast: bottomContext.
	bottomContext parentContext == suspendedContext
    ] whileFalse: [
	bottomContext := bottomContext parentContext.
    ].
    suspendedContext continue: self.
!

generateNext
    "Invoke the continuation via #invokeGenerator,
     then resume execution when #yield: is invoked.  Then,
     use the information in the contexts instance variable
     to reconstruct the continuation's chain of contexts."
    | ctx |
    self invokeGenerator.
    ctx := topContext.
    [ contexts isEmpty ] whileFalse: [
	ctx parentContext: contexts removeFirst.
	ctx := ctx parentContext.
    ].
!

invokeGenerator
    "This swizzles the contexts, inserting the execution state of
     the continuation between the #invokeGenerator context and
     the #generateNext context, then starts evaluating the code
     in the continuation."

    atEnd := true.
    suspendedContext := thisContext parentContext.
    bottomContext parentContext: suspendedContext.
    thisContext parentContext: topContext.
! !

!Integer methodsFor: 'examples of generators'!

generatorForGeneratorExample
    | gen |
    gen := Generator new.
    'Entering gen' displayNl.
    1 to: self do: [ :each |
	('Yielding ', each printString, '... ') display.
	gen yield: each.
	'Resuming gen' displayNl
    ]!

generatorBlockExample
    ^Generator on: [ :gen |
        'Entering gen' displayNl.
        1 to: self do: [ :each |
	    ('Yielding ', each printString, '... ') display.
	    gen yield: each.
	    'Resuming gen' displayNl
        ] ]!

generatorExample: gen
    | n |
    ('Running on ', gen printString) displayNl.
    [
	'Calling next... ' display.
	n := gen next.
	n notNil
    ] whileTrue: [
	('Got ', n printString) displayNl
    ]! !

10 generatorExample: 10 generatorForGeneratorExample!
Eval [
    Smalltalk byteCodeCounter printNl.
    10 generatorExample: 10 generatorBlockExample.
    Smalltalk byteCodeCounter printNl
]
