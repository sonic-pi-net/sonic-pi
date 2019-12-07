structure C = struct
   val a = 12
   fun f x = x + 5
end

(*(*(*(*(* This file is all pretty strange Standard ML *)*)*)*) (**)*)
(* Robert J. Simmons *)

(* Comments (* can be nested *) *)
structure S = struct
  val x = (1, 2, "three")
end

structure Sv = struct
  (* These look good *)
  val x = (1, 2, "three")
  val z = #2 x

  (* Although these look bad (not all the numbers are constants),       *
   * they never occur in practice, as they are equivalent to the above. *)
  val x = {1 = 1, 3 = "three", 2 = 2}
  val z = #
            2 x

  val || = 12
end

signature S = sig end

structure S = struct
  val x = (1, 2, "three")
  datatype 'a t = T of 'a
       and u = U of v * v
  withtype v = {left: int t, right: int t}
  exception E1 of int and E2
  fun 'a id (x: 'a) : 'a = x

  val 
      'a id = fn (x : 'a) => x
end

signature R = sig
  type t
  val x : t
  val f : t * int -> int
end
structure R : R = struct
  datatype t = T of int
  val x : t = T 0
  fun f (T x, i : int) : int = x + i
  fun 'a id (x: 'a) : 'a = x
end

signature BA_Z = sig 
   val s: int
   include S R
end 

structure b______ = struct (* What (* A * strange * name *) for ) a ( struct *)

val !%&$#+-/:<=>?@\~`^|* = 3

type struct' = int list
and 'a sig' = 'a list
and ('a, 'b) end' = 'b option * 'a list

structure baz = struct
  structure Bar = struct 
    val foo = !%&$#+-/:<=>?@\~`^|*
  end  
end

infixr +!+ 
fun (a +!+ b) = (op +) (a, b)

open baz S R

val$$$ = fn x => fn y => fn z => fn w => w
val (foo, ++, bar, ||) = (4, baz.Bar.foo, !%&$#+-/:<=>?@\~`^|*, Bar.foo)
val _ = $$$foo++bar||

val val'ue : ' list = []
val struct3 : (' -> ') = fn x => x
val end_struct_' : ('a -> 'a) = fn x => x
val x : (''a -> ''a) = fn x => x
val x : ('''' -> '''') = fn x => x
val x : unit = print "Weird, huh?\n"
val w = {x=1,y=2,##= =3,4=3}
val {##=, x, 4=a,...} = w
val z = #4 w
val z = # ##= w

fun f x y 0 = 4 
  | f x y z = 4 + Sv.||

exception Foo of int
datatype ('0, 'b, '_, ') f'o'o = Bar | baZ12' | dsfa_fad | #@$ | Bug
and (', ''', '''', ''''') bar = 
   Bee of unit
 | Ben of (', ''', '''', ''''') f'o'o * int
 | X of ''' list

fun q x = raise Foo x
and h x = raise Foo (~x)

val x = 4
and y = 5

fun q 0 = 4
  | q 1 = (case 1 of 1 => 2 | 3 => 4 | x => y)
  | q y = case y of 1 => 2 | 3 => 4 | x => y

val x = ref true
fun q 0 = 4
  | q 1 = if false then case 1 of 1 => 2 | 3 => 4 | x => y else 19
  | q 2 = (while !x handle Match => !x | Fail _ => !x do () ; 2)
  | q x = (raise Match) handle Domain => 9 | Match => 3

fun p 0 = 12
  | p 1 = 8
  | p 2 = r false
  | p x = r true
and r true = 19
  | r false = 12

val _ = 123
val _ = 0001
val _ = ~123
val _ = ~0001
val _ = 0w12412
val _ = 0w12412
val _ = 0xfA0
val _ = ~0xfA0
val _ = 0wxfA0
val _ = 1.4
val _ = ~1.4
val _ = 1e~2
val _ = 1E~2
val _ = 1e2
val _ = 1E2
val _ = 1.4e~2
val _ = 1.4E~2
val _ = 1.4e2
val _ = 1.4E2

val c = #"\000"
val st = "foo \
 	 \ bar" ^ "baz \        
  	 \ and \ 
   	 \ such\n"

val () = print st

val _ = foo::bar::4::[++]

end
