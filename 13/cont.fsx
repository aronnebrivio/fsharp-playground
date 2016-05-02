(* Stack and heaps: slides *)

// a inefficient way to build a list of ones
let rec bigList n =
  if n=0 then [] else 1::bigList(n-1);;

let ok =    bigList 200000;;
//let notok = bigList 200000 * 10;;

// The stack is big, the heap bigger; building on the heap, iteratively
let rec bigListA n xs =
  if n=0 then xs  else bigListA (n-1) (1::xs);;

let bok =    bigListA (200000 * 10) [];;


(* We have seen how using accumulators some functions become iterative
and can be compiled into a loop.

So far so good.

The bad news is that adding an accumulator does not alwas turn a
recursive function into an iterative one.

Take the case of binary trees

*)
#r "FsCheck"
open FsCheck


type BinTree<'a> = | Leaf
                   | Node of BinTree<'a> * 'a *  BinTree<'a>;;

// the usual recursive function counting nodes
                   
let rec count = function
    | Leaf          -> 0
    | Node(tl,_,tr) -> count tl + count tr + 1;;

// let's put an accumulator in:
    
let countA bt =
  let rec cA bt acc =
    match bt with
      | Leaf -> acc
      | Node(l,_,r) -> cA l (cA r (1 + acc))
  cA bt 0


// they are the "same" function
let prop_count (t : int BinTree) =
  count t = countA t

do Check.Quick prop_count

(* but countA is *not* tail recursive according to the definition
above, because you need to recur on the left **and** on the right.

So, stack-wise, we're kind of screwed.

BTW, if you think you can fix this using **two** accumulators, be my guest.

==> What we can do is to use **continuations**.

This is a very general technique that adds a function as an extra
argument to the given procedure, which describes "the rest of the
computation to be performed"

Continuations are very useful in making the flow of control explicit
and thus manipulate the said flow, as we will see later.

In the tail recursion case, suppose we have a function

  f : t1 -> t2

We add an extra argument

  k : t2 → t2

This argument k represents the remaining computation that should be
performed when (f v) terminates. Such function k is called a
continuation. So, the continuation based function has type

fC : t1 -> (t2 -> t2) -> t2.


The basic property is that

       (f v) = (fC v id)
       
where id is the identity function fun x -> x 
-- or more in general something to be done exiting the computation

It's a general result that any function can be translated
(CPS-converted) to a tail recursive one. However, no free lunch: what
you save in stack space, you pay on the heap allocating closures.
*)

let id x = x

// the factorial, continuation style, where the CPS function is local:

let factC n =
  let rec fc n  k =
    if n = 0 then k 1
       else  fc (n - 1) (fun res -> k (n * res))
  fc n id

(* Let's trace it:

fc 3 id =

  fc 2 (fun x -> id (3 * x)) =     // note: under the fun, id is **not** reduced
    
  fc 1 (fun y -> (fun x -> id (3 * x)) (2 * y)) =

  fc 0 (fun z ->   (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * z)) = // base case k 1

  (fun z ->   (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * z)) 1 = // and now just function application

  (fun y -> (fun x -> id (3 * x)) (2 * y)) (1 * 1) =

  (fun x -> id (3 * x)) (2 * 1 * 1)) =

  id (3 * 2 * 1 *1) = 6
   *)

// It's intersting to compare wrt #time, fact, factA and
// factC. Basically factA is faster, but factC may handle bigger data
// structures (not here, it's just numbers)

let factA n =
  let rec loop acc n =
    match n with 
    0 -> acc
    | m -> loop (m * acc) (m - 1)
  loop 1 n

let bg = List.init 100000 (fun _ -> 15)
    
#time

let fc = List.map factC bg

let fa = List.map factA bg

// an example with an initial continuation that does more than id

let print n = printfn "the result is %d: " n

let factCp n =
  let rec fc n  k =
    if n = 0 then k 1
       else  fc (n - 1) (fun res -> k (n * res))
  fc n print

let ff = factCp 10


// CPS on lists: biglist 

let rec bigListC n k =
    if n=0 then k []
    else bigListC (n-1) (fun res -> k (1::res));;

// less efficient than the accumulator one

let bl = bigListC 100000 id

// another example: append. It's efficient, but not tail recursive. Let's do it CPS

let appC xs ys =
  let rec aC xs ys k =
    match xs with
      | [] -> k ys
      | x :: xs' -> aC xs' ys (fun r ->  k (x :: r ))
  aC xs ys id

(* traccia

appC ([1,2], [3,4]) =
=> aC ([1,2], [3,4], id)
=> aC ([2], [3,4], fun r1 -> id (1::r1))
=> aC ([], [3,4], fun r2 -> (fun r1 -> id (1::r1)) (2::r2))
=> (fun r2 -> (fun r1 -> id (1::r1)) (2::r2)) [3,4]
=> (fun r1 -> id (1::r1)) (2::[3,4])
=> (fun r1 -> id (1::r1)) [2,3,4]
=> id (1::[2,3,4])
=> id [1,2,3,4]
=> [1,2,3,4]

How would you do this with an accumulator (w/o reversing, of course) ?
 *)  

// Back to our tree count: let's do an iterative-CPS version:
// This is tricky!

let rec countC t k =
  match t with
  | Leaf          -> k 0
  | Node(tl,n,tr) ->
      countC tl (fun vl -> countC tr (fun vr -> k(vl+vr+1)));;

let t = (Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))) 
let nn = countC (Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))) id;;

(*
Note that both calls of countC are tail calls, and so are the calls of the continuation c,
and the stack will therefore not grow due to the evaluation of countC and the associated
continuations.
*)
let prop_countC (t : int BinTree) =
  countC t id = countA t

do Check.Quick prop_countC


// Let's go back to the circuit example:

type Circuit<'a> = | Comp of 'a
                   | Ser  of Circuit<'a> * Circuit<'a>
                   | Par  of Circuit<'a> * Circuit<'a>;;

let cmp = Ser(Par(Comp 0.25, Comp 1.0), Comp 1.5);;

// cunting components: first the standard recursive one

let rec countCI = function
    | Comp _     -> 1
    | Ser(c1,c2) -> countCI c1 + countCI c2
    | Par(c1,c2) -> countCI c1 + countCI c2;;

// foldback on circuits, not tail recursive
// c:('a -> 'b) * s:('b -> 'b -> 'b) * p:('b -> 'b -> 'b) -> Circuit<'a> -> 'b
let rec circRec (c,s,p) = function
    | Comp x     -> c x
    | Ser(c1,c2) ->
          s (circRec (c,s,p) c1) (circRec (c,s,p) c2)
    | Par(c1,c2) ->
          p (circRec (c,s,p) c1) (circRec (c,s,p) c2);;

let count1 circ = circRec((fun _ -> 1), (+), (+)) circ;;

// CPS version
let countcik c =
    let rec countCIK  c k = 
        match c with
        | Comp _          -> k 1
        | Ser(c1,c2) | Par(c1,c2) ->
            countCIK c1 (fun vl -> countCIK c2 (fun vr -> (vl+vr) |> k))
    countCIK c id


let prop_countCI (c : int Circuit) =
  countcik c  = count1 c

do Check.Quick prop_countCI

// changing the control flow

(*

Consider a function that traverses a list and returns the shortest
prefix of the list such that a predicate p is false on the next
element .  If p holds on all elements, we return None to indicate no
such prefix exists.

let ev = prefix (fun x -> x % 2 = 0) [2;4;5;8]

==>   Some [2; 4]

let no = prefix (fun x -> x % 2 = 0) [2;4;2;8]

==> None

let so = prefix (fun x -> x % 2 = 0) [1;2;4;5;8]

==> Some []
   *)


let consO y = function
  None -> None
  | Some ys -> Some (y :: ys)

let rec prefix p xs =
  match xs with
    | [] -> None
    | y :: ys ->
      if p y then consO y (prefix p ys)  else (Some  [])

let ev = prefix (fun x -> x % 2 = 0) [2;4;5;8]
let no = prefix (fun x -> x % 2 = 0) [2;4;2;8]
let so = prefix (fun x -> x % 2 = 0) [1;2;4;5;8]

(*

Note an inefficiency in this function: if p is is true for every
element in the list, consO is called on every element after the end
of the list has been reached, passing along None all the way up to be
returned as the overall result.

ex:  prefix (fun x -> x % 2 = 0) [2;4;2;8]

==> consO 2 (cons0 4 (cons0 2 (cons0 8 None)))) ==> None

Instead, we would like to return None immediately once we have reached
the end of the list and found no element on which p is false.

We do this CPS style, where:

- we use as initial continuation a variant of id, i.e.  (fun r -> Some r)

- in the “success” case (we find an element where p is false)
we call the continuation cont on the expected answer ( [] in this
program).

- In the case of a “failure” (we do not find a place where p is false)
we discard the continuation and return directly (with None in this
program)

- 
  *)


let prefixC p xs =
  let rec pC xs k =
    match xs with
    | [] -> None
    | y :: ys ->
      if p y then pC  ys (fun r -> k (y ::r))  else (k  [])
  pC xs (fun r -> Some r)

let ex = prefixC (fun x -> x = 1) [1..10]


(* Another crucial use of continuations is to implement backtracking,
to be seen in another lecture.

Another way to alter the flow of control is using exceptions, as we show next
    *)



