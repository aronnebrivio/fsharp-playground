#r "FsCheck"
open FsCheck
open System


(* --------------------------------
   Configurations
   --------------------------------
   *)


// Can we trust FsCheck?

let prop_isSmallerThan80 x = 
    x < 80

do Check.Quick prop_isSmallerThan80

// What about increasing the number of tests? We do this by changing
// the default ("Quick") configuration. There is a field called
// MaxTest that we can set.

let record = Config.Quick

let config = {
    Config.Quick with 
        MaxTest = 500
    }


// note, you need to call Check.One

do Check.One(config,prop_isSmallerThan80 )
// got it

// Things is,  the ints are not generated uniformly, but clustered around zero. 

(*
   In, fact generators start with small numbers and gradually
increase them. This is controlled by the StartSize and EndSize
settings. By default, StartSize is 1 and EndSize is 100. So at the end
of the test, the "size" parameter to the generator will be 100.  *)
  
let config2 = {
    Config.Quick with
        MaxTest = 100
        StartSize = 40
        EndSize = 500
    }

do Check.One(config2,prop_isSmallerThan80 )

(*
Other fields:

- Replay allows to use the same seed, if you want to replay a run
    Replay = Random.StdGen (995282583,295941602) |> Some 

- Every and EveryShrink controls the Verbose output and shrinking
   *)


(* Truth to be told, changing the configuration will not help much --
most of times we need custom-made generators. *)


(* --------------------------------
   Generators in properties
   --------------------------------
   *)



// Let's revisit one of our fav examples

let rec insert (x, xs) = 
    match xs with
    | [] -> [x]
    | c::_ when x <= c -> x::xs 
    | c::cs ->  c::insert (x, cs)
    
let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y ::ys)

let prop_insert (x:int, xs) =
  ordered xs ==> ordered (insert (x, xs))

do Check.Quick prop_insert

// MONITORING

// Arguments exhausted after *** tests.

(* It is important to be aware of the distribution of test cases: if
   test data is not well distributed then conclusions drawn from the
   test results may be invalid. In particular, the ==> operator can
   skew the distribution of test data badly, since only test data
   which satisfies the given condition is used.

   FsCheck offers some combinator to observe the distribution. Just
put them in the test itself *)

// Counting Trivial Cases: how many tests just consider lists that are empty or singleton?
//  Prop.trivial : bool -> 'a -> Property

let insertTrivial (x:int) xs = 
  ordered xs ==> ordered (insert (x ,xs))
  |> Prop.trivial (List.length xs <= 1)

do Check.Quick insertTrivial

// Can we actually see what's happening?
//Collecting Data Values:

let insertCollect (x:int) xs = 
  ordered xs ==> (ordered (insert (x ,xs)))
      |> Prop.collect (List.length xs)
do Check.Quick insertCollect

// Pretty lousy, innit?

// Another example
let rec remove x = function
    | [] -> []
    | y::ys -> if x=y then remove x ys else y :: (remove x ys)

let rec removeDup xs = 
    match xs with
    | [] -> []
    | y::ys -> y::(removeDup (remove y ys))


// let's see if removeDup does anything at all

let cnt x xs = List.foldBack (fun  h t -> if x = h then 1 + h else t) xs 0

let rec has_dupl = function
      [] -> false
      | x :: xs -> cnt x xs > 0 || has_dupl xs
      

let prop_remord_tr (xs : int list) =
  ordered xs ==> ordered (removeDup xs )
    |> Prop.trivial (not (has_dupl xs))
    
do Check.Quick prop_remord_tr

(*
There are two (inter-related) issues here:

1. This checks are conditional and have **hard-to-satisfy** premises

  ordered xs ==> P xs

  the likelihood that a list of 3 elements is ordered is 33%, hence
    the reply "Arguments exhausted after 55 tests", meaning that we have reached
    the MaxFail threshold while producing 55 test that survive

2. Of the instances that survive the precondition, it is no suprise that they
have very short length and are not that interesting.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 How to make random testing more effective is a hot research topic in
 FP testing. One can

- drop random testing and shift to *exhaustive* testing, or even
  symbolic execution ("narrowing")
    ===> (Lazy)SmallCheck

- Use logic programming techique to improve test generation

   ===> SparseCheck, smart generators in Isabelle/HOL

- Find "deeper" enumeration techniques

 ===> Feat

 Here we just follow the QuickCheck approach, that is, we explain how
 to write **custom made** generators

 1. In particular we can write generators so that we can quantify only over certain specific domains

 2. We can change probability distribution to suit our needs
   *)



// But wait!  naive fix: increase the number of tests that can fail

let config3 = {
    Config.Quick with 
        MaxFail = 1000
    }


do Check.One(config3, prop_remord_tr)

// it takes forever and coverage does not gen any better


(* It's better to write a custom generator.

FsCheck gives you generator for primitive types and then lift them up
to discriminated unions. All these combinators live in the modules Gen
and Arb, where the latter package up generators with shrinkers (more later).

Bottomline: use Gen for generating random data, Arb for generators to
go into properties (with shrinkers)

There are many ways to write generators and we'll stick to the minimum

*)


// Arb.generate<'a> returns the generator of the registered Arbitrary
// instance for the given type 'a. This is type driven and easy to use

let intGenerator = Arb.generate<int>  

// To see what's going on we use Gen.sample. 
// generate three ints with a maximum size of 100
let js = Gen.sample 100 3 intGenerator 

// Then you can lift it to compound types, and this is what FsCheck does under the hood

// generate 10 int lists with a maximum size of 5
let intListGenerator = Arb.generate<int list>

let iss = Gen.sample 5 10 intListGenerator 

// The best thing is that the generator will work with your own user-defined types too!

type Color = Red | Green of int | Blue of bool

let colorGenerator = Arb.generate<Color>

// generate 10 colors with a maximum size of 50 (for ints)
let cs = Gen.sample 50 10 colorGenerator 


(* Now, we can use some combinators to filter what we want from
generators. 

Arb.from<'a> Get the Arbitrary instance for the given type. Like Arb.generate, but yielding a Arb not a Gen

Arb.filter : (('a -> bool) -> Arbitrary<'a> -> Arbitrary<'a>)

   filters the generator and shrinker for a given Arbitrary instance
   to contain only those values that match with the given filter function


Arb.mapFilter : (('a -> 'a) -> ('a -> bool) -> Arbitrary<'a> -> Arbitrary<'a>)

   Arb.mapFilter maps the generator and filter the shrinkers for a
   given Arbitrary instance. Mapping the generator is sometimes
   faster than to filter

   *)


  
let orderedArb =
  Arb.filter ordered Arb.from<int list>

// let's look at this

let look arb = 
    arb |> Arb.toGen |> Gen.sample 500 10 

let os = look orderedArb
   
// OK, we could do better (less empties)

let orderedNEArb =
  Arb.filter (fun xs -> ordered xs &&  List.isEmpty xs |> not) Arb.from<int list>

let eos = look orderedNEArb


// the one from the book
let orderedMapArb =
  Arb.mapFilter List.sort ordered Arb.from<list<int>>

let bs = look orderedMapArb

// Now, how can we say to QC to use this generator, rather than the standard one?

// use Prop.forAll :  (Arbitrary<'a> -> ('a -> 'b) -> Property) 
// Quantified property combinator. Provide a custom test data generator to a property.
// back to conditional properties : forall xs : ordered Px as:


let prop_insertWithArb arb (x : int) =
  Prop.forAll arb (fun xs -> ordered(insert (x, xs)))
         
do Check.Quick <| prop_insertWithArb orderedArb
do Check.Quick <| prop_insertWithArb orderedNEArb

// note: no test exhaustion. Now let's see coverage
let prop_insertWithArbCollect arb x =
  Prop.forAll arb (fun xs -> ordered(insert (x, xs)) |> Prop.collect (List.length xs))
do Check.Quick (prop_insertWithArbCollect orderedNEArb)

// this give the best distribution
do Check.Quick (prop_insertWithArbCollect orderedMapArb)
// not too shabby


// other examples


(*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Controlling data distribution:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Sometimes we want to have generators with a *bias*, that is, we give
to some constructor a higher likelihood to be generated. E.g. in a
tree we want more nodes than leaves. In an arithmetic expression more
variables than constants.

We can control the distribution of results using the *frequency*
combinator. frequency chooses a generator from the list randomly, but
weighs the probability of choosing each alternative by the factor
given.

*)
let boolGen = Arb.generate<bool>  
let bools = Gen.sample 1 15 boolGen

let prop_bool (p : bool) q =
  not (p && q) = not p || not q

// do Check.Verbose prop_bool
(*
- Gen.oneof: (seq<Gen<'a>> -> Gen<'a>)
   pick one Gen in a sequence with equal probability

- Gen.frequency : seq<int * Gen<'a>> -> Gen<'a>
   Build a generator that generates a value from one of the generators
in the given non-empty seq, with given probabilities.  *)

let genBoolmoreTrueGen =
  Gen.frequency [ (4, Arb.generate<bool>); (1, Arb.generate<bool>)]
let bst = Gen.sample 1 15 genBoolmoreTrueGen

(* Suppose now that we want to use the new generator from now on. We
can *register* it and therefore override the default one
*)

type MyGenerators =
    static member  genBoolmoreTrueGen() =
        {new Arbitrary<bool>() with
            override x.Generator =  genBoolmoreTrueGen
            override x.Shrinker t = Seq.empty }

let _ = Arb.register<MyGenerators>()

do Check.Verbose prop_bool


// something more interesting: generators for recusive data types
type Tree = Leaf of int | Branch of Tree * Tree

// do not call it !! It does not terminate
let rec unsafeTree() = 
  Gen.oneof [ Gen.map Leaf Arb.generate<int> 
              Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

// 
let treeGen =
    let rec tree' s =  // s:int -> Gen<Tree>
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

Gen.sample 10 5 treeGen

// More bias to Branches

let treeMoreBGen =
    let rec tree' s =  // s:int -> Gen<Tree>
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.frequency [ (1, Gen.map Leaf Arb.generate<int>) 
                            (4,  Gen.map2 (fun x y -> Branch (x,y)) subtree subtree)]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

Gen.sample 10 5 treeMoreBGen


// --------------------------------------------
// Other examples that can be skipped:
// GOTO PATTERNS


// Let's try a generator for positive integeres using mapFilter, by
// mapping the absolute value abs : int -> int

let posInt =
  Arb.mapFilter abs (fun x -> x > 0) Arb.from<int>

// I coud have done it by filtering, but it's slower


// let's use it on two definitions of the factorial function, one
// continuation based the other accumulator based
  
let factC n =
  let rec fc n  k =
    if n = 0 then (k 1)
       else  fc (n - 1) (fun res -> k (n * res))
  fc n id

let factA n =
  let rec fc n  a =
    if n = 0 then a
       else  fc (n - 1) (n *a)
  fc n 1


// with filtered gen
let prop_facQ =
  Prop.forAll  posInt (fun n ->
                       let fa = factA n
                       let fc = factC n
                       fa = fc |> Prop.trivial (fc = 0)) 
do Check.Quick  prop_facQ

// EXERCISE: fix the above generator so that it avoids overflow to 0
// either using bigint or restricting the range of n (on my machine) or changing config or ...



// gen for logic

type logic = Var of string | K of int | Not of logic | And of logic * logic

// standard generator

let logGenerator = Arb.generate<logic>

Gen.sample 10 8 logGenerator

// custom generator with more weights to complex formulae



let log =
    let rec log' s = 
        match s with
        | 0 -> Gen.map Var Arb.generate<string>
        | n when n>0 -> 
            let subtree = log' (n/2)
            Gen.frequency [(1,Gen.map Var Arb.generate<string>);
                           (4,Gen.map K Arb.generate<int>);
                           (6,Gen.map Not subtree);
                           (8,Gen.map2 (fun x y -> And (x,y)) subtree subtree)
                            ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized log'


Gen.sample 10 5 log
Gen.sample 10 5 logGenerator

// -------------------------------


// PBT PATTERNS: slides first!

(* ======================================================
Different paths, same destination
====================================================== *)

let add x  y = x + y

// First a weak spec: note that we pass the sorting function as an argument


let ``+1 then sort should be same as sort then +1`` sortFn xs = 
    
    let result1 = xs |> sortFn |> List.map (add 1)
    let result2 = xs |> List.map (add 1) |> sortFn 
    result1 = result2

// good  test    
do Check.Quick (``+1 then sort should be same as sort then +1``  List.sort)
// bad test
do Check.Quick (``+1 then sort should be same as sort then +1``  id)

    
// a spec which actually uses the sortedness
let ``append minValue then sort should be same as sort then prepend minValue`` sortFn xs = 
    let minValue = Int32.MinValue
   
    let appendThenSort = (xs @ [minValue]) |> sortFn 
    let sortThenPrepend = minValue :: (sortFn xs)
    appendThenSort = sortThenPrepend 

// test
Check.Quick (``append minValue then sort should be same as sort then prepend minValue`` List.sort)

Check.Quick (``append minValue then sort should be same as sort then prepend minValue`` id)
// bad implementation fails

(* ======================================================
"There and back again"
====================================================== *)

(* The first example often mentioned is the "hello world!" of PBT:

rev (rev xs) =   xs.

Note that's not typical as reverse is its own inverse. It's more an
involution, see afterwards

*)

// an example with char (fu)
let ``upper and lower a char is a char``  c =
   ((Char.ToUpper c |> Char.ToLower) = c)

do Check.Quick ``upper and lower a char is a char`` 

// note that chars like '2' are both upper and lower
let ``upper and lower a char is a char2``  c =
  ((Char.ToUpper c |> Char.ToLower) = c) || ((Char.ToLower c |> Char.ToUpper) = c)

do Check.Quick ``upper and lower a char is a char2`` 




/// Converts a list of characters into a string.
let implode (xs:char list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()


/// To explode a string into a list of characters, we see a string as a sequence of char
let ``explode and implode are inverse``  cs =
  implode cs |> Seq.toList = cs 

do Check.Quick  ``explode and implode are inverse``

let ``implode and explode are inverse``  s =
   Seq.toList s |> implode = s 

do Check.Quick ``implode and explode are inverse``


// add/contains

let ``if i add to a set, it's there`` (x: int) ss  =
  (Set.union (Set.singleton x) ss) |>  Set.contains x

do Check.Quick  ``if i add to a set, it's there``;;



(* ======================================================
"Some things never change"
====================================================== *)

let ``sorting preserves elements`` (xs: int list)=
  set xs = set (List.sort xs)

do Check.Quick  ``sorting preserves elements``

let ``map preserves len`` f (xs: int list) =
  List.length xs =  (List.map f xs |> List.length)

// here FSCheck invents a function
do Check.Check    ``map preserves len``

// here I pass one myself
do Check.Quick    (``map preserves len`` abs)


(* ======================================================
The more things change, the more they stay the same"
====================================================== *)

let ``sort idempotent`` (xs : int list) =
  List.sort (List.sort xs) = List.sort xs

do Check.Quick  ``sort idempotent``

let  ``toupper idempotent`` c =
  (Char.ToUpper c |> Char.ToUpper) = (Char.ToUpper c);;

do Check.Quick  ``toupper idempotent``


(* Other examples that can be skpped: *)


// Different paths, same destination

let ``append an int then reverse should be same as reverse then prepend same value`` revFn (n : int) xs = 
  (xs @ [n]) |> revFn =  n :: (revFn xs)

// test

Check.Quick (``append an int then reverse should be same as reverse then prepend same value``  List.rev)

// 
Check.Quick (``append an int then reverse should be same as reverse then prepend same value`` (fun _ -> []))


// 2 There and back again
let subtract x y = x - y

let ``+n then -n to m should be m`` m (n: int)   =
  let sum = add m n
  let subt = subtract sum n
  subt  =  m

do Check.Quick ``+n then -n to m should be m``





