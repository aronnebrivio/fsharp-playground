(* COMPUTING WITH RELATIONS (via SETS)

We see a relation from a set A to a set B as a subset of A * B. 
A relation r is called finite if it is a finite subset
of A * B. Assuming that the sets A and B are represented by F# types
’a and ’b we can represent a finite relation r by a value of type
Set<’a * ’b>.


i. Give as a **concrete** example the "less" relation over the set
{1,2,3}. Do not write the function, just list the elements as a set.

ii. The *domain* dom r of a relation r from A in B is the set of elements a in A
where there exists an element b in B such that (a, b) ∈ r. Write an F#
declaration expressing the domain function.  

The *range*  r of a relation r is the set of elements b in B where
there exists an element a in A such that (a, b) ∈ r. Write an F#
declaration expressing the range function.

iii. The identity relation on a set S is the relation associating every s ∈ S 
to itself.  Write an F# declaration computing the identity relation from a given S.

iv. A relation r from a set A to the same set is said to be symmetric
if (a1 , a2 ) ∈ r implies (a2 , a1 ) ∈ r for any elements a1 and a2 in A.
The symmetric closure of a relation r is the smallest symmetric
relation containing r. Declare an F# function to compute the symmetric
closure.

v. If r is a finite relation from A to B and a is an element of A , then the application of r to a ,
apply r a, is the set of elements b in B such that (a, b) ∈ r . Write an F# declaration expressing
the apply function.

   *)

// just a type abbreviation

type rel<'a,'b when 'a: comparison and 'b : comparison> = Set<'a * 'b>;;


// i
let lessrel = Set.ofList [(1,2); (1,3) ; (2 , 3)];;

// ii
let dom (R : rel<'a,'b>) = Set.map (fun (x,y) -> x) R;;

let t = dom lessrel ;;

let range R = Set.map (fun (x,y) -> y) R;;

let t1 = range lessrel ;;

// iii

let idRel s = Set.map (fun x -> (x,x)) s

let sid = Set.ofList  ['a'..'f'] |> idRel ;;

// iv

let sym R = Set.map (fun (x,y) -> (y,x)) R;;

let t2 = sym lessrel ;;

let symclosure R = sym R |> Set.union R ;;

let t3 = symclosure lessrel;;

// v
let apply R a = Set.filter (fun (x,y) -> x = a) R |> range

let ra1 = apply lessrel 1
let ra2 = apply lessrel 2
let ra3 = apply lessrel 3


// membership
let mem c R = Set.exists (fun d -> c = d) R

let r1 = mem (1,2) lessrel
let r2 = mem (2,1) lessrel

(*
   5. The relation composition r ◦◦ s of a relation r from a set A to a set B and a relation s from
B to a set C is a relation from A to C . It is defined as the set of pairs (a, c) where there exist
an element b in B such that (a, b) ∈ r and (b, c) ∈ s . Declare an F# function to compute the
relational composition.
6. A relation r from a set A to the same set A is said to be transitive if (a 1 , a 2 ) ∈ r and
(a 2 , a 3 ) ∈ r implies (a 1 , a 3 ) ∈ r for any elements a 1 , a 2 and a 3 in A . The transitive closure
of a relation r is the smallest transitive relation containing r . If r contains n elements, then
the transitive closure can be computed as the union of the following n relations:
r ∪ (r ◦◦ r) ∪ (r ◦◦ r ◦◦ r) ∪ · · · ∪ (r ◦◦ r

                                        *)
