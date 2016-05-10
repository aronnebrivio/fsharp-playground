#r "FsCheck"
open FsCheck
open System

(*
1. 
	Sets can be seen as lists without repetitions. Validate the set
	version of the ordered/insert property, namely

	"if ss is an ordered set then inserting x in ss is still ordered"

	You need to define a generator such that it only emits ordered lists
	w/o repetitions. Then quantify over such list.

	Hint: use mapFilter with the remove function as seen before

	(Again: use lists, not Sets)

	Finally, redefine you generator so that lists are not only ordered and
	w/o repetitions, but  also have length between n and m with m > n.
*)
let rec ordered xs = 
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys ->  (x <= y) && ordered (y::ys);;

let rec noRep xs =
    match xs with
    | [] -> true
    | [x] -> true
    | x::y::ys -> (x <> y) && noRep (y::ys);;

let rec insert a ls =
    match ls with
    | [] -> [a]
    | x::xs -> if a>x then x::insert a xs
               else a::ls;;

let orderedNoRep xs = noRep xs && ordered xs;;

let orderedNoRepArb = Arb.mapFilter List.sort orderedNoRep Arb.from<list<int>>;;

//test
orderedNoRepArb |> Arb.toGen |> Gen.sample 500 20;;

let ``if ss is an ordered set then inserting x in ss is still ordered`` arb a =
    Prop.forAll arb (fun xs -> 
                        let xs2 = insert a xs
                        ordered xs2);;
//test (change to Check.Quick for less output)
do Check.Verbose <| ``if ss is an ordered set then inserting x in ss is still ordered`` orderedNoRepArb;;
    