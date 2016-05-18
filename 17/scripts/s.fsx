// using the dll

#r "listFS.dll"
#r "treeFS.dll"
open FSet

// let's create a set
let oneset = ofList [1..10]

// what? I can't see squat!

// utility to see what's going on, but still return a set
let print ss =
  let xs = toList ss |> List.sort
  printf "result set: %A\n" xs
  ss

let _ = print oneset

let ss = add 3 empty |> add 5 |> add 2 |> add 77 |> print

let ss1 = (add 3 ss) |> print;;

let u = union oneset ss |> print;;

// after, try with other representation



// let's test our Set implementation wrt the Set collection
#r "FsCheck"
open FsCheck

(*
 - a suite of tests, as a list, each with a label to identify the culprit

- note that use of List as a mediation betweeb FSet and Set

- note that use of List.sort to avoid false postives due to FSet as list  not being ordered

*)


let test_set x (xs : int list) ys =
  [
    empty |> toList = (Set.empty |> Set.toList) |@ "empty set"
    isEmpty (ofList xs) = Set.isEmpty (Set.ofList xs) |@ "is empty"
    contains x (ofList xs) = Set.contains x (Set.ofList xs) |@ "contains"
    (add x (ofList xs) |> toList |> List.sort) = (Set.add x (Set.ofList xs) |> Set.toList)  |@ "add"
    (union  (ofList xs) (ofList ys) |> toList |> List.sort) =
       (Set.union  (Set.ofList xs)  (Set.ofList ys)|> Set.toList) |@ "union"
    (ofList xs |> toList |> List.sort ) = (Set.ofList xs |> Set.toList) |@ "list"
    ]

do Check.Quick test_set


(* Let's actually use this ADT for something useful: evaluation of
logical formulae, where propositional variables takes values in an
enviroments, with the understanding that if a variable occurs, then
it's true, otherwise false *)

type form =
  V of int
  | K of bool
  | Not of form
  | And of form * form


type env = FSet<int>

let rec eval env = function
  K b -> b
  | V i -> contains i env
  | Not f -> eval env f |> not
  | And (f1, f2) -> (eval  env f1) && (eval env f2)


let test() =
  let [e] = Gen.sample 20 1 Arb.generate<form>
  let env = Gen.sample 20 20 Arb.generate<int>
  printfn "\tinput formula: %A\n\tinput env: %A" e env
  eval (FSet.ofList env) e
  
  
