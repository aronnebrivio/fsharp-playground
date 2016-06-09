type boolex = Const of bool | Var of char | Neg of boolex | And of boolex * boolex;;
type environment = char list;;

// 1)
let rec eval b e =
    match b with
    | Const x -> x
    | Var x -> List.contains x e
    | Neg x -> not (eval x e)
    | And(x,y) -> (eval x e) && (eval y e);;

// test
let env1 = ['a'; 'b'; 'c'];;
let env2 = ['d'; 'e'];;
eval (Neg(Var 'e')) env1;;
eval (Neg(Var 'e')) env2;;
eval (And(Const false, Var 'e')) env2;;

// 2)
type ifex = Constif of bool | Varif of char | If of ifex * ifex * ifex;;

// 3)
let rec ifeval b e =
    match b with
    | Constif x -> x
    | Varif x -> List.contains x e
    | If(x,y,z) -> if (ifeval x e)=true then ifeval y e
                   else ifeval z e;;

// test
ifeval (If(Constif(3 = 4), Constif(1<2), If(Constif(true), Varif('a'), Varif('b')))) env1;;

// 4)
let rec bool2if b =
    match b with
    | Const x -> Constif x
    | Var x -> Varif x
    | Neg x -> If(bool2if(x), Constif(false), Constif(true))
    | And(x,y) -> If(bool2if(x), bool2if(y), Constif(false));;

// test
bool2if (And(Const false, Var 'e'));;

// 5)
#r "FsCheck"
open FsCheck

let b2if_prop bx env = eval bx env = ifeval (bool2if bx) env;;
do Check.Quick b2if_prop;;

// the skulls

// sequenze
// i)
let nat = Seq.initInfinite (fun x -> x);;
let seq1 = seq {
    yield 0
    yield 1
    yield 2
    yield 0
    yield 3
    yield 4
    yield 4
    yield 3
    yield 1
    yield! Seq.skip 5 nat
};;
let rec tmp x = 
    seq {
        yield x
        yield x
        yield! (tmp (x + 1))
    };;

let seq2 = tmp 0;;

let rec tmp2 x =
    seq {
        yield! Seq.take x nat
        yield! tmp2 (x + 1)
    };;
let seq3 = tmp2 2;;

// ii)
let rec distinct s =
    seq {
        let first = Seq.item 0 s
        yield first
        yield! (distinct (Seq.filter (fun x -> x <> first) (Seq.skip 1 s)))
    }
//test
distinct seq1 |> Seq.take 20 |> Seq.toList;;

//iii)
let rec isEqual n s0 s1 =
    match n with
    | 0 -> true
    | n ->
        let (a, b) = ((Seq.item 0 s0), (Seq.item 0 s1))
        a = b && isEqual (n - 1) (Seq.skip 1 s0) (Seq.skip 1 s1);;

// test
isEqual 20 nat (distinct seq1) ;;
isEqual 20 nat (distinct seq2) ;;
isEqual 20 nat (distinct seq3) ;;

