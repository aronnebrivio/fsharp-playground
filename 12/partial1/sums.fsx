// i)
type sums =
    | C of int
    | Add of sums list;;

let s1 = Add [C 1; C 2; C 3];;
let s2 = Add [C 10; s1];;
let s3 = Add [s1; C -5; s2];;

// ii)
// funzione che mi conta `quanti Add ho nella sommatoria`
let rec countAdd e =
    match e with
    | C n -> 0
    | Add es -> (List.map countAdd es |> List.sum) + 1;;

// iii)
// calcola la sommatoria
let rec ev e =
    match e with
    | C n -> n
    | Add es -> (List.map ev es |> List.sum);;