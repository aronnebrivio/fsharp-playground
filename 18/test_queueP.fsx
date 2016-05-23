#r "queueP.dll"
open Queue

// a)
let q1 = put 3 empty |> put 5 |> put 10;;

//b)
let (x1,q2) = get q1;;
// val x1 : int = 3

//c)
let (x2,q3) = get q2;;
// val x2 : int = 5

//d)
let q4 = q3 |> put 15 |>  put 20;;
let (y1, q4_1) = get q4;;
// val y1 : 10
let (y2, q4_2) = get q4_1;;
// val y2 : 15
let (y3, q5) = get q4_2;;
// val y3 : 20

isEmpty q1;; //false
isEmpty empty;; //true
toList q1;; // int list = [3; 5; 10]
ofList [1..10];;
put_list [1..10] q1 |> toList;; // int list = [3; 5; 10; 1..10]