#r "Stack.dll"
open Stack

type operator = Add | Prod | Minus;;

type token =
  | Op of operator
  | C  of int;;

let rpn1 = [ C 7 ; C 5 ;  Op Minus ];;
// 7 5 -   
// 7 - 5 

let rpn2 = [ C 10 ; C 3 ; C 2 ; Op Prod ; Op Add ];;
// 10 3 2 * +
// 10 + 3 * 2  

let rpn3 = [ C 10 ; C 3 ; Op Add ; C 2 ; Op Prod  ];;
// 10 3 + 2 * 
// (10 + 3) * 2  

let rpn4 = [ C 10 ; C 6 ; C 1 ; Op Minus ; Op Prod ; C 4 ; Op Minus ; C 2 ; C 5 ; Op Prod ;  Op Add ];;
// 10 6 1 - * 4 - 2 5 * +
// 10 * (6 - 1)  - 4 +  2 * 5

let rec eval token stack =
    match token with
    | [] -> top stack
    | x::xs -> if x=(C n) then 
                    push n stack
                    eval xs stack
               else 
                    let n1 = top stack
                    pop stack
                    let n2 = top stack
                    pop stack
                    push (n1 + n2) stack
                    eval xs stack;;

let evalRpn token =
    let st = empty
