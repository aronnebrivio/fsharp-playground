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
    | (C n)::xs -> let stk = push n stack
                   eval xs stk
    | (Op n)::xs -> match n with
                    | Add -> 
                        let (n1, stack) = pop stack
                        let (n2, stack) = pop stack
                        let stk = push (n2 + n1) stack
                        eval xs stk
                    | Minus -> 
                        let (n1, stack) = pop stack
                        let (n2, stack) = pop stack
                        let stk = push (n2 - n1) stack
                        eval xs stk
                    | Prod -> 
                        let (n1, stack) = pop stack
                        let (n2, stack) = pop stack
                        let stk = push (n2 * n1) stack
                        eval xs stk;;

let evalRpn token =
    let st = empty
    let res = eval token st
    res;;

// test
evalRpn rpn1;;
evalRpn rpn2;;
evalRpn rpn3;;
evalRpn rpn4;;