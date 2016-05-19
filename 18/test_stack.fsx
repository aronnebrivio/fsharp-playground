#r "Stack.dll"
open Stack

let s = push 1 empty |> push 2 |> push 3 |> push 18;;
let (x,s1) = pop s;;
top s;;
size s;;