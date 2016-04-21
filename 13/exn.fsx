// we have seen how to protect from run-time errors (DividebyZero) with options
// odiv: (int -> int -> int option)

let odiv x y =
    if y = 0
        then None
            else Some (x/y);;

(* EXCEPTIONS are more flexible and informative
 (but more expensive in F#)

In general: FP expressions
	- always have (unique) type
	- may have a value (upon termination)
	- may have an effect.
	
an *effect* is any action resulting from
evaluation of an expression other than returning a value. 

1. Exceptions. Evaluation may be aborted by signaling an exceptional
condition.

2. Mutation. Storage may be allocated and modified during evaluation.

3. Input/output. It is possible to read from an input source and write to
an output sink during evaluation.

4. Communication. Data may be sent to and received from communication
channels.

-- Static violations are signalled by type checking errors; 
-- dynamic violations are signalled by raising exceptions.

The primary benefits of the exceptions

1. They force you to consider the exceptional case (if you don’t, you’ll
get an uncaught exception at run-time), and

2. They allow you to segregate the special case from the normal case in
the code (rather than clutter the code with explicit checks).

// -----------------------------------------------------------

What is the type  of exceptional values?


- It must be **one and the same** for all exceptions in a program. 

1. A very naive choice: type of strings. 

    This allows one to associate an “explanation” with an exception. 
    For example, one may write
        raise "Division by zero error."

    -> but a handler for such an exception would have to interpret (parse) the
    string if it is to distinguish one exception from another,


2.  integers, so that exceptional conditions are encoded as error numbers
that describe the source of the error. 
    By dispatching on the numeric code the handler can determine how to recover from it. 

    -> one must establish a globally agreed-upon system of numbering, 
        no modular decomposition and component reuse. 
        
    -> it is practically impossible to associate meaningful data with an exceptional condition

3.  sum type whose classes are the exceptional conditions.  

    For example

    type exn = DivByZero  | FileNotFound of string, . . .

    -> it imposes a static classification of the sources of
        failure in a program. There must be one, globally agreed-upon type


SOLUTION:

----> An **extensible sum type** is one that permits new tags to be
created dynamically so that the collection of possible tags on values
of the type is not fixed statically, but only at runtime.

// ---------------------------------------------------------------
There are 3 basic ways to use exceptions in F#

i.   Using one of the built in functions, such as failwith

ii.  Using one of the standard .NET exception classes that inherits
     from System.Exception (not detailed here)

iii. Using your own custom exception types

i.  pre-defined exn in F#
*)
// let x = 1 / 0

// ===> System.DivideByZeroException: Division by zero

// let _ = List.head ([]:int list)
// ===>  System.ArgumentException: The input list was empty.

// let [] =  [1;2]

// ==> MatchFailureException

// let req = System.Net.WebRequest.Create("not a URL");;
//  ==> System.UriFormatException: Invalid URI: The format of the URI could not be determined

(*
BUILTIN excn:

val failwith : string -> 'a
val failwithf : StringFormat<'a,'b> -> 'a
val invalidArg : (string -> string -> 'a)
                  parameter-name  message
etc.

 Of course, just signaling excpetions is not terribly useful:

 Exceptions are caught using a try-catch block, as in other languages. 
 F# calls it try-with instead, and testing for each type of exception 
 uses the standard pattern matching syntax.

 try e with p1 -> e1 | ..... | pn -> en

 where p_i is like a 'pattern' of match cases, but of type exception
 *)


// with (predefined) exn
// val divide0 : int -> int -> string

// note same types in both alternatives (here string)
let divide0 x y =
   try
      string (x / y)
   with
      | :? System.DivideByZeroException -> "Division by zero!"

let result1 = divide0 100 ( 3 - 3)
let result2 = divide0 100 ( 10 - 5)

(*
 because exceptions are objects in F#
 System.DivideByZeroException is a class that inherits from System.Exception
 here ex is a value of type  System.DivideByZeroException
 and the pattern ':? class' checks whether ex is such a value
*)


// excn AND options
// val divide1 : x:int -> y:int -> int option
let divide1 x y =
   try
      Some (x / y)
   with
      | :? System.DivideByZeroException ->
        printfn "Division by zero!"
        None

let result3 = divide1 100 3
let result4 = divide1 100 0


// Catching a failwith(f)

let f  = function
  | n when n < 0 -> failwithf "your input %d was negative!" n
  | n -> n;;

let g n =
  try
    f n |> string
  with
    Failure s -> s

let z1 = g 1
let z2 = g -2

(*
EXERCISE: define the head of a list **using** List.head and catching
the exception

val safe_head : 'a list -> 'a option

let sh = safe_head ([]:int list)
    > val sh : int option = None
let st = safe_head [1;2]
    > val st : int option = Some 1

*)

let safe_head xs =
  try
    Some (List.head xs)
  with
  :? System.ArgumentException ->
     printfn "Empty list!"
     None


// iii. defining custom exceptions 


exception NegFactorial

// we use raise : (System.Exception -> 'a)

let check_fact n =
    let rec fact = function
        | 0 -> 1
        | m -> m * fact (m - 1)
    if n >= 0 then fact n else raise NegFactorial

// val handle_fact_s : n:int -> string
let handle_fact_s n =
    try
        string(check_fact n)
    with
        | NegFactorial ->  "negative integer not valid" 
       
let p1 = handle_fact_s 4
let p2 = handle_fact_s -42
       
// more informative, with sprintf to create the return formatted string
// sprintf : Printf.StringFormat<'a> -> 'a
let handle_fact n =
    try
        let res = check_fact n
        sprintf "input: %d, output: %d" n res
    with
        | NegFactorial -> sprintf " your input: %d is negative!" n
 
let p4 = handle_fact 4
let p5 = handle_fact -42

// exceptions can take values

exception NegFactorial2 of int

let check_fact2 n =
    let rec fact = function
        | 0 -> 1
        | m -> m * fact (m - 1)
    if n >= 0 then fact n else raise <| NegFactorial2 n

// here does not help, but in general it's useful
let handle_fact2 n =
    try
        let res = check_fact2 n
        sprintf "input: %d, output: %d" n res
    with
        | NegFactorial2 m -> sprintf " your input: %d is negative!" m

(* ***************************************** *)
// exc to change the flow of computation
//     breaking out of a loop/recursion

#time;; 
let zeros =  0 :: List.init  10000000 (fun i -> 3);;

let fmult_list xs = List.foldBack ( * ) xs 1

// this is wasteful, as it scans the whole list

let res = fmult_list zeros

exception Found_zero;;

let mult_list l =
    let rec mult_aux  l =
      match l with
        | [] ->  1
        | 0 :: _ -> raise Found_zero
        | n :: xs -> n * (mult_aux xs)
    try
        mult_aux l
    with
        |Found_zero -> 0;;

let m1 = mult_list zeros


(*
Note that exceptions are not handled very efficiently in .NET, 
so you may want to ether use continuations or option types for 
changing the control flow: here's the sam eexample with options
*)

// 
let rec multListOpt2  l =
      match l with
        | [] ->  Some 1
        | 0 :: _ -> None
        | n :: xs -> 
            match (multListOpt2 xs) with
            None -> None
            | Some k ->  Some (n * k ) 


// a (monadic) trick: an operator (read "bind") akin to List.map to hide the partiality
let (>>) f = function
    None -> None
    | Some x -> Some (f x)

let rec multListOpt  l =
      match l with
        | [] ->  Some 1
        | 0 :: _ -> None
        | n :: xs -> (fun v -> n * v)  >> multListOpt xs
 
let m2 = multListOpt zeros



