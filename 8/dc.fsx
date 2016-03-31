// PARTE B) DAYCARE
#r @"FsCheck";;
open FsCheck;;

type category =
    | Daycare
    | Nursery
    | Recreation ;;

// i)
type names = string;;
type child = {name : names; category : category};;
type childDes = Child of names * category;;
// ii)
let rec number (cat, cd) =
    match cd with
    | [] -> 0
    | [x] -> 
        match x with
        | (_,c) when c=cat -> 1
        | (_,_) -> 0
    | x::xs ->
        let n = number (cat, xs)
        match x with
        | (_,c) when c=cat -> 1+n
        | (_,_) -> n ;;

    // test
    let ch1 = {name="Rossi"; category=Daycare};;
    let ch2 = {name="Bianchi"; category=Nursery};;
    let ch3 = {name="Brivio"; category=Recreation};;
    let ch4 = {name="Pignatale"; category=Recreation};;
    let ch5 = {name="Fortunato"; category=Daycare};;
    let ch6 = {name="Lupoli"; category=Daycare};;
    let cd1 = ("Rossi", Daycare);;
    let cd2 = ("Bianchi", Nursery);;
    let cd3 = ("Brivio", Recreation);;
    let cd4 = ("Pignatale", Recreation);;
    let cd5 = ("Fortunato", Daycare);;
    let cd6 = ("Lupoli", Daycare);;
    let cd7 = ("Brivio", Daycare);;
    let cdl = [cd1; cd2; cd3; cd4; cd5; cd6; cd7];;
    number(Recreation, cdl);;

// ii')
let number_prop (cdl) =
    let l1 = number(Daycare, cdl)
    let l2 = number(Nursery, cdl)
    let l3 = number(Recreation, cdl)
    cdl.Length = (l1+l2+l3)

do Check.Quick number_prop 

// iii)
let rec remove (nms, cd : childDes list) =
    match cd with
    | [] -> []
    | [x] ->
        match x with
        | (_,c) when c=nms -> []
        | (_,_) -> [x]
    | x::xs ->
        let n = number (nms, xs)
        match x with
        | (_,c) when c=nms -> xs
        | (_,_) -> x::xs ;;

//let pay (nms, cd) = 