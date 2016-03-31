#r @"FsCheck";;
open FsCheck;;

// 0)
type valV = { studente : string ; voto : int };;
type valG = { studente : string ; giudizio : string };;

// 1)
let valuta vall =
    match vall.voto with
    | x when x<18 -> { studente = vall.studente ; giudizio = "insufficiente"}
    | x when x<=22 -> { studente = vall.studente ; giudizio = "sufficiente"}
    | x when x<=27 -> { studente = vall.studente ; giudizio = "buono"}
    | x -> { studente = vall.studente ; giudizio = "ottimo"} ;;

    // test
    let v1 = {studente="Bianchi"; voto=16};;
    let v2 = {studente="Rossi"; voto=20};;
    let v3 = {studente="Brivio"; voto=25};;
    let v4 = {studente="Pigna"; voto=29};;
    valuta(v1);;
    valuta(v2);;
    valuta(v3);;
    valuta(v4);;

// 2)
let rec valutaList vlist =
    match vlist with
    | [] -> []
    | [x] -> [valuta(x)]
    | x::xs -> valuta(x) :: valutaList(xs) ;;

    // test
    let vlist = [v1; v2; v3; v4];;

let ``valutaList è una map di valuta`` (xs : valV list)  =
  List.map valuta xs = valutaList xs;;
do Check.Quick  ``valutaList è una map di valuta``

// 3)
let rec creaValList (slist : string list, vlist : int list) =
    match slist with
    | [] -> []
    | [x] -> match vlist with
             | [] -> []
             | y::ys -> [{studente=x; voto=y}]
    | x::xs -> match vlist with
               | [] -> []
               | y::ys -> {studente=x; voto=y} :: creaValList(xs,ys) ;;

    // test
    let sl1 = [];;
    let sl2 = ["Rossi"; "Bianchi"; "Brivio"];;
    let vl1 = [];;
    let vl2 = [11; 20; 30];;
    let vl3 = [11; 20];;
    let vl4 = [11; 20; 30; 28];;
    creaValList(sl1,vl1);;
    creaValList(sl1,vl2);;
    creaValList(sl2,vl1);;
    creaValList(sl2,vl2);;
    creaValList(sl2,vl3);;
    creaValList(sl2,vl4);;

// 4)
let rec sommaAndConta vlist =
    match vlist with
    | [] -> (0,0)
    | [x] -> (x.voto,1)
    | x::xs -> 
        let (s,n) = sommaAndConta xs
        (s+x.voto,n+1);;

let media vlist =
    let (s,n) = sommaAndConta vlist
    (float s)/(float n) ;;