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