#use "schemas.ml";;
#use "exemples.ml";;

(* Tests *)
let testFactorielle n =
    let test = ref true in
    for i = 0 to n do
        let r = factorielle i in
        if r <> (factorielleRecTer i)
            || r <> (factorielleWhile i)
            || r <> (factorielleFor i)
            || r <> (factorielle2RecTer i)
            || r <> (factorielle2While i)
            || r <> (factorielle2For i)
        then test := false
    done;
    !test
;;

let testLongueurListe n =
    let test = ref true
    and listeN n =
        let rec aux n l =
            if n = 0 then l else aux (n-1) (n::l)
        in aux n []
    in
    for i = 0 to n do
        let l = listeN i in
        let r = longueurListe l in
        if r <> (longueurListeRecTer l)
            || r <> (longueurListe4While l)
            || r <> (longueurListe4For l)
        then test := false
    done;
    !test
;;

let testListeX2 n =
    let test = ref true
    and listeN n =
        let rec aux n l =
            if n = 0 then l else aux (n-1) (n::l)
        in aux n []
    in
    for i = 0 to n do
        let l = listeN i in
        let r = listeX2 l in
        if r <> (listeX2_RecTerB2 l)
            || r <> (listeX2_WhileB2 l)
            || r <> (listeX2_ForB2 l)
        then test := false
    done;
    !test
;;

let testFibonacci n =
    let test = ref true in
    for i = 0 to n do
        let r = fibonacci n in
        if r <> (fibonacciRecTer n)
            || r <> (fibonacciWhile n)
            || r <> (fibonacciFor n)
        then test := false
    done;
    !test
;;

let testPuissanceFun n x f =
    let test = ref true in
    for i = 0 to n do
        let r = ref x in
        for j = 1 to i do
            r := f !r;
        done;
        if !r <> ((puissanceFunRecTer i f) x)
            || !r <> ((puissanceFunWhile i f) x)
            || !r <> ((puissanceFunFor i f) x)
        then test := false;
    done;
    !test
;;

let testAll n =
    testFactorielle n
    && testLongueurListe n
    && testListeX2 n
    && testFibonacci n
    && testPuissanceFun n 0 (fun x -> x + 1)
;;
