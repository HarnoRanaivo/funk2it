#use "schemas.ml";;

(* Exemples basiques *)
let rec factorielle n =
    if n = 0 then 1
    else ( * ) n (factorielle (n-1))
;;

let factorielleRecTer = terminaleB 1 ((=) 0) ((+) (-1)) ( * );;
let factorielleWhile = iterativeBWhile 1 ((=) 0) ((+) (-1)) ( * );;
let factorielleFor = iterativeBFor 1 ((=) 0) ((+) (-1)) ( * );;

(* Longueur d'une liste *)
let estVide l = match l with [] -> true | t :: q -> false;;
let queue l = match l with [] -> [] | t :: q -> q;;
let tete l = match l with t :: q -> t;;
let projectionBadd1 a b = b + 1;;

let rec longueurListe l = match l with
    [] -> 0
    | t :: q -> 1 + longueurListe q
;;

let rec longueurListe2 l =
    if (estVide l) then 0
    else 1 + (longueurListe (queue l))
;;

let rec longueurListe3 l = 
    if (estVide l) then 0
    else projectionBadd1 l (longueurListe3 (queue l))
;;

let longueurListeRecTer = terminaleB 0 estVide queue projectionBadd1;;
let longueurListe4While = iterativeBWhile 0 estVide queue projectionBadd1;;
let longueurListe4For = iterativeBFor 0 estVide queue projectionBadd1;;

let rec listeX2 l = match l with
    [] -> []
    | t :: q -> (t*2) :: (listeX2 q)
;;

let rec listeX2_2 l =
    if (estVide l) then []
    else (tete l) :: (listeX2_2 (queue l))
;;

(* :: n'est pas commutatif ! *)
let concatX2 a b = ((tete a)*2) :: b;;
let concatInvX2 a b = b @ [((tete a) * 2)];;

let listeX2_3 = terminaleB [] estVide queue concatInvX2;;

(* Fibonacci *)
let rec fibonacci n =
    if n = 0 || n = 1 then 1
    else (+) (fibonacci (n-1)) (fibonacci (n-2))
;;

let fibonacciRecTer = terminale2 1 (fun x -> x = 0 || x = 1) ((+) (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x);;
let fibonacciWhile = iterative2While 1 (fun x -> x = 0 || x = 1) ((+) (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x);;
let fibonacciFor = iterative2For 1 (fun x -> x = 0 || x = 1) ((+) (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x);;

(* Arbres *)
type 'a arbreBinaire =
    Feuille of 'a
    | Noeud of 'a arbreBinaire * 'a * 'a arbreBinaire
;;

let rec hauteurArbreBin a = match a with
    Feuille(x) -> 1
    | Noeud(gauche, x, droite) -> 1 + (hauteurArbreBin gauche) + (hauteurArbreBin droite)
;;

let estFeuille a = match a with
    Feuille(x) -> true
    | Noeud(gauche, x, droite) -> false
;;

let arbreGauche (Noeud(gauche, x, droite)) = gauche;;

let arbreDroit (Noeud(gauche, x, droite)) = droite;;

let max a b = if a > b then a else b;;

let rec hauteurArbreBin2 a =
    if (estFeuille a) then 1
    else max (1 + (hauteurArbreBin2 (arbreGauche a))) (1 + (hauteurArbreBin2 (arbreDroit a)))
;;

let hauteurArbreBinRecTer = terminale2 1 estFeuille arbreGauche arbreDroit max ((+) 1) ((+) 1);;
let hauteurArbreBinWhile = iterative2While 1 estFeuille arbreGauche arbreDroit max ((+) 1) ((+) 1);;
let hauteurArbreBinFor = iterative2For 1 estFeuille arbreGauche arbreDroit max ((+) 1) ((+) 1);;

(* Puissance *)
let rec puissance n x =
    if n = 0 then 1
    else x * (puissance (n-1) x)
;;

let puissance2 n x =
    let rec aux n x acc =
        if n = 0 then acc
        else aux (n-1) x (( * ) x acc)
    in aux n x 1
;;

let curryInv f a b = (f (a, b));;

let puissanceRecTer =
    let puissanceRecTerAux = terminaleB 1 (fun (a, b) -> (=) 0 a) (fun (a,b) -> (((+) (-1) a), b)) (fun (a, b) c -> ( * ) b c)
    in curryInv puissanceRecTerAux
;;

let puissanceWhile =
    let puissanceWhileAux = iterativeBWhile 1 (fun (a, b) -> (=) 0 a) (fun (a,b) -> (((+) (-1) a), b)) (fun (a, b) c -> ( * ) b c)
    in curryInv puissanceWhileAux
;;

let puissanceFor =
    let puissanceForAux = iterativeBFor 1 (fun (a, b) -> (=) 0 a) (fun (a,b) -> (((+) (-1) a), b)) (fun (a, b) c -> ( * ) b c)
    in curryInv puissanceForAux
;;

let puissanceFun n x =
    let composition a b n = a (b n) in
    let rec aux n x acc =
        if n = 0 then acc
        else aux (n-1) x (composition x acc)
    in aux n x (fun x -> x)
;;

let puissanceFunRecTer =
    let puissanceFunRecTerAux = terminaleB (fun x -> x) (fun (a, b) -> (=) 0 a) (fun (a, b) -> ((a-1), b)) (fun (a, b) x y -> b (x y))
    in curryInv puissanceFunRecTerAux
;;

let puissanceFunWhile =
    let puissanceFunWhileAux = iterativeBWhile (fun x -> x) (fun (a, b) -> (=) 0 a) (fun (a, b) -> ((a-1), b)) (fun (a, b) x y -> b (x y))
    in curryInv puissanceFunWhileAux
;;

let puissanceFunFor =
    let puissanceFunForAux = iterativeBFor (fun x -> x) (fun (a, b) -> (=) 0 a) (fun (a, b) -> ((a-1), b)) (fun (a, b) x y -> b (x y))
    in curryInv puissanceFunForAux
;;
