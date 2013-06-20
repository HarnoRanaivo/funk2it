#use "schemas.ml";;

(*let factorielleRec = recursiveTerminale 1 ((=) 0) ((+) (-1)) ( * );;*)
(* let factorielleIt = iterative 1 ((=) 0) ((+) (-1)) ( * ) ;; *)

(*
let teteListe liste = match liste with
    [] -> []
    | t :: q ->  t
;;

let queueListe liste = match liste with
    [] -> []
    | t :: q -> q
;;

(recursiveTerminale 1 ((=) 0) ((+) (-1)) ( * )) 5;;

(* let longueurListe =
  let successeurY x y = y + 1 in
  recursiveTerminale 0 listeEstVide queueListe successeurY;; *)
 *)

let listeEstVide liste = match liste with
    [] -> true
    | t :: q -> false
;;

(*
type 'a arbre =
    Feuille of 'a
    | Noeud of 'a arbre * 'a arbre
;;

let hauteursNoeuds arbres = match arbres with
    [] -> []
    | (Feuille(f), n) :: q -> q
    | (Noeud(gauche, droite), n) :: q -> (gauche, n+1) :: (droite, n+1) :: q
;;

let maxTeteN arbres n = match arbres with
    (Feuille(f), h) :: q -> if h > n then h else n
    | _ -> n
;;

let hauteurArbreRec arbre =
    (recursiveTerminale 0 listeEstVide hauteursNoeuds maxTeteN) [(arbre, 1)];;

let hauteurArbreIt arbre =
    (iterative 0 listeEstVide hauteursNoeuds maxTeteN) [(arbre, 1)];;

(*
hauteurArbreRec (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
hauteurArbreRec (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
hauteurArbreIt (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
hauteurArbreIt (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
 *)

let rec hauteurArbre2aux a =
    let max a b = if a > b then a else b in
    match a with
        Feuille(f) -> 1
        | Noeud(g, d) -> max (1 + (hauteurArbre2aux g)) (1 + (hauteurArbre2aux d))
;;

let hauteurArbre2 arbre = hauteurArbre2aux arbre;;

let hArbre a =
    let estFeuille a = match a with
        Feuille(f) -> true
        | Noeud(g, d) -> false
    and gauche (Noeud(g, d)) = g
    and droite (Noeud(g, d)) = d
    and plusplus x = x + 1
    and max a b = if a > b then a else b
    in (recursiveTerminale2 1 estFeuille gauche droite max plusplus plusplus) a
;;

let rec hauteurArbre2aux_2 a n m =
    let max a b = if a > b then a else b in
    match a with
    Feuille(f) -> max n m
    | Noeud(g, d) -> hauteurArbre2aux_2 g (n+1) (hauteurArbre2aux_2 d (n+1) m)
;;

let hauteurArbre2_2 arbre = hauteurArbre2aux_2 arbre 1 1;;

let arbre1 = (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
let arbre2 = (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
let arbre3 = (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(3)), Feuille(1))));;
let arbre4 = (Noeud(Noeud(Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(3)), Feuille(1)), Noeud(Feuille(1), Feuille(2))));;


(*
hauteurArbre2 arbre4;;
hauteurArbre2_2 arbre4;;
 *)
let arbre5 = Noeud(arbre4, arbre2);;

(*
hauteurArbre2 arbre5;;
hauteurArbre2_2 arbre5;;
 *)

hauteurArbreRec arbre5;;
hArbre arbre5;;
*)

(*
let rec fibo1 n =
    if n = 0  || n = 1 then 1
    else (+) (fibo1 (n-1)) (fibo1 (n-2))
;;

let fibo2 n =
    let zeroUn x = x = 0 || x = 1
    and moins1 x = x - 1
    and moins2 x = x - 2
    and plus x y = x + y
    and identite x = x
    in recursiveTerminale2 1 zeroUn moins1 moins2 plus identite identite n
;;

let fibo3 n =
    let zeroUn x = x = 0 || x = 1
    and moins1 x = x - 1
    and moins2 x = x - 2
    and plus x y = x + y
    and identite x = x
    in iterative2 1 zeroUn moins1 moins2 plus identite identite n
;;

let fibo4 n =
    let rec aux n a b =
        if n = 0 || n = 1 then b
        else aux (n-1) b (a+b)
    in aux n 1 1
;;

fibo1 9;;
fibo2 9;;
fibo3 9;;
fibo4 9;;
 *)

(*
let rec mc91 n =
    if n > 100 then n - 10
    else mc91 (mc91 (n + 11))
;;

let mc91Iter n =
    let c = (ref 1)
    and resultat = (ref n) in
    while !c <> 0 do
        if !resultat > 100 then
        (
            resultat := !resultat - 10;
            c := !c - 1;
        )
        else
        (
            resultat := !resultat + 11;
            c := !c + 1;
        )
    done;
    !resultat
;;

let mc91RecTer n =
    let rec aux n m =
        if m = 0 then n
        else if n > 100 then aux (n-10) (m-1)
        else aux (n+11) (m+1)
    in aux n 1
;;

mc91 1;;
mc91Iter 1;;
mc91RecTer 1;;
*)
let rec f n =
    if n >= 20 then n / 2
    else 3 * n * (f (n + 5))
;;

let rec aux n a =
    if n >= 20 then a + n/2
    else aux (n+5) (n*a*3)
;;

let f2 n = aux n 1;;
let rec aux2 n a =
    if n = 0 then a
    else aux2 (n-1) (a*n)
;;

(* let faa n = aux2 n 1;;*)
f 5;;
f2 5;;
