#use "schemas.ml";;
(*
let factorielleRec = recursiveTerminale 1 ((=) 0) ((+) (-1)) ( * );;
let factorielleIt = iterative 1 ((=) 0) ((+) (-1)) ( * );;

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
    in (recursiveTerminale2 1 estFeuille gauche droite max plusplus) a
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
hauteurArbre2 arbre1;;
hauteurArbre2 arbre2;;
hauteurArbre2 arbre3;;

hauteurArbre2_2 arbre1;;
hauteurArbre2_2 arbre2;;
hauteurArbre2_2 arbre3;;
hauteurArbre2_2 arbre4;;
 *)

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

let rec fibo1 n =
    if n = 0  || n = 1 then 1
    else (+) (fibo1 (n-1)) (fibo1 (n-2))
;;

let fibo2 n =
    let zeroUn x = if x = 0 || x = 1 then true else false
    and moins1 x = x - 1
    and moins2 x = x - 2
    and plus x y = x + y
    and identite x = x
    in recursiveTerminale2 1 zeroUn moins1 moins2 plus identite n
;;

#trace recursiveTerminale;;
#trace recursiveTerminale2;;
fibo1 1;;
fibo2 1;;
