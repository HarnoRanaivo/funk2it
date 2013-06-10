#use "schemas.ml";;

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

let listeEstVide liste = match liste with
    [] -> true
    | t :: q -> false
;;

let longueurListe =
  let successeurY x y = y + 1 in
  recursiveTerminale 0 listeEstVide queueListe successeurY;;

type 'a arbre =
    Feuille of 'a
    | Noeud of 'a arbre * 'a arbre
;;

let rec h arbre = match arbre with
    Feuille(f) -> 1
    | Noeud(g, d) -> h(g) + h(d) + 1
;;

let maxTeteInt l n = match l with
    [] -> n
    | (a, b) :: q -> if b > n then b else n;;

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
    (recursiveTerminale 0 listeEstVide hauteursNoeuds maxTeteN) [(arbre, 0)];;

let hauteurArbreIt arbre =
    (iterative 0 listeEstVide hauteursNoeuds maxTeteN) [(arbre, 0)];;

hauteurArbreRec (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
hauteurArbreRec (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
hauteurArbreIt (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
hauteurArbreIt (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;

let hauteurArbre2 arbre =
    let max a b = if a > b then a else b in
    let rec aux a = match a with
        Feuille(f) -> 1
        | Noeud(g, d) -> max (1 + (aux g)) (1 + (aux d))
    in aux arbre
;;

let rec hauteurArbre2aux a n m =
    let max a b = if a > b then a else b in
    match a with
    Feuille(f) -> max n m
    | Noeud(g, d) -> hauteurArbre2aux g (n+1) (hauteurArbre2aux d (n+1) m)
;;

let hauteurArbre2_2 arbre = hauteurArbre2aux arbre 1 1;;

let arbre1 = (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
let arbre2 = (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
let arbre3 = (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(3)), Feuille(1))));;
let arbre4 = (Noeud(Noeud(Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(3)), Feuille(1)), Noeud(Feuille(1), Feuille(2))));;

hauteurArbre2 arbre1;;
hauteurArbre2 arbre2;;
hauteurArbre2 arbre3;;

hauteurArbre2_2 arbre1;;
hauteurArbre2_2 arbre2;;
hauteurArbre2_2 arbre3;;
hauteurArbre2_2 arbre4;;
