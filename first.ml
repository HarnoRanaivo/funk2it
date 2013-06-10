let recursiveTerminale r condition suivant operation = function x ->
    let rec aux y t =
        if (condition y) then t
        else (aux (suivant y) (operation y t))
    in
    (aux x r)
;;

let iterative r condition suivant operation = function x ->
    let resultat = ref r
    and y = ref x in
    while not (condition !y) do
        resultat := operation !y !resultat;
        y := suivant !y;
    done;
    !resultat
;;

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

let successeurY x y = y + 1;;

let longueurListe = recursiveTerminale 0 listeEstVide queueListe successeurY;;

type 'a arbre =
    Feuille of 'a
    | Noeud of 'a arbre * 'a arbre
;;

let maxTeteInt l n = match l with
    (a, b) :: q -> if b > n then b else n;;

let hauteursNoeuds arbres = match arbres with
    [] -> []
    | (Feuille(f), n) :: q -> q
    | (Noeud(gauche, droite), n) :: q -> (gauche, n+1) :: (droite, n+1) :: q
;;

let maxTeteN arbres n = match arbres with
    (Feuille(f), h) :: q -> if h > n then h else n
    | _ -> n
;;

let hauteurArbre arbre =
    (recursiveTerminale 0 listeEstVide hauteursNoeuds maxTeteN) [(arbre, 0)];;

hauteurArbre (Noeud(Feuille(1), Noeud(Feuille(2), Feuille(3))));;
hauteurArbre (Noeud(Noeud(Feuille(1), Feuille(2)), Noeud(Noeud(Feuille(1), Feuille(2)), Feuille(1))));;
