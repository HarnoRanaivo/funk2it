(* Fonctions de la forme :
 * let rec fun base condition suivant operation = function x ->
 *      if (condition x) then base
 *      else operation exp (fun suivant x)
 * ;;
 *)
let recursiveTerminale base condition suivant operation = function x ->
    let rec aux y t =
        if (condition y) then t
        else (aux (suivant y) (operation y t))
    in
    (aux x base)
;;

let iterative base condition suivant operation = function x ->
    let resultat = ref base
    and y = ref x in
    while not (condition !y) do
        resultat := operation !y !resultat;
        y := suivant !y;
    done;
    !resultat
;;

(* ??
 * Fonctions de la forme :
 * let rec fun base condition suivant1 suivant2 operation = function x ->
 *      if (condition x) then base
 *      else operation (fun suivant1 x) (fun suivant2 x)
 * ;;
 *)
let recursiveTerminale2 base condition suivant1 suivant2 operation = function x ->
    let rec aux y t u =
        if (condition y) then t
        else (aux (suivant1 y) (operation y t) (aux (suivant2 y) (operation y t) u))
    in
    (aux x base base)
;;
