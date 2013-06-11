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
let recursiveTerminale2 base condition suivant1 suivant2 operation op = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((suivant1 x), (op acc))
        :: ((suivant2 x), (op acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (recursiveTerminale base isEmpty toList newoper) [(x, base)]
;;
