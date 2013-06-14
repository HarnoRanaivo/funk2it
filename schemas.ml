(* Fonctions de la forme :
 * let rec fun base condition suivant operation = function x ->
 *      if (condition x) then base
 *      else operation x (fun suivant x)
 * ;;
 *
 * operation doit être commutative et transitive ?
 *
 * Exemple:
 * let rec factorielle n =
 *      if n = 0 then 1
 *      else ( * ) n (factorielle (n-1))
 * ;;
 *
 * base : 1
 * condition : ((=) 0)
 * suivant : ((+) (-1))
 * operation : ( * )
 *
 * let factorielleRecTer = recursiveTerminale 1 ((=) 0) ((+) (-1)) ( * );;
 * let factorielleIter = iterative 1 ((=) 0) ((+) (-1)) ( * );;
 *
 * Dans les cas où base est une fonction...??
 * L'accumulateur semble devoir commencer à l'élément neutre de l'operation...
 * let recursiveTerminale base condition suivant operation neutre = function x ->
 *      let rec aux y t =
 *          if condition y then operation (base y) a
 *          else aux (suivant y) (operation y a)
 *      in aux y neutre
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
(* Peut se transformer en for, dans les cas où (condition, suivant) sont
 * si condition : (fun x -> x >= N) et suivant : (fun x -> x + 1)
 * ou si condition : (fun x -> x <= N) et suivant : (fun x -> x - 1)
 *
 * let iterative base operation = function x ->
 *      let resultat = ref base
 *      for y = x to N do
 *          resultat := operation y !resultat;
 *      done;
 *      !resultat
 * ;;
 *)


(* ??
 * Fonctions de la forme :
 * let rec fun base condition suivant1 suivant2 operation op1 op2 = function x ->
 *      if (condition x) then base
 *      else operation (op1 (fun suivant1 x)) (op2 (fun suivant2 x))
 * ;;
 *
 * Exemple 1 :
 *
 * let rec fibonacci n =
 *      if n = 0 || n = 1 then 1
 *      else (+) (fibonacci (n-1)) (fibonacci (n-2))
 * ;;
 *
 * condition : x = 0 || x = 1
 * base : 1
 * operation : (+)
 * op1 : (fun x -> x)
 * op2 : (fun x -> x)
 * suivant1 : ((+) (-1))
 * suivant2 : ((+) (-2))
 *
 * let fibonacciRecTer = recursiveTerminale2 1 (fun x -> x = 0 || x = 1) ((+)
 * (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x);;
 * let fibonacciIter = recursiveTerminale2 1 (fun x -> x = 0 || x = 1) ((+)
 * (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x);;
 *
 * (Bien que fibonacci puisse être définie par ce qui suit...)
 * let fibonacci n =
 *      let rec aux n a b =
 *          if n = 0 || n = 1 then b
 *          else aux (n-1) b b+a
 *      in aux n 1 1
 * ;;
 *
 * Exemple 2 :
 * type 'a arbre =
 *      Feuille of 'a
 *      | Noeud of 'a arbre * 'a arbre
 * ;;
 *
 * let max a b = if a > b then a else b;;
 *
 * let rec hauteurArbre a = match a with
 *      Feuille(f) -> 1
 *      | Noeud(g, d) -> max (1 + (hauteurArbre g)) (1 + (hauteurArbre d))
 * ;;
 *
 * base : 1
 * condition : (fun a -> match a with Feuille(f) -> true | Noeud(g, d) -> false)
 * suivant1 : (fun (Noeud(g, d)) -> g)
 * suivant2 : (fun (Noeud(g, d)) -> d)
 * operation : max
 * op1 : ((+) 1)
 * op2 : ((+) 1)
 *)
let recursiveTerminale2 base condition suivant1 suivant2 operation op1 op2 = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((suivant1 x), (op1 acc))
        :: ((suivant2 x), (op2 acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (recursiveTerminale base isEmpty toList newoper) [(x, base)]
;;

let iterative2 base condition suivant1 suivant2 operation op1 op2 = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((suivant1 x), (op1 acc))
        :: ((suivant2 x), (op2 acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (iterative base isEmpty toList newoper) [(x, base)]
;;

