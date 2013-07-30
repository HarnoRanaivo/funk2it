(* Fonctions de la forme :
 * let rec fun base condition iterateur operation = function x ->
 *      if (condition x) then base
 *      else operation x (fun iterateur x)
 *
 * operation doit être commutative et transitive ?
 *
 * Exemple:
 * let rec factorielle n =
 *      if n = 0 then 1
 *      else ( * ) n (factorielle (n-1))
 *
 * base : 1
 * condition : ((=) 0)
 * iterateur : ((+) (-1))
 * operation : ( * )
 *
 * let factorielleRecTer = recursiveTerminale 1 ((=) 0) ((+) (-1)) ( * )
 * let factorielleIter = iterative 1 ((=) 0) ((+) (-1)) ( * )
 *)
let terminaleB base condition iterateur operation = function x ->
    let rec aux y t =
        if (condition y) then t
        else (aux (iterateur y) (operation y t))
    in
    (aux x base)

let iterativeBWhile base condition iterateur operation = function x ->
    let resultat = ref base
    and y = ref x in
    while not (condition !y) do
        resultat := operation !y !resultat;
        y := iterateur !y;
    done;
    !resultat

let iterativeBFor base condition iterateur operation = function x ->
    let resultat = ref base
    and y = ref x
    and n argument condition iterateur =
        let rec aux arg acc =
            if (condition arg) then acc
            else (aux (iterateur arg) (acc+1))
        in (aux argument 0)
    in
    for i = 0 to ((n x condition iterateur) - 1) do
        resultat := operation !y !resultat;
        y := iterateur !y;
    done;
    !resultat

(* Peut se transformer en for,
 * si condition : (fun x -> x >= N) et iterateur : (fun x -> x + 1)
 * ou si condition : (fun x -> x <= N) et iterateur : (fun x -> x - 1)
 *
 * let iterative base operation = function x ->
 *      let resultat = ref base
 *      for y = x to N do
 *          resultat := operation y !resultat;
 *      done;
 *      !resultat
 *)

(*
 * Dans les cas où base est une fonction...??
 * L'accumulateur semble devoir commencer à l'élément neutre de l'operation...
 *)
let recursiveTerminale3 base condition iterateur operation neutre = function x ->
     let rec aux y a =
         if condition y then operation (base y) a
         else aux (iterateur y) (operation y a)
     in aux x neutre

(* ??
 * Fonctions de la forme (plusieurs appels récursifs) :
 * let rec fun base condition iterateur1 iterateur2 operation op1 op2 = function x ->
 *      if (condition x) then base
 *      else operation (op1 (fun iterateur1 x)) (op2 (fun iterateur2 x))
 *
 * Exemple 1 :
 *
 * let rec fibonacci n =
 *      if n = 0 || n = 1 then 1
 *      else (+) (fibonacci (n-1)) (fibonacci (n-2))
 *
 * condition : x = 0 || x = 1
 * base : 1
 * operation : (+)
 * op1 : (fun x -> x)
 * op2 : (fun x -> x)
 * iterateur1 : ((+) (-1))
 * iterateur2 : ((+) (-2))
 *
 * let fibonacciRecTer = recursiveTerminale2 1 (fun x -> x = 0 || x = 1) ((+)
 * (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x)
 * let fibonacciIter = recursiveTerminale2 1 (fun x -> x = 0 || x = 1) ((+)
 * (-1)) ((+) (-2)) (+) (fun x -> x) (fun x -> x)
 *
 * (Bien que fibonacci puisse être définie par ce qui suit...)
 * let fibonacci n =
 *      let rec aux n a b =
 *          if n = 0 || n = 1 then b
 *          else aux (n-1) b b+a
 *      in aux n 1 1
 *
 * Exemple 2 :
 * type 'a arbre =
 *      Feuille of 'a
 *      | Noeud of 'a arbre * 'a arbre
 *
 * let max a b = if a > b then a else b
 *
 * let rec hauteurArbre a = match a with
 *      Feuille(f) -> 1
 *      | Noeud(g, d) -> max (1 + (hauteurArbre g)) (1 + (hauteurArbre d))
 *
 * base : 1
 * condition : (fun a -> match a with Feuille(f) -> true | Noeud(g, d) -> false)
 * iterateur1 : (fun (Noeud(g, d)) -> g)
 * iterateur2 : (fun (Noeud(g, d)) -> d)
 * operation : max
 * op1 : ((+) 1)
 * op2 : ((+) 1)
 *)
let terminale2 base condition iterateur1 iterateur2 operation op1 op2 = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((iterateur1 x), (op1 acc))
        :: ((iterateur2 x), (op2 acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (terminaleB base isEmpty toList newoper) [(x, base)]

let iterative2While base condition iterateur1 iterateur2 operation op1 op2 = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((iterateur1 x), (op1 acc))
        :: ((iterateur2 x), (op2 acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (iterativeBWhile base isEmpty toList newoper) [(x, base)]

let iterative2For base condition iterateur1 iterateur2 operation op1 op2 = function x ->
    let isEmpty l = match l with
        [] -> true
        | h :: q -> false
    and toList l = match l with
        [] -> []
        | (x, acc) :: q -> if (condition x) then q else ((iterateur1 x), (op1 acc))
        :: ((iterateur2 x), (op2 acc)) :: q
    and newoper l acc = match l with
        [] -> acc
        | (x, accL) :: [] -> acc
        | (x, accL) :: q -> if (condition x) then (operation accL acc) else
            acc
    in
    (iterativeBFor base isEmpty toList newoper) [(x, base)]

(* Fonctions de la forme (l'appel récursif contient un appel récursif) :
 * let rec f n =
 *      if (condition n) then base
 *      else operation n (g (f (h (f (i n)))))
 *
 * Difficile de définir une fonction de cette forme qui termine.
 * McCarthy91 termine :
 * let rec mc91 n =
 *      if n > 100 then n - 10
 *      else mc91 (mc91 (n + 11))
 *
 * let mc91RecTer n =
 *     let rec aux n m =
 *         if m = 0 then n
 *         else if n > 100 then aux (n-10) (m-1)
 *         else aux (n+11) (m+1)
 *     in aux n 1
 *)

let terminaleB2 base condition iterateur operation = function x ->
    let composition a b = fun x -> a (b x) in
    let rec aux accumulateur argument =
        if (condition argument) then accumulateur (base argument)
        else aux (composition accumulateur (operation argument)) (iterateur argument)
    in aux (fun x -> x) x

let iterativeB2While base condition iterateur operation = function x ->
    let composition a b = fun x -> a (b x)
    and fonctionOrdreSup = ref (fun x -> x)
    and y = ref x in
    while not (condition !y) do
        fonctionOrdreSup := composition !fonctionOrdreSup (operation !y);
        y := iterateur !y;
    done;
    !fonctionOrdreSup (base !y)

let iterativeB2For base condition iterateur operation = function x ->
    let composition a b = fun x -> a (b x)
    and fonctionOrdreSup = ref (fun x -> x)
    and y = ref x
    and n argument condition iterateur =
        let rec aux arg acc =
            if (condition arg) then acc
            else (aux (iterateur arg) (acc+1))
        in (aux argument 0)
    in
    for i = 0 to ((n x condition iterateur) - 1) do
        fonctionOrdreSup := composition !fonctionOrdreSup (operation !y);
        y := iterateur !y;
    done;
    !fonctionOrdreSup (base !y)
