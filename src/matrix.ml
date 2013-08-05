open Bonus
open Schemas

type 'a matrix = 'a list list

let getLine (matrix : 'a matrix) y =
    List.nth matrix y

let nth (matrix : 'a matrix) x y =
    List.nth (getLine matrix y) x

let getColumn (matrix : 'a matrix) x =
    let rec aux matrix y  = match matrix with
        [] -> []
        | head :: tail -> (List.nth head y) :: (aux tail (y+1))
    in aux matrix 0

let height (matrix : 'a matrix) =
    let rec aux matrix n = match matrix with
        [] -> n
        | head :: tail -> aux tail (n+1)
    in aux matrix 0

let width (matrix : 'a matrix) =
    List.length (List.hd matrix)

let isValid (matrix : 'a matrix) =
    let rec aux matrix n = match matrix with
        [] -> true
        | head :: tail ->
            if (=) (List.length head) n then aux tail n
            else failwith "Invalid matrix."
    in aux (List.tl matrix) (List.length (List.hd matrix))

let setElement (matrix : 'a matrix) x y e =
    List.setElement matrix y (List.setElement (List.nth matrix y) x e)

let add m n =
    let height = (List.length m) - 1
    and width = (List.length (List.hd m)) - 1
    and currentMatrix = ref m in
    for i = 0 to width do
        for j = 0 to height do
            currentMatrix := setElement !currentMatrix i j ((+) (nth m i j) (nth n i j))
        done;
    done;
    !currentMatrix

let add2 m n =
    let rec auxLines m n o i =
        if (List.length m) = 0 then o
        else auxLines (List.tl m) (List.tl n) (List.setElement o i ((+) (List.hd m) (List.hd n))) (i+1)
    in
    let rec aux m n o i =
        if (List.length m) = 0 then o
        else aux (List.tl m) (List.tl n) (List.setElement o i (auxLines (List.hd m) (List.hd n) (List.hd m) 0)) (i+1)
    in aux m n m 0

let add3 m n =
    let rec auxLines m n o =
        if (List.length m) = 0 then o
        else auxLines (List.tl m) (List.tl n) (o @ [(+) (List.hd m) (List.hd n)])
    in
    let rec aux m n o =
        if (List.length m) = 0 then o
        else aux (List.tl m) (List.tl n) (o @ [(auxLines (List.hd m) (List.hd n) [])])
    in
    aux m n []

let add4 m n =
    let addLines = iterativeBFor
        []
        (fun (x, y) -> ((=) (List.length x) 0))
        (fun (x, y) -> (List.tl x, List.tl y))
        (fun (x, y) z -> (z @ [ (+) (List.hd x) (List.hd y) ]))
    in
    let eachLine = iterativeBFor
        []
        (fun (x, y) -> ((=) (List.length x) 0))
        (fun (x, y) -> (List.tl x, List.tl y))
        (fun (x, y) z -> (z @ [ addLines (List.hd x, List.hd y) ]))
    in eachLine (m, n)

let add5 m n =
    let resultat = ref []
    and y = ref (m, n)
    and n =
        let rec aux (x, y) acc =
            if ((=) (List.length x) 0) then acc
            else (aux (List.tl x, List.tl y) ((+) acc 1))
        in (aux (m, n) 0) - 1
    and addLines = iterativeBFor
        []
        (fun (x, y) -> ((=) (List.length x) 0))
        (fun (x, y) -> (List.tl x, List.tl y))
        (fun (x, y) z -> (z @ [ (+) (List.hd x) (List.hd y) ]))
    in
    for i = 0 to n do
        resultat := !resultat @ [ addLines (List.hd (fst !y), List.hd (snd !y)) ];
        y := (List.tl (fst !y), List.tl (snd !y));
    done;
    !resultat

let add6 x y =
    let resultat1 = ref []
    and y1 = ref (x, y)
    and n =
        let rec aux arg acc =
            if (List.length (fst arg)) = 0 then acc
            else aux (List.tl (fst arg), List.tl (snd arg)) (acc+1)
        in ((-) (aux (x, y) 0) 1)
    in
    for i = 0 to n do
        resultat1 := !resultat1 @ [(
            let resultat2 = ref []
            and y2 = ref (List.hd (fst !y1), List.hd (snd !y1))
            in
            let m =
                let rec aux arg acc =
                    if (List.length (fst arg)) = 0 then acc
                    else aux (List.tl (fst arg), List.tl (snd arg)) (acc+1)
                in ((-) (aux !y2 0) 1)
            in
            for j = 0 to m do
                resultat2 := !resultat2 @ [(+) (List.hd (fst !y2)) (List.hd (snd !y2))];
                y2 := (List.tl (fst !y2), List.tl (snd (!y2)));
            done;
            !resultat2
        )];
        y1 := (List.tl (fst !y1), List.tl (snd !y1));
    done;
    !resultat1

let add7 x y =
    let rec aux arg acc =
            if (List.length (fst arg)) = 0 then acc
            else aux (List.tl (fst arg), List.tl (snd arg)) (acc+1)
    in
    let resultat1 = ref []
    and y1 = ref (x, y)
    and n = ((-) (aux (x, y) 0) 1)
    in
    for i = 0 to n do
        let resultat2 = ref []
        and y2 = ref (List.hd (fst !y1), List.hd (snd !y1))
        in
        let m = ((-) (aux !y2 0) 1)
        in
        for j = 0 to m do
            resultat2 := !resultat2 @ [(+) (List.hd (fst !y2)) (List.hd (snd !y2))];
            y2 := (List.tl (fst !y2), List.tl (snd (!y2)));
        done;
        resultat1 := !resultat1 @ [!resultat2];
        y1 := (List.tl (fst !y1), List.tl (snd !y1));
    done;
    !resultat1
