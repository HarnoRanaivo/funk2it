module List = struct
    include List

    let swapElements l x y =
        let listSize = List.length l in
        if x >= listSize || y >= listSize then failwith "List too short."
        else match l with
            [] -> failwith "The list is empty!"
            | t :: q ->
                let a = min x y
                and b = max x y in
                let athElement = List.nth l a
                and bthElement = List.nth l b in
                let rec aux l position = match l with
                    [] -> []
                    | listHead :: listTail ->
                        if position = a then bthElement :: (aux listTail (position+1))
                        else if position = b then athElement :: listTail
                        else listHead :: (aux listTail (position+1))
                in
                aux l 0

    let setElement l n x =
        let listSize = List.length l in
        if n > listSize then failwith "List too short."
        else let rec aux l position = match l with
                [] -> [x]
                | listHead :: listTail ->
                    if position = n then x :: listTail
                    else listHead :: (aux listTail (position+1))
            in
            aux l 0

    let insertSort l =
        let maxPosition = (List.length l) - 1
        and currentList = ref l in
        for i = 1 to maxPosition do
            let x = List.nth !currentList i
            and hole = ref i in
            while !hole > 0 && (x < (List.nth !currentList (!hole - 1))) do
                currentList := setElement !currentList !hole (List.nth !currentList (!hole-1));
                hole := !hole - 1;
            done;
            currentList := setElement !currentList !hole x;
        done;
        !currentList

    let selectSort l =
        let n = List.length l - 1
        and currentList = ref l in
        for i = 0 to n - 1 do
            let minimum = ref i in
            for j = i+1 to n do
                if (List.nth !currentList j) < (List.nth !currentList !minimum) then minimum := j;
            done;
            if !minimum <> i then currentList := swapElements !currentList i !minimum;
        done;
        !currentList

    (*
    let selectSort2 l =
        let n = List.length l - 1 in
        let rec rechercheMin = terminaleB
            1
            (fun (x, y, z) -> y <= n)
            (fun (x, y, z) -> (x, (y+1), z))
            (fun (x, y, z) a -> if (List.nth x y) < (List.nth x z) then y else z)
        in let rec aux = terminaleB
            []
            (fun (x, y) -> y < n)
            (fun (x, y) -> (swapElements x y (rechercheMin (x, y+1, y+1)), y+1))
            (fun (x, y) a ->
                let m = rechercheMin (x, y+1, y+1) in
                if m <> y then swapElements x y m else x
            )
        in aux (l, 0)
    *)


    let doubleList l =
        let maxPosition = (List.length l) - 1
        and currentList = ref [] in
        for i = 0 to maxPosition do
            currentList := setElement !currentList i (( * ) 2 (List.nth l i));
        done;
        !currentList

    let doubleListRec l =
        let maxPosition = (List.length l) - 1 in
        let rec aux l i =
            if i = (maxPosition + 1) then l
            else aux (setElement l i (( * ) 2 (List.nth l i))) (i+1)
        in aux l 0

    let multList l m =
        let lSize = List.length l in
        if lSize <> (List.length m) then failwith "Wrong sizes."
        else
            let maxPosition = lSize - 1
            and currentList = ref [] in
            for i = 0 to maxPosition do
                let ithL = List.nth l i
                and ithM = List.nth m i in
                currentList := setElement !currentList i (( * ) ithL ithM);
            done;
            !currentList

end

let curry f x y = f (x, y)
