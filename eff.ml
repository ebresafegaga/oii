
type empty 

effect Fail : empty
effect Decide : bool 

let fail () = Obj.magic (perform Fail)
let decide () = perform Decide 

let chooseTrue x =  match x with 
    | x -> x 
    | effect Decide k -> continue k true 

let chooseMax x = match x with 
    | x -> x 
    | effect Decide k -> 
        max (continue (Obj.clone_continuation k) true) (continue  (Obj.clone_continuation k) false)

let chooseAll x = match x with 
    | x -> [x] 
    | effect Fail _ -> [] 
    | effect Decide k -> 
        (continue (Obj.clone_continuation k) true) @ (continue (Obj.clone_continuation k) false) 

let backtrack x = match x () with 
    | x -> x 
    | effect Decide k -> 
        match continue (Obj.clone_continuation k) false with 
        | x -> x 
        | effect Fail _ -> continue (Obj.clone_continuation k) true

type d = Good | Bad 

type tree =
    | Leaf of d 
    | Node of tree * tree  

type direction = Left | Right 

let rec findGood acc  = function 
    | Leaf Good -> List.rev acc
    | Leaf Bad -> fail () 
    | Node (left, right) -> 
        if decide () 
            then findGood (Left :: acc) left 
            else findGood (Right :: acc) right

let findGood2 tree = 
    let module D = struct exception DeadEnd end in 
    let open D in 
    let rec findGood2 acc tree =
        let rec loop = function 
            | Leaf Good -> List.rev acc
            | Leaf Bad -> raise DeadEnd 
            | Node (left, right) ->  
                match findGood2 (Left :: acc) left with 
                | result -> result  
                | exception DeadEnd -> findGood2 (Right :: acc) right
        in 
        loop tree
    in 
    match findGood2 [] tree with 
    | k -> k 
    | exception DeadEnd -> []
    
let tree = 
    Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad,  Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad, Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad,  Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad, Leaf Bad)), 
            Leaf Bad)))), 
            Leaf Bad)))), 
            Leaf Bad)))), 
             Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad,  Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad, Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad,  Node (
        Node (Leaf Bad, Leaf Bad), 
        Node (
            Node (
                Leaf Bad, 
                Node (Leaf Bad, Leaf Bad)), 
            Leaf Bad)))), 
            Leaf Bad)))), 
            Leaf Bad)))), 
            Leaf Bad))))


let result = fun () -> findGood [] tree

let actual = 
    match backtrack result with 
    | k -> k 
    | effect Fail _ -> [] 

let actual2 = findGood2 tree

let () = 
    let f = function Left -> print_endline "Left" | Right -> print_endline "Right" in
    (match actual=actual2 with 
     | true -> print_endline "they are equal"
     | false -> print_endline "they are NOT equal");
    actual2 |> List.iter f;
    print_endline "";
    actual |> List.iter f

