
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
                Node (Leaf Bad, Leaf Good)), 
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

let () = 
    let f = function Left -> print_endline "Left" | Right -> print_endline "Right" in
    actual
    |> List.iter f