


(* type aligned list *)


type ('a, 'b) ta_list = 
    | TalStart : ('a -> 'b) -> ('a , 'b) ta_list 
    | TalCons : ('a, 'b) ta_list * ('b -> 'c) -> ('a, 'c) ta_list

let talStart f = TalStart f 
let talCons f fs = TalCons (fs, f)

let rec compose : type a b c. (a, b) ta_list -> (b, c) ta_list -> (a, c) ta_list = fun f -> function  
    | TalStart k -> TalCons (f, k)
    | TalCons (k, j) -> 
        let r = compose f k in 
        TalCons (r, j)

let pipes f = 
    talStart f
    |> talCons (fun n -> Some n)
    |> talCons (function None -> 0 | Some x -> compare x "")
    
let rec un_wrap : type a b. (a, b) ta_list -> (a -> b) = function 
    | TalStart f -> f
    | TalCons (fs, f) -> 
        let g = un_wrap fs in 
        fun x -> f (g x)





