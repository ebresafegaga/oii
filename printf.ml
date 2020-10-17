
type ('ty, 'v) my_format = 
    | End : ('v, 'v) my_format
    | Constant : string * ('ty, 'v) my_format -> ('ty, 'v) my_format
    | Hole : ('ty, 'v) my_format -> (string -> 'ty, 'v) my_format


let rec my_kprintf : type ty v. (string -> v) -> (ty, v) my_format -> ty = fun k -> function 
    | End -> k ""
    | Constant (const, fmt) -> 
        my_kprintf (fun str -> k (const ^ str)) fmt
    | Hole fmt -> 
        let f s = my_kprintf (fun str -> k (s ^ str)) fmt in 
        f

let my_printf fmt = my_kprintf (fun x -> x) fmt


let s = my_printf