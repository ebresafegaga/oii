open Vector

type empty = E
type ('left, 'item, 'right) branch = B

type 'a t = 
    | Empty : empty t 
    | Branch : 'left t * 'item * 'right t -> ('left, 'item, 'right) branch t


let myTree = 
    Branch (
        Branch (
            Empty, 
            None, 
            Empty), 
        10, 
        Branch (
            Branch (
                Empty, 
                "here",
                Empty), 
            [1;3;], 
            Branch (
                Empty, 
                Some [[[[[[[]]]]]]], (* because, why not? *)
                Empty)))