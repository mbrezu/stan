
let (|>) x f = f x

let (<|) f x = f x;;

let const k = fun _ -> k;;

module StringSet = Set.Make(String);;

module StringMap = Map.Make(String);;

let map_of_list list =
  let rec mol_iter map list =
    match list with
      | [] -> map
      | (key, value) :: tl ->
          mol_iter (StringMap.add key value map) tl
  in
    mol_iter StringMap.empty list;;

