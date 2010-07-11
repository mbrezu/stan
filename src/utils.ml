(*
    STAN - a STatic ANalysis tool for PL/SQL. Copyright (C) 2010 Miron Brezuleanu

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see
    <http://www.gnu.org/licenses/>.
*)

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

let list_of_map map =
  let list =
    StringMap.fold
      (fun key value list -> (key, value) :: list)
      map
      []
  in
    List.sort compare list;;

