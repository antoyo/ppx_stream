(*
 * Copyright (C) 2014  Boucher, Antoni <bouanto@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

let list_stream = [%stream "x"; "="; "42"; "+"; "24"; ";"]

type expression =
    | Addition of int * int
    | Assignation of string * expression

let expression stream = match%parse stream with
    | [operand1; "+"; operand2] -> Addition (int_of_string operand1, int_of_string operand2)

let start_assign stream = match%parse stream with
    | [name; "="] -> name

let value stream = match%parse stream with
    | [expression [@as value]] -> value

let value = function%parse
    | [expression [@as value]] -> value

let assignation stream = match%parse stream with
    (*| [name; "="] -> (match%parse stream with
        | [expression as value] -> Assignation (name, value)
    )*)
    (*| [start_assign as name; value as v] -> Assignation (name, v)*)
    | [start_assign [@as name]; value [@as v]; ";"] -> Assignation (name, v)
    (*| [name; "="] -> Assignation (name, value stream)*)
    (*| [name; "="; expression as value; ";"] -> Assignation (name, value)*)
    (*| [name; "="; expression as value] -> Assignation (name, value)*)

let () =
    (*match%parse list_stream with
    | "x" -> print_endline "variable"*)

    let list_stream = [%stream "x"; "*"; "y"] in
    match%parse list_stream with
    | [ start_assign [@as name] ] -> print_endline name
    | [ operand1; "*"; operand2 ] -> print_endline "multiplication"

    (*match assignation list_stream with
    | Assignation (name, value) -> print_endline name*)

    (*let _ = match%parse list_stream with
    | ["x"; "="; "10"] ->
            print_endline "x = 10"
    | ["="; word] ->
            print_string "= ";
            print_endline word;
    in
    let _ = match%parse list_stream with
    | [";"; word; "="] ->
            print_endline word
    | ["="; word] ->
            print_endline word
    | ";" ->
            print_endline ";"
    | _ ->
            print_endline "No choice."
    in
    ()*)
