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

let list_stream = [%stream "if"; "("; ")"]

type expression =
    | Addition of int * int
    | Assignation of string * expression

let expression stream = match%parse stream with
    | [operand1; "+"; operand2] -> Addition (int_of_string operand1, int_of_string operand2)

(*let assignation stream = match%parse stream with
    | [name; "="; expression as value] -> Assignation (name, value)*)

let () =
    let _ = match%parse list_stream with
    | ["hello"; "world"; "!"] ->
            print_endline "Hello World!"
    | ["bonjour"; word] ->
            print_string "Bonjour ";
            print_string word;
            print_endline "!";
    in
    let _ = match%parse list_stream with
    | ["hello"; word; "!"] ->
            print_endline word;
            print_endline "Hello World!"
    | ["bonjour"; word] ->
            print_string "Bonjour ";
            print_string word;
            print_endline "!";
    | "!" ->
            print_endline "Exclamation"
    | _ ->
            print_endline "No choice."
    in
    ()
