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

let digit_of_char character = int_of_char character - (int_of_char '0')

let parse_int stream =
    let rec parse_int value = function%parse
        | ['0' .. '9' as digit] -> parse_int (value * 10 + (digit_of_char digit)) stream
        | _ -> value
    in parse_int 0 stream

(*let rec parse_int = function%parse
    | ['0' .. '9' as digit; parse_int [@as rest]] -> digit_of_char digit
    | _ -> 0*)

let () =
    let stream = [%stream '1'; '2'; '3'; 'a'] in
    let hex = parse_int stream in
    print_int hex;
    print_endline ""
