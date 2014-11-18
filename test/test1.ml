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

let empty_stream : string Stream.t = [%stream]

let singleton_stream : string Stream.t = [%stream "hello"]

let list_stream = [%stream "hello"; "world"; "!"]

let int_stream = [%stream 1; 2; 3]

let char_stream = [%stream 'a'; 'b'; 'c']

let float_stream = [%stream 1.2; 2.3; 3.4]

(*let combine_stream stream1 stream2 =
    let stream2_next _ =
        try
            Some (Stream.next stream2)
        with Stream.Failure ->
            None
    in
    let stream1_next _ =
        try
            Some (Stream.next stream1)
        with Stream.Failure ->
            stream2_next 0
    in
    Stream.from stream1_next*)

(*let combined_stream = combine_stream list_stream singleton_stream*)

(*let combined_stream = [%stream list_stream; singleton_stream]*)

let list_stream = Stream.of_list ["hello"; "world"; "!"]
let list_stream = Stream.of_list ["bonjour"; "moi"; "!"]

let () =
    (*let _ = match%parse list_stream with
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
    in*)

    print_endline (Stream.next singleton_stream);
    print_endline (Stream.next list_stream);
    print_endline (Stream.next list_stream);
    print_endline (Stream.next list_stream);
    print_int (Stream.next int_stream);
    print_endline "";
    print_int (Stream.next int_stream);
    print_endline "";
    print_int (Stream.next int_stream);
    print_endline "";
    print_char (Stream.next char_stream);
    print_endline "";
    print_char (Stream.next char_stream);
    print_endline "";
    print_float (Stream.next float_stream);
    print_endline "";
    (*print_endline (Stream.next combined_stream);
    print_endline (Stream.next combined_stream);
    print_endline (Stream.next combined_stream);
    print_endline (Stream.next combined_stream);*)
