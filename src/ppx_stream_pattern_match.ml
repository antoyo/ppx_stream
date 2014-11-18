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

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

exception Error of Location.t

let () =
    Location.register_error_of_exn (fun exn ->
        match exn with
        | Error loc ->
                Some (error ~loc "match%parse accepts list of elements, e.g. match%parse expression with | [\"if\"; \"(\"]")
        | _ -> None)

(*let gen_stream_from_list loc elements =
    Exp.apply ~loc (Exp.ident ({
        txt = Ldot (Lident "Stream", "of_list");
        loc
    })) ["", elements]

let list_from_constant loc constant =
    Exp.construct ~loc ({
        txt = Lident "::";
        loc
    }) (Some (Exp.tuple [
        Exp.constant ~loc constant;
        Exp.construct {
            txt = Lident "[]";
            loc
        } None
    ]))

let rec stream_subexpression loc = function
    | Pexp_constant (Const_char _ | Const_int _ | Const_string _ | Const_float _ as constant) -> list_from_constant loc constant
    | Pexp_sequence ({
            pexp_loc = loc1;
            pexp_desc = hd;
        }, {
            pexp_loc = loc2;
            pexp_desc = tl;
        }) ->
        let hd_expression = stream_subexpression loc1 hd in
        let tl_expression = stream_subexpression loc2 tl in
        Exp.apply ~loc (Exp.ident ({
            txt = Lident "@";
            loc
        }))
        [ "", hd_expression
        ; "", tl_expression
        ]

let stream_expression loc pstr =
    let elements = match pstr with
    | PStr [{
        pstr_desc = Pstr_eval ({
            pexp_loc = loc;
            pexp_desc = expression
        }, _)
    }] -> stream_subexpression loc expression
    | PStr [] ->
        Exp.construct {
            txt = Lident "[]";
            loc
        } None
    | _ -> raise (Error loc)
    in gen_stream_from_list loc elements*)

let rec list_length = function
    | {ppat_desc = Ppat_construct ({
            txt = Lident "::"
        }, Some {
            ppat_desc = Ppat_tuple tuple_list
        })
    } ->
            (match tuple_list with
            | [{
                ppat_desc = Ppat_constant _ | Ppat_var _
            }; rest] ->
                1 + list_length rest
            )
    | {ppat_desc = Ppat_construct ({
            txt = Lident "[]"
        }, None)
    } -> 0
    | { ppat_desc = Ppat_any } -> 0

let transform_match_case loc match_expression stream_list statements other_cases =
    let length = 
        try
            list_length stream_list
        with Match_failure _ -> raise (Error loc)
    in
    if length > 0 then (
        Exp.match_ (
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "npeek");
                loc;
            }))
            [ ("", Exp.constant (Const_int length))
            ; ("", match_expression)
            ]
        )
        (Exp.case stream_list
            (Exp.sequence (Exp.for_ (Pat.any ()) (Exp.constant (Const_int 1)) (Exp.constant (Const_int length)) Upto (
                Exp.apply ~loc (Exp.ident ({
                    txt = Ldot (Lident "Stream", "junk");
                    loc;
                }))
                [ ("", match_expression) ]
            )) statements)
        ::
            (match other_cases with
            | Some cases -> [Exp.case (Pat.any ()) cases]
            | None -> []
            )
        )
    )
    else (
        statements
    )

let rec transform_match_cases loc match_expression = function
    | [{
        pc_lhs = stream_list;
        pc_rhs = statements;
    }] -> transform_match_case loc match_expression stream_list statements None
    | {
        pc_lhs = stream_list;
        pc_rhs = statements;
    } :: rest ->
        transform_match_case loc match_expression stream_list statements (Some (transform_match_cases loc match_expression rest))

let match_parse loc = function
    | PStr [{
        pstr_desc = Pstr_eval ({
            pexp_loc = loc;
            pexp_desc = Pexp_match (match_expression, match_cases);
        }, _)
    }] ->
        transform_match_cases loc match_expression match_cases

let stream_mapper argv =
    { default_mapper with
        expr = fun mapper expr ->
            match expr with
            (*| { pexp_desc = Pexp_extension ({ txt = "stream"; loc }, pstr)} -> stream_expression loc pstr*)
            | { pexp_desc = Pexp_extension ({ txt = "parse"; loc }, pstr)} -> match_parse loc pstr
            | x -> default_mapper.expr mapper x
    }

let () = run_main stream_mapper
