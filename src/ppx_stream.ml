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

(*
 * TODO: when using Stream.npeek 1, switch to Stream.peek.
 * TODO: allow an attribute like [@repeat] to support infinite streams.
 * TODO: a match%parse in a match%parse does not work.
 *)

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

exception MatchError of Location.t
exception StreamError of Location.t
exception UnexpectedError

type typed_stream =
    | Value of pattern
    | SubStream of pattern

let () =
    Location.register_error_of_exn (fun exn ->
        match exn with
        | MatchError loc ->
                Some (error ~loc "match%parse accepts list of elements of a single element, e.g. match%parse expression with | [\"if\"; \"(\"] | \"return\"")
        | StreamError loc ->
                Some (error ~loc "[%stream] accepts list of elements, e.g. [%stream \"if\"; \"(\"]")
        | _ -> None)

let rec stream_subexpression loc = function
    | Pexp_constant (Const_char _ | Const_int _ | Const_string _ | Const_float _ as constant) ->
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "ising");
                loc;
            }))
            [ ("", Exp.constant constant) ]
    | Pexp_ident _ as stream -> {
        pexp_desc = stream;
        pexp_loc = loc;
        pexp_attributes = [];
    }
    | Pexp_sequence ({
            pexp_loc = loc1;
            pexp_desc = Pexp_ident _;
        } as hd, {
            pexp_loc = loc2;
            pexp_desc = tl;
        }) ->
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "iapp");
                loc;
            }))
            [ ("", hd)
            ; ("", stream_subexpression loc2 tl)
            ]
    | Pexp_sequence (hd, {
            pexp_loc = loc2;
            pexp_desc = tl;
        }) ->
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "icons");
                loc;
            }))
            [ ("", hd)
            ; ("", stream_subexpression loc2 tl)
            ]
    | _ -> raise (StreamError loc)

let stream_expression loc pstr =
    match pstr with
    | PStr [{
        pstr_desc = Pstr_eval ({
            pexp_loc = loc;
            pexp_desc = expression
        }, _)
    }] -> stream_subexpression loc expression
    | PStr [] ->
            Exp.ident ({
                txt = Ldot (Lident "Stream", "sempty");
                loc;
            })
    | _ -> raise (MatchError loc)

let rec list_length loc = function
    | { ppat_desc = Ppat_construct ({
            txt = Lident "::"
        }, Some {
            ppat_desc = Ppat_tuple tuple_list
        })
    } ->
        (match tuple_list with
        | [_; rest] ->
            1 + (list_length loc rest)
        | _ -> raise (MatchError loc)
        )
    | { ppat_desc = Ppat_construct ({
            txt = Lident "[]"
        }, None)
    } -> 0
    | { ppat_desc = Ppat_constant _ } -> 1
    | { ppat_desc = Ppat_any } -> 0
    | _ -> raise (MatchError loc)

let is_list loc = function
    | { ppat_desc = Ppat_construct ({
            txt = Lident "::"
        }, _)
    } -> true
    | { ppat_desc = Ppat_constant _ } -> false
    | { ppat_desc = Ppat_any } -> false
    | _ -> raise (MatchError loc)

let constant_to_option loc constant =
    Pat.construct ({
            txt = Lident "Some";
            loc;
    }) (Some constant)

let type_stream_pattern loc stream_list : typed_stream list list =
    let rec group_similar_pattern acc = function
        | [] -> [List.rev acc]
        | Value _ as value :: rest -> (match acc with
            | Value _ :: _ | [] -> group_similar_pattern (value :: acc) rest
            | SubStream _ :: _ -> List.rev acc :: group_similar_pattern [value] rest
        )
        | SubStream _ as sub_stream :: rest -> (match acc with
            | Value _ :: _ -> List.rev acc :: group_similar_pattern [sub_stream] rest
            | SubStream _ :: _ | [] -> group_similar_pattern (sub_stream :: acc) rest
        )
    in
    let rec type_stream_pattern acc = function
        | { ppat_desc = Ppat_construct ({
                txt = Lident "::"
            }, Some {
                ppat_desc = Ppat_tuple tuple_list
            })
        } ->
            (match tuple_list with
            | [{ ppat_attributes = [({
                    txt = "as"
            }, PStr [{
                pstr_desc = Pstr_eval ({
                    pexp_desc = Pexp_ident {
                        txt = Lident _
                    }
                }, _)
            }])]} as alias; rest] ->
                    type_stream_pattern (SubStream alias :: acc) rest
            | [_ as value; rest] ->
                    type_stream_pattern (Value value :: acc) rest
            | _ -> raise (MatchError loc)
            )
        | { ppat_desc = Ppat_construct ({
                txt = Lident "[]"
            }, None)
        } -> acc
        | _ -> raise (MatchError loc)
    in group_similar_pattern [] (List.rev (type_stream_pattern [] stream_list))

let create_one_match loc match_expression stream_list statements other_cases =
    Exp.match_ (
        Exp.apply ~loc (Exp.ident ({
            txt = Ldot (Lident "Stream", "peek");
            loc;
        }))
        [ ("", match_expression) ]
    )
    (Exp.case (constant_to_option loc stream_list)
        (Exp.sequence (
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "junk");
                loc;
            }))
            [ ("", match_expression) ]
        ) statements)
    ::
        (match other_cases with
        | Some cases -> [Exp.case (Pat.any ()) cases]
        | None -> []
        )
    )

let rec pattern_of_typed_stream_list loc = function
    | [Value value | SubStream value] ->
            Pat.construct ({
                txt = Lident "::";
                loc;
            }) (Some (Pat.tuple
                [ value
                ; Pat.construct {
                    txt = Lident "[]";
                    loc;
                } None
                ])
            )
    | (Value value | SubStream value) :: rest ->
            Pat.construct ({
                txt = Lident "::";
                loc;
            }) (Some (Pat.tuple
                [ value
                ; pattern_of_typed_stream_list loc rest
                ]
            ))
    | [] ->
            Pat.construct {
                txt = Lident "[]";
                loc;
            } None

let rec create_lets loc match_expression statements = function
    | [SubStream ({
            ppat_desc = Ppat_var { txt = sub_stream_name };
             ppat_attributes = [({
                    txt = "as"
            }, PStr [{
                pstr_desc = Pstr_eval ({
                    pexp_desc = Pexp_ident {
                        txt = Lident variable_name
                    }
                }, _)
            }])]
        })] ->
        (Exp.let_ Nonrecursive [(Vb.mk (Pat.var {
            txt = variable_name;
            loc;
        }) (Exp.apply ~loc (Exp.ident ({
            txt = Lident sub_stream_name;
            loc;
        }))
        [ ("", match_expression)]
        )
        )]
        statements)
    | SubStream ({
            ppat_desc = Ppat_var { txt = sub_stream_name };
             ppat_attributes = [({
                    txt = "as"
            }, PStr [{
                pstr_desc = Pstr_eval ({
                    pexp_desc = Pexp_ident {
                        txt = Lident variable_name
                    }
                }, _)
            }])]
        }) :: rest ->
        (Exp.let_ Nonrecursive [(Vb.mk (Pat.var {
            txt = variable_name;
            loc;
        }) (Exp.apply ~loc (Exp.ident ({
            txt = Lident sub_stream_name;
            loc;
        }))
        [ ("", match_expression)]
        )
        )]
        (create_lets loc match_expression statements rest))
    | _ -> raise UnexpectedError

and create_matches loc match_expression typed_stream_list statements other_cases length =
    let rec create_matches = function
        | [SubStream ({
            ppat_desc = Ppat_var { txt = sub_stream_name };
             ppat_attributes = [({
                    txt = "as"
            }, PStr [{
                pstr_desc = Pstr_eval ({
                    pexp_desc = Pexp_ident {
                        txt = Lident variable_name
                    }
                }, _)
            }])]
        }) :: _ as sub_stream_list] -> create_lets loc match_expression statements sub_stream_list
        | (SubStream _ :: _ as sub_stream_list) :: rest -> create_lets loc match_expression (create_matches rest) sub_stream_list
        | [(Value _ :: _ as value_list)] ->
                let stream_list = pattern_of_typed_stream_list loc value_list in
                Exp.match_ (
                    Exp.apply ~loc (Exp.ident ({
                        txt = Ldot (Lident "Stream", "npeek");
                        loc;
                    }))
                    [ ("", Exp.constant (Const_int (List.length value_list)))
                    ; ("", match_expression)
                    ]
                )
                (Exp.case stream_list
                    (Exp.sequence (Exp.for_ (Pat.any ()) (Exp.constant (Const_int 1)) (Exp.constant (Const_int (List.length value_list))) Upto (
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
        | (Value _ :: _ as value_list) :: rest ->
                let stream_list = pattern_of_typed_stream_list loc value_list in
                Exp.match_ (
                    Exp.apply ~loc (Exp.ident ({
                        txt = Ldot (Lident "Stream", "npeek");
                        loc;
                    }))
                    [ ("", Exp.constant (Const_int (List.length value_list)))
                    ; ("", match_expression)
                    ]
                )
                [(Exp.case stream_list
                    ((Exp.sequence (Exp.for_ (Pat.any ()) (Exp.constant (Const_int 1)) (Exp.constant (Const_int (List.length value_list))) Upto (
                            Exp.apply ~loc (Exp.ident ({
                                txt = Ldot (Lident "Stream", "junk");
                                loc;
                            }))
                            [ ("", match_expression) ]
                        )) (create_matches rest)
                    ))
                )]
        | _ -> raise UnexpectedError
    in create_matches typed_stream_list

let transform_match_case loc match_expression stream_list statements other_cases =
    let length = list_length loc stream_list in
    let is_list = is_list loc stream_list in
    if is_list then (
        if length > 0 then (
            let typed_stream_list = type_stream_pattern loc stream_list in
            create_matches loc match_expression typed_stream_list statements other_cases length
        )
        else (
            statements
        )
    )
    else (
        Exp.match_ (
            Exp.apply ~loc (Exp.ident ({
                txt = Ldot (Lident "Stream", "peek");
                loc;
            }))
            [ ("", match_expression) ]
        )
        (Exp.case (constant_to_option loc stream_list)
            (Exp.sequence (
                Exp.apply ~loc (Exp.ident ({
                    txt = Ldot (Lident "Stream", "junk");
                    loc;
                }))
                [ ("", match_expression) ]
            ) statements)
        ::
            (match other_cases with
            | Some cases -> [Exp.case (Pat.any ()) cases]
            | None -> []
            )
        )
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
    | [] -> raise (MatchError loc)

let match_parse loc = function
    | PStr [{
        pstr_desc = Pstr_eval ({
            pexp_loc = loc;
            pexp_desc = Pexp_match (match_expression, match_cases);
        }, _)
    }] ->
        transform_match_cases loc match_expression match_cases
    | PStr [{
        pstr_desc = Pstr_eval ({
            pexp_loc = loc;
            pexp_desc = Pexp_function match_cases;
        }, _)
    }] ->
        Exp.fun_ "" None (Pat.var {
            txt = "__ppx_stream__";
            loc;
        })
        (transform_match_cases loc (Exp.ident {
            txt = Lident "__ppx_stream__";
            loc;
        }) match_cases)
    | _ -> raise (MatchError loc)

let stream_mapper argv =
    { default_mapper with
        expr = fun mapper expr ->
            match expr with
            | { pexp_desc = Pexp_extension ({ txt = "stream"; loc }, pstr)} -> stream_expression loc pstr
            | { pexp_desc = Pexp_extension ({ txt = "parse"; loc }, pstr)} -> match_parse loc pstr
            | x -> default_mapper.expr mapper x
    }

let () = run_main stream_mapper
