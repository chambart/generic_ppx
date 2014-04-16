open Asttypes
open! Location
open Parsetree

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module CharSet = Set.Make(Char)

let s c = String.make 1 c

exception Not_known of char

let ident_of_char c = match c with
  | 'a' .. 'z' ->
    s (Char.uppercase c)
  | 'A' .. 'Z' ->
    "U" ^ (s c)
  | '0' .. '9' ->
    "N" ^ (s c)
  | '%' ->
    "Percent"
  | '(' ->
    "Lparen"
  | ')' ->
    "Rparen"
  | '[' ->
    "Lbracket"
  | ']' ->
    "Rbracket"
  | '{' ->
    "Lbrace"
  | '}' ->
    "Rbrace"
  | ' ' ->
    "Space"
  | '\n' ->
    "Newline"
  | '_' ->
    "Underscore"
  | '!' ->
    "Bang"
  | '"' ->
    "Doublequote"
  | '#' ->
    "Sharp"
  | '$' ->
    "Dollar"
  | '&' ->
    "Ampersand"
  | '\\' ->
    "Backslash"
  | '\'' ->
    "Quote"
  | '*' ->
    "Star"
  | '+' ->
    "Plus"
  | ',' ->
    "Comma"
  | '-' ->
    "Minus"
  | '.' ->
    "Dot"
  | '/' ->
    "Slash"
  | ':' ->
    "Colon"
  | c ->
    raise (Not_known c)
    (* Misc.fatal_error (Printf.sprintf "character %c" c) *)

let chars =
  let rec aux i set =
    if i >= 256
    then set
    else aux (i+1) (CharSet.add (Char.chr i) set)
  in
  aux 0 CharSet.empty

let strmap =
  let rec aux i map =
    if i >= 256
    then map
    else
      let c = Char.chr i in
      let map = try
          let s = ident_of_char c in
          StringMap.add s c map
        with _ -> map in
      aux (i+1) map
  in
  aux 0 StringMap.empty

let is_special_case = function
  | ({ txt = "OTHERS" }, _, _, _) -> true
  | _ -> false

exception Missing of string

type ('a,'b) lr =
  | Left of 'a
  | Right of 'b

let remaining_cases strset =
  try
    let set = StringSet.fold (fun s acc ->
        match s with
        | "OTHERS" -> acc
        | "Start" -> acc
        | "End" -> acc
        | _ ->
          try CharSet.add (StringMap.find s strmap) acc
          with Not_found -> raise (Missing s))
        strset CharSet.empty in
    let set = CharSet.diff chars set in
    let aux c set =
      try StringSet.add (ident_of_char c) set
      with _ -> set in
    Left (CharSet.fold aux set StringSet.empty)
  with Missing s -> Right s

let char_of_ident id =
  try StringMap.find id strmap
  with Not_found -> assert false

let loc_char i loc =
  let loc_start =
    { loc.loc_start with
      pos_cnum = loc.loc_start.Lexing.pos_cnum + i + 1 } in
  let loc_end =
    { loc.loc_start with
      pos_cnum = loc.loc_start.Lexing.pos_cnum + i + 2 } in
  { loc with
    loc_start;
    loc_end }

let list_of_string loc s =
  let l = ref [] in
  String.iteri (fun i c -> l := (c,loc_char i loc) :: !l) s;
  List.rev !l

let to_constructors loc s =
  let l = list_of_string loc s in
  let constr loc str expr =
    let lident = mkloc (Longident.Lident str) loc in
    Ast_mapper.E.construct lident (Some expr) false
  in
  let aux (c,loc) expr = constr loc (ident_of_char c) expr in
  let end_constr =
    Ast_mapper.E.construct (mkloc (Longident.Lident "End") loc) None false in
  constr loc "Start" (List.fold_right aux l end_constr)

module Main : sig end = struct

  let is_special_operator s =
    String.length s > 1 && s.[0] = '!'

  let rec replace_last_ident s = function
    | Longident.Lident _ -> Longident.Lident s
    | Ldot(t,_) -> Ldot(t,s)
    | Lapply(t1,t2) -> Lapply(t1,replace_last_ident s t2)


  class char_mapper c = object
    inherit Ast_mapper.mapper as parent
    method! expr expr =
      match expr.pexp_desc with
      | Pexp_construct ({ txt = Lident "CHAR" }, None, _) ->
        let pexp_desc = Pexp_constant (Const_char c) in
        { expr with pexp_desc }
      | _ -> parent#expr expr

  end

  let map_char c expr =
    match expr.pexp_desc with
    | Pexp_construct ({ txt = Lident "CHAR" }, None, _) ->
      let pexp_desc = Pexp_constant (Const_char c) in
      { expr with pexp_desc }
    | _ -> Ast_mapper.E.map (new char_mapper c) expr

  let map_pattern expr pats f =
    let aux (p,e) = function
      | None -> None (* some pattern with the wrong shape *)
      | Some (r,l,set) -> match p.ppat_desc with
        | Ppat_construct ({ txt = lident }, _, _) ->
          begin match Longident.last lident with
            | "OTHERS" ->
              Some (Some (p, e), l, set)
            | s ->
              let set = StringSet.add s set in
              match e.pexp_desc with
              | Pexp_construct ({ txt = Lident "DROP" }, None, _) ->
                Some (r,l,set)
              | _ ->
                Some (r,(p,e)::l,set)
          end
        | _ -> None
    in
    begin match List.fold_right aux pats (Some (None,[],StringSet.empty)) with
      | None -> expr
      | Some (special_pattern, rest, used_cases) ->
        match remaining_cases used_cases with
        | Right s ->
          expr
        | Left remainings ->
          match special_pattern with
          | None -> expr
          | Some (p,e) ->
            let make_case s =
              match p.ppat_desc with
              | Ppat_construct ({ txt = lident } as li, subpat, b) ->
                let ppat_desc =
                  Ppat_construct ({ li with txt = replace_last_ident s lident },
                                  subpat, b) in
                let e = map_char (char_of_ident s) e in
                { p with ppat_desc }, e
              | _ -> assert false
            in
            let cases = StringSet.fold (fun s l -> make_case s :: l) remainings rest in
            { expr with pexp_desc = f cases }
    end

  class mapper = object
    inherit Ast_mapper.mapper as parent
    method! expr expr =
      let expr = parent#expr expr in
      match expr.pexp_desc with
      | Pexp_apply
          ( { pexp_desc =
                Pexp_ident { txt = Lident op_ident } } as
            fun_exp,
            [label,{ pexp_loc; pexp_desc = Pexp_constant (Const_string s) }] )
        when is_special_operator op_ident ->
        { expr with
          pexp_desc =
            Pexp_apply (fun_exp, [label, to_constructors pexp_loc s])}

      | Pexp_match ( cond, pats ) ->
        map_pattern expr pats
          (fun pats -> Pexp_match ( cond, pats ))

      | Pexp_function (label, lab_def, pats) ->
        map_pattern expr pats
          (fun pats -> Pexp_function (label, lab_def, pats))

      | _ -> expr

    method! type_declaration typedecl = match typedecl.ptype_kind with
      | Ptype_abstract | Ptype_record _ -> typedecl

      | Ptype_variant cases ->
        let special_case =
          try Some (List.find is_special_case cases)
          with Not_found -> None in
        match special_case with
        | None -> typedecl
        | Some special_case ->
          let strset = List.fold_left (fun set ({ txt = str }, _, _, _) ->
              StringSet.add str set) StringSet.empty cases in
          match remaining_cases strset with
          | Right s ->
            Misc.fatal_error (Printf.sprintf "case %s is not recognized" s)
          | Left remainings ->
            let (strloc, lst, opt, loc) = special_case in
            let res = List.map (fun case ->
                if is_special_case case
                then
                  StringSet.fold (fun str acc ->
                      ({ strloc with txt = str }, lst, opt, loc)::acc)
                    remainings []
                else [case])
                cases in
            { typedecl with ptype_kind = Ptype_variant (List.flatten res) }
  end

  let mapper = new mapper

  let () = Ast_mapper.main mapper
end
