(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(* CF mli *)

(* depends *)
module Format = Base.Format
module List = Base.List
module String = Base.String

(* alias *)
module QCons = QmlAstCons.UntypedExpr

(* shorthands *)
module Q = QmlAst

(* refactoring in progress *)

(* -- *)

let debug fmt =
  OManager.printf ("@{<cyan>[JsSerialize]@}@ @[<2>"^^fmt^^"@]@.")

module JsIdent =
struct
  module Hashtbl = Base.Hashtbl

  let table = ( Hashtbl.create 16 : (string, QmlAst.ident * QmlAst.code_elt) Hashtbl.t )

  let resolve cident =
    let string = JsPrint.string_of_ident (JsCons.Ident.ident cident) in
    match Hashtbl.find_opt table string with
    | Some ident ->
        (*
          An opa toplevel identifier, previously generated by the lines below.
        *)
        fst ident
    | None ->
      let ident = Ident.next string in
      let () =
        let def = QCons.ident (OpaMapToIdent.val_ Opacapi.JsIdent.define_rename) in
        let apply = QCons.apply def [QCons.directive (`tagged_string (string, Q.Client_closure_use)) [] []] in
        let code_elt = Q.NewVal(Annot.nolabel "Serializer.JsIdent.resolve", [ident, apply]) in
        Hashtbl.add table string (ident, code_elt)
      in
      ident

  let get_toplevel_declarations () =
    let fold _name (_, code_elt) acc = code_elt :: acc in
    let res = Hashtbl.fold fold table [] in
    Hashtbl.clear table ;
    res

  let is_toplevel_declaration ident =
    Return.set_checkpoint (
      fun label ->
        Hashtbl.iter (
          fun _ (ident', _) ->
            if Ident.equal ident ident' then Return.return label true
        ) table ;
        false
    )
end


module JsSerializer =
struct
  (* shorthand *)
  module J = JsAst

  (*
    cf package stdlib.js

    The following ast is the exact replication of the runtime opa ast.
    We use this structure for generating the runtime ast, before serializing it.
  *)

  type jsast_ident = string

  type jsast_mini_expr =
    | Verbatim of string
    | Ident of jsast_ident
    | Expr of QmlAst.expr
    | SetDistant of jsast_ident list
    | TypeUse of string
    | TypeDef of string
    | RpcUse of string
    | RpcDef of string

  type jsast_key_ident =
    | KI_key of jsast_ident
    | KI_ident of jsast_ident

  type jsast_code_elt = {
    ident : jsast_key_ident ;
    definition : [ `Rpc of string | `Type of string | `Nothing ];
    root : bool ;
    content : jsast_mini_expr list ;
  }

  type jsast_code = {
    ast : jsast_code_elt list ;
    plugin : string option ;
  }

  (*
    A printer, just for debugging
  *)

  let pp_mini_expr fmt = function
    | Verbatim s -> Format.fprintf fmt "{verbatim:%S}" s
    | Ident s -> Format.fprintf fmt "{ident:%S}" s
    | Expr e -> Format.fprintf fmt "{expr:%a}" QmlPrint.pp#expr e
    | SetDistant idents -> Format.fprintf fmt "{set_distant : [%a]}" (Format.pp_list ",@ " Format.pp_print_string) idents
    | TypeUse s -> Format.fprintf fmt "{TypeUse: %s}" s
    | TypeDef s -> Format.fprintf fmt "{TypeDef: %s}" s
    | RpcUse s -> Format.fprintf fmt "{RpcUse: %s}" s
    | RpcDef s -> Format.fprintf fmt "{RpcDef: %s}" s

  let pp_key_ident fmt = function
    | KI_key s -> Format.fprintf fmt "{key:%S}" s
    | KI_ident s -> Format.fprintf fmt "{ident:%S}" s

  let pp_definition fmt = function
    | `Rpc s -> Format.fprintf fmt "`Rpc %s" s
    | `Type s -> Format.fprintf fmt "`Type %s" s
    | `Nothing -> Format.fprintf fmt "`Nothing"

  let pp_code_elt fmt elt =
    Format.fprintf fmt (
      "@[<2>{@\nident: %a ;@\nroot: %a@\ndefinition: %a@\n@[<2>content: %a@]@]@\n}"
    )
      pp_key_ident elt.ident
      Format.pp_print_bool elt.root
      pp_definition elt.definition
      (Format.pp_list ",@ " pp_mini_expr) elt.content

  let pp_code fmt code =
    let i = ref 0 in
    let pp_code_elt fmt elt =
      Format.fprintf fmt "elt %d@\n" !i ;
      incr(i) ;
      pp_code_elt fmt elt
    in
    Option.iter (Format.fprintf fmt "%s@\n") code.plugin;
    Format.pp_list "@\n" pp_code_elt fmt code.ast

  module X =
  struct
    type lexem = jsast_mini_expr
    type t = lexem list
    let append t lexem = lexem :: t
    let empty = []
      (* *)
    let ident s = Ident s
    let verbatim s = Verbatim s
    let qml e = Expr e
    let serialized = function
      | JsAstRuntime.SetDistant idents -> [SetDistant (List.map JsPrint.string_of_ident idents)]
      | JsAstRuntime.TaggedString (string, kind) ->
          (* escaping the string now allows us not to escape it at runtime
           * same escaping as in pass_GenerateServerAst *)
          let string = JsPrint.escape_string string in
          (match kind with
           | Q.Type_use -> [TypeUse string]
           | Q.Type_def -> [TypeDef string]
           | Q.Rpc_use -> [RpcUse string]
           | Q.Rpc_def -> [RpcDef string]
           | Q.Client_closure_use -> assert false)
  end

  module S = JsPrint.Make ( X )

  (*
    Function used to tag toplevel applications.
  *)
  let pure_funs = ref IdentSet.empty (* FIXME: dirty -> should propagate an env instead *)

  (*
    <!> BEWARE, if a new expr case appear in the ast, and potentially executed,
    we may have to change the exists for itering statements inside expressions.
    Currently for this particulary check (side effects), we should not enter inside
    statements because statements in expression are in function definition only,
    which are statements not executed.
  *)
  let is_register_expr expr =
    JsWalk.OnlyExpr.exists (function
    | J.Je_runtime (_, JsAstRuntime.TaggedString (_, (QmlAst.Rpc_def | QmlAst.Type_def)) ) -> true
    | _ -> false) expr


  let is_register_element elt =
    JsWalk.OnlyStatement.exists (function
      | J.Js_var (_, _, Some e)
      | J.Js_return (_, Some e)
      | J.Js_expr (_, e) -> is_register_expr e
      | _ -> false
    ) elt

  let is_pure = function
    | J.ExprIdent ident -> IdentSet.mem ident !pure_funs
    | _ -> false

  let add_pure_funs = function
    | J.ExprIdent ident -> pure_funs := IdentSet.add ident !pure_funs
    | _ -> ()

  let is_in_local_vars local_vars = function
   | J.Je_ident(_,J.ExprIdent ident) -> IdentSet.mem ident local_vars
   | J.Je_ident(_,J.Native( `local, str) )-> IdentSet.mem (Ident.source str) local_vars
   | _ -> false

  let add_local_vars local_vars = function
    | J.ExprIdent ident -> IdentSet.add ident local_vars
    | J.Native( `local, str) -> IdentSet.add (Ident.source str) local_vars
    | _ -> local_vars

  exception Return_true

  (* side_effect comment :
     if the side effect of an operator, changes a local variable there is no external (function) side-effect
     since assignment op are widely used by the code generator to implement perfectly pure local environment
     we need to handle this *)
  let rec is_side_effect_expr ~local_vars expr =
    JsWalk.OnlyExpr.exists (function
    | J.Je_unop (_, unop, e) when JsAst.is_side_effect_unop unop ->
      is_side_effect_expr ~local_vars e || not(is_in_local_vars local_vars e)

    | J.Je_binop (_, binop, e1, e2) when JsAst.is_side_effect_binop binop ->
      is_side_effect_expr ~local_vars e1 || is_side_effect_expr ~local_vars e2
      || not(is_in_local_vars local_vars e1)

    | J.Je_call (_, f, args, pure) ->
      let side_effect_fun = match f with
        | J.Je_ident (_, ident) when not pure -> not (is_pure ident)
        (* applied anonymous function <=> block of code,
           e.g. created by the code generator, to split big datastruture,
           or toplevel val with local environment *)
        | J.Je_function (_, _, _, body) -> is_side_effect ~local_vars body
        | _ -> not pure
      in side_effect_fun || List.exists (is_side_effect_expr ~local_vars) args

    | J.Je_runtime (_, e) -> (
      match e with
      | JsAstRuntime.SetDistant _ -> true
      | JsAstRuntime.TaggedString _ -> false
    )

    | _ -> false
    ) expr

  (* TODO, block statement in non toplevel mode *)
  and is_side_effect_stmt ~toplevel ~local_vars stmt =
    (* the problem with statement is that when you have {var x = 1; var y = 2}
     * then in the ast, you do not say that it defines x
     * (you can't even say it, you have only one definition for per code element in the ast)
     * so these blocks look like they are never used, and get cleaned
     * that's why toplevel statement having local_vars are considered as root (see snd_ ) *)
    let snd_ = if toplevel
      then fun (local_vars,b) ->  not(IdentSet.is_empty local_vars) || b
      else snd
    in
    let se_opt_expr e = Option.default_map false (is_side_effect_expr ~local_vars) e in
    let se_stmt stmt = snd_ (is_side_effect_stmt ~toplevel ~local_vars stmt) in
    let se_opt_stmt stmt = Option.default_map false se_stmt stmt in
    match stmt with
        | J.Js_var (_, ident, expr) -> (
          let _ = match expr with
            | Some(J.Je_function (_, _, _, body)) when not(is_side_effect ~local_vars body) -> add_pure_funs ident;
            | _ -> ()
          in
          add_local_vars local_vars ident, (match expr with
          | Some(expr) ->  is_side_effect_expr ~local_vars expr
          | None -> false)
        )

        | J.Js_function (_, ident, _, body) ->
          if not(is_side_effect body) then add_pure_funs ident;
          add_local_vars local_vars ident,false

        | J.Js_return (_, Some e)
        | J.Js_expr (_, e) ->
          local_vars,is_side_effect_expr ~local_vars e

        (* this case aren't supposed to happen at toplevel, however they can appear
         * when looking at the body of a function *)
        | J.Js_return (_, None)
        | J.Js_break _
        | J.Js_continue _ ->
            local_vars,false

        | J.Js_comment _ ->
            (*
              We want to keep all toplevel comments in debug mode, so we considerate them as root,
              the minimifier will removes comments anyway if the server is not in debug js.
            *)
            local_vars, toplevel

        | J.Js_switch(_, e, cases, default) ->
          let se_case (e,stmt) = se_stmt stmt
                              || is_side_effect_expr ~local_vars e in
          local_vars,
          se_opt_stmt default
          || is_side_effect_expr ~local_vars e
          || List.exists se_case cases

        | J.Js_if(_, e, then_, opt_else) ->
          local_vars,
          is_side_effect_expr ~local_vars e
          || snd_ (is_side_effect_stmt ~toplevel ~local_vars then_)
          || se_opt_stmt opt_else

        | J.Js_dowhile(_, stmt, e)
        | J.Js_while(_, e ,stmt)
        | J.Js_with(_, e ,stmt) ->
          local_vars,
          is_side_effect_expr ~local_vars e
          || snd_ (is_side_effect_stmt ~toplevel ~local_vars stmt)

        | J.Js_for(_, oe1, oe2, oe3, stmt) ->
          local_vars,
          se_opt_expr oe1
          || se_opt_expr oe2
          || se_opt_expr oe3
          || snd_ (is_side_effect_stmt ~toplevel ~local_vars stmt)


        | J.Js_forin(_, e1, e2, stmt) ->
          local_vars,
          is_side_effect_expr ~local_vars e1
          || is_side_effect_expr ~local_vars e2
          || snd_ (is_side_effect_stmt ~toplevel ~local_vars stmt)

        | J.Js_block(_, stmt_list) -> local_vars, List.exists se_stmt stmt_list

        | J.Js_label _ -> local_vars, false

        (*
          The rest is currently not supposed to happens, because they are not elemts
          generated by the js back-end, but may in the future be used (parsing and cleaning jsbsl)
        *)
        | J.Js_empty _
        | J.Js_throw _
        | J.Js_trycatch _
          ->
            (* TODO *)
            local_vars,true

  (* side effect on local vars are ignored *)
  and is_side_effect ?(toplevel=false) ?(local_vars=IdentSet.empty) (elt:J.statement list) =
    try
      let (_,bool) = List.fold_left (fun (local_vars,bool) stmt -> if bool then raise Return_true
        else is_side_effect_stmt ~toplevel ~local_vars stmt
      ) (local_vars,false) elt
      in bool
    with Return_true -> true

  let serialize
      ~client_roots
      ?(key=false)
      ( elt : JsAst.code_elt ) =
    let ident, exprident =
      let jsident =
        match elt with
        | J.Js_var (_, ident, _)
        | J.Js_function (_, ident, _, _) ->
            ident
        | J.Js_comment (_, _) ->
            J.ExprIdent (Ident.next "comment")
        | _ ->
            (*
              There: It is about toplevel statement parsed in the bsl.
              There is an extra management of unicity keys, so these elements
              will not be duplicated thanks to the unicity keys runtime cleaning.
              We can put a dummy identifier for these statements.
            *)
            J.ExprIdent (Ident.next "toplevel_statement")
      in
      match jsident with
      | J.ExprIdent exprident ->
          JsPrint.string_of_ident jsident, exprident
      | J.Native (_, native) ->
          native, Ident.next native
    in
    let rev_list = S.code_elt elt in
    let definition, content =
      (* reversing the list and looking for typedef and rpcdef at the same time *)
      let rec aux def_kind acc = function
        | [] -> def_kind, acc
        | h :: t ->
            let def_kind =
              match h with
              | TypeDef string -> assert (def_kind = `Nothing); `Type string
              | RpcDef string -> assert (def_kind = `Nothing); `Rpc string
              | TypeUse _
              | RpcUse _
              | SetDistant _
              | Expr _
              | Ident _
              | Verbatim _ -> def_kind in
            aux def_kind (h :: acc) t in
      aux `Nothing [] rev_list in
    (* registering a type name of a rpc doesn't count as a side effect or else you can't clean anything
     * the runtime cleaning looks for more detailed dependencies to see if it will be kept or not *)
    let root =
      IdentSet.mem exprident client_roots || (definition = `Nothing && (is_side_effect ~toplevel:true [elt]) && not(is_register_element elt))
    in
    (*
      Adding key unicity for registration
    *)
    let ident =
      match key with
      | true -> KI_key ident
      | false ->
          match exprident with
          | Ident.FakeSource _ -> KI_key ident
          | Ident.Source _ | Ident.Internal _ -> KI_ident ident
    in
    {
      ident;
      root;
      definition;
      content;
    }
end

module QmlSerializer =
struct

  module S = JsSerializer

  let cons = ref QmlAstCons.untyped_cons

  let label = Annot.nolabel "JsSerializer"

  module JsAstLabel =
  struct
    (* Meta infos, corresponding to the module JsAst of the stdlib *)
    let content = "c"
    let root = "r"

    let declaration = "d"
    let definition = "d"
    let key = "k"

    (* JsAst.mini_expr *)
    let ident = "i"

    let verbatim = "v"

    let set_distant = "s"

    let type_use = "tu"
    let type_def = "td"

    let rpc_use = "ru"
    let rpc_def = "rd"

    (* ServerAst.definition *)
    let rpc = "r"
    let typ = "t"

  end

  module AdHocSerialize =
  struct
    let ser_int b i =
      (* we need to make sure that the length of an integer is fixed (or predictable at least) *)
      (* big bytes first *)
      for j = 3 downto 0 do
        Buffer.add_char b (Char.chr ((i lsr (j*8)) mod 256));
      done
    let ser_string b s =
      ser_int b (String.length s);
      Buffer.add_string b s
    let ser_key_ident b = function
      | S.KI_key key -> Buffer.add_char b '\000'; ser_string b key
      | S.KI_ident ident -> Buffer.add_char b '\001'; ser_string b ident
    let ser_root b = function
      | false -> Buffer.add_char b '\000'
      | true -> Buffer.add_char b '\001'
    let ser_mini_expr ((b,l) as acc) = function
      | S.Verbatim s -> Buffer.add_char b '\000'; ser_string b s; acc
      | S.Ident s -> Buffer.add_char b '\001'; ser_string b s; acc
      | S.Expr e ->
          Buffer.add_char b '\002';
          let string = !cons#string (Buffer.contents b) in
          Buffer.reset b;
          let l = `expr e :: `string string :: l in
          (b,l)
      | S.SetDistant idents ->
          Buffer.add_char b '\003';
          ser_int b (List.length idents);
          List.iter (ser_string b) idents;
          acc
      | S.RpcDef string ->
          Buffer.add_char b '\004';
          ser_string b string;
          acc
      | S.RpcUse string ->
          Buffer.add_char b '\005';
          ser_string b string;
          acc
      | S.TypeDef string ->
          Buffer.add_char b '\006';
          ser_string b string;
          acc
      | S.TypeUse string ->
          Buffer.add_char b '\007';
          ser_string b string;
          acc
    let ser_definition b = function
      | `Nothing -> Buffer.add_char b '\000'
      | `Rpc string -> Buffer.add_char b '\001'; ser_string b string
      | `Type string -> Buffer.add_char b '\002'; ser_string b string
    let ser_content ((b,_) as acc) l =
      ser_int b (List.length l);
      List.fold_left ser_mini_expr acc l
    let ser_code_elt ((b,_) as acc) {S.content; definition; ident; root} =
      let acc = ser_content acc content in
      ser_definition b definition;
      ser_key_ident b ident;
      ser_root b root;
      acc
    let ser_code ((b,_) as acc) {JsSerializer. ast=l; plugin=_plugin} =
      ser_int b (List.length l);
      List.fold_left ser_code_elt acc l
    let ser_ast ((b,_) as acc) l =
      ser_int b (List.length l);
      List.fold_left ser_code acc l
    let ser_ast l =
      let b = Buffer.create 20000 in
      let acc = (b, []) in
      let (_,l) = ser_ast acc l in
      let l =
        if Buffer.length b = 0 then l else
          let string = !cons#string (Buffer.contents b) in
          `string string :: l in
      let idents = List.map (fun _ -> Ident.next "adhoc") l in
      let code_elts =
        List.map2
          (fun ident e ->
             match e with
             | `string e
             | `expr e -> Q.NewVal (label, [ident, e]))
          idents l in
      let tystring = Q.TypeConst Q.TyString in
      let ty_sl = Q.TypeArrow ([tystring], tystring) in
      let rev_list =
        List.concat_map2
          (fun e ident ->
             let gen_ident () = !cons#ident ident tystring in
             let r =
               match e with
               | `string _ -> []
               | `expr _ ->
                   let sl = !cons#ident
                     (OpaMapToIdent.val_ Opacapi.Client_code.serialize_string_length) ty_sl in
                   [!cons#apply sl [gen_ident ()]]
             in
             gen_ident () :: r
          ) l idents in
      #<If:JS_SERIALIZE$contains "overhead">
      let r = ref 0 in
      let count =
        List.fold_left
          (fun count -> function
           | `expr _ -> count
           | `string (Q.Const (_, Q.String string)) ->
                 for i = 0 to String.length string - 1 do
                   if string.[i] < '\005' then incr r
                 done;
                 count + String.length string
           | `string _ -> assert false
          ) 0 l in
      Printf.printf "length: %d, overhead: %d, %.2f%%\n%!" count !r (100. *. float !r /. float count);
      #<End>;
      code_elts, !cons#record ["adhoc", !cons#list (List.rev rev_list); "package_", !cons#string (ObjectFiles.get_current_package_name ())]
  end

  (** {6 Nodes} *)
  (**
     We can extend this interface, if we need more precise js ast at runtime.
     Invariant: in the returned t, there are no 2 successives lexem verbatim.
  *)

  let list gen_a l =
    !cons#directive `llarray (List.map gen_a l) []

  let ident string =
    let string = !cons#string string in
    !cons#record [JsAstLabel.ident, string]

  let key string =
    let string = !cons#string string in
    !cons#record [JsAstLabel.key, string]

  let key_ident key ident =
    let key = !cons#string key in
    let ident = !cons#string ident in
    !cons#record [
      JsAstLabel.key, key ;
      JsAstLabel.ident, ident ;
    ]

  let verbatim string =
    let string = !cons#string string in
    !cons#record [JsAstLabel.verbatim, string]

  let qml qml =
    !cons#record [JsAstLabel.verbatim, qml]

  let set_distant ls =
    !cons#record [JsAstLabel.set_distant, list !cons#string ls]

  let type_use s =
    !cons#record [JsAstLabel.type_use, !cons#string s]

  let type_def s =
    !cons#record [JsAstLabel.type_def, !cons#string s]

  let rpc_use s =
    !cons#record [JsAstLabel.rpc_use, !cons#string s]

  let rpc_def s =
    !cons#record [JsAstLabel.rpc_def, !cons#string s]

  let mini_expr = function
    | S.Verbatim s -> verbatim s
    | S.Ident s -> ident s
    | S.Expr e -> qml e
    | S.SetDistant ls -> set_distant ls
    | S.TypeDef s -> type_def s
    | S.TypeUse s -> type_use s
    | S.RpcUse s -> rpc_use s
    | S.RpcDef s -> rpc_def s

  let declaration string =
    let string = !cons#string string in
    !cons#record [JsAstLabel.declaration, string]

  let definition d = match d with
    | `Nothing -> !cons#cheap_void
    | `Rpc s   -> !cons#record ["r", !cons#string s]
    | `Type s  -> !cons#record ["t", !cons#string s]

  (*
    Possibly optimized in the future.
    Returns a list of declarations, and the expression.
  *)
  let code_elt elt =
    let ident =
      match elt.S.ident with
      | S.KI_key k -> key k
      | S.KI_ident i -> ident i
    in
    let root =
      let value = !cons#bool elt.S.root in
      let bypass = QCons.bypass Opacapi.Opabsl.BslReference.create in
      let apply = QCons.apply bypass [value] in
      apply
    in
    let content =
      let content = elt.S.content in
      QCons.directive `llarray (List.map mini_expr content) []
    in
    let code_elt =
      !cons#record [
        JsAstLabel.ident,      ident ;
        JsAstLabel.root,       root ;
        JsAstLabel.content,    content ;
        JsAstLabel.definition, definition elt.S.definition;
      ]
    in
    code_elt

  let code code = list code_elt code

  let ast c =
    list
      (function
       | {JsSerializer. ast; plugin=None} ->
           !cons#record ["ast", code ast]
       | {JsSerializer. ast; plugin=Some plugin} ->
           !cons#record ["ast", code ast; "plugin", !cons#string plugin])
      c

  (*
    The dependencies of the generated code is hard to predict,
    because of Hole and DynamicExpr contained in it.
    We use this function for computing the set of dependencies.
  *)
  let get_deps acc e =
    QmlAstWalk.Expr.fold
      (fun acc e ->
         match e with
         | Q.Ident (_,i) -> IdentSet.add i acc
         | _ -> acc
      )
      acc
      e

  let pull_expr_out code =
    List.fold_left
      (fun (outs, code) elt ->
         let defs, content =
           List.fold_left
             (fun (outs, content) elt -> match elt with
              | S.Expr e ->
                  let iout = Ident.next "jsout" in
                  let iexp = !cons#ident iout (Q.TypeConst Q.TyString) in
                  ((iout, e)::outs, S.Expr iexp :: content)
              | e -> (outs, e::content)
             ) ([], []) elt.S.content
         in let content = List.rev content
         in defs@outs, {elt with S.content}::code
      ) ([], []) code.JsSerializer.ast

  let insert_code ~kind ( js_code : JsSerializer.jsast_code list ) ( server_code : QmlAst.code ) =
    let () =
      #<If:JS_SERIALIZE>
        let outputer oc js_code =
          let fmt = Format.formatter_of_out_channel oc in
          Format.pp_list "@\n" JsSerializer.pp_code  fmt js_code
        in
        let _ = PassTracker.file ~filename:"js_serialize" outputer js_code in
        ()
      #<End>
    in
    let insert =
      match kind with
      | `adhoc ->
          let register_js_code = OpaMapToIdent.val_ Opacapi.Client_code.register_js_code in
          let register_js_code = QCons.ident register_js_code in
          (* the order in code_elts doesn't matter *)
          let code_elts, e = AdHocSerialize.ser_ast js_code in
          let register_call = !cons#apply register_js_code [ e ] in
          List.rev (Q.NewVal (label, [ Ident.next "js_code", register_call ]) :: code_elts)
      | `ast ->
          let register_js_code =
            OpaMapToIdent.val_ Opacapi.Client_code.register_js_code
          in
          let register_js_code = QCons.ident register_js_code in
          (* This is needed because we want that the js code registering be
             skipped by cps *)
          let outs, js_code =
            List.fold_left
              (fun (aouts, acode) code ->
                 let outs, ast = pull_expr_out code in
                 aouts@outs, {code with JsSerializer.ast}::acode
              ) ([], []) js_code
          in
          let js_code = ast (List.rev js_code) in
          let js_code = !cons#record ["ast", js_code] in
          let register_call = !cons#apply register_js_code [ js_code ] in
          let register_elt = Ident.next "register_js_ast", register_call in
          let insert = Q.NewVal (label, List.rev( register_elt :: outs )) in
          [ insert ]
    in
    let deps = QmlAstWalk.CodeExpr.fold get_deps IdentSet.empty insert in
    QmlAstUtils.Code.insert ~deps ~insert server_code
end
