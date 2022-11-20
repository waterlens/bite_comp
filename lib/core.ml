open Result
open Printf

type var = Named of string | Annotated of var * ty

and ty =
  | TVar of var
  | TCtor of ctor * ty list
  | TApp of ty * ty
  | TForall of string * ty
  | TArrow of ty * ty
  | TLit of tlit
  | TNever

and tlit = TInt | TBool
and core = Core of binding list

and binding =
  | Rec of (var * expr) list
  | NonRec of (var * expr)
  | RecType of (var * ty) list

and expr =
  | Var of var
  | Lam of var * expr
  | App of expr * expr
  | Ctor of ctor * expr list
  | Lit of literal
  | Let of binding * expr
  | Case of expr * (pattern * expr) list
  | Mark of var * expr
  | Goto of var * expr

and pattern = PCon of ctor * var list | PWild
and ctor = CtUnit | CtHandle | CtReturn | CtTuple | CtCont | CtUnion
and literal = Int of int | Bool of bool

type arity = Any | None | Number of int

exception Exn of string

let ctor_of_string = function
  | "unit" -> CtUnit
  | "return" -> CtReturn
  | "handle" -> CtHandle
  | "tuple" -> CtTuple
  | "cont" -> CtCont
  | "union" -> CtUnion
  | _ -> raise @@ Exn "not a ctor"

let tlit_of_string = function
  | "bool" -> TBool
  | "int" -> TInt
  | _ -> raise @@ Exn "not a type literal"

let string_of_ctor = function
  | CtUnit -> "#unit"
  | CtReturn -> "#return"
  | CtHandle -> "#handle"
  | CtTuple -> "#tuple"
  | CtCont -> "#cont"
  | CtUnion -> "#union"

let aux_show_list f l = String.concat " " @@ List.map f l

let rec show_ty = function
  | TVar var -> show_var var
  | TCtor (ctor, tys) ->
      sprintf "%s [%s]" (string_of_ctor ctor) (aux_show_list show_ty tys)
  | TApp (t1, t2) -> sprintf "(%s %s)" (show_ty t1) (show_ty t2)
  | TForall (name, ty) -> sprintf "\\%s. (%s)" name (show_ty ty)
  | TArrow (t1, t2) -> sprintf "%s -> %s" (show_ty t1) (show_ty t2)
  | TLit TInt -> "%int"
  | TLit TBool -> "%bool"
  | TNever -> "!"

and show_var = function
  | Named s -> s
  | Annotated (s, ty) -> sprintf "(%s: %s)" (show_var s) (show_ty ty)

let show_ty_ctx =
  aux_show_list (fun (s, t) -> sprintf "%s : %s\n" s @@ show_ty t)

let arity_of_ctor = function
  | CtUnit -> None
  | CtHandle -> Any
  | CtReturn -> Number 1
  | CtTuple -> Any
  | CtCont -> Number 2
  | CtUnion -> Any

let arity_satisfy x = function
  | None when x = 0 -> true
  | Any -> true
  | Number y when x = y -> true
  | _ -> false

let ( let* ) = bind
let ( let+ ) x f = map f x

let rec iterM f = function
  | [] -> ok ()
  | a :: l ->
      let* () = f a in
      let+ t = iterM f l in
      t

let rec mapM f = function
  | [] -> ok []
  | a :: l ->
      let* r = f a in
      let+ t = mapM f l in
      r :: t

let rec map2M f l1 l2 =
  match (l1, l2) with
  | [], [] -> ok []
  | a1 :: l1, a2 :: l2 ->
      let* r = f a1 a2 in
      let+ t = map2M f l1 l2 in
      r :: t
  | _, _ -> invalid_arg "map2M"

let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      let r = f a1 a2 a3 in
      r :: map3 f l1 l2 l3
  | _, _, _ -> invalid_arg "map3"

let rec map3M f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> ok []
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      let* r = f a1 a2 a3 in
      let+ n = map3M f l1 l2 l3 in
      r :: n
  | _, _, _ -> invalid_arg "map3"

let rec mapiM i f = function
  | [] -> ok []
  | a :: l ->
      let* r = f i a in
      let+ n = mapiM (i + 1) f l in
      r :: n

let mapiM f l = mapiM 0 f l

let rec fold_leftM f accu l =
  match l with
  | [] -> ok accu
  | a :: l ->
      let* n = f accu a in
      fold_leftM f n l

type fail_reason =
  | NotApplicable of string
  | WrongInfo of string
  | LackInfo of string

let wrong_info x = WrongInfo x
let lack_info x = LackInfo x
let not_applicable x = NotApplicable x

let cexpr_of_ctor = function
  | CtUnit -> ok @@ C.lit @@ C.lint 1
  | CtReturn -> ok @@ C.lit @@ C.lint 2
  | CtHandle -> ok @@ C.lit @@ C.lint 3
  | CtTuple -> ok @@ C.lit @@ C.lint 4
  | CtCont -> ok @@ C.lit @@ C.lint 5
  | CtUnion -> error @@ not_applicable "union should not be nested"

let show_fail_reason = function
  | NotApplicable s -> s
  | WrongInfo s -> s
  | LackInfo s -> s

let rec name_of_var = function
  | Named s -> s
  | Annotated (v, _) -> name_of_var v

let type_of_var ctx = function
  | Named s ->
      Option.to_result
        ~none:
          (lack_info
          @@ sprintf "can't determine the type of %s\nctx:\n %s" s
          @@ show_ty_ctx ctx)
      @@ List.assoc_opt s ctx
  | Annotated (_, ty) -> ok ty

let rec type_of_var' ctx var =
  let+ t = type_of_var ctx var in
  expand_type_var ctx t

and expand_type_var ctx = function
  | TVar var as ty -> value ~default:ty @@ type_of_var ctx var
  | _ as ty -> ty

let rec equal_ty ctx t1 t2 =
  let ( =* ) = equal_ty ctx in
  match (t1, t2) with
  | TVar v1, TVar v2 when name_of_var v1 = name_of_var v2 -> true
  | _ -> (
      match (expand_type_var ctx t1, expand_type_var ctx t2) with
      | TCtor (c1, ts1), TCtor (c2, ts2) ->
          c1 = c2
          && List.length ts1 = List.length ts2
          && List.for_all2 ( =* ) ts1 ts2
      | TApp (t1, t2), TApp (t3, t4) -> t1 =* t3 && t2 =* t4
      | TForall (v1, t1), TForall (v2, t2) -> v1 = v2 && t1 =* t2
      | TArrow (t1, t2), TArrow (t3, t4) -> t1 =* t3 && t2 =* t4
      | TNever, TNever -> true
      | TLit l1, TLit l2 -> l1 = l2
      | _ -> false)

let filter_never_type =
  List.filter (fun x -> match x with TNever -> false | _ -> true)

let rec exhaust ctx branches types exhausted =
  match branches with
  | [] ->
      if exhausted then (true, [])
      else if [] = filter_never_type types then (true, types)
      else (false, types)
  | (pat, _) :: xs -> (
      match pat with
      | PWild -> exhaust ctx xs types true
      | PCon (ctor, _) ->
          exhaust ctx xs
            (List.filter
               (fun x ->
                 match x with
                 | TCtor (ctor', _) when ctor = ctor' -> false
                 | _ -> true)
               types)
            false)

let check_bind eq t1 t2 =
  match (t1, t2) with
  | (Ok t1 as t), Ok t2 when eq t1 t2 -> t
  | (Ok _ as t), Error (LackInfo _) -> t
  | Error (LackInfo _), (Ok _ as t) -> t
  | _, (Error _ as e) -> e
  | (Error _ as e), _ -> e
  | Ok _, Ok _ ->
      error
      @@ wrong_info
           "the annotated type is not equal with checked type of expression"

let types_of_vars ctx =
  mapM (fun (var, _) ->
      let+ ty = type_of_var ctx var in
      let name = name_of_var var in
      (name, ty))

let check_and_update_pattern_bindings ctx vars tys eq =
  let+ tys =
    map2M (check_bind eq) (List.map (type_of_var ctx) vars) (List.map ok tys)
  in
  let bindings = List.map2 (fun var ty -> (name_of_var var, ty)) vars tys in
  bindings @ ctx

let check_pattern_var_num vars tys =
  if List.length vars <> List.length tys then
    error @@ wrong_info
    @@ sprintf "wrong number of pattern variables: expected %d, got %d"
         (List.length tys) (List.length vars)
  else ok ()

let match_ctor_with_types types ctor =
  let+ ty =
    Option.to_result
      ~none:
        (wrong_info
        @@ sprintf "can't found type for ctor: %s"
        @@ string_of_ctor ctor)
    @@ List.find_opt
         (fun ty ->
           match ty with
           | TCtor (ctor', _) when ctor = ctor' -> true
           | _ -> false)
         types
  in
  ty

let bind_type_in_pattern ctx pat ty =
  let eq = equal_ty ctx in
  match pat with
  | PWild -> ok ctx
  | PCon (ctor, vars) -> (
      match ty with
      | TCtor (ctor', tys) when ctor = ctor' ->
          let* _ = check_pattern_var_num vars tys in
          check_and_update_pattern_bindings ctx vars tys eq
      | _ -> error @@ wrong_info "not a correct ctor type")

let bind_type_in_union_pattern ctx pat types =
  let eq = equal_ty ctx in
  match pat with
  | PWild -> ok ctx
  | PCon (ctor, vars) -> (
      let* ty = match_ctor_with_types types ctor in
      match ty with
      | TCtor (ctor', tys) when ctor = ctor' ->
          let* _ = check_pattern_var_num vars tys in
          check_and_update_pattern_bindings ctx vars tys eq
      | _ -> error @@ wrong_info "not a correct ctor type")

let rec type_of_expr' ctx expr =
  let+ t = type_of_expr ctx expr in
  expand_type_var ctx t

and type_of_expr ctx =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  function
  | Var v -> type_of_var ctx v
  | Lam (x, b) ->
      let* xt = type_of_var ctx x in
      let* bt = type_of_expr ((name_of_var x, xt) :: ctx) b in
      ok @@ TArrow (xt, bt)
  | App (x, y) -> (
      let* xt = type_of_expr' ctx x in
      match xt with
      | TArrow (a, b) ->
          let* ty = type_of_expr ctx y in
          if ty =* a then ok b else error @@ wrong_info "wrong type of argument"
      | _ -> error @@ wrong_info "not an applicable type")
  | Ctor (ctor, xs) ->
      if not @@ arity_satisfy (List.length xs) @@ arity_of_ctor ctor then
        error @@ wrong_info
        @@ sprintf "%s: wrong number of arguments" (string_of_ctor ctor)
      else
        let* xst = mapM (fun x -> type_of_expr ctx x) xs in
        ok @@ TCtor (ctor, xst)
  | Let (bind, expr) ->
      let* new_ctx = update_ctx_with_binding ctx bind ~allow_rec:false in
      type_of_expr new_ctx expr
  | Case (expr, branches) -> (
      let* ty = type_of_expr' ctx expr in
      let check_branches_type_same f =
        let* brt =
          mapM
            (fun (pat, expr) ->
              let* new_ctx = f pat in
              type_of_expr new_ctx expr)
            branches
        in
        let brt' = filter_never_type brt in
        if brt' = [] then ok TNever
        else
          let t1 = List.hd brt' in
          if List.for_all (fun ty -> ty =* t1) brt' then ok t1
          else error @@ wrong_info "the types of branches are not all the same"
      in
      match ty with
      | TCtor (CtUnion, xs) ->
          let exhausted, remained = exhaust ctx branches xs false in
          if exhausted then
            if filter_never_type remained = [] then
              check_branches_type_same (fun pat ->
                  bind_type_in_union_pattern ctx pat xs)
            else error @@ wrong_info "unused cases"
          else
            error @@ wrong_info
            @@ sprintf "cases are not exhausted: %s\n expr type: %s"
                 (aux_show_list show_ty remained)
                 (aux_show_list show_ty xs)
      | TCtor _ as t ->
          let exhausted, remained = exhaust ctx branches [ t ] false in
          if exhausted then
            if filter_never_type remained = [] then
              check_branches_type_same (fun pat ->
                  bind_type_in_pattern ctx pat t)
            else error @@ wrong_info "unused cases"
          else
            error @@ wrong_info
            @@ sprintf "cases are not exhausted: %s\n expr type: %s"
                 (aux_show_list show_ty remained)
                 (show_ty t)
      | TNever ->
          if branches = [] then ok TNever
          else error @@ wrong_info "can't match never type"
      | _ as t ->
          error @@ wrong_info
          @@ sprintf "can't match this type: %s"
          @@ show_ty t)
  | Mark (var, exp) -> (
      let* label = type_of_var' ctx var in
      match label with
      | TCtor (CtCont, [ x; y ]) ->
          let new_ctx = (name_of_var var, label) :: ctx in
          let* ty = type_of_expr new_ctx exp in
          let+ ty =
            if ty =* TNever then ok @@ TCtor (CtUnion, [ x ])
            else if TCtor (CtReturn, [ ty ]) =* y then
              ok @@ TCtor (CtUnion, [ x; TCtor (CtReturn, [ ty ]) ])
            else
              error @@ wrong_info
              @@ sprintf
                   "%s: label has wrong return type: expr type is %s but label \
                    is %s"
                   (name_of_var var)
                   (show_ty @@ TCtor (CtReturn, [ ty ]))
                   (show_ty label)
          in
          ty
      | _ ->
          error @@ wrong_info
          @@ sprintf "%s: label should be annotated with cont type"
               (name_of_var var))
  | Goto (var, exp) ->
      let* label = type_of_var' ctx var in
      let+ _ =
        match label with
        | TCtor (CtCont, x :: _) ->
            let* ty = type_of_expr ctx exp in
            if ty =* x then ok ty
            else
              error @@ wrong_info
              @@ sprintf
                   "go to wrong label: checked expr type: %s\nannotated: %s"
                   (show_ty ty) (show_ty x)
        | _ ->
            error @@ wrong_info
            @@ sprintf "%s: label should be annotated with cont type"
                 (name_of_var var)
      in
      TNever
  | Lit (Int _) -> ok @@ TLit TInt
  | Lit (Bool _) -> ok @@ TLit TBool

and update_ctx_with_binding ctx bind ~allow_rec =
  match bind with
  | NonRec binding -> update_ctx_with_nonrec_binding ctx binding
  | Rec bindings -> update_ctx_with_rec_binding ctx bindings ~allow_rec
  | RecType bindings -> update_ctx_with_type_rec_binding ctx bindings

and update_ctx_with_nonrec_binding ctx (x, e1) =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  let+ t = check_bind (type_of_var ctx x) (type_of_expr ctx e1) in
  (name_of_var x, t) :: ctx

and update_ctx_with_rec_binding ctx bindings ~allow_rec =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  let* types_of_vars = types_of_vars ctx bindings in
  let new_ctx = types_of_vars @ ctx in
  let expr_types =
    types_of_exprs (if allow_rec then new_ctx else ctx) bindings
  in
  let+ final =
    map2M
      (fun (name, t1) t2 ->
        let+ ty = check_bind (Ok t1) t2 in
        (name, ty))
      types_of_vars expr_types
  in
  final @ ctx

and update_ctx_with_type_rec_binding ctx tbindings =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  let+ new_bindings =
    mapM
      (fun (v, t) ->
        let+ checked_ty = check_bind (type_of_var ctx v) (Ok t) in
        (name_of_var v, checked_ty))
      tbindings
  in
  new_bindings @ ctx

and types_of_exprs ctx = List.map (fun (_, exp) -> type_of_expr ctx exp)

let check_core core =
  let ctx = [] in
  let (Core binding) = core in
  let+ ctx =
    fold_leftM
      (fun ctx binding -> update_ctx_with_binding ctx binding ~allow_rec:true)
      ctx binding
  in
  ctx

let cexpr_of_var cexprctx name =
  Option.to_result ~none:(not_applicable @@ name ^ " can't be a C expression")
  @@ List.assoc_opt name cexprctx

let rec ty_to_ctype ctx = function
  | TVar _ as t -> ty_to_ctype ctx @@ expand_type_var ctx t
  | TCtor (ctor, _) -> (
      ok
      @@
      match ctor with
      | CtUnit -> C.raw_type "rt_ty_unit *" [||]
      | CtReturn -> C.raw_type "rt_ty_return *" [||]
      | CtHandle -> C.raw_type "rt_ty_handle *" [||]
      | CtTuple -> C.raw_type "rt_ty_tuple *" [||]
      | CtCont -> C.raw_type "rt_ty_cont *" [||]
      | CtUnion -> C.raw_type "rt_ty_union *" [||])
  | TApp _ | TForall _ ->
      error @@ not_applicable "unable to convert high order type to ctype"
  | TArrow (a, b) ->
      let* a = ty_to_ctype ctx a in
      let+ b = ty_to_ctype ctx b in
      C.func b [ a ]
  | TLit TInt -> ok @@ C.int
  | TLit TBool -> ok @@ C.int
  | TNever -> ok @@ C.ptr @@ C.void

let ctype_of_expr ctx expr =
  let* t = type_of_expr ctx expr in
  let+ ct = ty_to_ctype ctx t in
  ct

let emit_core_binding_decl funcs tyctx var = function
  | Lam (x, b) ->
      let* t1 = type_of_var tyctx x in
      let* t1' = ty_to_ctype tyctx t1 in
      let nx = name_of_var x in
      let tyctx = (nx, t1) :: tyctx in
      let* t2' = ctype_of_expr tyctx b in
      let cfunc =
        C.make_cfunction_declaration (name_of_var var) t2' [ (None, t1') ]
      in
      funcs := cfunc :: !funcs;
      ok ()
  | _ ->
      error
      @@ not_applicable "other expressions are not allowed in global scope"

let bind_vars_in_pattern_aux tyctx cexprctx fresh stmts (ctor, vars) ty cexpr =
  match ty with
  | TCtor (ctor', tys) when ctor = ctor' ->
      let vts = List.combine vars tys in
      let+ nvb =
        mapiM
          (fun i (var, ty) ->
            let* ct = ty_to_ctype tyctx ty in
            let tmp = fresh () in
            let st =
              C.decl tmp ct @@ Option.some
              @@ C.raw_expr "rt_extract_field(#0, #1)"
                   [| cexpr; C.lit @@ C.lint i |]
            in
            stmts := st :: !stmts;
            ok (name_of_var var, C.var tmp))
          vts
      in
      nvb @ cexprctx
  | _ -> error @@ wrong_info "not a correct ctor type"

let bind_vars_in_pattern tyctx cexprctx fresh stmts (pat, _) types cexpr =
  match pat with
  | PWild -> ok cexprctx
  | PCon (ctor, vars) ->
      let* ty = match_ctor_with_types types ctor in
      bind_vars_in_pattern_aux tyctx cexprctx fresh stmts (ctor, vars) ty cexpr

let rec emit_expr fresh stmts tyctx cexprctx =
  let emit_expr_aux x = emit_expr fresh stmts tyctx cexprctx x in
  function
  | Var var ->
      let+ cexpr = cexpr_of_var cexprctx @@ name_of_var var in
      cexpr
  | Lam _ ->
      error
      @@ not_applicable
           "doesn't allow high order functions in normal expressions"
  | App (f, a) as expr ->
      let* f = emit_expr_aux f in
      let* a = emit_expr_aux a in
      let* ct = ctype_of_expr tyctx expr in
      let tmp = fresh () in
      let st = C.decl tmp ct @@ Option.some @@ C.call f [ a ] in
      stmts := st :: !stmts;
      ok @@ C.var tmp
  | Ctor (ctor, xs) -> (
      let* xs = mapM (fun x -> emit_expr_aux x) xs in
      match ctor with
      | CtUnit -> ok @@ C.call (C.var "rt_make_unit") []
      | CtReturn -> ok @@ C.raw_expr "rt_make_return(#@)" @@ Array.of_list xs
      | CtHandle -> ok @@ C.raw_expr "rt_make_handle(#@)" @@ Array.of_list xs
      | CtTuple -> ok @@ C.raw_expr "rt_make_tuple(#@)" @@ Array.of_list xs
      | CtCont -> ok @@ C.raw_expr "rt_make_cont(#@)" @@ Array.of_list xs
      | CtUnion -> ok @@ C.raw_expr "rt_make_union(#@)" @@ Array.of_list xs)
  | Lit (Bool b) -> ok @@ C.lit @@ C.lbool b
  | Lit (Int n) -> ok @@ C.lit @@ C.lint n
  | Let (binding, e) ->
      let emit_local_binding = function
        | NonRec (var, expr) ->
            let tmp = fresh () in
            let* ct = ctype_of_expr tyctx expr in
            let+ init = emit_expr_aux expr in
            let st = C.decl tmp ct @@ Option.some init in
            stmts := st :: !stmts;
            (tyctx, (name_of_var var, C.var tmp) :: cexprctx)
        | Rec bindings ->
            let tmps = List.map (fun _ -> fresh ()) bindings in
            let* ts = types_of_vars tyctx bindings in
            let* cts = mapM (fun (_, ty) -> ty_to_ctype tyctx ty) ts in
            let+ inits = mapM (fun (_, expr) -> emit_expr_aux expr) bindings in
            let sts =
              map3
                (fun tmp ct init -> C.decl tmp ct @@ Option.some init)
                tmps cts inits
            in
            stmts := List.rev sts @ !stmts;
            let cexprctx =
              List.map2 (fun (name, _) tmp -> (name, C.var tmp)) ts tmps
            in
            (tyctx, cexprctx)
        | RecType bindings ->
            let+ tyctx = update_ctx_with_type_rec_binding tyctx bindings in
            (tyctx, cexprctx)
      in
      let* tyctx, cexprctx = emit_local_binding binding in
      let tmp = fresh () in
      let* ct = ctype_of_expr tyctx e in
      let* e = emit_expr fresh stmts tyctx cexprctx e in
      let st = C.decl tmp ct @@ Option.some e in
      stmts := st :: !stmts;
      ok @@ C.var tmp
  | Case (e, branches) as e' ->
      let* t = type_of_expr' tyctx e in
      let* ct = ctype_of_expr tyctx e' in
      let* ce = emit_expr_aux e in
      let tag = C.raw_expr "rt_extract_tag(#0)" [| ce |] in
      let emit_branch (pat, expr) stmts output =
        let* new_tyctx =
          match t with
          | TCtor (CtUnion, xs) -> bind_type_in_union_pattern tyctx pat xs
          | TCtor _ as t' -> bind_type_in_pattern tyctx pat t'
          | _ -> raise @@ Exn "unreachable"
        in
        let ts =
          match t with
          | TCtor (CtUnion, xs) -> xs
          | TCtor _ as t' -> [ t' ]
          | _ -> raise @@ Exn "unreachable"
        in
        let* new_cexprctx =
          bind_vars_in_pattern tyctx cexprctx fresh stmts (pat, expr) ts ce
        in
        let* bre = emit_expr fresh stmts new_tyctx new_cexprctx expr in
        let st = C.expr @@ C.asgn (C.var output) bre in
        stmts := st :: !stmts;
        ok ()
      in
      let out = fresh () in
      let* bre =
        mapM
          (fun (pat, expr) ->
            let stmts = ref [] in
            let* () = emit_branch (pat, expr) stmts out in
            let stmts = C.block @@ List.rev (C.break :: !stmts) in
            match pat with
            | PWild -> ok (Option.none, stmts)
            | PCon (ctor, _) ->
                let+ disc = cexpr_of_ctor ctor in
                (Option.some disc, stmts))
          branches
      in
      let st = C.decl out ct @@ Option.none in
      let sw = C.switch tag bre in
      stmts := sw :: st :: !stmts;
      ok @@ C.var out
  | Mark (label, exp) as e' ->
      let name = name_of_var label in
      let* ct = ctype_of_expr tyctx e' in
      let* lt = type_of_var' tyctx label in
      let* clt = ty_to_ctype tyctx lt in
      let tmp = fresh () in
      let mk_label =
        C.decl tmp clt @@ Option.some @@ C.raw_expr "rt_new_label()" [||]
      in
      let out = fresh () in
      let out_decl = C.decl out ct Option.none in
      let normal = ref [] in
      let* result =
        emit_expr fresh normal ((name, lt) :: tyctx)
          ((name, C.var tmp) :: cexprctx)
          exp
      in
      let st = C.expr @@ C.asgn (C.var out) result in
      normal := st :: !normal;
      let st =
        C.if'
          (C.raw_expr "rt_mark(#0)" [| C.var tmp |])
          (C.expr
          @@ C.asgn (C.var out)
               (C.raw_expr "rt_get_data_from_cont(#0)" [| C.var tmp |]))
          (Option.some @@ C.block @@ List.rev !normal)
      in
      stmts := st :: out_decl :: mk_label :: !stmts;
      ok @@ C.var out
  | Goto (label, exp) ->
      let* arg = emit_expr_aux exp in
      let* var = cexpr_of_var cexprctx (name_of_var label) in
      let st = C.expr @@ C.raw_expr "rt_goto(#0, #1)" [| var; arg |] in
      stmts := st :: !stmts;
      ok @@ C.raw_expr "rt_unreachable()" [||]

let emit_core_binding funcs tyctx cexprctx var = function
  | Lam (x, b) ->
      let* t1 = type_of_var tyctx x in
      let* t1' = ty_to_ctype tyctx t1 in
      let nx = name_of_var x in
      let tyctx = (nx, t1) :: tyctx in
      let* t2 = ctype_of_expr tyctx b in
      let fresh_id = ref 0 in
      let fresh () =
        let id = !fresh_id in
        fresh_id := id + 1;
        "tmp_" ^ string_of_int id
      in
      let stmts = ref [] in
      let cexprctx = (nx, C.var nx) :: cexprctx in
      let* final = emit_expr fresh stmts tyctx cexprctx b in
      let stmts = List.rev (C.return final :: !stmts) in
      let cfunc =
        C.make_cfunction (name_of_var var) t2
          [ (Option.some @@ name_of_var x, t1') ]
          stmts
      in
      funcs := cfunc :: !funcs;
      ok ()
  | _ ->
      error
      @@ not_applicable "other expressions are not allowed in global scope"

let update_cexpr_ctx_with_binding cexprctx = function
  | NonRec (x, _) ->
      let name = name_of_var x in
      (name, C.var name) :: cexprctx
  | Rec bindings ->
      let names = List.map (fun (var, _) -> name_of_var var) bindings in
      let new_bindingds = List.combine names (List.map C.var names) in
      new_bindingds @ cexprctx
  | RecType _ -> cexprctx

let emit_core core =
  let (Core binding) = core in
  let funcs = ref [] in
  (* forward declaration *)
  let* _ =
    fold_leftM
      (fun tyctx binding ->
        let* tyctx = update_ctx_with_binding tyctx binding ~allow_rec:true in
        let* _ =
          match binding with
          | NonRec (var, expr) -> emit_core_binding_decl funcs tyctx var expr
          | Rec bds ->
              iterM
                (fun (var, expr) -> emit_core_binding_decl funcs tyctx var expr)
                bds
          | RecType _ -> ok ()
        in
        ok tyctx)
      [] binding
  in
  let+ _ =
    fold_leftM
      (fun (tyctx, cexprctx) binding ->
        let* tyctx = update_ctx_with_binding tyctx binding ~allow_rec:true in
        let cexprctx = update_cexpr_ctx_with_binding cexprctx binding in
        let* _ =
          match binding with
          | NonRec (var, expr) ->
              emit_core_binding funcs tyctx cexprctx var expr
          | Rec bds ->
              iterM
                (fun (var, expr) ->
                  emit_core_binding funcs tyctx cexprctx var expr)
                bds
          | RecType _ -> ok ()
        in
        ok (tyctx, cexprctx))
      ([], []) binding
  in
  C.pp_csource @@ List.rev @@ !funcs