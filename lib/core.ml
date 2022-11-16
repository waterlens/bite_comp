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

and pattern = PVar of var | PCon of ctor * var list | PWild
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

let type_of ctx = function
  | TVar var as ty -> value ~default:ty @@ type_of_var ctx var
  | _ as ty -> ty

let rec equal_ty ctx t1 t2 =
  let ( =* ) = equal_ty ctx in
  match (t1, t2) with
  | TVar v1, TVar v2 when name_of_var v1 = name_of_var v2 -> true
  | _ -> (
      match (type_of ctx t1, type_of ctx t2) with
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
      | PVar _ -> exhaust ctx xs types true
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
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok _, Ok _ ->
      error
      @@ wrong_info
           "the annotated type is not equal with checked type of expression"

let rec type_of_expr ctx =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  function
  | Var v -> type_of_var ctx v
  | Lam (x, b) ->
      let* xt = type_of_var ctx x in
      let* bt = type_of_expr ((name_of_var x, xt) :: ctx) b in
      ok @@ TArrow (xt, bt)
  | App (x, y) -> (
      let* xt = type_of_expr ctx x in
      let xt = type_of ctx xt in
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
  | Let (bind, e2) ->
      let* new_ctx = update_ctx_with_binding ctx bind in
      type_of_expr new_ctx e2
  | Case (expr, branches) -> (
      let* ty = type_of_expr ctx expr in
      let ty = type_of ctx ty in
      let check_pattern_bind ctx vars tys =
        let+ tys =
          map2M check_bind
            (List.map (fun x -> type_of_var ctx x) vars)
            (List.map (fun x -> ok x) tys)
        in
        let bindings =
          List.map2 (fun var ty -> (name_of_var var, ty)) vars tys
        in
        bindings @ ctx
      in
      let check_pattern_var_num vars tys =
        if List.length vars <> List.length tys then
          error @@ wrong_info
          @@ sprintf "wrong number of pattern variables: expected %d, got %d"
               (List.length tys) (List.length vars)
        else ok ()
      in
      let bind_pattern ctx pat ty =
        match pat with
        | PWild -> ok ctx
        | PVar v -> ok @@ ((name_of_var v, ty) :: ctx)
        | PCon (ctor, vars) -> (
            match ty with
            | TCtor (ctor', tys) when ctor = ctor' ->
                let* _ = check_pattern_var_num vars tys in
                check_pattern_bind ctx vars tys
            | _ -> error @@ wrong_info "not a correct ctor type")
      in
      let bind_union_pattern ctx pat types =
        match pat with
        | PWild -> ok ctx
        | PVar v -> ok @@ ((name_of_var v, TCtor (CtUnion, types)) :: ctx)
        | PCon (ctor, vars) -> (
            let ty =
              List.find
                (fun ty ->
                  match ty with
                  | TCtor (ctor', _) when ctor = ctor' -> true
                  | _ -> false)
                types
            in
            match ty with
            | TCtor (ctor', tys) when ctor = ctor' ->
                let* _ = check_pattern_var_num vars tys in
                check_pattern_bind ctx vars tys
            | _ -> error @@ wrong_info "not a correct ctor type")
      in
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
                  bind_union_pattern ctx pat xs)
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
              check_branches_type_same (fun pat -> bind_pattern ctx pat t)
            else error @@ wrong_info "unused cases"
          else
            error @@ wrong_info
            @@ sprintf "cases are not exhausted: %s\n expr type: %s"
                 (aux_show_list show_ty remained)
                 (show_ty t)
      | TNever ->
          if branches = [] then ok TNever
          else error @@ wrong_info "can't match never type"
      | _ -> error @@ wrong_info "can't match these types")
  | Mark (var, exp) -> (
      let* label = type_of_var ctx var in
      let label = type_of ctx label in
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
      let* label = type_of_var ctx var in
      let label = type_of ctx label in
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

and update_ctx_with_binding ctx bind =
  let ( =* ) t1 t2 = equal_ty ctx t1 t2 in
  let check_bind = check_bind ( =* ) in
  match bind with
  | NonRec (x, e1) ->
      let+ t = check_bind (type_of_var ctx x) (type_of_expr ctx e1) in
      (name_of_var x, t) :: ctx
  | Rec bindings ->
      let rec type_bindings acc = function
        | [] -> ok @@ List.rev acc
        | (var, _) :: xs ->
            let* ty = type_of_var ctx var in
            let name = name_of_var var in
            let+ acc = type_bindings ((name, ty) :: acc) xs in
            acc
      in
      let rec expr_types acc ctx = function
        | [] -> List.rev acc
        | (_, exp) :: xs ->
            let ty = type_of_expr ctx exp in
            let acc = expr_types (ty :: acc) ctx xs in
            acc
      in
      let* type_bindings = type_bindings [] bindings in
      let new_ctx = type_bindings @ ctx in
      let expr_types = expr_types [] new_ctx bindings in
      let+ _ =
        map2M (fun (_, t1) t2 -> check_bind (Ok t1) t2) type_bindings expr_types
      in
      new_ctx
  | RecType bindings ->
      let+ new_bindings =
        mapM
          (fun (v, t) ->
            let+ checked_ty = check_bind (type_of_var ctx v) (Ok t) in
            (name_of_var v, checked_ty))
          bindings
      in
      new_bindings @ ctx

let check_core core =
  let ctx = [] in
  let (Core binding) = core in
  let+ ctx =
    fold_leftM
      (fun ctx binding -> update_ctx_with_binding ctx binding)
      ctx binding
  in
  ctx

let cexpr_of_var cexprctx name =
  Option.to_result ~none:(not_applicable @@ name ^ " can't be a C expression")
  @@ List.assoc_opt name cexprctx

let rec ty_to_ctype ctx = function
  | TVar _ as t -> ty_to_ctype ctx @@ type_of ctx t
  | TCtor (ctor, _) -> (
      ok
      @@
      match ctor with
      | CtUnit -> C.inline_type "struct rt_unit *"
      | CtReturn -> C.inline_type "struct rt_return *"
      | CtHandle -> C.inline_type "struct rt_handle *"
      | CtTuple -> C.inline_type "struct rt_tuple *"
      | CtCont -> C.inline_type "struct rt_cont *"
      | CtUnion -> C.inline_type "union rt_union *")
  | TApp _ | TForall _ ->
      error @@ not_applicable "unable to convert high order type to ctype"
  | TArrow (a, b) ->
      let* a = ty_to_ctype ctx a in
      let+ b = ty_to_ctype ctx b in
      C.func b [ a ]
  | TLit TInt -> ok @@ C.int
  | TLit TBool -> ok @@ C.int
  | TNever -> ok @@ C.void

let placeholder = C.inline_expr "rt_expr_placeholer"

let fplaceholder =
  C.make_cfunction_declaration "rt_function_placeholder" (C.ptr C.void) []

let emit_core_binding_decl buf tyctx var = function
  | Lam (x, b) ->
      let* t1 = type_of_var tyctx x in
      let* t1' = ty_to_ctype tyctx t1 in
      let* t2 = type_of_expr ((name_of_var x, t1) :: tyctx) b in
      let* t2' = ty_to_ctype tyctx t2 in
      let cfunc =
        C.make_cfunction_declaration (name_of_var var) t2' [ (None, t1') ]
      in
      Buffer.add_string buf @@ C.show_cfunction cfunc;
      ok ()
  | _ ->
      error
      @@ not_applicable "other expressions are not allowed in global scope"

let rec emit_expr fresh stmts tyctx cexprctx = function
  | Var var ->
      let+ cexpr = cexpr_of_var cexprctx @@ name_of_var var in
      cexpr
  | Lam _ ->
      error
      @@ not_applicable
           "doesn't allow high order functions in normal expressions"
  | App (f, a) as expr ->
      let* f = emit_expr fresh stmts tyctx cexprctx f in
      let* a = emit_expr fresh stmts tyctx cexprctx a in
      let* t = type_of_expr tyctx expr in
      let* ct = ty_to_ctype tyctx t in
      let tmp = fresh () in
      let st = C.decl tmp ct @@ Option.some @@ C.call f [ a ] in
      stmts := st :: !stmts;
      ok @@ C.var tmp
  | Ctor (ctor, xs) -> ok placeholder
  | Lit (Bool b) -> ok @@ C.lit @@ C.lbool b
  | Lit (Int n) -> ok @@ C.lit @@ C.lint n
  | Let (binding, e) -> ok placeholder
  | Case (e, branches) -> ok placeholder
  | Mark (label, exp) -> ok placeholder
  | Goto (label, exp) -> ok placeholder

let emit_core_binding buf tyctx var = function
  | Lam (x, b) ->
      let* t1 = type_of_var tyctx x in
      let* t1' = ty_to_ctype tyctx t1 in
      let* t2 = type_of_expr ((name_of_var x, t1) :: tyctx) b in
      let* t2' = ty_to_ctype tyctx t2 in
      let cfunc =
        C.make_cfunction_declaration (name_of_var var) t2' [ (None, t1') ]
      in
      Buffer.add_string buf @@ C.show_cfunction cfunc;
      ok ()
  | _ ->
      error
      @@ not_applicable "other expressions are not allowed in global scope"

let emit_core core =
  let ctx = [] in
  let (Core binding) = core in
  let buf = Buffer.create 1024 in
  let* ctx =
    fold_leftM
      (fun ctx binding -> update_ctx_with_binding ctx binding)
      ctx binding
  in
  let* () =
    iterM
      (fun binding ->
        match binding with
        | NonRec (var, expr) -> emit_core_binding_decl buf ctx var expr
        | Rec bds ->
            iterM
              (fun (var, expr) -> emit_core_binding_decl buf ctx var expr)
              bds
        | RecType _ -> ok ())
      binding
  in
  let+ () =
    iterM
      (fun binding ->
        match binding with
        | NonRec (var, expr) -> emit_core_binding buf ctx var expr
        | Rec bds ->
            iterM (fun (var, expr) -> emit_core_binding buf ctx var expr) bds
        | RecType _ -> ok ())
      binding
  in
  Buffer.to_bytes buf